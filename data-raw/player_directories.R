
# Info --------------------------------------------------------------------

# This script will parse player directories for CBS, FFToday, Fantasypros and NFL.
# Make sure you load the ffanalytics package first to have access to tidyverse and rvest.
# The command to create the sysdata.rda file is usethis::use_data(player_ids, overwrite = TRUE, internal = TRUE).
# This command needs to be executed from the package root folder.

#### CBS Players #### ----
team_links <- read_html("https://www.cbssports.com/search/football/players/#tab-content-2") %>%
  html_elements("a[href*='roster']") %>% html_attr("href") %>% as.list()
names(team_links) <- team_links %>% str_extract("[A-Z]{2,3}")
cbs_data <- function(u){
  cbs_pge <- read_html(u)
  cbs_pge %>% html_elements("span.CellPlayerName-icon") %>% xml_remove()
  cbs_pge %>% html_elements("span.CellPlayerName--short") %>% xml_remove()
  pids <- cbs_pge  %>% html_elements("span.CellPlayerName--long a") %>%
    projection_sources$CBS$extract_pid()
  cbs_pge %>%
    html_elements("div.Page-colMain table.TableBase-table") %>% html_table() %>%
    map(mutate, EXP = as.character(EXP), NO = as.character(NO),
        Player = str_trim(str_remove_all(Player, "[:cntrl:]"))) %>% bind_rows() %>%
    add_column(src_id = pids, .before = 1) %>% select(src_id, Player, POS) %>%
    rename_all(tolower) %>%
    mutate(pos = recode(pos, !!!ffanalytics:::pos_corrections)) %>%
    mutate(pos = recode(pos, OLB = "LB", ILB = "LB", NT = "DL")) %>%
    filter(pos %in% c("QB", "RB", "WR", "TE", "K", "DL", "LB", "DB"))
}
final_cbs <- map(team_links, cbs_data) %>% bind_rows(.id = "team")
#### FFToday Players #### ----
fft_pos_players <- function(u){
  get_fft <- read_html(u)
  player_teams <- get_fft %>% html_elements("td span.smallbody a") %>% html_text() %>%
    str_extract("[A-Z]{2,3}$")
  pids <- get_fft %>% html_elements("td span.smallbody a") %>%
    projection_sources$FFToday$extract_pid()
  get_fft %>% html_elements("td span.smallbody a") %>% html_text() %>%
    str_remove("[A-Z]{2,3}$") %>% str_trim() %>% str_remove("\\s(S|J)r\\.") %>%
    str_split(",\\s") %>%
    map(as.list) %>% map(`names<-`, c("last_name", "first_name")) %>%
    map(as_tibble) %>% bind_rows() %>% unite(player, first_name, last_name, sep = " ") %>%
    mutate(team = player_teams) %>% add_column(src_id = pids, .before = 1)
}
fft <- c("QB", "RB", "WR", "TE", "K",  "DL",  "LB", "DB")
fft <- setNames(fft, fft)
fft <- map(fft, ~paste0("http://www.fftoday.com/stats/players?Pos=", .x)) %>%
  map(fft_pos_players) %>% bind_rows(.id = "pos")
final_fft

#### FantasyPros #### ----
# Fantasy pros numeric ids

# Getting Players from last years stats
# Getting links
fp_pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB")
last_year = 2020

fp_lastyr_links = paste0("https://www.fantasypros.com/nfl/stats/",
                         tolower(fp_pos), ".php?year=", last_year)
fp_lastyr_session = session(fp_lastyr_links[1])

fp_lastyr_l = lapply(fp_lastyr_links, function(x) {

  print(paste0("Starting ", x, " in 5 seconds"))
  Sys.sleep(5)

  html_page = fp_lastyr_session %>%
    session_jump_to(x) %>%
    read_html()

  pos = sub(".*?stats/(.*?)\\..*", "\\1", x)

  name_id = html_page %>%
    html_elements("td.player-label > a.player-name") %>%
    html_attr("href") %>%
    sub(".*?stats/(.*?)\\.php$", "\\1", .)

  internal_id = html_page %>%
    html_elements("a.fp-player-link") %>%
    html_attr("class") %>%
    sub(".+\\-([0-9]+)$", "\\1", .)

  player_name = html_page %>%
    html_elements("a.fp-player-link") %>%
    html_attr("fp-player-name") %>%
    sub(".+\\-([0-9]+)$", "\\1", .)

  data.frame(player_name,
             pos,
             name_id,
             internal_id)


})
rm(fp_lastyr_session)
gc()
fp_lastyr = bind_rows(fp_lastyr_l)

# Pulling from the depth charts
fp_dc_session = session("https://www.fantasypros.com/nfl/depth-charts.php")

# Getting team links
fp_dc_links <- read_html(fp_dc_session) %>%
  html_elements("a[href *='/depth-chart/']:not([class])") %>%
  html_attr("href")

fp_dc_l = lapply(fp_dc_links, function(x) {

  print(paste0("Starting ", x, " in 5 seconds"))
  Sys.sleep(5)

  team_session = fp_dc_session %>%
    session_jump_to(x)

  html_page = read_html(team_session)
  l = vector("list", 2L)

  for(i in 1:2){

    name_id = html_page %>%
      html_elements("td.player-label > a.player-name") %>%
      html_attr("href") %>%
      sub(".*?players/(.*?)\\.php$", "\\1", .)

    internal_id = html_page %>%
      html_elements("table > tbody > tr") %>%
      html_attr("class") %>%
      sub(".+\\-([0-9]+)$", "\\1", .)

    player_name = html_page %>%
      html_elements("td.player-label > a.player-name") %>%
      html_text2()

    pos = html_page %>%
      html_elements("table > tbody > tr > td:nth-child(1)") %>%
      html_text2() %>%
      sub("\\d+", "", .) %>%
      tolower()

    l[[i]] = data.frame(player_name,
                        pos,
                        name_id,
                        internal_id)

    # Switch to defense
    if(i == 1) {
      html_page = session_jump_to(team_session, "?side=dst")
    }

  }
  bind_rows(l)

})

rm(fp_dc_session)
gc()
fp_dc = bind_rows(fp_dc_l)

final_fp_all = bind_rows(fp_lastyr, fp_dc) %>%
  distinct()

rm(list = ls(pattern = "^fp_"))

#### NFL Players #### ----
nfl_url <- "https://fantasy.nfl.com/research/players/"
nfl_session <- session(nfl_url)
nfl_table <- tibble(nfl_id = character(), player = character(), pos = character(), team = character())
repeat{
  print(nfl_session$url)
  player_ids <- nfl_session %>% read_html() %>% html_elements("table td:first-child a.playerName") %>%
    html_attr("href") %>% str_extract("[0-9]+$")
  player_data <- nfl_session %>% read_html() %>% html_elements("table td:first-child") %>% html_text() %>%
    str_trim() %>% str_remove("\\s[A-Z]$") %>% str_split_fixed(" - ", n= 2) %>%
    `colnames<-`(c("player", "team")) %>%  as_tibble()  %>%
    extract(player, c("player", "pos"), "(.+)\\s([QRWTBKDEF]{1,3}$)")  %>%
    add_column(nfl_id = player_ids, .before = 1)
  nfl_table <- nfl_table %>% bind_rows(player_data)
  next_link <- nfl_session %>% html_node("li.next a") %>%
    html_attr("href")
  if(is.na(next_link))
    break()
  next_link <- str_replace(next_link, "statSeason=2020", "statSeason=2021")
  next_link <- str_replace(next_link, "statWeek=17", "statWeek=1")
  nfl_session <- nfl_session %>% jump_to(paste0(nfl_url, next_link))
}


#### NUmber fire #### ----
