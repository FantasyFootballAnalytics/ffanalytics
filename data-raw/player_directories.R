
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
  Sys.sleep(1)
  next_link <- str_replace(next_link, "statSeason=2020", "statSeason=2021")
  next_link <- str_replace(next_link, "statWeek=17", "statWeek=1")
  nfl_session <- nfl_session %>% jump_to(paste0(nfl_url, next_link))
}


#### NUmber fire #### ----
html_page = read_html("https://www.numberfire.com/nfl/players")

# Player names
nf_player_pos = html_page %>%
  html_elements("div.all-players__wrap > div") %>%
  html_text2()

# Numberfire ids
nf_ids = html_page %>%
  html_elements("div.all-players__wrap > div > a") %>%
  html_attr("href") %>%
  basename()

final_nf = data.frame(numberfire_id = nf_ids,
                      player = sub("(.+?)\\s+\\(.*", "\\1", nf_player_pos),
                      pos = sub(".+?\\s+\\(([A-Z]+),.+", "\\1", nf_player_pos))

final_nf = final_nf %>%
  transmute(numberfire_id,
            merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
            merge_id = paste0(merge_id, "_", tolower(pos)))


rm(list = ls(pattern = "^nf_|html_page"))

#### RTSports #### ----


rt_links = paste0("https://www.freedraftguide.com/football/draft-guide-rankings-provider.php?POS=",
                  rts_pos_idx)

rt_l = lapply(rt_links, function(x) {

  print("Waiting 10 seconds")
  Sys.sleep(10)

  page_l = httr::content(httr::GET(x))
  page_l = unlist(page_l[names(page_l) == "player_list"], recursive = FALSE)

  page_l = lapply(page_l, `[`, c("name", "player_id", "position"))
  bind_rows(page_l)

})

final_rt = bind_rows(rt_l) %>%
  transmute(pos = setNames(names(rts_pos_idx), rts_pos_idx)[position],
            player = name,
            rts_new_id = player_id)

#### Fleaflicker ----

html_page = read_html("https://www.fleaflicker.com/nfl/cheat-sheet")

flfl_name_id = html_page %>%
  html_elements("table > tr > td:nth-child(2) > div > div > a") %>%
  html_attr("href") %>%
  basename() %>%
  sub("(.*)\\-([0-9]+)$", "\\1_\\2", .)

flfl_pos = html_page %>%
  html_elements("table > tr > td:nth-child(1)") %>%
  html_text2() %>%
  grep("^[A-Z0-9/]+$", ., value = TRUE) %>%
  gsub("\\d+|/", "", .)

final_flfl = data.frame(flfl_name_id = flfl_name_id,
                        pos = flfl_pos)

final_flfl = final_flfl %>%
  extract(flfl_name_id, c("first_name", "last_name", "fleaflicker_id"), "(.*?)\\-(.*?)_(.*)") %>%
  transmute(merge_id = paste0(first_name, last_name),
            merge_id = gsub("[[:punct:]]", "", tolower(merge_id)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            merge_id, fleaflicker_id)

#### Yahoo ----

html_session = session("https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=AD&pos=ALL&sort=DA_AP")

l_yahoo = list()
i = 0

while(length(l_yahoo) < 12) {

  next_page = paste0(html_session$url, "&count=", length(l_yahoo) * 50)

  html_page = html_session %>%
    session_jump_to(next_page) %>%
    read_html()

  i = i + 1

  yahoo_id = html_page %>%
    html_elements("table > tbody > tr > td > div > div > span > a") %>%
    html_attr("data-ys-playerid")

  yahoo_name_pos = html_page %>%
    html_elements("table > tbody > tr > td > div > div > div") %>%
    html_text2() %>%
    grep(":", ., fixed = TRUE, invert = TRUE, value = TRUE) %>%
    data.table::tstrsplit("\\s+[A-Za-z]+\\s+\\-\\s+")

  temp_df = data.frame(stats_id = yahoo_id)
  temp_df[c("player", "pos")] = yahoo_name_pos

  l_yahoo[[i]] = temp_df

  print(paste0("Read ", i, "/12 pages"))
  print("Sleeping for 2 seconds")
  Sys.sleep(2)

}

final_yahoo = bind_rows(l_yahoo)

final_yahoo = final_yahoo %>%
  transmute(merge_id = gsub("[[:punct:]]", "", tolower(player)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            merge_id, stats_id)



# Cleaning up above scrapes

rm(list = grep("^(?!final).+", ls(), value = TRUE, perl = TRUE))
gc()



# updating player_ids table by name & pos

curr_ids = ffanalytics:::player_ids

my_fl_ids = httr::GET("https://api.myfantasyleague.com/2021/export?TYPE=players&L=&APIKEY=&DETAILS=1&SINCE=&PLAYERS=&JSON=1") %>%
  httr::content() %>%
  `[[`("players") %>%
  `[[`("player") %>%
  purrr::map(tibble::as_tibble) %>%
  dplyr::bind_rows() %>%
  mutate(nfl_id = basename(nfl_id)) %>% # prob unnecessary in future
  tidyr::extract(name, c("last_name", "first_name"), "(.+),\\s(.+)") %>%
  mutate(across(everything(), ~gsub("(?![.-])[[:punct:]]", "", ., perl = TRUE))) %>%
  dplyr::mutate(name = paste0(first_name," ",last_name))

updated_ids = my_fl_ids %>%
  select(first_name, last_name, position, id, team, ends_with("_id")) %>%
  filter(if_any(ends_with("_id"), ~ . != 0 & !is.na(.))) %>%
  mutate(merge_id = paste0(first_name, last_name),
         merge_id = gsub("[[:punct:]]|\\s+", "", tolower(merge_id)),
         merge_id = paste0(merge_id, "_", tolower(position)))

new_ids = mget(ls(pattern = "^final"))
new_ids = Reduce(function(x, y) full_join(x, y, "merge_id"), new_ids) %>%
  filter(!grepl("_(dst|def)$", merge_id)) %>%
  distinct()

# Updating the common columns in the myfantasyleague data
# common_cols = setdiff(intersect(names(new_ids), grep("_id$", names(updated_ids), value = TRUE)), "merge_id")
curr_cols = setdiff(grep("_id$", names(curr_ids), value = TRUE), "id")

sum(!is.na(curr_ids$stats_id))
sum(!is.na(curr_ids$fleaflicker_id))
sum(!is.na(curr_ids$numfire_id))

for(j in curr_cols) {

  if(j %in% names(updated_ids) && j %in% names(new_ids)) {

    df_updated = updated_ids[c("id", "merge_id", j)]
    updated_name = paste0(j, "_updated")
    names(df_updated)[3] = updated_name

    df_new = new_ids[c("merge_id", j)]
    new_name = paste0(j, "_new")
    names(df_new)[2] = new_name

    curr_ids = curr_ids %>%
      left_join(df_updated, "id") %>%
      left_join(df_new, "merge_id")

    curr_ids[[j]] = coalesce(curr_ids[[j]], curr_ids[[updated_name]], curr_ids[[new_name]])
    curr_ids[c(updated_name, new_name, "merge_id")] = NULL

  } else if(j %in% names(updated_ids)) {

    df_updated = updated_ids[c("id", j)]
    updated_name = paste0(j, "_updated")
    names(df_updated)[2] = updated_name

    curr_ids = curr_ids %>%
      left_join(df_updated, "id")

    curr_ids[[j]] = coalesce(curr_ids[[j]], curr_ids[[updated_name]])
    curr_ids[[updated_name]] = NULL

  } else if(j %in% names(new_ids)) {

    df_updated = updated_ids[c("id", "merge_id")]

    df_new = new_ids[c("merge_id", j)]
    new_name = paste0(j, "_new")
    names(df_new)[2] = new_name

    curr_ids = curr_ids %>%
      left_join(df_updated, "id") %>%
      left_join(df_new, "merge_id")


    curr_ids[[j]] = coalesce(curr_ids[[j]], curr_ids[[new_name]])
    curr_ids[c(new_name, "merge_id")] = NULL

  }
}

sum(!is.na(curr_ids$stats_id))
sum(!is.na(curr_ids$fleaflicker_id))
sum(!is.na(curr_ids$numfire_id))

# Run necessary QA. Looks at the data. Etc..

dim(ffanalytics:::player_ids)
dim(curr_ids)

colSums(!is.na(ffanalytics:::player_ids))
colSums(!is.na(curr_ids))

# New - old
colSums(!is.na(curr_ids)) - colSums(!is.na(ffanalytics:::player_ids))


saveRDS(curr_ids, "/Users/Andrew/Desktop/player_ids.rds")

# After running necessary QA, restart R, run the below lines
player_ids = readRDS("/Users/Andrew/Desktop/player_ids.rds")
usethis::use_data(player_ids, overwrite = TRUE, internal = TRUE)







