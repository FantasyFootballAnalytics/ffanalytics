#### CBS Players ####
team_links <- read_html("https://www.cbssports.com/search/football/players/#tab-content-2") %>%
  html_nodes("a[href*='roster']") %>% html_attr("href") %>% as.list()
names(team_links) <- team_links %>% str_extract("[A-Z]{2,3}") 
cbs_data <- function(u){
  cbs_pge <- read_html(u)
  cbs_pge %>% html_nodes("span.CellPlayerName-icon") %>% xml_remove()
  cbs_pge %>% html_nodes("span.CellPlayerName--short") %>% xml_remove()
  pids <- cbs_pge  %>% html_nodes("span.CellPlayerName--long a") %>%
    projection_sources$CBS$extract_pid()
  cbs_pge %>%
    html_nodes("div.Page-colMain table.TableBase-table") %>% html_table() %>%
    map(mutate, EXP = as.character(EXP), NO = as.character(NO), 
        Player = str_trim(str_remove_all(Player, "[:cntrl:]"))) %>% bind_rows() %>%
    add_column(src_id = pids, .before = 1) %>% select(src_id, Player, POS) %>%
    rename_all(tolower) %>% 
    mutate(pos = recode(pos, !!!ffanalytics:::pos_corrections)) %>%
    mutate(pos = recode(pos, OLB = "LB", ILB = "LB", NT = "DL")) %>%
    filter(pos %in% c("QB", "RB", "WR", "TE", "K", "DL", "LB", "DB"))
}
cbs_players <- team_links %>% map(cbs_data) %>% bind_rows(.id = "team")
#### FFToday Players ####
fft_pos_players <- function(u){
  get_fft <- read_html(u) 
  player_teams <- get_fft %>% html_nodes("td span.smallbody a") %>% html_text() %>%
    str_extract("[A-Z]{2,3}$")
  pids <- get_fft %>% html_nodes("td span.smallbody a") %>%
    projection_sources$FFToday$extract_pid()
  get_fft %>% html_nodes("td span.smallbody a") %>% html_text() %>%
    str_remove("[A-Z]{2,3}$") %>% str_trim() %>% str_remove("\\s(S|J)r\\.") %>%
    str_split(",\\s") %>% 
    map(as.list) %>% map(`names<-`, c("last_name", "first_name")) %>% 
    map(as_tibble) %>% bind_rows() %>% unite(player, first_name, last_name, sep = " ") %>%
    mutate(team = player_teams) %>% add_column(src_id = pids, .before = 1)
}
fft <- c(QB = "QB", RB = "RB", WR = "WR", TE = "TE", K = "K",  DL = "DL",  LB = "LB", DB = "DB")  %>%
  map(~paste0("http://www.fftoday.com/stats/players?Pos=", .x)) %>%
  map(fft_pos_players) %>% bind_rows(.id = "pos")
#### FantasyPros ####
# Open FantasyPros depth charts page in an html session
fp_dc_page <- rvest::html_session("https://www.fantasypros.com/nfl/depth-charts.php")
# Get the page content:
page_content <- xml2::read_html(fp_dc_page)
# Now we find links to each of the teams depth charts pages
# a tags with a href that contains '/depth-chart/' and doesn't have a class
# attribute
team_dc_links <- rvest::html_nodes(fp_dc_page, css = "a[href *='/depth-chart/']:not([class])")
# Extract the relative urls and team names
team_dc_urls <- rvest::html_attr(team_dc_links, "href")
team_names <-  rvest::html_text(team_dc_links)
team_pos_tables <- data.frame()
for(tm in seq_along(team_dc_urls)){
  # go to the team page
  team_page <- rvest::jump_to(fp_dc_page, team_dc_urls[[tm]])
  # We have to extract data in two rounds, one for Off one for Def
  for(i in 1:2){
    # Get the page conten
    team_page_content <- httr::content(team_page$response)
    # Extract player identifiers as players page name
    player_ids <- gsub(".php", "", basename(rvest::html_attr(rvest::html_nodes(team_page_content, "a[class = 'player-name']"), "href")))
    # Get the position tables
    position_tables <- rvest::html_table(rvest::html_nodes(team_page_content, "table[class *= 'position-table']"))
    # Standardize the column names and get Position names and ensure rank is numeric
    pos_tables <- lapply(position_tables, function(p_tbl){
      names(p_tbl) <- gsub("Quarterbacks|Running Backs|Wide Receivers|Tight Ends|Defensive Lineman|Linebacker|Defensive Back", 
                           "Player", names(p_tbl))
      names(p_tbl) <- gsub("(QB|RB|WR|TE|DL|LB|DB)\\s", "", names(p_tbl))
      p_tbl$Pos <- gsub("[0-9]", "", p_tbl$Pos)
      p_tbl[, grep("Rank",names(p_tbl))] <- as.numeric(p_tbl[, grep("Rank", names(p_tbl))])
      return(p_tbl)
    })
    # Combine the tables and set player names and ids
    pos_table <- dplyr::bind_rows(pos_tables)
    pos_table$Team <- team_names[tm]
    pos_table$id <- player_ids 
    team_pos_tables <- dplyr::bind_rows(team_pos_tables, pos_table)
    # On the first run through we need to switch to the Def page 
    if(i == 1)
      team_page <- rvest::jump_to(team_page, "?side=dst")
  }
}
#### NFL Players ####
nfl_url <- "https://fantasy.nfl.com/research/players/"
nfl_session <- html_session(nfl_url)
nfl_table <- tibble(nfl_id = NA, player = NA, pos = NA, team = NA) %>% slice(0)
repeat{
  print(nfl_session$url)
  player_ids <- nfl_session %>% read_html() %>% html_nodes("table td:first-child a.playerName") %>%
    html_attr("href") %>% str_extract("[0-9]+$")
  player_data <- nfl_session %>% read_html() %>% html_nodes("table td:first-child") %>% html_text() %>%
    str_trim() %>% str_remove("\\s[A-Z]$") %>% str_split_fixed(" - ", n= 2) %>%
    `colnames<-`(c("player", "team")) %>%  as_tibble()  %>%
    extract(player, c("player", "pos"), "(.+)\\s([QRWTBKDEF]{1,3}$)")  %>%
    add_column(nfl_id = player_ids, .before = 1) 
  nfl_table <- nfl_table %>% bind_rows(player_data)
  next_link <- nfl_session %>% html_node("li.next a") %>%
    html_attr("href")
  if(is.na(next_link))
    break()
  next_link <- str_replace(next_link, "statSeason=2019", "statSeason=2020")
  next_link <- str_replace(next_link, "statWeek=17", "statWeek=1")
  nfl_session <- nfl_session %>% jump_to(paste0(nfl_url, next_link))
}