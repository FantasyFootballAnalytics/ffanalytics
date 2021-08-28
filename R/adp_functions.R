#' Get ADP/AAV data from RTSports
#'
#' This function scrapes ADP or AAV data from RTSports
#' @param aav Indicates whether AAV data is scraped. If set to \code{FALSE} (Default),
#' ADP data is scraped. Set it to \code{TRUE} to scrape AAV data.
#' @return A \link{data.frame} with the results.
#' @export
rts_draft <- function(aav = FALSE){
  draft_url <- "https://www.freedraftguide.com/football/adp-aav-provider.php?NUM=&STYLE=0&AAV="
  if(aav)
    draft_url <- paste0(draft_url, "YES")

  draft_col <- "avg"
  names(draft_col) <- ifelse(aav, "aav", "adp")

  rts_adp <- httr::content(httr::GET(draft_url))

  adp_tbl <- bind_rows(lapply(rts_adp$player_list, data.frame, stringsAsFactors = FALSE)) %>%
    rename(rts_id = player_id)

  adp_tbl <- add_column(adp_tbl, id = id_col(adp_tbl$rts_id, "rts_id"), .before = 1) %>%
    rename(!!!draft_col) %>% clean_names()

  if(aav){
    adp_tbl <- mutate(adp_tbl, aav = as.numeric(aav))
  } else {
    adp_tbl <- mutate(adp_tbl, adp = as.numeric(adp))
  }

  return(adp_tbl)
}

#' Get ADP data from CBS
#'
#' This function scrapes ADP data from CBS Sports
#' @return A \link{data.frame} with the results.
#' @export
cbs_draft <- function(){
  draft_url <- "https://www.cbssports.com/fantasy/football/draft/averages/both/h2h/all"

  draft_page <- read_html(draft_url)

  cbs_ids = draft_page %>%
    html_elements("span.CellPlayerName--short > span > a") %>%
    html_attr("href") %>%
    dirname() %>%
    basename()

  draft_page %>%
    html_element("#TableBase > div > div > table") %>%
    html_table() %>%
    extract(Player, c("player", "pos", "team"), "(.*?)\\n\\s+(.*?)\\s+(.*)") %>%
    transmute(id = player_ids$id[match(cbs_ids, player_ids$cbs_id)],
              cbs_id = cbs_ids,
              player,
              pos,
              team,
              change = as.numeric(replace(Trend, Trend == "—", 0)),
              adp = as.numeric(`Avg Pos`),
              high = as.numeric(sub("/\\d+", "", `Hi/Lo`)),
              low = as.numeric(sub("\\d+/", "", `Hi/Lo`)),
              percent_drafted = Pct)

}

#' Get ADP/AAV data from ESPN
#'
#' This function scrapes ADP and AAV data from ESPN
#' @return A \link{data.frame} with the results. Contains both ADP and AAV
#' @export
espn_draft <- function(){
  draft_url <- "http://games.espn.com/ffl/livedraftresults?position=ALL"

  draft_pge <- read_html(draft_url)

  draft_pge %>% html_node("table.tableBody tr.tableHead") %>% xml_remove()
  draft_pge %>% html_node("table.tableBody tr.tableSubHead") %>% xml_remove()
  draft_pge %>% html_node("table.tableBody tr.tableBody") %>% xml_remove()

  espn_ids <- draft_pge %>% html_nodes("table.tableBody td a.flexpop") %>%
    html_attr("playerid")

  draft_tbl <- draft_pge %>% html_node("table.tableBody") %>% html_table(header = TRUE) %>%
    clean_names() %>%  rename(adp = "avg_pick", aav = "avg_value") %>%
     extract(player_team, c("player", "team"), "([A-Za-z\\.\\s\\-/\\']+)\\*?\\,?\\s?(.*)") %>%
    add_column(espn_id = espn_ids, .before = 1)

  draft_tbl <- add_column(draft_tbl, id = id_col(draft_tbl$espn_id, "espn_id"), .before = 1) %>%
     mutate(adp = as.numeric(adp), aav = as.numeric(aav))

  return(draft_tbl)
}

#' Get ADP/AAV data from Yahoo
#'
#' This function scrapes ADP or AAV data from Yahoo Sports
#' @param aav Indicates whether AAV data is scraped. If set to \code{FALSE} (Default),
#' ADP data is scraped. Set it to \code{TRUE} to scrape AAV data.
#' @return A \link{data.frame} with the results.
#' @export
yahoo_draft <- function(aav = FALSE){
  draft_type <- ifelse(aav, "AD", "SD")

  draft_url <- sprintf("https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=%s&pos=ALL",
                       draft_type)

  draft_col <- ifelse(aav, "Avg Salary",  "Avg Pick")
  names(draft_col) <- ifelse(aav, "aav",  "adp")

  draft_session <- session(draft_url)
  yahoo_adp <- data.frame()
  recode_vals <- c("jac" = "30", "bal" = "33", "lar" = "14", "phi" = "21", "det" = "8",
                       "lac" = "24", "nor" = "18", "sea" = "26", "chi" = "3",  "car" = "29",
                       "pit" = "23", "nwe" = "17",  "kan" = "12", "min" = "16", "dal" = "6",
                       "was" = "28", "den" = "7", "ari" = "22", "ten" = "10", "tam" = "27",
                       "buf" = "2", "cin" = "4", "atl" = "1", "gnb" = "9", "mia" = "15",
                       "ind" = "11", "nyg" = "19",  "hou" = "34", "sfo" = "25", "cle" = "5",
                       "nyj" = "20", "oak" = "13")
  while(nrow(yahoo_adp) < 200) {

    draft_page <- read_html(draft_session)

    draft_tbl <- draft_page %>% html_element("#draftanalysistable") %>% html_table()

    names(draft_tbl) <- gsub("[^[:alnum:]]$", "", names(draft_tbl))

    yahoo_ids <-  draft_session %>%
      html_elements("tbody > tr > td > div > div > span > a") %>%
      html_attr("data-ys-playerid")

    draft_tbl <- draft_tbl %>%
      rename(!!!draft_col) %>%
      add_column(yahoo_id = yahoo_ids, .before = 1)

    if(any(names(draft_tbl) == "aav")) {
      draft_tbl <- draft_tbl %>%
        mutate(aav = as.numeric(gsub("^\\$", "", aav)))
    }

    yahoo_adp <- bind_rows(yahoo_adp, draft_tbl) %>%
      mutate(yahoo_id = recode(yahoo_id, !!!recode_vals))

    next_url <- draft_session %>%
      html_node("a:contains('Next')") %>%
      html_attr("href")

    Sys.sleep(.5)

    if(aav && nrow(yahoo_adp) > 200)
      break

    draft_session <- next_url %>% jump_to(x=draft_session, url =.)
  }

  yahoo_adp <- yahoo_adp %>%
    extract(., Name, c("Note", "Player", "Team", "Pos", "Status/Game/Opp"),
            "\\s*(.+Note[s]*)\\s+(.+)\\s([[:alpha:]]{2,3})\\s\\-\\s([[:alpha:]]{1,3},*[[:alpha:]]*)\\s{2,}(.+)") %>%
    select(., -one_of(c("Note", "Status/Game/Opp"))) %>% clean_names() %>%
    add_column(id = id_col(yahoo_adp$yahoo_id, "stats_id"), .before = 1)

  return(yahoo_adp)
}


#' Get ADP/AAV data from NFL
#'
#' This function scrapes ADP or AAV data from NFL
#' @return A \link{data.frame} with the results. Contains both ADP and AAV data
#' @export
nfl_draft = function() {
year = format(Sys.Date(), format = "%Y")

nfl_url = paste0("https://fantasy.nfl.com/draftcenter/breakdown?leagueId=&offset=1&count=200&position=all&season=",
                 year, "&sort=draftAveragePosition")

html_page = read_html(nfl_url)

nfl_table = html_page %>%
  html_elements("tbody") %>%
  html_table() %>%
  `[[`(1) %>%
  extract(X1, c("player", "pos", "team"), "(.*?)\\s+([A-Z]{2,3}).*?([A-Z]{2,3}).*") %>%
  rename(adp = X2, avg_round = X3, average_salary = X4)

nfl_ids = html_page %>%
  html_elements("tbody > tr > td > div > a") %>%
  html_attr("href") %>%
  unique() %>%
  sub(".*playerId=", "", .)

nfl_table %>%
  mutate(id = player_ids$id[match(nfl_ids, player_ids$nfl_id)],
         nfl_id = nfl_ids)


}




#' Get ADP/AAV data from multple sources
#'
#' This function scrapes ADP or AAV data from multiple sources
#' @param sources Indicates what sources to retrieve data from. Choose from
#' \code{c("RTS", "CBS", "ESPN", "Yahoo", "NFL")}. Multiple sources are allowed.
#' If omitted all sources will be scraped.
#' @param type Specifies what data to collect. Should one of \code{c("ADP", "AAV")}.
#' If omitted then ADP data will be scraped.
#' @return A \link{data.frame} with the results. The player's id from the
#' \code{player_ids} table and a column for each source. The average value is also
#' returned if multiple sources are specified
#' @export
get_adp <- function(sources = c("RTS", "CBS", "ESPN", "Yahoo", "NFL", "FFC"),
                    type = c("ADP", "AAV")){
  type <- match.arg(type)
  sources <- match.arg(sources, several.ok = TRUE)

  draft_type <- tolower(type)

  if("CBS" %in% sources & type == "AAV")
    sources <- setdiff(sources, c("CBS"))
  if("FFC" %in% sources & type == "AAV")
    sources <- setdiff(sources, c("FFC", "ESPN", "FFC", "NFL", "CBS"))

  draft_funs <- list(rts = rts_draft, cbs = cbs_draft, espn = espn_draft,
                     yahoo = yahoo_draft, nfl = nfl_draft, ffc = ffc_draft)

  draft_funs <- draft_funs[tolower(sources)]

  draft_list <- lapply(draft_funs, function(f){
    f_args <- list()

    if("aav" %in% names(formals(f)))
      f_args$aav <- type == "AAV"

    f <- possibly(f, otherwise = tibble())
    tbl <- do.call(f, args = f_args)
    if(nrow(tbl) > 0)
      return(tbl[!is.na(tbl$id),])
  })


  #draft_list <- keep(draft_list, ~ nrow(.x) > 0)
  draft_list <- compact(draft_list)

  draft_funs <- draft_funs[names(draft_list)]

  draft_table <- draft_list[[1]][, c("id", draft_type)]

  if(length(draft_funs) > 1)
    for(src in 2:length(draft_funs)){
      adp_suffix <- paste0("_", names(draft_funs)[(src-1):src])

      draft_table <- full_join(draft_table, draft_list[[src]][, c("id", draft_type)],
                               by = "id", suffix = adp_suffix)
    }

  if(any(names(draft_table) == draft_type)){
    draft_col <- draft_type
    names(draft_col) <- paste(draft_type, names(draft_funs)[length(draft_funs)], sep = "_")
    draft_table <- rename(draft_table, !!!draft_col)
  }

  if(length(sources) > 1){
    draft_table <- draft_table %>%
      mutate(adp_avg = select(draft_table, matches(draft_type)) %>% rowMeans(na.rm = TRUE))

    if(type == "ADP"){
      draft_table <-  arrange(draft_table, adp_avg)
    } else {
      draft_table <-  arrange(draft_table, desc(adp_avg))
    }
  }

  names(draft_table) <-  c("id", names(draft_list), "Avg")[1:length(draft_table)]

  return(distinct(draft_table))
}

#' Get ADP data from Fantasy Football Calculator
#'
#' This function scrapes ADP data from Fantasy Football Calculator
#' @param format Indicates which league format data should be returned for.
#' Should be one of \code{c("standard", "ppr", "2qb", "dynasty", "rookie")}
#' @param pos indicates the position the data should be returned for. Should be
#' one of \code{c("all", "qb", "rb", "wr", "te", "def", "pk")}
#' @param season Indicates the season that data should be retuned for.
#' @return A \link{data.frame} with the results.
#' @export
ffc_draft <- function(format=c("standard", "ppr", "2qb", "dynasty", "rookie"),
                      pos = c("all", "qb", "rb", "wr", "te", "def", "pk")){

  season = format(Sys.Date(), format = "%Y")
  ffc_url <- paste0("https://fantasyfootballcalculator.com/adp?format=standard&year=", season, "&teams=12&view=graph&pos=all")
  format <- match.arg(format)
  pos <- match.arg(pos)


  ffc_url <- modify_url(ffc_url, query = list(format = format, year = season, teams = 12, view = "graph", pos = pos))

  ffc_page <- read_html(ffc_url)
  ffc_page %>% html_elements("table.adp tr th.visible-xs") %>% xml_remove()
  ffc_page %>% html_elements("table.adp tr th:last-child") %>% xml_remove()
  ffc_page %>% html_elements("table.adp tr td:last-child") %>% xml_remove()

  pids <- ffc_page %>% html_elements("table.adp td a") %>% xml_attr("href") %>% str_remove("/players/")
  ffc_page %>% xml_node("table.adp") %>% html_table() %>%
    clean_names() %>% rename(player = "name") %>%
    add_column(fp_id = pids, .before = 1) %>%
    mutate(player = recode(player, "Pat Mahomes" = "Patrick Mahomes"),
           fp_id = recode(fp_id, "ronald-jones" = "ronald-jones-ii",
                          "david-johnson" = "david-johnson-rb",
                          "michael-thomas" = "michael-thomas-wr")) %>%
    add_column(id = ffanalytics:::id_col(.$fp_id, "fantasypro_id"), .before = 1) %>%
    mutate(id = ifelse(is.na(id), ffanalytics:::match_players(.), id)) %>%
    rename(adp = overall)

}
