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
  draft_url <- "https://www.cbssports.com/fantasy/football/draft/averages?&print_rows=9999"

  draft_pge <- read_html(draft_url)

  draft_pge %>% html_node("table.data tr.title") %>% xml_remove()
  draft_pge %>% html_node("table.data tr.footer") %>% xml_remove()

  cbs_ids <- draft_pge %>% html_nodes("table.data tr td a") %>% html_attr("href") %>%
    str_extract_all(pattern = "[0-9]{3,}") %>% unlist(use.names = FALSE)

  draft_tbl <- draft_pge %>% html_node("table.data") %>% html_table(header = TRUE) %>%
    extract(Player, c("Player", "Pos", "Team"), "(.+)\\,\\s(.+)\\,\\s(.+)") %>%
    extract("HI/LO", c("Best", "Worst"), "([0-9]+)/([0-9]+)") %>%
    rename(adp = "Avg Pos") %>% add_column(cbs_id = cbs_ids, .before = 1)

  draft_tbl <- add_column(draft_tbl, id = id_col(draft_tbl$cbs_id, "cbs_id"), .before = 1) %>%
    clean_names() %>% mutate(adp = as.numeric(adp))

  return(draft_tbl)
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

  draft_col <- ifelse(aav, "Avg Cost",  "Avg Pick")
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
  repeat({

    draft_page <- read_html(draft_session)

    draft_tbl <- draft_page %>% html_node("#draftanalysistable") %>% html_table()

    names(draft_tbl) <- gsub("[^[:alnum:]]$", "", names(draft_tbl))

    yahoo_ids <-  draft_session %>%
      html_nodes("a[href *= 'nfl/players']:not(a[class *='playernote']), a[href *= 'nfl/teams']:not(a[class *='playernote'])") %>%
      html_attr("href") %>%
      basename()

    draft_tbl <- draft_tbl %>%
      rename(!!!draft_col) %>% add_column(yahoo_id = yahoo_ids, .before = 1)

    if(any(names(draft_tbl) == "aav"))
      draft_tbl <- draft_tbl %>% mutate(aav = as.numeric(gsub("^\\$", "", aav)))

    yahoo_adp <- bind_rows(yahoo_adp, draft_tbl) %>%
      mutate(yahoo_id = recode(yahoo_id, !!!recode_vals))

    next_url <- draft_session %>%
      html_node("a:contains('Next')") %>%
      html_attr("href")

    if(is.na(next_url))
      break

    draft_session <- next_url %>% jump_to(x=draft_session, url =.)
  })
  yahoo_adp <- yahoo_adp %>%
    extract(., Name, c("Note", "Player", "Team", "Pos", "Status/Game/Opp"),
            "\\s*(.+Note[s]*)\\s+(.+)\\s([[:alpha:]]{2,3})\\s\\-\\s([[:alpha:]]{1,3},*[[:alpha:]]*)\\s{2,}(.+)") %>%
    select(., -one_of(c("Note", "Status/Game/Opp"))) %>% clean_names() %>%
    add_column(id = id_col(yahoo_adp$yahoo_id, "stats_id"), .before = 1)

  return(yahoo_adp)
}


p_url <- function(p, season = 2018, weekNo = 0){
  nfl_base <- "http://api.fantasy.nfl.com/v1/players/researchinfo"
  nfl_query <- list(season = season, week = weekNo, count = 1000, format = "json", position = p)
  u <- parse_url(nfl_base)
  u$query <- nfl_query
  build_url(u)
}

url_data <- function(u){
  p <- 0
  out_tbl <- tibble()
  repeat{
    uq <- parse_url(u)$query
    uq$offset <- p
    u <- modify_url(u, query = uq)

    u_data <- content(GET(u))
    if(length(u_data$players) == 0)
      break
    else {
      p_data <- u_data$players %>%
        map(`[`, c("id", "esbid")) %>%
        modify_depth(2, ~ if_else(is.null(.x), as.character(NA), .x )) %>%
        map(as_tibble) %>%
        bind_rows() %>%
        rename(nfl_id = id)
      out_tbl <- bind_rows(out_tbl, p_data)
    }
    p <- p + 1000
  }
  out_tbl
}

#' Get ADP/AAV data from NFL
#'
#' This function scrapes ADP or AAV data from NFL
#' @return A \link{data.frame} with the results. Contains both ADP and AAV data
#' @export
nfl_draft <- function(){

  api_url <- "http://api.fantasy.nfl.com/v1/players/userdraftranks?format=json&count=100&offset=0"
  draft_tbl <- tibble()

  repeat({
    nfl_tbl <- content(GET(api_url))$players %>%
      map(`[`, c("esbid", "rank", "aav", "teamAbbr", "position")) %>%
      modify_depth(2, ~ if_else(is.null(.x), as.character(NA), .x )) %>%
      map(as_tibble) %>% bind_rows()

    if(nrow(nfl_tbl) == 0)
      break

    draft_tbl <- bind_rows(draft_tbl, nfl_tbl)
    api_url <- parse_url(api_url)

    api_qry <- api_url$query
    api_qry$offset <- as.integer(api_qry$offset) + 100

    api_url <- modify_url(api_url, query = api_qry)

  })


  if(nrow(draft_tbl) > 0){

    draft_pos <- unique(draft_tbl$position)

    p_esbid <- draft_pos %>% map(p_url) %>% map(url_data) %>% bind_rows()

    draft_tbl <- draft_tbl %>% rename(adp = rank, team = teamAbbr) %>%
      add_column(nfl_id = p_esbid$nfl_id[match(draft_tbl$esbid, p_esbid$esbid)], .before = 1)

    draft_tbl <- draft_tbl %>% rowwise() %>%
      mutate(nfl_id = ifelse(position == "DEF", nflTeam.id[which(nflTeam.abb == team)], nfl_id)) %>%
      ungroup()

    draft_tbl <- draft_tbl %>%
      add_column(id = id_col(draft_tbl$nfl_id, "nfl_id"), .before = 1) %>%
      mutate(adp = as.numeric(adp), aav = as.numeric(aav))
  }
  return(draft_tbl)
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
    sources <- setdiff(sources, "CBS")
  if("FFC" %in% sources & type == "AAV")
    sources <- setdiff(sources, "FFC")

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
                      pos = c("all", "qb", "rb", "wr", "te", "def", "pk"),
                              season=2019){

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
