#' Get ADP/AAV data from RTSports
#'
#' This function scrapes ADP or AAV data from RTSports
#' @param metric Indicates whether APD or AAV data is scraped. By default, ADP
#' data is scraped. Set it to \code{"aav"} to scrape AAV data.
#' @return A \link{data.frame} with the results.
#' @export
rts_draft <- function(metric = c("adp", "aav")){

  metric = match.arg(tolower(metric), c("adp", "aav"))
  is_aav = (metric == "aav")

  draft_url = "https://www.freedraftguide.com/football/adp-aav-provider.php?NUM=&STYLE=0&AAV="
  if(is_aav) {
    draft_url = paste0(draft_url, "YES")
  }

  rts_json = httr2::request(draft_url) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  dplyr::bind_rows(rts_json$player_list) %>%
    dplyr::rename(rts_id = player_id) %>%
    dplyr::transmute(
      id = get_mfl_id(rts_id),
      rts_id,
      !!metric := as.numeric(avg),
      name,
      team,
      position,
      bye_week = as.integer(bye_week),
      change = if(metric == "adp") {change} else {NULL}
    )

}

#' Get ADP data from CBS
#'
#' This function scrapes ADP data from CBS Sports
#' @return A \link{data.frame} with the results.
#' @export
cbs_draft <- function(metric = "adp") {
  draft_url <- "https://www.cbssports.com/fantasy/football/draft/averages/both/h2h/all"

  draft_page <- rvest::read_html(draft_url)

  cbs_id = draft_page %>%
    rvest::html_elements("span.CellPlayerName--short > span > a") %>%
    rvest::html_attr("href") %>%
    dirname() %>%
    basename()

  draft_page %>%
    rvest::html_element("#TableBase > div > div > table") %>%
    rvest::html_table() %>%
    tidyr::extract(Player, c("player", "pos", "team"), "(.*?)\\n\\s+(.*?)\\s+(.*)") %>%
    dplyr::transmute(
      id = get_mfl_id(cbs_id),
      cbs_id = cbs_id,
      player,
      pos,
      team,
      change = as.integer(replace(Trend, Trend == "—", 0)),
      adp = as.numeric(`Avg Pos`),
      high_adp = as.integer(sub("/\\d+", "", `Hi/Lo`)),
      low_adp = as.integer(sub("\\d+/", "", `Hi/Lo`)),
      percent_drafted = Pct
      )

}

#' Get ADP/AAV data from Yahoo
#'
#' This function scrapes ADP or AAV data from Yahoo Sports
#' @param metric Indicates whether ADP or AAV data is scraped. By default, ADP
#' data is scraped. Set it to \code{"aav"} to scrape AAV data.
#' @return A \link{data.frame} with the results.
#' @export
yahoo_draft = function(metric = c("adp", "aav")) {
  metric = match.arg(tolower(metric), c("adp", "aav"))
  is_aav = (metric == "aav")

  draft_url <- sprintf("https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=%s&pos=ALL",
                       if(is_aav) "AD" else "SD")
  html_session = rvest::session(draft_url)

  max_pages = 4 + (as.integer(is_aav) * 4)
  l_yahoo = vector("list", max_pages)
  i = 0

  while(i < max_pages) {
    next_page = paste0(html_session$url, "&count=", i * 50)
    i = i + 1

    html_page = html_session %>%
      rvest::session_jump_to(next_page) %>%
      rvest::read_html()

    yahoo_id = html_page %>%
      html_elements("table > tbody > tr > td > div > div > span > a") %>%
      html_attr("data-ys-playerid")

    yahoo_tbl = html_page %>%
      rvest::html_element("table") %>%
      rvest::html_table() %>%
      dplyr::mutate(stats_id = yahoo_id)

    l_yahoo[[i]] = yahoo_tbl

    Sys.sleep(1)
  }

  output_df = dplyr::bind_rows(l_yahoo) %>%
    dplyr::mutate(Name = sapply(strsplit(Name, "\\s*\n\\s*"), `[`, 2)) %>%
    tidyr::extract(Name, c("player", "team", "pos"), "(.*?)\\s+([A-Za-z]+)\\s+-\\s+([A-Z]+)$")
  names(output_df) = gsub("[^[:alnum:]]$", "", names(output_df))

  if(is_aav) {
    output_df %>%
      dplyr::transmute(
        id = get_mfl_id(stats_id),
        yahoo_id = stats_id,
        player,
        team,
        pos,
        aav = as.numeric(gsub("[^[:digit:].]*", "", `Avg Salary`)),
        percent_drafted = as.numeric(sub("%", "", `Percent Drafted`))
      )
  } else {
    output_df %>%
      dplyr::transmute(
        id = get_mfl_id(stats_id),
        yahoo_id = stats_id,
        player,
        team,
        pos,
        adp = `Avg Pick`,
        percent_drafted = as.numeric(sub("%", "", `Percent Drafted`, fixed = TRUE))
      )
  }
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


mfl_draft = function(metric = c("adp", "aav")) {
  metric = match.arg(tolower(metric), c("adp", "aav"))
  is_aav = (metric == "aav")


  if(is_aav) {
    url = sprintf("https://api.myfantasyleague.com/%d/export?TYPE=aav&PERIOD=RECENT&IS_PPR=-1&IS_KEEPER=N&JSON=1",
                  get_scrape_year())
    cols = setNames(c("id", "averageValue", "minValue", "maxValue", "auctionSelPct"),
                    c("id", "aav", "min_aav", "max_aav", "draft_percentage"))
  } else {
    url = paste0(sprintf("https://api.myfantasyleague.com/%d/export?TYPE=adp", get_scrape_year()),
                 "&PERIOD=RECENT&FCOUNT=12&IS_PPR=-1&IS_KEEPER=N&IS_MOCK=0&CUTOFF=10&DETAILS=&JSON=1")
    cols = setNames(c("id", "averagePick", "minPick", "maxPick", "draftSelPct"),
                    c("id", "adp", "min_adp", "max_adp", "draft_percentage"))
  }

  mfl_json = httr2::request(url) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  out = mfl_json[[metric]]$player %>%
    dplyr::bind_rows() %>%
    dplyr::select(!!!cols) %>%
    type.convert(as.is = TRUE) %>%
    dplyr::mutate(id = as.character(id))

  if(is_aav) { # $1000 split among 12 franchises (adjusted to ~$200 per team)
    out$aav = out$aav * (200 / (1000 / 12))
  }
  out

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
ffc_draft <- function(format= c("standard", "ppr", "half-ppr", "2qb", "dynasty", "rookie"),
                      pos = c("all", "qb", "rb", "wr", "te", "def", "pk"),
                      n_teams = c("12", "8", "10", "14"),
                      metric = "adp"){

  format <- match.arg(format, c("standard", "ppr", "half-ppr", "2qb", "dynasty", "rookie"))
  pos <- match.arg(pos, c("all", "qb", "rb", "wr", "te", "def", "pk"))
  n_teams <- match.arg(n_teams, c("12", "8", "10", "14"))

  ffc_url <- paste0("https://fantasyfootballcalculator.com/api/v1/adp/",
                    format, "?teams=", n_teams, "&year=", get_scrape_year(),
                    "&position=", pos)

  ffc_json = ffc_url %>%
    httr2::request() %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(check_type = FALSE)

  dplyr::bind_rows(ffc_json$players) %>%
    dplyr::transmute(
      id = get_mfl_id(player_name = name, team = team, pos = position),
      ffc_id = player_id,
      player = name,
      pos = position,
      team = team,
      adp
    )

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
get_adp <- function(sources = c("RTS", "CBS", "Yahoo", "NFL", "FFC", "MFL"),
                    metric = c("adp", "aav")) {
  metric <- match.arg(tolower(metric), c("adp", "aav"))
  sources <- match.arg(sources, c("RTS", "CBS", "Yahoo", "NFL", "FFC", "MFL"), several.ok = TRUE)


  if(metric == "aav") {
    sources = setdiff(sources, c("CBS", "FFC"))
  }

  draft_l = lapply(tolower(sources), function(source) {
    df = match.fun(paste0(source, "_draft"))(metric = metric)
    df = df[c("id", metric)]
    names(df)[2] = paste0(metric, "_", source)
    df
  })
  draft_l = Filter(Negate(is.null), draft_l)

  if(length(draft_l) > 1) {
    out = Reduce(function(x, y) dplyr::full_join(x, y, "id"), draft_l)
    out[[paste0(metric, "_avg")]] = rowMeans(out[-1], na.rm = TRUE)

    if(metric == "aav") {
      out = out[order(out[[paste0(metric, "_avg")]], decreasing = TRUE), ]
    } else {
      out = out[order(out[[paste0(metric, "_avg")]]), ]
    }
  }

}





