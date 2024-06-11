#' Get ADP/AAV data from RTSports
#'
#' This function scrapes ADP or AAV data from RTSports
#' @param metric Indicates whether APD or AAV data is scraped. By default, ADP
#' data is scraped. Set it to \code{"aav"} to scrape AAV data.
#' @return A \link{data.frame} with the results.
#' @export
rts_draft <- function(metric = c("adp", "aav")){

  metric = match.arg(tolower(metric), c("adp", "aav"))
  obj_name = paste0("RTS ", toupper(metric))
  is_cached = obj_name %in% list_ffanalytics_cache(quiet = TRUE)$object

  if(is_cached) {
    rts_json = get_cached_object(sprintf("rts_%s.rds", metric))
  } else {
    draft_url = "https://www.freedraftguide.com/football/adp-aav-provider.php?NUM=&STYLE=0&AAV="
    is_aav = (metric == "aav")

    if(is_aav) {
      draft_url = paste0(draft_url, "YES")
    }

    rts_json = httr2::request(draft_url) %>%
      httr2::req_user_agent("ffanalytics R package (https://github.com/FantasyFootballAnalytics/ffanalytics)") %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()

    cache_object(rts_json, sprintf("rts_%s.rds", metric))
  }

  dplyr::bind_rows(rts_json$player_list) %>%
    dplyr::rename(rts_id = player_id) %>%
    dplyr::mutate(bye_week = replace(bye_week, bye_week == '-', NA)) %>%
    dplyr::transmute(
      id = get_mfl_id(rts_id, player_name = name, team = team, pos = position),
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
cbs_draft = function(metric = "adp") {

  is_cached = "CBS ADP" %in% list_ffanalytics_cache(quiet = TRUE)$object

  if(is_cached) {
    out_df = get_cached_object("cbs_adp.rds")
    return(out_df)
  }

  draft_url <- "https://www.cbssports.com/fantasy/football/draft/averages/both/h2h/all"

  draft_page <- rvest::read_html(draft_url)

  cbs_id = draft_page %>%
    rvest::html_elements("span.CellPlayerName--long > span > a") %>%
    rvest::html_attr("href") %>%
    dirname() %>%
    dirname() %>%
    basename()

  out_df = draft_page %>%
    rvest::html_element("#TableBase > div > div > table") %>%
    rvest::html_table() %>%
    tidyr::extract(
      Player, c("player", "pos", "team"),
      "\\n\\s+(.*?)\\n\\s+([A-Z]{1,3})\\s+([A-Z]{2,3})") %>%
    dplyr::transmute(
      id = get_mfl_id(cbs_id, player_name = player, pos = pos, team = team),
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

  cache_object(out_df, "cbs_adp.rds")
  out_df

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
  is_cached = "Yahoo ADP/AAV" %in% list_ffanalytics_cache(TRUE)$object

  if(is_aav) {
    adp_aav_cols = c("aav", "projected_av", "percent_drafted")
  } else {
    adp_aav_cols = c("adp", "percent_drafted")
  }


  if(is_cached) {
    out_df = get_cached_object("yahoo_adp_aav.rds") %>%
      dplyr::select(id, yahoo_id, player_name, team, pos, dplyr::all_of(adp_aav_cols))
    return(out_df)
  }

  req_obj = request("https://pub-api-ro.fantasysports.yahoo.com/fantasy/v2/league/423.l.public;out=settings/players;position=ALL;start=0;count=200;sort=rank_season;search=;out=auction_values;out=expert_ranks;expert_ranks.rank_type=projected_season_remaining/draft_analysis;cut_types=diamond;slices=last7days?format=json_f") %>%
    req_method("GET") %>%
    req_headers(
      Accept = "*/*",
      Host = "pub-api-ro.fantasysports.yahoo.com",
      Origin = "https://football.fantasysports.yahoo.com",
      Connection = "keep-alive",
      `Sec-Fetch-Dest` = "empty",
      `Sec-Fetch-Mode` = "no-cors",
      `Sec-Fetch-Site` = "none",
      `Accept-Encoding` = "gzip, deflate, br",
      `Accept-Language` = "en-US,en;q=0.9"
    ) %>%
    httr2::req_user_agent("ffanalytics R package (https://github.com/FantasyFootballAnalytics/ffanalytics)") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  player_data = req_obj$fantasy_content$league$players %>%
    lapply(function(x) {
    data.frame(
      player_name = x$player$name$full,
      yahoo_id = x$player$player_id,
      team = x$player$editorial_team_abbr,
      pos = x$player$eligible_positions[[1]]$position,
      adp = x$player$draft_analysis$average_pick,
      percent_drafted = x$player$draft_analysis$percent_drafted,
      aav = x$player$draft_analysis$average_cost,
      projected_av = x$player$projected_auction_value
    )
  })



  out_df = data.table::rbindlist(player_data) %>%
    dplyr::tibble() %>%
    dplyr::mutate(dplyr::across(team:projected_av, ~ replace(.x, .x == "-", NA))) %>%
    dplyr::mutate(dplyr::across(team:projected_av, ~ type.convert(.x, as.is = TRUE))) %>%
    dplyr::mutate(
      team = toupper(team),
      team = ifelse(team %in% names(team_corrections), unlist(team_corrections)[team], team),
      stats_id = yahoo_id,
      id = get_mfl_id(stats_id, player_name = player_name, pos = pos, team = team),
      percent_drafted = percent_drafted * 100
      )

  cache_object(out_df, "yahoo_adp_aav.rds")
  out_df %>%
    dplyr::select(id, yahoo_id, player_name, team, pos, dplyr::all_of(adp_aav_cols))

}


#' Get ADP/AAV data from NFL
#'
#' This function scrapes ADP or AAV data from NFL
#' @return A \link{data.frame} with the results. Contains both ADP and AAV data
#' @export
nfl_draft = function(metric = "adp") {
  year = get_scrape_year()

  is_cached = "NFL ADP" %in% list_ffanalytics_cache(quiet = TRUE)$object

  if(is_cached) {
    out_df = get_cached_object("nfl_adp.rds")
    return(out_df)
  }

  nfl_url = paste0("https://fantasy.nfl.com/draftcenter/breakdown?leagueId=&offset=1&count=200&position=all&season=",
                   year, "&sort=draftAveragePosition")

  html_page = rvest::read_html(nfl_url)

  nfl_table = html_page %>%
    rvest::html_elements("tbody") %>%
    rvest::html_table() %>%
    base::`[[`(1) %>%
    extract(X1, c("player", "pos", "team"), "(.*?)\\s+([A-Z]{2,3}).*?([A-Z]{2,3}).*") %>%
    rename(adp = X2, avg_round = X3, average_salary = X4)

  nfl_id = html_page %>%
    html_elements("tbody > tr > td > div > a") %>%
    html_attr("href") %>%
    unique() %>%
    sub(".*playerId=", "", .)

  out_df = nfl_table %>%
    mutate(id = get_mfl_id(nfl_id, player_name = player, pos = pos, team = team),
           nfl_id = !!nfl_id)

  cache_object(out_df, "nfl_adp.rds")
  out_df
}


#' Get ADP or AAV data from MyFantasyLeague
#'
#' This function scrapes ADP or AAV data from MyFantasyLeague. More details on
#' the API available at \link{https://api.myfantasyleague.com/2023/api_info?STATE=details}
#' @param metric Indicated whether to pull ADP (default) or AAV
#' @param period Includes metric for drafts following this time-period
#' @param format Scoring system for receptions
#' @param nteams Number of teams in the league
#' @param is_keeper Whether or not the league is a keeper league and/or a rookie draft
#' @param is_mock Whether or not it is a mock draft
#' @param cutoff Includes players in at least __\% of drafts. Default to 10\% of drafts.
#' @return A \link{data.frame} with the results.
#' @export
mfl_draft = function(metric = c("adp", "aav"),
                     period = c("RECENT", "ALL", "DRAFT", "JUNE", "JULY", "AUG1", "AUG15", "START", "MID", "PLAYOFF"),
                     format = c("All Leagues", "PPR", "Std"),
                     nteams = c(12, 8, 10, 14, 16),
                     is_keeper = c("No", "Keeper", "Rookie Only"),
                     is_mock = c("No", "Mock", "All Leagues"),
                     cutoff = 10) {

  # Todo: clean up the way arguments are input
  metric = match.arg(tolower(metric), c("adp", "aav"))
  is_aav = (metric == "aav")

  period = match.arg(toupper(period), c("RECENT", "ALL", "DRAFT", "JUNE", "JULY", "AUG1", "AUG15", "START", "MID", "PLAYOFF"))
  fcount = match.arg(as.character(nteams), as.character(c(12, 8, 10, 14, 16)))
  format = match.arg(as.character(format), c("All Leagues", "PPR", "Std"))
  is_keeper = match.arg(is_keeper, c("No", "Keeper", "Rookie Only"))
  is_mock = match.arg(as.character(is_mock), c("No", "Mock", "All Leagues"))
  cutoff = as.integer(cutoff)

  # Checking to see if default arguments are used / the result may be cached
  is_cache_format = (
    period == "RECENT"
    && fcount == "12"
    && format == "All Leagues"
    && is_keeper == "No"
    && is_mock == "No"
    && cutoff == "10"
  )

  obj_name = paste0("MFL ", toupper(metric))
  is_cached = obj_name %in% list_ffanalytics_cache(quiet = TRUE)$object

  if(is_cached && is_cache_format) {
    out_df = get_cached_object(sprintf("mfl_%s.rds", metric))
    return(out_df)
  }

  format = switch(format,
    "All Leagues" = -1,
    "PPR" = 1,
    "Std" = 0
  )
  is_keeper = paste0(substr(is_keeper, 1, 1), collapse = "")
  is_mock = switch(is_mock,
    "No" = 0,
    "Mock" = 1,
    "All Leagues" = -1
  )

  if(is_aav) {
    url = sprintf("https://api.myfantasyleague.com/%d/export?TYPE=%s&PERIOD=%s&IS_PPR=%d&IS_KEEPER=%s&JSON=1",
                  get_scrape_year(), metric, period, format, is_keeper)
    cols = setNames(c("id", "averageValue", "minValue", "maxValue", "auctionSelPct"),
                    c("id", "aav", "min_av", "max_av", "draft_percentage"))
  } else {
    url = sprintf("https://api.myfantasyleague.com/%d/export?TYPE=%s&PERIOD=%s&FCOUNT=%s&IS_PPR=%s&IS_KEEPER=%s&IS_MOCK=%s&CUTOFF=%d&DETAILS=&JSON=1",
                  get_scrape_year(), metric, period, fcount, format, is_keeper, is_mock, cutoff)
    cols = setNames(c("id", "averagePick", "minPick", "maxPick", "draftSelPct"),
                    c("id", "adp", "min_dp", "max_dp", "draft_percentage"))
  }

  mfl_json = httr2::request(url) %>%
    httr2::req_user_agent("ffanalytics R package (https://github.com/FantasyFootballAnalytics/ffanalytics)") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  out_df = mfl_json[[metric]]$player %>%
    dplyr::bind_rows() %>%
    dplyr::select(!!!cols) %>%
    type.convert(as.is = TRUE) %>%
    dplyr::mutate(id = as.character(id))

  if(is_aav) { # $1000 split among N franchises (adjusted to ~$200 per team)
    out_df$aav = out_df$aav * (200 / (1000 / as.integer(fcount)))
  }

  if(is_cache_format) {
    cache_object(out_df, sprintf("mfl_%s.rds", metric))
  }
  out_df

}



#' Get ADP data from Fantasy Football Calculator
#'
#' This function scrapes ADP data from Fantasy Football Calculator
#' @param format Indicates which league format data should be returned for.
#' Should be one of \code{c("standard", "ppr", "2qb", "dynasty", "rookie")}
#' @param pos indicates the position the data should be returned for. Should be
#' one of \code{c("all", "qb", "rb", "wr", "te", "def", "pk")}
#' @param season Indicates the season that data should be returned for.
#' @return A \link{data.frame} with the results.
#' @export
ffc_draft <- function(format= c("standard", "ppr", "half-ppr", "2qb", "dynasty", "rookie"),
                      pos = c("all", "qb", "rb", "wr", "te", "def", "pk"),
                      n_teams = c("12", "8", "10", "14"),
                      metric = "adp"){

  format <- match.arg(format, c("standard", "ppr", "half-ppr", "2qb", "dynasty", "rookie"))
  pos <- match.arg(pos, c("all", "qb", "rb", "wr", "te", "def", "pk"))
  n_teams <- match.arg(n_teams, c("12", "8", "10", "14"))

  # Checking to see if default arguments are used / the result may be cached
  is_cache_format = (
    n_teams == "12"
    && format == "standard"
    && pos == "all"
  )

  is_cached = "FFC ADP" %in% list_ffanalytics_cache(TRUE)$object

  if(is_cached && is_cache_format) {
    out_df = get_cached_object("ffc_adp.rds")
    return(out_df)
  }

  ffc_url <- paste0("https://fantasyfootballcalculator.com/api/v1/adp/",
                    format, "?teams=", n_teams, "&year=", get_scrape_year(),
                    "&position=", pos)

  ffc_json = ffc_url %>%
    httr2::request() %>%
    httr2::req_user_agent("ffanalytics R package (https://github.com/FantasyFootballAnalytics/ffanalytics)") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(check_type = FALSE)

  out_df = dplyr::bind_rows(ffc_json$players) %>%
    dplyr::transmute(
      id = get_mfl_id(player_name = name, team = team, pos = position),
      ffc_id = player_id,
      player = name,
      pos = position,
      team = team,
      adp
    )

  if(is_cache_format) {
    cache_object(out_df, "ffc_adp.rds")
  }
  out_df

}


#' Get ADP/AAV data from ESPN
#'
#' This function scrapes ADP or AAV data from ESPN
#' @param metric Indicates whether APD or AAV data is scraped. By default, ADP
#' data is scraped. Set it to \code{"aav"} to scrape AAV data.
#' @return A \link{data.frame} with the results.
#' @export
espn_draft <- function(metric = c("adp", "aav")){

  metric = match.arg(tolower(metric), c("adp", "aav"))
  obj_name = "ESPN ADP/AAV"
  is_cached = obj_name %in% list_ffanalytics_cache(quiet = TRUE)$object
  season = ffanalytics:::get_scrape_year()

  if(is_cached) {
    l_pos = get_cached_object("espn_adp_aav.rds")
  } else {
    slot_nums = c("QB" = 0, "RB" = 2, "WR" = 4, "TE" = 6, "K" = 17, "DST" = 16)
    position = c("QB", "RB", "WR", "TE", "K", "DST")

    l_pos = lapply(position, function(pos){

      if(pos != position[1]) {
        Sys.sleep(2)
      }

      pos_idx = slot_nums[pos]
      limit = dplyr::case_when(
        pos == "QB" ~ 42,
        pos == "RB" ~ 100,
        pos == "WR" ~ 150,
        pos == "TE" ~ 60,
        pos == "K" ~ 35,
        pos == "DST" ~ 32
      )
      base_url = base_url = paste0(
        "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/", season,
        "/segments/0/leaguedefaults/3?scoringPeriodId=0&view=kona_player_info"
      )

      fantasy_filter = paste0(
        '{"players":{',
        '"filterSlotIds":{"value":[', pos_idx, ']},',
        '"filterStatsForSourceIds":{"value":[1]},',
        '"filterStatsForSplitTypeIds":{"value":[', filter_split_id, ']},',
        '"sortAppliedStatTotal":{"sortAsc":false,"sortPriority":3,"value":"11', season, week, '"},',
        '"sortDraftRanks":{"sortPriority":2,"sortAsc":true,"value":"PPR"},',
        '"sortPercOwned":{"sortAsc":false,"sortPriority":4},',
        '"limit":', limit, ',',
        '"offset":0,',
        '"filterRanksForScoringPeriodIds":{"value":[2]},',
        '"filterRanksForRankTypes":{"value":["PPR"]},',
        '"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},',
        '"filterStatsForTopScoringPeriodIds":{"value":2,',
        '"additionalValue":["00', season, '","10', season, '","11', season, week, '","02', season, '"]}}}'
      )

      espn_json = httr2::request(base_url) %>%
        httr2::req_method("GET") %>%
        httr2::req_headers(
          Accept = "application/json",
          `Accept-Encoding` = "gzip, deflate, br",
          Connection = "keep-alive",
          Host = "lm-api-reads.fantasy.espn.com",
          `X-Fantasy-Source` = "kona",
          `X-Fantasy-Filter` = fantasy_filter,
        ) %>%
        httr2::req_user_agent("ffanalytics R package (https://github.com/FantasyFootballAnalytics/ffanalytics)") %>%
        httr2::req_perform() %>%
        httr2::resp_body_json() %>%
        base::`[[`("players")

      l_players = vector("list", length(espn_json))

      for(i in seq_along(espn_json)) {

        # Player ADP/AAV
        keep_cols = c("auctionValueAverage", "averageDraftPosition", "percentOwned")

        l_players[[i]] = espn_json[[i]]$player$ownership[keep_cols]
        l_players[[i]][] = lapply(l_players[[i]], round)

        # Misc player info
        l_players[[i]]$espn_id = espn_json[[i]]$id
        l_players[[i]]$player_name = espn_json[[i]]$player$fullName
        l_players[[i]]$team = espn_team_nums[as.character(espn_json[[i]]$player$proTeamId)]
        l_players[[i]]$position = pos
      }

      out_df = dplyr::bind_rows(l_players)

      if(pos == "DST") { # ESPN ID's coming in as negative for 2023 wk 0 DST
        out_df$id = ffanalytics:::get_mfl_id(
          team = out_df$team,
          pos = out_df$position
        )
      } else {
        out_df$id = ffanalytics:::get_mfl_id(
          out_df$espn_id,
          player_name = out_df$player_name,
          pos = out_df$position
        )
      }

      out_df = out_df %>%
        dplyr::select(id, espn_id, pos = position,
                      player = player_name, team, adp = averageDraftPosition,
                      aav = auctionValueAverage, percent_owned = percentOwned)

      idx = names(out_df) %in% c("id", "espn_id")
      out_df[idx] = lapply(out_df[idx], as.character)
      out_df[!idx] = type.convert(out_df[!idx], as.is = TRUE)
      out_df
    })

    cache_object(l_pos, "espn_adp_aav.rds")
  }

  if(metric == "adp") {
    dplyr::bind_rows(l_pos) %>%
      dplyr::select(-aav) %>%
      dplyr::arrange(.data[[metric]])
  } else {
    dplyr::bind_rows(l_pos) %>%
      dplyr::select(-adp) %>%
      dplyr::arrange(dplyr::desc(.data[[metric]]))
  }
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
get_adp <- function(sources = c("RTS", "CBS", "Yahoo", "NFL", "FFC", "MFL", "ESPN"),
                    metric = c("adp", "aav")) {
  metric <- match.arg(tolower(metric), c("adp", "aav"))
  sources <- match.arg(sources, c("RTS", "CBS", "Yahoo", "NFL", "FFC", "MFL", "ESPN"), several.ok = TRUE)
  is_aav = (metric == "aav")

  if(is_aav) {
    sources = setdiff(sources, c("CBS", "FFC", "NFL"))
  }

  draft_l = lapply(tolower(sources), function(source) {
    tryCatch(
      expr = {
        df = match.fun(paste0(source, "_draft"))(metric = metric)
        df = df[c("id", metric)]
        names(df)[2] = paste0(metric, "_", source)
        df[!is.na(df$id), ]
      },
      error = function(error) {
        message(
          paste0(
            " Error with the ", toupper(source), " ", toupper(metric),
            " data. It is not included in the table or summary columns"
          )
        )
        NULL
      }
    )
  })

  draft_l = Filter(Negate(is.null), draft_l)

  if(length(draft_l) == 0) {
    return(NULL)
  }

  if(length(draft_l) > 1) {
    out = Reduce(function(x, y) dplyr::full_join(x, y, "id"), draft_l)
    out[[paste0(metric, "_avg")]] = rowMeans(out[-1], na.rm = TRUE)
    out[[paste0(metric, "_sd")]] = row_sd(out[-1], na.rm = TRUE)

    if(is_aav) {
      out = out[order(out[[paste0(metric, "_avg")]], decreasing = TRUE), ]
    } else {
      out = out[order(out[[paste0(metric, "_avg")]]), ]
    }
  } else {
    out = draft_l[[1]]
  }
  out
}



