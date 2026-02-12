#' Scrape data from multiple sources and multiple positions
#'
#' This function scrapes data from multiple sources and multiple positions and
#' returns a list of \link{tibble}s with the results. Results contain raw data
#' from the sources.
#'
#' @param src the sources that data should be scraped from should be one or more
#' of \code{c("CBS", "ESPN", "FantasyData", "FantasyPros", "FantasySharks",
#' "FFToday", "FleaFlicker", "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL",
#' "RTSports","Walterfootball", "FanDuel")}
#' @param pos the posistions that data should be scraped for. Should be one or more
#' of \code{c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB")}
#' @param season The seaon for which data should be scraped. Should be set to the
#' current season.
#' @param week The week for which data should be scraped. Set to 0 to get season
#' data.
#' @export
scrape_data <- function(
  src = c("CBS", "ESPN", "FantasyPros", "FantasySharks", "FFToday",
          "FleaFlicker", "NumberFire", "FantasyFootballNerd", "NFL",
          "RTSports", "Walterfootball", "FanDuel"),
  pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
  season = NULL, week = NULL, ...){

  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }

  src = match.arg(src, several.ok = TRUE,
                  c("CBS", "ESPN", "FantasyData", "FantasyPros", "FantasySharks", "FFToday",
                    "FleaFlicker", "NumberFire", "FantasyFootballNerd", "NFL",
                    "RTSports", "Walterfootball", "FanDuel")
                  )

  # Check for NumberFire in src and convert to FanDuel
  if("NumberFire" %in% src) {
    message("\nHeads up! NumberFire is now FanDuel... Using FanDuel for scrape")

    src <- src[src != "NumberFire"]

    if(!("FanDuel" %in% src)) {
      src <- c(src, "FanDuel")
    }
  }

  pos = match.arg(pos, several.ok = TRUE,
                  c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"))

  args = list(pos = pos, season = season, week = week)
  additional_args = list(...)

  if(!"espn_league_id" %in% names(additional_args)) {
    additional_args[["espn_league_id"]] = 1595759
  }
  args = c(args, additional_args)
  args = args[nchar(names(args)) > 0]


  # Running the sefl-contained scrapes (temporary, untill new scrapes are finalized)
  l_selfcont = lapply(src, function(self_src) {
    scrape_fun = get(paste0("scrape_", tolower(self_src)), mode = "function", envir = asNamespace("ffanalytics"))
    fun_formals = formals(scrape_fun)
    fun_args = args[names(args) %in% names(fun_formals)]
    fun_args$pos = intersect(fun_args$pos, eval(fun_formals$pos))

    if(week == 0 && !fun_formals$draft) {
      message(paste0("\nDraft data not available for ", self_src))
      return(NULL)
    }
    if(week > 0 && !fun_formals$weekly) {
      message(paste0("\nWeekly data not available for ", self_src))
      return(NULL)
    }

    scrape = tryCatch(
      expr = {
        do.call(scrape_fun, fun_args)
        },
      error = function(error) {
        message(
          paste0(
            " Uh oh! Error with the ", self_src, " scrape.\n",
            " To get a more specific error message, run:\n",
            "   ffanalytics:::scrape_", tolower(self_src), "(",
            "pos = ", capture.output(dput(pos)),
            ", season = ", season, ", week = ", week, ")"
            )
          )
        NULL
        }
      )

    lapply(scrape, function(x) Filter(function(j) any(!is.na(j)), x)) # remove all NA columns
  })

  all_positions = unique(unlist(lapply(l_selfcont, names)))
  l_out = vector("list", length(all_positions))
  names(l_out) = all_positions

  for(self_scr in l_selfcont) {
    for(scr_pos in names(self_scr)) {
      l_out[[scr_pos]] = bind_rows(l_out[[scr_pos]], self_scr[[scr_pos]])
    }
  }

  l_out = l_out[setdiff(pos, c("IDP", "CB", "S", "DT", "DE"))]
  l_out = Filter(Negate(is.null), l_out)
  attr(l_out, "season") = season
  attr(l_out, "week") = week

  l_out
}



