#' Scrape data from multiple sources and multiple positions
#'
#' This function scrapes data from multiple sources and multiple positions and
#' returns a list of \link{tibble}s with the results. Results contain raw data
#' from the sources.
#'
#' @param src the sources that data should be scraped from should be one or more
#' of \code{c("CBS", "ESPN", "FantasyData", "FantasyPros", "FantasySharks",
#' "FFToday", "FleaFlicker", "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL",
#' "RTSports","Walterfootball")}
#' @param pos the posistions that data should be scraped for. Should be one or more
#' of \code{c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB")}
#' @param season The seaon for which data should be scraped. Should be set to the
#' current season.
#' @param week The week for which data should be scraped. Set to 0 to get season
#' data.
#' @export
scrape_data <- function(
  src = c("CBS", "ESPN", "FantasyData", "FantasyPros", "FantasySharks", "FFToday",
          "FleaFlicker", "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL",
          "RTSports","Walterfootball"),
  pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
  season = 2021, week = 0,
  ...){

  # For now, unnamed argument on whether or not to impute the walterfootball
  # reg_tds column (into respective rush_tds and rec_tds)
  if(!exists("walterftb_impute_reg") || !is.logical(walterftb_impute_reg)) {
    walterftb_impute_reg = TRUE
  }

  if(missing(week)) {
    week <- 0
  }

  # Temp fix for fantasy pros until we make scrape self-contained
  projection_sources$FantasyPros$get_path = function(season, week, position) {
    if(week %in% 1:17) {
      paste0(tolower(position), ".php?week=", week)
    } else {
      paste0(tolower(position), ".php")
    }
  }



  src <- match.arg(src, several.ok = TRUE,
                   c("CBS", "ESPN", "FantasyData", "FantasyPros", "FantasySharks", "FFToday",
                     "FleaFlicker", "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL",
                     "RTSports","Walterfootball"))
  src_selfcont = intersect(src, c("NFL", "CBS", "FantasySharks", "NumberFire", "Walterfootball"))
  src = setdiff(src, src_selfcont)
  pos <- match.arg(pos, several.ok = TRUE,
                   c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"))

  if(any(src == "NumberFire") & any(c("DL", "LB", "DB") %in% pos))
    # pos <- c(pos, "IDP") # temporary, until I redo scrapes

  if(any(src == "FleaFlicker") & "DL" %in% pos)
    pos <- c(pos, "DE", "DT")

  if(any(src == "FleaFlicker") & "DB" %in% pos)
    pos <- c(pos, "CB", "S")

  names(pos) <- pos
  src_data <- map(pos, ~ map(projection_sources[src], ~ .x)) %>% purrr::transpose() %>%
    map( ~ imap(.x, ~ scrape_source(.x, season, week, .y))) %>%
    purrr::transpose() %>% map(compact) %>% map(bind_rows, .id = "data_src")

  if(any(names(src_data) == "IDP")){
    idp_data <- filter(src_data$IDP, data_src == "NumberFire") %>%
      split(.$pos)
    for(p in names(idp_data)){
      src_data[[p]] <- bind_rows(list(src_data[[p]], idp_data[[p]]))
    }
  }


  if(any(names(src_data) == "CB")){
    src_data$DB <- bind_rows(src_data$DB, filter(src_data$CB, data_src == "FleaFlicker"))
  }

  if(any(names(src_data) == "S")){
    src_data$DB <- bind_rows(src_data$DB, filter(src_data$S, data_src == "FleaFlicker"))
  }

  if(any(names(src_data) == "DE")){
    src_data$DL <- bind_rows(src_data$DL, filter(src_data$DE, data_src == "FleaFlicker"))
  }

  if(any(names(src_data) == "DT")){
    src_data$DL <- bind_rows(src_data$DL, filter(src_data$DT, data_src == "FleaFlicker"))
  }

  src_data <- map(src_data,
                  ~ {if(any(names(.x) == "site_src")){
                       mutate(.x, data_src = if_else(is.na(site_src), data_src, paste(data_src, site_src, sep = ": ")))
                  } else {
                      .x
                    }}
  )

  # Running the sefl-contained scrapes (temporary, untill new scrapes are finalized)
  l_selfcont = lapply(src_selfcont, function(self_src) {
    scrape_fun = get(paste0("scrape_", tolower(self_src)), mode = "function", envir = asNamespace("ffanalytics"))
    fun_formals = formals(scrape_fun)

    if(week == 0 && !fun_formals$draft) {
      message(paste0("Draft data not available for ", self_src))
      return(NULL)
    }
    if(week > 0 && !fun_formals$weekly) {
      message(paste0("Weekly data not available for ", self_src))
      return(NULL)
    }

    scrape = scrape_fun(pos = intersect(pos, as.character(fun_formals$pos)[-1]),
                        season = season,
                        week = week)
    lapply(scrape, function(x) Filter(function(j) any(!is.na(j)), x)) # remove all NA columns
  })

  # Merging the scrapes
  scraped_positions = unique(unlist(lapply(l_selfcont, names)))
  l_out = vector("list", length(scraped_positions))
  names(l_out) = scraped_positions
  for(scr_pos in names(src_data)) {
    l_out[[scr_pos]] = bind_rows(l_out[[scr_pos]], src_data[[scr_pos]])
  }
  for(self_scr in l_selfcont) {
    for(scr_pos in names(self_scr)) {
      l_out[[scr_pos]] = bind_rows(l_out[[scr_pos]], self_scr[[scr_pos]])
    }
  }

  l_out <- l_out[setdiff(pos, c("IDP", "CB", "S", "DT", "DE"))]
  l_out = Filter(Negate(is.null), l_out)
  attr(l_out, "season") <- season
  attr(l_out, "week") <- week

  l_out
}

#' Scrape data for a specific position from a single source
#' @export
scrape_source <- function(src, season, week, position){
  src_type <- intersect(c("html_source", "json_source", "xlsx_source"), class(src))
  cat("Scraping", position, "projections from \n", src$get_url(season, week, position), "\n")
  src_res <- switch(src_type,
                    "html_source" = src$open_session(season, week, position)$scrape(),
                    src$scrape(season, week, position))
  return(src_res)
}




