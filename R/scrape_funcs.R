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
  season = 2021, week = 0){

  if(missing(week))
    week <- 0
  src <- match.arg(src, several.ok = TRUE)
  pos <- match.arg(pos, several.ok = TRUE)

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

  src_data <- src_data[setdiff(pos, c("IDP", "CB", "S", "DT", "DE"))]
  attr(src_data, "season") <- season
  attr(src_data, "week") <- week

  return(src_data)
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




