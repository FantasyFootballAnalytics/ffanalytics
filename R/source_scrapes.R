
get_url = function(season, week = NULL, position , ...){
  if(!(week %in% self$min_week:self$max_week))
    return(NULL)

  if(is.null(week) || week == 0)
    allowed_pos <- self$season_pos
  else
    allowed_pos <- self$week_pos

  if(!(position %in% allowed_pos))
    return(NULL)

  if(is.null(self$url_positions) || is.null(self$url_positions(position)))
    p_id <- position
  else
    p_id <- self$url_positions(position)

  if(private$data_host() %in% c("www.numberfire.com", "api.fantasy.nfl.com", "www.fantasyfootballnerd.com"))
    position <- p_id


  full_url <- self$base
  cur_path <- httr::parse_url(full_url)$path
  cur_query <- httr::parse_url(full_url)$query

  if(!is.null(self$get_path) && !is.null(self$get_path(season, week, position))){
    u_path <- self$get_path(season, week, position)
    new_path <- paste0(cur_path, u_path)

    if(private$data_host()=="www.fantasyfootballnerd.com")
      new_path <- glue::glue(new_path, api_key = self$api_key)

    full_url <- httr::modify_url(full_url, path = new_path)
  } else if(private$data_host()=="football.fantasysports.yahoo.com") {
    new_path <- paste0(cur_path, paste(self$league_id, "players", sep = "/"))
    full_url <- httr::modify_url(full_url, path = new_path)
  }

  if(!is.null(self$get_query) && !is.null(self$get_query(season, week, p_id, ...))){
    new_query <- self$get_query(season, week, p_id, ...)
    full_url <- httr::modify_url(full_url, query = new_query)
  }

  return(full_url)
}


open_session = function(season, week = NULL, position , ...){
  session_url <- self$get_url(season, week, position , ...)
  if(is.null(session_url)){
    self$close_session()
    return(invisible(self))
  }

  if(private$data_host() == "www.fantasysharks.com"){
    private$session <- session_url
  } else {
    src_session <- session(session_url)
    src_session[c("season", "week", "position")] <- list(season, week, position)
    private$session <- src_session
  }
  invisible(self)
}




# Starting to make new self-contained scrapes
scrape_cbs <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
                       season = 2020,
                       week = 0) {






}




