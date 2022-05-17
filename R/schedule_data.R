
get_scrape_year <- function(date) {
  if(missing(date)) {
    date = Sys.Date()
  }
  date = as.POSIXlt(date)
  cal_year = date$year + 1900L
  cal_month = date$mon + 1L

  if(cal_month %in% 1:3) {
    cal_year - 1L
  } else {
    cal_year
  }
}

get_first_last_kickoff <- function(year) {
  if(missing(year)) {
    year = get_scrape_year()
  }
  if(!is.numeric(year)) {
    stop("year must be numeric")
  }

  schedule_url = sprintf("https://api.myfantasyleague.com/%d/export?TYPE=nflSchedule&W=ALL&JSON=1",
                         year)

  req_body = httr2::request(schedule_url) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  req_schedule = req_body[[c("fullNflSchedule", "nflSchedule")]]
  req_schedule = req_schedule[sapply(req_schedule, function(x) any(names(x) == "matchup"))]
  names(req_schedule) = paste0("week_", seq_len(length(req_schedule)))
  first_last_games = lapply(req_schedule, function(x) {
    kick_unixtimes = sapply(x$matchup, `[[`, "kickoff")
    kick_unixtimes = as.integer(kick_unixtimes)
    first_last = c(first = min(kick_unixtimes), last = max(kick_unixtimes))
    as.POSIXct(first_last, origin = "1970-01-01")
  })
  first_last_games

}

get_scrape_starts = function(first_last_games) {
  if(missing(first_last_games)) {
    first_last_games = get_first_last_kickoff()
  }
  first_last_df = dplyr::bind_rows(first_last_games)
  last_vec = first_last_df$last
  last_vec = as.Date(substr(last_vec, 1, 10)) + 1L
  last_vec = dplyr::lag(last_vec)
  last_vec[1] = as.Date(first_last_df$first[1]) - 7L
  last_vec[19:22] = last_vec[18] + c(7, 14, 21, 35)
  last_vec
}
get_scrape_week = function(scrape_start_date) {
  if(missing(scrape_start_date)) {
    scrape_start_date = get_scrape_starts()
  }
  sum(Sys.Date() >= scrape_start_date)
}

first_last_games = get_first_last_kickoff()
scrape_start_date = get_scrape_starts(first_last_games)


# rrapply::rrapply(req_body,
#                 function(x, .xname) .xname == "kickoff",
#                 how = "prune")


