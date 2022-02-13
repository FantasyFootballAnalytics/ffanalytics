
schedule_url <- "https://api.myfantasyleague.com/2021/export?TYPE=nflSchedule&W=ALL&JSON=1"

schedule_data <- httr::GET(schedule_url) %>%
  httr::content() %>%
  .[[c("fullNflSchedule", "nflSchedule")]] %>%
  purrr:::keep(~ "matchup" %in% names(.))

names(schedule_data) <- paste0("week_", seq_len(length(schedule_data)))

schedule_data <- purrr::map(schedule_data, ~ `names<-`(.x$matchup, paste0("match_", seq_along(.x$matchup))))

schedule_data$week_22$match_1$kickoff = schedule_data$week_22$match_1
schedule_data$week_22$match_2 = NULL
schedule_data$week_22$match_3 = NULL

# The nesting structure does not work well with the superbowl, manually added below as "last" game
first_last_games <- purrr::modify_depth(schedule_data, 2, `[[`, "kickoff") %>%
  purrr::map(unlist, use.name = FALSE) %>%
  purrr::map(as.numeric) %>%
  purrr::map(summary) %>%
  purrr::map(`[`, c("Min.", "Max.")) %>%
  purrr::map(~ as.POSIXct(as.numeric(.x), origin = "1970-01-01")) %>%
  purrr::map(`names<-`, c("first", "last"))

scrape_start_date <- first_last_games %>%
  purrr::map_chr(`[[`, "last") %>%
  lag %>%
  as.numeric() %>%
  as.POSIXct(origin = "1970-01-01") %>%
  as.Date()

scrape_start_date[1] <- as.Date(format(first_last_games$week_1[["first"]], format = "%Y-%m-%d")) - 7L

scrape_start_date[19:22] <- scrape_start_date[18] + c(7, 14, 21, 35)

scrape_week <- function(){
  length(which(Sys.Date() >= scrape_start_date))
  }
