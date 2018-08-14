schedule_url <- "https://www70.myfantasyleague.com/2018/export?TYPE=nflSchedule&W=ALL&JSON=1"

schedule_data <- schedule_url %>% GET() %>% content() %>%
  .[[c("fullNflSchedule", "nflSchedule")]] %>% .[1:17]

names(schedule_data) <- paste0("week_", 1:17)

schedule_data <- schedule_data %>%
  purrr::map(~ `names<-`(.x$matchup, paste0("match_", seq_along(.x$matchup))))


first_last_games <- schedule_data %>% purrr::modify_depth(2, `[[`, "kickoff") %>%
  purrr::map(unlist, use.name = FALSE) %>% purrr::map(as.numeric) %>% purrr::map(summary) %>%
  purrr::map(`[`, c("Min.", "Max.")) %>%
  purrr::map(~ as.POSIXct(as.numeric(.x), origin = "1970-01-01")) %>%
  purrr::map(`names<-`, c("first", "last"))



scrape_start_date <- first_last_games %>% purrr::map_chr(`[[`, "last") %>% lag %>% as.numeric() %>%
  as.POSIXct(origin = "1970-01-01") %>% as.Date() %>% `+`(1)

scrape_start_date[1] <-  first_last_games %>% purrr::map_chr(`[[`, "first") %>% min %>% as.numeric() %>%
  as.POSIXct(origin = "1970-01-01") %>% as.Date() %>% `-`(7)


scrape_week <- function(){which(Sys.Date() >= scrape_start_date) %>% length()}
