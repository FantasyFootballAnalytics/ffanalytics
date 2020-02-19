#' @import tidyverse httr janitor rvest glue
#' @importFrom lubridate year
.onLoad <- function(libname, pkgname){
  player_table <<- httr::GET("https://api.myfantasyleague.com/2019/export?TYPE=players&DETAILS=1&SINCE=&PLAYERS=&JSON=1") %>%
    httr::content() %>% `[[`("players") %>% `[[`("player") %>%
    purrr::map(tibble::as.tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(position %in% c("QB", "RB", "WR", "TE", "PK", "Def", "DE", "DT", "LB", "CB", "S")) %>%
    dplyr::select(id, name, position, team, weight, draft_year, draft_team, draft_round, draft_pick, birthdate) %>%
    tidyr::extract(name, c("last_name", "first_name"), "(.+),\\s(.+)") %>%
    dplyr::mutate(birthdate = as.Date(as.POSIXct(as.numeric(birthdate), origin = "1970-01-01")),
                  position = dplyr::recode(position, Def = "DST", PK = "K"),
                  age = as.integer(lubridate::year(Sys.time()) - lubridate::year(birthdate)),
                  exp = 2019 - as.integer(draft_year))
}
