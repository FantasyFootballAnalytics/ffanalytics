#' @import dplyr tibble tidyr purrr stringr httr janitor rvest glue
#' @importFrom lubridate year
#' @importFrom data.table fread
.onLoad <- function(libname, pkgname){
  player_table <<- dplyr::as_tibble(
    data.table::fread("https://s3.us-east-2.amazonaws.com/ffanalytics/packagedata/player_table.csv",
                      colClasses = c("character", "character", "character", "character", "character",
                                     "integer", "integer", "character", "integer", "integer",
                                     "Date", "integer", "integer"),
                      col.names = c("id", "last_name", "first_name", "position", "team", "weight",
                                    "draft_year", "draft_team", "draft_round", "draft_pick", "birthdate",
                                    "age", "exp"),
                      sep = ",", skip = 0, data.table = FALSE, showProgress = FALSE)
  )
}
