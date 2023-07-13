#' @import dplyr tidyr purrr httr2 rvest rrapply
#' @importFrom data.table fread rbindlist
#' @importFrom readxl read_xlsx

.onLoad <- function(libname, pkgname){
  player_table = data.table::fread("https://s3.us-east-2.amazonaws.com/ffanalytics/packagedata/player_table.csv",
                    colClasses = c("character", "character", "character", "character", "character",
                                    "integer", "integer", "character", "integer", "integer",
                                    "Date", "integer", "integer"),
                    col.names = c("id", "last_name", "first_name", "position", "team", "weight",
                                  "draft_year", "draft_team", "draft_round", "draft_pick", "birthdate",
                                  "age", "exp"),
                      sep = ",", skip = 0, data.table = FALSE, showProgress = FALSE)
  player_table = dplyr::tibble(player_table)
  environment(player_table) = asNamespace("ffanalytics")
  assignInNamespace("player_table", player_table, ns = "ffanalytics")

}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Note: the ffanalytics package locally caches ADP & ECR data scrapes. Cached scrapes",
    "\nolder than 8 hours are dropped (upon checking)",
    "\n  - See ?clear_ffanalytics_cache() for how to manually clear the cache",
    "\n  - Use list_ffanalytics_cache() to see what is currently cached"
    )
}
