nfl_stats <- dplyr::bind_rows(
  lapply(httr::content(httr::GET("http://api.fantasy.nfl.com/v1/game/stats?format=json"))$stats,
         tibble::as.tibble)
)

nfl_cols <- as.character(nfl_stats$id)
names(nfl_cols) <- nfl_stats$shortName

devtools::use_data(nfl_cols, overwrite = TRUE)
