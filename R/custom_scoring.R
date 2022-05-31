
#' Create Custom Scoring
#'
#' This function geerates a list of scoring settings that can be used as custom
#' scoring setting for calculations. THe resulting list will still need to have
#' a \code{pts_bracket} element added to handle points allowed for DSTs. See
#' \code{vignette("scoring_settings")} about this.
#'
#' @param ... this can be indivdual scoring variables or a list of scoring variables
#' for specific positions. The list will need to be named with a separated list of
#' positions. Note that scoring for passing, kicking, and dst should never be specified
#' within a list.
#'
#' @examples
#' # Settings for a standard league:
#' custom_scoring(pass_yds = 0.04, pass_tds = 4,
#'                rush_yds = 0.1, rush_tds = 6,
#'                rec_yds = 0.1, rec_tds = 6)
#'
#' # Settings for a PPR league
#' custom_scoring(pass_yds = 0.04, pass_tds = 4,
#'                rush_yds = 0.1, rush_tds = 6,
#'                rec = 1, rec_yds = 0.1, rec_tds = 6)
#'
#' # Settings for a PPR league with TE premium
#' custom_scoring(pass_yds = 0.04, pass_tds = 4,
#'                rush_yds = 0.1, rush_tds = 6,
#'                RB = list(rec = 0, rec_yds = 0.1, rec_tds = 6),
#'                WR = list(rec = 1, rec_yds = 0.1, rec_tds = 6),
#'                TE = list(rec = 1, rec_yds = 0.1, rec_tds = 6))
#' @export
custom_scoring = function(...) {
  l = list(...)
  l_values = unlist(l, use.names = TRUE)

  name_l = strsplit(names(l_values), ".", fixed = TRUE)
  name_l = lapply(name_l, function(x) {
    c(scoring_type_for_cols[x[length(x)]], x)
  })

  for(i in seq_along(name_l)) {
    scoring_empty[[name_l[[i]]]] = unname(l_values)[i]
  }

  custom_scoring_obj = rrapply::rrapply(
    scoring_empty,
    function(x) !is.null(x) & x != 0,
    how = "prune"
  )

  needs_all_pos = intersect(names(custom_scoring_obj), c("pass", "rush", "rec", "misc", "ret", "idp"))
  all_pos_vec = sapply(custom_scoring_obj[needs_all_pos], function(x) {
    !any(names(x) %in% c("QB", "RB", "WR", "TE", "DL", "LB", "DB"))
  })

  for(i in names(all_pos_vec)) {
    custom_scoring_obj[[i]][["all_pos"]] = unname(all_pos_vec[i])
    all_pos_pos = which(names(custom_scoring_obj[[i]]) == "all_pos")
    pos_order = c(all_pos_pos, setdiff(seq_along(custom_scoring_obj[[i]]), all_pos_pos))
    custom_scoring_obj[[i]] = custom_scoring_obj[[i]][pos_order]
  }
  custom_scoring_obj

}



