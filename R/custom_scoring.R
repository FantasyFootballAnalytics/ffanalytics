
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
    function(x) !is.null(x),
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


#' Create Scoring Tables when computing projections table
#'
#' Creates tables for the DST points brackets and the scoring tables by position
#' used in the `projections_table()` function.
make_scoring_tables = function(scoring_rules) {

  # First, the points bracket for DST
  l_pts_bracket = rapply(scoring_rules$pts_bracket, as.numeric, how = "replace")
  scoring_rules$pts_bracket = NULL

  # Next the main scoring tables (by position)
  scoring_l = vector("list", 9L)
  names(scoring_l) = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB")
  all_pos_idx = unlist(lapply(scoring_rules, `[[`, "all_pos"))

  # if no custom scoring (create several copies of the same table)
  if(all(all_pos_idx)) {
    scoring_table = dplyr::tibble(
      category = rep(names(scoring_rules), times = lengths(scoring_rules)),
      column = sub(".*?\\.", "", names(unlist(scoring_rules, recursive = FALSE))),
      val = unlist(scoring_rules)
    )
    for(pos in names(scoring_l)) {
      if(pos %in% "DST") {
        scoring_table = rbind(scoring_table,
                              data.frame(category = "dst", column = "pts_bracket", val = 1))
      }
      scoring_l[[pos]] = scoring_table
    }
  } else { # if there is custom scoring by position (create separate tables)
    custom_cols = names(all_pos_idx[!all_pos_idx])
    for(pos in names(scoring_l)) {
      temp_scoring = scoring_rules

      for(col in custom_cols) { # if one column is custom, use it, else make NULL
        if(pos %in% names(temp_scoring[[col]])) {
          temp_scoring[[col]] = temp_scoring[[col]][[pos]]
        } else {
          temp_scoring[[col]] = NULL
        }
      }

      scoring_table = dplyr::tibble(
        category = rep(names(temp_scoring), times = lengths(temp_scoring)),
        column = sub(".*?\\.", "", names(unlist(temp_scoring, recursive = FALSE))),
        val = unlist(temp_scoring)
      )

      if(pos %in% "DST") {
        scoring_table = rbind(scoring_table,
                              data.frame(category = "dst", column = "pts_bracket", val = 1))
      }
      scoring_l[[pos]] = scoring_table
    }
  }

  list(pts_bracket = l_pts_bracket,
       scoring_tables = scoring_l)

}



