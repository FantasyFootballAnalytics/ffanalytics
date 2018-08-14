rules <- function(rule_list){

  var_pattern <- list(pass = "^pass", rush = "^rush", rec = "^rec",
                      ret = "^ret", kick = "^fg|^xp", dst = "^dst", idp = "^idp")

  scoring_cols <- map(projection_sources, `[[`, "stat_cols") %>%
    map(names) %>% reduce(union) %>% str_replace("rxx", "rec") %>%
    c(., "rush_50_yds", "rush_100_yds", "rush_150_yds", "rush_200_yds")

  rule_list <- rule_list[intersect(names(rule_list), scoring_cols)]

  pos_sr <- map(var_pattern, ~ rule_list[str_subset(names(rule_list), .x)]) %>%
    discard(~length(.) == 0)

  pn <- map(pos_sr, names) %>% reduce(union)


  pos_sr$misc <- rule_list[setdiff(names(rule_list), pn)]

  return(pos_sr %>% discard(~ length(.) == 0))
}

pos_scor <- function(pname, rule_list){
  pos <- pname %>% str_split("[:punct:]|[:space:]|\\|") %>% simplify() %>% str_subset(".+")
  names(pos) <- pos

  map(pos, ~ rules(rule_list)) %>% transpose()
}

grp_rule <- function(x, p){
  sp <- ffwebscrape:::scoring_positions[[p]]

  map(x, ~ .x[intersect(names(.x), sp)]) %>% reduce(~ list_merge(.x,!!!.y)) %>%
    modify_depth(2, sum)
}

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
#'                "RB, WR, TE" = list(rec = 1, rec_yds = 0.1, rec_tds = 6),
#'                "TE" = list(rec = 0.5))
#' @export
custom_scoring <- function(...){
  sr <- list(pass = list(), rush = list(all_pos = TRUE), rec = list(all_pos = TRUE),
             misc = list(all_pos = TRUE), ret = list(all_pos = TRUE),
             kick = list(), dst = list(), idp = list(all_pos = TRUE))

  scoring_args <- as.list(match.call())[-1]

  scoring_cols <- map(projection_sources, `[[`, "stat_cols") %>%
    map(names) %>% reduce(union) %>% str_replace("rxx", "rec") %>%
    c(., "rush_50_yds", "rush_100_yds", "rush_150_yds", "rush_200_yds")

  all_pos_scoring <- scoring_args[scoring_cols] %>% discard(is.null)
  grp_sr <- list()

  if(length(all_pos_scoring) > 0){
    grp_sr <- rules(all_pos_scoring)
    sr <- list_modify(sr, !!! grp_sr)
  }

  pos_scoring <- scoring_args[setdiff(names(scoring_args), names(all_pos_scoring))] %>%
    map(eval)
  pos_sr <- list()

  if(length(pos_scoring) > 0){
    pos_sr <- imap(pos_scoring, ~ pos_scor(.y, .x)) %>%
      transpose() %>%
      imap( ~grp_rule(.x, .y ))
    sr <- list_modify(sr, !!! pos_sr)
  }

  sr <- sr %>% discard(~ all(names(.) == "all_pos"))

  ap <- sr[intersect(names(sr), c("rec", "rush", "misc", "ret", "idp"))]%>%
    imap(~ list(all_pos = !any(ffwebscrape:::scoring_positions[[.y]] %in% names(.x))))

  sr %>% list_modify(!!! ap)
}
