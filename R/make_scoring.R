scoring_positions = list(
  pass = "QB",
  rush = c("QB", "RB", "WR", "TE"),
  rec =  c("RB", "WR", "TE"),
  misc = c("QB", "RB", "WR", "TE"),
  kick = "K",
  ret = c("RB", "WR", "TE"),
  idp = c("DL", "LB", "DB"),
  dst = "DST"
)


make_scoring_tbl <- function(scoring_rules){
  scoring_rules$pts_bracket <- NULL

  check_one <- scoring_rules %>% map(names) %>% map(`!=`, "all_pos") %>%
    map_lgl(all)
  if(any(check_one)){
    one_pos <- scoring_rules %>% map(names) %>% map(`!=`, "all_pos") %>%
      map_lgl(all) %>% scoring_rules[.] %>% map(as_tibble) %>%
      imap(~ mutate(.x, pos = scoring_positions[[.y]])) %>%
      map(gather, "data_col", "points", -pos) %>% bind_rows()
  } else {
    one_pos <- tibble()
  }

  check_mult <- scoring_rules %>% map(names) %>% map(`==`, "all_pos") %>%
    map_lgl(any) %>% scoring_rules[.] %>% map_lgl(`[[`, "all_pos")

  if(any(check_mult)){
    mult_pos <- scoring_rules %>% map(names) %>% map(`==`, "all_pos") %>%
      map_lgl(any) %>% scoring_rules[.] %>% map_lgl(`[[`, "all_pos") %>%
      which(.) %>% names(.) %>% scoring_rules[.] %>%
      imap(~ map(scoring_positions[[.y]], append, x = .x)) %>%
      modify_depth(2, function(x){
        names(x)[length(x)] <- "pos"
        x}) %>% modify_depth(2, as_tibble) %>%
      modify_depth(2, select, -all_pos) %>%
      modify_depth(2, gather, "data_col", "points", -pos) %>%
      modify_depth(1, bind_rows) %>% bind_rows()
  } else {
    mult_pos <- tibble()
  }

  check_diff <-  scoring_rules %>% map(names) %>% map(`==`, "all_pos") %>%
    map_lgl(any) %>% scoring_rules[.] %>% map_lgl(`[[`, "all_pos") %>%
    `!`
  if(any(check_diff)){
    diff_pos <- scoring_rules %>% map(names) %>% map(`==`, "all_pos") %>%
      map_lgl(any) %>% scoring_rules[.] %>% map_lgl(`[[`, "all_pos") %>%
      `!` %>% which(.) %>% names(.) %>% scoring_rules[.] %>%
      map(list_modify, all_pos = NULL) %>%
      map(function(lst){lst %>% imap(~ append(.x, list(pos = .y)))}) %>%
      modify_depth(2, as_tibble)  %>%
      modify_depth(2, gather, "data_col", "points", -pos) %>%
      modify_depth(1, bind_rows) %>% bind_rows()
  } else {
    diff_pos <- tibble()
  }
  return(bind_rows(one_pos, mult_pos, diff_pos))
}

dst_points <- function(pts_allow, bracket){
  is_season <- all(pts_allow[is.finite(pts_allow)] > 100)
  season_factor <- 1
  if(is_season){
    pts_allow <- pts_allow / 16
    season_factor <- 16
  }
  bracket_tbl <- map(bracket, as_tibble) %>% bind_rows() %>%
    arrange(threshold) %>%
    mutate(low_thr = lag(threshold) + 1,
           low_thr = ifelse(is.na(low_thr), -99, low_thr))


  map_dbl(pts_allow, function(pts){
    idx <- imap_lgl(bracket_tbl$low_thr, ~ between(pts, .x,
                                          bracket_tbl$threshold[[.y]]))
    if(any(!is.na(idx)) && any(idx))
       bracket_tbl$points[idx]* season_factor
    else
      0
    })
}

