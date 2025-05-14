


get_mfl_id = function(id_col = NULL, player_name = NULL, first = NULL,
                      last = NULL, pos = NULL, team = NULL) {
  l_p_info = list(
    player_name = player_name,
    first = first,
    last = last,
    pos = pos,
    team = team,
    id = NA
  )
  max_len = max(lengths(l_p_info))
  length_1 = lengths(l_p_info) == 1

  l_p_info[length_1] = lapply(l_p_info[length_1], function(x) {
    rep(x, max_len)
  })

  if(!is.null(player_name)) {
    if(is.null(first)) {
      l_p_info$first = sub("\\s+.*$", "", player_name)
    }

    if(is.null(last)) {
      l_p_info$last = sub(".*?\\s+", "", player_name)
    }
  }


  l_p_info = Filter(Negate(is.null), l_p_info)
  l_p_info = lapply(l_p_info, function(x) {
    x = rename_vec(toupper(x), unlist(pos_corrections))
    x = rename_vec(x, unlist(team_corrections))
    x = gsub("\\s+(defense|jr|sr|[iv]+)\\.?$", "", tolower(x))
    x = gsub("[[:punct:]]+|\\s+", "", x)
    x
  })

  if(!is.null(id_col)) {
    col_name = deparse(substitute(id_col))
    if(grepl("$", col_name, fixed = TRUE)) {
      col_name = sub(".+\\$", "", col_name)
    }
    l_p_info$id = player_ids$id[match(id_col, player_ids[[col_name]])]

    if(!anyNA(l_p_info$id)) {
      return(l_p_info$id)
    }
  }

  ref_table = player_table %>%
    mutate(across(where(is.character), tolower)) %>%
    transmute(id = id,
              player_name = paste(first_name, last_name),
              player_name = gsub("\\s+(defense|jr|sr|[iv]+)\\.?$", "", player_name),
              player_name = gsub("[[:punct:]]|\\s+", "", player_name),
              last = gsub("\\s+(defense|jr|sr|[iv]+)\\.?$", "", last_name),
              last = gsub("[[:punct:]]|\\s+", "", last),
              first = gsub("\\s+(defense|jr|sr|[iv]+)\\.?$", "", first_name),
              first = gsub("[[:punct:]]|\\s+", "", first),
              pos = rename_vec(toupper(position), unlist(pos_corrections)),
              pos = tolower(pos),
              team = rename_vec(toupper(team), unlist(team_corrections)),
              team = tolower(team))

  # If pos = DST, replace by team name
  if("pos" %in% names(l_p_info)) {
    l_p_info$id = ifelse(
      l_p_info$pos == "dst",
      ref_table$id[match(l_p_info$team, ref_table$team)],
      l_p_info$id
    )
  }

  col_combos = list(
    c("player_name", "pos", "team"),
    c("last", "pos", "team"),
    c("player_name", "team"),
    c("player_name", "pos"),
    # c("last", "team"),
    c("first", "pos", "team")
  )
  combo_idx = vapply(col_combos, function(x) {
    all(x %in% names(l_p_info))
  }, logical(1L))

  for(combo in col_combos[combo_idx]) {
    id_idx = is.na(l_p_info$id)

    l_p_info_vec = do.call(paste0, l_p_info[combo])[id_idx]
    ref_table_vec = do.call(paste0, ref_table[combo])

    # Removing dups from reftable
    ref_dups = ref_table_vec[duplicated(ref_table_vec)]
    keep_in_ref = !ref_table_vec %in% ref_dups
    ref_table_vec = ref_table_vec[keep_in_ref]


    l = lapply(l_p_info_vec, function(y) {
      which(ref_table_vec %in% y)
    })
    l[lengths(l) != 1] = NA_integer_
    match_vec = unlist(l)

    l_p_info$id[id_idx] = ref_table$id[keep_in_ref][match_vec]

  }
  l_p_info$id

}

get_scrape_year <- function(date) {
  if(missing(date)) {
    date = Sys.Date()
  }
  date = as.POSIXlt(date)
  cal_year = date$year + 1900L
  cal_month = date$mon + 1L

  if(cal_month %in% 1:3) {
    cal_year - 1L
  } else {
    cal_year
  }
}

rename_vec = function(x, new_names, old_names = NULL) {
  if(is.null(old_names)) {
    old_names = names(new_names)
    if(is.null(names(new_names))) {

      message = paste0("Must supply old_names argument, or "
                       , deparse(substitute(new_names))
                       , " needs to be a named vector with the "
                       , "old names  as the named portion")
      stop(message)
    }
  }

  idx = match(x, old_names)
  x[!is.na(idx)] = new_names[omit_NA(idx)]
  x
}

omit_NA = function(x) {
  x[!is.na(x)]
}

row_sd = function(x, na.rm = FALSE) {
  if(is.data.frame(x)) {
    x = do.call(cbind, x)
  }
  dim_x = dim(x)

  if(na.rm && anyNA(x)) {
    n_minus_1 = dim_x[2] - .rowSums(is.na(x), dim_x[1], dim_x[2]) - 1L
  } else {
    n_minus_1 = dim_x[2] - 1
  }

  r_mean = .rowMeans(x, dim_x[1], dim_x[2], na.rm = na.rm)

  r_var = .rowSums((x - r_mean)^2 / n_minus_1, dim_x[1], dim_x[2], na.rm = na.rm)
  r_sd = sqrt(r_var)
  r_sd[n_minus_1 <= 1] = NA
  r_sd
}

impute_and_score_sources = function(data_result, scoring_rules) {
  scoring_objs = make_scoring_tables(scoring_rules)

  data_result = impute_via_rates_and_mean(data_result, scoring_objs)
  data_result = impute_bonus_cols(data_result, scoring_objs$scoring_tables)

  data_result[] = source_points(data_result, scoring_rules, return_data_result = TRUE)
  data_result
}


# Returns new player_id table
update_player_id_table = function(player_id_table = NULL, id_column, value) {

}

get_pos_src_from_scrape = function(data_result) {
  data_by_pos_src = lapply(data_result, function(x) {
    split(x, x$data_src)
  })
  src_pos = stack(lapply(data_by_pos_src, names))
  split(as.character(src_pos$ind), src_pos$values)
}

# TODO: This may be supersceeded by caching at the scrape level
extract_src_scrapes_from_scrape = function(data_result) {
  pos_src = get_pos_src_from_scrape(data_result)

  lapply(setNames(names(pos_src), names(pos_src)), function(x) {
    positions = setNames(pos_src[[x]], pos_src[[x]])
    lapply(positions, function(pos) {
      data_result[[pos]][data_result[[pos]]$data_src == x,]
    })
  })
}



# actual_points_scoring(
#   season = 2023,
#   summary_level = c("season", "week"),
#   stat_type = c("player", "dst", "team"),
#   season_type = c("REG", "POST", "REG+POST"),
#   scoring_rules = NULL,
#   vor_baseline = NULL,
#   rename_colums = TRUE
# )
actual_points_scoring = function(season = NULL,
                                 summary_level = c("season", "week"),
                                 stat_type = c("player", "dst", "team"),
                                 season_type = c("REG", "POST", "REG+POST"),
                                 scoring_rules = NULL,
                                 vor_baseline = NULL,
                                 rename_colums = TRUE) {

  # Checking if suggested package is installed w/ correct version
  if(isFALSE(requireNamespace("nflfastR", quietly = TRUE))) {
    stop(
      "Package \"nflfastR\" (>= v5.0.0) must be installed to use this function.",
      call. = FALSE
    )
  }
  if(isTRUE(requireNamespace("nflfastR", quietly = TRUE))) {
    # need to find more robust way to check
    pkg_version = as.integer(substr(packageVersion("nflfastR"), 1, 1))

    if(pkg_version < 5L)
      stop(
        "Package \"nflfastR\" (>= v5.0.0) must be installed to use this function.",
        call. = FALSE
      )
  }



  # Argument checking / filling missing arguments
  if(is.null(season)) {
    stop("Must provide argument to season. E.g., `season = 2023`")
  }
  if(length(season) != 1) {
    stop("Must provide one year to season argument. e.g., `season = 2023`")
  }

  summary_level = match.arg(tolower(summary_level), c("season", "week"))
  stat_type = match.arg(tolower(stat_type), c("player", "dst", "team"))
  season_type = match.arg(toupper(season_type), c("REG", "POST", "REG+POST"))
  if(is.null(scoring_rules)) {
    scoring_rules = scoring
  }
  if(is.null(vor_baseline)) {
    vor_baseline = default_baseline
  }

  # browser()

  if(isTRUE(stat_type == "dst")) {
    nflf_stat_type = "team"
  } else {
    nflf_stat_type = stat_type
  }


  # Passing arguments to the actuals functions
  nflf_stat = nflfastR::calculate_stats(
    season = season,
    summary_level = summary_level,
    stat_type = nflf_stat_type,
    season_type = season_type
  )

  nflf_stat = nflf_stat %>%
    dplyr::filter(grepl(sub("+", "|", !!season_type, fixed = TRUE), .data$season_type))

  # Loading pbp and filtering for season_type
  nflf_pbp = nflfastR::load_pbp(
    season = season,
    file_type = "rds"
  )
  nflf_pbp = nflf_pbp %>%
    dplyr::filter(grepl(sub("+", "|", !!season_type, fixed = TRUE), .data$season_type)) %>%
    dplyr::mutate(home_team = rename_vec(home_team, unlist(team_corrections)),
                  away_team = rename_vec(away_team, unlist(team_corrections)))



  # Adding week if summary level = "season"
  if(isTRUE(summary_level %in% c("season"))) {
    nflf_stat$week = 0L
    nflf_pbp$week = 0L
  } else {
    nflf_stat$opponent_team = rename_vec(nflf_stat$opponent_team, unlist(team_corrections))
  }

  # browser()


  # Updating columns names to match our standard column names
  if(isTRUE(stat_type == "player")) {
    names(nflf_stat) = rename_vec(names(nflf_stat), nflfastr_player_cols)
    nflf_stat$recent_team = rename_vec(nflf_stat$recent_team, unlist(team_corrections))

  } else {
    # names(nflfastr_player_cols) = gsub("idp", "dst", names(nflfastr_player_cols), fixed = TRUE)
    nflfastr_player_cols = gsub("idp", "dst", nflfastr_player_cols, fixed = TRUE)
    names(nflf_stat) = rename_vec(names(nflf_stat), nflfastr_player_cols)

    nflf_stat$team = rename_vec(nflf_stat$team, unlist(team_corrections))
    nflf_stat$gsis_id = nflf_stat$team
    nflf_stat$pos = "DST"

    nflf_pbp$passer_player_id = ifelse(!is.na(nflf_pbp$passer_player_id), nflf_pbp$posteam, NA)
    nflf_pbp$rusher_player_id = ifelse(!is.na(nflf_pbp$rusher_player_id), nflf_pbp$posteam, NA)
    nflf_pbp$receiver_player_id = ifelse(!is.na(nflf_pbp$receiver_player_id), nflf_pbp$posteam, NA)

  }

  scoring_objs = make_scoring_tables(scoring_rules)


  # Calculating specific data from PBP data
  # pass_40_yds, pass_300_yds, pass_350_yds, pass_400_yds
  # rush_40_yds, rush_100_yds, rush_150_yds, rush_200_yds
  # rec_40_yds, rec_100_yds, rec_150_yds, rec_200_yds
  df_pass_40_yds = nflf_pbp %>%
    dplyr::filter(!is.na(passer_player_id)) %>%
    dplyr::group_by(season_year = season, week, gsis_id = passer_player_id) %>%
    dplyr::summarise(
      pass_40_yds = sum(passing_yards >= 40, na.rm = TRUE),
      .groups = "drop"
    )

  df_rush_40_yds = nflf_pbp %>%
    dplyr::filter(!is.na(rusher_player_id)) %>%
    dplyr::group_by(season_year = season, week, gsis_id = rusher_player_id) %>%
    dplyr::summarise(
      rush_40_yds = sum(rushing_yards >= 40, na.rm = TRUE),
      .groups = "drop"
    )

  df_rec_40_yds = nflf_pbp %>%
    dplyr::filter(!is.na(receiver_player_id)) %>%
    dplyr::group_by(season_year = season, week, gsis_id = receiver_player_id) %>%
    dplyr::summarise(
      rec_40_yds = sum(receiving_yards >= 40, na.rm = TRUE),
      .groups = "drop"
    )

  nflf_stat = nflf_stat %>%
    dplyr::left_join(df_pass_40_yds, c("season_year", "week", "gsis_id")) %>%
    dplyr::left_join(df_rush_40_yds, c("season_year", "week", "gsis_id")) %>%
    dplyr::left_join(df_rec_40_yds, c("season_year", "week", "gsis_id")) %>%
    dplyr::mutate(
      pass_inc = pass_att - pass_comp,
      pass_comp_pct = round(pass_comp / pass_att, 5),
      fumbles = rowSums(dplyr::pick(pass_sack_fumbles,  rush_fumbles,  rec_fumbles), na.rm = TRUE),
      two_pts = rowSums(dplyr::pick(pass_two_pts, rush_two_pts, rec_two_pts), na.rm = TRUE),
      return_yds = kickoff_return_yds + punt_return_yds,
      pass_300_yds = as.integer(pass_yds >= 300, na.rm = TRUE),
      pass_350_yds = as.integer(pass_yds >= 350, na.rm = TRUE),
      pass_400_yds = as.integer(pass_yds >= 400, na.rm = TRUE),
      rush_100_yds = as.integer(rush_yds >= 100, na.rm = TRUE),
      rush_150_yds = as.integer(rush_yds >= 150, na.rm = TRUE),
      rush_200_yds = as.integer(rush_yds >= 200, na.rm = TRUE),
      rec_100_yds = as.integer(rec_yds >= 100, na.rm = TRUE),
      rec_150_yds = as.integer(rec_yds >= 150, na.rm = TRUE),
      rec_200_yds = as.integer(rec_yds >= 200, na.rm = TRUE),
      pos = ifelse(pos == "SPEC", "K", pos)
    )

  data_result = split.data.frame(nflf_stat, nflf_stat$pos)
  attr(data_result, "season") = unique(nflf_stat$season_year)

  if(length(unique(nflf_stat$week)) > 1) {
    attr(data_result, "week") = 1
  } else {
    attr(data_result, "week") = unique(nflf_stat$week)
  }

  # Add points allowed & cleanup columns
  if(isTRUE(stat_type %in% c("team", "dst"))) {

    # Points allowed
    temp_scores = nflf_pbp %>%
      dplyr::group_by(season, week, home_team, away_team) %>%
      dplyr::summarise(total_home_score = max(total_home_score),
                       total_away_score = max(total_away_score),
                       .groups = "drop")

    actual_allowed = dplyr::bind_rows(
      dplyr::select(
        temp_scores, season_year = season, week, team = home_team,
        team_pts_scored = total_home_score, opp_pts_scored = total_away_score,
        dst_pts_allowed = total_away_score
        ),
      dplyr::select(
        temp_scores, season_year = season, week, team = away_team,
        team_pts_scored = total_away_score, opp_pts_scored = total_home_score,
        dst_pts_allowed = total_home_score
        )
    )

    if(summary_level == "season") {
      actual_allowed = actual_allowed %>%
        dplyr::group_by(season_year, week, team) %>%
        dplyr::summarise(team_pts_scored = sum(team_pts_scored, na.rm = TRUE),
                         opp_pts_scored = sum(opp_pts_scored, na.rm = TRUE),
                         dst_pts_allowed = sum(dst_pts_allowed, na.rm = TRUE),
                         .groups = "drop")
    }

    data_result[["DST"]] = data_result[["DST"]] %>%
      dplyr::left_join(actual_allowed, c("season_year", "week", "team")) %>%
      dplyr::select(-gsis_id, -pos)

  }


  if(isTRUE(stat_type == "dst")) {
    scoring_rules = scoring_rules[c("ret", "dst", "pts_bracket")]
  }
  data_result[] = source_points(data_result, scoring_rules, return_data_result = TRUE, is_actual = TRUE)

  nflf_out_df = dplyr::bind_rows(data_result)



  if(isFALSE(rename_colums)) {
    switch_back_names = setNames(names(nflfastr_player_cols), nflfastr_player_cols)

    names(nflf_out_df) = rename_vec(names(nflf_out_df), switch_back_names)
  }
  nflf_out_df
}
# devtools::load_all()
# df_out = actual_points_scoring(
#   season = 2019,
#   summary_level = c("season"),
#   stat_type = c("player", "dst", "team"),
#   season_type = c("REG", "POST", "REG+POST"),
#   scoring_rules = NULL,
#   vor_baseline = NULL,
#   rename_colums = TRUE
# )













