


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








