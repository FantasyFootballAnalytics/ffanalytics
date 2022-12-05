


get_mfl_id = function(id_col = NULL, player_name = NULL, first = NULL,
                      last = NULL, pos = NULL, team = NULL) {
  l_p_info = list(
    player_name = player_name,
    first = first,
    last = tolower(last),
    pos = pos,
    team = team,
    id = NA
  )

  l_p_info = Filter(Negate(is.null), l_p_info)
  l_p_info = lapply(l_p_info, function(x) {
    x = rename_vec(x, unlist(pos_corrections))
    x = rename_vec(x, unlist(team_corrections))
    x = gsub("[[:punct:]]+", "", x)
    x = gsub("\\s+(?i)(defense|jr|sr|[iv]+)$", "", x)
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
    transmute(id = id,
              player_name = paste(first_name, last_name),
              player_name = gsub("\\s+(?i)(defense|jr|sr|[iv]+)$", "", player_name),
              player_name = gsub("[[:punct:]]|\\s+", "", tolower(player_name)),
              last = gsub("\\s+(?i)(defense|jr|sr|[iv]+)$", "", last_name),
              last = gsub("[[:punct:]]|\\s+", "", tolower(last)),
              pos = rename_vec(position, unlist(pos_corrections)),
              team = rename_vec(team, unlist(team_corrections)))


  df_p_info = as.data.frame(l_p_info)

  # If pos = DST, replace by team name
  if("pos" %in% names(df_p_info)) {
    df_p_info$id = ifelse(
      df_p_info$pos == "DST",
      ref_table$id[match(df_p_info$team, ref_table$team)],
      df_p_info$id
    )
    if(all(df_p_info$pos == "DST")) {
      return(df_p_info$id)
    }
  }

  if(!"player_name" %in% names(df_p_info)) {
    df_p_info$player_name = tolower(paste0(df_p_info$first, df_p_info$last))
  } else {
    df_p_info$player_name = gsub("\\s+", "", tolower(df_p_info$player_name))
  }

  df_p_info = df_p_info %>%
    mutate(
      id = dplyr::case_when(
        !is.na(id) ~ id,
        paste0(player_name, pos, team) %in% do.call(paste0, ref_table[c("player_name", "pos", "team")]) ~
          ref_table$id[match(paste0(player_name, pos, team), do.call(paste0, ref_table[c("player_name", "pos", "team")]))],
        paste0(player_name, pos) %in% do.call(paste0, ref_table[c("player_name", "pos")]) ~
          ref_table$id[match(paste0(player_name, pos), do.call(paste0, ref_table[c("player_name", "pos")]))],
        paste0(player_name, team) %in% do.call(paste0, ref_table[c("player_name", "team")]) ~
          ref_table$id[match(paste0(player_name, team), do.call(paste0, ref_table[c("player_name", "team")]))],
        TRUE ~ NA_character_
      )
    )

  if("last" %in% names(df_p_info) && "team" %in% names(df_p_info) && "pos" %in% names(df_p_info)) {
    df_p_info$id = ifelse(
      is.na(df_p_info$id),
      ref_table$id[match(paste0(df_p_info$last, df_p_info$pos, df_p_info$team),
                         do.call(paste0, ref_table[c("last", "pos", "team")]))],

      df_p_info$id
    )
  }

  df_p_info$id
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


