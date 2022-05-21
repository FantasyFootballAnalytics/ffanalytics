

id_col = function(x, match_col){
  player_ids$id[match(x, player_ids[[match_col]])]
}

get_mfl_id = function(id_col = NULL, player_name = NULL, first = NULL,
                      last = NULL, pos = NULL, team = NULL) {

  if(!is.null(id_col)) {
    col_name = deparse(substitute(id_col))
    if(grepl("$", col_name, fixed = TRUE)) {
      col_name = sub(".+\\$", "", id_col)
    }
    ids = player_ids$id[match(id_col, player_ids[[col_name]])]
    return(ids)
  }

  ref_table = player_table %>%
    transmute(id = id,
              player_name = paste(first_name, last_name),
              player_name = gsub("\\s+(?i)(defense|jr|sr|[iv]+)$", "", player_name),
              player_name = gsub("[[:punct:]]|\\s+", "", tolower(player_name)),
              pos = rename_vec(position, unlist(pos_corrections)),
              team = rename_vec(team, unlist(team_corrections)))

  l_p_info = list(
    player_name = player_name,
    first = first,
    last = last,
    pos = pos,
    team = team
  )
  l_p_info = Filter(Negate(is.null), l_p_info)

  l_p_info = lapply(l_p_info, function(x) {
    x = rename_vec(x, unlist(pos_corrections))
    x = rename_vec(x, unlist(team_corrections))
    x = gsub("[[:punct:]]+", "", x)
    x = gsub("\\s+(?i)(defense|jr|sr|[iv]+)$", "", x)
    x
  })
  df_p_info = as.data.frame(l_p_info)

  if(!"player_name" %in% names(df_p_info)) {
    df_p_info$player_name = tolower(paste0(df_p_info$first, df_p_info$last))
  } else {
    df_p_info$player_name = gsub("\\s+", "", tolower(df_p_info$player_name))
  }

  # If pos = DST, replace by team name
  df_p_info = df_p_info %>%
    mutate(
      id = dplyr::case_when(
        pos == "DST" ~ ref_table$id[match(team, ref_table$team)],
        paste0(player_name, pos, team) %in% do.call(paste0, ref_table[c("player_name", "pos", "team")]) ~
          ref_table$id[match(paste0(player_name, pos, team), do.call(paste0, ref_table[c("player_name", "pos", "team")]))],
        paste0(player_name, pos) %in% do.call(paste0, ref_table[c("player_name", "pos")]) ~
          ref_table$id[match(paste0(player_name, pos), do.call(paste0, ref_table[c("player_name", "pos")]))],
        paste0(player_name, team) %in% do.call(paste0, ref_table[c("player_name", "team")]) ~
          ref_table$id[match(paste0(player_name, team), do.call(paste0, ref_table[c("player_name", "team")]))],
        TRUE ~ NA_character_
      )
    )
  df_p_info$id

}

match_by_col <- function(x, y, match_col, id_vars){
  x_col <- x[[match_col]]
  y_col <- y[[match_col]]

  x_dups <- x_col[duplicated(x_col)]
  y_dups <- y_col[duplicated(y_col)]

  val_match <- intersect(x_col[!(x_col %in% x_dups)], y_col[!(y_col %in% y_dups)])

  xy_match <- inner_join(x[x[[match_col]] %in% val_match, c(match_col, id_vars[1])],
                         y[y[[match_col]] %in% val_match, c(match_col, id_vars[2])],
                         by = match_col) %>% select(id_vars)
  return(xy_match)
}

clean_pname <- function(x){
  gsub("[J|S]r\\.*$|[[:punct:]]|\\s",  "", x)
}

match_players <- function(x){
  x <- mutate(x, pos = recode(pos, !!!pos_corrections), team = recode(team, !!!team_corrections),
              player = gsub("\\s[JS]r\\.*|\\s[I|V]+$", "", player))
  p_tbl <- player_table %>% unite("name", c("first_name", "last_name"), sep = " ") %>%
    mutate(position = recode(position, !!!pos_corrections),
           team = recode(team, !!!team_corrections),
           name = gsub("\\s[JS]r\\.*|\\s[I|V]+$", "", name))

  match_pos <- unique(x$pos)

  p_tbl <- filter(p_tbl, position %in% match_pos) %>%
    mutate(match_name = tolower(clean_pname(recode(name, !!!name_corrections ))),
           match_name_pos = paste(match_name, tolower(position), sep = "-"),
           match_name_pos_tm = paste(match_name_pos, tolower(team), sep = "-"))

  x <- x %>%
    mutate(match_name = tolower(clean_pname(recode(player, !!!name_corrections ))),
           match_name_pos = paste(match_name, tolower(pos), sep = "-"),
           match_name_pos_tm = paste(match_name_pos, tolower(team), sep = "-"))

  x <- mutate(x, tmp_id = 1:nrow(x))

  matched <- data.frame(tmp_id=as.integer(NA), id = as.character(NA), stringsAsFactors = FALSE)[-1,]

  for(col in c("match_name_pos_tm", "match_name_pos", "match_name")){
    x_tbl <- filter(x, !(x$tmp_id %in% matched$tmp_id))
    y_tbl <- filter(p_tbl, !(p_tbl$id %in% matched$id))
    match_ids <- match_by_col(x_tbl, y_tbl, col, c("tmp_id", "id"))
    matched <- bind_rows(list(matched, match_ids))
  }

  return(matched$id[match(x$tmp_id, matched$tmp_id)])
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

