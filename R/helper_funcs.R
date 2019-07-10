make_df_colnames <- function(tbl){
  rm_txt <- c("DEFENSIVE PLAYERS ", "PLAYERS ", "KICKERS ", "[[:cntrl:]]",
              "Sort", "First:", "Last:", "^Projected ", "\\sWeek [0-9]+",
              "\\sWild Card", "\\sDivisional", "\\sConference",  "\\sSuper Bowl",
              "[^[:alnum:]]$")
  rm_pattern <- paste(rm_txt, collapse = "|")
  cnames <- str_trim(paste(names(tbl), tbl[1,]))
  cnames <- str_trim(gsub(rm_pattern, "", cnames))
  cnames[which(nchar(cnames) == 0)] <- "Z"
  return(make.unique(cnames, sep = ""))
}

num_header_rows <- function(html_pg, tbl_css){
  header_rows <- html_pg %>% html_node(tbl_css) %>%
    html_node("thead") %>% html_children() %>% length()

  return(header_rows)
}

check_2rth <- function(tbl){
  nm <- names(tbl)

  if(any(nchar(nm) == 0)){
    names(tbl) <- make_df_colnames(tbl)
    tbl <- tbl %>% slice(-1)
  } else {
    num_cols <- ncol(tbl)
    if(length(unique(nm)) < num_cols){
      names(tbl) <- make_df_colnames(tbl)
      tbl <- tbl %>% slice(-1)
    }
  }
  return(tbl)
}

id_col <- function(x, match_col){
  player_ids$id[match(x, player_ids[[match_col]])]
}


clean_format <- function(df){
  formatted_num <- intersect(names(df), c("pass_yds", "rush_yds", "rec_yds", "xp_pct", "fg_pct"))
  remove_format <- function(x)gsub("\\,|%", "", x)
  if(length(formatted_num) > 0)
    df <- df %>% mutate_at(formatted_num, remove_format)
  return(df)
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

  x <- add_column(x, tmp_id = 1:nrow(x))

  matched <- data.frame(tmp_id=as.integer(NA), id = as.character(NA), stringsAsFactors = FALSE)[-1,]

  for(col in c("match_name_pos_tm", "match_name_pos", "match_name")){
    x_tbl <- filter(x, !(x$tmp_id %in% matched$tmp_id))
    y_tbl <- filter(p_tbl, !(p_tbl$id %in% matched$id))
    match_ids <- match_by_col(x_tbl, y_tbl, col, c("tmp_id", "id"))
    matched <- bind_rows(list(matched, match_ids))
  }

  return(matched$id[match(x$tmp_id, matched$tmp_id)])
}


available_sources <- function(period = c("season", "week")){
  pos_group <- paste0(period, "_pos")
  projection_sources %>% map_lgl(~ length(.x[[pos_group]]) > 0) %>%
    which(.) %>% names()
}

available_position <- function(period = c("season", "week"), src = NULL){
  pos_group <- paste0(period, "_pos")
  if(is.null(src))
    src <- available_sources(period)
  else
    src <- intersect(available_sources(period), src)

  projection_sources[src] %>% map(`[[`, pos_group) %>% reduce(union)

}


