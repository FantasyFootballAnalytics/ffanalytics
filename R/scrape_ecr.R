#' @export
scrape_ecr <- function(rank_period = c("draft", "weekly", "ros", "dynasty", "rookies"),
                       position = c("Overall", "QB", "RB", "WR", "TE", "K", "FLEX",
                                    "DST", "IDP", "DL", "LB", "DB"),
                       rank_type = c("Std", "PPR", "Half")){
  rank_period <- match.arg(rank_period)
  position <- match.arg(position)
  rank_type <- match.arg(rank_type)

  if(rank_period == "weekly" & position == "Overall"){
    stop("Overall weekly ranks are not provided", call. = FALSE)
  }

  if(rank_period == "ros" & position == "IDP"){
    stop("Combined IDP ROS ranks are not provided", call. = FALSE)
  }

  ranks_url <- "https://www.fantasypros.com/nfl/rankings/"

  rk_type <- switch(rank_type, "Std" = "", "PPR" = "ppr", "Half" = "half-point-ppr")

  rk_pos <- tolower(position)

  if(rank_period == "draft"){
    if(position == "Overall"){
      rk_php <- ifelse(rank_type == "Std", "consensus-cheatsheets.php",
                       paste0(rk_type, "-cheatsheets.php"))
    } else {
      rk_php <- switch(rank_type,
                       "Std" = paste0(rk_pos, "-cheatsheets.php"),
                       ifelse(position %in% c("RB", "WR", "TE", "FLEX"),
                              paste0(rk_type, "-", rk_pos, "-cheatsheets.php"),
                              paste0(rk_pos, "-cheatsheets.php")))
    }
  } else if(rank_period == "weekly"){
    rk_php <- switch(rank_type,
                     "Std" = paste0(rk_pos, ".php"),
                     ifelse(position %in% c("RB", "WR", "TE", "FLEX"),
                            paste0(rk_type, "-", rk_pos, ".php"),
                            paste0(rk_pos, ".php")))
  } else if(rank_period == "ros"){
    rk_php <- switch(rank_type,
                     "Std" = paste0("ros-", rk_pos, ".php"),
                     ifelse(position %in% c("RB", "WR", "TE", "FLEX", "Overall"),
                            paste0("ros-", rk_type, "-", rk_pos, ".php"),
                            paste0("ros-", rk_pos, ".php")))
  } else if(rank_period == "dynasty"){
    rk_php <- paste0("dynasty-", rk_pos, ".php")
  } else {
    rk_php <- "rookies.php"
  }

  rank_page <- read_html(paste0(ranks_url, rk_php))

  num_experts <- rank_page %>%
    html_nodes("#experts input.expert[type='checkbox']") %>%
    html_attr("checked") %>% `==`("checked") %>% which() %>% length()

  rank_page %>% html_nodes("tr.tier-row") %>% xml_remove()

  rank_page %>% html_nodes("#rank-data tr.static, tr.table-ad, tr.rank-table-ad") %>% xml_remove()
  rank_tbl <- rank_page %>% html_node("#rank-data") %>% html_table(fill = TRUE) %>%
    repair_names() %>% select(-matches("V[0-9]+|Notes"))

  player_col <- rank_tbl %>% select(matches("\\(Team\\)$|DST$")) %>%
    unlist(use.names = FALSE)

  player_team <- player_col %>% str_remove("\\s[A-Z]{2,}$") %>% str_extract("[A-Z]{2,}$")
  player_col <- player_col %>% str_remove("[A-Z]{2,}\\s[A-Z]{2,}$")

  p_col <- rank_tbl %>% select(matches("\\(Team\\)$|DST$")) %>% names()

  rank_tbl <- rank_tbl %>% extract(p_col , c("Player", "Abbr Name", "team"), "([A-Za-z0-9\\.\\-'\\s]+)([A-Z]\\.\\s[A-Za-z0-9\\.\\-']+)\\s([A-Z]{2,}$)")
  fp_ids <- rank_page %>% html_nodes("#rank-data tr.player-row td.player-label > a[href*='players'],a[href*='/teams/']") %>%
    html_attr("href") %>% basename() %>% gsub(".php", "", .)

  if(all(is.na(rank_tbl$`Abbr Name`))){
    return(rank_tbl %>% add_column(id = NA_character_, .before = 1)  %>%
      janitor::clean_names() %>%
             mutate(avg = NA_real_, std_dev = NA_real_) %>% slice(0))
  }

  rank_tbl <- rank_tbl %>% select(-matches("\\(Team\\)$|^WSI|DST$")) %>%
    add_column(fantasypro_id = fp_ids, .before = 1)

  if(any(is.na(rank_tbl$Player)))
   rank_tbl$Player[is.na(rank_tbl$Player)] <- player_col[is.na(rank_tbl$Player)]

  if(any(is.na(rank_tbl$team)))
     rank_tbl$team[is.na(rank_tbl$team)] <- player_team[is.na(rank_tbl$team)]

  if(any(names(rank_tbl) == "Pos"))
    rank_tbl <- rank_tbl %>% extract(Pos, c("Pos", "Pos Rank"), "([A-Z]+)([0-9]+)")

  rank_tbl <- rank_tbl %>% janitor::clean_names()

  if(any(names(rank_tbl) == "fantasypro_id"))
    rank_tbl <- rank_tbl %>% add_column(id = ffanalytics:::id_col(rank_tbl$fantasypro_id, "fantasypro_id"), .before = 1)


  rank_tbl <- rank_tbl %>%
    mutate_at(vars(one_of(c("pos_rank", "bye", "best", "worst", "avg", "std_dev", "adp", "vs_adp"))), as.numeric)

  return(rank_tbl %>% `attr<-`("experts", num_experts))
}
