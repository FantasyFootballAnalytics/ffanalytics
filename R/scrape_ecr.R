#' @export
scrape_ecr <- function(rank_period = c("draft", "weekly", "ros", "dynasty", "rookies"),
                       position = c("Overall", "QB", "RB", "WR", "TE", "K", "FLEX", "DST", "IDP",
                                    "DL", "LB", "DB"),
                       rank_type = c("Std", "PPR", "Half")) {

  if (rank_period == "weekly" & position == "Overall") {
    stop("Overall weekly ranks are not provided", call. = FALSE)
  }

  if (rank_period == "ros" & position == "IDP") {
    stop("Combined IDP ROS ranks are not provided", call. = FALSE)
  }

  ranks_url <- "https://www.fantasypros.com/nfl/rankings/"
  rk_type <- switch(rank_type, "Std" = "", "PPR" = "ppr", "Half" = "half-point-ppr")
  rk_pos <- tolower(position)

  if (rank_period == "draft") {
    if (position == "Overall") {
      rk_php <- ifelse(rank_type == "Std", "consensus-cheatsheets.php",
                       paste0(rk_type, "-cheatsheets.php"))
    } else {
      rk_php <- switch(rank_type,
                       "Std" = paste0(rk_pos, "-cheatsheets.php"),
                       ifelse(position %in% c("RB", "WR", "TE", "FLEX"),
                              paste0(rk_type, "-", rk_pos, "-cheatsheets.php"),
                              paste0(rk_pos, "-cheatsheets.php")))
    }
  } else if (rank_period == "weekly") {
    rk_php <- switch(rank_type,
                     "Std" = paste0(rk_pos, ".php"),
                     ifelse(position %in% c("RB", "WR", "TE", "FLEX"),
                            paste0(rk_type, "-", rk_pos, ".php"),
                            paste0(rk_pos, ".php")))
  } else if (rank_period == "ros") {
    rk_php <- switch(rank_type,
                     "Std" = paste0("ros-", rk_pos, ".php"),
                     ifelse(position %in% c("RB", "WR", "TE", "FLEX", "Overall"),
                            paste0("ros-", rk_type, "-", rk_pos, ".php"),
                            paste0("ros-", rk_pos, ".php")))
  } else if (rank_period == "dynasty") {
    rk_php <- paste0("dynasty-", rk_pos, ".php")
  } else {
    rk_php <- "rookies.php"
  }

  rank_page <- read_html(paste0(ranks_url, rk_php))

  rank_tab = rank_page %>%
    html_element("body") %>%
    html_elements("script:contains('var ecrData')") %>%
    html_text2()

  rank_tab = stringr::str_extract_all(rank_tab, "(\\{.*\\})")[[1]][1]
  rank_tab = strsplit(rank_tab, "\\},\\{|\\:\\[\\{")[[1]][-1]
  rank_tab = lapply(rank_tab, function(x) strsplit(x, ",", fixed = TRUE))

  rank_tab = rapply(rank_tab, function(x) grep("player(.*_id|_name)|rank_", x, value = TRUE), how = "list")
  rank_tab = rank_tab[sapply(rank_tab, lengths) > 0]
  rank_tab = unlist(rank_tab, recursive = FALSE)

  rank_tab = lapply(rank_tab, function(x) gsub('"', "", x, fixed = TRUE))
  rank_tab_names = gsub(":.*", "", rank_tab[[1]])
  rank_tab = lapply(rank_tab, function(x) gsub(".*:", "", x))
  rank_tab = lapply(rank_tab, function(x) `names<-`(x, rank_tab_names))
  rank_tab = lapply(rank_tab, `[`, c("player_id", "rank_ave", "rank_std"))

  bind_rows(rank_tab) %>%
    transmute(id = player_ids$id[match(player_id, player_ids$fantasypro_num_id)],
              avg = as.numeric(rank_ave),
              std_dev = as.numeric(rank_std))

}


