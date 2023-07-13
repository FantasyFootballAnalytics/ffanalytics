#' @export
scrape_ecr <- function(rank_period = c("draft", "weekly", "ros", "dynasty", "rookies"),
                       position = c("Overall", "QB", "RB", "WR", "TE", "K", "SUPERFLEX", "DST", "IDP",
                                    "DL", "LB", "DB"),
                       rank_type = c("Std", "PPR", "Half")) {

  rank_period = match.arg(rank_period, c("draft", "weekly", "ros", "dynasty", "rookies"))
  position = match.arg(position, c("Overall", "QB", "RB", "WR", "TE", "K", "SUPERFLEX", "DST", "IDP",
                                   "DL", "LB", "DB"))
  rank_type = match.arg(rank_type, c("Std", "PPR", "Half"))


  is_cache_format = rank_period %in% c("draft", "weekly")

  obj_name = paste("ECR", tools::toTitleCase(rank_period), position, rank_type)
  is_cached = obj_name %in% list_ffanalytics_cache(TRUE)$object
  file_name = sprintf("ecr_%s_%s_%s.rds", rank_period, tolower(position), tolower(rank_type))

  if(is_cached && is_cache_format) {
    out_df = get_cached_object(file_name)
    return(out_df)
  }


  if (rank_period == "weekly" & any(position == "Overall")) {
    stop("Overall weekly ranks are not provided", call. = FALSE)
  }

  if (rank_period == "ros" & any(position == "IDP")){
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

  rank_page <- rvest::read_html(paste0(ranks_url, rk_php))

  rank_tab = rank_page %>%
    rvest::html_element("body") %>%
    rvest::html_elements("script:contains('var ecrData')") %>%
    html_text2()

  rank_tab = sub(".*?var ecrData = (.+?;).*", "\\1", rank_tab)
  rank_tab = strsplit(rank_tab, "\\},\\{|\\:\\[\\{")[[1]][-1]
  rank_tab = lapply(rank_tab, function(x) strsplit(x, ",", fixed = TRUE))

  rank_tab = rapply(rank_tab, function(x) grep("player(.*_id|_name)|rank_", x, value = TRUE), how = "list")
  rank_tab = rank_tab[sapply(rank_tab, lengths) > 0]
  rank_tab = unlist(rank_tab, recursive = FALSE)

  rank_tab = lapply(rank_tab, function(x) gsub('"', "", x, fixed = TRUE))
  rank_tab_names = gsub(":.*", "", rank_tab[[1]])
  rank_tab = lapply(rank_tab, function(x) gsub(".*:", "", x))
  rank_tab = lapply(rank_tab, function(x) `names<-`(x, rank_tab_names))

  # rank_tab = lapply(rank_tab, `[`, c("player_id", "rank_ave", "rank_std"))

  out_df = bind_rows(rank_tab) %>%
    mutate(fantasypro_num_id = player_id) %>%
    transmute(id = get_mfl_id(fantasypro_num_id, player_name = player_name,
                              team = player_team_id, pos = player_position_id),
              avg = as.numeric(rank_ave),
              std_dev = as.numeric(rank_std),
              ecr_rank = as.integer(rank_ecr),
              ecr_min = as.integer(rank_min),
              ecr_max = as.integer(rank_max))

  if(is_cache_format) {
    cache_object(out_df, file_name)
  }
  out_df


}










