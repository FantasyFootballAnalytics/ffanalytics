# Functions to be depreciated. Keeping for now but not exporting. These will be
# dropped in v3.1 or v3.2

#' Determine Tiers by Position
#'
#' This function determines tiers for each position by applying Cohen's D effect
#' size
#' @param data_tbl An output from the \link{projected_points} function
#' @param d_threshold THe thresholds to use when applying Cohens'd D function to
#' determine the tiers. If omitted then the \link{default_threshold} will be used.
#' @param src_points An output from the \link{source_points} function
set_tiers <- function(data_tbl, d_threshold = NULL, src_points){
  if(is.null(d_threshold)) {
    d_threshold <- default_threshold
  }

  tier_tbl <- data_tbl %>%
    filter(pos %in% names(d_threshold)) %>%
    mutate(dthres = d_threshold[pos],
           tier = ifelse(pos_rank == 1L, 1L, NA))

  repeat{
    before_na <- sum(is.na(tier_tbl$tier))
    tier_tbl <-
      tier_tbl %>%
      filter(tier == tier[which.max(tier)]) %>%
      group_by(pos) %>%
      summarise(tier_id = first(id, order_by = -points),
                cur_tier = as.integer(max(tier, na.rm = TRUE)),
                dthres= max(dthres, na.rm = TRUE)) %>%
      inner_join(tier_tbl %>% group_by(pos) %>% filter(is.na(tier)) %>%
                   summarise(max_id = first(id, order_by = -points)), by = "pos") %>%
      group_by(pos) %>%
      mutate(d_val = cohens_d(src_points[src_points$id == tier_id,]$points,
                              src_points[src_points$id == max_id,]$points),
             tier = ifelse(d_val > dthres, cur_tier + 1L, cur_tier)) %>%
      select(pos, id = max_id, new_tier = tier) %>%
      right_join(tier_tbl, by = c("pos", "id")) %>%
      mutate(tier = ifelse(is.na(tier) & !is.na(new_tier), new_tier, tier)) %>%
      select(-new_tier)

    after_na <- sum(is.na(tier_tbl$tier))
    if(before_na == after_na | after_na == 0)
      break
  }

  tier_tbl %>% select(-dthres) %>% ungroup()
}


#' Calculate VOR
#'
#' This function calculates the VOR based on an output from the \link{projected_points}
#' and if floor or ceiling VOR is requested with floor and ceiling added from the
#' \link{confidence_interval} function
#' @param points_table An output from the \link{projected_points} function and merged
#' with output from the the \link{projected_points} function and merged if floor or ceiling vor
#' is requested
#' @param vor_baseline The VOR Baseline to be used. If omitted then the
#' \link{default_baseline} will be used
#' @param vor_var One of \code{c("points", "floor", "ceiling")} indicating which
#' basis is used for the vor calculation
set_vor <- function(points_table, vor_baseline = NULL, vor_var = c("points", "floor", "ceiling")){
  if(is.null(vor_baseline))
    vor_baseline <- default_baseline

  vor_var <- match.arg(vor_var)

  vor_tbl <- select(points_table, "id", "pos", vor_var) %>%
    rename(vor_var = !!vor_var) %>% group_by(pos) %>%
    mutate(vor_rank = dense_rank(-vor_var), vor_base = vor_baseline[pos]) %>%
    filter(vor_rank >= vor_base - 1 &  vor_rank <= vor_base + 1)  %>%
    arrange(pos) %>%
    summarise(vor_base = mean(vor_var)) %>%  ungroup() %>%
    select(pos, vor_base) %>% inner_join(points_table, by = c("pos")) %>%
    rename(vor_var = !!vor_var) %>%
    mutate(vor = vor_var - vor_base,
           rank = dense_rank(-vor), !!vor_var := vor_var) %>%
    select(id, pos, vor, rank) %>%
    rename_with(~paste(vor_var, ., sep = "_"), where(is.numeric)) %>%
    ungroup()

  return(vor_tbl)
}

#' Calculate VOR for Points, Ceiling and Floor
#'
#' This function calculates VOR for projected points as well as the floor and
#' ceiling values.
#' @param tbl The output from the \link{projected_points} function that has
#' been merged with the output from  he \link{confidence_interval} function
#' @param vor_baseline The VOR baseline values to be used. If omitted then the
#' \link{default_baseline} will be used
add_vor <- function(tbl, vor_baseline = NULL){
  accumulate(c("points", "floor", "ceiling"),
             ~ inner_join(.x, set_vor(.x, vor_baseline, vor_var = .y),
                          by = c("id", "pos")),
             .init = tbl)[[4]]
}


#' Risk calculation based on two variables
#'
#' Calculation of risk is done by scaling the standard deviation variables
#' passed and averaging them before returning a measure with mean 5 and standard
#' deviation of 2
calculate_risk <- function(var1, var2){
  var1 <- as.numeric(var1)
  var2 <- as.numeric(var2)
  Z_var1 <- scale(var1)
  Z_var2 <- scale(var2)

  Z_var1[is.na(Z_var1)] <- Z_var2[is.na(Z_var1)]
  Z_var2[is.na(Z_var2)] <- Z_var1[is.na(Z_var2)]

  risk_value <- 2 * scale(rowMeans(data.frame(Z_var1, Z_var2), na.rm=TRUE)) + 5

  c(risk_value)

}

#' Add calculated risk to the table
#'
#' Calculation of risk is done by scaling the standard deviation variables
#' passed and averaging them before returning a measure with mean 5 and standard
#' deviation of 2
#' @export
add_risk <- function(projection_table){

  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")

  projection_table %>%
    group_by(pos) %>%
    # Calculate Risk values
    mutate(risk = calculate_risk(sd_pts, sd_ecr)) %>%
    ungroup() %>%
    `attr<-`(which = "season", season) %>%
    `attr<-`(which = "week", week) %>%
    `attr<-`(which = "lg_type", lg_type)
}


id_col = function(x, match_col){
  player_ids$id[match(x, player_ids[[match_col]])]
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


name_corrections <- list(
  "Ty Hilton" = "T.Y. Hilton",
  "Timothy Wright" = "Tim Wright",
  "Christopher Ivory" = "Chris Ivory",
  "Domanique Davis" = "Dominique Davis",
  "Ben Watson" = "Benjamin Watson",
  "Stevie Johnson" = "Steve Johnson",
  "Lesean McCoy" = "LeSean McCoy",
  "Luke Wilson" = "Luke Willson",
  "Thaddeus Lewis" = "Thad Lewis",
  "Walter Powell" = "Walt Powell",
  "Wilson VanHooser" = "Wilson Van Hooser",
  "Steve Hauschka" = "Steven Hauschka",
  "Stephen Hauschka" = "Steven Hauschka",
  "Daniel Herron" = "Dan Herron",
  "Robert Housler" = "Rob Housler",
  "Corey Philly Brown" = "Philly Brown",
  "Foswhitt Whittaker" =  "Fozzy Whittaker",
  "CJ Anderson" = "C.J. Anderson",
  "TY Hilton" = "T.Y. Hilton",
  "Boobie Dixon" = "Anthony Dixon",
  "EZ Nwachukwu" = "Uzoma Nwachukwu",
  "Dave Paulson" = "David Paulson",
  "Joe DonDuncan" = "Joe Don Duncan",
  "T Y Hilton" = "T.Y. Hilton",
  "Dqwell Jackson" = "DQwell Jackson",
  "Art Jones" = "Arthur Jones",
  "Navorro Bowman" =  "NaVorro Bowman",
  "Devante Parker" = "DeVante Parker",
  "AJ McCarron" = "A.J. McCarron",
  "TJ Yeldon" = "T.J. Yeldon",
  "CJ Prosise" = "C.J. Prosise",
  "AJ Green" = "A.J. Green",
  "David A. Johnson" = "David Johnson",
  "Adrian L. Peterson" = "Adrian Peterson",
  "Jonathan C. Stewart" = "Jonathan Stewart",
  "Chris D. Johnson" = "Chris Johnson",
  "Austin D. Johnson" = "Austin Johnson",
  "Steve L. Smith" = "Steve Smith",
  "Michael A. Thomas" = "Michael Thomas",
  "Devin A. Smith" = "Devin Smith",
  "Michael D. Thomas" = "Michael Thomas",
  "Robert Kelley" = "Rob Kelley",
  "Fairbairn Ka'imi" = "Ka'imi Fairbairn",
  "Will Lutz" = "Wil Lutz")




