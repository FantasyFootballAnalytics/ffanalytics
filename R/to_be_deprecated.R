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
add_risk <- function(projection_table){

  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")

  projection_table %>%
    dplyr::group_by(pos) %>%
    dplyr::mutate(risk = calculate_risk(sd_pts, sd_ecr)) %>%
    dplyr::ungroup() %>%
    `attr<-`(which = "season", season) %>%
    `attr<-`(which = "week", week) %>%
    `attr<-`(which = "lg_type", lg_type)
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

#' Aggregate Projected Stats
#'
#' This function aggregates the projected stats collected from each source with
#' the \link{scrape_data} function.
#' @param data_result An output from the \link{scrape_data} function.
#' @param src_weights A named vector with the weights from each source.
#' See \link{default_weights}
aggregate_stats <- function(data_result, src_weights = NULL) {

  .Deprecated(msg = "`aggregate_stats()` is depreciated\naggregation is now done in the `projections_table()` function")
  if(is.null(src_weights)){
    data_src <- data_result %>% map(`[[`, "data_src") %>% reduce(union)
    src_weights <- default_weights[data_src]
  }

  weight_tbl <- src_weights %>% tibble(data_src = names(.), weight = .)

  data_result %>% stats_by_category() %>%
    map(inner_join, weight_tbl, by = "data_src") %>%
    map(gather, "data_col", "stat_value",
        -c(id, data_src, pos, weight)) %>%
    bind_rows() %>% group_by(pos, id, data_col) %>%
    summarise(robust = wilcox.loc(stat_value, na.rm = TRUE),
              average = mean(stat_value, na.rm = TRUE ),
              weighted = weighted.mean(stat_value, w = weight, na.rm = TRUE)) %>%
    gather("avg_type", "stat_value", -c(id, pos, data_col))
}

#' Calculate the Upper and Lower Limits for Projected Points
#'
#' This function calculates the ceiling and floor for projected points from
#' different sources based on quantiles
#' @param src_pts An output from the \link{source_points} function
#' @param weights A named vector with the weights from each source.
#' See \link{default_weights}
confidence_interval <- function(src_pts, weights = NULL){

  weight_tbl <- weights_from_src(src_pts, weights)

  src_pts %>% inner_join(weight_tbl, by = "data_src") %>%
    group_by(id) %>%
    mutate(n_obs = n(),
           weight = if_else(n_obs == 1 & weight == 0, 1, weight)) %>%
    ungroup() %>%
    select(-n_obs) %>%
    split(.$pos) %>%
    map(~ split(.x, .x$id)) %>%
    modify_depth(2, ~ get_quant(.x$points, .x$weight)) %>%
    modify_depth(3, t) %>%
    modify_depth(3, as_tibble) %>%
    modify_depth(2, bind_rows, .id  = "avg_type") %>%
    modify_depth(1, bind_rows, .id = "id") %>%
    bind_rows(.id = "pos") %>%
    #mutate(`5%` = ifelse(is.na(`5%`),` 5%`, `5%`)) %>% select(-` 5%`) %>%
    rename(floor = "5%", ceiling = "95%")
}


#' Calculate Projected Points
#'
#' This function calculates the projected points for each player based on the
#' aggregated stats from the \link{aggregate_stats} function. The resulting table
#' contains the projected points, the position rank and the points drop-off for
#' each player.
#' @param agg_stats An output from the \link{aggregate_stats} function
#' @param scoring_rules The scoring rules to be used.
projected_points <- function(agg_stats, scoring_rules) {
  scoring_tbl <- make_scoring_tbl(scoring_rules)

  dst_pt_allow <- NULL

  if("dst" %in% names(scoring_rules))
    dst_pt_allow <- scoring_rules[[c("dst", "dst_pts_allowed")]]

  dst_bracket <- is.null(dst_pt_allow) & !is.null(scoring_rules$pts_bracket)

  dst_src <- agg_stats %>% slice(0) %>% mutate(points = 0)
  if(dst_bracket){
    dst_src <- agg_stats %>%  filter(data_col == "dst_pts_allowed") %>%
      mutate(points = dst_points(stat_value, scoring_rules$pts_bracket))
  }

  dst_agg <- dst_src %>% slice(0)

  if(dst_bracket){
    dst_agg <- agg_stats %>% filter(data_col == "dst_pts_allowed") %>%
      mutate(points = dst_points(stat_value, scoring_rules$pts_bracket))
  }
  agg_stats  %>%
    inner_join(scoring_tbl, by = c("pos", "data_col")) %>%
    mutate(points = stat_value * points) %>%
    bind_rows(dst_agg) %>%
    group_by(pos, avg_type, id) %>%
    summarise(points = if_else(all(is.na(points)), NA_real_, sum(points, na.rm = TRUE))) %>%
    mutate(pos_rank = dense_rank(-points),
           drop_off = points - (lead(points, order_by = pos_rank) +
                                  lead(points, 2, order_by = pos_rank)) / 2) %>%
    ungroup()
}

#' Imputing values for missing data
#'
#' Differnt sources provide different data. The functions below will fill out
#' NA values for some sources with values based on data from other sources
#' @name Impute
NULL

#' @rdname Impute
impute_na_off <- function(tbl){
  if(length(tbl) == 0)
    return(tbl)

  type_col <- c(pass = "pass_yds", rush = "rush_yds", rec = "rec_yds")
  stat_type <- names(type_col)[which(type_col %in% names(tbl))]

  if("reg_tds" %in% names(tbl)){
    rrr_tds <- tbl %>%
      mutate(reg_tds = ifelse(is.na(reg_tds), rec_tds + rush_tds, reg_tds)) %>%
      val_from_rate(rec_tds, reg_tds) %>%
      val_from_calc(tbl, rush_tds, reg_tds)

    tbl <- tbl %>% select(-reg_tds) %>%
      rename(ru_tds = rush_tds, re_tds = rec_tds) %>%
      left_join(rrr_tds, by = c("id", "data_src")) %>%
      mutate(rush_tds = ifelse(is.na(rush_tds), ru_tds, rush_tds),
             rec_tds = ifelse(is.na(rec_tds), re_tds, rec_tds)) %>%
      select(-ru_tds, re_tds)

    if(stat_type == "rec")
      tbl <- tbl %>% select(-rush_tds, -reg_tds)

    if(stat_type == "rush")
      tbl <- tbl %>% select(-rec_tds, -reg_tds)
  } else {
    if(stat_type == "rec")
      tbl <- tbl %>% select(-rush_tds)

    if(stat_type == "rush")
      tbl <- tbl %>% select(-rec_tds)
  }

  names(tbl) <- gsub("(pass|rush|rec)_(.+)", "\\2", names(tbl))

  rec_targets <-  "tgt" %in% names(tbl)


  if("rec" %in% names(tbl)){
    tbl <- rename(tbl, comp = rec)
    if(rec_targets)
      tbl <- rename(tbl, att = tgt)
    else
      tbl <- mutate(tbl, att = comp)
  }

  if("avg" %in% names(tbl)){
    tbl$att <- calc_touch_from_avg(tbl$yds, tbl$avg, tbl$att)
  }


  if(!("att" %in% names(tbl))){
    tbl$att <- NA_real_
  }

  out_df <- tbl %>% val_from_rate(yds, att) %>% miss_rate(tbl, yds, att)

  if("comp" %in% names(tbl) & stat_type == "pass"){
    out_df <- out_df %>% val_from_calc(tbl, comp, att) %>%
      val_from_calc(tbl, int, att) %>%
      val_from_calc(tbl, tds, comp) %>%
      mutate(inc = att - comp, comp_pct = ifelse(att == 0, 0, comp / att))
  } else if("comp" %in% names(tbl) & stat_type == "rec"){
    out_df <- out_df %>% val_from_calc(tbl, comp, att) %>%
      val_from_calc(tbl, tds, att)
  } else {
    out_df <- out_df %>% val_from_calc(tbl, tds, att)
  }

  out_df <- mutate(out_df, avg = ifelse(att == 0, 0, yds / att))

  if(any(grepl("[0-9]{2,}_tds$", names(tbl)))){
    out_df <- out_df %>%
      dist_rate(tbl, tds, `09_tds`, `1019_tds`, `2029_tds`, `3039_tds`, `4049_tds`, `50_tds`)
  }

  if(any(names(tbl) == "1st" & stat_type != "rush"))
    out_df <-  out_df %>% val_from_calc(tbl, `1st`, comp)

  if(any(names(tbl) == "1st" & stat_type == "rush"))
    out_df <-  out_df %>% val_from_calc(tbl, `1st`, att)

  if(any(names(tbl) == "40_yds" & stat_type == "pass"))
    out_df <-  out_df %>% val_from_calc(tbl, `40_yds`, comp)

  if(any(names(tbl) == "40_yds" & stat_type != "pass"))
    out_df <-  out_df %>% val_from_calc(tbl, `40_yds`, att)

  if(any(names(tbl) == "40_tds"))
    out_df <-  out_df %>% val_from_calc(tbl, `40_tds`, tds)

  id_pt <- paste(stat_type, "id", sep = "_")
  src_pt <- paste(stat_type, "data_src", sep = "_")
  names(out_df) <- paste(stat_type, names(out_df), sep = "_") %>%
    gsub(id_pt, "id", ., fixed = TRUE) %>%
    gsub(src_pt, "data_src", ., fixed = TRUE) %>%
    gsub("rec_comp", "rec", ., fixed = TRUE) %>%
    gsub("rec_att", "rec_tgt", ., fixed = TRUE)

  if(!rec_targets & "rec_tgt" %in% names(out_df))
    out_df <- select(out_df, -rec_tgt)
  else if (rec_targets & "rec_tgt" %in% names(out_df)){
    out_df <- mutate(out_df, rec_yds_tgt = rec_yds / rec_tgt)
  }

  check_col <- function(x){is.na(x) | x ==0}
  tbl_cols <- select(out_df, -id, -data_src) %>% mutate(across(everything(), check_col)) %>% rowSums()

  return(out_df[tbl_cols < ncol(select(out_df, -id, - data_src)), ])
}


#' @rdname Impute
kick_impute <- function(kick_tbl){
  kick_cols <- names(kick_tbl)
  kick_dist <- c("fg_0019", "fg_2029", "fg_3039", "fg_4049", "fg_50")

  if(!("fg" %in% kick_cols) & all(kick_dist %in% kick_cols))
    kick_tbl <- mutate(kick_tbl, fg = sum_columns(kick_tbl, fg_0019, fg_2029, fg_3039, fg_4049, fg_50, na.rm = TRUE))

  if(!("fg_miss" %in% kick_cols) & all(c("fg_miss_0019", "fg_miss_2029", "fg_miss_3039", "fg_miss_4049", "fg_miss_50") %in% kick_cols))
    kick_tbl <- mutate(kick_tbl, fg_miss = sum_columns(kick_tbl, fg_miss_0019, fg_miss_2029, fg_miss_3039, fg_miss_4049, fg_miss_50, na.rm = TRUE))

  # Checking to see if there is a 10-19 fg column listed in the table. If there
  # is then it is then the value is moved to the 0-19 column and the 10-19 column
  # is removed.
  if("fg_1019" %in% kick_cols){
    if("fg_0019" %in% kick_cols){
      kick_tbl <- mutate(kick_tbl, fg_0019 = ifelse(is.na(fg_0019) & !is.na(fg_1019), fg_1019, fg_0019))
    } else {
      kick_tbl <- mutate(kick_tbl, fg_0019 = fg_1019)
    }

    kick_tbl <- select(kick_tbl, -fg_1019)
  }

  # Adding up the field goals by distance and checking to make sure that the
  # field goal column has the total, taking into account that some sources
  # does not distribute fg below 39 yards.
  if(all(kick_dist %in% kick_cols)){
    if(any(kick_cols == "fg_0039")){
      kick_tbl <- kick_tbl %>%
        mutate(fg_tot = sum_columns(kick_tbl, fg_0019, fg_2029, fg_0039, fg_3039, fg_4049, fg_50, na.rm = TRUE),
               fg = ifelse(!is.na(fg_tot) & is.na(fg), fg_tot, fg))
    } else {
      kick_tbl <- kick_tbl %>%
        mutate(fg_tot = sum_columns(kick_tbl, fg_0019, fg_2029, fg_3039, fg_4049, fg_50, na.rm = TRUE),
               fg = ifelse(!is.na(fg_tot) & is.na(fg), fg_tot, fg))
    }
    kick_tbl <- kick_tbl %>% select(-fg_tot)
  } else if(all(c("fg_0039", "fg_4049", "fg_50") %in% kick_cols)){
    kick_tbl <- kick_tbl %>%
      mutate(fg_tot = sum_columns(kick_tbl, fg_0039, fg_4049, fg_50, na.rm = TRUE),
             fg = ifelse(!is.na(fg_tot) & is.na(fg), fg_tot, fg)) %>%
      select(-fg_tot)
  }

  # Making sure that attempts are calculated if only made and missed are present
  if(all(c("xp", "xp_miss") %in% kick_cols) & !("xp_att" %in% kick_cols)){
    kick_tbl <- mutate(kick_tbl, xp_att = ifelse(!is.na(xp) & !is.na(xp_miss), xp + xp_miss, as.numeric(NA)))
  }

  if(all(c("fg", "fg_miss") %in% kick_cols) & !("fg_att" %in% kick_cols)){
    kick_tbl <- mutate(kick_tbl, fg_att = ifelse(!is.na(fg) & !is.na(fg_miss), fg + fg_miss, as.numeric(NA)))
  }


  # Imputing values for attempts and made, for both field goals and XP and
  # calulating the missed field goals and XP.
  if("fg_att" %in% kick_cols){
    kicking <- kick_tbl %>% val_from_rate(fg, fg_att) %>% mutate(fg_miss = fg_att - fg)
  } else {
    kicking <- kick_tbl %>% select(id, data_src, fg)
  }

  if("xp_att" %in% kick_cols){
    kicking <- kicking %>%
      inner_join(val_from_rate(kick_tbl, xp, xp_att), by = c("id", "data_src")) %>%
      mutate(xp_miss = xp_att - xp)
  } else {
    kicking <- kicking %>%
      inner_join(select(kick_tbl, id, data_src, xp), by = c("id", "data_src"))
  }

  # Imputing values for field goals by distance.
  if(all(kick_dist %in% kick_cols)){
    if(any(kick_cols == "fg_0029")){
      kick_tbl <- kick_tbl %>%
        mutate(tot_0029 = sum_columns(kick_tbl, fg_0019, fg_2029),
               fg_0029 = ifelse(!is.na(tot_0029) & is.na(fg_0029), tot_0029, fg_0029)) %>%
        select(-tot_0029)

      kicking <- kicking %>% inner_join(select(kick_tbl, id, data_src, fg_0029), by = c("id", "data_src")) %>%
        dist_rate(kick_tbl, fg_0029, fg_0019, fg_2029) %>% select(-fg_0029)

      kick_tbl <- select(kick_tbl, -fg_0019, -fg_2029) %>%
        inner_join(select(kicking, id, data_src, fg_0019, fg_2029), by = c("id", "data_src"))

      kicking <- kicking %>% select(-fg_0019, -fg_2029) %>%
        dist_rate(kick_tbl, fg, fg_0019, fg_2029, fg_3039, fg_4049, fg_50)

    }

    if(any(kick_cols == "fg_0039")){
      kick_tbl <- kick_tbl %>%
        mutate(tot_0039 = sum_columns(kick_tbl, fg_0019, fg_2029, fg_3039),
               fg_0039 = ifelse(!is.na(tot_0039) & is.na(fg_0039), tot_0039, fg_0039)) %>%
        select(-tot_0039)

      kicking <- kicking %>% inner_join(select(kick_tbl, id, data_src, fg_0039), by = c("id", "data_src")) %>%
        dist_rate(kick_tbl, fg_0039, fg_0019, fg_2029, fg_3039) %>% select(-fg_0039)

      kick_tbl <- select(kick_tbl, -fg_0019, -fg_2029, -fg_3039) %>%
        inner_join(select(kicking, id, data_src, fg_0019, fg_2029, fg_3039), by = c("id", "data_src"))

      kicking <- kicking %>% select(-fg_0019, -fg_2029, -fg_3039) %>%
        dist_rate(kick_tbl, fg, fg_0019, fg_2029, fg_3039, fg_4049, fg_50)
    }

    if(!any(kick_cols %in% c("fg_0029", "fg_0039"))){
      kicking <- kicking %>%
        dist_rate(kick_tbl, fg, fg_0019, fg_2029, fg_3039, fg_4049, fg_50)
    }

    if("fg_miss" %in% names(kick_tbl)){
      if(!all(c("fg_miss_0019", "fg_miss_2029", "fg_miss_3039", "fg_miss_4049", "fg_miss_50") %in% names(kick_tbl))){
        kick_tbl <- kick_tbl %>%
          mutate(fg_miss_0019 = NA_real_, fg_miss_2029 = NA_real_, fg_miss_3039 = NA_real_, fg_miss_4049 = NA_real_,
                 fg_miss_50 = NA_real_)
      }

      if("fg_miss" %in% intersect(names(kicking), names(kick_tbl)))
        kick_tbl <- select(kick_tbl, -fg_miss)

      kicking <- kicking %>% inner_join(select(kick_tbl, id, data_src, matches("^fg_miss")), by = c("id", "data_src")) %>%
        mutate(fg_miss_0019 = if_else(is.na(fg_miss_0019), fg_miss * fg_0019 / fg, fg_miss_0019),
               fg_miss_2029 = if_else(is.na(fg_miss_2029), fg_miss * fg_2029 / fg, fg_miss_2029),
               fg_miss_3039 = if_else(is.na(fg_miss_3039), fg_miss * fg_3039 / fg, fg_miss_3039),
               fg_miss_4049 = if_else(is.na(fg_miss_4049), fg_miss * fg_4049 / fg, fg_miss_4049),
               fg_miss_50 = if_else(is.na(fg_miss_50), fg_miss * fg_50 / fg, fg_miss_50)
        )
    }
  }

  return(kicking)
}



calc_touch_from_avg <- function(yds, avg, touch){
  sel <- yds != 0 & !is.na(yds) & avg != 0 & !is.na(avg)
  touch[sel] <- yds[sel] / avg[sel]
  touch
}





get_stat_cols <- function(tbl, match_pattern){
  id_cols <- select(tbl, id, data_src)

  stat_cols <- select(tbl, matches(match_pattern))

  check_cols <- stat_cols %>% is.na() %>% rowSums()

  check_sums <-  stat_cols %>%
    mutate(across(everything(), as.numeric)) %>%
    rowSums(., na.rm = TRUE)

  if(length(stat_cols) > 0){
    stat_tbl <- bind_cols(id_cols, stat_cols)
    stat_tbl <- stat_tbl[check_cols < length(stat_cols) & check_sums != 0, ]
    return(stat_tbl)
  }
  return(data.frame())
}

sum_columns <- function(tbl, ..., na.rm = FALSE){
  sum_vars <- quos(...)
  select(tbl, !!! sum_vars) %>% rowSums(na.rm = na.rm)
}

miss_rate <- function(tbl_rate, tbl_raw, grp_var, avg_var){
  fv <- enquo(grp_var)
  av <- enquo(avg_var)

  res <- tbl_raw %>%
    transmute(var_lim = ceiling(!!fv), var_tgt = !!av) %>%
    group_by(var_lim) %>% summarise(avg = mean(var_tgt, na.rm = TRUE)) %>%
    filter(!is.na(var_lim))

  var_name <- quo_name(av)

  res <- tbl_rate %>% mutate(var_lim = ceiling(!!fv)) %>%
    left_join(res, by = "var_lim") %>%
    mutate(!!var_name := ifelse(is.na(!!av), avg, !!av)) %>%
    select(-var_lim, -avg)

  return(res)
}


rate_stat <- function(x, y)ifelse(y != 0, x / y, 0)

from_rate <- function(var1, var2, rate) ifelse(is.na(var1) & rate > 0, var2 * rate, var1)

nan_zero <- function(x) ifelse(is.nan(x) | is.infinite(x), 0, x)

val_from_rate <- function(tbl, var_1, var_2){
  v1 <- enquo(var_1)
  v2 <- enquo(var_2)

  var_tbl <- select(tbl, id, !!v1, !!v2, data_src) %>% filter(!is.na(id))

  miss_var <- var_tbl[!complete.cases(var_tbl),]

  if(nrow(miss_var) > 0){
    tvars <- names(var_tbl)[2:3]
    miss_tbl <- var_tbl[complete.cases(var_tbl),] %>%
      transmute(id, rt = rate_stat(!!v1, !!v2)) %>%
      group_by(id) %>%
      summarise(rate_var = mean(rt, na.rm = TRUE)) %>%
      inner_join(x=miss_var, by = "id") %>%
      mutate(!!tvars[1] := nan_zero(from_rate(!!v1, !!v2, rate_var)),
             !!tvars[2] := nan_zero(from_rate(!!v2, !!v1, 1/rate_var))) %>%
      select(-rate_var)

    var_tbl <- bind_rows(var_tbl[complete.cases(var_tbl),], miss_tbl)
  }

  return(var_tbl)
}

val_from_calc <- function(calc_tbl, stat_tbl, var_1, var_2){
  v1 <- enquo(var_1)
  v2 <- enquo(var_2)

  stat_tbl <- select(stat_tbl, id, data_src, !!v1)

  stat_var <- names(stat_tbl)[length(stat_tbl)]

  if(any(names(calc_tbl) == stat_var))
    calc_tbl <- select(calc_tbl, - !!v1)

  calc_vars <- paste0("^", paste(setdiff(names(calc_tbl), c("id", "data_src")), collapse = "$|^"), "$")

  if(nrow(stat_tbl[!complete.cases(stat_tbl),]) > 0){
    var_tbl <- calc_tbl %>%
      inner_join(stat_tbl, by = c("id", "data_src")) %>%
      val_from_rate(!!v1, !!v2) %>%
      select(-matches(calc_vars)) %>%
      right_join(calc_tbl, by = c("id", "data_src"))
  } else {
    var_tbl <- stat_tbl %>% inner_join(calc_tbl, by = c("id", "data_src"))
  }
  return(var_tbl)
}

dist_rate <- function(rate_tbl, stat_tbl, base_var, ...){
  b_var <- enquo(base_var)
  d_var <- quos(...)

  for(i in seq_along(d_var)){
    rate_tbl <- rate_tbl %>% val_from_calc(stat_tbl, !!d_var[[i]], !!b_var)
  }

  return(rate_tbl)
}


#' Scrape data for a specific position from a single source
scrape_source <- function(src, season, week, position){
  src_type <- intersect(c("html_source", "json_source", "xlsx_source"), class(src))
  cat("Scraping", position, "projections from \n", src$get_url(season, week, position), "\n")
  src_res <- switch(src_type,
                    "html_source" = src$open_session(season, week, position)$scrape(),
                    src$scrape(season, week, position))
  return(src_res)
}




#' Organize scraped data by stat category
#'
#' Breaks the data into the following categories, and imputes missing values
#' based on rate stats across sources. For example, if passing attemts are missing
#' then they will be imputed by calculating pass yards per attempt across sources
#' and dividing the pass yards for the source with pass yards per attempt across
#' sources.
#' \describe{
#'   \item{pass}{Passing stats}
#'   \item{rush}{Rushing stats}
#'   \item{rec}{Receiving stats}
#'   \item{misc}{Miscellaneous offensive stats}
#'   \item{ret}{Return stats}
#'   \item{kick}{Kicking stats}
#'   \item{dst}{DST stats}
#'   \item{idp}{IDP stats}
#' }
#' @include impute_funcs.R
stats_by_category <- function(data_results){
  rm_dupe_rows <- function(t)t[!duplicated(t),]
  no_rows <- function(t)(nrow(t) == 0)

  scrape_pos <-  data_results %>% map(`[`, c("id", "data_src")) %>%
    bind_rows(.id = "pos")

  data_cat <- list(tibble())

  if(any(names(data_results) %in% c("QB", "RB", "WR", "TE"))){
    data_cat <- map(list(pass = "^pass_", rush = "^rush_|^reg|rec_tds",
                         rec = "^rec|^reg|rush_tds"),
                    lapply, X = data_results, FUN = get_stat_cols) %>%
      map(bind_rows) %>% map(rm_dupe_rows) %>%
      map(group_by, id, data_src) %>%
      map(summarise_at, vars(-one_of("id", "data_src")), mean, na.rm = TRUE) %>%
      map(ungroup) %>%
      map(impute_na_off)

    data_cat <- append(data_cat,
                       map(list(misc = "^games$|^fum|^sac|^two", ret = "^ret" ),
                           lapply, X = data_results, FUN = get_stat_cols) %>%
                         map(bind_rows) %>% discard(~ nrow(.) == 0) %>% map(rm_dupe_rows)  %>%
                         map(group_by, id, data_src) %>%
                         map(summarise_at, vars(-one_of("id", "data_src")), mean, na.rm = TRUE) %>%
                         map(ungroup))
  }

  if("K" %in% names(data_results)){
    data_cat$kick <- map(data_results, get_stat_cols,
                         match_pattern = "^fg|^xp" ) %>%
      bind_rows() %>%  group_by(id, data_src) %>%
      summarise_at(vars(-one_of("id", "data_src")), mean, na.rm = TRUE) %>%
      ungroup() %>% kick_impute()
  }

  if("DST" %in% names(data_results)){
    data_cat$dst <- map(data_results, get_stat_cols,
                        match_pattern = "^dst" ) %>%
      bind_rows()
  }

  if(any(names(data_results) %in% c("DL", "LB", "DB"))){
    data_cat$idp <- map(data_results, get_stat_cols,
                        match_pattern = "^idp" ) %>%
      bind_rows() %>% rm_dupe_rows()  %>% group_by(id, data_src) %>%
      summarise_at(vars(-one_of("id", "data_src")), mean, na.rm = TRUE) %>%
      ungroup()
  }

  data_cat <- compact(data_cat) %>%
    map(inner_join, scrape_pos, by = c("id", "data_src")) %>%
    map(filter, !is.na(id))

  return(data_cat)
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


#' Cohen's d
#'
#' Function to calculate Cohen's D value when testing effect size
cohens_d <- function(x, y, na.rm = TRUE) {
  if(na.rm && (anyNA(x) || anyNA(y))) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  n_x <- length(x) - 1
  n_y <- length(y) - 1
  mean_diff  <- abs(mean.default(x) - mean.default(y))
  if(n_x == 0 & n_y > 0) {
    common_sd <- sqrt(n_y * var(y) / n_y)
  } else if (n_x > 0 & n_y == 0){
    common_sd <- sqrt(n_x * var(x) / n_x)
  } else if (n_x > 0 & n_y  > 0) {
    common_sd <- sqrt((n_x * var(x) + n_y * var(y)) / (n_x + n_y))
  } else {
    common_sd <- sd(c(x, y)) / 2L
  }

  mean_diff / common_sd
}


# Helper functions to calculate the quantiles and standard deviations for the
# source points. Used in the points_sd and confidence interval functions
quant_funcs <- list(average = quantile,
                    robust = quantile,
                    weighted = whdquantile)
quant_args <- list(list(probs = c(0.05, 0.95)),  list(probs = c(0.05, 0.95)),
                   list(probs = c(0.05, 0.95)))

get_quant <- function(pts, wt) invoke_map(quant_funcs, quant_args, x = pts, na.rm = TRUE, w = wt)

sd_funcs <- list(average = function(x, w, na.rm) sd(x, na.rm = na.rm),
                 robust = function(x, w, na.rm) mad(x, na.rm = na.rm),
                 weighted = weighted.sd)

get_sd <- function(pts, wt) {
  length_pts = length(pts[!is.na(pts)])

  if(length_pts <= 1) {
    return(list(average = NA, robust = NA, weighted = NA))
  }

  lapply(sd_funcs, function(fun) {
    fun(pts, wt, na.rm = TRUE)
  })
}


#' Calculate Source Points
#'
#' Function to calculate the projected points for each source.
#' @param data_result An output from the \link{scrape_data} function.
#' @param scoring_rules The scoring rules to be used.
source_points_old <- function(data_result, scoring_rules){

  scoring_tbl <- make_scoring_tbl(scoring_rules)

  long_result <- data_result %>%
    stats_by_category() %>%
    map(gather, "data_col", "stat_value", -c(id, data_src, pos)) %>%
    bind_rows()

  dst_pt_allow <- NULL

  if("dst" %in% names(scoring_rules))
    dst_pt_allow <- scoring_rules[[c("dst", "dst_pts_allowed")]]

  dst_bracket <- is.null(dst_pt_allow) & !is.null(scoring_rules$pts_bracket)

  dst_src <- long_result %>% slice(0) %>% mutate(points = 0)
  if(dst_bracket){
    dst_src <- long_result %>%  filter(data_col == "dst_pts_allowed") %>%
      mutate(points = dst_points(stat_value, scoring$pts_bracket))
  }

  long_result %>%
    inner_join(scoring_tbl, by = c("pos", "data_col")) %>%
    mutate(points = stat_value * points)  %>%
    bind_rows(dst_src) %>%
    group_by(pos, data_src, id) %>%
    summarise(points = sum(points, na.rm = TRUE)) %>% ungroup()
}


# Generate weights from a source points table if no weights are given
weights_from_src <- function(src_pts, weights = NULL){
  if(is.null(weights)){
    weights <- default_weights[unique(src_pts$data_src)]
  }
  weights %>% tibble(data_src = names(.), weight = .)
}

#' Calculate Standard Deviations for Projected Points
#'
#' This function calculates the standard deviaion for projected points from
#' different sources
#' @param src_pts An output from the \link{source_points} function
#' @param weights A named vector with the weights from each source.
#' See \link{default_weights}
points_sd <- function(src_pts, weights = NULL){

  weight_tbl <- weights_from_src(src_pts, weights)

  src_pts %>%
    inner_join(weight_tbl, by = "data_src") %>%
    group_by(id) %>%
    mutate(n_obs = n(),
           weight = if_else(n_obs == 1 & weight == 0, 1, weight)) %>%
    ungroup() %>%
    select(-n_obs) %>%
    split(.$pos) %>%
    map(~ split(.x, .x$id)) %>%
    modify_depth(2, ~ get_sd(.x$points, .x$weight)) %>%
    modify_depth(2, as_tibble) %>%
    modify_depth(1, bind_rows, .id = "id") %>%
    bind_rows(.id = "pos") %>%
    gather("avg_type", "sd_pts", -id, -pos)
}




