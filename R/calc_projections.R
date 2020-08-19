#' Calculate Weighted Standard Deviation
#'
#' Function to calculate weighted standard deviation.
#' @param x The observations to calculate the standard deviations from
#' @param w The weights associated with each observation.
#' @param na.rm If \code{TRUE}, then NA values will be removed.
weighted.sd <- function(x, w, na.rm = FALSE){
  sum.w <- sum(w, na.rm = na.rm)
  sum.w2 <- sum(w^2, na.rm = na.rm)
  mean.w <- sum(x * w,na.rm = na.rm) / sum(w, na.rm = na.rm)
  x.sd.w <- sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2))
  return(x.sd.w)
}

#' Wilcox Location Parameter
#'
#' Modified function to calculate Wilcox' Location paramenter
wilcox.loc <- function(vec, na.rm = FALSE){
  n <- length(vec)
  # If number of observations is less than 2 then we just return mean as location estimate
  if(n <= 2){
    return(mean(vec, na.rm = na.rm))
  }

  # Calculating the paired avagerages
  pairAvg <- sort(c(vec, combn(vec, 2, function(x)mean(x, na.rm = na.rm))))
  return(median(pairAvg, na.rm = na.rm))
}

#' Cohen's d
#'
#' Function to calculate Cohen's D value when testing effect size
cohens_d <- function(x, y, na.rm = TRUE) {
  if(na.rm){
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  n.x <- length(x)- 1
  n.y <- length(y)- 1
  mean.diff  <- abs(mean(x) - mean(y))
  if(n.x == 0 & n.y > 0) {
    common.sd <- sqrt(n.y * var(y)/n.y)
  } else if (n.x > 0 & n.y == 0){
    common.sd <- sqrt(n.x * var(x)/n.x)
  } else if (n.x > 0 & n.y  > 0) {
    common.sd <- sqrt((n.x * var(x) + n.y * var(y))/(n.x + n.y))
  } else {
    common.sd <- sd(c(x, y)) / 2
  }

  return(mean.diff/common.sd)
}

#' Default Weights for Projection Sources
#'
#' These are the weights that are used for each source when calculation weighted
#' averages and standard deviations if no weights are specified.
#' \code{c(CBS = 0.344, Yahoo = 0.400,  ESPN = 0.329,  NFL = 0.329,
#' FFToday = 0.379, NumberFire = 0.322, FantasyPros = 0.000,
#' FantasySharks = 0.327, FantasyFootballNerd = 0.000,
#' Walterfootball = 0.281, RTSports = 0.330,
#' FantasyData = 0.428, Fleaflicker = 0.428)}
default_weights <- c(CBS = 0.344, Yahoo = 0.400,  ESPN = 0.329,  NFL = 0.329,
                    FFToday = 0.379, NumberFire = 0.322, FantasyPros = 0.000,
                    FantasySharks= 0.327, FantasyFootballNerd = 0.000,
                    Walterfootball = 0.281, RTSports= 0.330,
                    FantasyData= 0.428, FleaFlicker = 0.428)

# Helper functions to calculate the quantiles and standard deviations for the
# source points. Used in the points_sd and confidence interval functions
quant_funcs <- list(average = quantile, robust = quantile,
                    weighted =  purrr::possibly(Hmisc::wtd.quantile, c(`5%` = NaN, `95%` = NaN)))
quant_args <- list(list(probs = c(0.05, 0.95)),  list(probs = c(0.05, 0.95)),
                   list(probs = c(0.05, 0.95), type = "i/n"))

get_quant <- function(pts, wt)invoke_map(quant_funcs, quant_args, x = pts, na.rm = TRUE, weights = wt)

sd_funcs <- list(average = function(x, w, na.rm)sd(x, na.rm = na.rm),
                 robust = function(x, w, na.rm)mad(x, na.rm = na.rm),
                 weighted = weighted.sd)
sd_args <- list(list(na.rm = TRUE), list(na.rm = TRUE), list(na.rm = TRUE))
get_sd <- function(pts, wt)invoke_map(sd_funcs, sd_args, x = pts, w = wt)


#' Calculate Source Points
#'
#' Function to calculate the projected points for each source.
#' @param data_result An output from the \link{scrape_data} function.
#' @param scoring_rules The scoring rules to be used.
source_points <- function(data_result, scoring_rules){
  scoring_tbl <- make_scoring_tbl(scoring_rules)

  long_result <- data_result %>%
    stats_by_category() %>%
    map(gather, "data_col", "stat_value", -c(id, data_src, pos)) %>%
    bind_rows()

  dst_pt_allow <- NULL

  if("dst" %in% names(scoring_rules))
    dst_pt_allow <- scoring_rules[[c("dst", "dst_pts_allowed")]]

  dst_bracket <- is.null(dst_pt_allow) & !is.null(scoring_rules$pts_bracket)

  dst_src <- long_result %>% slice(0) %>% add_column(points = 0)
  if(dst_bracket){
    dst_src <- long_result %>%  filter(data_col == "dst_pts_allowed") %>%
      mutate(points = ffanalytics:::dst_points(stat_value, scoring$pts_bracket))
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

  src_pts %>% inner_join(weight_tbl, by = "data_src") %>%
    group_by(id) %>%
    mutate(n_obs = n(),
           weight = if_else(n_obs == 1 & weight == 0, 1, weight)) %>%
    ungroup() %>% select(-n_obs) %>%
    split(.$pos) %>% map(~ split(.x, .x$id)) %>%
    modify_depth(2, ~ get_sd(.x$points, .x$weight)) %>% modify_depth(2, as.tibble) %>%
    modify_depth(1, bind_rows, .id = "id") %>% bind_rows(.id = "pos") %>%
    gather("avg_type", "sd_pts", -id, -pos)
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
    ungroup() %>% select(-n_obs) %>%
    split(.$pos) %>% map(~ split(.x, .x$id)) %>%
    modify_depth(2, ~ get_quant(.x$points, .x$weight)) %>% modify_depth(3, t) %>%
    modify_depth(3, as.tibble) %>% modify_depth(2, bind_rows, .id  = "avg_type") %>%
    modify_depth(1, bind_rows, .id = "id") %>% bind_rows(.id = "pos") %>%
    mutate(`5%` = ifelse(is.na(`5%`),` 5%`, `5%`)) %>% select(-` 5%`) %>%
    rename(floor = "5%", ceiling = "95%")
}

#' Aggregate Projected Stats
#'
#' This function aggregates the projected stats collected from each source with
#' the \link{scrape_data} function.
#' @param data_result An output from the \link{scrape_data} function.
#' @param src_weights A named vector with the weights from each source.
#' See \link{default_weights}
#' @export
aggregate_stats <- function(data_result, src_weights = NULL){

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

#' Calculate Projected Points
#'
#' This function calculates the projected points for each player based on the
#' aggregated stats from the \link{aggregate_stats} function. The resulting table
#' contains the projected points, the position rank and the points drop-off for
#' each player.
#' @param agg_stats An output from the \link{aggregate_stats} function
#' @param scoring_rules The scoring rules to be used.
projected_points <- function(agg_stats, scoring_rules){
  scoring_tbl <- make_scoring_tbl(scoring_rules)

  dst_pt_allow <- NULL

  if("dst" %in% names(scoring_rules))
    dst_pt_allow <- scoring_rules[[c("dst", "dst_pts_allowed")]]

  dst_bracket <- is.null(dst_pt_allow) & !is.null(scoring_rules$pts_bracket)

  dst_src <- agg_stats %>% slice(0) %>% add_column(points = 0)
  if(dst_bracket){
    dst_src <- agg_stats %>%  filter(data_col == "dst_pts_allowed") %>%
      mutate(points = ffanalytics:::dst_points(stat_value, scoring_rules$pts_bracket))
  }

  dst_agg <- dst_src %>% slice(0)

  if(dst_bracket){
    dst_agg <- agg_stats %>%  filter(data_col == "dst_pts_allowed") %>%
      mutate(points = ffanalytics:::dst_points(stat_value, scoring_rules$pts_bracket))
  }
  agg_stats  %>%
    inner_join(scoring_tbl, by = c("pos", "data_col")) %>%
    mutate(points = stat_value * points) %>%
    bind_rows(dst_agg) %>%
    group_by(pos, avg_type, id) %>%
    summarise(points = if_else(all(is.na(points)), NA_real_, sum(points, na.rm = TRUE))) %>%
    mutate(pos_rank = dense_rank(-points),
           drop_off =  points - (lead(points, order_by = pos_rank) +
                                   lead(points, 2, order_by = pos_rank)) /2 ) %>%
    ungroup()
}


#' Default VOR Baseline
#'
#' This is the default baseline that is used if not otherwise specified when
#' calculating VOR:
#' \code{c(QB = 13, RB = 35, WR = 36, TE = 13, K = 8, DST = 3, DL = 10, LB = 10, DB = 10)}
default_baseline <- c(QB = 13, RB = 35, WR = 36, TE = 13, K = 8, DST = 3, DL = 10, LB = 10, DB = 10)

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
    summarise(vor_base = mean(vor_var)) %>%  ungroup() %>%
    select(pos, vor_base) %>% inner_join(points_table, by = c("pos")) %>%
    rename(vor_var = !!vor_var) %>%
    mutate(vor = vor_var - vor_base,
           rank = dense_rank(-vor), !!vor_var := vor_var) %>%
    select(id, pos, vor, rank) %>% rename_if(is.numeric, funs(paste(vor_var, ., sep = "_"))) %>%
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

#' Default Threshold Values for Tiers
#'
#' These are the default threshold values used when applying Cohen's D values
#' to determine tiers:
#' \code{c(QB = 1, RB = 1, WR = 1, TE = 1, K = 1, DST = 0.1, DL = 1, DB = 1, LB = 1)}
default_threshold <-  c(QB = 1, RB = 1, WR = 1, TE = 1, K = 1, DST = 0.1, DL = 1, DB = 1, LB = 1)

#' Determine Tiers by Position
#'
#' This function determines tiers for each position by applying Cohen's D effect
#' size
#' @param data_tbl An output from the \link{projected_points} function
#' @param d_threshold THe thresholds to use when applying Cohens'd D function to
#' determine the tiers. If omitted then the \link{default_threshold} will be used.
#' @param src_points An output from the \link{source_points} function
set_tiers <- function(data_tbl, d_threshold = NULL, src_points){
  if(is.null(d_threshold))
    d_threshold <- default_threshold

  tier_tbl <- data_tbl %>% filter(pos %in% names(d_threshold)) %>%
    mutate(dthres = d_threshold[pos], tier = ifelse(pos_rank == 1, 1L, NA))

  repeat{
    before_na <- sum(is.na(tier_tbl$tier))
    tier_tbl <-
      tier_tbl %>% group_by(pos) %>% filter(tier == tier[which.max(tier)]) %>%
      summarise(tier_id = first(id, order_by = -points),
                cur_tier = as.integer(max(tier, na.rm = TRUE)),
                dthres= max(dthres, na.rm = TRUE)) %>%
      inner_join(tier_tbl %>% group_by(pos) %>% filter(is.na(tier)) %>%
                   summarise(max_id = first(id, order_by = -points)), by = "pos") %>%
      group_by(pos) %>%
      mutate(d_val = cohens_d(src_points[src_points$id == tier_id,]$points,
                              src_points[src_points$id == max_id,]$points),
             tier = ifelse(d_val > dthres, cur_tier + 1L, cur_tier)) %>%
      select(pos, id = max_id, new_tier = tier) %>% right_join(tier_tbl, by = c("pos", "id")) %>%
      mutate(tier = ifelse(is.na(tier) & !is.na(new_tier), new_tier, tier)) %>%
      select(-new_tier)

    after_na <- sum(is.na(tier_tbl$tier))
    if(before_na == after_na | after_na == 0)
      break
  }

  tier_tbl %>% select(-dthres) %>% ungroup()
}

#' Create a Projections Table
#'
#' This function creates the projections table based on the scraped data from the
#' \link{scrape_data} function. The output is a table containing the projected
#' points, confidence intervals, standard deviation for points, and if seasonal
#' data also the VOR values
#' @param data_result An output from the \link{scrape_data} function
#' @param scoring_rules The scoring rules to be used for calculations. See
#' \code{vignette("scoring_settings")} on how to define custom scoring settings.
#' If omitted then default \link{scoring} settings will be used.
#' @param src_weights A named vector defining the weights for each source to be
#' used in calculations. If omitted then \link{default_weights} will be used.
#' @param vor_baseline A named vector defineing the baseline to use for VOR
#' calculations. If omitted then the \link{default_baseline} will be used.
#' @param tier_thresholds The threshold values to be used when determining tiers.
#' If omitted then the \link{default_threshold} will be used.
#' @export
projections_table <- function(data_result, scoring_rules = NULL, src_weights = NULL,
                              vor_baseline = NULL, tier_thresholds = NULL){

    season <- attr(data_result, "season")
    week <- attr(data_result, "week")

    data_result <- purrr:::keep(data_result, ~ nrow(.) > 0) %>%
      `attr<-`(which = "season", season) %>%
      `attr<-`(which = "week", week)

    if(is.null(scoring_rules))
    scoring_rules <- scoring

  if(scoring_rules$rec$all_pos){
    lg_type <- scoring_rules$rec$rec %>% rep(length(data_result)) %>%
      `names<-`(names(data_result)) %>%
      map_chr(~ case_when(.x > 0.5 ~ "PPR", .x > 0  ~ "Half", TRUE ~ "Std"))
  } else {
    lg_type <- map(scoring_rules$rec[-which(names(scoring_rules$rec) == "all_pos")], `[[`, "rec") %>%
      purrr:::keep(~ !is.null(.x)) %>%
      map_chr(~ case_when(.x > 0.5 ~ "PPR", .x > 0  ~ "Half", TRUE ~ "Std"))

    lg_type[setdiff(names(data_result), names(lg_type))] < "Std"
  }

  data_list <- invoke_map(list(src_pts = source_points, agg_stats = aggregate_stats),
                          list(list(data_result = data_result, scoring_rules = scoring_rules),
                               list(data_result = data_result, src_weights = src_weights)))

  pts_uncertainty <- invoke_map(list(points_sd, confidence_interval),
                                src_pts = data_list$src_pts, weights = src_weights) %>%
    reduce(inner_join, by = c("pos", "id","avg_type"))

  out_df<- data_list$agg_stats %>%
    projected_points(scoring_rules) %>%
    inner_join(pts_uncertainty, by = c("pos", "id","avg_type")) %>%
    group_by(avg_type) %>%
    set_tiers(tier_thresholds, data_list$src_pts ) %>%
    ungroup()

  if(attr(data_result, "week") == 0){
    out_df <- out_df %>% split(.$avg_type) %>%
      map(add_vor, vor_baseline = vor_baseline) %>% bind_rows() %>%
      rename(rank = points_rank)
  }

  out_df %>%
    `attr<-`(which = "season", attr(data_result, "season")) %>%
    `attr<-`(which = "week", attr(data_result, "week")) %>%
    `attr<-`(which = "lg_type", lg_type)
}

#' Add ECR to the Projection Table
#'
#' This function will add the ECR values to the projetions table generated from
#' the \link{projections_table} function. It will add the positional ECR, the
#' standard deviation for the positional ECR, and if seasonal data also the
#' overal ECR value
#' @param projection_table An output from the \link{projections_table} function.
#' @export
add_ecr <- function(projection_table){
  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")
  ecr_pos <- lg_type %>%
    imap(~ scrape_ecr(rank_period = ifelse(week == 0, "draft", "week"),
                      position = .y, rank_type = .x)) %>%
    map(select, id, pos_ecr = avg, sd_ecr = std_dev) %>% bind_rows()

  projection_table <- left_join(projection_table, ecr_pos, by = "id")
  if(week == 0){
    lg_ov <- ifelse(any(lg_type == "PPR"), "PPR", ifelse(any(lg_type == "Half"), "Half", "Std"))
    ecr_overall <- scrape_ecr(rank_period = "draft", rank_type = lg_ov) %>%
      select(id, ecr = avg)
    projection_table <- left_join(projection_table, ecr_overall, by = "id")
  }
  projection_table  %>%
    `attr<-`(which = "season", season) %>%
    `attr<-`(which = "week", week) %>%
    `attr<-`(which = "lg_type", lg_type)
}

#' Add ADP to the Projections Table
#'
#' This function will add the ADP data to the projections table from the
#' \link{projections_table} function. It will add the average ADP from the sources
#' specfied, and the difference between the overall rank and ADP
#' @param projection_table An output from the \link{projections_table} function
#' @param sources Which ADP sources should be added. should be one or more of
#' \code{c("RTS", "CBS", "ESPN", "Yahoo", "NFL", "FFC")}
#' @export
add_adp <- function(projection_table,
                    sources = c("RTS", "CBS", "ESPN", "Yahoo", "NFL", "FFC")){

  sources <- match.arg(sources, several.ok = TRUE)

  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")

  if (week != 0){
    warning("ADP data is not available for weekly data", call. = FALSE)
    return(projection_table)
  }
  adp_tbl <- get_adp(sources, type = "ADP") %>% select(1, length(.)) %>%
    rename_at(length(.), function(x){return("adp")})

  projection_table <- left_join(projection_table, adp_tbl, by = "id") %>%
    mutate(adp_diff = rank - adp)

  projection_table  %>%
    `attr<-`(which = "season", season) %>%
    `attr<-`(which = "week", week) %>%
    `attr<-`(which = "lg_type", lg_type)
}

#' Add AAV to the Projections Table
#'
#' This function will add the AAV data to the projections table from the
#' \link{projections_table} function.
#' @param projection_table An output from the \link{projections_table} function
#' @param sources Which AAV sources should be added. should be one or more of
#' \code{c("RTS", "ESPN", "Yahoo", "NFL")}
#' @export
add_aav <- function(projection_table,
                    sources = c("RTS", "ESPN", "Yahoo", "NFL")){

  sources = match.arg(sources, several.ok = TRUE)

  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")

  if (week != 0){
    warning("AAV data is not available for weekly data", call. = FALSE)
    return(projection_table)
  }
  adp_tbl <- get_adp(sources, type = "AAV") %>% select(1, length(.)) %>%
    rename_at(length(.), function(x){return("aav")})

  projection_table <- left_join(projection_table, adp_tbl, by = "id")
  
  projection_table  %>%
    `attr<-`(which = "season", season) %>%
    `attr<-`(which = "week", week) %>%
    `attr<-`(which = "lg_type", lg_type)
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

  return(risk_value)

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


#' Add player information to the table
#'
#' Adds player information to the projections table
#' @export
add_player_info <- function(projection_table){
  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")

  select(player_table, id, first_name, last_name, team, position, age, exp) %>%
    inner_join(projection_table, by = "id") %>%
    `attr<-`(which = "season", season) %>%
    `attr<-`(which = "week", week) %>%
    `attr<-`(which = "lg_type", lg_type)
}
