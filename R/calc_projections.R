#' Calculate Weighted Standard Deviation
#'
#' Function to calculate weighted standard deviation.
#' @param x The observations to calculate the standard deviations from
#' @param w The weights associated with each observation.
#' @param na.rm If \code{TRUE}, then NA values will be removed.
weighted.sd <- function(x, w, na.rm = FALSE){
  len_x = length(x)

  if(len_x == 1 || len_x != length(w) || all(is.na(w))) {
    return(NA)
  }

  sum.w <- sum(w, na.rm = na.rm)
  sum.w2 <- sum(w^2, na.rm = na.rm)
  mean.w <- sum(x * w,na.rm = na.rm) / sum.w
  sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2))
}

#' Altered MAD function
#'
#' NA for length = 1 & an ignored weight function
mad2 = function(x, center = median(x), constant = 1.4826, na.rm = FALSE,
                low = FALSE, high = FALSE, w) {
  if(length(x) %in% c(0, 1)) {
    return(NA)
  } else {
    mad(x, center, constant, na.rm, low, high)
  }
}

#' Weighted Harrell-Davis quantile estimator
#'
#' From: https://aakinshin.net/posts/weighted-quantiles/
whdquantile <- function(x, w = NA, probs, na.rm) { #na.rm is ignored

  cdf.gen <- function(n, p) {
    function(cdf.probs) {
      pbeta(cdf.probs, (n + 1L) * p, (n + 1L) * (1L - p))
    }
  }

  w_zero = !(w <= 0 | is.na(w))
  x_non_na = !is.na(x)
  x = x[w_zero & x_non_na]
  w = w[w_zero & x_non_na]

  length_x = length(x)
  length_w = length(w)
  if (length_x <= 1L) {
    return(NA)
  }
  if (length_w == 0L) {
    w = rep(1L, length_x)
  }
  if (length_x != length_w) {
    message("Length of x != length of w. NA returned")
    return(NA)
  }

  nw <- sum(w)^2L / sum(w^2L) # Kish's effective sample size
  idx <- order(x)
  x <- x[idx]
  w <- w[idx]

  w <- w / sum(w)
  cdf.probs <- cumsum(c(0, w))
  names(probs) = sprintf("%1.0f%%", probs * 100)

  vapply(probs, function(p) {
    cdf <- cdf.gen(nw, p)
    q <- cdf(cdf.probs)
    w <- tail(q, -1L) - head(q, -1L)
    sum(w * x)
  }, numeric(1L))

}

#' Wilcox Location Parameter
#'
#' Modified function to calculate Wilcox' Location paramenter
wilcox.loc <- function(vec, na.rm = FALSE, w = NULL){

  # If number of observations is less than 2 then we just return mean as location estimate
  if(length(vec) <= 2){
    return(mean(vec, na.rm = na.rm))
  }

  # Calculating the paired averages
  pairAvg <- sort(c(vec, combn(vec, 2, function(x) sum(x, na.rm = na.rm) / 2)))
  median.default(pairAvg, na.rm = na.rm)
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
quant_funcs <- list(average = quantile,
                    robust = quantile,
                    weighted = whdquantile)
quant_args <- list(list(probs = c(0.05, 0.95)),  list(probs = c(0.05, 0.95)),
                   list(probs = c(0.05, 0.95)))

get_quant <- function(pts, wt)invoke_map(quant_funcs, quant_args, x = pts, na.rm = TRUE, w = wt)

sd_funcs <- list(average = function(x, w, na.rm)sd(x, na.rm = na.rm),
                 robust = function(x, w, na.rm)mad(x, na.rm = na.rm),
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
    dst_agg <- agg_stats %>% filter(data_col == "dst_pts_allowed") %>%
      mutate(points = ffanalytics:::dst_points(stat_value, scoring_rules$pts_bracket))
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

    if(is.null(scoring_rules)) {
      scoring_rules <- scoring
    }


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
    map(select, id, pos_ecr = avg, sd_ecr = std_dev) %>%
    bind_rows()

  projection_table <- left_join(projection_table, ecr_pos, by = "id")
  if(week == 0){
    lg_ov <- ifelse(any(lg_type == "PPR"), "PPR", ifelse(any(lg_type == "Half"), "Half", "Std"))
    ecr_overall <- scrape_ecr(rank_period = "draft", rank_type = lg_ov, position = "Overall") %>%
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

#' New, lighter, projections_table function
#'
#' Testing & improving now TO replace the current function in the next major app update
projections_table2 = function(data_result, scoring_rules = NULL, src_weights = NULL,
                              vor_baseline = NULL, tier_thresholds = NULL,
                              avg_type = c("average", "robust", "weighted")) {

  # Filling in missing arguments
  if(is.null(scoring_rules)) {
    scoring_rules = scoring
  }
  if(is.null(src_weights)) {
    src_weights = default_weights
  }
  if(is.null(vor_baseline)) {
    vor_baseline = default_baseline
  }
  if(is.null(tier_thresholds)) {
    tier_thresholds = default_threshold
  }

  # Grabbing attributes
  season = attr(data_result, "season")
  week = attr(data_result, "week")

  if(scoring_rules$rec$all_pos){
    lg_type = rep(scoring_rules$rec$rec, length(data_result))
    lg_type = case_when(lg_type > .5 ~ "PPR",
                        lg_type > 0 ~ "Half",
                        TRUE ~ "Std")
    names(lg_type) = names(data_result)
  } else {
    lg_type = lapply(scoring_rules$rec[names(scoring_rules$rec) != "all_pos"], `[[`, "rec")
    lg_type = Filter(Negate(is.null), lg_type)
    lg_type = vapply(lg_type, function(x) if(x > .5) "PPR" else if(x > 0) "Half" else "Std", character(1L))
    lg_type[setdiff(names(data_result), names(lg_type))] < "Std"
  }


  # Setting up the scoring table
  scoring_l = vector("list", length(data_result))
  names(scoring_l) = names(data_result)
  all_pos_idx = unlist(lapply(scoring_rules, `[[`, "all_pos"))
  l_pts_bracket = scoring_rules$pts_bracket
  scoring_rules$pts_bracket = NULL

  if(all(all_pos_idx)) { # if no custom scoring
    scoring_table = dplyr::tibble(
      category = rep(names(scoring_rules), times = lengths(scoring_rules)),
      column = sub(".*?\\.", "", names(unlist(scoring_rules, recursive = FALSE))),
      val = unlist(scoring_rules)
    )
    for(pos in names(scoring_l)) {
      scoring_l[[pos]] = scoring_table
    }
  } else { # if there is custom scoring
    custom_cols = names(all_pos_idx[!all_pos_idx])
    for(pos in names(scoring_l)) {
      temp_scoring = scoring_rules

      for(col in custom_cols) { # if one column is custom, use it, else make NULL
        if(pos %in% names(temp_scoring[[col]])) {
          temp_scoring[[col]] = temp_scoring[[col]][[pos]]
        } else {
          temp_scoring[[col]] = NULL
        }
      }

      scoring_table = dplyr::tibble(
        category = rep(names(temp_scoring), times = lengths(temp_scoring)),
        column = sub(".*?\\.", "", names(unlist(temp_scoring, recursive = FALSE))),
        val = unlist(temp_scoring)
      )
      scoring_l[[pos]] = scoring_table
    }
  }

  # temp until fix on walterfootball scrape
  if("QB" %in% names(data_result) && "rec_tds" %in% names(data_result$QB)) {
    data_result$QB$rush_tds = coalesce(data_result$QB$rush_tds, data_result$QB$rec_tds)
    data_result$QB$rec_tds = NULL
  }

  # Imputing values
  data_result = sapply(names(data_result), function(pos) {
    df = data_result[[pos]]
    df = df[!is.na(df$id), ]
    df$weights = src_weights[df$data_src]
    df_names = names(df)

    # Kickers are weird
    if(pos == "K") {
      mis_cols = c("fg_miss_0019", "fg_miss_2029", "fg_miss_3039", "fg_miss_4049", "fg_miss_50")
      fg_cols = c("fg_0019", "fg_2029", "fg_3039", "fg_4049", "fg_50", "fg_0039")

      if(!("fg_miss" %in% df_names) && all(mis_cols %in% df_names)) {
        df$fg_miss = rowSums(df[mis_cols], na.rm = TRUE)
      }
      if("fg" %in% df_names && anyNA(df$fg)) {
        tot_cols = intersect(df_names, fg_cols)
        idx = is.na(df$fg)
        df$fg[idx] = rowSums(df[idx, tot_cols], na.rm = TRUE)
      }
      if(!"xp_att" %in% df_names) {
        if(!"xp_miss" %in% df_names) {
          df$xp_att = NA
        } else {
          df$xp_att = df$xp + df$xp_miss # if either are NA it returns NA
        }
      }
      if(!"fg_miss" %in% df_names) {
        df$fg_miss = NA
      } else {
        idx = is.na(df$fg_att)
        df$fg_att[idx] = df$fg[idx] + df$fg_miss[idx]
      }

      if("fg_pct" %in% df_names) {
        idx = is.na(df$fg_att)
        df$fg_att[idx] = df$fg[idx] / (df$fg_pct[idx] * .01)
      }
    }

    # intersecting column names (that have a non-zero scoring value)
    impute_cols = intersect(df_names, scoring_table$column[scoring_table$val != 0])
    impute_cols = names(Filter(anyNA, df[impute_cols])) # only grabbing columns with missing values

    # Commented out for now to match old projections_table() function results
    # if(pos == "DST") {
    #   if(week == 0) {
    #     score_pts_bracket(df$dst_pts_allowed / 17, l_pts_bracket)
    #   } else {
    #     score_pts_bracket(df$dst_pts_allowed, l_pts_bracket)
    #   }
    # }
    df = group_by(df, id)
    fun_names = names(fun_list)

    for (col in impute_cols) {
      if(col %in% fun_names) {
        df = call_impute_fun(df, col)
      } else {
        df = mutate(df, !!col := derive_from_mean(!!as.symbol(col)))
      }

    }
    df

  }, simplify = FALSE)

  # Scoring sources / totaling sources
  l_raw_points = lapply(names(data_result), function(pos) {
    scoring_table = scoring_l[[pos]]
    cols = intersect(lapply(data_result, names)[[pos]], scoring_table$column) # grabbing scoring columns
    scored_vals = mapply(`*`, data_result[[pos]][cols], scoring_table[match(cols, scoring_table$column), ]$val)
    rowSums(scored_vals, na.rm = TRUE)
  })

  # Adding total to data_result object
  data_result = Map(cbind, data_result, "raw_points" = l_raw_points)
  l_avg_types = list()

  # Calculating totals for each avg_type
  for(type in avg_type) {

    # Setting up avg_type summary function
    if(type == "average") {
      fun_avg = mean.default
      fun_sd = function(x, na.rm = FALSE, w) sd(x, na.rm)
      fun_quan = quantile
    } else if(type == "robust") {
      fun_avg = wilcox.loc
      fun_sd = mad2
      fun_quan = quantile
    } else if(type == "weighted") {
      fun_avg = weighted.mean
      fun_sd = weighted.sd
      fun_quan = whdquantile
    }

    l_avg_types[[type]] = sapply(names(data_result), function(pos) {

      df = data_result[[pos]] %>%
        summarise(pos = !!pos,
                  points = fun_avg(raw_points, na.rm = TRUE, w = weights),
                  sd_pts = fun_sd(raw_points, na.rm = TRUE, w = weights),
                  drop_quantile = list(fun_quan(raw_points, c(.05, .95), na.rm = TRUE, w = weights)),
                  floor = drop_quantile[[1]][1],
                  ceiling = drop_quantile[[1]][2]) %>%
        select(-drop_quantile) %>%
        arrange(points)

      pts_sd = median(df$sd_pts, na.rm = TRUE)
      tier_thresh = tier_thresholds[pos]


      df %>%
        mutate(pos_rank = dense_rank(-points),
               dropoff = c(NA_real_, diff(points))) %>%
        arrange(desc(points)) %>%
        mutate(tier = 1 + trunc((cumsum(dropoff) - dropoff[1]) / (pts_sd * tier_thresh)),
               tier = dense_rank(tier)) %>%
        filter(points > 0)

    }, simplify = FALSE)


  }

  out = bind_rows(lapply(l_avg_types, bind_rows, .id = "pos"), .id = "avg_type")

  # Adding VOR and rank
  out$temp_vor_pos = default_baseline[out$pos]

  out = out %>%
    group_by(avg_type, pos) %>%
    mutate(temp_floor_rank = dense_rank(-floor),
           temp_ceiling_rank = dense_rank(-ceiling),
           temp_vor_ref_points = points[which.max(pos_rank == temp_vor_pos)], # which.max in-case there are NA ranks
           points_vor = points - temp_vor_ref_points,
           temp_vor_ref_floor = floor[which.max(temp_floor_rank == temp_vor_pos)],
           floor_vor = floor - temp_vor_ref_floor,
           temp_vor_ref_ceiling = ceiling[which.max(temp_ceiling_rank == temp_vor_pos)],
           ceiling_vor = ceiling - temp_vor_ref_ceiling) %>%
    ungroup(pos) %>%
    mutate(rank = dense_rank(-points_vor),
           floor_rank = dense_rank(-floor_vor),
           ceiling_rank = dense_rank(-ceiling_vor)) %>%
    select(avg_type, id, pos, points, sd_pts, dropoff, floor, ceiling, points_vor,
           floor_vor, ceiling_vor, rank, floor_rank, ceiling_rank, pos_rank, tier) %>%
    ungroup()

  attr(out, "season") = season
  attr(out, "week") = week
  attr(out, "lg_type") = lg_type
  out
}


