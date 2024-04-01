#' Calculate Weighted Standard Deviation
#'
#' Function to calculate weighted standard deviation.
#' @param x The observations to calculate the standard deviations from
#' @param w The weights associated with each observation.
#' @param na.rm If \code{TRUE}, then NA values will be removed.
weighted.sd <- function(x, w, na.rm = FALSE){
  w_zero = !(w <= 0 | is.na(w))
  x_non_na = !is.na(x)
  x = x[w_zero & x_non_na]
  w = w[w_zero & x_non_na]

  if (length(x) <= 1L) {
    return(NA)
  }

  sum.w <- sum(w, na.rm = na.rm)
  sum.w2 <- sum(w^2, na.rm = na.rm)
  mean.w <- sum(x * w, na.rm = na.rm) / sum.w
  sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2))
}

#' Altered MAD function
#'
#' NA for length = 1 & an ignored weight function
mad2 = function(x, center = median(x), constant = 1.4826, na.rm = FALSE,
                low = FALSE, high = FALSE, w) {
  if(length(x) %in% c(0L, 1L)) {
    return(NA)
  } else {
    mad(x, center, constant, na.rm, low, high)
  }
}

#' Weighted Harrell-Davis quantile estimator
#'
#' From: Andrey Akinshin (2023) "Weighted quantile estimators" arXiv:2304.07265 [stat.ME]
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
  if(length(vec) <= 2L){
    return(mean.default(vec, na.rm = na.rm))
  }
  # Calculating the paired averages
  pairAvg <- sort(c(vec, combn(vec, 2L, function(x) sum(x, na.rm = na.rm) / 2)))
  median.default(pairAvg, na.rm = na.rm)
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
                    WalterFootball = 0.281, RTSports= 0.330,
                    FantasyData= 0.428, FleaFlicker = 0.428)



#' Default VOR Baseline
#'
#' This is the default baseline that is used if not otherwise specified when
#' calculating VOR:
#' \code{c(QB = 13, RB = 35, WR = 36, TE = 13, K = 8, DST = 3, DL = 10, LB = 10, DB = 10)}
default_baseline <- c(QB = 13, RB = 35, WR = 36, TE = 13, K = 8, DST = 3, DL = 10, LB = 10, DB = 10)



score_pts_bracket = function(points, pts_bracket) {
  criteria = vapply(pts_bracket, `[[`, numeric(1L), 1L)
  vals = vapply(pts_bracket, `[[`, numeric(1L), 2L)
  thresh_idx = t(vapply(points, `<=`, logical(length(criteria)), criteria))
  vals[max.col(thresh_idx, "first")]
}


score_dst_pts_allowed = function(data_result, pts_bracket) {
  week = attr(data_result, "week")
  year = attr(data_result, "season")
  df = data_result[["DST"]]
  na_idx = is.na(df$dst_pts_allowed)

  if(year >= 2021) {
    n_games = 17L
  } else {
    n_games = 16L
  }

  if(week == 0) {
    set.seed(1L)

    ids_idx = coalesce(
      match(df$id[!na_idx], pts_bracket_coefs$id),
      match(df$id[!na_idx], pts_bracket_coefs$nfl_id)
    )

    ppg = df$dst_pts_allowed[!na_idx] / n_games
    team = pts_bracket_coefs$team[ids_idx]
    idx = match(team, pts_bracket_coefs$team)
    ppg_sd = pts_bracket_coefs$Intercept[idx] + (pts_bracket_coefs$season_mean[1] * ppg)

    game_l = Map(function(x, y) {
      season_games = round(rnorm(17, x, y))
      season_games = replace(season_games, season_games < 0, 0)
      score_pts_bracket(season_games, pts_bracket)
    }, ppg, ppg_sd)
    df$dst_pts_allowed[!na_idx] = vapply(game_l, sum, numeric(1L))
  } else {
    df$dst_pts_allowed[!na_idx] = score_pts_bracket(df$dst_pts_allowed, pts_bracket)
  }
  df$dst_pts_allowed
}

source_points = function(data_result, scoring_rules, return_data_result = FALSE) {

  year = attr(data_result, "season")
  week = attr(data_result, "week")

  scoring_cleaned = make_scoring_tables(scoring_rules)
  scoring_tables = scoring_cleaned$scoring_tables
  pts_bracket = scoring_cleaned$pts_bracket

  # Scoring the points brackets
  data_result$DST$dst_pts_allowed = score_dst_pts_allowed(data_result, pts_bracket)

  l_raw_points = lapply(names(data_result), function(pos) {
    scoring_table = scoring_tables[[pos]]
    cols = intersect(lapply(data_result, names)[[pos]], scoring_table$column) # grabbing scoring columns
    if(length(cols) > 0) {
      scored_vals = mapply(`*`, data_result[[pos]][cols], scoring_table[match(cols, scoring_table$column), ]$val)
      rowSums(scored_vals, na.rm = TRUE)
    } else {
      NA
    }
  })
  names(l_raw_points) = names(data_result)
  data_result = Map(cbind, data_result, "raw_points" = l_raw_points)

  if(return_data_result) {
    attr(data_result, "season") = year
    attr(data_result, "week") = week
    data_result
  } else {
    data_result = lapply(data_result, `[`, c("pos", "data_src", "id", "raw_points"))
    dplyr::bind_rows(data_result) %>%
      dplyr::arrange(pos, id, data_src) %>%
      dplyr::as_tibble()
  }
}



#' Default Threshold Values for Tiers
#'
#' These are the default threshold values used when applying Cohen's D values
#' to determine tiers:
#' \code{c(QB = 1, RB = 1, WR = 1, TE = 1, K = 1, DST = 0.1, DL = 1, DB = 1, LB = 1)}
default_threshold <-  c(QB = 1, RB = 1, WR = 1, TE = 1, K = 1, DST = 0.1, DL = 1, DB = 1, LB = 1)


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
projections_table = function(data_result, scoring_rules = NULL, src_weights = NULL,
                              vor_baseline = NULL, tier_thresholds = NULL,
                              avg_type = c("average", "robust", "weighted"),
                              return_raw_stats = FALSE) {

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

  # Computing league type
  if(scoring_rules$rec$all_pos){
    if(is.null(scoring_rules$rec$rec)) {
      obj_name = deparse(substitute(scoring_rules))
      scoring_rules$rec$rec = 0L
      message(paste0("Note: ", obj_name, "$rec$rec not specified. Default value is 0 (i.e., 0 PPR)"))
    }
    lg_type = rep(scoring_rules$rec$rec, length(data_result))
    lg_type = dplyr::case_when(lg_type > .5 ~ "PPR",
                               lg_type > 0 ~ "Half",
                               TRUE ~ "Std")
    names(lg_type) = names(data_result)
  } else {
    lg_type = lapply(scoring_rules$rec[names(scoring_rules$rec) != "all_pos"], `[[`, "rec")
    lg_type = Filter(Negate(is.null), lg_type)
    lg_type = vapply(lg_type, function(x) if(x > .5) "PPR" else if(x > 0) "Half" else "Std", character(1L))
    lg_type[setdiff(names(data_result), names(lg_type))] <- "Std"
  }


  # Setting up the scoring table ----
  scoring_objs = make_scoring_tables(scoring_rules)
  scoring_l = scoring_objs$scoring_tables
  l_pts_bracket = scoring_objs$pts_bracket

  # Adding weight and removing empty id's
  data_result[] = lapply(data_result, function(df) {
    df = df[!is.na(df$id), ]
    df$weights = src_weights[df$data_src]
    df
  })

  # Imputing values ----
  data_result[] = impute_via_rates_and_mean(data_result, scoring_objs)
  data_result = impute_bonus_cols(data_result, scoring_objs$scoring_tables)

  # To return the aggregataed stats instead of the fantasy points
  if(return_raw_stats) {

    df_l = sapply(names(data_result), function(pos) {
      df = group_by(data_result[[pos]], id)
      scoring_table = scoring_l[[pos]]
      cols = intersect(names(df), scoring_table$column[scoring_table$val != 0])
      l_avg_types = vector("list", length(avg_type))
      names(l_avg_types) = avg_type

      # Removing one-only source id's
      df = df %>%
        filter(n() > 1)

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

        l_avg_types[[type]] = lapply(cols, function(col) {

          col_sym = as.symbol(col)

          out = df %>%
            summarise(points = fun_avg(!!col_sym, na.rm = TRUE, w = weights),
                      sd = fun_sd(!!col_sym, na.rm = TRUE, w = weights))

          names(out)[-1] = sub("_points", "", paste0(col, "_", names(out)[-1]))
          out
        })

        l_avg_types[[type]] = Reduce(function(x, y) left_join(x, y, "id") , l_avg_types[[type]])

      }

      df["id"] %>%
        filter(!duplicated(id)) %>%
        left_join(bind_rows(l_avg_types, .id = "avg_type"), "id")

    })

    return(bind_rows(df_l, .id = "position"))

  }

  # Scoring sources / totaling sources
  data_result[] = source_points(data_result, scoring_rules, return_data_result = TRUE)

  l_avg_types = vector("list", length(avg_type))
  names(l_avg_types) = avg_type

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
        filter(points > 0 & is.finite(points)) %>%
        arrange(points)

      pts_sd = median.default(df$sd_pts, na.rm = TRUE)
      tier_thresh = tier_thresholds[pos]


      df %>%
        mutate(pos_rank = dense_rank(-points),
               dropoff = c(0, diff(points))) %>%
        arrange(desc(points)) %>%
        mutate(tier = 1 + trunc((cumsum(dropoff) - dropoff[1]) / (pts_sd * tier_thresh)),
               tier = dense_rank(tier))

    }, simplify = FALSE)


  }

  out = bind_rows(lapply(l_avg_types, bind_rows, .id = "pos"), .id = "avg_type")

  # Adding VOR and rank
  out$temp_vor_pos = vor_baseline[out$pos]

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

#' Add ECR to the Projection Table
#'
#' This function will add the ECR values to the projections table generated from
#' the \link{projections_table} function. It will add the positional ECR, the
#' standard deviation for the positional ECR, and if seasonal data also the
#' overall ECR value
#' @param projection_table An output from the \link{projections_table} function.
#' @export
add_ecr <- function(projection_table){
  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")
  message("Scraping ECR data (w/ 2 second delay between pages if not cached)")

  if(week == 0) {
    rank_per = "draft"
  } else {
    rank_per = "week"
  }

  if(week == 0) {
    if(any(lg_type == "PPR")) {
      lg_ov = "PPR"
    } else if(any(lg_type == "Half")) {
      lg_ov = "Half"
    } else {
      lg_ov = "Std"
    }

    ecr_overall = scrape_ecr(rank_period = "draft", rank_type = lg_ov, position = "Overall") %>%
      dplyr::select(id, overall_ecr = avg)
    projection_table = dplyr::left_join(projection_table, ecr_overall, by = "id")
  }

  scraped_ecr = vector("list", length(lg_type))
  for(i in seq_along(lg_type)) {
    cached_objects = names(list_ffanalytics_cache(TRUE)$object)
    req_obj = paste0(
      "ecr_", rank_per, "_",
      tolower(names(lg_type)[i]), "_",
      tolower(lg_type[i]), ".rds"
      )
    if(!req_obj %in% cached_objects) {
      Sys.sleep(2)
    }
    scraped_ecr[[i]] = scrape_ecr(rank_period = rank_per,
                                  position = names(lg_type)[i],
                                  rank_type = lg_type[i])

  }
  pos_ecr = dplyr::bind_rows(scraped_ecr) %>%
    dplyr::select(id, pos_ecr = avg, sd_ecr = std_dev)

  projection_table = dplyr::left_join(projection_table, pos_ecr, by = "id")

  attr(projection_table, "lg_type") = lg_type
  attr(projection_table, "season") = season
  attr(projection_table, "week") = week

  projection_table
}

#' Add ADP to the Projections Table
#'
#' This function will add the ADP data to the projections table from the
#' \link{projections_table} function. It will add the average ADP from the sources
#' specfied, and the difference between the overall rank and ADP
#' @param projection_table An output from the \link{projections_table} function
#' @param sources Which ADP sources should be added. should be one or more of
#' \code{c("RTS", "CBS", "MFL", "Yahoo", "NFL", "FFC")}
#' @export
add_adp <- function(projection_table,
                    sources = c("RTS", "CBS", "Yahoo", "NFL", "FFC", "MFL")){

  sources <- match.arg(sources, c("RTS", "CBS", "Yahoo", "NFL", "FFC", "MFL"), several.ok = TRUE)

  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")

  if(week != 0) {
    warning("ADP data is not available for weekly data", call. = FALSE)
    return(projection_table)
  }
  message("Scraping ADP data")

  adp_tbl <- get_adp(sources, metric = "adp")

  if(ncol(adp_tbl) == 2) {
    names(adp_tbl)[2] = "adp"
  } else {
    adp_tbl = adp_tbl %>%
      dplyr::select(id, adp = adp_avg, adp_sd)
  }

  projection_table <- left_join(projection_table, adp_tbl, by = "id") %>%
    dplyr::mutate(adp_diff = rank - adp)

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
                    sources = c("RTS", "ESPN", "Yahoo", "NFL", "MFL")) {

  sources = match.arg(sources, c("RTS", "ESPN", "Yahoo", "NFL", "MFL"), several.ok = TRUE)

  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")

  if (week != 0){
    warning("AAV data is not available for weekly data", call. = FALSE)
    return(projection_table)
  }
  message("Scraping AAV Data")
  adp_tbl <- get_adp(sources, metric = "aav")

  if(ncol(adp_tbl) == 2) {
    names(adp_tbl)[2] = "aav"
  } else {
    adp_tbl = adp_tbl %>%
      dplyr::select(id, aav = aav_avg, aav_sd)
  }

  projection_table <- dplyr::left_join(projection_table, adp_tbl, by = "id")

  projection_table  %>%
    `attr<-`(which = "season", season) %>%
    `attr<-`(which = "week", week) %>%
    `attr<-`(which = "lg_type", lg_type)
}


#' Uncertainty calculation
#'
#' Calculation of uncertainty returns a value from 1 to 99 where higher values
#' indicate more uncertainty (i.e., more variability).
calculate_uncertainty <- function(..., percentage = TRUE) {

  vars_list = list(...)
  vars_m = do.call(cbind, vars_list)

  mean_risk <- scale(rowMeans(scale(vars_m), na.rm = TRUE))[, 1]

  if(percentage) {
    out = round(percent_rank(mean_risk), 2)
    out[out <= .01] = .01
    out[out >= .99] = .99
    out
  } else {
    mean_risk[is.na(mean_risk)] <- NA
    mean_risk
  }

}

#' Add uncertainty to the table
#'
#' Calculation of uncertainty is done by scaling the standard deviation
#' variables, averaging them, and then creating a within-position percentile
#' rank ranging from 1 to 99. A score of 1 indicates there is very little
#' uncertainty (low standard deviation) and a score of 99 indicates there is
#' a large degree of uncertainty
#'
#' A low score means there is general agreement among experts and projections.
#' A high score indicates there is a lot of variability in rankings and/or
#' projections. By default `add_uncertainty()` uses `sd_pts` and `sd_ecr` to
#' compute uncertantity.
#' @export
add_uncertainty <- function(projection_table){

  attr_season = attr(projection_table, "season")
  attr_week = attr(projection_table, "week")
  attr_lg_type = attr(projection_table, "lg_type")

  projection_table %>%
    dplyr::group_by(pos) %>%
    dplyr::mutate(uncertainty = calculate_uncertainty(sd_pts, sd_ecr)) %>%
    dplyr::ungroup() %>%
    `attr<-`("season", attr_season) %>%
    `attr<-`("week", attr_week) %>%
    `attr<-`("lg_type", attr_lg_type)
}


#' Add player information to the table
#'
#' Adds player information to the projections table
#' @export
add_player_info <- function(projection_table){
  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")

  players = dplyr::select(player_table, id, first_name, last_name, team, position, age, exp)

  dplyr::left_join(projection_table, players, by = "id") %>%
    `attr<-`(which = "season", season) %>%
    `attr<-`(which = "week", week) %>%
    `attr<-`(which = "lg_type", lg_type)
}

#' New, lighter, projections_table function
#'
#' Keeping until we transition in the app
projections_table2 = projections_table





