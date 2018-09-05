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
 tbl_cols <- select(out_df, -id, - data_src) %>% mutate_all(check_col) %>% rowSums()

  return(out_df[tbl_cols < ncol(select(out_df, -id, - data_src)),])
}


#' @rdname Impute
kick_impute <- function(kick_tbl){
  kick_cols <- names(kick_tbl)
  kick_dist <- c("fg_0019", "fg_2029", "fg_3039", "fg_4049", "fg_50")

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

    kicking <- kicking %>%
      mutate(fg_miss_0019 = fg_miss * fg_0019 / fg,
             fg_miss_2029 = fg_miss * fg_2029 / fg,
             fg_miss_3039 = fg_miss * fg_3039 / fg,
             fg_miss_4049 = fg_miss * fg_4049 / fg,
             fg_miss_50 = fg_miss * fg_50 / fg
      )
  }

  return(kicking)
}



calc_touch_from_avg <- function(yds, avg, touch){
  sel <- yds != 0 & !is.na(yds) & avg != 0 & !is.na(avg)
  touch[sel] <- yds[sel] / avg[sel]
  touch
}




#' @export
rate_stat <- function(x, y)ifelse(y != 0, x / y, 0)

#' @export
from_rate <- function(var1, var2, rate)ifelse(is.na(var1) & rate > 0, var2 * rate, var1)

#' @export
nan_zero <- function(x)ifelse(is.nan(x) | is.infinite(x), 0, x)

#' @export
val_from_rate <- function(tbl, var_1, var_2){
  v1 <- enquo(var_1)
  v2 <- enquo(var_2)

  var_tbl <- select(tbl, id, !!v1, !!v2, data_src) %>% filter(!is.na(id))

  miss_var <- var_tbl[!complete.cases(var_tbl),]

  if(nrow(miss_var) > 0){
    tvars <- names(var_tbl)[2:3]
    miss_tbl <- var_tbl[complete.cases(var_tbl),] %>%
      transmute(id, rt = rate_stat(!!v1, !!v2)) %>% group_by(id) %>%
      summarise(rate_var = mean(rt, na.rm = TRUE)) %>%
      inner_join(x=miss_var, by = "id") %>%
      mutate(!!tvars[1] := nan_zero(from_rate(!!v1, !!v2, rate_var)),
             !!tvars[2] := nan_zero(from_rate(!!v2, !!v1, 1/rate_var))) %>%
      select(-rate_var)

    var_tbl <- bind_rows(var_tbl[complete.cases(var_tbl),], miss_tbl)
  }

  return(var_tbl)
}

#' @export
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
      val_from_rate(!!v1, !!v2) %>% select(-matches(calc_vars)) %>%
      right_join(calc_tbl, by = c("id", "data_src"))
  } else {
    var_tbl <- stat_tbl %>% inner_join(calc_tbl, by = c("id", "data_src"))
  }
  return(var_tbl)
}

#' @export
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

dist_rate <- function(rate_tbl, stat_tbl, base_var, ...){
  b_var <- enquo(base_var)
  d_var <- quos(...)

  for(i in seq_along(d_var)){
    rate_tbl <- rate_tbl %>% val_from_calc(stat_tbl, !!d_var[[i]], !!b_var)
  }

  return(rate_tbl)
}

get_stat_cols <- function(tbl, match_pattern){
  id_cols <- select(tbl, id, data_src)

  stat_cols <- select(tbl, matches(match_pattern))

  check_cols <- stat_cols %>% is.na() %>% rowSums()

  check_sums <-  stat_cols %>% rowSums(na.rm=TRUE)

  if(length(stat_cols) > 0){
    stat_tbl <- bind_cols(id_cols, stat_cols)
    stat_tbl <- stat_tbl[check_cols < length(stat_cols) & check_sums != 0,]
    return(stat_tbl)
  }
  return(data.frame())
}

sum_columns <- function(tbl, ..., na.rm = FALSE){
  sum_vars <- quos(...)
  select(tbl, !!! sum_vars) %>% rowSums(na.rm = na.rm)
}


