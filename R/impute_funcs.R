

# Below are all the impute functions that are used in the new projections table app
call_impute_fun = function(df, col) {
  col_fun = impute_fun_list[[col]]
  args = sapply(formalArgs(col_fun), as.symbol, simplify = FALSE)

  mutate(df, !!col := col_fun(!!!args)) # `!!!` unquotes list of arguments for function
}

derive_from_rate = function(need, ref) {
  idx = is.na(need)
  if(all(idx, na.rm = TRUE)) {
    return(NA)
  }
  if(any(ref[!idx] == 0, na.rm = TRUE)) {
    derive_from_mean(need)
  } else {
    replace(need, idx, (sum(need / ref, na.rm = TRUE) / sum(!idx)) * ref[idx])
  }

}

derive_from_mean = function(need) {
  idx = is.na(need)
  if(all(idx, na.rm = TRUE)) {
    return(NA)
  }
  replace(need, idx, mean.default(need, na.rm = TRUE))
}

impute_fun_list = list(
  pass_att = function(pass_att, pass_yds) {
    derive_from_rate(pass_att, pass_yds)
  },
  pass_comp = function(pass_comp, pass_yds) {
    derive_from_rate(pass_comp, pass_yds)
  },
  pass_tds = function(pass_tds, pass_comp) {
    derive_from_rate(pass_tds, pass_comp)
  },
  pass_int = function(pass_int, pass_att) {
    derive_from_rate(pass_int, pass_att)
  },
  rush_att = function(rush_att, rush_yds) {
    derive_from_rate(rush_att, rush_yds)
  },
  rush_tds = function(rush_tds, rush_yds) {
    derive_from_rate(rush_tds, rush_yds)
  },
  rec_tgt = function(rec_tgt, rec) {
    derive_from_rate(rec_tgt, rec)
  },
  rec_tgt = function(rec, rec_yds) {
    derive_from_rate(rec, rec_yds)
  },
  rec_tds = function(rec_tds, rec_yds) {
    derive_from_rate(rec_tds, rec_yds)
  },
  xp_att = function(xp_att, xp) {
    derive_from_rate(xp_att, xp)
  },
  fg_att = function(fg_att, fg) {
    derive_from_rate(fg_att, fg)
  },
  fg_0019 = function(fg_0019, fg_1019, fg) {
    df_names = names(df)
    if(all(c("fg_1019", "fg_0019") %in% df_names)) {
      fg_0019 = ifelse(is.na(fg_0019) & !is.na(fg_1019), fg_1019, fg_0019)
    }
    derive_from_rate(fg_0019, fg)
  },
  fg_2029 = function(fg_2029, fg) {
    derive_from_rate(fg_2029, fg)
  },
  fg_3039 = function(fg_3039, fg) {
    derive_from_rate(fg_3039, fg)
  },
  fg_4049 = function(fg_4049, fg) {
    derive_from_rate(fg_4049, fg)
  },
  fg_50 = function(fg_50, fg) {
    derive_from_rate(fg_50, fg)
  },
  fg_miss = function(fg_miss, fg, fg_att) {
    idx = is.na(fg_miss)
    fg_miss[idx] = fg_att[idx] - fg[idx]
    derive_from_rate(fg_miss, fg)
  }
)


impute_via_rates_and_mean = function(data_result, scoring_objs) {

  data_result[] = lapply(names(data_result), function(pos) {
    df = data_result[[pos]]
    df_names = names(df)
    scoring_table = scoring_objs$scoring_tables[[pos]]

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
    # if < 0 make sure to flatten at zero
    impute_cols = intersect(df_names, scoring_table$column[scoring_table$val != 0])

    if(pos == "DST") {
      impute_cols = unique(c(impute_cols, "dst_pts_allowed"))
    }

    impute_cols = names(Filter(anyNA, df[impute_cols])) # only grabbing columns with missing values

    df = group_by(df, id)
    fun_names = names(impute_fun_list)

    for (col in impute_cols) {
      if(col %in% fun_names) {
        df = call_impute_fun(df, col)
      } else {
        df = mutate(df, !!col := derive_from_mean(!!as.symbol(col)))
      }
    }

    df
  })

  data_result

}















