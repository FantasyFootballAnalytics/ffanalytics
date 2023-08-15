


# External ----

#' Manually clear cache associated with ffanalytics package
#'
#' Clears all scraped data cache associated with the ffanalytics package, or a
#' subset of objects from the cache
#' @param ffa_objects (\emph{optional}) a character vector of object names (i.e.,
#' returned by \code{list_ffanalytics_cache()})
#' @export
clear_ffanalytics_cache = function(ffa_objects = NULL) {

  ensure_cache_dir_exists()
  cache_dir = tools::R_user_dir("ffanalytics", "cache")
  current_file_names = list.files(cache_dir, full.names = TRUE)


  if(!is.null(ffa_objects)) {
    object_rds_names = setNames(names(cache_file_names), cache_file_names)[ffa_objects]
    files_to_remove = basename(current_file_names) %in% object_rds_names

    if(any(files_to_remove, na.rm = TRUE)) {
      file.remove(current_file_names[files_to_remove])
      if(sum(files_to_remove, na.rm = TRUE) != length(ffa_objects)) {
        message("Note: Not all of the listed objects were removed\n\nUse `list_ffanalytics_cache()` to check object names")
      }
    } else {
      message("Note: None of the listed objects were removed\n\nUse `list_ffanalytics_cache()` to see object names")
    }

  } else {
    file.remove(current_file_names)
  }
  invisible()
}

#' Checks the scrapes that are currently cached
#'
#' Checks the cached data, removes cached objects >= 8 hours old, and returns a list
#' of objects that are currently cached
#' @param quiet whether the function should return a message if cache is empty
#' @return A \code{data.frame} with the object (scrape) name, and time since caching
#' @export
list_ffanalytics_cache = function(quiet = FALSE) {
  cache_dir = tools::R_user_dir("ffanalytics", "cache")
  clear_cache_by_time()

  current_file_names = list.files(cache_dir)
  if(length(current_file_names) == 0L && !quiet) {
    message("ffanalytics cache is empty")
  }
  file_mtimes = file.mtime(list.files(cache_dir, full.names = TRUE))
  file_order = order(file_mtimes, decreasing = TRUE)
  secs_since_cache = as.numeric(difftime(Sys.time(), file_mtimes, units = "secs"))

  dplyr::tibble(
    object = cache_file_names[current_file_names],
    hr_min_since_cache = format(as.POSIXct(secs_since_cache, origin = '1970-01-01', tz = 'UTC'), '%H:%M')
  )[file_order, ]
}

# Internal ----

cache_object = function(object, file_name) {
  ensure_cache_dir_exists()
  clear_cache_by_time()
  cache_dir = tools::R_user_dir("ffanalytics", "cache")

  current_file_names = list.files(cache_dir)

  if(!file_name %in% current_file_names) {
    saveRDS(
      object,
      file.path(tools::R_user_dir("ffanalytics", "cache"), file_name)
    )
  }
}

get_cached_object = function(file_name) {
  cache_dir = tools::R_user_dir("ffanalytics", "cache")
  readRDS(file.path(cache_dir, file_name))
}

ensure_cache_dir_exists = function() {
  cache_dir = tools::R_user_dir("ffanalytics", "cache")

  if(!file.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

clear_cache_by_time = function() {
  cache_dir = tools::R_user_dir("ffanalytics", "cache")
  file_names = list.files(cache_dir, full.names = TRUE)

  if(length(file_names) == 0) {
    return(NULL)
  }

  file_mtimes = file.mtime(file_names)
  files_to_clear = difftime(Sys.time(), file_mtimes, units = "hours") > 8

  if(any(files_to_clear, na.rm = TRUE)) {
    file.remove(file_names[files_to_clear])
  }
}

cache_file_names = c(
  "yahoo_adp_aav.rds" = "Yahoo ADP/AAV",
  "espn_adp_aav.rds" = "ESPN ADP/AAV",
  "cbs_adp.rds" = "CBS ADP",
  "rts_adp.rds" = "RTS ADP",
  "rts_aav.rds" = "RTS AAV",
  "nfl_adp.rds" = "NFL ADP",
  "mfl_adp.rds" = "MFL ADP",
  "mfl_aav.rds" = "MFL AAV",
  "ffc_adp.rds" = "FFC ADP",
  "ecr_draft_overall_std.rds" = "ECR Draft Overall Std",
  "ecr_draft_overall_half.rds" = "ECR Draft Overall Half",
  "ecr_draft_overall_ppr.rds" = "ECR Draft Overall PPR",
  "ecr_weekly_overall_std.rds" = "ECR Weekly Overall Std",
  "ecr_weekly_overall_half.rds" = "ECR Weekly Overall Half",
  "ecr_weekly_overall_ppr.rds" = "ECR Weekly Overall PPR",
  "ecr_draft_qb_std.rds" = "ECR Draft QB Std",
  "ecr_draft_qb_half.rds" = "ECR Draft QB Half",
  "ecr_draft_qb_ppr.rds" = "ECR Draft QB PPR",
  "ecr_weekly_qb_std.rds" = "ECR Weekly QB Std",
  "ecr_weekly_qb_half.rds" = "ECR Weekly QB Half",
  "ecr_weekly_qb_ppr.rds" = "ECR Weekly QB PPR",
  "ecr_draft_rb_std.rds" = "ECR Draft RB Std",
  "ecr_draft_rb_half.rds" = "ECR Draft RB Half",
  "ecr_draft_rb_ppr.rds" = "ECR Draft RB PPR",
  "ecr_weekly_rb_std.rds" = "ECR Weekly RB Std",
  "ecr_weekly_rb_half.rds" = "ECR Weekly RB Half",
  "ecr_weekly_rb_ppr.rds" = "ECR Weekly RB PPR",
  "ecr_draft_wr_std.rds" = "ECR Draft WR Std",
  "ecr_draft_wr_half.rds" = "ECR Draft WR Half",
  "ecr_draft_wr_ppr.rds" = "ECR Draft WR PPR",
  "ecr_weekly_wr_std.rds" = "ECR Weekly WR Std",
  "ecr_weekly_wr_half.rds" = "ECR Weekly WR Half",
  "ecr_weekly_wr_ppr.rds" = "ECR Weekly WR PPR",
  "ecr_draft_te_std.rds" = "ECR Draft TE Std",
  "ecr_draft_te_half.rds" = "ECR Draft TE Half",
  "ecr_draft_te_ppr.rds" = "ECR Draft TE PPR",
  "ecr_weekly_te_std.rds" = "ECR Weekly TE Std",
  "ecr_weekly_te_half.rds" = "ECR Weekly TE Half",
  "ecr_weekly_te_ppr.rds" = "ECR Weekly TE PPR",
  "ecr_draft_k_std.rds" = "ECR Draft K Std",
  "ecr_draft_k_half.rds" = "ECR Draft K Half",
  "ecr_draft_k_ppr.rds" = "ECR Draft K PPR",
  "ecr_weekly_k_std.rds" = "ECR Weekly K Std",
  "ecr_weekly_k_half.rds" = "ECR Weekly K Half",
  "ecr_weekly_k_ppr.rds" = "ECR Weekly K PPR",
  "ecr_draft_superflex_std.rds" = "ECR Draft SUPERFLEX Std",
  "ecr_draft_superflex_half.rds" = "ECR Draft SUPERFLEX Half",
  "ecr_draft_superflex_ppr.rds" = "ECR Draft SUPERFLEX PPR",
  "ecr_weekly_superflex_std.rds" = "ECR Weekly SUPERFLEX Std",
  "ecr_weekly_superflex_half.rds" = "ECR Weekly SUPERFLEX Half",
  "ecr_weekly_superflex_ppr.rds" = "ECR Weekly SUPERFLEX PPR",
  "ecr_draft_dst_std.rds" = "ECR Draft DST Std",
  "ecr_draft_dst_half.rds" = "ECR Draft DST Half",
  "ecr_draft_dst_ppr.rds" = "ECR Draft DST PPR",
  "ecr_weekly_dst_std.rds" = "ECR Weekly DST Std",
  "ecr_weekly_dst_half.rds" = "ECR Weekly DST Half",
  "ecr_weekly_dst_ppr.rds" = "ECR Weekly DST PPR",
  "ecr_draft_idp_std.rds" = "ECR Draft IDP Std",
  "ecr_draft_idp_half.rds" = "ECR Draft IDP Half",
  "ecr_draft_idp_ppr.rds" = "ECR Draft IDP PPR",
  "ecr_weekly_idp_std.rds" = "ECR Weekly IDP Std",
  "ecr_weekly_idp_half.rds" = "ECR Weekly IDP Half",
  "ecr_weekly_idp_ppr.rds" = "ECR Weekly IDP PPR",
  "ecr_draft_dl_std.rds" = "ECR Draft DL Std",
  "ecr_draft_dl_half.rds" = "ECR Draft DL Half",
  "ecr_draft_dl_ppr.rds" = "ECR Draft DL PPR",
  "ecr_weekly_dl_std.rds" = "ECR Weekly DL Std",
  "ecr_weekly_dl_half.rds" = "ECR Weekly DL Half",
  "ecr_weekly_dl_ppr.rds" = "ECR Weekly DL PPR",
  "ecr_draft_lb_std.rds" = "ECR Draft LB Std",
  "ecr_draft_lb_half.rds" = "ECR Draft LB Half",
  "ecr_draft_lb_ppr.rds" = "ECR Draft LB PPR",
  "ecr_weekly_lb_std.rds" = "ECR Weekly LB Std",
  "ecr_weekly_lb_half.rds" = "ECR Weekly LB Half",
  "ecr_weekly_lb_ppr.rds" = "ECR Weekly LB PPR",
  "ecr_draft_db_std.rds" = "ECR Draft DB Std",
  "ecr_draft_db_half.rds" = "ECR Draft DB Half",
  "ecr_draft_db_ppr.rds" = "ECR Draft DB PPR",
  "ecr_weekly_db_std.rds" = "ECR Weekly DB Std",
  "ecr_weekly_db_half.rds" = "ECR Weekly DB Half",
  "ecr_weekly_db_ppr.rds" = "ECR Weekly DB PPR"
)












