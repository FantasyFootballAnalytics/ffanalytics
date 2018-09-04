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
#' @export
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

  data_cat <- discard(data_cat, no_rows) %>%
    map(inner_join, scrape_pos, by = c("id", "data_src")) %>%
    map(filter, !is.na(id))

 return(data_cat)
}




