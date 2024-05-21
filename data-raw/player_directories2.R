
# Info --------------------------------------------------------------------

# This script will parse player directories for CBS, FFToday, Fantasypros and NFL.
# Make sure you load the ffanalytics package first to have access to tidyverse and rvest.
# The command to create the sysdata.rda file is usethis::use_data(player_ids, overwrite = TRUE, internal = TRUE).
# This command needs to be executed from the package root folder.

#### CBS Players #### ----
devtools::load_all()
library(dplyr)

scrape_cbs = ffanalytics:::scrape_cbs()

final_cbs = dplyr::bind_rows(scrape_cbs) %>%
  dplyr::transmute(merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
                   merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
                   cbs_id = src_id,
                   id)

#### FFToday Players #### ----

scrape_fft = ffanalytics:::scrape_fftoday()

final_fft = dplyr::bind_rows(scrape_fft) %>%
  dplyr::transmute(player = ifelse(pos == "DST", team, player),
                   merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
                   merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
                   fftoday_id = src_id,
                   id,
                   player = NULL)



#### FantasyPros #### ----
# Fantasy pros numeric ids

# Getting Players from last years stats
# Getting links

scrape_fp = scrape_fantasypros()

final_fp_all = dplyr::bind_rows(scrape_fp) %>%
  dplyr::distinct() %>%
  transmute(player = ifelse(pos == "DST", team, player),
            merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            fantasypro_num_id = src_id,
            id,
            player = NULL)

#### NFL Players #### ----

scrape_nfl = ffanalytics:::scrape_nfl()

final_nfl = dplyr::bind_rows(scrape_nfl) %>%
  transmute(player = ifelse(pos == "DST", team, player),
            merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            nfl_id = src_id,
            id,
            player = NULL)




#### NUmber fire #### ----
scrape_nf = scrape_numberfire()

final_nf = scrape_nf %>%
  dplyr::bind_rows() %>%
  transmute(player = ifelse(pos == "DST", team, player),
            merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            numfire_id = src_id,
            id,
            player = NULL)



#### RTSports #### ----

rt_scrape = ffanalytics:::scrape_rtsports()

final_rt = rt_scrape %>%
  dplyr::bind_rows() %>%
  transmute(player = ifelse(pos == "DST", team, player),
            merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            rts_id = src_id,
            id,
            player = NULL)


#### Fleaflicker ----

ff_scrape = scrape_fleaflicker()

final_flfl = ff_scrape %>%
  dplyr::bind_rows() %>%
  transmute(player = ifelse(pos == "DST", team, player),
            merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            fleaflicker_id = src_id,
            id,
            player = NULL)


#### Yahoo ----

yahoo_draft_info = ffanalytics:::yahoo_draft()

final_yahoo = yahoo_draft_info %>%
  transmute(merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player_name)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            stats_id = yahoo_id,
            id)


# Getting ESPN ID's
scrape_espn = ffanalytics:::scrape_espn()

final_espn = scrape_espn %>%
  dplyr::bind_rows() %>%
  transmute(merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            espn_id = src_id,
            id)


# Cleaning up above scrapes

rm(list = grep("^(?!final).+", ls(), value = TRUE, perl = TRUE))
gc()



# updating player_ids table by name & pos

curr_ids = ffanalytics:::player_ids

my_fl_ids = httr::GET("https://api.myfantasyleague.com/2024/export?TYPE=players&L=&APIKEY=&DETAILS=1&SINCE=&PLAYERS=&JSON=1") %>%
  httr::content() %>%
  `[[`("players") %>%
  `[[`("player") %>%
  purrr::map(tibble::as_tibble) %>%
  dplyr::bind_rows() %>%
  mutate(nfl_id = basename(nfl_id)) %>% # prob unnecessary in future
  tidyr::extract(name, c("last_name", "first_name"), "(.+),\\s(.+)") %>%
  mutate(across(everything(), ~gsub("(?![.-])[[:punct:]]", "", ., perl = TRUE))) %>%
  dplyr::mutate(name = paste0(first_name," ",last_name))

updated_ids = my_fl_ids %>%
  select(first_name, last_name, position, id, team, ends_with("_id")) %>%
  filter(if_any(ends_with("_id"), ~ . != 0 & !is.na(.))) %>%
  mutate(position = ifelse(position %in% names(pos_corrections), unlist(pos_corrections)[position], position),
         merge_id = paste0(first_name, last_name),
         merge_id = gsub("[[:punct:]]|\\s+", "", tolower(merge_id)),
         merge_id = paste0(merge_id, "_", tolower(position)))

new_ids = mget(ls(pattern = "^final"))
new_ids = Reduce(function(x, y) {
  full_join(x, y, "merge_id") %>%
    mutate(id = case_when(
      !is.na(id.x) & !is.na(id.y) & id.x != id.y ~ NA_character_,
      TRUE ~ coalesce(id.x, id.y)
      )) %>%
    select(-id.x, -id.y)
}, new_ids) %>%
  filter(!grepl("_(dst|def)$", merge_id)) %>%
  distinct()

# Updating the common columns in the myfantasyleague data
# common_cols = setdiff(intersect(names(new_ids), grep("_id$", names(updated_ids), value = TRUE)), "merge_id")
curr_cols = setdiff(grep("_id$", names(curr_ids), value = TRUE), "id")


for(j in curr_cols) {

  if(j %in% names(updated_ids) && j %in% names(new_ids)) {

    df_updated = updated_ids[c("id", "merge_id", j)]
    updated_name = paste0(j, "_updated")
    names(df_updated)[3] = updated_name

    df_new = new_ids[!is.na(new_ids[[j]]) & !is.na(new_ids$id), c("id", "merge_id", j)]
    new_name = paste0(j, "_new")
    names(df_new)[3] = new_name

    curr_ids = curr_ids %>%
      full_join(df_updated, "id") %>%
      left_join(distinct(select(df_new, -id)), "merge_id") %>%
      left_join(distinct(select(df_new, -merge_id)), "id")

    curr_ids[[j]] = do.call(dplyr::coalesce, curr_ids[grepl(j, names(curr_ids), fixed = TRUE)])
    curr_ids[grep(paste0(j, "_.+|merge_id"), names(curr_ids))] = NULL

  } else if(j %in% names(updated_ids)) {

    df_updated = updated_ids[c("id", j)]
    updated_name = paste0(j, "_updated")
    names(df_updated)[2] = updated_name

    curr_ids = curr_ids %>%
      full_join(df_updated, "id")

    curr_ids[[j]] = coalesce(curr_ids[[j]], curr_ids[[updated_name]])
    curr_ids[[updated_name]] = NULL

  } else if(j %in% names(new_ids)) {

    df_updated = updated_ids[c("id", "merge_id")]

    df_new = new_ids[!is.na(new_ids[[j]]) & !is.na(new_ids$id), c("id", "merge_id", j)]
    new_name = paste0(j, "_new")
    names(df_new)[3] = new_name

    curr_ids = curr_ids %>%
      full_join(df_updated, "id") %>%
      left_join(distinct(select(df_new, -id)), "merge_id") %>%
      left_join(distinct(select(df_new, -merge_id)), "id")


    curr_ids[[j]] = do.call(dplyr::coalesce, curr_ids[grepl(j, names(curr_ids), fixed = TRUE)])
    curr_ids[grep(paste0(j, "_.+|merge_id"), names(curr_ids))] = NULL

  }
}


# Run necessary QA. Looks at the data. Etc..



dim(ffanalytics:::player_ids)
dim(curr_ids)


curr_ids[curr_ids$id %in% curr_ids$id[duplicated(curr_ids$id)], ]



colSums(!is.na(ffanalytics:::player_ids))
colSums(!is.na(curr_ids))

# New - old
colSums(!is.na(curr_ids)) - colSums(!is.na(ffanalytics:::player_ids))

# any duplicates
sum(duplicated(curr_ids$id))
sum(duplicated(ffanalytics:::player_ids))

temp_file = tempfile(fileext = ".rds")
print(temp_file)
saveRDS(curr_ids, temp_file)


# After running necessary QA, replace data
player_ids = readRDS(temp_file)
usethis::use_data(bonus_col_sets, bonus_col_coefs,
                  pts_bracket_coefs, player_ids,
                  overwrite = TRUE, internal = TRUE)





