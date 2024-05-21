
# Info --------------------------------------------------------------------

# This script will parse player directories for CBS, FFToday, Fantasypros and NFL.
# Make sure you load the ffanalytics package first to have access to tidyverse and rvest.
# The command to create the sysdata.rda file is usethis::use_data(player_ids, overwrite = TRUE, internal = TRUE).
# This command needs to be executed from the package root folder.

#### CBS Players #### ----
cbs_links = paste0("https://www.cbssports.com/fantasy/football/depth-chart/",
                   c("QB", "RB", "WR", "TE", "K"))
cbs_session = rvest::session("https://www.cbssports.com/fantasy/football")

cbs_data = lapply(cbs_links, function(page) {
  Sys.sleep(5)
  print(paste0("Starting ", page))
  pos_page = cbs_session %>%
    rvest::session_jump_to(page) %>%
    rvest::read_html()

  cols12 = pos_page %>%
    rvest::html_elements("table > tbody > tr > td > span.CellPlayerName--short > span > a") %>%
    rvest::html_attr("href")

  cols3 = pos_page %>%
    rvest::html_elements("table > tbody > tr > td > div") %>%
    rvest::html_elements("div > span.CellPlayerName--short > span > a") %>%
    rvest::html_attr("href")

  cols = unique(c(cols12, cols3))

  data.frame(player_names = basename(dirname(cols)),
             player_id = basename(dirname(dirname(cols))),
             position = basename(page))
})

final_cbs = dplyr::bind_rows(cbs_data) %>%
  dplyr::transmute(cbs_id = player_id,
                   id = get_mfl_id(player_name = player_names, pos = position),
                   merge_id = paste0(gsub("\\(-)", "", player_names), "_", tolower(position)))


#### FFToday Players #### ----
fft_links = paste0("https://fftoday.com/stats/players?Pos=",
                   c("QB", "RB", "WR", "TE", "K", "DL", "LB", "DB"))
fft_session = rvest::session("https://fftoday.com/stats/players")

fft_data = lapply(fft_links, function(page){
  Sys.sleep(5)
  print(paste0("Starting ", page))
  pos_page = fft_session %>%
    rvest::session_jump_to(page) %>%
    rvest::read_html()

  cols = pos_page %>%
    rvest::html_elements("body > center > table:nth-child(4) > tr:nth-child(2) > td.bodycontent > table:nth-child(7) > tr > td > span.smallbody > a") %>%
    rvest::html_attr("href")

  data.frame(player_names = basename(cols),
             player_id = basename(dirname(cols)),
             pos = sub(".*=", "", page))

})

final_fft = dplyr::bind_rows(fft_data) %>%
  dplyr::transmute(fftoday_id = player_id,
                   merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player_names)),
                   merge_id = paste0(merge_id, "_", tolower(pos)))



#### FantasyPros #### ----
# Fantasy pros numeric ids

# Getting Players from last years stats
# Getting links
fp_pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB")
last_year = 2023

fp_lastyr_links = paste0("https://www.fantasypros.com/nfl/stats/",
                         tolower(fp_pos), ".php?year=", last_year)
fp_lastyr_session = rvest::session(fp_lastyr_links[1])

fp_lastyr_l = lapply(fp_lastyr_links, function(x) {

  print(paste0("Starting ", x, " in 5 seconds"))
  Sys.sleep(5)

  html_page = fp_lastyr_session %>%
    rvest::session_jump_to(x) %>%
    rvest::read_html()

  pos = sub(".*?stats/(.*?)\\..*", "\\1", x)

  name_id = html_page %>%
    rvest::html_elements("td.player-label > a.player-name") %>%
    rvest::html_attr("href") %>%
    sub(".*?stats/(.*?)\\.php$", "\\1", .)

  internal_id = html_page %>%
    rvest::html_elements("a.fp-player-link") %>%
    rvest::html_attr("class") %>%
    sub(".+\\-([0-9]+)$", "\\1", .)

  player_name = html_page %>%
    rvest::html_elements("a.fp-player-link") %>%
    rvest::html_attr("fp-player-name") %>%
    sub(".+\\-([0-9]+)$", "\\1", .)

  data.frame(player_name,
             pos,
             name_id,
             internal_id)


})
rm(fp_lastyr_session)
gc()
fp_lastyr = bind_rows(fp_lastyr_l)

# Pulling from the depth charts
fp_dc_session = rvest::session("https://www.fantasypros.com/nfl/depth-charts.php")

# Getting team links
fp_dc_links <- rvest::read_html(fp_dc_session) %>%
  rvest::html_elements("a[href *='/depth-chart/']:not([class])") %>%
  rvest::html_attr("href")

fp_dc_l = lapply(fp_dc_links, function(x) {

  print(paste0("Starting ", x, " in 5 seconds"))
  Sys.sleep(5)

  team_session = fp_dc_session %>%
    rvest::session_jump_to(x)

  html_page = rvest::read_html(team_session)
  l = vector("list", 2L)

  for(i in 1:2){

    name_id = html_page %>%
      rvest::html_elements("td.player-label > a.player-name") %>%
      rvest::html_attr("href") %>%
      sub(".*?players/(.*?)\\.php$", "\\1", .)

    internal_id = html_page %>%
      rvest::html_elements("table > tbody > tr") %>%
      rvest::html_attr("class") %>%
      sub(".+\\-([0-9]+)$", "\\1", .)

    player_name = html_page %>%
      rvest::html_elements("td.player-label > a.player-name") %>%
      rvest::html_text2()

    pos = html_page %>%
      rvest::html_elements("table > tbody > tr > td:nth-child(1)") %>%
      rvest::html_text2() %>%
      sub("\\d+", "", .) %>%
      tolower()

    l[[i]] = data.frame(player_name,
                        pos,
                        name_id,
                        internal_id)

    # Switch to defense
    if(i == 1) {
      html_page = rvest::session_jump_to(team_session, "?side=dst")
    }

  }
  dplyr::bind_rows(l)

})

rm(fp_dc_session)
gc()
fp_dc = bind_rows(fp_dc_l)

final_fp_all = bind_rows(fp_lastyr, fp_dc) %>%
  distinct() %>%
  transmute(fantasypro_id = name_id,
            fantasypro_num_id = internal_id,
            merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player_name)),
            merge_id = paste0(merge_id, "_", tolower(pos))) %>%
  filter(!duplicated(merge_id)) #

rm(list = ls(pattern = "^fp_"))

#### NFL Players #### ----
# nfl_url <- "https://fantasy.nfl.com/research/players/"
# nfl_session <- session(nfl_url)
# nfl_table <- tibble(nfl_id = character(), player = character(), pos = character(), team = character())
# repeat{
#   print(nfl_session$url)
#   player_ids <- nfl_session %>%
#     read_html() %>%
#     html_elements("table td:first-child a.playerName") %>%
#     html_attr("href") %>%
#     str_extract("[0-9]+$")
#   player_data <- nfl_session %>%
#     read_html() %>%
#     html_elements("table td:first-child") %>%
#     html_text() %>%
#     str_trim() %>%
#     str_remove("\\s[A-Z]$") %>%
#     str_split_fixed(" - ", n= 2) %>%
#     `colnames<-`(c("player", "team")) %>%
#     as_tibble() %>%
#     extract(player, c("player", "pos"), "(.+)\\s([QRWTBKDEF]{1,3}$)")  %>%
#     add_column(nfl_id = player_ids, .before = 1)
#   nfl_table <- nfl_table %>%
#     bind_rows(player_data)
#   next_link <- nfl_session %>%
#     html_node("li.next a") %>%
#     html_attr("href")
#   if(is.na(next_link))
#     break()
#   Sys.sleep(3)
#   next_link <- str_replace(next_link, "statSeason=2020", "statSeason=2021")
#   next_link <- str_replace(next_link, "statWeek=17", "statWeek=1")
#   nfl_session <- nfl_session %>% session_jump_to(paste0(nfl_url, next_link))
# }
#
# final_nfl = nfl_table %>%
#   mutate(player = replace(player, player == "Eli Mitchell", "Elijah Mitchell")) %>%
#   transmute(nfl_id,
#             merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
#             merge_id = paste0(merge_id, "_", tolower(pos)))
#
# rm(list = c("nfl_session", "nfl_table"))


# testing new NFL method because they have not updated player ids on player page
season = 2024
week = ffanalytics:::get_scrape_week()
pos = c("QB", "RB", "WR", "TE", "K")

base_link = paste0("https://fantasy.nfl.com/research/projections?position=1",
                 "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                 "&statType=seasonProjectedStats")

site_session = session(base_link)

nfl_data = lapply(pos, function(pos) {
  Sys.sleep(5)
  print(paste0("Starting ", pos))

  pos_scrape = nfl_pos_idx[pos]

  n_records = dplyr::case_when(
    pos == "QB" ~ 150,
    pos == "RB" ~ 250,
    pos == "WR" ~ 325,
    pos == "TE" ~ 200,
    pos == "K" ~ 500
  )

  if(week == 0) {
    scrape_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape,
                         "&count=", n_records,
                         "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                         "&statType=seasonProjectedStats")
  } else {
    scrape_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape[1],
                         "&count=", n_records,
                         "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                         "&statType=weekProjectedStats&statWeek=", week)
  }

  html_page = site_session %>%
    rvest::session_jump_to(scrape_link) %>%
    rvest::read_html()

  site_id = html_page %>%
    rvest::html_elements("table td:first-child a.playerName") %>%
    rvest::html_attr("href") %>%
    sub(".*=", "",  .)

  player_name = html_page %>%
    rvest::html_elements("table td > div > a") %>%
    rvest::html_text2() %>%
    grep("^View", ., fixed = FALSE, value = TRUE, invert = TRUE)

  data.frame(player_name = player_name,
             player_id = site_id,
             pos = pos)

})

final_nfl = dplyr::bind_rows(nfl_data) %>%
  transmute(nfl_id = player_id,
            merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player_name)),
            merge_id = paste0(merge_id, "_", tolower(pos)))




#### NUmber fire #### ----
html_page = rvest::read_html("https://www.numberfire.com/nfl/players")

# Player names
nf_player_pos = html_page %>%
  rvest::html_elements("div.all-players__wrap > div") %>%
  html_text2()

# Numberfire ids
nf_ids = html_page %>%
  rvest::html_elements("div.all-players__wrap > div > a") %>%
  rvest::html_attr("href") %>%
  basename()

final_nf = data.frame(numfire_id = nf_ids,
                      player = sub("(.+?)\\s+\\(.*", "\\1", nf_player_pos),
                      pos = sub(".+?\\s+\\(([A-Z]+),.+", "\\1", nf_player_pos))


final_nf = final_nf %>%
  transmute(numfire_id,
            merge_id = gsub("[[:punct:]]|\\s+", "", tolower(player)),
            merge_id = paste0(merge_id, "_", tolower(pos)))


rm(list = ls(pattern = "^nf_|html_page"))

#### RTSports #### ----


rt_links = paste0("https://www.freedraftguide.com/football/draft-guide-rankings-provider.php?POS=",
                  rts_pos_idx)

rt_data = lapply(rt_links, function(x) {

  print("Waiting 10 seconds")
  Sys.sleep(10)

  page_l = httr2::request(x) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  page_l = unlist(page_l[names(page_l) == "player_list"], recursive = FALSE)

  page_l = lapply(page_l, `[`, c("name", "player_id", "position"))
  dplyr::bind_rows(page_l)

})

final_rt = dplyr::bind_rows(rt_data) %>%
  dplyr::transmute(pos = setNames(names(rts_pos_idx), rts_pos_idx)[position],
                   player = name,
                   rts_new_id = player_id)

final_rt = final_rt %>%
  dplyr::transmute(rts_id = rts_new_id,
                   merge_id = gsub("[[:punct:]]", "", tolower(player)),
                   merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)))



#### Fleaflicker ----

html_page = rvest::read_html("https://www.fleaflicker.com/nfl/cheat-sheet")

flfl_name_id = html_page %>%
  html_elements("table > tr > td:nth-child(2) > div > div > a") %>%
  html_attr("href") %>%
  basename() %>%
  sub("(.*)\\-([0-9]+)$", "\\1_\\2", .)

flfl_pos = html_page %>%
  html_elements("table > tr > td:nth-child(1)") %>%
  html_text2() %>%
  grep("^[A-Z0-9/]+$", ., value = TRUE) %>%
  gsub("\\d+|/", "", .)

final_flfl = data.frame(flfl_name_id = flfl_name_id,
                        pos = flfl_pos)

final_flfl = final_flfl %>%
  extract(flfl_name_id, c("first_name", "last_name", "fleaflicker_id"), "(.*?)\\-(.*?)_(.*)") %>%
  transmute(merge_id = paste0(first_name, last_name),
            merge_id = gsub("[[:punct:]]", "", tolower(merge_id)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            merge_id,
            fleaflicker_id)

#### Yahoo ----
# TODO: update w/ new API
html_session = rvest::session("https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=AD&pos=ALL&sort=DA_AP")

l_yahoo = list()
i = 0

while(length(l_yahoo) < 14) {

  next_page = paste0(html_session$url, "&count=", length(l_yahoo) * 50)

  html_page = html_session %>%
    rvest::session_jump_to(next_page) %>%
    rvest::read_html()

  i = i + 1

  yahoo_id = html_page %>%
    html_elements("table > tbody > tr > td > div > div > span > a") %>%
    html_attr("data-ys-playerid")

  yahoo_name_pos = html_page %>%
    html_elements("table > tbody > tr > td > div > div > div") %>%
    html_text2() %>%
    grep(":|^(Bye|Final)", ., invert = TRUE, value = TRUE) %>%
    data.table::tstrsplit("\\s+[A-Za-z]+\\s+\\-\\s+")

  temp_df = data.frame(stats_id = yahoo_id)
  temp_df[c("player", "pos")] = yahoo_name_pos

  l_yahoo[[i]] = temp_df

  print(paste0("Read ", i, "/14 pages"))
  print("Sleeping for 2 seconds")
  Sys.sleep(2)

}

final_yahoo = bind_rows(l_yahoo)

final_yahoo = final_yahoo %>%
  transmute(merge_id = gsub("[[:punct:]]", "", tolower(player)),
            merge_id = paste0(gsub("\\s+", "", merge_id), "_", tolower(pos)),
            merge_id, stats_id)


# Getting ESPN ID's




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
new_ids = Reduce(function(x, y) full_join(x, y, "merge_id"), new_ids) %>%
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

    df_new = new_ids[!is.na(new_ids[[j]]), c("merge_id", j)]
    new_name = paste0(j, "_new")
    names(df_new)[2] = new_name

    curr_ids = curr_ids %>%
      full_join(df_updated, "id") %>%
      left_join(df_new, "merge_id")

    curr_ids[[j]] = coalesce(curr_ids[[j]], curr_ids[[updated_name]], curr_ids[[new_name]])
    curr_ids[c(updated_name, new_name, "merge_id")] = NULL

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

    df_new = new_ids[c("merge_id", j)]
    new_name = paste0(j, "_new")
    names(df_new)[2] = new_name

    curr_ids = curr_ids %>%
      full_join(df_updated, "id") %>%
      left_join(df_new, "merge_id")


    curr_ids[[j]] = coalesce(curr_ids[[j]], curr_ids[[new_name]])
    curr_ids[c(new_name, "merge_id")] = NULL

  }
}


# Run necessary QA. Looks at the data. Etc..



dim(ffanalytics:::player_ids)
dim(curr_ids)


View(curr_ids[curr_ids$id %in% curr_ids$id[duplicated(curr_ids$id)], ])



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





