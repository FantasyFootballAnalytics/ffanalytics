# 7-4-21
#
# make scrapes self-contained
#

library(rvest)
# library(purrr)
library(tidyverse)




# FantasySharks
scrape_fantasysharks <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = 2021, week = 0) {

  # these numbers for past seasons are from source_configs.R
  ###### (these do NOT change anything...only 2021 projections appear)
  year <- case_when(season == 2021 ~ 714,
                    season == 2020 ~ 682,
                    season == 2019 ~ 650,
                    season == 2018 ~ 618,
                    season == 2017 ~ 586)

  segment <- case_when(week == 0 ~ year,
                       week > 0 ~ year + week)


  l_pos <- lapply(pos, function(pos){

    position <- case_when(pos %in% "QB" ~ 1,
                          pos %in% "RB" ~ 2,
                          pos %in% "WR" ~ 4,
                          pos %in% "TE" ~ 5,
                          pos %in% "K" ~ 7,
                          pos %in% "DST" ~ 6,
                          pos %in% "DL" ~ 8,
                          pos %in% "LB" ~ 9,
                          pos %in% "DB" ~ 10)

    scrape_link <- paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=-1&Position=",
                          position, "&scoring=1&Segment=", segment,"&uid=4")

    scrape <- scrape_link %>%
      read_html() %>%
      html_nodes(xpath = '//*[@id="toolData"]') %>%
      html_table()


    # Rename duplicated column names
    data <- if (pos %in% "RB") {

      # suppress the "new names" message generated from .name_repair = "unique"
      suppressMessages(
        scrape[[1]] %>%
          as_tibble(.name_repair = "unique") %>%
          filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
          rename(">= 50 yd rush" = ">= 50 yd...13",
                 ">= 100 yd rush" = ">= 100 yd...14",
                 ">= 50 yd rec" = ">= 50 yd...20",
                 ">= 100 yd rec" = ">= 100 yd...21") %>%
          select(-`#`)
      )

    } else if (pos %in% c("WR", "TE")) {

      scrape[[1]] %>%
        as_tibble() %>%
        filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
        rename(">= 50 yd rec" = ">= 50 yd",
               ">= 100 yd rec" = ">= 100 yd",
               ">= 150 yd rec" = ">= 150 yd",
               ">= 200 yd rec" = ">= 200 yd") %>%
        select(-`#`)

    } else if (pos %in% "DST") {

      scrape[[1]] %>%
        as_tibble() %>%
        filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
        rename("dst_int" = "Int",
               "dst_fum_rec" = "Fum") %>%
        select(-`#`)

    } else if (pos %in% c("DL", "LB", "DB")) {

      scrape[[1]] %>%
        as_tibble() %>%
        filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
        rename("idp_sack" = "Scks",
               "idp_int" = "Int",
               "idp_fum_rec" = "Fum",
               "idp_tds" = "DefTD") %>%
        select(-`#`)

    } else {

      scrape[[1]] %>%
        as_tibble() %>%
        filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
        select(-`#`)
    }

    # Player ID's
    p_id <- scrape_link %>%
      read_html() %>%
      html_elements(css = 'td.playerLink a') %>%
      html_attr("href") %>%
      as_tibble() %>%
      mutate(value = str_extract(value, "[:digit:]{3,5}$")) %>%
      mutate(value = str_pad(value, 5, side = "left", 0)) %>%
      rename(id = value)

    # Combine player data with ID's
    pos_df <- data %>%
      bind_cols(p_id) %>%
      select(id, everything())

    # New column names
    # subset by the columns in pos_df
    stat_cols <- tibble(
      id = "id", Player = "player", Tm = "tm", Att = "pass_att", Comp = "pass_comp", `Pass Yds` = "pass_yds",
      `Pass TDs` = "pass_tds", `0-9 Pass TDs` = "pass_09_tds", `10-19 Pass TDs` = "pass_1019_tds",
      `20-29 Pass TDs` = "pass_2029_tds", `30-39 Pass TDs` = "pass_3039_tds", `40-49 Pass TDs` = "pass_4049_tds",
      `50+ Pass TDs` = "pass_50_tds", Int = "pass_int", Sck = "sacks",
      `>= 250 yd` = "pass_250_yds",  `>= 300 yd` = "pass_300_yds", `>= 350 yd` = "pass_350_yds",
      Rush = "rush_att", `Rsh Yds` = "rush_yds", `Rsh TDs` = "rush_tds", Fum = "fumbles_lost", Opp = "opp",
      `0-9 Rsh TDs` = "rush_09_tds", `10-19 Rsh TDs` = "rush_1019_tds", `20-29 Rsh TDs` = "rush_2029_tds",
      `30-39 Rsh TDs` = "rush_3039_tds", `40-49 Rsh TDs` = "rush_4049_tds", `50+ Rsh TDs` = "rush_50_tds",
      `>= 50 yd rush` = "rush_50_yds", `>= 100 yd rush` = "rush_100_yds",
      Tgt = "rec_tgt", `RZ Tgt` = "rec_rz_tgt", Rec = "rec", `Rec Yds` = "rec_yds", `Rec TDs` = "rec_tds",
      `>= 50 yd rec` = "rec_50_yds", `>= 100 yd rec` = "rec_100_yds",
      `>= 150 yd rec` = "rec_150_yds", `>= 200 yd rec` = "rec_200_yds",
      `0-9 Rec TDs` = "rec_09_tds", `10-19 Rec TDs` = "rec_1019_tds", `20-29 Rec TDs` = "rec_2029_tds",
      `30-39 Rec TDs` = "rec_3039_tds", `40-49 Rec TDs` = "rec_4049_tds", `50+ Rec TDs` = "rec_50_tds",
      `Punt Ret Yds` = "punt_ret_yds", `Kick Ret Yds` = "kick_ret_yds", XPM = "xp", XPA = "xp_att",
      FGM = "fg", FGA = "fg_att", `10-19 FGM` = "fg_0019", `20-29 FGM` = "fg_2029", `30-39 FGM` = "fg_3039",
      `40-49 FGM` = "fg_4049", `50+ FGM` = "fg_50", Miss = "fg_miss",
      `Yds Allowed` = "dst_yds_allowed", `100-199` = "dst_yds_199", `200-299` = "dst_yds_299",
      `300-349` = "dst_yds_349", `350-399` = "dst_yds_399", `400-449` = "dst_yds_449",
      `450-499` = "dst_yds_499", `500-549` = "dst_yds_549", `550+` = "dst_yds_550",
      `Pts Agn` = "dst_pts_allowed", `1-6` = "dst_pts_6", `7-13` = "dst_pts_13", `14-17` = "dst_pts_17",
      `18-20` = "dst_pts_20", `21-27` = "dst_pts_27", `28-34` = "dst_pts_34", `35-45` = "dst_pts_45", `46+` = "dst_pts_46",
      Scks = "dst_sacks", dst_int = "dst_int", dst_fum_rec = "dst_fum_rec", DefTD = "dst_td", Safts = "dst_safety",
      Tack = "idp_solo", Asst = "idp_asst", idp_sack = "idp_sack", PassDef = "idp_pd", idp_int = "idp_int",
      FumFrc = "idp_fum_force", idp_fum_rec = "idp_fum_rec",idp_tds = "idp_tds",
      Pts = "site_pts"
    ) %>% select(colnames(pos_df)) %>%
      as.character()

    # Rename columns with new names
    colnames(pos_df) <- stat_cols

    pos_df


  })


  # list elements named by position
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos

}


check <- scrape_fantasysharks(pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"))

df_2021 <- scrape_fantasysharks(season = 2021, pos = c("QB", "RB"))
df_2020 <- scrape_fantasysharks(season = 2020)








# URL's
QB_url <- "https://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=-1&Position=1&scoring=1&Segment=714&uid=4"
RB_url <- "https://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=-1&Position=2&scoring=1&Segment=714&uid=4"
WR_url <- "https://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=-1&Position=4&scoring=1&Segment=714&uid=4"
TE_url <- "https://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=-1&Position=5&scoring=1&Segment=714&uid=4"
K_url <- "https://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=-1&Position=7&scoring=1&Segment=714&uid=4"
DST_url <- "https://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=-1&Position=6&scoring=1&Segment=714&uid=4"

# Quarterback
QB_fantasyshark_scrape <- QB_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="toolData"]') %>%
  html_table()

QB_shark_data <- QB_fantasyshark_scrape[[1]] %>%
  as_tibble() %>%
  mutate(`#` = as.integer(`#`)) %>%
  drop_na()


# Runningback
RB_fantasyshark_scrape <- RB_url %>%
  read_html()%>%
  html_nodes(xpath = '//*[@id="toolData"]') %>%
  html_table()

RB_shark_data <- RB_fantasyshark_scrape[[1]] %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(`#` = as.integer(`#`)) %>%
  drop_na() %>%
  rename(">= 50 yd rush" = ">= 50 yd...13",
         ">= 100 yd rush" = ">= 100 yd...14",
         ">= 50 yd rec" = ">= 50 yd...20",
         ">= 100 yd rec" = ">= 100 yd...21")

# Wide Receiver
WR_fantasyshark_scrape <- WR_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="toolData"]') %>%
  html_table()

WR_shark_data <- WR_fantasyshark_scrape[[1]] %>%
  as_tibble() %>%
  mutate(`#` = as.integer(`#`)) %>%
  drop_na()


# Tight End
TE_fantasyshark_scrape <- TE_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="toolData"]') %>%
  html_table()

TE_shark_data <- TE_fantasyshark_scrape[[1]] %>%
  as_tibble() %>%
  mutate(`#` = as.integer(`#`)) %>%
  drop_na()


# Kicker
K_fantasyshark_scrape <- K_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="toolData"]') %>%
  html_table()

K_shark_data <- K_fantasyshark_scrape[[1]] %>%
  as_tibble() %>%
  mutate(`#` = as.integer(`#`)) %>%
  drop_na()


# Defense
DST_fantasyshark_scrape <- DST_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="toolData"]') %>%
  html_table()

DST_shark_data <- DST_fantasyshark_scrape[[1]] %>%
  as_tibble() %>%
  mutate(`#` = as.integer(`#`)) %>%
  drop_na()


player_id <- ffanalytics:::player_ids

p_id <- RB_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="toolData"]/tbody/tr[4]/td[2]/a')

'//*[@id="toolData"]/tbody/tr[5]/td[2]/a'


p_id <- RB_url %>%
  read_html() %>%
  html_elements(css = 'td.playerLink a') %>%
  html_attr("href") %>%
  as_tibble() %>%
  mutate(value = str_extract(value, "[:digit:]{5}$"))


check <- RB_shark_data %>%
  bind_rows(TE_shark_data)
