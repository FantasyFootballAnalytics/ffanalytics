# 7-4-21
#
# make scrapes self-contained
#

# library(janitor)
library(rvest)
# library(xml2)
# library(purrr)
library(tidyverse)

# NumberFire
scrape_numberfire <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST", "LB", "DB"), season = 2021, week = 0) {

  base_link <- paste0("https://www.numberfire.com/nfl/fantasy/fantasy-football-projections")
  site_session <- session(base_link)


  l_pos <- lapply(pos, function(pos){

    position <- case_when(pos %in% "QB" ~ "qb",
                          pos %in% "RB" ~ "rb",
                          pos %in% "WR" ~ "wr",
                          pos %in% "TE" ~ "te",
                          pos %in% "K" ~ "k",
                          pos %in% "DST" ~ "d",
                          pos %in% c("LB", "DB") ~ "idp")

    scrape_link <- case_when(week == 0 ~ paste0("https://www.numberfire.com/nfl/fantasy/remaining-projections/", position),
                             week > 0 ~ paste0("https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/", position))


    # numberFire unique player ID's
    numberfire_ids <- site_session %>%
      session_jump_to(scrape_link) %>%
      read_html() %>%
      html_elements(css = "td[class='player'] a") %>%
      html_attr("href") %>%
      basename() %>%
      as_tibble() %>%
      rename(numfire_id = value)

    # scrape contains list of player names and a list of data
    scrape <- site_session %>%
      session_jump_to(scrape_link) %>%
      read_html() %>%
      html_elements(css = 'table.projection-table') %>%
      html_table()


    # player names
    players <- scrape[[1]] %>%
      as_tibble(.name_repair = "minimal") %>%
      rename(Player = 1 ) %>%
      slice(-1L) %>%
      separate(col = 1, into = c("Player", "player_abv", "position_team"), sep = "\\n") %>%
      mutate(position = str_extract(position_team, pattern = "(?<=\\()[:upper:]{1,3}"),
             team = str_extract(position_team, pattern = "[:upper:]{2,3}(?=\\))")) %>%
      select(-player_abv, -position_team)


    # Data
    # rename columns by combining column names with first row of data (numberFire has double row names)
    names(scrape[[2]]) <- paste(names(scrape[[2]]), scrape[[2]][1, ])

    if(pos %in% "QB") {

      # remove first row that contains second half of column names
      # replace the "-" in confidence interval with ","
      # separate the confidence interval and pass c/a into two columns each
      # remove "#"'s from rank columns
      table <- scrape[[2]] %>%
        slice(-1L) %>%
        mutate(`numberFire CI` = str_replace(`numberFire CI`, "(?<=[:digit:])-", replacement = ","))%>%
        separate(col = `numberFire CI`, into = c("Lower", "Upper"), sep = ",") %>%
        separate(col = `Passing C/A`, into = c("pass_comp", "pass_att"), sep = "/") %>%
        mutate(across(starts_with("Ranks"), ~str_remove(., "#")))

    } else if(pos %in% c("LB", "DB")) {

      table <- scrape[[2]] %>%
        slice(-1L)

    } else {

      table <- scrape[[2]] %>%
        slice(-1L) %>%
        mutate(`numberFire CI` = str_replace(`numberFire CI`, "(?<=[:digit:])-", replacement = ",")) %>%
        separate(col = `numberFire CI`, into = c("Lower", "Upper"), sep = ",") %>%
        mutate(across(starts_with("Ranks"), ~str_remove(., "#")))
    }

    # FFA unique player ID's
    FFA_ids <- ffanalytics:::player_ids %>%
      as_tibble() %>%
      select(id, numfire_id)

    # combine numberFire ID's with player names and data
    pos_df <- numberfire_ids %>%
      bind_cols(players) %>%
      bind_cols(table) %>%
      inner_join(FFA_ids, by = "numfire_id") %>%
      mutate(data_src = "NumberFire")

    # New column names
    # subset by the columns in pos_df
    # replicated names between DST and IDP positions
    if (pos %in% c("DB", "LB")) {

      stat_cols <- tibble(
        id = "id",  numfire_id = "src_id", Player = "player", position = "position", team = "team", data_src = "data_src",
        `Defense Tackles` = "idp_solo", `Defense Sacks` = "idp_sack", `Defense INTs` = "idp_int",
        `Defense TDs` = "idp_tds", `Defense Passes Defended` = "idp_pd", `Defense Fum Rec` = "idp_fum_rec") %>%
        select(colnames(pos_df)) %>%
        as.character()

    } else {

      stat_cols <- tibble(
        id = "id", numfire_id = "src_id", Player = "player", position = "position", team = "team", data_src = "data_src",
        `numberFire FP` = "site_pts", Lower = "site_ci_low", Upper = "site_ci_high", `Ranks Ovr.` = "ranks_ovr", `Ranks Pos.` = "ranks_pos",
        pass_comp = "pass_comp", pass_att = "pass_att", `Passing Yds` = "pass_yds", `Passing TDs` = "pass_tds", `Passing Ints` = "pass_ints",
        `Rushing Att` = "rush_att", `Rushing Yds` = "rush_yds", `Rushing TDs` = "rush_tds",
        `Receiving Rec` = "rec", `Receiving Yds` = "rec_yds", `Receiving TDs` = "rec_tds",
        `Receiving Tgt` = "rec_tgt", `Kicking XPM` = "xp", `Kicking FGA` = "fg_att", `Kicking FGM` = "fg",
        `FG Made By Distance 0-19` = "fg_0019", `FG Made By Distance 20-29` = "fg_2029",
        `FG Made By Distance 30-39` = "fg_3039", `FG Made By Distance 40-49` = "fg_4049",
        `FG Made By Distance 50+` = "fg_50", `Defense Points Allowed` = "dst_pts_allowed",
        `Defense Yards Allowed` = "dst_yds_allowed", `Defense Sacks` = "dst_sacks",
        `Defense INTs` = "dst_int", `Defense Fumbles` = "dst_fum_rec", `Defense TDs` = "dst_td") %>%
        select(colnames(pos_df)) %>%
        as.character()
    }

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


numberFire_2021 <- scrape_numberfire(season = 2021, week = 0)

numberFire_week1 <- scrape_numberfire(season = 2021, week = 1)









