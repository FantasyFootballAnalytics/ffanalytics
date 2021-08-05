

library(rvest)
library(dplyr)
library(tidyr)

# Note: experimental. Not to be used yet.
scrape_cbs = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = 2021, week = 0,
                      draft = TRUE, weekly = TRUE) {

  if(week == 0) {
    scrape_week = "season"
  } else {
    scrape_week = week
  }

  base_link = paste0("https://www.cbssports.com/fantasy/football/stats/QB/2021/season/projections/nonppr/")
  site_session = session(base_link)

  l_pos = lapply(pos, function(pos) {
    scrape_link = paste0("https://www.cbssports.com/fantasy/football/stats/", pos, "/",
                         season, "/", scrape_week, "/projections/nonppr/")

    Sys.sleep(1L) # temporary, until I get an argument for honoring the crawl delay
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")

    html_page = site_session %>%
      session_jump_to(scrape_link) %>%
      read_html()

    # Removing element(s)
    xml_remove(html_elements(html_page, "span.CellPlayerName--short"))

    # Get column names
    col_names = html_page %>%
      html_element("#TableBase > div > div > table > thead > tr.TableBase-headTr") %>%
      html_text2() %>%
      strsplit("\\\n|\\\t")

    col_names = grep("[A-Z]", col_names[[1]], value = TRUE)
    col_names = cbs_columns[col_names]

    # Get PID
    if(pos == "DST") {
      site_id = html_page %>%
        html_elements("span.TeamName a") %>%
        html_attr("href") %>%
        sub(".*?([A-Z]{2,3}).*", "\\1",  .)
    } else {
      site_id = html_page %>%
        html_elements("table.TableBase-table a[href *= 'players']") %>%
        html_attr("href") %>%
        sub(".*?([0-9]+).*", "\\1", .)
    }

    # Creating and cleaning table
    out_df = html_page %>%
      html_element("#TableBase > div > div > table > tbody") %>%
      html_table() %>%
      `names<-`(col_names)

    if(pos != "DST") {
      out_df = out_df %>%
        separate(player, c("player", "pos", "team"), "\\s{2,}") %>%
        mutate(src_id = site_id,
               data_src = "CBS",
               id = player_ids$id[match(src_id, player_ids$cbs_id)])
    } else {
      out_df$team = site_id
      out_df$data_src = "CBS"
      dst_ids = ff_player_data[ff_player_data$position == "Def", c("id", "team")]
      dst_ids$team[dst_ids$team == "OAK"] = "LV"
      out_df$id = dst_ids$id[match(site_id, dst_ids$team)]
      out_df$src_id = player_ids$cbs_id[match(out_df$id, player_ids$id)]
    }

    # Misc cleanup before done
    out_df[out_df == "—"] = NA

    idx = names(out_df) %in% c("id", "src_id")
    out_df[!idx] = type.convert(out_df[!idx], as.is = TRUE)
    out_df[out_df$site_pts > 0,]
  })
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos
}

scrape_nfl = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = 2021, week = 0,
                      draft = TRUE, weekly = TRUE) {

  pos_scrape = nfl_pos_idx[pos]

  base_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape[1],
                     "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                     "&statType=seasonProjectedStats")
  site_session = session(base_link)

  l_pos = lapply(pos, function(pos) {
    pos_scrape = nfl_pos_idx[pos]
    if(week == 0) {
      scrape_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape,
                           "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                           "&statType=seasonProjectedStats")
    } else {
      scrape_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape,
                           "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                           "weekProjectedStats&statWeek=", week)
    }

    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")

    # Setting up hitting each page
    i = 0L
    page_link = scrape_link
    out_dfs = list()

    # Going through pages of NFL.com until a player has zero possible fantasy points
    # (sorted by site_pts by default). With the exception of DST where it exits the loop
    # after the second page
    while(i == 0L || min(out_dfs[[i]]$site_pts) != 0) {
      i = i + 1L

      if(i == 3L && pos == "DST") {
        break
      }

      Sys.sleep(1L) # temporary, until I get an argument for honoring the crawl delay

      html_page = site_session %>%
        session_jump_to(page_link) %>%
        read_html()

      # Get PID
      site_id = html_page %>%
        html_elements("table td:first-child a.playerName") %>%
        html_attr("href") %>%
        sub(".*=", "",  .)

      # Getting column names
      col_names = html_page %>%
        html_element("table > thead") %>%
        html_table(header = FALSE)

      col_names = trimws(paste(col_names[1, ], col_names[2, ]))
      col_names = nfl_columns[col_names]

      # Creating and cleaning table
      temp_df = html_page %>%
        html_element("table > tbody") %>%
        html_table(header = FALSE) %>%
        `names<-`(col_names)

      # Breaking out first column / cleaning (for DST)
      if(pos != "DST") {
        temp_df = temp_df %>%
          extract(player, c("player", "pos", "team"),
                  "(.*?)\\s+\\b(QB|RB|WR|TE|K)\\b.*?([A-Z]{2,3})")
      } else {
        temp_df$team = sub("\\s+DEF$", "", temp_df$team)
      }

      if(pos %in% c("RB", "WR", "TE") && "pass_int" %in% names(temp_df)) {
        temp_df$pass_int = NULL
      }

      # Misc column cleanup before done
      temp_df$data_src = "NFL"
      temp_df$src_id = as.character(site_id)
      temp_df$opp = NULL

      # Type cleanup
      temp_df[temp_df == "-"] = NA
      idx = names(temp_df) %in% c("id", "src_id")
      temp_df[!idx] = type.convert(temp_df[!idx], as.is = TRUE)

      # Adding it to a list of DF's from the pages
      out_dfs[[i]] = temp_df

      # Getting the next link
      page_link = html_page %>%
        html_element("li.next a") %>%
        html_attr("href")

    }

    # Combining df's, removing NA's, filtering out rows with
    out = bind_rows(out_dfs)
    out = out[out$site_pts > 0, ]
    # Adding IDs
    out$id = ffanalytics:::player_ids$id[match(out$src_id, ffanalytics:::player_ids$nfl_id)]
    out

  })

  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos
}


# ESPN's undocumented API



# FantasySharks
scrape_fantasysharks <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = 2021, week = 0,
                                 draft = TRUE, weekly = TRUE) {

  # historical scrapes (doesn't work)
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


# NumberFire
scrape_numberfire <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST", "LB", "DB"), season = 2021, week = 0,
                              draft = TRUE, weekly = TRUE) {

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


# Walter Football
scrape_walterfootball <- function(pos = c("QB", "RB", "WR", "TE", "K"), season = 2021, week = 0,
                                  draft = TRUE, weekly = FALSE) {

  url <- "http://walterfootball.com/fantasy2021rankingsexcel.xlsx"

  xlsx_file <- tempfile("wf", fileext = ".xlsx")
  xl_download <- download.file(url = url, destfile = xlsx_file, mode = "wb", quiet = TRUE)


  l_pos <- lapply(pos, function(pos){

    position <- case_when(pos %in% "QB" ~ "QBs",
                          pos %in% "RB" ~ "RBs",
                          pos %in% "WR" ~ "WRs",
                          pos %in% "TE" ~ "TEs",
                          pos %in% "K" ~ "Ks")

    # Data
    data <- if (pos %in% c("QB", "WR")) {
      # Supress "New names:..." message
      suppressMessages(
        read_xlsx(xlsx_file, sheet = position) %>%
          unite(col = Player, `First Name`, `Last Name`, sep = " ", remove = FALSE) %>%
          select_if(~ any(!is.na(.x))) %>%
          select(matches("^Pass|^Rush|^Catch|^Rec|^Reg TD$|^Int|^FG|^XP|name$|^player|^Team$|^Pos|^Bye")) %>%
          rename(last_name = `Last Name`, first_name = `First Name`, position = Pos)
      )
    } else {
      # Supress "New names:..." message
      # "BYE" spelled differently for RB, TE, and K
      suppressMessages(
        read_xlsx(xlsx_file, sheet = position) %>%
          unite(col = Player, `First Name`, `Last Name`, sep = " ", remove = FALSE) %>%
          select_if(~ any(!is.na(.x))) %>%
          select(matches("^Pass|^Rush|^Catch|^Rec|^Reg TD$|^Int|^FG|^XP|name$|^player|^Team$|^Pos|^Bye")) %>%
          rename(Bye = BYE, last_name = `Last Name`, first_name = `First Name`, position = Pos)
      )
    }

    # FFA unique player ID's
    FFA_ids <- ffanalytics:::player_table %>%
      as_tibble() %>%
      select(id, last_name, first_name, position)

    # Combine data w/ player ID's
    pos_df <- data %>%
      inner_join(FFA_ids, by = c("last_name", "first_name", "position")) %>%
      select(-last_name, -first_name) %>%
      mutate(data_src = "Walterfootball")


    # New column names
    # subset by the columns in pos_df
    stat_cols = tibble(
      id = "id", Player = "player", Team = "tm", Bye = "bye", position = "pos", data_src = "data_src",
      `PASS YDS` = "pass_yds", `PASS TD` = "pass_tds", INT = "pass_int",
      `RUSH YDS` = "rush_yds", CATCH = "rec", `REC YDS` = "rec_yds", `REG TD` = "rec_tds",
      `FG 1-39` = "fg_0039", `FG 40-49` = "fg_4049", `FG 50+` = "fg_50", XP = "xp",
      `Points (ESPN Scoring)` = "site_pts"
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

