

library(rvest)
library(dplyr)
library(tidyr)

# TODO add ID columns, add on.exit
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
               data_src = "CBS")
    } else {
      out_df$team = site_id
      out_df$data_src = "CBS"
      dst_ids = ff_player_data[ff_player_data$position == "Def", c("id", "team")]
      dst_ids$team[dst_ids$team == "OAK"] = "LV"
      out_df$src_id = dst_ids$id[match(site_id, dst_ids$team)]
    }

    # Adding IDs
    out_df$id = ffanalytics:::player_ids$id[match(out_df$src_id, ffanalytics:::player_ids$cbs_id)]


    # Misc cleanup before done
    out_df[out_df == "—"] = NA
    out_df

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
      # Misc column cleanup before done
      temp_df$data_src = "NFL"
      temp_df$src_id = site_id
      temp_df$opp = NULL

      # Type cleanup
      temp_df[temp_df == "-"] = NA
      temp_df = type.convert(temp_df, as.is = TRUE)

      # Adding it to a list of DF's from the pages
      out_dfs[[i]] = temp_df

      # Getting the next link
      page_link = html_page %>%
        html_element("li.next a") %>%
        html_attr("href")

    }

    # Combining df's, removing NA's, filtering our all NA columns
    out = bind_rows(out_dfs)
    out = out[!is.na(out[[1]]), ]

    # Adding IDs
    out$id = ffanalytics:::player_ids$id[match(out$src_id, ffanalytics:::player_ids$nfl_id)]


    out
  })

  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos
}

#Functional NFL Scrape...must run scrape_data function first for other sources. Will pull all OFF pos and DST then merge to my_scrape. Orginal data needs to be my_scrape. 
#Sample test code###
#my_scrape <- scrape_data(src= c("CBS", "FantasyPros", "FantasySharks", "FFToday", "NumberFire", "FantasyFootballNerd", "RTSports", "Walterfootball"),
#                         pos = c("QB","RB","WR","TE","K","DST"))
#scrape_nfl()
####################
scrape_nfl_test = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = 2021, week = 0,
                      draft = TRUE, weekly = TRUE) {
  
  #Matched Stat Columns used on NFL Site
  stat_cols = c(pass_yds = "Passing Yds", rush_yds = "Rushing Yds", pass_tds = "Passing TD",
                pass_int = "Passing Int", rush_tds = "Rushing TD", fumbles_lost =  "Fum Lost",
                two_pts = "Misc 2PT", games = "GP", rec = "Receiving Rec", rec_yds ="Receiving Yds",
                rec_tds = "Receiving TD", ret_tds = "Ret TD",
                xp = "PAT Made", fg_0019 = "FG Made 0-19", fg_2029 = "FG Made 20-29", fg_3039 = "FG Made 30-39",
                fg_4049 = "FG Made 40-49", fg_50 = "FG Made 50+", 
                dst_sacks = "Tackles Sack", dst_int = "Turnover Int",
                dst_fum_Rec = "Turnover Fum Rec", dst_td = "Misc FumTD", dst_pts_allowed = "Points Pts Allow", 
                dst_ret_tds = "Score TD", dst_2pt = "Score Def 2pt Ret",
                dst_safety = "Score Saf", dst_blk = "Block" ,
                site_season_pts = "Fantasy Points", site_pts = "Fantasy Points",
                src_id = "src_id", data_src = "data_src", player = "player", player = "Team", team = "team", pos = "pos", opp = "Opp")
  
  stat_cols <- plyr::ldply(stat_cols, data.frame)
  names(stat_cols) <- c("match","raw")
  
  NFL_DATA = lapply(pos, function(pos) {
    pos_scrape = switch(pos,"QB"= 1,"RB"= 2,"WR"= 3,"TE"= 4,"K"= 7,"DST"= 8)
    base_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape,
                       "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                       "&statType=seasonProjectedStats")
    site_session = html_session(base_link)
    offset=1
    # Setting up hitting each page
    i = 0L
    
    out_dfs = list()
    
    # Going through pages of NFL.com until a player has zero possible fantasy points
    # (sorted by Fantasy Points by default). With the exception of DST where it exits the loop
    # after the second page
    #site_pts is Fantasy Points on the NFL site
    while(i == 0L || min(out_dfs[[i]]$"Fantasy Points") != 0) {
      i = i + 1L
      
      if(i == 3L && pos == "DST") {
        break
      }
      
      if(week == 0) {
        scrape_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape,
                             "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                             "&statType=seasonProjectedStats&offset=",offset)
      } else {
        scrape_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape,
                             "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                             "weekProjectedStats&statWeek=", week)
      }
      
      cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")
      page_link = scrape_link
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
      #col_names = nfl_columns[col_names]
      
      # Creating and cleaning table
      temp_df = html_page %>%
        html_element("table > tbody") %>%
        html_table(header = FALSE) %>%
        `names<-`(col_names)
      
      # Breaking out first column / cleaning (for DST)
      if(pos != "DST") {
        temp_df = temp_df %>%
          extract(Player, c("player", "pos", "team"),
                  "(.*?)\\s+\\b(QB|RB|WR|TE|K)\\b.*?([A-Z]{2,3})")
      } else {
        temp_df$Team = sub("\\s+DEF$", "", temp_df$Team)        
      }
      # Misc column cleanup before done
      temp_df$data_src = "NFL"
      temp_df$src_id = site_id
      temp_df$opp = NULL
      
      # Type cleanup
      temp_df[temp_df == "-"] = NA
      temp_df = type.convert(temp_df, as.is = TRUE)
      
      # Adding it to a list of DF's from the pages
      out_dfs[[i]] = temp_df
      
      # Getting the next link
      offset = offset + 25
      page_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape,
                         "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                         "&statType=seasonProjectedStats&offset=",offset)
      
    }
    
    # Combining df's, removing NA's, filtering our all NA columns
    out = bind_rows(out_dfs)
    out = out[!is.na(out[[1]]), ]
  
  })
  names(NFL_DATA) <- c(pos)
  names(NFL_DATA[["QB"]]) <- stat_cols$match[match(names(NFL_DATA[["QB"]]), stat_cols$raw)]
  names(NFL_DATA[["RB"]]) <- stat_cols$match[match(names(NFL_DATA[["RB"]]), stat_cols$raw)]
  names(NFL_DATA[["WR"]]) <- stat_cols$match[match(names(NFL_DATA[["WR"]]), stat_cols$raw)]
  names(NFL_DATA[["TE"]]) <- stat_cols$match[match(names(NFL_DATA[["TE"]]), stat_cols$raw)]
  names(NFL_DATA[["K"]]) <- stat_cols$match[match(names(NFL_DATA[["K"]]), stat_cols$raw)]
  names(NFL_DATA[["DST"]]) <- stat_cols$match[match(names(NFL_DATA[["DST"]]), stat_cols$raw)]
  
  my_scrape[["QB"]] <- as_tibble(plyr::rbind.fill(my_scrape[["QB"]],NFL_DATA[["QB"]]))
  my_scrape[["RB"]] <- as_tibble(plyr::rbind.fill(my_scrape[["RB"]],NFL_DATA[["RB"]]))
  my_scrape[["WR"]] <- as_tibble(plyr::rbind.fill(my_scrape[["WR"]],NFL_DATA[["WR"]]))
  my_scrape[["TE"]] <- as_tibble(plyr::rbind.fill(my_scrape[["TE"]],NFL_DATA[["TE"]]))
  my_scrape[["K"]] <- as_tibble(plyr::rbind.fill(my_scrape[["K"]],NFL_DATA[["K"]]))
  my_scrape[["DST"]] <- as_tibble(plyr::rbind.fill(my_scrape[["DST"]],NFL_DATA[["DST"]]))
  return(my_scrape)
}


# ESPN's undocumented API
scrape_espn = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = 2021, week = 0,
                           draft = TRUE, weekly = TRUE) {
  #Columns listed on the site. ALOT more available, i just dont have any mapping
  stat_cols = c(id = "id", src_id = "src_id", player = "player", team = "team",  position = "position", data_src = "data_src",
                pass_att = 0, pass_comp= 1, pass_yds = 3, pass_tds = 4, pass_int = 20,
                rush_att = 23, rush_yds = 24, rush_tds = 25,
                rec = 53, rec_yds = 42, rec_tds = 43, rec_tgt = 58)
  stat_cols <- plyr::ldply(stat_cols, data.frame)
  names(stat_cols) <- c("match","raw")
  
  #Used to add columns dynamuclly since if a player doesnt have say any rushing yards they wont return any data for that stat
  stat_cols_raw <- c(id = NA, src_id =  NA, player = "", team = NA,  position = "", data_src = "ESPN",
                     "0" = NA, "1" = NA, "3" = NA, "4" = NA, "20" = NA,
                     "23" = NA, "24" = NA, "25" = NA,
                     "53" = NA, "42" = NA, "43" = NA, "58" = NA)
  
  #used to map position and team id to a text
  slotcodes = list(stat_cols = c("1" = "QB","2" = "RB", "3" = "WR", "4" = "TE", "5" = "K", "7" = "P", "9" = "IDP", "16" = "Def"))
  
  teamcodes = list(team_cols = c("0" = "FA", "1" = "ATL", "2" = "BUF", "3" = "CHI",  "4" = "CIN", "5" = "CLE", "6" = "DAL", "7" = "DEN", "8" = "DET",
                                 "9" ="GBP", "10" = "TEN", "11" = "IND", "12" = "KCC", "13" = "LVR", "14" = "LAR", "15" = "MIA", "16" = "MIN", "17" = "NEP",
                                 "18" = "NOS", "19" = "NYG", "20" = "NYJ",  "21" = "PHI", "22" = "ARI", "23" = "PIT", "24" = "LAC", "25" = "SFF", "26" = "SEA",
                                 "27" = "TBB", "28" = "WAS", "29" = "CAR", "30" = "JAX", "33" = "BAL", "34" = "HOU"))
  #create base dataset to add too  
  data_out <- tibble(id = NA, src_id =  NA, player = "", team = NA,  position = "", data_src = "ESPN", 
                            "0" = NA, "1" = NA, "3" = NA, "4" = NA, "20" = NA,
                            "23" = NA, "24" = NA, "25" = NA,
                            "53" = NA, "42" = NA, "43" = NA, "58" = NA)
  
  ESPN_DATA = lapply(pos, function(pos){
    
    pos_scrape <- switch(pos,"QB"= 0,"RB"= 2,"WR"= 4,"TE"= 6,"K"= 5,"DST"= 16)
    base_url <- "https://fantasy.espn.com/apis/v3/games/ffl/seasons/"
    season_yr <- 2021
    scrape_link <- paste0(base_url,season_yr,"/segments/0/leaguedefaults/1?scoringPeriodId=0&view=kona_player_info")
    xff=paste0('{"players":{"filterStatsForExternalIds":{"value":[2021]},
      "filterSlotIds":{"value":[',pos_scrape,']},
      "filterStatsForSourceIds":{"value":[1]},
      "sortAppliedStatTotal":{"sortAsc":false,"sortPriority":2,"value":"102021"},
      "sortDraftRanks":{"sortPriority":3,"sortAsc":true,"value":"STANDARD"},
      "sortPercOwned":{"sortPriority":4,"sortAsc":false},"limit":100,"offset":0,
      "filterRanksForScoringPeriodIds":{"value":[1]},
      "filterRanksForRankTypes":{"value":["STANDARD"]},
      "filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},
      "filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002021","102021","002020","022021"]}}}')
    
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")
    
    data <- httr::GET(scrape_link,httr::add_headers(`X-Fantasy-Filter` = xff)) %>% 
      httr::content("parsed", "application/json") %>% .[["players"]]
    #get data, clean up, and add characterustic data
    for (p in 1:length(data)){
      if (length(data[[p]][["player"]][["stats"]][[1]][["stats"]]) == 0){next}
      id_name <- data[[p]][["player"]][["fullName"]]
      tmp <- data %>% .[[p]] %>% .[["player"]] %>% .[["stats"]] %>% .[[1]] %>% .[["stats"]] %>% 
        lapply(c) %>% t() %>% as_tibble() %>%
        tibble::add_column(!!!stat_cols_raw[!names(stat_cols_raw) %in% names(.)]) %>% 
        select(c(id, src_id, player, team, position, data_src,"0","1","3","4",
                 "20","23","24","25","53","42","43","58")) %>% 
        NCmisc::Unlist(depth = 1) %>%
        mutate(across(c("0","1","3","4","20","23","24","25","53","42","43","58"), ~as.numeric(.)),
               src_id = as.character(data[[p]][["player"]][["id"]]),
               player = data[[p]][["player"]][["fullName"]],
               team = teamcodes[["team_cols"]][[as.character(data[[p]][["player"]][["proTeamId"]])]],
               position =slotcodes[["stat_cols"]][[as.character(data[[p]][["player"]][["defaultPositionId"]])]],
               id = ffanalytics:::player_ids$id[match(tolower(gsub(" ", "-", id_name)), 
                                                      ffanalytics:::player_ids$fantasypro_id)])
      
        data_out <- dplyr::bind_rows(data_out, tmp) %>% as_tibble()
      rm(tmp)
    }
    # remove first blank row, and filter N/A cols
    data_out <- slice_tail(data_out,n = nrow(data_out)-1) %>% mutate_if(is.numeric, round, digits=0)
    out = data_out
    out = out[!is.na(out[[1]]), ] 
  })
  
  names(ESPN_DATA) <- c(pos)
  names(ESPN_DATA[["QB"]]) <- stat_cols$match[match(names(ESPN_DATA[["QB"]]), stat_cols$raw)] 
  names(ESPN_DATA[["RB"]]) <- stat_cols$match[match(names(ESPN_DATA[["RB"]]), stat_cols$raw)]
  names(ESPN_DATA[["WR"]]) <- stat_cols$match[match(names(ESPN_DATA[["WR"]]), stat_cols$raw)]
  names(ESPN_DATA[["TE"]]) <- stat_cols$match[match(names(ESPN_DATA[["TE"]]), stat_cols$raw)]
  names(ESPN_DATA[["K"]]) <- stat_cols$match[match(names(ESPN_DATA[["K"]]), stat_cols$raw)]
  names(ESPN_DATA[["DST"]]) <- stat_cols$match[match(names(ESPN_DATA[["DST"]]), stat_cols$raw)]
  
  my_scrape[["QB"]] <- as_tibble(plyr::rbind.fill(my_scrape[["QB"]],ESPN_DATA[["QB"]]))
  my_scrape[["RB"]] <- as_tibble(plyr::rbind.fill(my_scrape[["RB"]],ESPN_DATA[["RB"]]))
  my_scrape[["WR"]] <- as_tibble(plyr::rbind.fill(my_scrape[["WR"]],ESPN_DATA[["WR"]]))
  my_scrape[["TE"]] <- as_tibble(plyr::rbind.fill(my_scrape[["TE"]],ESPN_DATA[["TE"]]))
  my_scrape[["K"]] <- as_tibble(plyr::rbind.fill(my_scrape[["K"]],ESPN_DATA[["K"]]))
  my_scrape[["DST"]] <- as_tibble(plyr::rbind.fill(my_scrape[["DST"]],ESPN_DATA[["DST"]]))
  .GlobalEnv$ESPN_DATA <- ESPN_DATA
  .GlobalEnv$my_scrape <- my_scrape

}
