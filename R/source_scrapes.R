

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
    out_df
    Sys.sleep(1L) # temporary, until I get an argument for honoring the crawl delay
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

    # Combining df's, removing NA's, filtering our all NA columns
    out = bind_rows(out_dfs)
    out = out[!is.na(out[[1]]), ]

    # Adding IDs
    out$id = ffanalytics:::player_ids$id[match(out$src_id, ffanalytics:::player_ids$nfl_id)]
    out

    Sys.sleep(1L) # temporary, until I get an argument for honoring the crawl delay
  })

  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos
}


# ESPN's undocumented API



