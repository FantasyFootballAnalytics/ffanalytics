

# CBS ----
scrape_cbs = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = NULL, week = NULL,
                      draft = TRUE, weekly = TRUE) {

  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }

  if(week %in% c(0, "ros")) {
    scrape_week = "restofseason"
  } else {
    scrape_week = week
  }

  message("\nThe CBS scrape uses a 2 second delay between pages")

  base_link = paste0("https://www.cbssports.com/fantasy/football/")
  site_session = rvest::session(base_link)

  l_pos = lapply(pos, function(pos) {
    scrape_link = paste0("https://www.cbssports.com/fantasy/football/stats/", pos, "/",
                         season, "/", scrape_week, "/projections/nonppr/")

    Sys.sleep(2L) # temporary, until I get an argument for honoring the crawl delay
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")

    html_page = site_session %>%
      session_jump_to(scrape_link) %>%
      read_html()

    # Get column names
    col_names = html_page %>%
      html_element("#TableBase > div > div > table > thead > tr.TableBase-headTr") %>%
      html_text2() %>%
      strsplit("\\\n|\\\t")

    col_names = grep("[A-Z]", col_names[[1]], value = TRUE)
    col_names = rename_vec(col_names, cbs_columns)

    # Get PID
    if(pos == "DST") {
      cbs_id = html_page %>%
        rvest::html_elements("span.TeamName a") %>%
        rvest::html_attr("href") %>%
        sub(".*?([A-Z]{2,3}).*", "\\1",  .)
    } else {
      cbs_id = html_page %>%
        rvest::html_elements("table > tbody > tr > td:nth-child(1) > span.CellPlayerName--long > span > a") %>%
        rvest::html_attr("href") %>%
        sub(".*?([0-9]+).*", "\\1", .)
    }

    # Creating and cleaning table
    out_df = html_page %>%
      rvest::html_element("#TableBase > div > div > table > tbody") %>%
      rvest::html_table() %>%
      `names<-`(col_names)

    if(pos != "DST") {
      out_df = out_df %>%
        tidyr::extract(player, c("player", "pos", "team"),
                       ".*?\\s{2,}[A-Z]{1,3}\\s{2,}[A-Z]{2,3}\\s{2,}(.*?)\\s{2,}(.*?)\\s{2,}(.*)") %>%
        dplyr::mutate(src_id = cbs_id,
                      data_src = "CBS",
                      id = player_ids$id[match(src_id, player_ids$cbs_id)])
      out_df$id = get_mfl_id(
        id_col = cbs_id,
        player_name = out_df$player,
        pos = out_df$pos,
        team = out_df$team
      )
    } else {
      out_df$team = cbs_id
      out_df$data_src = "CBS"
      dst_ids = ff_player_data[ff_player_data$position == "Def", c("id", "team")]
      dst_ids$team[dst_ids$team == "OAK"] = "LV"
      out_df$id = dst_ids$id[match(cbs_id, dst_ids$team)]
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

# NFL ----
scrape_nfl = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = NULL, week = NULL,
                      draft = TRUE, weekly = TRUE) {
  message("\nThe NFL.com scrape uses a 2 second delay between pages")

  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }

  pos_scrape = nfl_pos_idx[pos]

  base_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape[1],
                     "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                     "&statType=seasonProjectedStats")

  site_session = session(base_link)

  l_pos = lapply(pos, function(pos) {
    pos_scrape = nfl_pos_idx[pos]

    n_records = case_when(
      pos == "QB" ~ 42,
      pos == "RB" ~ 100,
      pos == "WR" ~ 150,
      pos == "TE" ~ 60,
      pos == "K" ~ 64,
      pos == "DST" ~ 32
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

    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")

    html_page = site_session %>%
      session_jump_to(scrape_link) %>%
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
    out_df = html_page %>%
      html_element("table > tbody") %>%
      html_table(header = FALSE) %>%
      `names<-`(col_names)

    # Breaking out first column / cleaning (for DST)
    if(pos != "DST") {
      out_df = out_df %>%
        extract(player, c("player", "pos", "team"),
                "(.*?)\\s+\\b(QB|RB|WR|TE|K)\\b.*?([A-Z]{2,3})")
    } else {
      out_df$team = sub("\\s+DEF$", "", out_df$team)
      out_df$pos = "DST"
    }

    if(pos %in% c("RB", "WR", "TE") && "pass_int" %in% names(out_df)) {
      out_df$pass_int = NULL
    }

    # Misc column cleanup before done
    out_df$data_src = "NFL"
    out_df$nfl_id = as.character(site_id)
    out_df$opp = NULL

    # Type cleanup
    out_df[out_df == "-"] = NA
    idx = names(out_df) %in% c("id", "nfl_id")
    out_df[!idx] = type.convert(out_df[!idx], as.is = TRUE)

    # Combining df's, removing NA's, filtering out rows with
    out_df = out_df[out_df$site_pts > 0 & !is.na(out_df$site_pts), ]
    # Adding IDs
    out_df$id = get_mfl_id(
      out_df$nfl_id,
      player_name = if(pos == "DST") NULL else out_df$player,
      pos = out_df$pos,
      team = out_df$team
    )
    out_df = out_df %>%
      dplyr::select(id, src_id = nfl_id, any_of("player"), pos, team, dplyr::everything())


    Sys.sleep(2L) # temporary, until I get an argument for honoring the crawl delay

    # Removing all NA columns
    Filter(function(x) any(!is.na(x)), out_df)

  })

  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos
}

# Fantasysharks ----
scrape_fantasysharks <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
                                 season = NULL, week = NULL, draft = TRUE, weekly = TRUE) {
  message("\nThe FantasySharks scrape uses a 2 second delay between pages")

  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }
  # historical scrapes (doesn't work)
  year = dplyr::case_when(
    season == 2023 ~ 778,
    season == 2022 ~ 746,
    season == 2021 ~ 714,
    season == 2020 ~ 682,
    season == 2019 ~ 650,
    season == 2018 ~ 618,
    season == 2017 ~ 586
  )

  # segment for url from user week input
  if (week == 0) {
    segment <- year
  } else if (week %in% c(1:22)) {
    segment <- year + week + 8
  } else if (week %in% "ros") {
    segment <- 717
  }

  l_pos <- lapply(pos, function(pos){

    position = dplyr::case_when(
      pos %in% "QB" ~ 1,
      pos %in% "RB" ~ 2,
      pos %in% "WR" ~ 4,
      pos %in% "TE" ~ 5,
      pos %in% "K" ~ 7,
      pos %in% "DST" ~ 6,
      pos %in% "DL" ~ 8,
      pos %in% "LB" ~ 9,
      pos %in% "DB" ~ 10
    )

    scrape_link <- paste0("https://www.fantasysharks.com/apps/bert/forecasts/projections.php?csv=1&Sort=",
                          "&League=-1&Position=",position, "&scoring=1&Segment=", segment, "&uid=4")

    Sys.sleep(2L) # temporary, until I get an argument for honoring the crawl delay
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")

    pos_df = data.table::fread(scrape_link, data.table = FALSE, showProgress = FALSE)
    pos_df$Rank = NULL

    # Rename columns with new names
    names(pos_df) = rename_vec(names(pos_df), fantasysharks_columns)
    names(pos_df)[duplicated(names(pos_df))] = c("rec_50_yds", "rec_100_yds")

    if(pos == "K") {
      names(pos_df) = replace(names(pos_df), names(pos_df) == "pass_att", "fg_att")
    }
    if(pos == "DST") {
      names(pos_df) = replace(names(pos_df), names(pos_df) == "pass_int", "dst_int")
      pos_df$id = sprintf("%04d", as.numeric(pos_df$id))
    }
    if(pos %in% c("DL", "LB", "DB")) {
      names(pos_df) = gsub("^(dst|pass)_", "idp_", names(pos_df))
    }

    pos_df$id = as.character(pos_df$id)
    pos_df$data_src = "FantasySharks"
    pos_df[-1] = type.convert(pos_df[-1], as.is = TRUE)
    pos_df = pos_df[pos_df$site_pts > 0, ]
    dplyr::as_tibble(pos_df)

  })


  # list elements named by position
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos

}

# Numberfire ----
scrape_numberfire <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST", "LB", "DB", "DL"),
                              season = NULL, week = NULL, draft = TRUE, weekly = TRUE) {

  message("\nThe numberFire scrape uses a 2 second delay between pages")

  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }

  base_link <- paste0("https://www.numberfire.com/nfl/fantasy/fantasy-football-projections")
  site_session <- rvest::session(base_link)

  # That IDP scrapes grabs everyone, this only hits website once grabs the positions later
  if(any(pos %in% c("LB", "DB", "DL"))) {
    site_pos <- c(setdiff(pos, c("LB", "DB", "DL")), "LB")
  } else {
    site_pos <- pos
  }


  l_pos <- lapply(site_pos, function(pos){

    position <- dplyr::case_when(
      pos %in% "QB" ~ "qb",
      pos %in% "RB" ~ "rb",
      pos %in% "WR" ~ "wr",
      pos %in% "TE" ~ "te",
      pos %in% "K" ~ "k",
      pos %in% "DST" ~ "d",
      pos %in% "LB" ~ "idp"
      )

    scrape_link <- dplyr::case_when(
      week %in% c(0, "ros") ~ paste0("https://www.numberfire.com/nfl/fantasy/remaining-projections/", position),
      week > 0 ~ paste0("https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/", position)
      )

    Sys.sleep(2L) # temporary, until I get an argument for honoring the crawl delay
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")


    # grabbing position page
    html_page <- site_session %>%
      rvest::session_jump_to(scrape_link) %>%
      rvest::read_html()


    # numberFire unique player ID's
    numfire_id <- html_page %>%
      rvest::html_elements(css = "td[class='player'] a") %>%
      rvest::html_attr("href") %>%
      basename()

    # scrape contains list of player names and a list of data
    scrape <- html_page %>%
      rvest::html_elements(css = 'table.projection-table') %>%
      rvest::html_table()


    # player names
    players = scrape[[1]] %>%
      dplyr::rename(Player = 1) %>%
      dplyr::slice(-1L) %>%
      tidyr::extract(Player, into = c("Player", "position", "team"),
                     "(.*?)\\n.*\\n.*?([A-Z]{1,3}),\\s*([A-Z]{2,3})")


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
        mutate(`numberFire CI` = sub("(\\d|\\.)\\-", "\\1,", `numberFire CI`)) %>%
        separate(col = `numberFire CI`, into = c("Lower", "Upper"), sep = ",") %>%
        separate(col = `Passing C/A`, into = c("pass_comp", "pass_att"), sep = "/") %>%
        mutate(across(starts_with("Ranks"), ~ gsub("#", "", .x, fixed = TRUE)))

    } else if(pos %in% c("LB", "DB")) {

      table <- scrape[[2]] %>%
        slice(-1L)

    } else {

      table <- scrape[[2]] %>%
        slice(-1L) %>%
        mutate(`numberFire CI` = gsub("(\\d|\\.)\\-", "\\1,", `numberFire CI`, perl = TRUE)) %>%
        separate(col = `numberFire CI`, into = c("Lower", "Upper"), sep = ",") %>%
        mutate(across(starts_with("Ranks"), ~gsub("#", "", .x, fixed = TRUE)))
    }

    # combine numberFire ID's with player names and data
    pos_df <- players %>%
      dplyr::bind_cols(table) %>%
      dplyr::mutate(id = get_mfl_id(numfire_id, player_name = Player, pos = position, team = team),
                    src_id = numfire_id,
                    data_src = "NumberFire") %>%
      dplyr::select(id, src_id, everything())

    # New column names
    # subset by the columns in pos_df
    # replicated names between DST and IDP positions
    if (pos %in% c("DB", "LB", "DL")) {
      names(pos_df) <- rename_vec(names(pos_df), numberfire_idp_columns)
    } else {
      names(pos_df) <- rename_vec(names(pos_df), numberfire_columns)
    }


    # Changing types before merging
    pos_df[] = lapply(pos_df, function(x) gsub("N/A|\\$", "", x))

    id_idx <- !names(pos_df) %in% "id"
    pos_df[id_idx] <- type.convert(pos_df[id_idx], as.is = TRUE)

    if("site_pts" %in% names(pos_df)) {
      pos_df[pos_df$site_pts > 0, ]
    } else {
      pos_df
    }

  })

  # Fixing idp scrapes (all come in with 'idp')
  if(any(pos %in% c("LB", "DB", "DL"))) {
    idp_idx <- match("LB", site_pos)
    df_idp <- l_pos[[idp_idx]]
    l_pos[[idp_idx]] <- NULL
    l_idp <- split(df_idp, df_idp$pos)
    l_idp <- l_idp[intersect(pos, names(l_idp))]

    names(l_pos) <- setdiff(pos, c("LB", "DB", "DL"))
    l_pos <- c(l_pos, l_idp)

  } else {
    # list elements named by position
    names(l_pos) = pos
  }

  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos

}

# Walterfootball ----
scrape_walterfootball <- function(pos = c("QB", "RB", "WR", "TE", "K"),
                                  season = NULL, week = NULL, draft = TRUE, weekly = FALSE) {

  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }

  # Currently unnamed argument for imputing REG TD columns, defaults to TRUE
  url <- paste0("http://walterfootball.com/fantasy", season, "rankingsexcel.xlsx")

  xlsx_file <- tempfile("wf", fileext = ".xlsx")
  xl_download <- download.file(url = url, destfile = xlsx_file, mode = "wb", quiet = TRUE)


  l_pos <- lapply(pos, function(pos){

    cat(paste0("Scraping ", pos, " projections from"), url, sep = "\n  ")

    position <- case_when(pos %in% "QB" ~ "QBs",
                          pos %in% "RB" ~ "RBs",
                          pos %in% "WR" ~ "WRs",
                          pos %in% "TE" ~ "TEs",
                          pos %in% "K" ~ "Ks")

    # Data
    data <- if (pos %in% c("QB", "WR")) {
      # Supress "New names:..." message
      suppressMessages(
        readxl::read_xlsx(xlsx_file, sheet = position) %>%
          mutate(`First Name` = replace(`First Name`, `First Name` == "Marcua", "Marcus")) %>%
          tidyr::unite(col = Player, `First Name`, `Last Name`, sep = " ", remove = FALSE) %>%
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

    # Combine data w/ player ID's
    pos_df <- data %>%
      mutate(id = get_mfl_id(last = last_name, first = first_name, pos = position),
             data_src = "WalterFootball") %>%
      select(id, everything(), -last_name, -first_name)

    # New column names
    names(pos_df) = rename_vec(names(pos_df), walterfootball_columns)
    df_names = names(pos_df)


    if("reg_tds" %in% df_names) {
      if(all(c("rush_yds", "rec_yds") %in% df_names)) {
        total_yds = pos_df$rush_yds + pos_df$rec_yds
        pos_df$rush_tds = ifelse(total_yds == 0, 0,
                                 (pos_df$rush_yds / total_yds) * pos_df$reg_tds)
        pos_df$rec_tds = ifelse(total_yds == 0, 0,
                                 (pos_df$rec_yds / total_yds) * pos_df$reg_tds)
        pos_df$reg_tds = NULL
      } else if(any(c("rush_yds", "rec_yds") %in% df_names)) {
        col_name = grep("(rush|rec)_yds", df_names, value = TRUE)
        names(pos_df)[df_names == "reg_tds"] = sub("(rush|rec)_yds", "\\1_tds", col_name)
      }
    }

    pos_df
  })

  # list elements named by position
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos

}

# FleaFlicker ----
scrape_fleaflicker <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
                               season = NULL, week = NULL, draft = FALSE, weekly = TRUE) {

  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }

  # IDP positions
  if("DL" %in% pos) {
    pos <- c(pos, "DE", "DT")
    pos <- pos[!pos %in% "DL"]
  }
  if ("DB" %in% pos) {
    pos <- c(pos, "CB", "S")
    pos <- pos[! pos %in% "DB"]
  }


  base_link <- paste0("https://www.fleaflicker.com/nfl/leaders")
  site_session <- session(base_link)


  l_pos <- lapply(pos, function(pos){

    position <- case_when(pos %in% "QB" ~ 4,
                          pos %in% "RB" ~ 1,
                          pos %in% "WR" ~ 2,
                          pos %in% "TE" ~ 8,
                          pos %in% "K" ~ 16,
                          pos %in% "DST" ~ 256,
                          pos %in% "DE" ~ 2048,
                          # 16384,
                          pos %in% "DT" ~ 64,
                          # 4096,
                          pos %in% "LB" ~ 128,
                          pos %in% "CB" ~ 512,
                          pos %in% "S" ~ 1024)


    # Setting up hitting each page
    offset = 0L
    out_dfs = list()

    scrape_link <- paste0("https://www.fleaflicker.com/nfl/leaders?week=", week, "&statType=7&sortMode=7&position=",
                          position, "&tableOffset=", offset)

    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n ")


    # The number of pages to scrape by position
    pos_pages <- case_when(pos %in% c("K", "DST") ~ 2L,
                           pos %in% c("QB") ~ 2L,
                           pos %in% c("DT") ~ 4L,
                           pos %in% c("TE") ~ 5L,
                           pos %in% c("DE", "LB", "S") ~ 6L,
                           pos %in% c("RB") ~ 6L,
                           pos %in% c("CB") ~ 6L,
                           pos %in% c("WR") ~ 6L)


    # Going through pages of fleaflicker.com until a player has projected fantasy points of 0 or less
    # while(i == 0L || min(temp_df$site_pts > 1)) {
    for (i in seq_len(pos_pages)) {

      page_link = paste0("https://www.fleaflicker.com/nfl/leaders?week=", week, "&statType=7&sortMode=7&position=",
                         position, "&tableOffset=", offset)


      if(i != 1L) {
        Sys.sleep(2L)
      }

      # 20 rows of player data by position
      html_page <- site_session %>%
        session_jump_to(page_link) %>%
        read_html()

      # FleaFlicker player ID's
      fleaflicker_id <- html_page %>%
        html_elements(css = "a.player-text") %>%
        html_attr("href") %>%
        sub(".*\\-(\\d+)$", "\\1", .)

      # scrape contains list of player names and a list of data
      scrape <- html_page %>%
        html_elements(css = '#body-center-main table') %>%
        html_table() %>%
        base::`[[`(1L)

      bottom_row_check = rapply(scrape, function(x) !grepl("Previous.*Next", x), how = "replace")
      scrape = scrape[as.logical(rowMeans(bottom_row_check)), ]


      # Column names
      col_names <- paste(names(scrape), scrape[1, ])
      col_names <- gsub("Week\\s+\\d+|Projected", "", col_names)
      col_names <- trimws(gsub("\\s+", " ", col_names))

      if(pos == "K") {
        col_names[c(10, 11, 13, 14)] = c("fg_att", "fg_pct", "xp_att", "xp_pct")
      }


      col_names = rename_vec(col_names, fleaflicker_columns)
      col_names[is.na(col_names)] = paste0("...", seq_len(sum(is.na(col_names))))
      names(scrape) <- col_names

      # Filtering out empty columns
      scrape = type.convert(scrape[2:(nrow(scrape)), ],
                            as.is = TRUE, na.strings = c("—", "NA", ""))
      scrape = Filter(function(x) any(!is.na(x)), scrape)


      # Creating and cleaning table
      ## Suppress "New names:" message from .name_repair = "unique"
      suppressMessages(
        if (pos %in% "DST") {
          temp_df <- scrape %>%
            tidyr::extract(player, c("player", "team", "bye"),
                           "(.*)\\s+D/ST\\s+([A-Z]{2,3}).*?(\\d+).*") %>%
            mutate(pos = "DST",
                   data_src = "FleaFlicker",
                   id = get_mfl_id(fleaflicker_id, pos = pos, player_name = team),
                   src_id = fleaflicker_id) %>%
            select(player, pos, team, bye, everything())
        } else {
          temp_df <- scrape %>%
            as_tibble(.name_repair = "unique") %>%
            # Remove "Q" at beginning of Player Name if followed by another uppercase letter
            mutate(player = gsub("^Q(?=[A-Z])", "", player, perl = TRUE)) %>%
            extract(player, into = c("first_name", "last_name", "pos_temp", "team", "bye"),
                    regex = "(.*?)\\s+(.*?)\\s+(.*?)\\s+(.*?)\\s+.*(\\d+)\\)$", convert = TRUE) %>%
            tidyr::unite("player", first_name:last_name, sep = " ", remove = FALSE) %>%
            mutate(data_src = "FleaFlicker") %>%
            # rename for now so while loop works
            mutate(src_id = fleaflicker_id,
                   id = get_mfl_id(
                     id_col = fleaflicker_id,
                     player_name = player,
                     first = first_name,
                     last = last_name,
                     pos = pos,
                     team = team
                     ),
                   pos_temp = pos) %>%
            rename(pos = pos_temp) %>%
            select(-first_name, -last_name)
        }
      )

      # Adding it to a list of DF's from the pages
      out_dfs[[i]] = temp_df

      if(min(temp_df$site_pts) <= 1 || nrow(temp_df) < 20) {
        break
      }

      # Add 1 to i for page number counter
      # Add 20 to offset for next page's URL
      offset = offset + 20L

    }


    # combine df's from each page
    out = bind_rows(out_dfs)

    # Converting types of columns
    idx = names(out) %in% c("id", "src_id")
    out[!idx] = type.convert(out[!idx], as.is = TRUE)

    out


  })



  # list elements named by position
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week

  ## Combine defensive dataframes
  # combine DE and DT into DL
  if (exists("DE", where = l_pos) && exists("DT", where = l_pos)) {
    l_pos$DL <- bind_rows(l_pos$DE, l_pos$DT) %>%
      mutate(pos = "DL") %>%
      distinct(src_id, .keep_all = TRUE)

    l_pos$DE <- NULL
    l_pos$DT <- NULL
  }

  # combine CB and S into DB
  if (exists("CB", where = l_pos) && exists("S", where = l_pos)) {
    l_pos$DB <- bind_rows(l_pos$CB, l_pos$S) %>%
      mutate(pos = "DB") %>%
      distinct(src_id, .keep_all = TRUE)

    l_pos$CB <- NULL
    l_pos$S <- NULL
  }


  l_pos

}

# FFToday ----
scrape_fftoday <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
                           season = NULL, week = NULL, draft = TRUE, weekly = TRUE) {

  message("\nThe FFToday scrape uses a 2 second delay between pages")

  # Error on this scrape fixed by this:
  # i.e., https://github.com/jeroen/curl/issues/156
  # httr::set_config(httr::config(http_version = 2))
  new_http_v = structure(
    list(method = NULL, url = NULL, headers = NULL, fields = NULL,
         options = list(http_version = 2), auth_token = NULL, output = NULL),
    class = "request")
  old_httr_config = getOption("httr_config")
  if(!is.null(old_httr_config)) {
    configs = c(old_httr_config, new_http_v)
  } else {
    options(httr_config = new_http_v)
  }
  on.exit({
    options(httr_config = old_httr_config)
  })


  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }


  base_link <- paste0("https://www.fftoday.com/rankings/index.html")
  site_session <- rvest::session(base_link)

  if(week > 18) {
    week = week + 2L
  }
  if(week > 0) {
    pos = setdiff(pos, c("DST", "DL", "LB", "DB"))
  }


  l_pos <- lapply(pos, function(pos){

    position = dplyr::case_when(
      pos == "QB" ~ 10,
      pos == "RB" ~ 20,
      pos == "WR" ~ 30,
      pos == "TE" ~ 40,
      pos == "DL" ~ 50,
      pos == "LB" ~ 60,
      pos == "DB" ~ 70,
      pos == "K" ~ 80,
      pos == "DST" ~ 99
      )


    # Setting up hitting each page
    i = 1L
    cur_page = 0L
    out_dfs = list()

    if(week == 0) {
      scrape_link = paste0("https://www.fftoday.com/rankings/playerproj.php?PosID=", position,
                           "&LeagueID=1")
    } else {
      scrape_link = paste0("https://www.fftoday.com/rankings/playerwkproj.php?Season=", season,
                           "&GameWeek=", week, "&PosID=", position, "&LeagueID=1")
    }

    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n ")


    # The number of pages to scrape by position
    pos_pages <- dplyr::case_when(
      pos %in% c("QB", "TE", "K", "DST") ~ 1L,
      pos %in% c("RB") ~ 2L,
      pos %in% c("WR", "DL", "DB", "LB") ~ 3L
      )

    # Going through n pages (depending on position) pages
    while (i <= pos_pages) {

      Sys.sleep(2L)

      if(week == 0) {
        page_link = paste0("https://www.fftoday.com/rankings/playerproj.php?Season=",
                           season, "&PosID=", position, "&LeagueID=1",
                           "&order_by=FFPts&sort_order=DESC&cur_page=", cur_page)
      } else {
        page_link = paste0("https://www.fftoday.com/rankings/playerwkproj.php?Season=", season,
                           "&GameWeek=", week, "&PosID=", position, "&LeagueID=1",
                           "&order_by=FFPts&sort_order=DESC&cur_page=", cur_page)
      }


      html_page <- site_session %>%
        session_jump_to(page_link) %>%
        read_html()

      # FFToday player ID's
      if(pos == "DST") {
        fftoday_id = html_page %>%
          html_elements("a[href *='stats/players']") %>%
          html_attr("href") %>%
          sub(".*?=(\\d{4}).*", "\\1", .) %>%
          grep("\\d{4}", ., value = TRUE)
      } else {
        fftoday_id = html_page %>%
          html_elements("a[href *='stats/players/']") %>%
          html_attr("href") %>%
          dirname() %>%
          basename()
      }

      # scrape contains list of player names and a list of data
      scrape <- html_page %>%
        html_elements(css = "table table table") %>%
        html_table() %>%
        base::`[[`(1) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(),
                                    ~ gsub(",", "", .x, fixed = TRUE)))

      # Column names
      scrape[2, ] = as.list(sub("^(.*?)\\n.*", "\\1", scrape[2, ]))
      col_names <- trimws(paste(scrape[1, ], scrape[2, ]))
      col_names = rename_vec(col_names, fftoday_columns)

      if(pos %in% c("DL", "DB", "LB")) {
        col_names = gsub("(dst|pass)_", "idp_", col_names)
      }

      scrape[] = rapply(scrape, function(x) gsub("%", "", x, fixed = TRUE), how = "replace")


      # Dataframe
      temp_df = type.convert(scrape[-c(1:2), ], as.is = TRUE)
      names(temp_df) = col_names


      # Create / fix additional columns
      temp_df$pos = pos
      temp_df$data_src = "FFToday"
      temp_df$src_id = fftoday_id
      temp_df$chg = NULL

      if(week > 0) {
        temp_df$opp = gsub("@", "", temp_df$opp, fixed = TRUE)
      }
      if(pos == "DST") {
        temp_df$id = get_mfl_id(fftoday_id, pos = temp_df$pos)
      } else {
        temp_df$id = get_mfl_id(fftoday_id, player_name = temp_df$player,
                                team = temp_df$team, pos = temp_df$pos)
      }
      if("bye" %in% col_names) {
        temp_df$bye = as.integer(gsub("-", "", temp_df$bye, fixed = TRUE))
      }

      # Adding it to a list of DF's from the pages
      out_dfs[[i]] = temp_df


      # Add 1 to i for page number counter
      i = i + 1L
      cur_page = cur_page + 1L

    }

    # combine df's from each page
    dplyr::bind_rows(out_dfs[sapply(out_dfs, nrow) > 0]) # temp fix for zero row dfs


  })

  # list elements named by position
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos

}

# Fantasypros ----
scrape_fantasypros = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                              season = NULL, week = NULL, draft = TRUE, weekly = TRUE) {
  message("\nThe FantasyPros scrape uses a 2 second delay between pages")

  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }



  if(week > 0) {
    scrape_week = paste0(".php?week=", week)
  } else {
    scrape_week = ".php?week=draft"
  }

  base_link = paste0("https://www.fantasypros.com/nfl/projections")
  site_session = rvest::session(base_link)

  l_pos = lapply(pos, function(pos) {
    scrape_link = paste0("https://www.fantasypros.com/nfl/projections/",
                         tolower(pos), scrape_week)

    Sys.sleep(2L) # temporary, until I get an argument for honoring the crawl delay
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")

    html_page = site_session %>%
      rvest::session_jump_to(scrape_link) %>%
      rvest::read_html()


    # Getting column names
    if(pos %in% c("K", "DST")) {
      col_names = html_page %>%
        rvest::html_element("table > thead") %>%
        rvest::html_text2() %>%
        strsplit("\\t") %>%
        base::`[[`(1)

      col_names = rename_vec(col_names, fantasypros_columns)

    } else {
      col_names = html_page %>%
        rvest::html_element("table > thead") %>%
        rvest::html_table()

      col_names = trimws(paste(col_names[1, ], col_names[2, ]))
      col_names = rename_vec(col_names, fantasypros_columns)
    }

    # Get PID
    fantasypro_num_id = html_page %>%
      rvest::html_elements("table > tbody > tr > td.player-label > a:nth-child(2)") %>%
      rvest::html_attr("class") %>%
      sub(".+\\-", "", .)

    # Creating and cleaning table
    out_df = html_page %>%
      rvest::html_element("table > tbody") %>%
      rvest::html_table() %>%
      mutate(across(everything(), ~gsub(",", "", .x, fixed = TRUE)))

    names(out_df) = col_names

    # Adding a few columns
    if(pos == "DST") {
      out_df$src_id = fantasypro_num_id
      out_df$data_src = "FantasyPros"
      out_df$pos = pos
      out_df$id = get_mfl_id(fantasypro_num_id)
    } else {
      out_df = out_df %>%
        extract(player, c("player", "team"), "(.*)\\s+([A-Z]{2,3})") %>%
        mutate(src_id = fantasypro_num_id,
               data_src = "FantasyPros",
               pos = pos,
               id = get_mfl_id(fantasypro_num_id, player_name = player, team = team, pos = pos))
    }

    # Misc cleanup before done
    idx = names(out_df) %in% c("id", "src_id")
    out_df[!idx] = type.convert(out_df[!idx], as.is = TRUE)
    out_df[out_df$site_pts > 0,]
  })
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos
}

# RTSports ----
scrape_rtsports = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                           season = NULL, week = 0, draft = TRUE, weekly = FALSE) {

  message("\nThe RTSports scrape uses a 5 second delay between pages")

  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }
  if(week > 0) {
    stop("RTS Sports projections are only available for week 0")
  }

  base_url = "https://www.freedraftguide.com/football/draft-guide-rankings-provider.php"

  l_pos = lapply(pos, function(x) {
    if(x != pos[1]) {
      Sys.sleep(5)
    }
    req = httr2::request(base_url) %>%
      httr2::req_url_query(POS = rts_pos_idx[x])

    cat(paste0("Scraping ", x, " projections from"), req$url, sep = "\n  ")
    rts_json = httr2::req_perform(req) %>%
      httr2::resp_body_json()

    p_info = rrapply::rrapply(
      rts_json,
      function(x, .xname, .xpos) {
        .xname %in% c("player_id", "stats_id", "name", "nfl_team") & length(.xpos) == 3
      },
      how = "melt") %>%
      tidyr::pivot_wider(names_from = L3, values_from = value) %>%
      dplyr::select(player_id, stats_id, name, nfl_team)

    p_data = rrapply::rrapply(
      rts_json,
      function(x, .xparents) "stats" %in% .xparents,
      how = "melt"
    ) %>%
      tidyr::pivot_wider(names_from = L4, values_from = value) %>%
      dplyr::select(-c(L1:L3)) %>%
      Filter(f = function(x) any(x[1] != x, na.rm = TRUE))

    if(x %in% c("RB", "WR", "TE") && "pass_yds" %in% names(p_data)) {
      if(!"pass_atts" %in% names(p_data)) {
        p_data[["pass_atts"]] = 0L
      }
    }

    out_df = dplyr::bind_cols(p_info, p_data)

    names(out_df) <- rename_vec(names(out_df), rts_columns)
    if(x != "DST") {
      out_df = out_df[out_df$site_pts > 0, ]
    }

    out_df = type.convert(out_df, as.is = TRUE)
    out_df$pos = x
    out_df$id = get_mfl_id(out_df$stats_id,
                           player_name = out_df$player,
                           team = out_df$team,
                           pos = out_df$pos)
    out_df$src_id = as.character(out_df$src_id)
    out_df$stats_id = NULL
    out_df$data_src = "RTSports"
    dplyr::select(out_df, id, src_id, pos, data_src, dplyr::everything())
  })

  # list elements named by position
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos
}

# ESPN ----
scrape_espn = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = NULL, week = NULL,
                       draft = TRUE, weekly = TRUE) {

  message("\nThe ESPN scrape uses a 2 second delay between pages")

  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }

  slot_nums = c("QB" = 0, "RB" = 2, "WR" = 4, "TE" = 6, "K" = 17, "DST" = 16)
  position = pos

  l_pos = lapply(position, function(pos){

    if(pos != position[1]) {
      Sys.sleep(2)
    }

    pos_idx = slot_nums[pos]
    limit = dplyr::case_when(
      pos == "QB" ~ 42,
      pos == "RB" ~ 100,
      pos == "WR" ~ 150,
      pos == "TE" ~ 60,
      pos == "K" ~ 35,
      pos == "DST" ~ 32
    )
    base_url = paste0(
      "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/", season,
      "/segments/0/leaguedefaults/3?scoringPeriodId=0&view=kona_player_info"
    )
    cat(paste0("Scraping ", pos, " projections from"),
        "https://fantasy.espn.com/football/players/projections", sep = "\n  ")

    if(week == 0) {
      filter_split_id = 0
    } else {
      filter_split_id = 1
    }

    fantasy_filter = paste0(
      '{"players":{',
      '"filterSlotIds":{"value":[', pos_idx, ']},',
      '"filterStatsForSourceIds":{"value":[1]},',
      '"filterStatsForSplitTypeIds":{"value":[', filter_split_id, ']},',
      '"sortAppliedStatTotal":{"sortAsc":false,"sortPriority":3,"value":"11', season, week, '"},',
      '"sortDraftRanks":{"sortPriority":2,"sortAsc":true,"value":"PPR"},',
      '"sortPercOwned":{"sortAsc":false,"sortPriority":4},',
      '"limit":', limit, ',',
      '"offset":0,',
      '"filterRanksForScoringPeriodIds":{"value":[2]},',
      '"filterRanksForRankTypes":{"value":["PPR"]},',
      '"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},',
      '"filterStatsForTopScoringPeriodIds":{"value":2,',
      '"additionalValue":["00', season, '","10', season, '","11', season, week, '","02', season, '"]}}}'
    )

    espn_json = httr2::request(base_url) %>%
      httr2::req_method("GET") %>%
      httr2::req_headers(
        Accept = "application/json",
        `Accept-Encoding` = "gzip, deflate, br",
        Connection = "keep-alive",
        Host = "lm-api-reads.fantasy.espn.com",
        `X-Fantasy-Source` = "kona",
        `X-Fantasy-Filter` = fantasy_filter,
      ) %>%
      httr2::req_user_agent("ffanalytics R package (https://github.com/FantasyFootballAnalytics/ffanalytics)") %>%
      httr2::req_perform() %>%
      httr2::resp_body_json() %>%
      base::`[[`("players")

    l_players = vector("list", length(espn_json))

    for(i in seq_along(espn_json)) {

      # Checking for empty stats (bye weeks)
      if(length(espn_json[[i]]$player$stats) == 0L) {
        next
      }

      # Player stats (only those on)
      l_players[[i]] = espn_json[[i]]$player$stats[[1]]$stats
      l_players[[i]] = l_players[[i]][names(l_players[[i]]) %in% names(espn_columns)]
      names(l_players[[i]]) = espn_columns[names(l_players[[i]])]
      l_players[[i]][] = lapply(l_players[[i]], round)

      # Misc player info
      l_players[[i]]$espn_id = espn_json[[i]]$id
      l_players[[i]]$player_name = espn_json[[i]]$player$fullName
      l_players[[i]]$team = espn_team_nums[as.character(espn_json[[i]]$player$proTeamId)]
      l_players[[i]]$position = pos
    }

    out_df = dplyr::bind_rows(l_players)
    out_df$data_src = "ESPN"

    if(pos == "DST") { # ESPN ID's coming in as negative for 2023 wk 0 DST
      out_df$id = ffanalytics:::get_mfl_id(
        team = out_df$team,
        pos = out_df$position
      )
    } else {
      out_df$id = ffanalytics:::get_mfl_id(
        out_df$espn_id,
        player_name = out_df$player_name,
        pos = out_df$position,
        team = out_df$team
      )
    }

    out_df = out_df %>%
      dplyr::select(id, src_id = espn_id, pos = position,
                    player = player_name, team, dplyr::everything())

    idx = names(out_df) %in% c("id", "src_id")
    out_df[idx] = lapply(out_df[idx], as.character)
    out_df[!idx] = type.convert(out_df[!idx], as.is = TRUE)
    out_df
  })

  names(l_pos) = position

  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos

}

# Fantasy football nerd ----
scrape_fantasyfootballnerd = function(pos = NULL, season = NULL, week = NULL,
                                      draft = TRUE, weekly = TRUE, ffnerd_api_key = NULL) {
  message(
    "\nThe FantasyFootballNerd scrape is not implemeted yet--we are working on it"
    )

}

scrape_fantasyfootballnerd_beta = function(pos = NULL, season = NULL, week = NULL,
                                           draft = TRUE, weekly = TRUE, ffnerd_api_key = NULL) {

  draft_url = "https://api.fantasynerds.com/v1/nfl/draft-projections?apikey=TEST"
  pos_url = "https://api.fantasynerds.com/v1/nfl/ros?apikey=TEST"
  weekly_url = "https://api.fantasynerds.com/v1/nfl/weekly-projections?apikey=TEST"

}


# FantasyData ----
scrape_fantasydata = function(pos = NULL, season = NULL, week = NULL,
                              draft = TRUE, weekly = TRUE, fantasydata_api_key = NULL) {
  message(
    "\nThe FantasyData scrape is behind a paywall and is not supported at this time"
      )
}

# Depreceated ----
scrape_yahoo = function(pos = NULL, season = NULL, week = NULL,
                        draft = TRUE, weekly = TRUE) {
  message(
    "\nThe Yahoo scrape is no longer supported because they now use FantasyPros projections"
    )
}
















