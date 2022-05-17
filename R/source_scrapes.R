

# CBS ----
scrape_cbs = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = 2021, week = 0,
                      draft = TRUE, weekly = TRUE) {


  if(week %in% c(0, "ros")) {
    scrape_week = "restofseason"
  } else {
    scrape_week = week
  }

  base_link = paste0("https://www.cbssports.com/fantasy/football/")
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

# NFL ----
scrape_nfl = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = 2021, week = 0,
                      draft = TRUE, weekly = TRUE) {

  pos_scrape = nfl_pos_idx[pos]

  base_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape[1],
                     "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                     "&statType=seasonProjectedStats")

  site_session = session(base_link)

  l_pos = lapply(pos, function(pos) {
    pos_scrape = nfl_pos_idx[pos]

    n_records = case_when(
      pos == "QB" ~ 40,
      pos == "RB" ~ 90,
      pos == "WR" ~ 150,
      pos == "TE" ~ 50,
      pos == "K" ~ 32,
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
    }

    if(pos %in% c("RB", "WR", "TE") && "pass_int" %in% names(out_df)) {
      out_df$pass_int = NULL
    }

    # Misc column cleanup before done
    out_df$data_src = "NFL"
    out_df$src_id = as.character(site_id)
    out_df$opp = NULL

    # Type cleanup
    out_df[out_df == "-"] = NA
    idx = names(out_df) %in% c("id", "src_id")
    out_df[!idx] = type.convert(out_df[!idx], as.is = TRUE)

    # Combining df's, removing NA's, filtering out rows with
    out_df = out_df[out_df$site_pts > 0 & !is.na(out_df$site_pts), ]
    # Adding IDs
    out_df$id = ffanalytics:::player_ids$id[match(out_df$src_id, ffanalytics:::player_ids$nfl_id)]

    Sys.sleep(1L) # temporary, until I get an argument for honoring the crawl delay

    # Removing all NA columns
    Filter(function(x) any(!is.na(x)), out_df)

  })

  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos
}

# Fantasysharks ----
scrape_fantasysharks <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"), season = 2021, week = 0,
                                 draft = TRUE, weekly = TRUE) {

  # historical scrapes (doesn't work)
  year <- case_when(season == 2021 ~ 714,
                    season == 2020 ~ 682,
                    season == 2019 ~ 650,
                    season == 2018 ~ 618,
                    season == 2017 ~ 586)

  # segment for url from user week input
  if (week == 0) {
    segment <- year
  } else if (week %in% c(1:22)) {
    segment <- year + week + 8
  } else if (week %in% "ros") {
    segment <- 717
  }



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

    Sys.sleep(1L) # temporary, until I get an argument for honoring the crawl delay
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")

    scrape <- scrape_link %>%
      read_html()


    # Rename duplicated column names
    data <- if (pos %in% "RB" & week %in% c(0, "ros")) {

      # suppress the "new names" message generated from .name_repair = "unique"
      suppressMessages(
        scrape %>%
          html_nodes(xpath = '//*[@id="toolData"]') %>%
          html_table() %>%
          # `[[`(1) %>%
          pluck(1L) %>%
          as_tibble(.name_repair = "unique") %>%
          filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
          rename(">= 50 yd rush" = ">= 50 yd...13",
                 ">= 100 yd rush" = ">= 100 yd...14",
                 ">= 50 yd rec" = ">= 50 yd...20",
                 ">= 100 yd rec" = ">= 100 yd...21") %>%
          select(-`#`)
      )

    } else if (pos %in% "RB" & week %in% c(1:22)) {

      # suppress the "new names" message generated from .name_repair = "unique"
      suppressMessages(
       scrape %>%
         html_nodes(xpath = '//*[@id="toolData"]') %>%
         html_table() %>%
         pluck(1L) %>%
         as_tibble(.name_repair = "unique") %>%
         filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
         rename("opp" = "Opp...4",
                 ">= 50 yd rec" = ">= 50 yd",
                 ">= 100 yd rec" = ">= 100 yd",
                 "opp_num" = "Opp...23") %>%
          select(-`#`)
      )

    } else if (pos %in% c("WR", "TE") & week %in% c(0, "ros")) {

      scrape %>%
        html_nodes(xpath = '//*[@id="toolData"]') %>%
        html_table() %>%
        pluck(1L) %>%
        filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
        rename(">= 50 yd rec" = ">= 50 yd",
               ">= 100 yd rec" = ">= 100 yd",
               ">= 150 yd rec" = ">= 150 yd",
               ">= 200 yd rec" = ">= 200 yd") %>%
        select(-`#`)

    } else if (pos %in% c("WR", "TE") & week %in% c(1:22)) {

      # suppress the "new names" message generated from .name_repair = "unique"
      suppressMessages(
        scrape %>%
          html_nodes(xpath = '//*[@id="toolData"]') %>%
          html_table() %>%
          pluck(1L) %>%
          as_tibble(.name_repair = "unique") %>%
          filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
          rename("opp" = "Opp...4",
                 "opp_num" = "Opp...20") %>%
          select(-`#`)
      )

    } else if (pos %in% "DST") {

      scrape %>%
        html_nodes(xpath = '//*[@id="toolData"]') %>%
        html_table() %>%
        pluck(1L) %>%
        filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
        rename("dst_int" = "Int",
               "dst_fum_rec" = "Fum") %>%
        select(-`#`)

    } else if (pos %in% c("DL", "LB", "DB")) {

      scrape %>%
        html_nodes(xpath = '//*[@id="toolData"]') %>%
        html_table() %>%
        pluck(1L) %>%
        filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
        rename("idp_sack" = "Scks",
               "idp_int" = "Int",
               "idp_fum_rec" = "Fum",
               "idp_tds" = "DefTD") %>%
        select(-`#`)

    } else if (pos %in% "QB" & week %in% c(1:22)) {

      # suppress the "new names" message generated from .name_repair = "unique"
      suppressMessages(
        scrape %>%
          html_nodes(xpath = '//*[@id="toolData"]') %>%
          html_table() %>%
          pluck(1L) %>%
          as_tibble(.name_repair = "unique") %>%
          filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
          rename("opp" = "Opp...4",
                 "opp_num" = "Opp...21") %>%
          select(-`#`)
      )

    } else {

      scrape %>%
        html_nodes(xpath = '//*[@id="toolData"]') %>%
        html_table() %>%
        pluck(1L) %>%
        filter(!str_detect(`#`, paste(c("Tier", "#", "Points Awarded"), collapse = '|'))) %>%
        select(-`#`)

    }

    # Player ID's
    p_id <- scrape %>%
      html_elements(css = 'td.playerLink a') %>%
      html_attr("href") %>%
      tibble(id = .) %>%
      mutate(id = str_extract(id, "[:digit:]{3,5}$"))

    if(pos == "DST") {
      p_id = p_id %>%
        mutate(id = str_pad(id, 4, side = "left", 0))
    }

    # Combine player data with ID's
    pos_df <- data %>%
      mutate(pos = !!pos,
             data_src = "FantasySharks") %>%
      bind_cols(p_id, .)

    # Rename columns with new names
    # names(pos_df) = fantasysharks_columns[names(pos_df)]
    names(pos_df) = rename_vec(names(pos_df), fantasysharks_columns)


    pos_df[-1] = type.convert(pos_df[-1], as.is = TRUE)
    pos_df[pos_df$site_pts > 0, ]

  })


  # list elements named by position
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos

}

# Numberfire ----
scrape_numberfire <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST", "LB", "DB", "DL"), season = 2021, week = 0,
                              draft = TRUE, weekly = TRUE) {

  base_link <- paste0("https://www.numberfire.com/nfl/fantasy/fantasy-football-projections")
  site_session <- session(base_link)

  # That IDP scrapes grabs everyone, this only hits website once grabs the positions later
  if(any(pos %in% c("LB", "DB", "DL"))) {
    site_pos <- c(setdiff(pos, c("LB", "DB", "DL")), "LB")
  } else {
    site_pos <- pos
  }


  l_pos <- lapply(site_pos, function(pos){

    position <- case_when(pos %in% "QB" ~ "qb",
                          pos %in% "RB" ~ "rb",
                          pos %in% "WR" ~ "wr",
                          pos %in% "TE" ~ "te",
                          pos %in% "K" ~ "k",
                          pos %in% "DST" ~ "d",
                          pos %in% "LB" ~ "idp")

    scrape_link <- case_when(week %in% c(0, "ros") ~ paste0("https://www.numberfire.com/nfl/fantasy/remaining-projections/", position),
                             week > 0 ~ paste0("https://www.numberfire.com/nfl/fantasy/fantasy-football-projections/", position))

    Sys.sleep(1L) # temporary, until I get an argument for honoring the crawl delay
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")


    # grabbing position page
    html_page <- site_session %>%
      session_jump_to(scrape_link) %>%
      read_html()


    # numberFire unique player ID's
    numberfire_ids <- html_page %>%
      html_elements(css = "td[class='player'] a") %>%
      html_attr("href") %>%
      basename() %>%
      tibble(numfire_id = .)

    # scrape contains list of player names and a list of data
    scrape <- html_page %>%
      html_elements(css = 'table.projection-table') %>%
      html_table()


    # player names
    players <- scrape[[1]] %>%
      rename(Player = 1) %>%
      slice(-1L) %>%
      extract(Player, into = c("Player", "position", "team"),
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
      select(id, numfire_id)

    # combine numberFire ID's with player names and data
    pos_df <- numberfire_ids %>%
      bind_cols(players) %>%
      bind_cols(table) %>%
      left_join(FFA_ids, by = "numfire_id") %>%
      mutate(data_src = "NumberFire")

    # New column names
    # subset by the columns in pos_df
    # replicated names between DST and IDP positions
    if (pos %in% c("DB", "LB", "DL")) {
      # numberfire_idp_columns[names(pos_df)]
      names(pos_df) <- rename_vec(names(pos_df), numberfire_idp_columns)
    } else {
      # names(pos_df) <- numberfire_columns[names(pos_df)]
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
scrape_walterfootball <- function(pos = c("QB", "RB", "WR", "TE", "K"), season = 2021, week = 0,
                                  draft = TRUE, weekly = FALSE, impute_reg = TRUE) {

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
      transmute(id,
                join_idx = paste0(gsub("[[:punct:]]|\\s+", "", tolower(first_name)),
                                  gsub("[[:punct:]]|\\s+", "", tolower(last_name)),
                                  tolower(position)))

    # Combine data w/ player ID's
    pos_df <- data %>%
      mutate(data_src = "Walterfootball",
             join_idx = gsub("[[:punct:]]|\\s+", "", tolower(Player)),
             join_idx = paste0(join_idx, tolower(position))) %>%
      left_join(FFA_ids, by = "join_idx") %>%
      select(-last_name, -first_name, -join_idx)


    # New column names
    # subset by the columns in pos_df
    names(pos_df) = walterfootball_columns[names(pos_df)]
    df_names = names(pos_df)


    if(impute_reg && "reg_tds" %in% df_names) {
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
                               season = 2021, week = 1, draft = FALSE, weekly = TRUE) {

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


      Sys.sleep(1L)


      # 20 rows of player data by position
      html_page <- site_session %>%
        session_jump_to(page_link) %>%
        read_html()

      # FleaFlicker player ID's
      site_id <- html_page %>%
        html_elements(css = "a.player-text") %>%
        html_attr("href") %>%
        sub(".*\\-(\\d+)$", "\\1", .)

      # scrape contains list of player names and a list of data
      scrape <- html_page %>%
        html_elements(css = '#body-center-main table') %>%
        html_table() %>%
        pluck(1L)

      # Column names
      col_names <- paste(names(scrape), scrape[1, ])
      col_names <- gsub("Week\\s+\\d+|Projected", "", col_names)
      col_names <- trimws(gsub("\\s+", " ", col_names))

      if(pos == "K") {
        col_names[c(10, 11, 13, 14)] = c("fg_att", "fg_pct", "xp_att", "xp_pct")
      }


      col_names = fleaflicker_columns[col_names]
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
            mutate(pos = "DST",
                   team = str_extract(player, "(?<=D/ST )\\w{2,3}"),
                   bye = str_extract(player, "(\\d+)"),
                   player = str_extract(player, ".+(?= D/ST)"),
                   data_src = "FleaFlicker",
                   scr_id = site_id) %>%
            select(player, pos, team, bye, everything())
        } else {
          temp_df <- scrape %>%
            as_tibble(.name_repair = "unique") %>%
            # Remove "Q" at beginning of Player Name if followed by another uppercase letter
            mutate(player = str_remove(player, "^Q(?=[:upper:])")) %>%
            extract(player, into = c("first_name", "last_name", "pos_temp", "team", "bye"),
                    regex = "(.*?)\\s+(.*?)\\s+(.*?)\\s+(.*?)\\s+.*(\\d+)\\)$", convert = TRUE) %>%
            unite("player", first_name:last_name, sep = " ") %>%
            mutate(bye = as.integer(str_extract(bye, pattern = "\\d+"))) %>%
            mutate(data_src = "FleaFlicker") %>%
            # rename for now so while loop works
            mutate(src_id = site_id,
                   pos_temp = pos) %>%
            rename(pos = pos_temp)
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
scrape_fftoday <- function(pos = c("QB", "RB", "WR", "TE", "K"),
                           season = 2021, week = 0, draft = FALSE, weekly = TRUE) {

  base_link <- paste0("https://www.fftoday.com/rankings/index.html")
  site_session <- session(base_link)

  if(week > 18) {
    week = week + 2L
  }


  l_pos <- lapply(pos, function(pos){

    position <- case_when(pos %in% "QB" ~ 10,
                          pos %in% "RB" ~ 20,
                          pos %in% "WR" ~ 30,
                          pos %in% "TE" ~ 40,
                          pos %in% "K" ~ 80)


    # Setting up hitting each page
    i = 1L
    cur_page = 0L
    out_dfs = list()


    scrape_link <- paste0("https://www.fftoday.com/rankings/playerwkproj.php?Season=", season,
                          "&GameWeek=", week, "&PosID=", position, "&LeagueID=1")


    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n ")


    # The number of pages to scrape by position
    pos_pages <- case_when(pos %in% c("QB", "TE", "K") ~ 1L,
                           pos %in% c("RB", "WR") ~ 2L)

    # Going through n pages (depending on position) pages
    while (i <= pos_pages) {

      Sys.sleep(1L)
      page_link = paste0("https://www.fftoday.com/rankings/playerwkproj.php?Season=", season,
                         "&GameWeek=", week, "&PosID=", position, "&LeagueID=1",
                         "&order_by=FFPts&sort_order=DESC&cur_page=", cur_page)

      html_page <- site_session %>%
        session_jump_to(page_link) %>%
        read_html()

      # FFToday player ID's
      site_id <- html_page %>%
        html_elements(css = "a[href *='stats/players/']") %>%
        html_attr("href") %>%
        str_extract("[0-9]{2,8}") %>%
        as_tibble() %>%
        rename(fftoday_id = value)

      # scrape contains list of player names and a list of data
      scrape <- html_page %>%
        html_elements(css = "table table table") %>%
        html_table() %>%
        `[[`(1)

      # Column names
      scrape[2, ] = as.list(sub("^(.*?)\\n.*", "\\1", scrape[2, ]))
      col_names <- trimws(paste(scrape[1, ], scrape[2, ]))
      col_names = fftoday_columns[col_names]

      # Dataframe
      temp_df = type.convert(scrape[-c(1:2), ], as.is = TRUE)
      names(temp_df) = col_names


      # Create / fix additional columns
      temp_df$opp = sub("@", "", temp_df$opp, fixed = TRUE)
      temp_df$pos = pos
      temp_df$data_src = "FFToday"
      temp_df$src_id = site_id$fftoday_id
      temp_df$id = player_ids$id[match(temp_df$src_id, player_ids$fftoday_id)]
      temp_df$chg = NULL


      # Adding it to a list of DF's from the pages
      out_dfs[[i]] = temp_df


      # Add 1 to i for page number counter
      i = i + 1L
      cur_page = cur_page + 1L

    }

    # combine df's from each page
    bind_rows(out_dfs[sapply(out_dfs, nrow) > 0]) # temp fix for zero row dfs


  })

  # list elements named by position
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos

}


# Fantasypros ----
scrape_fantasypros = function(pos = c("QB", "RB", "WR", "TE", "K", "DST"), season = 2021, week = 0,
                      draft = TRUE, weekly = TRUE) {


  if(week %in% 1:17) {
    scrape_week = paste0(".php?week=", week)
  } else {
    scrape_week = ".php"
  }

  base_link = paste0("https://www.fantasypros.com/nfl/projections")
  site_session = session(base_link)

  l_pos = lapply(pos, function(pos) {
    scrape_link = paste0("https://www.fantasypros.com/nfl/projections/",
                         tolower(pos), scrape_week)

    Sys.sleep(1L) # temporary, until I get an argument for honoring the crawl delay
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")

    html_page = site_session %>%
      session_jump_to(scrape_link) %>%
      read_html()


    # Getting column names
    if(pos %in% c("K", "DST")) {
      col_names = html_page %>%
        html_element("table > thead") %>%
        html_text2() %>%
        strsplit("\\t") %>%
        `[[`(1)

      col_names = fantasypros_columns[col_names]

    } else {
      col_names = html_page %>%
        html_element("table > thead") %>%
        html_table()

      col_names = trimws(paste(col_names[1, ], col_names[2, ]))
      col_names = fantasypros_columns[col_names]
    }

    # Get PID
    site_id = html_page %>%
      html_elements("table > tbody > tr > td.player-label > a:nth-child(2)") %>%
      html_attr("class") %>%
      sub(".+\\-", "", .) %>%
      as.numeric()

    # Creating and cleaning table
    out_df = html_page %>%
      html_element("table > tbody") %>%
      html_table()

    names(out_df) = col_names

    # Adding a few columns
    if(pos == "DST") {
      out_df$src_id = site_id
      out_df$data_src = "CBS"
      out_df$pos = pos
      out_df$id = player_ids$id[match(out_df$src_id, player_ids$fantasypro_num_id)]
    } else {
      out_df = out_df %>%
        extract(player, c("player", "team"), "(.*)\\s+([A-Z]{2,3})") %>%
        mutate(src_id = site_id,
               data_src = "CBS",
               pos = pos,
               id = player_ids$id[match(src_id, player_ids$fantasypro_num_id)])
    }

    # Misc cleanup before done
    idx = names(out_df) == "id"
    out_df[!idx] = type.convert(out_df[!idx], as.is = TRUE)
    out_df[out_df$site_pts > 0,]
  })
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos
}












