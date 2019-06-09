html_sites <- list(
  #### CBS ####
  CBS = list(
    base = "https://www.cbssports.com/fantasy/football/stats/",
    get_path = function(season, week, position){
      period <- ifelse(week == 0, "season", as.character(week))
      paste(toupper(position), season, period,"projections/nonppr", sep  = "/")
    },
    get_query = NULL,  #function(season, week, pos_id, ...)list(print_rows = 9999),
    min_week = 0,
    max_week = 17,
    season_pos = c("QB", "RB", "WR", "TE", "K", "DST"),
    week_pos = c("QB", "RB", "WR", "TE", "K", "DST"),

    id_col = "cbs_id",
    table_css = "table.TableBase-table",
    pid_css = "table.TableBase-table a[href *= 'players']",
    rm_elem = list("colgroup", "div.Tablebase-tooltip", "span.CellPlayerName--short"),
    extract_pid = function(p_node){
      p_node %>% html_attr("href") %>% str_extract("[0-9]{2,8}")},
    split_cols = list(
      list(
        col = function(p)list(TRUE ~ "Player"),
        into = function(p)list(TRUE ~ c("player", "position", "team")),
        regex = function(p)list(TRUE ~ "([A-Za-z0-9'-. ]+)[\\s[:cntrl:]]+([A-Z]+)[\\s[:cntrl:]]+•\\s([A-Z]+$)")
      )),
    stat_cols = c(
      pass_att = "Passing Att", pass_comp = "Passing Cmp", pass_yds = "Passing Yds",
      pass_tds = "Passing TD",  pass_int = "Passing INT", pass_comp_pct = "Passing CmpPct",
      pass_avg = "Passing YAtt", rush_att = "Rushing Att", rush_yds = "Rushing Yds",
      rush_avg = "Rushing Avg", rush_tds = "Rushing TD", rec = "Receiving Rec",
      rec_tgt = "Receiving Tgt", pass_rate = "Passing Rate", pass_yds_g = "Passing yds/g",
      rec_yds_g = "Receiving Yds/g", games = "GP",
      rec_yds = "Receiving Yds", rec_avg = "Receiving Avg", rec_tds = "Receiving TD",
      fumbles_lost = "Misc FL",  fg = "FG FGM", fg_att = "FG FGA", fg_long = "fg lng",
      fg_0019 = "FG 1-19", fg_2029 = "FG 20-29", fg_3039 = "FG 30-39", fg_4049 = "FG 40-49",
      fg_50 = "FG 50", fg_att_0019 = "FG 1-19A", fg_att_2029 = "FG 20-29A",
      fg_att_3039 = "FG 30-39A", fg_att_4049 = "FG 40-49A", fg_att_50 = "FG 50+A",
      xp = "XP XPM", xp_att = "XP XPA",
      dst_int = "INT", dst_tackles = "TK",
      dst_fum_rec = "FREC", dst_fum_force = "FUM", dst_sacks = "SCK", dst_td = "DTD",
      dst_safety = "SfTY",  dst_pts_allowed = "PTS", dst_yds_allowed = "Yards Allowed total",
      dst_pts_allowed_g = "PPG", dst_rush_yds_allowed = "Yards Allowed rush",
      dst_pass_yds_allowed = "Yards Allowed pass", dst_avg_yds_allowed = "Yards Allowed avg",
      site_pts = "FPTS", site_pts = "Misc FPTS", fppg = "Misc FPPG")
  ),
  #### ESPN ####
  ESPN = list(
    base = "http://games.espn.com/ffl/tools/projections",
    get_query = function(season, week, pos_id, ...){
      query <-list(slotCategoryId = pos_id)
      if(week == 0){
        query$seasonTotals <- "true"
      } else {
        query$scoringPeriodId <- week
      }
      query$seasonId <- season
      query$startIndex <- 0
      return(query)
    },
    url_positions = function(p)switch(p, QB = 0, RB = 2, WR = 4,TE = 6, DST = 16, K = 17),
    min_week = -1,
    max_week = -1,
    season_pos = c("QB", "RB", "WR", "TE", "K", "DST"),
    week_pos = c("QB", "RB", "WR", "TE", "K", "DST"),

    id_col = "espn_id",
    table_css = "#playertable_0",
    pid_css = "table td.playertablePlayerName a.flexpop:first-of-type",
    extract_pid = function(p_node){
      p_node %>% html_attr("playerid")},

    split_cols = list(
      list(
        col = function(p)list(TRUE ~ "PLAYER, TEAM POS"),
        into = function(p)list(p == "DST" ~  c("player", "pos", NA, NA),
                               TRUE ~ c("player", "team", "pos", "status")),
        regex = function(p)list(p == "DST" ~ "([A-Za-z0-9 ./'-\\*]+)\\s([A-Za-z/]+)",
                                TRUE ~ "([A-Za-z .'-\\*]+),\\s([A-Za-z]+)\\s*([A-Za-z]+)\\s*([A-Za-z]*)")
      ),
      list(
        col = function(p)list(TRUE ~ "PASSING C/A"),
        into = function(p)list(TRUE ~ c("pass_comp", "pass_att")),
        sep = function(p)list(TRUE ~ "/")
      ),
      list(
        col = function(p)list(TRUE ~ "KICKING 1-39"),
        into = function(p)list(TRUE ~ c("fg_0039", "fg_att_0039")),
        sep = function(p)list(TRUE ~ "/")
      ),
      list(
        col = function(p)list(TRUE ~ "KICKING 40-49"),
        into = function(p)list(TRUE ~ c("fg_4049", "fg_att_4049")),
        sep = function(p)list(TRUE ~ "/")
      ),
      list(
        col = function(p)list(TRUE ~ "KICKING 50"),
        into = function(p)list(TRUE ~ c("fg_50", "fg_att_50")),
        sep = function(p)list(TRUE ~ "/")
      ),
      list(
        col = function(p)list(TRUE ~ "KICKING TOT"),
        into = function(p)list(TRUE ~ c("fg", "fg_att")),
        sep = function(p)list(TRUE ~ "/")
      ),
      list(
        col = function(p)list(TRUE ~ "KICKING XP"),
        into = function(p)list(TRUE ~ c("xp", "xp_att")),
        sep = function(p)list(TRUE ~ "/")
      )
    ),
    stat_cols = c(
      pass_yds = "PASSING YDS", pass_tds = "PASSING TD", pass_int = "PASSING INT",
      rush_att = "RUSHING RUSH",rush_yds = "RUSHING YDS", rush_tds = "RUSHING TD",
      rec = "RECEIVING REC", rec_yds = "RECEIVING YDS", rec_tds = "RECEIVING TD",
      dst_tackle = "DEFENSIVE TT", dst_sacks = "DEFENSIVE SCK", dst_fum_force = "DEFENSIVE FF",
      dst_fum_rec = "DEFENSIVE FR", dst_int = "DEFENSIVE INT", dst_int_tds = "TD RETURNS ITD",
      dst_ret_tds = "TD RETURNS FTD", site_pts = "TOTAL PTS"
    )

  ),
  #### FantasyData ####
  FantasyData = list(
    base = "https://fantasydata.com/nfl-stats/fantasy-football-weekly-projections.aspx",
    get_query = function(season, week, pos_id, ...){
      query <- list(fs = 0, stype = 0, sn = 0, scope = 1, w = -1, ew = -1, s = "",
                    t = 0, p = 9, st="FantasyPoints", d = 1, ls = "", live="false",
                    pid="true", minsnaps=4)
      if(week > 0){
        query$w <- week - 1
        query$ew <- week -1
        query$scope <- 1
        query$stype <- ifelse(week > 17, 1, 0)
      } else {
        query$scope <- 0
        query$stype <- 0
        query$w <- 0
        query$ew <- 0
      }
      query$p <- pos_id
      return(query)
    },
    url_positions = function(p)switch(p, QB = 1, RB = 2, WR = 3, TE= 4, K = 5, DST= 6),
    min_week = -1,
    max_week = -1,
    season_pos = c("QB", "RB", "WR", "TE", "K", "DST"),
    week_pos = c("QB", "RB", "WR", "TE", "K", "DST"),
    id_col = "fantasydata_id",
    table_css = "table",
    extract_pid = function(p_node)html_attr(p_node, "playerid"),
    stat_cols = c(
      pass_comp = "PassingCompletions", pass_att = "PassingAttempts",
      pass_comp_pct = "PassingCompletionPercentage", pass_yds = "PassingYards",
      pass_avg = "PassingYardsPerAttempt", pass_tds = "PassingTouchdowns",
      pass_int = "PassingInterceptions", pass_rate = "PassingRating",
      rush_att = "RushingAttempts", rush_yds = "RushingYards",
      rush_avg = "RushingYardsPerAttempt", rush_tds = "RushingTouchdowns",
      rec_tgt = "ReceivingTargets", rec = "Receptions", rec_pct = "ReceptionPercentage",
      rec_yds = "ReceivingYards", rec_tds = "ReceivingTouchdowns", rec_long = "ReceivingLong",
      rec_yds_tgt = "ReceivingYardsPerTarget", rec_avg =  "ReceivingYardsPerReception",
      fumbles_total = "Fumbles", fumbles_lost = "FumblesLost",
      fg = "FieldGoalsMade", fg_att = "FieldGoalsAttempted", fg_pct = "FieldGoalPercentage",
      fg_long = "FieldGoalsLongestMade", xp = "ExtraPointsMade", xp_att = "ExtraPointsAttempted",
      dst_tfl = "TacklesForLoss", dst_sacks = "Sacks", dst_qb_hits = "QuarterbackHits",
      dst_int= "Interceptions", dst_fum_rec = "FumblesRecovered", dst_safety = "Safeties",
      dst_def_td = "DefensiveTouchdowns", dst_st_td = "SpecialTeamsTouchdowns",
      dst_pts_allowed = "PointsAllowed", site_pts = "Points")
  ),
  #### FantasyPros ####
  FantasyPros = list(
    base = "https://www.fantasypros.com/nfl/projections/",
    get_path = function(season, week, position)paste0(tolower(position), ".php"),
    get_query = function(season, week, pos_id, ...){
      if(week == 0)
        return(list(week = "draft"))
    },
    min_week = 0,
    max_week = 21,
    season_pos = c("QB", "RB", "WR", "TE", "K", "DST"),
    week_pos = c("QB", "RB", "WR", "TE", "K", "DST"),
    id_col = "fantasypro_id",
    table_css = "#data",
    pid_css = "a.player-name",
    extract_pid = function(p_node){p_node %>% html_attr("href") %>% basename() %>% str_replace("\\.php.*$", "")},
    rm_elem = list("tr.ad-row"),
    split_cols = list(
      list(
        col = function(p)list(p != "DST" ~ "Player"),
        into = function(p)list(p != "DST" ~ c("player", "team")),
        regex = function(p)list(p != "DST" ~ "(.+)\\s([A-Z]{2,3}$)")
      )
    ),
    stat_cols = c(
      pass_att = "PASSING ATT", pass_comp = "PASSING CMP", pass_yds = "PASSING YDS",
      pass_tds = "PASSING TDS", pass_int = "PASSING INTS", rush_att = "RUSHING ATT",
      rush_yds = "RUSHING YDS", rush_tds = "RUSHING TDS", rec = "RECEIVING REC",
      rec_yds = "RECEIVING YDS", rec_tds = "RECEIVING TDS", fg_att = "FGA",
      xp = "XPT", fumbles_lost = "MISC FL", dst_sacks = "SACK", dst_int = "INT",
      dst_fum_rec = "FR", dst_fum_force = "FF", dst_td = "TD", dst_safety = "SAFETY",
      dst_pts_allowed = "PA", dst_yds_allowed = "YDS AGN", site_pts = "FPTS",
      site_pts = "MISC FPTS")
  ),
  #### FantasySharks ####
  FantasySharks = list(
    base = "https://www.fantasysharks.com/apps/bert/forecasts/projections.php",
    get_query = function(season, week, pos_id, ...){
      query <- list()
      shark_segment <- function(season, week){
        shark_season <- c("2017"= 586, "2018" = 618, "2019" = 650, "2020" = 682)
        segment <- shark_season[as.character(season)] + week + 9 * (week > 0)
        return(segment)
      }
      query[c("League", "scoring", "uid")] <- c(-1, 1, 4)
      query$Segment <- shark_segment(season, week)
      query$Position <- pos_id
      return(query)
    },
    url_positions = function(p)switch(p, QB = 1, RB =2 , WR = 4, TE = 5, K = 7,
                                      DST = 6, DL = 8, LB = 9, DB = 10),
    min_week = 0,
    max_week = 21,
    season_pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
    week_pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
    id_col = "id",
    table_css = "#toolData",
    pid_css = "td.playerLink a",
    rm_elem = list("tr.separator",
                   "#toolData tr[valign ='middle']:not(:first-child)",
                   "#toolData tr[height ='20px']"),
    extract_pid = function(p_node){
      p_node %>% html_attr("href") %>%
        map(parse_url) %>% map_chr(`[[`, c("query", "id")) %>%
        `names<-`(NULL) %>% str_pad(4, "left", "0")
    },
    stat_cols = c(
      pass_att = "Att", pass_comp = "Comp", pass_yds = "Pass Yds", pass_tds = "Pass TDs",
      pass_09_tds = "0-9 Pass TDs", pass_1019_tds = "10-19 Pass TDs",
      pass_2029_tds = "20-29 Pass TDs", pass_3039_tds = "30-39 Pass TDs",
      pass_4049_tds = "40-49 Pass TDs", pass_50_tds = "50+ Pass TDs", pass_int = "Int",
      sacks = "Sck", pass_250_yds = ">= 250 yd", pass_300_yds = ">= 300 yd",
      pass_350_yds = ">= 350 yd", pass_400_yds =  ">= 400 yd", rush_att = "Rush",
      rush_yds = "Rush Yds", rush_tds = "Rush TDs", rush_09_tds = "0-9 Rsh TDs",
      rush_1019_tds = "10-19 Rsh TDs", rush_2029_tds = "20-29 Rsh TDs",
      rush_3039_tds = "30-39 Rsh TDs", rush_4049_tds = "40-49 Rsh TDs",
      rush_50_tds = "50+ Rsh TDs", rxx_50_yds = ">= 50 yd", rxx_100_yds = ">= 100 yd",
      rxx_150_yds = ">= 150 yd", rxx_200_yds = ">= 200 yd", rec_tgt = "Tgt",
      rec_rz_tgt = "RZTgt", rec = "Rec", rec_yds = "Rec Yds", rec_tds = "Rec TDs",
      rec_09_tds = "0-9 Rec TDs", rec_1019_tds = "10-19 Rec TDs",
      rec_2029_tds = "20-29 Rec TDs", rec_3039_tds = "30-39 Rec TDs",
      rec_4049_tds = "40-49 Rec TDs", rec_50_tds = "50+ Rec TDs",
      rec_50_yds = ">= 50 yd1", rec_100_yds = ">= 100 yd1", punt_ret_yds = "Punt Ret Yds",
      kick_ret_yds = "Kick Ret Yds",fumbles_lost = "Fum",  xp = "XPM", xp_att = "XPA",
      fg = "FGM", fg_att = "FGA", fg_0019 = "10-19 FGM", fg_2029 = "20-29 FGM",
      fg_3039 = "30-39 FGM", fg_4049 = "40-49 FGM", fg_50 = "50+ FGM", fg_miss = "Miss",
      dst_yds_allowed = "Yds Allowed", dst_yds_99 = "0-99", dst_yds_199 = "100-199",
      dst_yds_299 = "200-299", dst_yds_349 = "300-349", dst_yds_399 = "350-399",
      dst_yds_449 = "400-449", dst_yds_499 = "450-499", dst_yds_549 = "500-549",
      dst_yds_550 = "550+", dst_pts_allowed = "Pts Agn", dst_pts_0 = "0",
      dst_pts_6 = "1-6", dst_pts_13 = "7-13", dst_pts_17 = "14-17", dst_pts_20 = "18-20",
      dst_pts_27 = "21-27", dst_pts_34 = "28-34", dst_pts_45 = "35-45", dst_pts_46 = "46+",
      dst_sacks = "Scks", dst_int = "Int", dst_fum_rec = "Fum", dst_td = "DefTD",
      dst_safety = "Safts", idp_solo = "Tack", idp_asst = "Asst", idp_sack = "Scks",
      idp_pd = "PassDef", idp_int = "Int", idp_fum_force = "FumFrc", idp_fum_rec = "Fum",
      idp_tds = "DefTD", site_pts = "Pts"
    )
  ),
  #### FFToday ####
  FFToday = list(
    base = "http://www.fftoday.com/rankings/",
    get_path = function(season, week, position){
      fft_file <- ifelse(is.null(week) || week == 0, "proj",  "wkproj")
      paste0("player", fft_file, ".php")
    },
    get_query = function(season, week, pos_id, ...){
      query <- list(Season = season)
      if(!is.null(week) && week > 0)
        query$GameWeek <- week + ifelse(week > 17, 3, 0)
      query[c("LeagueID", "PosID", "cur_page")]  <- c(1, pos_id, 0)
      return(query)
    },
    url_positions = function(p)switch(p, QB = 10, RB = 20, WR = 30, TE = 40,
                                      K= 80, DST = 99, DL = 50, LB = 60, DB =70),
    min_week = 0,
    max_week = 21,
    season_pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
    week_pos = c("QB", "RB", "WR", "TE", "K"),
    id_col = "fftoday_id",
    table_css = "table",
    pid_css = "a[href *='stats/players/']",
    index = 11,
    extract_pid = function(p_node){
      p_node %>% html_attr("href") %>% str_extract("[0-9]{2,8}")},
    stat_cols = c(
      pass_comp = "Passing Comp", pass_att = "Passing Att", pass_yds = "Passing Yard",
      pass_tds = "Passing TD", pass_int = "Passing INT", rush_att = "Rushing Att",
      rush_yds = "Rushing Yard" , rush_tds = "Rushing TD", rec= "Receiving Rec",
      rec_yds = "Receiving Yard", rec_tds = "Receiving TD", fg = "FGM", fg_att = "FGA",
      fg_pct = "FG", xp = "EPM", xp_att = "EPA", fg = "FG Made", fg_miss = "FG Miss",
      xp = "XP Made", xp_miss = "XP Miss", dst_sacks = "Sack", dst_fum_rec = "FR",
      dst_int = "INT", dst_td = "DefTD", dst_pts_allowed = "PA", dst_pass_yds_g = "PaYd/G",
      dst_rush_yds_g = "RuYd/G", dst_safety = "Safety", dst_ret_tds = "KickTD",
      idp_solo = "Tackle", idp_asst = "Assist", idp_sack = "Sack", idp_pd = "PD",
      idp_int = "INT", idp_fum_force = "FF", idp_fum_rec = "FR", site_pts = "Fantasy FFPts",
      site_pts = "Fantasy FPts"
    )
  ),
  #### FleaFlicker ####
  FleaFlicker = list(
    base = "https://www.fleaflicker.com/nfl/leaders",
    get_query = function(season, week, pos_id, ...){
      sort_mode <- ifelse(week < 18, 1, 7)
      query <- list(statType = 7, sortMode = sort_mode)

      query$position <- pos_id
      query$tableOffset <- 0
      return(query)
    },
    url_positions = function(p)switch(p, "QB" = 4, "RB" = 1, "WR" = 2, "TE" = 8,
                                      "Flex" = 11, "K" = 16, "DST" = 256, "CB" = 512,
                                      "S" = 1024, "DE" = 2048, "DT" = 4096,
                                      "LB" = 128, "IDP" = 7808),
    min_week = 1,
    max_week = 21,
    week_pos = c("QB", "RB", "WR", "TE", "K", "DST", "CB", "S", "DE", "DT", "LB", "IDP"),
    id_col = "fleaflicker_id",
    table_css = "#body-center-main table",
    pid_css = "a.player-text",
    rm_elem = list("tr.table-pagination"),
    split_cols = list(
      list(
        col = function(p)list(TRUE ~ "Player Name"),
        into = function(p)list(TRUE ~ c("player", "pos", "team", "bye")),
        regex = function(p)list(TRUE ~ paste0("([A-Za-z0-9'-.\\s]+)\\s", "([A-Z/]{2,3})\\s",
                                              "([A-Z]{2,3})\\s*\\(*([0-9]*)\\)"))
      )
    ),
    extract_pid = function(p_node){
      p_node %>% html_attr("href") %>% str_extract("[0-9]{2,8}")},
    stat_cols = c(
      pass_comp = "Passing Cmp", pass_att = "Passing Att", pass_comp_pct = "Passing",
      pass_yds = "Passing Yd", pass_tds = "Passing TD", pass_int = "Passing INT",
      pass_rate = "Passing Rat", rush_att = "Rushing Att", rush_yds = "Rushing Yd",
      rush_tds = "Rushing TD", rec = "Receiving Rec", rec_tgt = "Receiving Tar",
      rec_yds = "Receiving Yd", rec_tds = "Receiving TD", fumbles_lost = "Misc Fum",
      fg = "Kicking FG" , fg_att = "Kicking Att",  xp = "Kicking XP",
      xp_att = "Kicking Att1", dst_int = "Defense INT", dst_sacks = "Defense Sack",
      dst_fum_force = "Defense FF", dst_fum_rec = "Defense FR",  dst_td = "Defense TD",
      dst_pts_allowed = "Defense Pts", dst_yds_allowed = "Defense Yd",
      dst_pts_allowed = "Team Defense Pts", dst_yds_allowed = "Team Defense Yd",
      idp_asst = "Defense Ast", idp_solo = "Defense Solo", idp_int = "Defense INT",
      idp_sack = "Defense Sack", idp_fum_force = "Defense FF",
      idp_fum_rec = "Defense FR", idp_td = "Defense TD", site_pts = "Fantasy FPts"
    )
  ),
  #### NumberFire ####
  NumberFire = list(
    base = "https://www.numberfire.com/nfl/fantasy/",
    get_path = function(season, week, position){
      if(is.null(week) || week == 0){
        nmf_path <-  "remaining-projections"
      } else{
        nmf_path <- "fantasy-football-projections"
      }

      if(position != "Off")
        nmf_path <- paste(nmf_path, position, sep = "/")

      return(nmf_path)
    },
    url_positions = function(p)switch(p, "Off" = "", "RB/WR" = "rbwr","DST" = "d",
                                      tolower(p)),
    min_week = 0,
    max_week = 21,
    season_pos = c("Off", "QB", "RB", "WR", "TE", "RB/WR", "K", "DST", "IDP"),
    week_pos = c("Off", "QB", "RB", "WR", "TE", "RB/WR", "K", "DST", "IDP"),
    id_col = "numfire_id",
    table_css = "table.projection-table",
    pid_css = "td[class='player'] a",
    index = 1:2,
    extract_pid = function(p_node){
      p_node %>% html_attr("href") %>% basename()},

    split_cols = list(
      list(
        col = function(p)list(TRUE ~ "Player"),
        into = function(p)list(TRUE ~ c("player", "abbr name", "pos", "team")),
        regex = function(p)list(TRUE ~ "([A-Za-z ,.'-/]+)\\n *([A-Za-z ,.'-/]+)\\n *\\(([A-Z]+), ([A-Z]+)\\)")
      ),
      list(
        col = function(p)list(TRUE ~ "Passing C/A"),
        into = function(p)list(TRUE ~ c("pass_comp", "pass_att")),
        sep = function(p)list(TRUE ~ "/")
      ),
      list(
        col = function(p)list(TRUE ~ "numberFire CI"),
        into = function(p)list(TRUE ~ c("site_ci_low", "site_ci_high")),
        regex = function(p)list(TRUE ~ "(\\-*[0-9\\.]+)\\-(\\-*[0-9\\.]+)")
      )
    ),
    stat_cols = c(
      pass_yds = "Passing Yds", pass_tds = "Passing TDs", pass_int = "Passing Ints",
      rush_att = "Rushing Att", rush_yds = "Rushing Yds", rush_tds = "Rushing TDs",
      rec = "Receiving Rec", rec_yds = "Receiving Yds", rec_tds = "Receiving TDs",
      xp = "Kicking XPM", fg_att = "Kicking FGA", fg = "Kicking FGM",
      fg_0019 = "FG Made By Distance 0-19", fg_2029 = "FG Made By Distance 20-29",
      fg_3039 = "FG Made By Distance 30-39", fg_4049 = "FG Made By Distance 40-49",
      fg_50 = "FG Made By Distance 50", dst_pts_allowed = "Defense Points Allowed",
      dst_yds_allowed = "Defense Yards Allowed" , dst_sacks = "Defense Sacks",
      dst_int = "Defense INTs", dst_fum_rec = "Defense Fumbles", dst_td = "Defense TDs",
      idp_solo = "Defense Tackles", idp_sack = "Defense Sacks", idp_int = "Defense INTs",
      idp_tds = "Defense TDs", idp_pd = "Defense Passes Defended",
      idp_fum_rec = "Defense Fum Rec", site_pts = "numberFire FP"
    )
  ),
  #### Yahoo ####
  Yahoo = list(
    base = "https://football.fantasysports.yahoo.com/f1/",
    league_id = "47241",

    get_query = function(season, week, pos_id, ...){

      stat_type <- list(...)[["stat_type"]]
      if(is.null(stat_type))
        stat_type <- "Projected"

      yahoo_qry <- list(sort = "PTS", sdir = "1", status = "A", pos = pos_id,
                        stat1 = "", jsenabled = 1, count = 0)

      yahoo_qry$stat1 <- switch(
        tolower(stat_type),
        "projected" = ifelse(week > 0, paste0("S_PW_", week) ,
                             paste0("S_PS_", season)),
        "actual" = ifelse(week > 0, paste0("S_W_", week) ,
                          paste0("S_S_", season)),
        "remaining season" = paste0("S_PSR_", season),
        "next 4 weeks" = "S_PN4W",
        "Llst 4 weeks" = "S_L4W",
        "avg last 4 weeks" = "S_AL4W"
      )

      return(yahoo_qry)
    },
    url_positions = function(p)switch(p, "DST" = "DEF", p),
    min_week = 0,
    max_week = 17,
    season_pos = c("O", "DP", "QB", "RB", "WR", "TE", "K", "DST", "D", "DB", "DL",
                   "LB", "DT", "DE", "CB", "S"),
    week_pos = c("O", "DP", "QB", "RB", "WR", "TE", "K", "DST", "D", "DB", "DL",
                 "LB", "DT", "DE", "CB", "S"),
    id_col = "stats_id",
    table_css = "table[class *='Table-interactive']",
    pid_css = "a[href *= 'nfl/players']:not(a[class *='playernote']), a[href *= 'nfl/teams']:not(a[class *='playernote'])",
    split_cols = list(
      list(
        col = function(p)list(
          p == "K" ~ "Kickers", p == "DST" ~ "Defense/Special Teams",
          p %in% c("O", "QB", "RB", "WR", "TE") ~ "Offense",
          p %in% c("DP", "D", "DB", "DL", "LB", "DT", "DE", "CB", "S") ~ "Defensive Players"
        ),
        into = function(p)list(TRUE ~ c("Note", "player", "team", "pos", "Status/Game/Opp")),
        regex = function(p)list(TRUE ~ "\\s*(.+Note[s]*)\\s+(.+)\\s([[:alpha:]]{2,3})\\s\\-\\s([[:alpha:]]{1,3},*[[:alpha:]]*)\\s{2,}(.+)")
      )
    ),
    extract_pid = function(p_node){
      p_node %>% html_attr("href") %>% basename()},
    recode_cols = list(
      name = list("src_id"),
      recode_vals = list(c("jac" = "30", "bal" = "33", "lar" = "14", "phi" = "21", "det" = "8",
                           "lac" = "24", "nor" = "18", "sea" = "26", "chi" = "3",  "car" = "29",
                           "pit" = "23", "nwe" = "17",  "kan" = "12", "min" = "16", "dal" = "6",
                           "was" = "28", "den" = "7", "ari" = "22", "ten" = "10", "tam" = "27",
                           "buf" = "2", "cin" = "4", "atl" = "1", "gnb" = "9", "mia" = "15",
                           "ind" = "11", "nyg" = "19",  "hou" = "34", "sfo" = "25", "cle" = "5",
                           "nyj" = "20", "oak" = "13"))
    ),
    stat_cols = c(
      games = "GP", pass_att = "Passing Att", pass_comp = "Passing Comp",
      pass_inc = "Passing Inc", pass_yds = "Passing Yds", pass_tds = "Passing TD",
      pass_1st = "Passing 1st Downs", pass_int = "Passing Int", sacks = "Passing Sack",
      pass_40_yds = "Passing 40 Yd Cmp", pass_40_tds = "Passing 40 Yd TD",
      rush_att = "Rushing Att", rush_yds = "Rushing Yds", rush_tds = "Rushing TD",
      rush_1st = "Rushing 1st Downs", rush_40_yds = "Rushing 40 Yd Att",
      rush_40_tds = "Rushing 40 YdTD", rec_tgt = "Receiving Tgt", rec = "Receiving Rec",
      rec_yds = "Receiving Yds", rec_tds = "Receiving TD", rec_1st = "Receiving 1st Downs",
      rec_40_yds = "Receiving 40 Yd Rec", rec_40_tds = "Receiving 40 Yd TD",
      ret_yds = "Return Yds", ret_tds = "Return TD", two_pts = "Misc 2PT",
      fumbles_total = "Fumbles Tot", fumbles_lost = "Fumbles Lost",
      site_pts = "Fantasy Fan Pts", fg_0019 = "Field Goals Made 0-19",
      fg_2029 = "Field Goals Made 20-29", fg_3039 = "Field Goals Made 30-39",
      fg_4049 = "Field Goals Made 40-49", fg_50 = "Field Goals Made 50",
      fg_miss_0019 = "Field Goals Missed 0-19", fg_miss_2029 = "Field Goals Missed 20-29",
      fg_miss_3039 = "Field Goals Missed 30-39", fg_miss_4049 = "Field Goals Missed 40-49",
      fg_miss_50 = "Field Goals Missed 50", xp = "PAT Made", xp_miss = "PAT Miss",
      dst_pts_allowed = "Pts vs.",  dst_sacks = "Tackles Sack", dst_safety = "Tackles Safe",
      dst_tfl = "Tackles TFL", dst_int = "Turnovers Int", dst_fum_rec = "Turnovers Fum Rec",
      dst_td = "TD TD",  dst_blk = "Miscellaneous Blk Kick",
      dst_4_down = "Miscellaneous 4 Dwn Stops", dst_yds_allowed = "Miscellaneous Yds Allow",
      dst_3_Out = "Miscellaneous 3 And Outs", dst_ret_yds = "Return Yds",
      dst_ret_tds = "Return TD", idp_ret_yds = "Return Yds", idp_ret_tds = "Return TD",
      idp_solo = "Tackles Tack Solo", idp_asst = "Tackles Tack Ast",
      idp_tfl = "Tackles TFL",  idp_sack = "Tackles Sack", idp_safety = "Tackles Safe",
      idp_pd = "Misc Pass Def", idp_blk = "Misc Blk Kick", idp_int = "Turnovers Int",
      idp_fum_force = "Turnovers Fum Force", idp_fum_rec = "Turnovers Fum Rec",
      idp_ret_yds = "Turnovers Ret Yds", idp_td = "TD TD"
    )
  )
)


json_sites <- list(
  #### FantasyFootballNerd ####
  FantasyFootballNerd = list(
    base = "http://www.fantasyfootballnerd.com/service/",
    api_key = "test",
    get_path = function(season, week, position){
      data_type <- ifelse(week == 0, "draft", "weekly")
      week_no <- ifelse(week == 0, "", as.character(week))
      sprintf("%s-projections/json/%s/%s/%s", data_type, "{api_key}", position, week_no)
    },
    url_positions = function(p)switch(p, "DST" = "DEF", p),
    min_week = 0,
    max_week = 17,
    season_pos = c("QB", "RB", "WR", "TE", "K", "DST"),
    week_pos = c("QB", "RB", "WR", "TE", "K", "DST"),
    id_col = "fantasynerd_id",
    json_elem = list(weekly = "Projections", season = "DraftProjections"),
    stat_elem = NULL,
    player_elem = NULL,
    stat_cols = c(pass_att = "passAtt", pass_comp = "passCmp", pass_yds = "passYds",
                  pass_tds = "passTD", pass_int = "passInt", rush_att = "rushAtt",
                  rush_yds = "rushYds", rush_tds = "rushTD", fumbles_lost = "fumblesLost",
                  rec = "receptions", rec = "rec", rec_yds = "recYds", rec_tds = "recTD", fg_att = "fgAtt",
                  dst_int = "defInt", dst_fum_rec = "defFR", dst_fum_force = "defFF",
                  dst_sacks = "defSack" , dst_td = "defTD", dst_ret_tds = "defRetTD",
                  dst_safety = "defSafety", dst_pts_allowed = "defPA",
                  dst_yds_allowed = "defYdsAllowed", xp = "xp", fg = "fg",
                  pass_comp = "completions", pass_att = "attempts",
                  pass_yds = "passingYards", pass_tds = "passingTD", pass_int = "passingInt",
                  rush_yds = "rushYards", site_pts = "fantasyPoints", fumbles_lost = "fumbles",
                  rec_yds = "recYards", dst_sacks = "sacks", dst_int = "interceptions",
                  dst_fum_rec = "fumbleRec", dst_td = "TD", dst_st_td = "specialTeamTD"
                  ),
    player_cols = c(src_id = "playerid", player = "display_name",  pos = "position")


  ),
  #### NFL ####
  NFL = list(
    base = "http://api.fantasy.nfl.com/v1/players/stats",

    get_query = function(season, week, pos_id, ...){
      if(is.null(week) || week == 0){
        nfl_type <- "seasonProjectedStats"
      } else {
        nfl_type <- "weekProjectedStats"
      }

      nfl_qry <- list(statType = nfl_type)

      if(!is.null(season))
        nfl_qry$season <- season

      if(!is.null(week) && week != 0){
        week <- as.character(week)
        week <- match.arg(week, choices = 1:16)
        nfl_qry$week <- week
      }

      if(!is.null(pos_id))
        nfl_qry$position <- pos_id

      nfl_qry$format <- "json"
      return(nfl_qry)
    },
    url_positions = function(p)switch(p, "DST" = "DEF", p),
    min_week = 0,
    max_week = 16,
    season_pos = c("QB", "RB", "WR", "TE", "K" , "DST", "DL", "LB", "DB"),
    week_pos = c("QB", "RB", "WR", "TE", "K" , "DST", "DL", "LB", "DB"),
    id_col = "nfl_id",
    json_elem = list(weekly = "players", season = "players"),
    stat_elem = "stats",
    player_elem = NULL,
    stat_cols = c(
      pass_yds = "Pass Yds", rush_yds = "Rush Yds", pass_tds = "Pass TD",
      pass_int = "Pass Int", rush_tds = "Rush TD", fumbles_lost =  "Fum Lost",
      two_pts = "2PT", games = "GP", rec = "Receptions", rec_yds ="Rec Yds",
      rec_tds = "Rec TD", ret_tds = "Return TD", idp_solo = "Tack",  idp_asst = "Ast",
      xp = "PAT Made", fg_0019 = "FG 0-19", fg_2029 = "FG 20-29", fg_3039 = "FG 30-39",
      fg_4049 = "FG 40-49", fg_50 = "FG Miss 50+", dst_sacks = "Sack", dst_int = "Int",
      dst_fum_Rec = "Fum Rec", dst_td = "TD", dst_pts_allowed = "Pts Allow", dst_ret_td = "Return TD",
      dst_safety = "Saf", dst_blk = "Block" , idp_sack = "Sack", idp_fum_force = "Frc Fum",
      idp_pd = "Pass Def", idp_blk = "Blk", idp_fum_rec = "Fum Rec", idp_int = "Int",
      idp_safety = "Saf", site_season_pts = "seasonProjectedPts", site_pts = "weekProjectedPts"
    ),
    player_cols = c(src_id = "id", player = "name", team = "teamabbr", pos = "position")


  ),
  #### RTSports ####
  RTSports = list(
    base = "https://www.freedraftguide.com/football/draft-guide-rankings-provider.php",

    get_query = function(season, week, pos_id, ...){
      return(list(POS = pos_id))
    },

    url_positions = function(p)switch(p, QB = 0, RB = 1, WR = 2, TE = 3, K = 4, DST = 5),
    min_week = 0,
    max_week = 0,
    season_pos = c("QB", "RB", "WR", "TE", "K" , "DST"),
    id_col = "rts_id",
    json_elem = list(weekly = "player_list", season = "player_list"),
    stat_elem = "stats",
    player_elem = c("player_id", "name", "position", "nfl_team"),
    stat_cols = c(
      pass_comp = "pass_comps", pass_yds = "pass_yds", pass_tds = "pass_tds",
      pass_int = "pass_ints", rush_att = "rush_atts", rush_yds = "rush_yds",
      rush_tds = "rush_tds",  rec = "rec_rcpts", rec_yds = "rec_yds",
      rec_tds = "rec_tds", xp = "kick_patm", fg = "kick_fgm", fg_att = "kick_fga",
      dst_pts_allowed = "pts_allowed", dst_yds_allowed = "yds_allowed",
      dst_sacks = "sacks", dst_int = "ints", dst_fum_rec = "fumbles_recovered",
      site_pts = "pts_ppr"
    ),
    player_cols = c(src_id = "player_id", player = "name", team = "nfl_team")
  )

)


xlsx_sites <- list(
  Walterfootball = list(
    base =  "http://walterfootball.com/",
    get_path = function(season, week, position)sprintf("fantasy%srankingsexcel.xlsx", as.character(season)),
    url_positions =function(p)switch(p, "QB" = "QBs", "RB" = "RBs", "WR" = "WRs", "TE" = "TEs", "K" = "Ks"),
    min_week = 0,
    max_week = 0,
    season_pos = c("QB", "RB", "WR", "TE", "K"),
    stat_cols = c(
      pass_yds = "PASS YDS", pass_tds = "PASS TD", pass_int = "INT",
      rush_yds = "RUSH YDS" , reg_tds = "REG TD", fg_0039 = "FG 1-39",
      fg_4049 = "FG 40-49", fg_50 = "FG 50+", xp = "XP"
    )
  )
)
projection_sources <- append(append(
  purrr::map(html_sites, do.call, what = ffanalytics:::html_source$new),
  purrr::map(json_sites, do.call, what = ffanalytics:::json_source$new)),
  purrr::map(xlsx_sites, do.call, what = ffanalytics:::xlsx_source$new)
)


usethis::use_data(projection_sources, overwrite = TRUE)

