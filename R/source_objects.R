
# Note, experimental w/ source_scrapes.R. Do not use yet.

# Creating player_table object so it is no longer sent to the
# global environemnt upon loading
player_table = NULL


# CBS ----
cbs_columns = c(
  "Player" = "player",
  "Team" = "team",
  "Pass Attempts" = "pass_att",
  "Pass Completions" = "pass_comp",
  "Passing Yards" = "pass_yds",
  "Touchdowns Passes" = "pass_tds",
  "Interceptions Thrown" = "pass_int",
  "Passing CmpPct" = "pass_comp_pct",
  "Passing YAtt" = "pass_avg",
  "Rushing Attempts" = "rush_att",
  "Rushing Yards" = "rush_yds",
  "Average Yards Per Rush" = "rush_avg",
  "Rushing Touchdowns" = "rush_tds",
  "Receptions" = "rec",
  "Targets" = "rec_tgt",
  "Passer Rating" = "pass_rate",
  "Passing Yards Per Game" = "pass_yds_g",
  "Yards Per Game" = "rec_yds_g",
  "Games Played" = "games",
  "Receiving Yards" = "rec_yds",
  "Average Yards Per Reception" = "rec_avg",
  "Receiving Touchdowns" = "rec_tds",
  "Fumbles Lost" = "fumbles_lost",
  "Field Goals Made" = "fg",
  "Field Goal Attempts" = "fg_att",
  "Longest Field Goal" = "fg_long",
  "Field Goals 1-19 Yards" = "fg_0019",
  "Field Goals 20-29 Yards" = "fg_2029",
  "Field Goals 30-39 Yards" = "fg_3039",
  "Field Goals 40-49 Yards" = "fg_4049",
  "Field Goals 50+ Yards" = "fg_50",
  "Field Goals 1-19 Yard Attempts" = "fg_att_0019",
  "Field Goals 20-29 Yard Attempts" = "fg_att_2029",
  "Field Goals 30-39 Yard Attempts" = "fg_att_3039",
  "Field Goals 40-49 Yard Attempts" = "fg_att_4049",
  "Field Goals 50+ Yards Attempts" = "fg_att_50",
  "Extra Points Made" = "xp",
  "Extra Points Attempted" = "xp_att",
  "Interceptions" = "dst_int",
  "Tackles" = "dst_tackles",
  "Defensive Fumbles Recovered" = "dst_fum_rec",
  "Forced Fumbles" = "dst_fum_force",
  "Sacks" = "dst_sacks",
  "Defensive Touchdowns" = "dst_td",
  "Safeties" = "dst_safety",
  "Points Allowed" = "dst_pts_allowed",
  "Total Yards Allowed" = "dst_yds_allowed",
  "Points Allowed Per Game" = "dst_pts_allowed_g",
  "Rushing Yards Allowed" = "dst_rush_yds_allowed",
  "Net Passing Yards Allowed" = "dst_pass_yds_allowed",
  "Yards Against Per Game" = "dst_avg_yds_allowed",
  "Fantasy Points" = "site_pts",
  "Fantasy Points Per Game" = "site_fppg"
  )

# NFL ----
nfl_columns = c(
  "Player" = "player",
  "id" = "src_id",
  "name" = "player",
  "teamabbr" = "team",
  "Team" = "team",
  "position" = "pos",
  "Opp" = "opp",
  "Passing Yds" = "pass_yds",
  "Rushing Yds" = "rush_yds",
  "Passing TD" = "pass_tds",
  "Passing Int" = "pass_int",
  "Rushing TD" = "rush_tds",
  "Fum Lost" = "fumbles_lost",
  "Misc 2PT" = "two_pts",
  "Misc FumTD" = "fumble_TD",
  "GP" = "games",
  "Receiving Rec" = "rec",
  "Receiving Yds" = "rec_yds",
  "Receiving TD" = "rec_tds",
  "Ret TD" = "return_tds",
  "Tack" = "idp_solo",
  "Ast" = "idp_asst",
  "PAT Made" = "xp",
  "FG Made 0-19" = "fg_0019",
  "FG Made 20-29" = "fg_2029",
  "FG Made 30-39" = "fg_3039",
  "FG Made 40-49" = "fg_4049",
  "FG Made 50+" = "fg_50",
  "Tackles Sack" = "dst_sacks",
  "Turnover Int" = "dst_int",
  "Turnover Fum Rec" = "dst_fum_rec",
  "Score TD" = "dst_td",
  "Pts Allow" = "dst_pts_allowed",
  "Ret TD" = "dst_ret_tds",
  "Score Saf" = "dst_safety",
  "Score Def 2pt Ret" = "dst_2pt_ret",
  "Block" = "dst_blk" ,
  "Sack" = "idp_sack",
  "Frc Fum" = "idp_fum_force",
  "Pass Def" = "idp_pd",
  "Blk" = "idp_blk",
  "Fum Rec" = "idp_fum_rec",
  "Int" = "idp_int",
  "Saf" = "idp_safety",
  "Points Pts Allow" = "dst_pts_allowed",
  "Fantasy Points" = "site_pts",
  "seasonProjectedPts" = "site_season_pts",
  "weekProjectedPts" = "site_pts"
)
nfl_pos_idx = c("QB" = 1, "RB" = 2, "WR" = 3, "TE" = 4, "K" = 7, "DST" = 8)

# RT Sports ----

rts_pos_idx = c("QB" = 0, "RB" = 1, "WR" = 2, "TE" = 3, "K" = 4, "DST" = 5)

# Fantasy sharks ----

fantasysharks_columns = c(
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
  Pts = "site_pts", pos = "pos", data_src = "data_src"
)

numberfire_idp_columns = c(id = "id",  numfire_id = "src_id", Player = "player", position = "pos", team = "team", data_src = "data_src",
                       `Defense Tackles` = "idp_solo", `Defense Sacks` = "idp_sack", `Defense INTs` = "idp_int",
                       `Defense TDs` = "idp_tds", `Defense Passes Defended` = "idp_pd", `Defense Fum Rec` = "idp_fum_rec",
                       `Fandual FP` = "fandual_site_points", `FanDuel Salary` = "fanduel_salary", `FanDuel Value` = "fanduel_value",
                       `DraftKings FP` = "draftkings_site_points", `DraftKings Salary` = "draftkings_salary", `DraftKings Value` = "draftkings_value",
                       `Yahoo FP` = "yahoo_site_points", `Yahoo Salary` = "yahoo_salary", `Yahoo Value` = "yahoo_value")

numberfire_columns = c(id = "id", numfire_id = "src_id", Player = "player", position = "pos", team = "team", data_src = "data_src",
                       `numberFire FP` = "site_pts", Lower = "site_ci_low", Upper = "site_ci_high", `Ranks Ovr.` = "ranks_ovr", `Ranks Pos.` = "ranks_pos",
                       pass_comp = "pass_comp", pass_att = "pass_att", `Passing Yds` = "pass_yds", `Passing TDs` = "pass_tds", `Passing Ints` = "pass_ints",
                       `Rushing Att` = "rush_att", `Rushing Yds` = "rush_yds", `Rushing TDs` = "rush_tds",
                       `Receiving Rec` = "rec", `Receiving Yds` = "rec_yds", `Receiving TDs` = "rec_tds",
                       `Receiving Tgt` = "rec_tgt", `Kicking XPM` = "xp", `Kicking FGA` = "fg_att", `Kicking FGM` = "fg",
                       `FG Made By Distance 0-19` = "fg_0019", `FG Made By Distance 20-29` = "fg_2029",
                       `FG Made By Distance 30-39` = "fg_3039", `FG Made By Distance 40-49` = "fg_4049",
                       `FG Made By Distance 50+` = "fg_50", `Defense Points Allowed` = "dst_pts_allowed",
                       `Defense Yards Allowed` = "dst_yds_allowed", `Defense Sacks` = "dst_sacks",
                       `Defense INTs` = "dst_int", `Defense Fumbles` = "dst_fum_rec", `Defense TDs` = "dst_td",
                       `Opp Team` = "opp_team", `Opp Rank` = "opp_team_rank", `Fandual FP` = "fandual_site_points",
                       `FanDuel Salary` = "fanduel_salary", `FanDuel Value` = "fanduel_value",
                       `DraftKings FP` = "draftkings_site_points", `DraftKings Salary` = "draftkings_salary", `DraftKings Value` = "draftkings_value",
                       `Yahoo FP` = "yahoo_site_points", `Yahoo Salary` = "yahoo_salary", `Yahoo Value` = "yahoo_value")

walterfootball_columns = c(id = "id", Player = "player", Team = "tm", Bye = "bye", position = "pos", data_src = "data_src",
                           `PASS YDS` = "pass_yds", `PASS TD` = "pass_tds", INT = "pass_int",
                           `RUSH YDS` = "rush_yds", CATCH = "rec", `REC YDS` = "rec_yds", `REG TD` = "reg_tds",
                           `FG 1-39` = "fg_0039", `FG 40-49` = "fg_4049", `FG 50+` = "fg_50", XP = "xp",
                           `Points (ESPN Scoring)` = "site_pts")




