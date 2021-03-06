
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
  "Turnover Fum Rec" = "dst_fum_Rec",
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

# ESPN ----





