

# Starting with sleeper as a test

get_league_info_sleeper = function(league_id) {
  # league_id = "1048413202226241536"

  is_18_digits = grepl("[0-9]{18}", league_id)

  league_url = paste0("https://api.sleeper.app/v1/league/", league_id)

  resp_json = league_url %>%
    httr2::request() %>%
    httr2::req_user_agent("ffanalytics R package (https://github.com/FantasyFootballAnalytics/ffanalytics)") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(check_type = FALSE)

  roster = unlist(resp_json$roster_positions)


  list(
    league_name = resp_json$name,
    n_teams = resp_json$total_rosters,
    starters = roster[roster != "BN"],
    n_bench = sum(roster == "BN"),
    scoring_obj = resp_json$scoring_settings
  )

}

# names(resp_json$scoring_settings) |> sort()
# scoring_obj = get_league_info_sleeper(1048413202226241536)$scoring_obj

clean_scoring_sleeper = function(scoring_obj) {

  s_obj = ffanalytics:::scoring_empty

  s_obj = .
  s_obj$dst$dst_blk = scoring_obj$blk_kick
  s_obj$pass$pass_300_yds = scoring_obj$bonus_pass_yd_300
  s_obj$pass$pass_400_yds = scoring_obj$bonus_pass_yd_400

  if(scoring_obj$bonus_rec_te > 0) {
    s_obj$rec$all_pos = FALSE
    s_obj$rec$QB$rec = scoring_obj$rec
    s_obj$rec$RB$rec = scoring_obj$rec
    s_obj$rec$WR$rec = scoring_obj$rec
    s_obj$rec$TE$rec = scoring_obj$rec + scoring_obj$bonus_rec_te
  } else {
    s_obj$rec$all_pos = TRUE
    s_obj$rec$rec = scoring_obj$rec
  }
  s_obj$rec$rec_100_yds = scoring_obj$bonus_rec_yd_100
  s_obj$rec$rec_200_yds = scoring_obj$bonus_rec_yd_200
  s_obj$rush$rush_100_yds = scoring_obj$bonus_rush_yd_100
  s_obj$rush$rush_200_yds = scoring_obj$bonus_rush_yd_200

  s_obj$dst$dst_fum_rec = scoring_obj$def_st_fum_rec
  s_obj$dst$dst_td = max(c(scoring_obj$def_st_td, scoring_obj$def_td))
  s_obj$idp$idp_fum_force = scoring_obj$ff

  s_obj$kick$fg_0019 = -99



  ### not incorporated:
  # def_st_ff


}



get_sleeper_avatar_png = function(avatar_id) {
  # avatar_id = "bb76fbabc362c2c071adc071f5ef5213"
  avatar_url = paste0("https://sleepercdn.com/uploads/", avatar_id)

  temp_file = tempfile(fileext = ".png")

  download.file(avatar_url, temp_file)
  temp_file
}


