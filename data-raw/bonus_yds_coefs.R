nflfastr_calc_cols = c(
  "player_id" = "nflfastr_id",
  "recent_team" = "team",
  "completions" = "pass_comp",
  "attempts" = "pass_att",
  "passing_yards" = "pass_yds",
  "passing_tds" = "pass_tds",
  "interceptions" = "pass_int",
  "sack_fumbles",
  "sack_fumbles_lost",
  "passing_air_yards",
  "passing_yards_after_catch",
  "passing_first_downs",
  "passing_epa",
  "passing_2pt_conversions",
  "pacr",
  "dakota",
  "carries" = "rush_att",
  "rushing_yards" = "rush_yds",
  "rushing_tds" = "rush_tds",
  "rushing_fumbles",
  "rushing_fumbles_lost",
  "rushing_first_downs",
  "rushing_epa",
  "rushing_2pt_conversions",
  "receptions" = "rec",
  "targets" = "rec_tgt",
  "receiving_yards" = "rec_yds",
  "receiving_tds" = "rec_tds",
  "receiving_fumbles",
  "receiving_fumbles_lost",
  "receiving_air_yards",
  "receiving_yards_after_catch",
  "receiving_first_downs",
  "receiving_epa",
  "receiving_2pt_conversions",
  "racr",
  "target_share",
  "air_yards_share",
  "wopr",
  "special_teams_tds",
  "fantasy_points",
  "fantasy_points_ppr"
)


# TODO break into season and weekly ofr the _40 cols
# Imperfect but gets a season-long estimate for projections
base_df = nflfastR::load_pbp(get_scrape_year() - 1) %>%
  dplyr::filter(week <= 18)

df_calc = base_df %>%
  nflfastR::calculate_player_stats(weekly = TRUE)
names(df_calc) = rename_vec(names(df_calc), nflfastr_calc_cols)

# Passing ----
# n_300 ~ season_total
df_pass = df_calc %>%
  dplyr::group_by(nflfastr_id) %>%
  dplyr::filter(sum(pass_yds, na.rm = TRUE) > 400) %>%
  dplyr::summarise(games = n(),
                   n_300 = sum(between(pass_yds, 300, 349)),
                   n_350 = sum(between(pass_yds, 350, 399)),
                   n_400 = sum(pass_yds >= 400),
                   pass_yds = sum(pass_yds, na.rm = TRUE))

coefs_pass_300_yds = coef(lm(n_300 ~ pass_yds, data = df_pass))
coefs_pass_350_yds = coef(lm(n_350 ~ pass_yds, data = df_pass))
coefs_pass_400_yds = coef(lm(n_400 ~ pass_yds, data = df_pass))

coefs_pass_40_yds = base_df %>%
  dplyr::select(passer, passer_id, game_id, passing_yards, pass_touchdown) %>%
  dplyr::filter(!is.na(passing_yards)) %>%
  dplyr::group_by(game_id, passer_id) %>%
  dplyr::summarise(pass_yds = sum(passing_yards),
                   pass_tds = sum(pass_touchdown),
                   n_pass_gte40 = sum(passing_yards >= 40)) %>%
  dplyr::filter(pass_yds > 100) %>%
  lm(n_pass_gte40 ~ pass_yds, data = .) %>%
  coef()

# Rushing ----
scoring$rush

df_rush = df_calc %>%
  dplyr::group_by(nflfastr_id) %>%
  dplyr::filter(sum(rush_yds, na.rm = TRUE) > 200) %>%
  dplyr::summarise(games = n(),
                   n_100 = sum(between(rush_yds, 100, 149)),
                   n_150 = sum(between(rush_yds, 150, 199)),
                   n_200 = sum(rush_yds >= 200),
                   rush_yds = sum(rush_yds, na.rm = TRUE))

coefs_rush_100_yds = coef(lm(n_100 ~ rush_yds, data = df_rush))
coefs_rush_150_yds = coef(lm(n_150 ~ rush_yds, data = df_rush))
coefs_rush_200_yds = coef(lm(n_200 ~ rush_yds, data = df_rush))

coefs_rush_40_yds = base_df %>%
  dplyr::select(rusher, rusher_id, game_id, rushing_yards, rush_touchdown) %>%
  dplyr::filter(!is.na(rushing_yards)) %>%
  dplyr::group_by(game_id, rusher_id) %>%
  dplyr::summarise(rush_yds = sum(rushing_yards),
                   rush_tds = sum(rush_touchdown),
                   n_rush_gte40 = sum(rushing_yards >= 40)) %>%
  dplyr::filter(rush_yds > 25) %>%
  lm(n_rush_gte40 ~ rush_yds, data = .) %>%
  coef()


# Recieving ----
df_rec = df_calc %>%
  dplyr::group_by(nflfastr_id) %>%
  dplyr::filter(sum(rec_yds, na.rm = TRUE) > 200) %>%
  dplyr::summarise(games = n(),
                   n_100 = sum(between(rec_yds, 100, 149)),
                   n_150 = sum(between(rec_yds, 150, 199)),
                   n_200 = sum(rec_yds >= 200),
                   rec_yds = sum(rec_yds, na.rm = TRUE))

coefs_rec_100_yds = coef(lm(n_100 ~ rec_yds, data = df_rec))
coefs_rec_150_yds = coef(lm(n_150 ~ rec_yds, data = df_rec))
coefs_rec_200_yds = coef(lm(n_200 ~ rec_yds, data = df_rec))

coefs_rec_40_yds = base_df %>%
  dplyr::select(receiver, receiver_id, game_id, receiving_yards) %>%
  dplyr::filter(!is.na(receiving_yards)) %>%
  dplyr::group_by(game_id, receiver_id) %>%
  dplyr::summarise(rec_yds = sum(receiving_yards),
                   n_rec_gte40 = sum(receiving_yards >= 40)) %>%
  dplyr::filter(rec_yds > 25) %>%
  lm(n_rec_gte40 ~ rec_yds, data = .) %>%
  coef()

bonus_col_coefs = mget(ls(pattern = "coefs_(pass|rush|rec)"))
names(bonus_col_coefs) = sub("coefs_", "", names(bonus_col_coefs))


bonus_col_sets = list(
  pass_300_yds = c("pass_300_yds", "pass_350_yds", "pass_400_yds"),
  pass_350_yds = c("pass_350_yds", "pass_400_yds"),
  rec_100_yds = c("rec_100_yds", "rec_150_yds", "rec_200_yds"),
  rec_150_yds = c("rec_150_yds", "rec_200_yds"),
  rush_100_yds = c("rush_100_yds", "rush_150_yds", "rush_200_yds"),
  rush_150_yds = c("rush_150_yds", "rush_200_yds")
)





usethis::use_data(bonus_col_sets, bonus_col_coefs,
                  pts_bracket_coefs, player_ids,
                  overwrite = TRUE, internal = TRUE)





