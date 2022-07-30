

# Imperfect approach but will work for now. Predicts the SD so we can do season
# long points bracket estimates
df1 = nflfastR::fast_scraper_schedules(get_scrape_year() - 1) %>%
  dplyr::select(game_id, away_team:home_score) %>%
  tidyr::pivot_longer(-game_id, names_to = c("var", ".value"), names_sep = "_") %>%
  dplyr::group_by(game_id) %>%
  dplyr::mutate(opp_score = rev(score),
                team = rename_vec(team, unlist(team_corrections))) %>%
  dplyr::filter(as.numeric(substr(game_id, 6, 7)) <= 17) %>%
  dplyr::group_by(team) %>%
  dplyr::summarise(n = n(),
                   season_mean = mean(opp_score),
                   season_sd = sd(opp_score))


model_coefs = nlme:::coef.lme(nlme::lme(season_sd ~ season_mean, random = ~ 1 | team, data = df1))
model_coefs$team = rownames(model_coefs)
model_coefs = model_coefs[(c(3, 1, 2))]
model_coefs[-1] = round(model_coefs[-1], 5)
rownames(model_coefs) = NULL
names(model_coefs) = gsub("\\(|\\)", "", names(model_coefs))


# Adding ids
model_coefs$id = get_mfl_id(pos = "DST", team = model_coefs$team)
model_coefs$nfl_id = player_ids$nfl_id[match(model_coefs$id, player_ids$id)]
pts_bracket_coefs = model_coefs[c("id", "nfl_id", "team", "Intercept", "season_mean")]

usethis::use_data(pts_bracket_coefs, player_ids, overwrite = TRUE, internal = TRUE)




