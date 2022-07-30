#' Default scoring rules for the calculations
#'
#' See \code{vignette("scoring_settings")} on how to define custom scoring settings.
#' @export
scoring <- list(
  pass = list(
    pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
    pass_int = -3, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
    pass_400_yds = 0
  ),
  rush = list(
    all_pos = TRUE,
    rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
    rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
  rec = list(
    all_pos = TRUE,
    rec = 0, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
    rec_150_yds = 0, rec_200_yds = 0
  ),
  misc = list(
    all_pos = TRUE,
    fumbles_lost = -3, fumbles_total = 0,
    sacks = 0, two_pts = 2
  ),
  kick = list(
    xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
    fg_50 = 5.0,  fg_miss = 0.0
  ),
  ret = list(
    all_pos = TRUE,
    return_tds = 6, return_yds = 0
  ),
  idp = list(
    all_pos = TRUE,
    idp_solo = 1, idp_asst = 0.5, idp_sack = 2, idp_int = 3,  idp_fum_force = 3,
    idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2
  ),
  dst = list(
    dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
    dst_blk = 1.5, dst_ret_yds = 0, dst_pts_allowed = 0
  ),
  pts_bracket = list(
    list(threshold = 0, points = 10),
    list(threshold = 6, points = 7),
    list(threshold = 20, points = 4),
    list(threshold = 34, points = 0),
    list(threshold = 99, points = -4)
  )
)


#' Object with potential scoring combinations based on the \code{scoring} object
#'
scoring_empty = list(pass = list(pass_att = NULL, pass_comp = NULL, pass_inc = NULL,
                                 pass_yds = NULL, pass_tds = NULL, pass_int = NULL, pass_40_yds = NULL,
                                 pass_300_yds = NULL, pass_350_yds = NULL, pass_400_yds = NULL,
                                 QB = list(pass_att = NULL, pass_comp = NULL, pass_inc = NULL,
                                           pass_yds = NULL, pass_tds = NULL, pass_int = NULL, pass_40_yds = NULL,
                                           pass_300_yds = NULL, pass_350_yds = NULL, pass_400_yds = NULL),
                                 RB = list(pass_att = NULL, pass_comp = NULL, pass_inc = NULL,
                                           pass_yds = NULL, pass_tds = NULL, pass_int = NULL, pass_40_yds = NULL,
                                           pass_300_yds = NULL, pass_350_yds = NULL, pass_400_yds = NULL),
                                 WR = list(pass_att = NULL, pass_comp = NULL, pass_inc = NULL,
                                           pass_yds = NULL, pass_tds = NULL, pass_int = NULL, pass_40_yds = NULL,
                                           pass_300_yds = NULL, pass_350_yds = NULL, pass_400_yds = NULL),
                                 TE = list(pass_att = NULL, pass_comp = NULL, pass_inc = NULL,
                                           pass_yds = NULL, pass_tds = NULL, pass_int = NULL, pass_40_yds = NULL,
                                           pass_300_yds = NULL, pass_350_yds = NULL, pass_400_yds = NULL)),
                     rush = list(all_pos = NULL, rush_yds = NULL, rush_att = NULL,
                                 rush_40_yds = NULL, rush_tds = NULL, rush_100_yds = NULL,
                                 rush_150_yds = NULL, rush_200_yds = NULL,
                                 QB = list(rush_yds = NULL, rush_att = NULL, rush_40_yds = NULL,
                                           rush_tds = NULL, rush_100_yds = NULL, rush_150_yds = NULL,
                                           rush_200_yds = NULL),
                                 RB = list(rush_yds = NULL, rush_att = NULL, rush_40_yds = NULL,
                                           rush_tds = NULL, rush_100_yds = NULL, rush_150_yds = NULL,
                                           rush_200_yds = NULL),
                                 WR = list(rush_yds = NULL, rush_att = NULL, rush_40_yds = NULL,
                                           rush_tds = NULL, rush_100_yds = NULL, rush_150_yds = NULL,
                                           rush_200_yds = NULL),
                                 TE = list(rush_yds = NULL, rush_att = NULL, rush_40_yds = NULL,
                                           rush_tds = NULL, rush_100_yds = NULL, rush_150_yds = NULL,
                                           rush_200_yds = NULL)),
                     rec = list(all_pos = NULL, rec = NULL, rec_yds = NULL, rec_tds = NULL,
                                rec_40_yds = NULL, rec_100_yds = NULL, rec_150_yds = NULL,
                                rec_200_yds = NULL,
                                QB = list(rec = NULL, rec_yds = NULL, rec_tds = NULL,
                                          rec_40_yds = NULL, rec_100_yds = NULL, rec_150_yds = NULL,
                                          rec_200_yds = NULL),
                                RB = list(rec = NULL, rec_yds = NULL, rec_tds = NULL, rec_40_yds = NULL,
                                          rec_100_yds = NULL, rec_150_yds = NULL, rec_200_yds = NULL),
                                WR = list(rec = NULL, rec_yds = NULL, rec_tds = NULL, rec_40_yds = NULL,
                                          rec_100_yds = NULL, rec_150_yds = NULL, rec_200_yds = NULL),
                                TE = list(rec = NULL, rec_yds = NULL, rec_tds = NULL, rec_40_yds = NULL,
                                          rec_100_yds = NULL, rec_150_yds = NULL, rec_200_yds = NULL)),
                     misc = list(all_pos = NULL, fumbles_lost = NULL, fumbles_total = NULL,
                                 sacks = NULL, two_pts = NULL,
                                 QB = list(fumbles_lost = NULL, fumbles_total = NULL, sacks = NULL, two_pts = NULL),
                                 RB = list(fumbles_lost = NULL, fumbles_total = NULL, sacks = NULL, two_pts = NULL),
                                 WR = list(fumbles_lost = NULL, fumbles_total = NULL, sacks = NULL, two_pts = NULL),
                                 TE = list(fumbles_lost = NULL, fumbles_total = NULL, sacks = NULL, two_pts = NULL)),
                     kick = list(xp = NULL, fg_0019 = NULL, fg_2029 = NULL, fg_3039 = NULL,
                                 fg_4049 = NULL, fg_50 = NULL, fg_miss = NULL),
                     ret = list(all_pos = NULL, return_tds = NULL, return_yds = NULL),
                     idp = list(all_pos = NULL, idp_solo = NULL, idp_asst = NULL, idp_sack = NULL,
                                idp_int = NULL, idp_fum_force = NULL, idp_fum_rec = NULL, idp_pd = NULL,
                                idp_td = NULL, idp_safety = NULL,
                                DL = list(idp_solo = NULL, idp_asst = NULL, idp_sack = NULL,
                                          idp_int = NULL, idp_fum_force = NULL, idp_fum_rec = NULL,
                                          idp_pd = NULL, idp_td = NULL, idp_safety = NULL),
                                LB = list(idp_solo = NULL, idp_asst = NULL, idp_sack = NULL,
                                          idp_int = NULL, idp_fum_force = NULL, idp_fum_rec = NULL,
                                          idp_pd = NULL, idp_td = NULL, idp_safety = NULL),
                                DB = list(idp_solo = NULL, idp_asst = NULL, idp_sack = NULL,
                                          idp_int = NULL, idp_fum_force = NULL, idp_fum_rec = NULL,
                                          idp_pd = NULL, idp_td = NULL, idp_safety = NULL)),
                     dst = list(dst_fum_rec = NULL, dst_int = NULL, dst_safety = NULL, dst_sacks = NULL,
                                dst_td = NULL, dst_blk = NULL, dst_ret_yds = NULL, dst_pts_allowed = NULL))



scoring_type_for_cols = c(
  pass_att = "pass", pass_comp = "pass", pass_inc = "pass", pass_yds = "pass",
  pass_tds = "pass", pass_int = "pass", pass_40_yds = "pass", pass_300_yds = "pass",
  pass_350_yds = "pass", pass_400_yds = "pass", all_pos = "rush",
  rush_yds = "rush", rush_att = "rush", rush_40_yds = "rush", rush_tds = "rush",
  rush_100_yds = "rush", rush_150_yds = "rush", rush_200_yds = "rush",
  all_pos = "rec", rec = "rec", rec_yds = "rec", rec_tds = "rec",
  rec_40_yds = "rec", rec_100_yds = "rec", rec_150_yds = "rec",
  rec_200_yds = "rec", all_pos = "misc", fumbles_lost = "misc",
  fumbles_total = "misc", sacks = "misc", two_pts = "misc", xp = "kick",
  fg_0019 = "kick", fg_2029 = "kick", fg_3039 = "kick", fg_4049 = "kick",
  fg_50 = "kick", fg_miss = "kick", all_pos = "ret", return_tds = "ret",
  return_yds = "ret", all_pos = "idp", idp_solo = "idp", idp_asst = "idp",
  idp_sack = "idp", idp_int = "idp", idp_fum_force = "idp", idp_fum_rec = "idp",
  idp_pd = "idp", idp_td = "idp", idp_safety = "idp", dst_fum_rec = "dst",
  dst_int = "dst", dst_safety = "dst", dst_sacks = "dst", dst_td = "dst",
  dst_blk = "dst", dst_ret_yds = "dst", dst_pts_allowed = "dst"
  )


