library(readxl)
library(rvest)
library(tidyverse)



# Walter Football
scrape_walterfootball <- function(pos = c("QB", "RB", "WR", "TE", "K"), season = 2021, week = 0) {

  url <- "http://walterfootball.com/fantasy2021rankingsexcel.xlsx"

  xlsx_file <- tempfile("wf", fileext = ".xlsx")
  xl_download <- download.file(url = url, destfile = xlsx_file, mode = "wb", quiet = TRUE)


  l_pos <- lapply(pos, function(pos){

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
      as_tibble() %>%
      select(id, last_name, first_name, position)

    # Combine data w/ player ID's
    pos_df <- data %>%
      inner_join(FFA_ids, by = c("last_name", "first_name", "position")) %>%
      select(-last_name, -first_name) %>%
      mutate(data_src = "Walterfootball")


    # New column names
    # subset by the columns in pos_df
    stat_cols = tibble(
      id = "id", Player = "player", Team = "tm", Bye = "bye", position = "pos", data_src = "data_src",
      `PASS YDS` = "pass_yds", `PASS TD` = "pass_tds", INT = "pass_int",
      `RUSH YDS` = "rush_yds", CATCH = "rec", `REC YDS` = "rec_yds", `REG TD` = "rec_tds",
      `FG 1-39` = "fg_0039", `FG 40-49` = "fg_4049", `FG 50+` = "fg_50", XP = "xp",
      `Points (ESPN Scoring)` = "site_pts"
    ) %>% select(colnames(pos_df)) %>%
      as.character()

    # Rename columns with new names
    colnames(pos_df) <- stat_cols

    pos_df

  })

  # list elements named by position
  names(l_pos) = pos
  attr(l_pos, "season") = season
  attr(l_pos, "week") = week
  l_pos

}
