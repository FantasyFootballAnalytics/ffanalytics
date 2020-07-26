#' Projection Source Object
#'
#' This objects represent the basic set of data needed for defining a projection
#' source. It is used as superclass for the specific source objects that are
#' specific to which type of data is returned. Use \link{html_source},
#' \link{json_source} and \link{xlsx_source} to create a projection source.
#'
#' @field base The base url for the source
#' @field id_col The name of the column in the \code{player_ids} table that
#' refers to the id of the player in the source.
#' @field league_id This is used for Yahoo sources. Set to the id of the league
#' that you want to scrape data from
#' @field api_key The api key associated with the source if applicable
#' @field get_path A function of \code{season, week, position} which generates
#' the url path to scrape data from
#' @field get_query A function of \code{season, week, position} which generates
#' the url query that will be used to scrape data
#' @field url_positions A function of position that will convert the standard
#' position designation \code{QB, RB, WR, TE, K, DST, DL, LB, DB} or others to the
#' designation that the site uses
#' @field min_week The minimum week that the site provide data for
#' @field max_week The maximum week that the site provide data for
#' @field season_pos A character vector of position names that the site provide data
#' for for season
#' @field week_pos A character vector of position names that the site provide data
#' for for weekly data,
#' @field stat_cols A named character vector that will convert the site column
#' names to standard column names.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{get_url}}{Function of \code{season, week, position} that will
#'   generate the full URL that will be scraped for that season, week and
#'   position}
#'   \item{\code{name_cols}}{Method that will take a table and set the column
#'   names as specified in the \code{stat_cols} field}
#'   \item{\code{set_id}}{Method that will take a table and add the MFL id column
#'   with values based on the \link{player_ids} table}
#' }
#' @docType class
#' @family source-class
#' @keywords internal
#' @format An R6 class object.
projection_source <- R6::R6Class(
  "projection_source",
  public = list(
    base = NULL,
    id_col = NULL,
    league_id = NULL,
    api_key = NULL,
    get_path = NULL,
    get_query = NULL,
    url_positions = NULL,
    min_week = NULL,
    max_week = NULL,
    season_pos = NULL,
    week_pos = NULL,
    stat_cols = NULL,
    initialize = function(id_col = NULL, base = NULL, league_id = NULL, api_key = NULL,
                          get_query = NULL, get_path = NULL, url_positions = NULL,
                          min_week = 0, max_week = NULL, season_pos = NULL, week_pos = NULL,
                          stat_cols = NULL
    ){
      self$id_col <- id_col
      self$base <- base
      self$league_id <- league_id
      self$api_key <- api_key
      self$get_query <- get_query
      self$get_path <- get_path
      self$url_positions <- url_positions
      self$min_week <- as.integer(min_week)
      self$max_week <- as.integer(max_week)
      self$season_pos <- season_pos
      self$week_pos <- week_pos
      self$stat_cols <- stat_cols
    },
    print = function(){
      cat(crayon::bold("Accessing:\t  "), self$base, "\n")
      cat(crayon::bold("Season data:\t  "), self$min_week == 0, "\n")
      cat(crayon::bold("Weekly data:\t  "), self$max_week > 0, "\n")
      cat(crayon::bold("Post season data: "), self$max_week >  17, "\n")
      cat(crayon::bold("Sesaon positions: "), paste(self$season_pos, collapse = ", "), "\n")
      cat(crayon::bold("Weekly positions: "), paste(self$week_pos, collapse = ", "), "\n")
    },
    get_url = function(season, week = NULL, position , ...){
      if(!(week %in% self$min_week:self$max_week))
        return(NULL)

      if(is.null(week) || week == 0)
        allowed_pos <- self$season_pos
      else
        allowed_pos <- self$week_pos

      if(!(position %in% allowed_pos))
        return(NULL)

      if(is.null(self$url_positions) || is.null(self$url_positions(position)))
        p_id <- position
      else
        p_id <- self$url_positions(position)

      if(private$data_host() %in% c("www.numberfire.com", "api.fantasy.nfl.com", "www.fantasyfootballnerd.com"))
        position <- p_id


      full_url <- self$base
      cur_path <- httr::parse_url(full_url)$path
      cur_query <- httr::parse_url(full_url)$query

      if(!is.null(self$get_path) && !is.null(self$get_path(season, week, position))){
        u_path <- self$get_path(season, week, position)
        new_path <- paste0(cur_path, u_path)

        if(private$data_host()=="www.fantasyfootballnerd.com")
          new_path <- glue::glue(new_path, api_key = self$api_key)

        full_url <- httr::modify_url(full_url, path = new_path)
      } else if(private$data_host()=="football.fantasysports.yahoo.com") {
        new_path <- paste0(cur_path, paste(self$league_id, "players", sep = "/"))
        full_url <- httr::modify_url(full_url, path = new_path)
      }

      if(!is.null(self$get_query) && !is.null(self$get_query(season, week, p_id, ...))){
        new_query <- self$get_query(season, week, p_id, ...)
        full_url <- httr::modify_url(full_url, query = new_query)
      }

      return(full_url)
    },
    name_cols = function(data_tbl, position){

      stat_cols <- tolower(self$stat_cols)
      data_tbl <- rename_all(data_tbl, tolower)
      stat_cols <- stat_cols[which(stat_cols %in% names(data_tbl))]

      if(position == "K"){
        rename_cols <- stat_cols[matches("^fg|^xp|^site|^games", vars = names(stat_cols))]
      } else if (position == "DST"){
        rename_cols <- stat_cols[matches("^dst|^site|^games", vars = names(stat_cols))]
      } else if (position %in% c("DL", "LB", "DB", "IDP", "D")){
        rename_cols <- stat_cols[matches("^idp|^site|^games", vars = names(stat_cols))]
      } else {
        rename_cols <- stat_cols[matches("^pass|^rush|^rec|^rxx|^fum|^sac|^two|^reg|^ret|^site|^games|^kick|^punt", vars = names(stat_cols))]
      }

      data_tbl <- data_tbl %>%
        rename(!!!rename_cols) %>%
        ffanalytics:::clean_format() %>%
        modify_at(names(stat_cols), as.numeric) %>%
        modify_at(stat_cols, as.numeric) %>%
        clean_names()

      if("bye" %in% names(data_tbl))
        data_tbl <- data_tbl %>% modify_at("bye", as.numeric)

      if(any(grepl("^rxx", names(data_tbl))) & any(grepl("rush_[0-9]+_tds", names(data_tbl))))
        data_tbl <- rename_at(data_tbl, vars(matches("^rxx_[0-9]+")), funs(gsub("rxx", "rush", .)))

      if(any(grepl("^rxx", names(data_tbl))) & any(grepl("rec_[0-9]+_tds", names(data_tbl))))
        data_tbl <- rename_at(data_tbl, vars(matches("^rxx_[0-9]+")), funs(gsub("rxx", "rec", .)))


      return(data_tbl)
    },
    set_id = function(data_tbl){
      if("src_id" %in% names(data_tbl))
        data_tbl %>% add_column(id =  id_col(data_tbl$src_id, self$id_col), .before = 1)
      else{
        pos <- private$session$pos
        if(!is.null(pos) && pos == "DST"){
          dst_data <- filter(ff_player_data, position == "Def")
          if(private$data_host() == "www.cbssports.com" & position == "DST") {
            dst_data$team <- replace(dst_data$team, dst_data$team == "OAK", "LV")
          }
          if("player" %in% names(data_tbl) && all(data_tbl$player %in% dst_data$name))
            data_tbl %>% add_column(id =   dst_data$id[match(data_tbl$player, dst_data$name)], .before = 1)
          else if("team" %in% names(data_tbl) && all(data_tbl$team %in% dst_data$team))
            data_tbl %>% add_column(id =   dst_data$id[match(data_tbl$team, dst_data$team)], .before = 1)
        } else {
          data_tbl
        }
      }
    }
  ),
  private = list(
    data_host = function()httr::parse_url(self$base)$hostname
  )
)

#' HTML source object
#'
#' This objects represent the basic set of data needed for defining a projection
#' source with HTML data. It is an extesnsion of the \link{projection_source}
#' object with some specific fields and methods related to scraping HTML data.
#'
#' @field table_css A string with a CSS selector identifying the HTML
#' \code{<table>} element holding the projection data.
#' @field pid_css A string with a CSS selector idenfitying the HTML node holding
#' the player id if available
#' @field rm_elem A character vector of CSS selectors identifying HTML nodes to
#' remove for successful scraping of the table.
#' @field index If \code{table_css} does not uniquely identify the table, use this
#' field to identify the index number for the table in the list of nodes. If
#' multiple numbers are specified then the tables are ssume to have identical
#' number of rows and will be combined with \link{bind_cols}
#' @field extract_pid A function that will take the HTML node holding the player
#' id and extract the specific player_id
#' @field split_cols A list with each element being a list representing input to
#' either \link{separate} or \link{extract}. Each input element should be in the
#' format of a function of position to allow for different handling of fields for
#' different positions. See the \code{projection_sources} object for predefined
#' sources.
#' @field recode_cols a list with each element being a list representing names
#' of columns to be recoded and a named vector for each column holding the recode
#' values to be used by \link{recode}.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{open_session}}{Takes \code{season, week, position} as input and
#'   and opens a session on the website via the \link{html_session} function after
#'   determining the URL}
#'   \item{\code{close_session}}{Closes the session that is currently open}
#'   \item{\code{get_table}}{Retrieves the table from the session without any
#'   spliltting of columns defined in \code{split_cols}, any recoding as defined
#'   in \code{recode_cols} or any renaming based on \code{stat_cols}}
#'   \item{\code{scrape}}{Scrapes data from the table specified and wrangles the
#'   columns based on \code{split_cols}, \code{recode_cols} and \code{stat_cols}}
#' }
#'
#' @note See \url{https://www.w3schools.com/cssref/css_selectors.asp} for details
#' on defining CSS selectors
#' @docType class
#' @family source-class
#' @keywords internal
#' @format An R6 class object.
html_source <- R6::R6Class(
  "html_source",
  inherit = projection_source,
  public = list(
    table_css = NULL,
    pid_css = NULL,
    rm_elem = NULL,
    index = NULL,
    extract_pid = NULL,
    split_cols = list(),
    recode_cols = list(),
    initialize = function(id_col = NULL, base = NULL, league_id = NULL, api_key = NULL,
                          get_query = NULL, get_path = NULL,
                          url_positions = NULL, min_week = 0, max_week = NULL,
                          season_pos = NULL, week_pos = NULL, table_css = NULL,
                          pid_css = NULL, rm_elem = NULL, index = NULL,
                          extract_pid = NULL, split_cols = list(),
                          stat_cols = NULL, recode_cols = list()
    ){

      super$initialize(id_col, base, league_id, api_key, get_query, get_path,
                       url_positions, min_week, max_week, season_pos, week_pos,
                       stat_cols)
      self$table_css <- table_css
      self$pid_css <- pid_css
      self$rm_elem <- rm_elem
      self$index <- index
      self$extract_pid <- extract_pid
      self$split_cols <- split_cols

      self$recode_cols <- recode_cols
    },
    open_session = function(season, week = NULL, position , ...){
      session_url <- self$get_url(season, week, position , ...)
      if(is.null(session_url)){
        self$close_session()
        return(invisible(self))
      }

      if(private$data_host() == "www.fantasysharks.com"){
        private$session <- session_url
      } else {
        src_session <- html_session(session_url)
        src_session[c("season", "week", "position")] <- list(season, week, position)
        private$session <- src_session
      }
      invisible(self)
    },
    close_session = function(){private$session <- NULL},
    get_table = function(){

      src_session <- private$session

      if(is.null(src_session))
        return(NULL)

      if(any(c("season", "week",  "position") %in% names(src_session))){
        season <- src_session$season
        week <- src_session$week
        position <- src_session$position
      } else {
        fs_pos <- c(QB = 1, RB =2 , WR = 4, TE = 5, K = 7, DST = 6, DL = 8, LB = 9, DB = 10)
        url_pos <- as.integer(httr::parse_url(src_session)$query$Position)
        position <- names(fs_pos)[which(fs_pos == url_pos)]
      }

      # Lookup the css selector for the table. If not found throw an error
      table_css <- self$table_css

      if(is.null(table_css))
        stop("Table selector not defined", call. = FALSE)

      # Lookup the css selector for the players and elements that can be removed
      pid_css <- self$pid_css
      rm_elem <- self$rm_elem


      # Initialize data frame to hold data.
      table_data <- tibble()

      repeat{
        data_page <- tryCatch(
          read_html(src_session),
          error = function(e){
            warning(e, call. = FALSE)
            next
          })

        if(length(rm_elem) > 0){
          map(rm_elem, html_nodes, x = data_page) %>% map(xml_remove)
        }

        if(length(html_nodes(data_page, table_css)) == 0){
          warning("Table not found on url: ", src_session$url, call. = FALSE)
          break
        }


        # For CBS we need to set the colspan on the first two cells in the table header
        if(private$data_host() == "www.cbssports.com" & position != "DST"){
          data_page %>% xml_nodes("tr.TableBase-headGroupTr th:nth-child(-n+2)") %>%
            `xml_attr<-`(attr = "colspan", value= "1")
        }

        if(private$data_host() == "www.cbssports.com" & position == "DST"){
          data_page %>% xml_nodes("tr.TableBase-headGroupTr th:nth-child(-n+10)") %>%
            `xml_attr<-`(attr = "colspan", value= "1")

          data_page %>% xml_nodes("tr.TableBase-headGroupTr th:nth-last-child(-n+2)") %>%
            `xml_attr<-`(attr = "colspan", value= "1")
        }


        if(is.null(self$index)){
          data_table <- data_page %>% html_node(table_css) %>%
            html_table(header = TRUE, fill = TRUE)

          header_rows <- data_page %>% html_node(table_css) %>%
            html_node("thead") %>% html_children() %>% length()

          if(private$data_host() == "www.fftoday.com" & !(position %in% c("DL", "LB", "DB")))
            header_rows <- 2

          if(private$data_host() == "www.fftoday.com" & (position %in% c("DL", "LB", "DB"))){
            names(data_table) <- data_table %>% make_df_colnames()
            data_table <- data_table %>% slice(-1)
          }

          if(header_rows > 1){
            names(data_table) <- make_df_colnames(data_table)
            data_table <- data_table %>% slice(-1)

            if(private$data_host() == "www.fftoday.com"){
              names(data_table) <- names(data_table) %>%
              str_remove("^[0-9\\s]+") %>%
              grep("^[^NA]", ., value = TRUE) %>% str_remove_all("[0-9]")
            }
          }
        } else {
          data_table <- html_nodes(data_page, table_css)%>%
            html_table(header = TRUE, fill = TRUE) %>% .[self$index] %>%
            map(check_2rth) %>% bind_cols
        }

        if(nrow(data_table) == 0)
          break

        # Cheking to see if the less than 10% of cells in first row in the table
        # is a numeric value; If so, then we suspect a two table header.
        #num_cols <- ncol(data_table)
        two_row_th <- FALSE #suppressWarnings(mean(is.na(as.numeric(slice(data_table, 1)))) > 0.9)

        if(private$data_host() == "fantasydata.com"){
          names(data_table)[2:length(data_table)] <- data_page %>%
            html_nodes("table tr th a") %>%
            html_attr("href") %>%
            gsub("(^.+','Sort\\$)(.+)('\\))", "\\2", .) %>%
            gsub("Fantasy*", "", ., ignore.case = TRUE)
        } else if(two_row_th){
          names(data_table) <- make_df_colnames(data_table)
          data_table <- data_table %>% slice(-1)
        } else {
          names(data_table) <- make.unique(names(data_table), sep = "")
        }


        if(!is.null(pid_css)){
          get_pid <- self$extract_pid
          player_ids <- data_page %>% html_nodes(pid_css) %>% get_pid()
          if(length(player_ids) == nrow(data_table))
            data_table <- data_table %>% add_column(src_id = player_ids, .before = 1)
        } else {
          src_id_col <- intersect(names(data_table), c("PlayerID"))
          if(length(src_id_col) > 0){
            names(src_id_col) <- "src_id"
            data_table <- rename(data_table, !!!src_id_col) %>%
              mutate(src_id = as.character(src_id))
          }
        }

        if(private$data_host() == "www.cbssports.com" & position == "DST"){
          team_names <- data_page %>% html_nodes("span.TeamName a") %>%
            html_attr("href") %>% str_extract("[A-Z]{2,3}")

          data_table <- mutate(data_table, Team = team_names)
        }

        table_data <- bind_rows(table_data, data_table)

        next_url <- data_page %>%
          html_node("a:contains('NEXT'), a:contains('Next')") %>% html_attr("href")

        if(is.na(next_url))
          break

        src_session <- src_session %>% jump_to(next_url)
      }

      return(table_data)
    },
    scrape = function(){
      src_table <- self$get_table()

      if(is.null(src_table))
        return(NULL)

      if("position" %in% names(private$session)){
        position <- private$session$position
      } else {
        fs_pos <- c(QB = 1, RB =2 , WR = 4, TE = 5, K = 7, DST = 6, DL = 8, LB = 9, DB = 10)
        url_pos <- as.integer(httr::parse_url(private$session)$query$Position)
        position <- names(fs_pos)[which(fs_pos == url_pos)]
      }


      split_cols <- self$split_cols %>%   modify_depth(2, ~ .(position)) %>%
        modify_depth(2, ~ case_when(!!! .)) %>% modify_depth(2, ~ .x[!is.na(.x)]) %>%
        map(~ .x[map_lgl(.x , ~ length(.x) > 0)])

      split_cols <- split_cols[map_lgl(split_cols, ~ any(names(src_table) == .x$col))]
      sep_cols <- split_cols[map_lgl(split_cols, ~ any(names(.x) == "sep"))]
      ex_cols <-  split_cols[!map_lgl(split_cols, ~ any(names(.x) == "sep"))]

      src_table <- src_table %>%
        mutate_if(is.character, ~ str_replace_all(., "\\-\\-", ""))

      if(length(sep_cols) > 0)
        src_table <- accumulate(sep_cols,
                                ~ separate(data = .x, .y[["col"]], .y[["into"]],
                                           .y[["sep"]], convert = TRUE),
                               .init = src_table)[[length(sep_cols) + 1]]

      if(length(ex_cols) > 0)
        src_table <- accumulate(ex_cols,
                                ~ extract(data = .x, .y[["col"]], .y[["into"]],
                                          .y[["regex"]], convert = TRUE),
                               .init = src_table)[[length(ex_cols) + 1]]

      if(length(self$recode_cols) > 0){
        idx <- which(self$recode_cols$name %>%
                       simplify() %in% names(src_table))

        if(length(idx) > 0){
          recode_col <- self$recode_cols$name[idx]
          recode_val <- self$recode_cols$recode_vals[idx]
          for(i in seq_along(idx)){
            src_table <- src_table %>%
              mutate_at(vars(one_of(recode_col[[i]])), funs(recode(., !!!recode_val[[i]])))
          }
        }
      }

      src_table <- self$name_cols(src_table, position)

      if(private$data_host() == "www.fftoday.com" & position == "DST")
        src_table <- rename(src_table, player = team)

      if(private$data_host() == "www.numberfire.com")
        src_table <- select(src_table, -abbr_name)

      if(any(grepl("name$", names(src_table)))){
        rn_name <- function(x) return("player")
        src_table <- src_table %>% rename_at(vars(matches("name$")), funs(rn_name(.)))
      }
      src_table <- src_table %>% self$set_id()

      return(src_table)
    }
  ),
  private = list(session = NULL)
)

#' JSON source object
#'
#' This objects represent the basic set of data needed for defining a projection
#' source with JSON data. It is an extesnsion of the \link{projection_source}
#' object with some specific fields and methods related to scraping JSON data.
#'
#' @field json_elem String containing the name of the JSON element in the result
#' that identifies the data
#' @field stat_elem String containing the name of the stats element in the JSON
#' result if applicable
#' @field player_elem Character vector containing the name of the player elements
#' in the JSON result if applicable
#' @field player_cols Named character vector used to rename the player columns in
#' the JSON result if needed.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{scrape}}{Scrape the data from the source based on season, week
#'   and position provided}
#' }
#' @docType class
#' @family source-class
#' @keywords internal
#' @format An R6 class object.
json_source <- R6::R6Class(
  "json_source",
  inherit = projection_source,
  public = list(
    json_elem = NULL,
    stat_elem = NULL,
    player_elem = NULL,
    player_cols = NULL,
    initialize = function(id_col = NULL, base = NULL, league_id = NULL, api_key = NULL,
                          get_query = NULL, get_path = NULL,
                          url_positions = NULL, min_week = 0, max_week = NULL,
                          season_pos = NULL, week_pos = NULL,  json_elem = NULL,
                          stat_elem = NULL, player_elem = NULL, stat_cols = NULL,
                          player_cols = NULL
    ){

      super$initialize(id_col, base, league_id, api_key, get_query, get_path,
                       url_positions, min_week, max_week, season_pos, week_pos,
                       stat_cols)
      self$json_elem <- json_elem
      self$stat_elem <- stat_elem
      self$player_elem <- player_elem
      self$player_cols <- player_cols

    },
    scrape = function(season, week = NULL, position , ...){
      scrape_url <- self$get_url(season, week, position , ...)

      if(is.null(scrape_url))
        return(NULL)

      json_elem <- ifelse(week == 0, self$json_elem$season,
                          self$json_elem$weekly)

      json_res <- httr::content(httr::GET(scrape_url))

      json_res <- json_res[[json_elem]]

      if(!is.null(self$stat_elem)){
        stats <- self$stat_elem
        stat_cols <- json_res %>% map(`[[`, stats) %>% map(as_tibble) %>% map_df(type.convert, as.is = TRUE)

        if(!is.null(self$player_elem)){
          player_info <- self$player_elem
        } else {
          player_info <- json_res %>% map(~setdiff(names(.x), stats)) %>% reduce(union)
        }

        data_table <- json_res %>%
          map(`[`, player_info) %>% map(discard, is.null) %>% map(as_tibble) %>% bind_rows()

        data_table <- bind_cols(data_table, stat_cols)
      } else {
        data_table <- bind_rows(map(json_res, as_tibble))
      }

      if(private$data_host() == "api.fantasy.nfl.com"){
        rn_cols <- nfl_cols[which(nfl_cols %in% names(data_table))]
        data_table <- data_table %>% rename(!!!rn_cols)
      }

      data_table <- data_table %>%
        select_if(~ !all(ifelse(is.na(.x),0,.x) == 0)) %>%
        select_if(~ !all(ifelse(is.na(.x),"",.x) == "")) %>%
        select_if(~ !all(ifelse(is.na(.x),"",.x) == "0.0")) %>%
        self$name_cols(position)

      player_cols <- self$player_cols[which(self$player_cols %in% names(data_table))]
      data_table <- data_table %>% rename(!!!player_cols) %>% self$set_id()
      if("src_id" %in% names(data_table)){
        data_table <- mutate(data_table, src_id = as.character(src_id))
      }
      if("position" %in% names(data_table)){
        data_table <- mutate(data_table, position = as.character(position))
      }

      return(data_table)
    }
  )
)


#' xlsx source object
#'
#' This objects represent the basic set of data needed for defining a projection
#' source with xlsx data. It is an extesnsion of the \link{projection_source}
#' object with some specific fields and methods related to scraping xlsx data.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{scrape}}{Scrape the data from the source based on season, week
#'   and position provided}
#' }
#' @docType class
#' @family source-class
#' @keywords internal
#' @format An R6 class object.
xlsx_source <- R6::R6Class(
  "xlsx_source",
  inherit = projection_source,
  public = list (
    initialize = function(id_col = NULL, base = NULL, league_id = NULL, api_key = NULL,
                          get_query = NULL, get_path = NULL, url_positions = NULL,
                          min_week = 0, max_week = NULL, season_pos = NULL, week_pos = NULL,
                          stat_cols = NULL
    ){
      super$initialize(id_col, base, league_id, api_key, get_query, get_path,
                       url_positions, min_week, max_week, season_pos, week_pos,
                       stat_cols)
    },
    scrape = function(season, week = NULL, position , ...){
      scrape_url <- self$get_url(season, week, position , ...)

      if(is.null(scrape_url))
        return(NULL)

      xl_file <-  tempfile("wf", fileext = ".xlsx")
      xl_dl <- download.file(scrape_url, xl_file, mode = "wb", quiet = TRUE)

      sheet_name = self$url_positions(position)

      data_table <- read_xlsx(path = xl_file, sheet = sheet_name) %>%
        select_if(~ any(!is.na(.x))) %>%
        select(matches("^Pass|^Rush|^Rec|^Reg TD$|^Int|^FG|^XP|name$|^player|^Team$|^Pos|^Bye"))

      if(length(matches("[La|Fir]st Name", vars = names(data_table))) > 0){
        data_table <- data_table %>%
          unite("Player", "First Name", "Last Name", sep = " ")
      }

      data_table <- self$name_cols(data_table, position)

      data_table <- add_column(data_table, id = match_players(data_table), .before = 1)

      return(data_table)
    }
  )
)
