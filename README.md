# ffanalytics

This package allows users to scrape projected stats from several sites that have
publicly available projections. Once data is scraped the user can then use functions
within the package to calculate projected points and produce rankings. The package
relies heavily on the vocabulary from the `tidyverse` and users will better be
able to use the package if they familiarize themselves with the `tidyverse` way 
of creating code.

**Version 3 of the package**:

Summer of 2022 we incremented to version 3.0 of the package. There are several
things worth highlighting:

Breaking changes:

* `add_risk()` is no longer exported and is superseded by `add_uncertainty()`
* Several helper functions are no-longer exported
* When loading `ffanalytics`, no other packages load with it (i.e., we removed 
all packages from the "Depends" field). Previously, calling `library(ffanalytics)`
also loaded `dplyr`, `tidyr`, and several other packages.
* We no longer use the projection_sources `R6` object internally

Updates:

* Individual scrapes are now self-contained internally (e.g. `ffanalytics:::scrape_cbs()`)
* Rate limits have been added to all scrapes (typically waiting 2 seconds between pages)
* The `projections_table` function has a new argument: `avg_type = c("average", "robust", "weighted")`. 
By default the `projections_table` function will compute all average types, but 
one or two can be specified. 


## Installation
Installation of the `ffanalytics` package can be done directly from github:
```
install.packages("remotes")
remotes::install_github("FantasyFootballAnalytics/ffanalytics")
```

## Projection sources
The following sources are available for scraping:

* For seasonal data: CBS, ESPN, FantasyPros, FantasySharks, FFToday, 
NumberFire, FantasyFootballNerd, NFL, RTSports, Walterfootball
* For weekly data: CBS, ESPN, FantasyPros, FantasySharks, FFToday, 
FleaFlicker, NumberFire, FantasyFootballNerd, NFL

Although the scrape functions allows the user to specify season and week, scraping
historical periods will not be successful.

## Scraping data
The main function for scraping data is `scrape_data`. This function will pull data
from the sources specified, for the positions specified in the season and week specified.
To pull data for QBs, RBs, WRs, TEs and DSTs from CBS, NFL and NumberFire for the 2022
season the user would run:
```
my_scrape <- scrape_data(src = c("CBS", "NFL", "NumberFire"), 
                         pos = c("QB", "RB", "WR", "TE", "DST"),
                         season = NULL, # NULL grabs the current season
                         week = NULL) # NULL grabs the current week
```

`my_scrape` will be a list of tibbles, one for each position scraped, which contains
the data for each source for that position. In the `tibble` the `data_src` column
specifies the source of the data.

## Calculating projections
Once data is scraped the projected points can be calculated. this is done with
the `projections_table` function:
```
my_projections <-  projections_table(my_scrape)
```
This will calculate projections using the default settings. You can provide additional
parameters for the `projections_table` function to customize the calculations. 
See `?projections_table` for details.

## Adding additional information
To add rankings information, risk value and ADP/AAV data use the `add_ecr`, 
`add_uncertainty` (superseding `add_risk`), `add_adp`, and `add_aav` functions:
```
my_projections <- my_projections %>% 
  add_ecr() %>% 
  add_adp() %>% 
  add_aav() %>%
  add_uncertainty() 
```
Note that `add_ecr` will need to be called before `add_uncertainty` to ensure that the
ECR data is available for the uncertainty calculation.

The `add_adp` and `add_aav` allows to specify sources for ADP and AAV. See `?add_adp`,
and `?add_aav` for details.

## Player data
Player data is pulled from MFL when the package loads and stored in the `player_table`
object. To add player data to the projections table use `add_player_info`, which adds
the player names, teams, positions, age, and experience to the data set.
```
my_projections <- my_projections %>% 
  add_player_info()
```
