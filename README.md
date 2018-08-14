# ffanalytics

This package allows users to scrape projected stats from several sites that have
publicly available projections. Once data is scraped the user can then use functions
within the package to calculate projected points and produce rankings. The package
relies heavily on the vocabulary from the `tidyverse` and users will better be
able to use the package if they familiarize themselves with the `tidyverse` way 
of creating code.

## Installation
Intallation of the `ffanalytics` package can be done directly from github:
`devtools::install_github("FantasyFootballAnalytics/ffanalytics")`

## Projection sources
The following sources are available for scraping:

* For seasonal data: CBS, ESPN, FantasyData, FantasyPros, FantasySharks, FFToday, 
FleaFlicker, NumberFire, Yahoo, FantasyFootballNerd, NFL, RTSports, Walterfootball
* For weekly data: CBS, ESPN, FantasyData, FantasyPros, FantasySharks, FFToday, 
FleaFlicker, NumberFire, Yahoo, FantasyFootballNerd, NFL

While the scrape functions allows the user to specify season and week, scraping
historical periods will not be successful.

Projection sources are defined as `R6` classes and the `projection_sources` object
is a list containing the projection sources defined in the pacakge. Review the
`source_classes.R` file to see how these classes are defined and the `source_configs.R`
file in the `data-raw` directory has all the individual sources defined and running
that script will re-create the `projections_sources` object for the package

## Scraping data
The main function for scraping data is `scrape_data`. This function will pull data
from the sources specified, for the positions specified in the season and week specificed.
To pull data for QBs, RBs, WRs, TEs and DSTs from CBS, ESPN and Yahoo for the 2018
season the user would run:
```
my_scrape <- scrape_data(src = c("CBS", "ESPN", "Yahoo"), 
                         pos = c("QB", "RB", "WR", "TE", "DST"),
                         season = 2018, week = 0)
```

`my_scrape` will be a list of tibbles, one for each positon scraped, which contains
the data for each source for that position. In the `tibble` the `data_src` column
speficies the source of the data.

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
To add rankings information, risk value and ADP/AAV data use the `add_ecr`, `add_risk`, 
`add_adp`, and `add_aav` functions:
```
my_projections <- my_projections %>% add_ecr() %>% add_risk() %>%
  add_adp() %>% add_aav()
```
Note that `add_ecr` will need to be called before `add_risk` to ensure that the
ECR data is available for the risk calculation.

The `add_adp` and `add_aav` allows to specify sources for ADP and AAV. See `?add_adp`,
and `?add_aav` for details.

## Player data
Player data is pulled from MFL when the package loads and stored in the `player_table`
object. To add player data to the projections table use `add_player_info`, which adds
the player names, teams, positions, age, and experience to the data set.
```
my_projections <- my_projections %>% add_player_info()
```


