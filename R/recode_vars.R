
name_corrections <- list(
  "Ty Hilton" = "T.Y. Hilton",
  "Timothy Wright" = "Tim Wright",
  "Christopher Ivory" = "Chris Ivory",
  "Domanique Davis" = "Dominique Davis",
  "Ben Watson" = "Benjamin Watson",
  "Stevie Johnson" = "Steve Johnson",
  "Lesean McCoy" = "LeSean McCoy",
  "Luke Wilson" = "Luke Willson",
  "Thaddeus Lewis" = "Thad Lewis",
  "Walter Powell" = "Walt Powell",
  "Wilson VanHooser" = "Wilson Van Hooser",
  "Steve Hauschka" = "Steven Hauschka",
  "Stephen Hauschka" = "Steven Hauschka",
  "Daniel Herron" = "Dan Herron",
  "Robert Housler" = "Rob Housler",
  "Corey Philly Brown" = "Philly Brown",
  "Foswhitt Whittaker" =  "Fozzy Whittaker",
  "CJ Anderson" = "C.J. Anderson",
  "TY Hilton" = "T.Y. Hilton",
  "Boobie Dixon" = "Anthony Dixon",
  "EZ Nwachukwu" = "Uzoma Nwachukwu",
  "Dave Paulson" = "David Paulson",
  "Joe DonDuncan" = "Joe Don Duncan",
  "T Y Hilton" = "T.Y. Hilton",
  "Dqwell Jackson" = "DQwell Jackson",
  "Art Jones" = "Arthur Jones",
  "Navorro Bowman" =  "NaVorro Bowman",
  "Devante Parker" = "DeVante Parker",
  "AJ McCarron" = "A.J. McCarron",
  "TJ Yeldon" = "T.J. Yeldon",
  "CJ Prosise" = "C.J. Prosise",
  "AJ Green" = "A.J. Green",
  "David A. Johnson" = "David Johnson",
  "Adrian L. Peterson" = "Adrian Peterson",
  "Jonathan C. Stewart" = "Jonathan Stewart",
  "Chris D. Johnson" = "Chris Johnson",
  "Austin D. Johnson" = "Austin Johnson",
  "Steve L. Smith" = "Steve Smith",
  "Michael A. Thomas" = "Michael Thomas",
  "Devin A. Smith" = "Devin Smith",
  "Michael D. Thomas" = "Michael Thomas",
  "Robert Kelley" = "Rob Kelley",
  "Fairbairn Ka'imi" = "Ka'imi Fairbairn",
  "Will Lutz" = "Wil Lutz")

pos_corrections = list(Def = "DST", D = "DST", DEF = "DST", "D/ST" = "DST", PK = "K" ,
                       CB = "DB", S = "DB", DE = "DL", DT = "DL")


team_corrections <- list(KCC = "KC", SFO = "SF", TBB = "TB", NEP = "NE", RAM = "LAR",
                         LA = "LAR", SDC = "SD", ARZ = "ARI", NOR = "NO", GBP = "GB",
                         JAX = "JAC", WSH = "WAS", HST = "HOU", CLV = "CLE", BLT = "BAL",
                         NWE = "NE", NOS = "NO", LVR = "LV")



nflTeam.abb <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
                 "DAL", "DEN", "DET", "GB",  "HOU", "IND", "JAX", "KC",
                 "MIA", "MIN", "NO",  "NE",  "NYG", "NYJ", "PHI", "PIT",
                 "LA",  "SF",  "LAC", "TB",  "TEN", "WAS", "SEA", "LV")

mflTeam.abb <- gsub("JAX", "JAC", nflTeam.abb)

nflTeam.id <- c("100026", "100001", "100002", "100003", "100004", "100005", "100006", "100007",
                "100008", "100009", "100010", "100011", "100013", "100014", "100015", "100016",
                "100019", "100020", "100022", "100021", "100023", "100024", "100025", "100027",
                "100017", "100029", "100028", "100031", "100012", "100032", "100030", "100018")


nflTeam.city <- c("Arizona",   "Atlanta",       "Baltimore",   "Buffalo",     "Carolina",  "Chicago",      "Cincinnati",   "Cleveland",
                  "Dallas",    "Denver",        "Detroit",     "Green Bay",   "Houston",   "Indianapolis", "Jacksonville", "Kansas City",
                  "Miami",     "Minnesota",     "New Orleans", "New England", "New York",  "New York",     "Philadelphia", "Pittsburgh",
                  "Los Angeles", "San Francisco", "Los Angeles",    "Tampa Bay",  "Tennessee", "Washington",   "Seattle", "Las Vegas")


nflTeam.name <- c("Cardinals", "Falcons", "Ravens", "Bills",      "Panthers", "Bears",    "Bengals",  "Browns",
                  "Cowboys",   "Broncos", "Lions",  "Packers",    "Texans",   "Colts",    "Jaguars",  "Chiefs",
                  "Dolphins",  "Vikings", "Saints", "Patriots",   "Giants",   "Jets",     "Eagles",   "Steelers",
                  "Rams",  "49ers",   "Chargers",   "Buccaneers", "Titans",   "Redskins", "Seahawks", "Raiders")
