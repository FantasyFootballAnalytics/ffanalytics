

pos_corrections = list(Def = "DST", D = "DST", DEF = "DST", "D/ST" = "DST", PK = "K" ,
                       CB = "DB", S = "DB", DE = "DL", DT = "DL")


team_corrections <- list(KCC = "KC", SFO = "SF", TBB = "TB", NEP = "NE", RAM = "LAR",
                         LA = "LAR", SDC = "SD", ARZ = "ARI", NOR = "NO", GBP = "GB",
                         JAX = "JAC", WSH = "WAS", HST = "HOU", CLV = "CLE", BLT = "BAL",
                         NWE = "NE", NOS = "NO", LVR = "LV")



nflTeam.abb <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
                 "DAL", "DEN", "DET", "GB",  "HOU", "IND", "JAC", "KC",
                 "MIA", "MIN", "NO",  "NE",  "NYG", "NYJ", "PHI", "PIT",
                 "LA",  "SF",  "LAC", "TB",  "TEN", "WAS", "SEA", "LV")

mflTeam.abb <- replace(nflTeam.abb, nflTeam.abb == "JAX", "JAC")
mflTeam.abb <- replace(mflTeam.abb, nflTeam.abb == "LA", "LAR")


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
