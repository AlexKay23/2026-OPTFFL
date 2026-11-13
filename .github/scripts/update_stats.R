library(jsonlite)

league_id <- "710908445"  # <-- Keep your numeric ESPN League ID here
season <- 2026
url <- paste0(
  "https://lm-api-reads.fantasy.",
  "espn",
  ".com/apis/v3/games/ffl/seasons/",
  season,
  "/segments/0/leagues/",
  league_id,
  "?view=mTeam&view=mMatchup"
)

# Fetch league data from ESPN public API
data <- fromJSON(url, simplifyVector = FALSE)

# 1. Map team IDs to names and find the Points For (PF) leader
teams <- list()
pf_leader_name <- "None"
pf_leader_pts <- 0

for (t in data$teams) {
  tid <- as.character(t$id)
  name <- if (!is.null(t$name) && t$name != "") {
    t$name
  } else {
    trimws(paste(t$location, t$nickname))
  }
  if (name == "") name <- paste("Team", tid)
  
  teams[[tid]] <- name
  
  pf <- as.numeric(t$points)
  if (!is.null(pf) && length(pf) > 0 && pf > pf_leader_pts) {
    pf_leader_pts <- pf
    pf_leader_name <- name
  }
}

# 2. Loop through completed matchups to find the single-week high score
high_week_name <- "None"
high_week_pts <- 0
high_week_num <- 0
current_week <- if (!is.null(data$scoringPeriodId)) data$scoringPeriodId else 1

for (m in data$schedule) {
  week <- if (!is.null(m$matchupPeriodId)) m$matchupPeriodId else 0
  if (week > current_week) next
  
  for (side in c("home", "away")) {
    team_data <- m[[side]]
    if (is.null(team_data)) next
    
    pts <- as.numeric(team_data$totalPoints)
    if (!is.null(pts) && length(pts) > 0 && pts > high_week_pts) {
      high_week_pts <- pts
      tid <- as.character(team_data$teamId)
      high_week_name <- if (!is.null(teams[[tid]])) teams[[tid]] else paste("Team", tid)
      high_week_num <- week
    }
  }
}

# 3. Check All-Time League History (start with 2026's high score as the baseline)
all_time_name <- high_week_name
all_time_pts <- high_week_pts
all_time_week <- high_week_num
all_time_year <- season

history_url <- paste0(
  "https://lm-api-reads.fantasy.",
  "espn",
  ".com/apis/v3/games/ffl/leagueHistory/",
  league_id,
  "?view=mTeam&view=mMatchup"
)

# Safely fetch past seasons (handles cases where league has no prior history)
history_data <- tryCatch(fromJSON(history_url, simplifyVector = FALSE), error = function(e) NULL)

if (!is.null(history_data)) {
  for (past_season in history_data) {
    yr <- past_season$seasonId
    past_teams <- list()
    
    for (t in past_season$teams) {
      tid <- as.character(t$id)
      name <- if (!is.null(t$name) && t$name != "") {
        t$name
      } else {
        trimws(paste(t$location, t$nickname))
      }
      if (name == "") name <- paste("Team", tid)
      past_teams[[tid]] <- name
    }
    
    for (m in past_season$schedule) {
      week <- if (!is.null(m$matchupPeriodId)) m$matchupPeriodId else 0
      for (side in c("home", "away")) {
        team_data <- m[[side]]
        if (is.null(team_data)) next
        
        pts <- as.numeric(team_data$totalPoints)
        if (!is.null(pts) && length(pts) > 0 && pts > all_time_pts) {
          all_time_pts <- pts
          tid <- as.character(team_data$teamId)
          all_time_name <- if (!is.null(past_teams[[tid]])) past_teams[[tid]] else paste("Team", tid)
          all_time_week <- week
          all_time_year <- yr
        }
      }
    }
  }
}

# 4. Save all three results as Shields.io endpoint JSON files
dir.create("stats", showWarnings = FALSE)

write_json(list(
  schemaVersion = 1,
  label = "Points For Leader",
  message = sprintf("%s (%.2f)", pf_leader_name, pf_leader_pts),
  color = "brightgreen"
), "stats/pf_leader.json", auto_unbox = TRUE)

write_json(list(
  schemaVersion = 1,
  label = "2026 Weekly High",
  message = sprintf("%s - %.2f (Wk %d)", high_week_name, high_week_pts, high_week_num),
  color = "orange"
), "stats/high_score.json", auto_unbox = TRUE)

write_json(list(
  schemaVersion = 1,
  label = "All-Time Weekly High",
  message = sprintf("%s - %.2f (Wk %d, %d)", all_time_name, all_time_pts, all_time_week, all_time_year),
  color = "purple"
), "stats/all_time_high.json", auto_unbox = TRUE)
