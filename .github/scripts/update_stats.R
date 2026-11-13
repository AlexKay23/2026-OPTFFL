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

# 3. Save results as Shields.io endpoint JSON files
dir.create("stats", showWarnings = FALSE)

write_json(list(
  schemaVersion = 1,
  label = "Points For Leader",
  message = sprintf("%s (%.2f)", pf_leader_name, pf_leader_pts),
  color = "brightgreen"
), "stats/pf_leader.json", auto_unbox = TRUE)

write_json(list(
  schemaVersion = 1,
  label = "Weekly High Score",
  message = sprintf("%s - %.2f (Wk %d)", high_week_name, high_week_pts, high_week_num),
  color = "orange"
), "stats/high_score.json", auto_unbox = TRUE)
