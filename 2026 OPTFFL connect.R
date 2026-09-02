
### Fonts ####
library(tidyverse)
library(purrr)
library(ggimage)
library(showtext)
library(gganimate)

font_add(family = "FranklinGothic", regular = "framd.ttf")
font_add(family = "ComicSans", regular = "comic.ttf")
font_add(family = "PalatinoLinotype", regular = "pala.ttf")
font_add(family = "Trebuchet", regular = "trebuc.ttf")
showtext_auto()


######

# https://github.com/k5cents/fflr
# install.packages("remotes")
# remotes::install_github("k5cents/fflr")

scoring_week <- 1

library(fflr)
library(nflverse)
library(tidyverse)
library(gt)
library(ggimage)

fflr::ffl_id(leagueId = 710908445)
ffl_id(leagueId = "710908445")
#league_info()
#league_teams()
#team_roster()
         
fflr::league_members(seasonId = 2026)
logo <- league_teams(seasonId = 2026) %>% 
  select(2:4)

all_rost <- team_roster(scoringPeriodId = scoring_week,seasonId = 2026)

fflr::ffl_api(seasonId = 2026)

rosters <- map(all_rost,as_tibble)
roster_data <- map_dfr(all_rost,~.x)

record_data <- fflr::league_standings(seasonId = 2026) %>% 
  group_by(abbrev) %>% 
  top_n(1,scoringPeriodId) %>% 
  mutate(record = paste(wins,losses, sep = "-")) %>% 
  select(4,18)


# plot scoring
total_scores <- roster_data %>% 
  filter(lineupSlot != "BE") %>% 
  inner_join(.,logo) %>% 
  inner_join(.,record_data) %>% 
  group_by(abbrev) %>%
  mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
  select(abbrev,logo,totalScore,record) %>% 
  distinct()

# total_score_plot <- ggplot(total_scores,aes(x=reorder(abbrev,totalScore),y =totalScore))+
#   geom_col(fill = "darkgreen")+
#   geom_text(aes(label = record), position = position_stack(vjust = .85), color = "#f3f088", size = 5,family = "Trebuchet", fontface = "bold")+
#   coord_flip()+
#   geom_image(aes(image = logo),
#              size = 0.09,position = position_stack(vjust = 1))+
#   labs(title = "Total Points Scored",y="Points")+
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),axis.title.y = element_blank(),
#         panel.background = element_rect(fill = "#f3f088", color = NA),
#         plot.background = element_rect(fill = "#f3f088", color = NA),
#         panel.grid.major.y = element_line(color = "#f3f088"),
#         panel.grid.minor.y = element_line(color = "#f3f088"),
#         panel.grid.major.x = element_line(color = "#eec56f",linewidth = 2,linetype = 2),
#         panel.grid.minor.x = element_line(color = "#eec56f",linewidth = 2,linetype = 2),
#         text = element_text(family = "Trebuchet",face = "bold"),
#         plot.title = element_text(hjust = 0.5,size = 24),
#         axis.title.x = element_text(size = 20,family = "Trebuchet",face = "bold"),
#         axis.text.x = element_text(size = 15))
# 
# total_score_plot

##  Similar as above, but points should be made up by Position

lineSlotScores <- roster_data %>% 
  filter(lineupSlot != "BE") %>% 
  inner_join(.,logo) %>% 
  inner_join(.,record_data) %>% 
  group_by(abbrev) %>% 
  mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(abbrev,lineupSlot) %>% 
  mutate(Pos_Points = sum(actualScore)) %>% 
  select(4,5,13,18:20) %>% 
  distinct() %>% 
  ungroup()

lineSlotScores$lineupSlot <- factor(lineSlotScores$lineupSlot,
                                    levels = c("K","D/ST","FLEX","TE","WR","RB","QB"))

slot_colors <- c("K" = "#ff0073", "D/ST" = "darkcyan", "FLEX" = "#be0000", 
                 "TE" = "darkgoldenrod", "WR" = "darkgrey", "RB" = "darkblue", "QB" = "darkgreen")

# points_by_post <- ggplot(lineSlotScores, aes(x = reorder(abbrev,totalScore), y = actualScore, fill = lineupSlot)) +
#   geom_col() +
#   scale_fill_manual(values = slot_colors)+
#   coord_flip() +
#   geom_image(aes(x = abbrev, y = -10, image = logo), size = 0.085) +  # Adjust y to place logos below the bars
#   scale_y_continuous(expand = expansion(mult = c(0.07, 0.07))) +  # Adjust the y-axis to make space for logos
#   labs(title = "Total Points by Lineup Slot",
#        x = "Team",
#        y = "Total Points",
#        fill = "Line-up Slot") +
#   guides(fill = guide_legend(reverse = TRUE))+
#   theme_minimal() +
#   theme(axis.text.y = element_blank(),  # Remove x-axis text.. remember the coord flip
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         panel.background = element_rect(fill = "#f3f088", color = NA),
#         plot.background = element_rect(fill = "#f3f088", color = NA),
#         panel.grid.major.y = element_line(color = "#f3f088"),
#         panel.grid.minor.y = element_line(color = "#f3f088"),
#         panel.grid.major.x = element_line(color = "#eec56f",linewidth = 2,linetype = 2),
#         panel.grid.minor.x = element_line(color = "#eec56f",linewidth = 2,linetype = 2),
#         text = element_text(family = "Trebuchet",face = "bold"),
#         plot.title = element_text(hjust = 0.5,size = 24),
#         axis.title.x = element_text(size = 20,family = "Trebuchet",face = "bold"),
#         axis.text.x = element_text(size = 15),
#         legend.title = element_text(face = "bold"))



#1f77b4 (Blue)
#ff7f0e (Orange)
#2ca02c (Green)
#d62728 (Red)
#9467bd (Purple)
#8c564b (Brown)
#e377c2 (Pink)


## Total Running Score


w1_roster <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 1) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
  filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
  select(2,4,17)
w2_roster <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 2) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
  filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>%
  select(2,4,17)
w3_roster <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 3) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
   filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
   select(2,4,17)
 w4_roster <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 4) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
   filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
   select(2,4,17)
 w5_roster <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 5) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
   filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
   select(2,4,17)
w6_roster <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 6) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
  filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>%
  select(2,4,17)
w7_roster <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 7) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
  filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>%
  select(2,4,17)
# w8_roster <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 8) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
#   select(2,4,17)
# w9_roster <- fflr::team_rosterseasonId = 2026,scoringPeriodId = 9) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
#   select(2,4,17)
# w10_roster <- fflr::team_roster(scoringPeriodId = 10) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
#   select(2,4,17)
# w11_roster <- fflr::team_roster(scoringPeriodId = 11) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
#   select(2,4,17)
# w12_roster <- fflr::team_roster(scoringPeriodId = 12) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
#   select(2,4,17)
# w13_roster <- fflr::team_roster(scoringPeriodId = 13) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
#   select(2,4,17)
# w14_roster <- fflr::team_roster(scoringPeriodId = 14) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE") %>% group_by(abbrev) %>% mutate(totalScore = sum(actualScore,na.rm = TRUE)) %>% 
#   select(2,4,17)

d <- list(w1_roster) %>% 
  reduce(full_join) %>% distinct() %>% inner_join(.,logo)

d_col <- d %>% summarise(all_score = sum(totalScore,na.rm = T)) %>% inner_join(.,logo)

all_week_scores_bar_chart <- ggplot(d_col,aes(x=reorder(abbrev,all_score),y =all_score))+
  geom_col(fill = "darkgreen")+
  coord_flip()+
  geom_image(aes(image = logo),
             size = 0.09,position = position_stack(vjust = 1))+
  labs(title = "Total Points Scored",y="Points")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),axis.title.y = element_blank(),
        panel.background = element_rect(fill = "#fafafa", color = NA),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        panel.grid.major.y = element_line(color = "#fafafa"),
        panel.grid.minor.y = element_line(color = "#fafafa"),
        panel.grid.major.x = element_line(color = "#eec56f",linewidth = 2,linetype = 2),
        panel.grid.minor.x = element_line(color = "#eec56f",linewidth = 2,linetype = 2),
        text = element_text(family = "Trebuchet",face = "bold"),
        plot.title = element_text(hjust = 0.5,size = 24),
        axis.title.x = element_text(size = 20,family = "Trebuchet",face = "bold"),
        axis.text.x = element_text(size = 15))


OPTFFL_team_colors <- c("KAY" = "#909090",
                        "BTW" = "#c9a01d",
                        "CW" = "#fec95f",
                        "KAS" = "#842020",
                        "BWb" = "#1b8842",
                        "JOHN" = "#faaf7d",
                        "NGT" = "#c27514",
                        "OPE" = "#cc5d3f",
                        "CNP" = "#129b51",
                        "EET" = "#bfe3ce",
                        "JOSh" = "#2d2e2c",
                        "TCC" = "orange",
                        "LLL" = "#0077B5",
                        "NNT" = "#9e89c0",
                        "GPG" = "#fff200",
                        "YARN" = "#ed2246"
)



### Cumulative Sum #### 


cumulative_score <- d %>% group_by(abbrev) %>% mutate(cumulative_score = cumsum(totalScore))

cumulaitve_plot_line <- ggplot(cumulative_score,aes(x=scoringPeriodId,y =cumulative_score,color=abbrev))+
  scale_color_manual(values = OPTFFL_team_colors)+
  geom_line(linewidth = 2.5)+
  geom_point(size = 4)+
  # geom_image(aes(x = scoringPeriodId, y = cumulative_score,image = logo), size = 0.1,inherit.aes = FALSE)+
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 0.1))+
  theme_bw()+
  labs(x="Scoring Week",y="Cumulative Points",title = "Cumulative Points By Week")+
  theme(panel.background = element_rect(fill = "#fafafa", color = NA),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        panel.grid.major.y = element_line(color = "#fafafa"),
        panel.grid.minor.y = element_line(color = "#fafafa"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Trebuchet",face = "bold"),
        plot.title = element_text(hjust = 0.5,size = 24),
        axis.title.x = element_text(size = 20,family = "Trebuchet",face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 20,family = "Trebuchet",face = "bold"),
        axis.text.y = element_text(size = 15))




library(plotly)
library(htmlwidgets)
ggplotly(cumulaitve_plot_line)
saveWidget(ggplotly(cumulaitve_plot_line),file = "cumulative_line_plot.html")



ggplot(d,aes(x=abbrev,y=totalScore))+geom_boxplot(aes(size = totalScore))




### what if we did it by position and filled it by team?

#Biggest_Bench
# roster_data %>% 
#   filter(lineupSlot == "BE") %>% 
#   arrange(desc(actualScore)) %>% view()
# 
# roster_data %>% 
#   filter(lineupSlot == "BE") %>% 
#   summarise(total_points = sum(actualScore,na.rm = T))
# 
# 
# roster_data %>% 
#   filter(lineupSlot != "BE") %>% 
#   mutate(diff = projectedScore - actualScore) %>% 
#   select(4,5,8,6,12,13,17) %>% 
#   arrange(desc(diff)) %>% view()

# Ranking Table

standings_table <- league_standings(seasonId = 2026) %>% 
  inner_join(.,logo) %>%
  mutate(record = paste(wins,losses, sep = "-")) %>% 
  select("logo","playoffSeed","record") %>% 
  arrange(playoffSeed) %>% 
  gt() %>%
  text_transform(
    locations = cells_body(columns = c(logo)),
    fn = function(x) {
      web_image(url = x, height = 50)
    }
  ) %>%
  cols_label(
    playoffSeed = "Playoff Seed",
    logo = "",
    record = "Record"
  ) %>% 
  tab_style(
    style = cell_text(size = px(40)),  # Adjust the size as needed
    locations = cells_body(columns = c(playoffSeed, record))
  ) %>% 
  cols_align(
    align = "center",
    columns = c(playoffSeed)
  ) %>% 
  tab_options(
    table.background.color = "#fafafa"
  ) %>% 
  tab_style(
    style = cell_text(font = "Trebuchet", weight = "bold", size = px(20)),
    locations = cells_column_labels(columns = c(playoffSeed, record))
  )

gtsave(standings_table,"w7.png")

standings_table %>%  gtsave("StandingsRankings.png",expand=10)

#####

ggplot(d, aes(x = totalScore, y = abbrev, fill = scoringPeriodId)) +
  geom_point()+
  geom_line(aes(group = abbrev),color = "#e7e")

"#e7e7e7"
"#436685"
"#bf2f24"


ggplot(d, aes(x = totalScore, y = abbrev, fill = scoringPeriodId))+
  geom_violin()+
  geom_jitter(height = 0,width = 0.1)+
  coord_flip()

ggplot(d_col,aes(x = reorder(abbrev,+all_score),y= all_score))+
  geom_bar(stat = "identity",fill=alpha("darkgreen"))+
  ylim(-500,1000)+
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4),"cm"),
    plot.background = element_rect(fill = "#fafafa", color = NA)
    
  )+
  coord_polar()+
  geom_image(aes(image = logo),
             size = 0.08,position = position_stack(vjust = 1.26))
#####
# szn_stat_2024 <- nflreadr::load_player_stats(seasons = most_recent_season(),stat_type = "offense")
# rushing <- nflreadr::load_nextgen_stats(seasons = most_recent_season(),stat_type = "rushing")
# recing <- nflreadr::load_nextgen_stats(seasons = most_recent_season(),stat_type = "receiving")
# 
# 
# colnames(load_pbp(seasons = most_recent_season()))
# 
# pbp_2024 <- load_pbp(seasons = 2024)
# 
# m <- load_ff_opportunity(seasons = 2024,stat_type = "weekly")