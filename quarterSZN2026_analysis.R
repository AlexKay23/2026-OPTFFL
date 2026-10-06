# Packages and Fonts ####
library(tidyverse)
library(purrr)
library(ggimage)
library(showtext)


font_add(family = "FranklinGothic", regular = "framd.ttf")
font_add(family = "ComicSans", regular = "comic.ttf")
font_add(family = "PalatinoLinotype", regular = "pala.ttf")
font_add(family = "Trebuchet", regular = "trebuc.ttf")
showtext_auto()


library(fflr)
library(nflverse)
library(gt)
library(ggimage)
library(ggrepel)

fflr::ffl_id(leagueId = 710908445)


# Load Data ####

scoring_week <- 4

logo <- league_teams(seasonId = 2026) %>% 
  select(2:4)

all_rost <- team_roster(seasonId = 2026,scoringPeriodId = scoring_week)



rosters <- map(all_rost,as_tibble)
roster_data <- map_dfr(all_rost,~.x)


record_data <- fflr::league_standings(seasonId = 2026) %>% 
  group_by(abbrev) %>% 
  top_n(1,scoringPeriodId) %>% 
  mutate(record = paste(wins,losses, sep = "-")) %>% 
  select(4,18)


## Weekly Scoring Load ####
w1_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 1) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
  filter(lineupSlot != "BE")
w2_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 2) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
  filter(lineupSlot != "BE") 
 w3_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 3) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
   filter(lineupSlot != "BE")
w4_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 4) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
   filter(lineupSlot != "BE")
# w5_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 5) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
#   filter(lineupSlot != "BE")
# w6_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 6) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
#   filter(lineupSlot != "BE")
# w7_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 7) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w8_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 8) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w9_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 9) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w10_roster_Qszn <- fflr::team_roster(scoringPeriodId = 10) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w11_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 11) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w12_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 12) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w13_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 13) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w14_roster_Qszn <- fflr::team_roster(seasonId = 2026,scoringPeriodId = 14) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")

## Weekly Scores Join

qzn_data <- list(w1_roster_Qszn,
     w2_roster_Qszn,
     w3_roster_Qszn,
     w4_roster_Qszn) %>% 
  reduce(full_join) %>% distinct() %>% inner_join(.,logo)


# Position Points in Flex Spont ####

## Count of position occurances in the FLEX Spot
qzn_data %>% 
  filter(lineupSlot == "FLEX") %>% 
  count(position) %>% 
  view()

## plot out points of FLEX

ggplot(qzn_data %>% filter(lineupSlot == "FLEX") %>% inner_join(., record_data),
       aes(x=position,y=actualScore,color=record))+geom_point()+
  facet_wrap(~record)

ggplot(qzn_data %>% filter(lineupSlot == "FLEX"),
       aes(x=position,y=actualScore))+geom_boxplot()

ggplot(qzn_data %>% filter(lineupSlot == "FLEX") %>% inner_join(., record_data),
       aes(x=position,y=actualScore))+geom_boxplot()+
  facet_wrap(~record)



ggplot(qzn_data %>% filter(lineupSlot == "FLEX") %>% inner_join(., record_data),
       aes(x=position,y=actualScore,color=position))+geom_point()+
  facet_wrap(~abbrev)



# Margin of Victory ####


fflr::league_standings() %>% 
  group_by(abbrev)

margin_victory <- fflr::tidy_scores() %>% 
  group_by(matchupPeriodId, matchupId) %>% 
  mutate(abs_diff = abs(totalPoints - lag(totalPoints))) %>%
  filter(matchupPeriodId <= 2) %>% 
  ungroup() %>% 
  group_by(matchupPeriodId) %>% 
  mutate(avg_margin = mean(abs_diff,na.rm=TRUE)) %>% 
  ungroup()


mean(margin_victory$abs_diff,na.rm = TRUE)


ggplot(margin_victory, aes(x=expectedWins,y=abs_diff,colour = abbrev))+geom_point()+
  facet_wrap(~isWinner)





 

# JJC Award #####
# Most RB Points
# 
# qzn_data %>% 
#   filter(position == "RB") %>% 
#   group_by(playerId) %>% 
#   mutate(rb_points = sum(actualScore,na.rm = TRUE)) %>% 
#   arrange(desc(rb_points))


qzn_data %>%
  filter(position == "RB") %>%
  group_by(playerId, firstName, lastName, abbrev) %>%  # include abbrev here
  summarize(rb_points = sum(actualScore, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(rb_points)) %>%
  ggplot(aes(x = rb_points,
             y = reorder(paste0(firstName, lastName), rb_points),
             fill = abbrev)) +  # now abbrev is available
  geom_col() +
  labs(x = "RB Points", y = "Player Name", title = "RB Points by Player", fill = "Team") +
  theme_minimal()



qzn_data %>% 
  filter(position == "RB") %>% 
  group_by(abbrev) %>% 
  mutate(rb_points = sum(actualScore, na.rm = TRUE)) %>% 
  arrange(desc(rb_points))

qzn_data %>% 
  filter(position =="RB") %>% 
  group_by(abbrev) %>% 
  summarise(rb_points =sum(actualScore, na.rm = TRUE), .groups = "drop") %>% 
  arrange(desc(rb_points)) %>% 
  ggplot(aes(x=reorder(abbrev,rb_points),y=rb_points))+
  geom_col()+
  labs(x = "Team", y = "RB Points", title = "RB Points by Team") +
  theme_minimal()


qzn_data %>%
  filter(position == "WR") %>%
  group_by(abbrev) %>%
  summarize(wr_points = sum(actualScore, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(wr_points)) %>%
  ggplot(aes(x = reorder(abbrev, wr_points), y = wr_points)) +
  geom_col() +
  labs(x = "Team", y = "WR Points", title = "WR Points by Team") +
  theme_minimal()




# Point Distribution ####

library(ggridges)

weekly_scores <- qzn_data %>% 
  filter(lineupSlot != "BE") %>% 
  group_by(scoringPeriodId,abbrev) %>% 
  mutate(weeklyScore = sum(actualScore,na.rm = TRUE)) %>% 
  mutate(division = case_when(
    abbrev == "KAY" ~ "Oreo",
    abbrev == "BTW" ~ "Pringle",
    abbrev == "CW" ~ "Pringle",
    abbrev == "KAS" ~ "Pringle",
    abbrev == "BWb" ~ "Oreo",
    abbrev == "YARN" ~ "Pringle",
    abbrev == "JOHN" ~ "Oreo",
    abbrev == "OPE" ~ "Pringle",
    abbrev == "CNP" ~ "Pringle",
    abbrev == "NGT" ~ "Oreo",
    abbrev == "JOSh" ~ "Oreo",
    abbrev == "TCC" ~ "Pringle",
    abbrev == "LLL" ~ "Oreo",
    abbrev == "NNT" ~ "Pringle",
    abbrev == "GPG" ~ "Oreo",
    abbrev == "PAPC" ~ "Oreo"
    
    
  ))

ggplot(weekly_scores, aes(x=weeklyScore,fill = abbrev))+
  geom_density()

ggplot(weekly_scores, aes(x=weeklyScore,y=abbrev,group = abbrev,fill = abbrev))+
  geom_density_ridges()+
  facet_wrap(~division,scales = "free_y",nrow = 1)

OPTFFL_team_colors <- c("KAY" = "#909090",
                        "BTW" = "#c9a01d",
                        "CW" = "#fec95f",
                        "KAS" = "#842020",
                        "BWb" = "#1b8842",
                        "JOHN" = "#faaf7d",
                        "NGT" = "#c27514",
                        "OPE" = "#cc5d3f",
                        "CNP" = "#129b51",
                        "PAPC" = "#bfe3ce",
                        "JOSh" = "#2d2e2c",
                        "TCC" = "orange",
                        "LLL" = "#0077B5",
                        "NNT" = "#9e89c0",
                        "GPG" = "#fff200",
                        "YARN" = "#ed2246"
)

weekly_ridge_score_plot <- ggplot(weekly_scores, aes(x = weeklyScore, y = abbrev, group = abbrev, fill = abbrev)) +
  geom_density_ridges() +
  facet_wrap(~division, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = OPTFFL_team_colors)+
  theme_bw()+
  labs(title = "Weekly Scores Density Plot",x="Weekly Score")+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#fafafa", color = NA),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        strip.background = element_rect(fill = "#fafafa", color = NA),
        strip.text.x.top = element_text(size=18),
        text = element_text(family = "Trebuchet",face = "bold"),
        plot.title = element_text(hjust = 0.5,size = 24),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(weekly_scores, aes(x=weeklyScore,y=abbrev,fill = abbrev))+
  geom_boxplot(fill="darkgreen")+
  geom_point()+
  facet_wrap(~division,scales = "free_y")

standings_data <- fflr::league_standings(seasonId = 2026) %>% 
  inner_join(.,logo) %>% 
  mutate(record = paste(wins,losses, sep = "-"))


pts_for_vs_against_plot <- ggplot(standings_data,
       aes(x=pointsFor,y=pointsAgainst))+
  geom_point()+
  geom_image(aes(image = logo),
             size = .15)+
  # geom_text(label = standings_data$record,
  #           nudge_y = 18,
  #           nudge_x = 9,
  #           size = 5,fontface = "bold")+
  labs(title = "Points For vs Points Against",
       x = "Points For",
       y = "Points Against")+
  geom_vline(xintercept = mean(standings_data$pointsFor),color="darkgreen",linewidth=1.5,alpha=0.4)+
  geom_hline(yintercept = mean(standings_data$pointsAgainst),color="darkgreen",linewidth=1.5,alpha=0.4)+
  # annotate("text",y = mean(standings_data$pointsAgainst)+5,x=425,label="avg pts against")+
  # annotate("text",x=mean(standings_data$pointsFor)-5,y=265,label="avg pts For",angle=90)+
  annotate("text",x= 345,y=345,label = "Lucky or Bad?",size=5,fontface="bold")+
  annotate("text",x= 475, y=350, label = "Easy Schedule",size=5,fontface="bold")+
  annotate("text",x= 470, y= 470, label= "Battle Tested",size=5,fontface="bold")+
  annotate("text",x= 345,y=470,label="Tough Schedule",size=5,fontface="bold")+
  theme_bw()+
  theme(text = element_text(family = "Trebuchet",face = "bold"),
        plot.title = element_text(hjust = 0.5,size = 24),
        axis.title.x = element_text(size = 20,family = "Trebuchet",face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 20,family = "Trebuchet",face = "bold"),
        axis.text.y = element_text(size = 15),
        panel.background = element_rect(fill = "#fafafa", color = NA),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



### Projected vs Actual

projected_vs_actuals <- qzn_data %>% 
  filter(lineupSlot != "IR") %>%  # remove IR spot from lineup
  group_by(scoringPeriodId,teamId) %>%  #group by week and team
  mutate(sum_projected = sum(projectedScore)) %>%  # calculates the teams projected score for the week
  mutate(sum_actual = sum(actualScore)) %>%   # calculates the teams actual points for the week
  select(scoringPeriodId,teamId,abbrev,logo,sum_projected,sum_actual) %>% 
  distinct() %>% #removes duplicate rows
  ungroup() %>% # ungroup to start fresh
  mutate(pct_projected_pts = sum_actual/sum_projected) %>% 
  mutate(projectVsActual = sum_actual - sum_projected) %>% 
  mutate(leauge_projVsActual = mean(projectVsActual,na.rm=TRUE)) %>% 
  mutate(sd = sd(projectVsActual,na.rm = TRUE)) %>% 
  mutate(lower_limit = leauge_projVsActual - (2 * sd),
         upper_limit = leauge_projVsActual + (2* sd)) %>% 
  mutate(result_check = case_when(projectVsActual < lower_limit ~ "Bust",
                                  projectVsActual > upper_limit ~ "Boom",
                                  TRUE ~ "In range")) %>%  #interesting note, as of 10/2, CWs W1 -40 not a bust!!!
  group_by(teamId) %>% #group by team to calculate mean projected difference below
  mutate(team_avg_outcome_diff = mean(projectVsActual,na.rm=TRUE)) %>% 
  mutate(team_avg_pct_pts = mean(pct_projected_pts, na.rm=TRUE)) %>% 
  mutate(team_avg_projected_pts = mean(sum_projected, na.rm=TRUE)) %>% 
  ungroup() %>% 
  left_join(., select(standings_data,teamId,playoffSeed))
  

leauge_projVsActual_line_Value <- projected_vs_actuals %>% pull(leauge_projVsActual) %>% mean(na.rm = TRUE)



avg_proj_points_diff_plot<-ggplot(projected_vs_actuals %>% select(abbrev,team_avg_outcome_diff,team_avg_projected_pts) %>% distinct() %>% mutate(logo_path = paste0("team_logos/",abbrev,".png")), 
       aes(x=reorder(abbrev, -team_avg_projected_pts),y=team_avg_outcome_diff,fill = abbrev))+
  geom_col()+
  scale_fill_manual(values = OPTFFL_team_colors)+
  geom_image(aes(image = logo_path),
             size = 0.09,position = position_stack(vjust = 1))+
  geom_hline(yintercept = leauge_projVsActual_line_Value,linewidth = 1.5,colour = "blue",linetype = 2)+
  geom_text(aes(x="YARN",y= leauge_projVsActual_line_Value -1.5 ,label = paste0("Leauge Average ",round(leauge_projVsActual_line_Value,2))),
            family = "Trebuchet",fontface = "bold",size = 4.5,check_overlap = TRUE)+
  xlab("Teams Ordered by Average Projected Points")+
  ylab("Average Projected - Actual Point Differental")+
  theme(
    panel.background = element_rect(fill = "#fafafa", color = NA),
    plot.background = element_rect(fill = "#fafafa", color = NA),
    text = element_text(family = "Trebuchet",face = "bold"),panel.grid.major.y = element_line(color = "#fafafa"),
    panel.grid.minor.y = element_line(color = "#fafafa"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )


standings_proj_points_diff_plot<-ggplot(projected_vs_actuals %>% select(abbrev,team_avg_outcome_diff,playoffSeed) %>% distinct() %>% mutate(logo_path = paste0("team_logos/",abbrev,".png")), 
                                  aes(x=reorder(abbrev, playoffSeed),y=team_avg_outcome_diff,fill = abbrev))+
  geom_col()+
  scale_fill_manual(values = OPTFFL_team_colors)+
  geom_image(aes(image = logo_path),
             size = 0.09,position = position_stack(vjust = 1))+
  geom_hline(yintercept = leauge_projVsActual_line_Value,linewidth = 1.5,colour = "blue",linetype = 2)+
  geom_text(aes(x="NNT",y= leauge_projVsActual_line_Value +1.5 ,label = paste0("Leauge Average ",round(leauge_projVsActual_line_Value,2))),
            family = "Trebuchet",fontface = "bold",size = 4.5,check_overlap = TRUE)+
  xlab("Teams Ordered by Playoff seed")+
  ylab("Average Projected - Actual Point Differental")+
  theme(
    panel.background = element_rect(fill = "#fafafa", color = NA),
    plot.background = element_rect(fill = "#fafafa", color = NA),
    text = element_text(family = "Trebuchet",face = "bold"),panel.grid.major.y = element_line(color = "#fafafa"),
    panel.grid.minor.y = element_line(color = "#fafafa"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

today <- lubridate::today()
ggsave(
  filename = paste0("Weekly Ridge Plot ", today, ".png"),
  plot = weekly_ridge_score_plot,
  path = "Recaps/QZN Plots")
ggsave(
  filename = paste0("Points for vs against Plot ", today, ".png"),
  plot = pts_for_vs_against_plot,
  path = "Recaps/QZN Plots")
ggsave(
  filename = paste0("Average Points Diff Plot Projections ", today, ".png"),
  plot = avg_proj_points_diff_plot,
  path = "Recaps/QZN Plots")
ggsave(
  filename = paste0("Average Points Diff Plot Standings ", today, ".png"),
  plot = standings_proj_points_diff_plot,
  path = "Recaps/QZN Plots")



