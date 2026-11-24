
### Fonts ####
library(tidyverse)
library(purrr)
library(ggimage)
library(showtext)
library(gganimate)
library(magick)
library(rsvg)
library(httr)

font_add(family = "FranklinGothic", regular = "framd.ttf")
font_add(family = "ComicSans", regular = "comic.ttf")
font_add(family = "PalatinoLinotype", regular = "pala.ttf")
font_add(family = "Trebuchet", regular = "trebuc.ttf")
showtext_auto()


######

# https://github.com/k5cents/fflr
# install.packages("remotes")
# remotes::install_github("k5cents/fflr")

library(fflr)
library(nflverse)
library(tidyverse)
library(gt)
library(gtExtras)
library(ggimage)

nfl_player_stats <- nflreadr::load_player_stats(seasons = most_recent_season(),summary_level = "reg")
nfl_player_ids <- load_ff_playerids() %>%
  select(gsis_id, espn_id, name, position, team) %>%
  filter(!is.na(gsis_id), !is.na(espn_id))

#in nfl_players_stats player_id = gsis_id in nfl_player_ids
# gsis_id to match the nflreadr data together
# then need espn_id from player_id to match to fflr data



player_data_with_ids <- nfl_player_ids %>%
  inner_join(nfl_player_stats, by = join_by(gsis_id == player_id)) %>%
  mutate(
    espn_id = as.integer(espn_id),
    half_ppr_score = (receptions * 0.5) + fantasy_points
  ) %>%
  select(gsis_id, espn_id, name, headshot_url,receptions, fantasy_points, half_ppr_score,position.x)

fflr::ffl_id(leagueId = 710908445)
ffl_id(leagueId = "710908445")
logo <- league_teams(seasonId = 2026) %>% 
  select(2:4)

ffl_id(leagueId = 710908445)
draft_results_2026 <- draft_recap(seasonId = 2026)

draft_with_stats <- draft_results_2026 %>%
  mutate(player_id = as.integer(playerId)) %>%
  left_join(player_data_with_ids, by = join_by(playerId == espn_id)) %>% 
  select(-c(3,5,8,11,16)) %>% 
  left_join(logo, by="abbrev") %>% 
  mutate(position = case_when(position.x == "PK" ~ "K",
                              TRUE ~ position.x))

# D/ST are not in the data set and appear as NA
# KAY pick 98 (Stribbling) has scored 0 points, so there is no data
# CW drafted two DST, so one was in the BE spot


ggplot(draft_with_stats, aes(x=pickId,y=half_ppr_score))+geom_point()
ggplot(draft_with_stats, aes(x=pickId,y=half_ppr_score,color = lineupSlot))+geom_point()
ggplot(draft_with_stats, aes(x=pickId,y=half_ppr_score,color = position))+geom_point()
ggplot(draft_with_stats,aes(x=factor(roundId),y=half_ppr_score))+geom_boxplot()
ggplot(draft_with_stats,aes(x=factor(roundId),y=half_ppr_score))+geom_col()



ggplot(draft_with_stats,aes(x=pickId,y=half_ppr_score))+geom_image(aes(image = logo))+
  labs(x="Draft Pick",y="Player Cumulative Points",title = "Points Scored By Draft Picks")+
  theme(panel.background = element_rect(fill = "#fafafa", color = NA),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.minor.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Trebuchet",face = "bold"),
        plot.title = element_text(hjust = 0.5,size = 24),
        axis.title.x = element_text(size = 20,family = "Trebuchet",face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 20,family = "Trebuchet",face = "bold"),
        axis.text.y = element_text(size = 15))



# Post-Game Draft Analysis
# How did the pick do compared to the rest of the round
#  Pick Round rank
#     Example: did the 4th pick score the most or least points in the round
#  Calculate average round pick rating.
# How did the pick do compared to the same position?
#   was the drafted Kicker 1 the kicker 1 in points?
#   Calculate the difference off from the actual
#       drafted - finish (wr4 drafted finsished WR80 = -76)
#   Calculate the average difference for each team at each position.
# Scale the values, then sum the scaled values into a composite score. Higher points should mean better draft
#


# Step 1 How did the pick do compared to others in the position.

## Set-up for data getting the drafted position - actual.
### pick_diff shows how far off the position ranking is from the draft pick was for that position
### Example: Gibbs was the 1st RB drafted, and is ranked RB1, so its 0. was picked at exact value for the position.
draftPosVfinishPos <- draft_with_stats %>% 
      group_by(position) %>% 
      mutate(pos_rank = rank(-half_ppr_score)) %>% 
      ungroup() %>% 
      group_by(position) %>% 
      mutate(pos_draft_rank = rank(pickId)) %>% 
      mutate(pick_diff = pos_draft_rank - pos_rank) %>% 
      ungroup()

## team average
### testing on a single team
draftPosVfinishPos %>% 
  filter(abbrev == "KAY") %>% 
  group_by(position) %>% 
  mutate(avg_diff = mean(pick_diff, .na.rm=TRUE)) %>% 
  view()

# plots for step 1

ggplot(draftPosVfinishPos %>% filter(!is.na(position)))+
  geom_segment( aes(y=name.x,x=pos_rank,
                    xend = pick_diff))+
  geom_point(aes(x=pos_rank,y=name.x))+
  geom_point(aes(x=pick_diff, y=name.x))+
  geom_vline(xintercept = 0)+
  facet_wrap(vars(abbrev),scales = "free_y")




draftPosVfinishPos %>%
  ungroup() %>% # Crucial if the dataframe was grouped earlier!
  filter(!is.na(position)) %>%
  mutate(
    diff_val = pick_diff - pos_rank,
    pickId_num = as.numeric(pickId)
  ) %>%
  arrange(desc(pickId_num)) %>% # Sort descending so lowest pickId ends up at the top
  mutate(name.x = fct_inorder(name.x)) %>% # Lock in the exact row order
  ggplot(aes(y = name.x)) +
  geom_segment(aes(x = pos_rank, xend = pick_diff, color = diff_val), linewidth = 1.2) +
  geom_point(aes(x = pos_rank)) +
  geom_point(aes(x = pick_diff)) +
  geom_vline(xintercept = 0) +
  scale_color_gradient2(
    name = "Difference",
    low = "red",
    mid = "yellow",
    high = "green"
  ) +
  facet_wrap(vars(abbrev), scales = "free_y")


draftPosVfinishPos %>% 
  select(pickId,name.x,position.x,headshot_url,logo,pos_draft_rank,pos_rank,pick_diff) %>% 
  relocate(logo,.after = pickId) %>% 
  relocate(headshot_url, .before = name.x) %>%
  gt() %>% 
  text_transform(
    locations = cells_body(columns = c(logo,headshot_url)),
    fn = function(x) {
      web_image(url = x, height = 50)
    }
  ) %>% 
  cols_label(pickId = "Pick Number",
             logo = "Team",
             headshot_url= " ",
             name.x = "Player Name",
             position.x = "Position",
             pos_draft_rank = "Positional Drafted Rank",
             pos_rank = "Positional Ranking",
             pick_diff = "Rank Differnce from Drafted"
             
             )


