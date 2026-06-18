library(tidyverse)
library(StatsBombR)
library(ggplot2)
library(gt)

wwc_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/wwc_passes.csv")

wwc_passes <- wwc_passes |>
  mutate(pass_distance = sqrt(((pass.end_location.x - location.x)^2) + 
                                ((pass.end_location.y - location.y)^2)),
         successful_pass = as.numeric(ifelse(is.na(pass.outcome.name), TRUE, FALSE)),
         shot_assist = as.numeric(ifelse(is.na(pass.shot_assist), FALSE, TRUE)),
         goal_assist = as.numeric(ifelse(is.na(pass.goal_assist), FALSE, TRUE)),
         is_pressure = as.numeric(ifelse(is.na(under_pressure), FALSE, TRUE)),
         pass_angle_degrees = pass.angle * (180/pi),
         win_arial = as.numeric(ifelse(is.na(pass.aerial_won), FALSE, TRUE))
  )

WWC_2023_rankings_v3 <- read_csv("Bothra/WWC_2023_rankings_v3.csv")

#==================================================================================
# intro data
wwc_passes |> head(6) |>
  select(period, team.name, player.name, position.name,
         location.x, location.y, pass.outcome.name, pass.angle) |>
  mutate(team.name = str_remove(team.name, "Women's"),
         pass.angle = round(pass.angle, 2)) |>
  mutate(pass.outcome.name = ifelse(is.na(pass.outcome.name), 
                                    "Complete",
                                    pass.outcome.name)) |>
  gt() |>
  cols_label(
    period = "Period",
    team.name = "Team",
    player.name = "Player",
    position.name = "Position",
    location.x = "X-loc",
    location.y = "Y-loc",
    pass.outcome.name = "Outcome",
    pass.angle = "Angle"
  )

head(wwc_passes)

colnames(wwc_passes)
length(wwc_passes$player.name)
length(wwc_passes$pass.recipient.name)

# long pass EDA ====================================================
long_data <- wwc_passes |>
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |>
  group_by(team.name) |>
  summarize(
    long_pass_perc = sum(successful_pass) / n(),
    n = n(),
    .groups = "drop"
  )

press_long_data <- wwc_passes |>
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |>
  filter(is_pressure == 1) |>
  group_by(team.name) |>
  summarize(
    press_long_perc = sum(successful_pass) / n(),
    press_passes = n()
  )

nopress_long_data <- wwc_passes |>
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |>
  filter(is_pressure == 0) |>
  group_by(team.name) |>
  summarize(
    nopress_long_perc = sum(successful_pass) / n(),
    nopress_passes = n()
  )

# average long-pass distance
long_pass_dist <- wwc_passes |>
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |>
  group_by(team.name) |>
  summarize(
    avg_pass_dist = mean(pass.length),
    avg_pass_angle = atan2(
      mean(sin(pass.angle), na.rm = TRUE),
      mean(cos(pass.angle), na.rm = TRUE)
    ) * 180 / pi,
    med_pass_dist = median(pass.length)
  )
# this shows a lot of the bad teams have longest pass distance 
# could it be due to their most common long passer being the goalie

long_pass_top_position <- wwc_passes |>
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |>
  group_by(team.name, position.name) |>
  summarize(
    pos_long_passes = n()
  ) |>
  filter(pos_long_passes >= max(pos_long_passes))

poss_time_data <- wwc_passes |>
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |>
  group_by(team.name) |>
  summarize(
    avg_posstime = mean(TimeInPoss)
  )

# future improvement: figure out pass outcome being incomplete but aerial still one
# include only the forward moving/offense-oriented positions
aerial_success <- wwc_passes |>
  filter(pass.height.name == "High Pass") |>
  #filter(str_detect(position.name, "Attacking | Wing | Forward")) |>
  group_by(team.name) |>
  summarize(
    aerial_perc = sum(win_arial) / n(),
    arials_won = sum(win_arial)
  ) 

# pass region near box, yields many shot attempts
prop_box_passes <- wwc_passes |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |> 
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(pass.end_location.x >= 90 & 
           pass.end_location.x <= 115 &
           pass.end_location.y <= 60 & 
           pass.end_location.y >= 20) |>
  group_by(team.name) |>
  summarize(
    boxpass_succ_rate = sum(successful_pass) / n(),
    n_box = n(),
    n_box_shots = sum(shot_assist),
    shot_efficiency = sum(goal_assist) / n_box_shots,
    shots_per_boxpass = sum(shot_assist) / n(),
    avg_boxpass_posstime = mean(TimeInPoss)
  ) |>
  left_join(WWC_2023_rankings_v3 |> select(ranking, team.name, games_played, wins), 
            join_by(team.name)) |>
  mutate(shots_per_game = n_box_shots / games_played,
         boxpasses_per_game = n_box / games_played) |>
  # look at teams with a good number of box passes
  filter(n_box >= 30)

# goalkeeper long pass percent of total
keeper_data <- wwc_passes |>
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |>
  mutate(keeper_binary = ifelse(position.name == "Goalkeeper", 1, 0)) |>
  group_by(team.name) |>
  summarize(
    keeper_long_perc = sum(keeper_binary) / n()
  ) |>
  arrange(desc(keeper_long_perc))

press_ratio_data <- wwc_passes |>
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |>
  group_by(team.name) |>
  summarize(
    press_pass_ratio = sum(is_pressure) / n(),
    long_pass_perc = sum(successful_pass) / n()
  )

# ranking of all columns
box_rank_data <- prop_box_passes |>
  left_join(WWC_2023_rankings_v3, join_by(team.name)) 

data_list <- list(long_data, press_long_data, nopress_long_data, 
                  long_pass_top_position, long_pass_dist, WWC_2023_rankings_v3, 
                  poss_time_data, aerial_success, keeper_data, box_rank_data)
library(purrr)
long_data_all <- reduce(data_list, left_join, by = "team.name") |>
  mutate(
    # positive means they're better at passing NOT under pressure
    delta_press_perc = nopress_long_perc - press_long_perc
  )

corr_data <- long_data_all |>
  select(where(is.numeric)) |>
  cor(use = "pairwise.complete.obs") |>
  as_tibble(rownames = "variable") |>
  select(variable, long_pass_perc) |>
  arrange(desc(long_pass_perc))

# we observe avg posstime and keeper long percentage have noticeably high correlations
View(corr_data)

# top vs bottom team comparisons
long_data_all <- long_data_all |>
  arrange(desc(long_pass_perc))

long_top5 <- long_data_all |> arrange(ranking) |> slice_head(n = 6) # b/c sweden's doubled

long_bottom5 <- long_data_all |> arrange(ranking) |>slice_tail(n = 5)

top_shots5 <- wwc_passes |>
  filter(team.name %in% long_top5$team.name) |>
  group_by(team.name) |>
  summarize(
    shots_total = sum(shot_assist)
  ) |>
  left_join(WWC_2023_rankings_v3, join_by(team.name)) |>
  mutate(
    shots_per_game = shots_total / games_played
  )
mean(top_shots5$shots_per_game)

bottom_shots5 <- wwc_passes |>
  filter(team.name %in% long_bottom5$team.name) |>
  group_by(team.name) |>
  summarize(
    shots_total = sum(shot_assist)
  ) |>
  left_join(WWC_2023_rankings_v3, join_by(team.name)) |>
  mutate(
    shots_per_game = shots_total / games_played
  )
mean(bottom_shots5$shots_per_game)

# pass VISUALIZATIONs =========================================================

library(ggsoccer)
long_passes_team <- wwc_passes |>
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |>
  filter(is.na(pass.outcome.name) | pass.outcome.name != "Out")

# EDA: ALL PASSES HEATMAP final locations split by success
long_passes_team |>
  mutate(is_pressure = as.factor(is_pressure)) |>
  ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_density2d_filled(aes(pass.end_location.x, pass.end_location.y, 
                            fill=after_stat(level)), 
                        alpha = 0.7,
                        contour_var='ndensity') +
  scale_fill_manual(values = colorRampPalette(c("white", "#21468B"))(10),
                    guide = "none") +
  # geom_point(aes(x = pass.end_location.x, y = pass.end_location.y, 
  #                color = is_pressure), alpha = 0.7, size = 2) +
  # scale_color_manual(
  #   values = c("1" = "#CD212A", "0" = "#008C45")
  # ) +
  scale_y_reverse() +
  facet_wrap(~successful_pass, ncol = 1,
             labeller = labeller(
               successful_pass = c("0" = "Unsuccessful Passes", "1" = "Successful Passes"))) +
  theme_pitch() +
  theme(
    #plot.background = element_rect(fill="#21468B"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold", family = "serif") 
  )


# initial pass location - all teams
long_passes_team |>
  mutate(is_pressure = as.factor(is_pressure)) |>
  ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_density2d_filled(aes(location.x, location.y, 
                            fill=after_stat(level)), 
                        alpha = 0.7,
                        contour_var='ndensity') +
  scale_fill_manual(values = colorRampPalette(c("white", "#21468B"))(10),
                    guide = "none") +
  # geom_point(aes(x = pass.end_location.x, y = pass.end_location.y, 
  #                color = is_pressure), alpha = 0.7, size = 2) +
  # scale_color_manual(
  #   values = c("1" = "#CD212A", "0" = "#008C45")
  # ) +
  scale_y_reverse() +
  facet_wrap(~successful_pass, ncol = 1,
             labeller = labeller(
               successful_pass = c("0" = "Unsuccessful Passes", "1" = "Successful Passes"))) +
  theme_pitch() +
  theme(
    #plot.background = element_rect(fill="#21468B"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold", family = "serif") 
  )

# JUST successful passes - end location
team_names <- unique(wwc_passes$team.name) # used to look at tournament-wide trends

top5_pass <- function(name_of_team) {long_passes_team |>
    filter(team.name %in% name_of_team) |>
    filter(successful_pass == 1) |>
    mutate(position_grouped = case_when(
      position.name %in% c("Goalkeeper", "Left Center Back", "Right Center Back",
                           "Left Back") ~ position.name,
      .default = "Other"
    )) |>
    ggplot() +
    annotate_pitch(dimensions = pitch_statsbomb) +
    geom_density2d_filled(aes(pass.end_location.x, pass.end_location.y, 
                              fill=after_stat(level)), 
                          alpha = 0.7,
                          contour_var='ndensity') +
    scale_fill_manual(values = colorRampPalette(c("white", "dodgerblue4"))(10),
                      guide = "none")  +
    # geom_point(aes(x = pass.end_location.x, y = pass.end_location.y,
    #                color = TimeInPoss), alpha = 0.7, size = 2) + # playing with TIMEINPOSS gradient
    # scale_color_gradient(
    #   name = "Poss Time",
    #   low = "#FFD700",
    #   high = "darkred"
    # ) +
    scale_y_reverse() + 
    theme_pitch() +
    coord_fixed(
      xlim = c(0, 120),
      ylim = c(0, 80),
      clip = "on"
    )
  # labs(
  #   title = name_of_team,
  #   subtitle = "2023 Women's World Cup",
  #   caption = "Data: StatsBomb"
  # ) +
  # theme(
  #   plot.title = element_text(hjust = 0.5, vjust = 0, size = 10),
  #   plot.subtitle = element_text(hjust = 0.5),
  #   text = element_text(family = "serif", color = "#21468B")
  # ) 
}

top5_pass(c(unique(long_top5$team.name)))

bottom5_pass <- function(name_of_team) {long_passes_team |>
    filter(team.name %in% name_of_team) |>
    filter(successful_pass == 1) |>
    mutate(position_grouped = case_when(
      position.name %in% c("Goalkeeper", "Left Center Back", "Right Center Back",
                           "Left Back") ~ position.name,
      .default = "Other"
    )) |>
    ggplot() +
    annotate_pitch(dimensions = pitch_statsbomb) +
    geom_density2d_filled(aes(pass.end_location.x, pass.end_location.y, 
                              fill=after_stat(level)), 
                          alpha = 0.7,
                          contour_var='ndensity') +
    scale_fill_manual(values = colorRampPalette(c("white", "indianred4"))(10),
                      guide = "none")  +
    # geom_point(aes(x = pass.end_location.x, y = pass.end_location.y,
    #                color = TimeInPoss), alpha = 0.7, size = 2) + # playing with TIMEINPOSS gradient
    # scale_color_gradient(
    #   name = "Poss Time",
    #   low = "#FFD700",
    #   high = "darkred"
    # ) +
    scale_y_reverse() + 
    theme_pitch() +
    coord_fixed(
      xlim = c(0, 120),
      ylim = c(0, 80),
      clip = "on"
    )
  # labs(
  #   title = name_of_team,
  #   subtitle = "2023 Women's World Cup",
  #   caption = "Data: StatsBomb"
  # ) +
  # theme(
  #   plot.title = element_text(hjust = 0.5, vjust = 0, size = 10),
  #   plot.subtitle = element_text(hjust = 0.5),
  #   text = element_text(family = "serif", color = "#21468B")
  # ) 
}

bottom5_pass(c(unique(long_bottom5$team.name)))


mean(long_top5$ranking)
mean(long_bottom5$ranking)

# FINAL TABLE visualization of 2 key metrics
top_summary <- long_passes_team |>
  filter(team.name %in% long_top5$team.name) |>
  summarize(
    `Success Rate` = mean(successful_pass),
    `Avg Pass Distance (yds)` = mean(pass.length),
    `Avg Time In Poss (s)` = mean(TimeInPoss),
    `Goalie Long Pass %` = mean(position.name == "Goalkeeper")
  )

bottom_summary <- long_passes_team |>
  filter(team.name %in% long_bottom5$team.name) |>
  summarize(
    `Success Rate` = mean(successful_pass),
    `Avg Pass Distance (yds) ` = mean(pass.length),
    `Avg Time In Poss (s)` = mean(TimeInPoss),
    `Goalie Long Pass %` = mean(position.name == "Goalkeeper")
  )

comparison_table <- tibble(
  Metric = names(top_summary),
  `Top 5 Teams` = unlist(top_summary),
  `Bottom 5 Teams` = unlist(bottom_summary)
)

comparison_table |>
  gt() |>
  fmt_number(decimals = 2) |>
  fmt_number(
    columns = c(`Top 5 Teams`, `Bottom 5 Teams`),
    rows = Metric == "Successful Passes",
    decimals = 0
  ) |>
  fmt_percent(
    columns = c(`Top 5 Teams`, `Bottom 5 Teams`),
    rows = Metric %in% c("Success Rate", "Goalie Long Pass %"),
    decimals = 1
  )

