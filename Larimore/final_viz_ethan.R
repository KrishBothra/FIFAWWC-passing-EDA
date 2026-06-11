library(tidyverse)
library(StatsBombR)
library(ggplot2)

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

# head(wwc_passes)

colnames(wwc_passes)
length(wwc_passes$player.name)
length(wwc_passes$pass.recipient.name)

# Position data ===============================================================
# x is length of field, y is side to side
# angle is based on y 
max(wwc_passes$location.x)
min(wwc_passes$location.x)

max(wwc_passes$location.y)
min(wwc_passes$location.y)

# how often does each team appear? (pass count)
wwc_passes |>
  count(team.name) |>
  arrange(desc(n))

# most successful passing teams?
pass_completion_data <- wwc_passes |>
  group_by(team.name) |>
  summarize(completion_perc = sum(successful_pass) / n(),
            n = n()) |>
  arrange(desc(completion_perc))

# START HERE: long pass EDA ====================================================
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
    ) * 180 / pi
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

# future improvement: figure out pass outcome being incomplete but aerial still one
aerial_success <- wwc_passes |>
  filter(pass.height.name == "High Pass") |>
  group_by(team.name) |>
  summarize(
    aerial_perc = sum(win_arial) / n(),
    n = n()
  )

# pass region near box, yields many shot attempts
prop_box_passes <- wwc_passes |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |> 
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(pass.end_location.x >= 90 & 
           pass.end_location.x <= 115 &
           pass.end_location.y <= 60 & 
           pass.end_location.y >= 20) |>
  #filter(successful_pass == 1) |>
  group_by(team.name) |>
  summarize(
    boxpass_succ_rate = sum(successful_pass) / n(),
    n_box = n(),
    n_box_shots = sum(shot_assist),
    shot_efficiency = sum(goal_assist) / n_box_shots,
    shots_per_boxpass = sum(shot_assist) / n(),
    avg_pass_posstime = mean(TimeInPoss),
    avg_timetopossend = mean(TimeToPossEnd)
  ) |>
  left_join(WWC_2023_rankings_v3 |> select(ranking, team.name, games_played, wins), 
            join_by(team.name)) |>
  mutate(shots_per_game = n_box_shots / games_played,
         boxpasses_per_game = n_box / games_played) |>
  # look at teams with a good number of box passes
  filter(n_box >= 30)

# negative is good! lower number --> better ranking
cor(prop_box_passes$avg_pass_posstime, prop_box_passes$ranking, 
    method = "spearman", use = "complete.obs")
# ranking of all columns
prop_box_passes |>
  select(-team.name) |>
  cor(use = "complete.obs") |>
  as_tibble(rownames = "variable") |>
  select(variable, wins) |>
  arrange(wins)

data_list <- list(long_data, press_long_data, nopress_long_data, 
                  long_pass_top_position, long_pass_dist, WWC_2023_rankings_v3)
library(purrr)
long_data_all <- reduce(data_list, left_join, by = "team.name") |>
  mutate(
    # positive means they're better at passing NOT under pressure
    delta_press_perc = nopress_long_perc - press_long_perc
  )

long_data_all |>
  select(where(is.numeric)) |>
  cor(use = "pairwise.complete.obs") |>
  as_tibble(rownames = "variable") |>
  select(variable, wins) |>
  arrange(wins)

long_data_all |>
  group_by(position.name) |>
  summarize(
    avg_rank = mean(ranking),
    n()
  )



# netherlands pass VISUALIZATION

library(ggsoccer)
long_passes_team <- wwc_passes |>
  filter(pass.length >= 25 & (pass.end_location.x - location.x) >= 10) |>
  filter(team.name == "Italy Women's") |>
  #filter(pass.height.name == "High Pass") |>
  filter(is.na(pass.type.name) | pass.type.name != "Throw-in") |>
  filter(is.na(pass.outcome.name) | pass.outcome.name != "Out") 

# bucketing posstime
summary(long_passes_team$TimeInPoss)


# prop_box_passes |>
#   filter(team.name == "Italy Women's") |>
#   ggplot() +
#   annotate_pitch(dimensions = pitch_statsbomb) +
#   geom_segment(
#     aes(x = location.x, y = location.y, 
#         xend = pass.end_location.x, yend = pass.end_location.y,
#         color = successful_pass),
#     arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
#     alpha = 0.7
#   ) +
#   scale_y_reverse() +
#   theme(legend.position = "bottom")

# heatmaps of long pass end locations?
# ALL PASS HEATMAP
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
  geom_point(aes(x = pass.end_location.x, y = pass.end_location.y, 
                 color = is_pressure), alpha = 0.7, size = 2) +
  scale_color_manual(
    values = c("1" = "#CD212A", "0" = "#008C45")
  ) +
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

# JUST successful passes
succ_pass <- long_passes_team |>
  filter(successful_pass == 1) |>
  mutate(is_pressure = factor(is_pressure, levels = c(0, 1), 
                              labels = c("No Pressure", "Under Pressure"))) |>
  ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_density2d_filled(aes(pass.end_location.x, pass.end_location.y, 
                            fill=after_stat(level)), 
                        alpha = 0.7,
                        contour_var='ndensity') +
  scale_fill_manual(values = colorRampPalette(c("white", "#008C45"))(10),
                    guide = "none")  +
  geom_point(aes(x = pass.end_location.x, y = pass.end_location.y, 
                 color = TimeInPoss), alpha = 0.7, size = 2) + # playing with TIMEINPOSS gradient
  # scale_color_manual(
  #   name = "Pressure",
  #   values = c("No Pressure" = "#21468B", "Under Pressure" = "#CD212A"),
  #   guide = guide_legend(override.aes = list(
  #     shape    = 16, 
  #     size     = 3, 
  #     alpha    = 1,
  #     fill     = NA, # transparent key background
  #     linetype = 0 # remove border box
  #   ))
  # ) +
  scale_y_reverse() +
  theme_pitch() +
  labs(
    title = "Successful Long Passes by Italy Women's",
    subtitle = "2023 Women's World Cup",
    caption = "Data: StatsBomb"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 0, size = 12),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(family = "serif", color = "#21468B")
  )

succ_pass

# UNSUCCESSFUL PAsSEs
nosucc_pass <- long_passes_team |>
  filter(successful_pass == 0) |>
  mutate(is_pressure = factor(is_pressure, levels = c(0, 1), 
                              labels = c("No Pressure", "Under Pressure"))) |>
  ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_density2d_filled(aes(pass.end_location.x, pass.end_location.y, 
                            fill=after_stat(level)), 
                        alpha = 0.7,
                        contour_var='ndensity') +
  scale_fill_manual(values = colorRampPalette(c("white", "black"))(10),
                    guide = "none")  +
  geom_point(aes(x = pass.end_location.x, y = pass.end_location.y, 
                 color = is_pressure), alpha = 0.7, size = 2) +
  scale_color_manual(
    name = "Pressure",
    values = c("No Pressure" = "#21468B", "Under Pressure" = "#CD212A"),
    guide = guide_legend(override.aes = list(
      shape    = 16, 
      size     = 3, 
      alpha    = 1,
      fill     = NA, # transparent key background
      linetype = 0 # remove border box
    ))
  ) +
  scale_y_reverse() +
  theme_pitch() +
  labs(
    title = "Unsuccessful Long Passes by Italy Women's",
    subtitle = "2023 Women's World Cup",
    caption = "Data: StatsBomb"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 0, size = 12),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(family = "serif", color = "#21468B")
  )

nosucc_pass
library(patchwork)
succ_pass / nosucc_pass

