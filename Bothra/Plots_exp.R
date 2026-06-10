library(circular)
library(CircStats)
library(sportyR)
library(patchwork)
library(gt)
library(tidyverse)

country1 <- "Colombia Women's"
country2 <- "Morocco Women's"

wwc_passes <- read_csv("Bothra/wwc_passes.csv")
wwc_passes_UP <- read_csv("Bothra/wwc_passes_UP.csv")
wwc_passes_NUP <- read_csv("Bothra/wwc_passes_NUP.csv")
wwc_2023_rankings <- read_csv("Bothra/WWC_2023_rankings_v3.csv")

#boxplot- SUCCESSFUL PASSES BASED ON ANGLE - UNDERPRESSURE
wwc_passes_UP |>
  filter(team.name == "Netherlands Women's") |>
  mutate(success = is.na(pass.outcome.name)) |>
  ggplot(aes(x = success, y = pass_angle_degrees, fill = success)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("TRUE" = "green", "FALSE" = "red"),
    labels = c("TRUE" = "Successful", "FALSE" = "Unsuccessful")
  ) +
  labs(
    x = "Pass Outcome",
    y = "Pass Angle (degrees)"
  )



#density plot - SUCCESSFUL PASSES BASED ON ANGLE - UNDERPRESSURE

wwc_passes_UP |>
  filter(team.name %in% c(country1,
                          country2)) |>
  mutate(success = is.na(pass.outcome.name)) |>
  ggplot(
    aes(
      x = pass_angle_degrees,
      fill = success
    )
  ) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ age_grp + team.name) +
  scale_fill_manual(
    values = c("TRUE" = "green",
               "FALSE" = "red"),
    labels = c("TRUE" = "Successful",
               "FALSE" = "Unsuccessful")
  ) +
  labs(
    x = "Pass Angle (degrees)",
    y = "Density",
    fill = "Pass Outcome"
  )


soccer_pitch <- geom_soccer('fifa')

wwc_passes_NUP |>
  filter(team.name %in% c(country1,
                          country2)) |>
  mutate(success = !is.na(pass.outcome.name)) |>
  
  ggplot(
    aes(
      x = pass_angle_degrees,
      fill = success
    )
  ) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10) +
  scale_x_continuous(
    breaks = c(0, 90, 180, -90),
    labels = c("0°", "90°", "180°", "-90°")
  ) +
  coord_polar(start = -pi/2) +
  facet_wrap(~team.name)


rose_1 <- wwc_passes_UP |>
  filter(team.name == country1) |>
  mutate(success = !is.na(pass.outcome.name)) |>
  ggplot(aes(x = pass_angle_degrees, fill = success)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10) +
  scale_x_continuous(
    breaks = c(0, 90, 180, -90),
    labels = c("0°", "90°", "180°", "-90°")
  ) +
  coord_polar(start = -pi/2) +
  #facet_wrap(~team.name) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = "transparent", colour = NA),
    strip.text = element_text(colour = "white"),
    legend.position = 'none'
  )

rose_2 <- wwc_passes_UP |>
  filter(team.name == country2) |>
  mutate(success = !is.na(pass.outcome.name)) |>
  ggplot(aes(x = pass_angle_degrees, fill = success)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10) +
  scale_x_continuous(
    breaks = c(0, 90, 180, -90),
    labels = c("0°", "90°", "180°", "-90°")
  ) +
  coord_polar(start = -pi/2) +
  #facet_wrap(~team.name) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = "transparent", colour = NA),
    strip.text = element_text(colour = "white"),
    legend.position = 'none'
  )

rose_1
rose_2

soccer_pitch + inset_element(rose_1, left = 0.1, bottom = 0.1, right = 0.9, top = 0.9) 

soccer_pitch + inset_element(rose_2, left = 0.1, bottom = 0.1, right = 0.9, top = 0.9) 


compare_angle_overlay <- wwc_passes_UP |>
  filter(team.name %in% c(country1, country2)) |>
  mutate(success = !is.na(pass.outcome.name)) |>
  ggplot(aes(x = pass_angle_degrees, fill = team.name)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10, alpha = 0.8, position = "identity") +
  coord_polar(start = -pi/2) +
  theme_void()

soccer_pitch + inset_element(compare_angle_overlay, left = 0, bottom = 0, right = 1.21, top = 1) 


((((((((((((((((((((((((((((((((((((((((((((((((((((()))))))))))))))))))))))))))))))))))))))))))))))))))))

teams <- unique(wwc_passes_UP$team.name)

#NOT UNDER PRESSURE  ***************************************************************************

results <- combn(teams, 2, simplify = FALSE) |>
  map_dfr(function(pair) {
    x <- circular(wwc_passes_NUP |> filter(team.name == pair[1]) |> pull(pass_angle_degrees), units = "degrees")
    y <- circular(wwc_passes_NUP |> filter(team.name == pair[2]) |> pull(pass_angle_degrees), units = "degrees")
    test <- watson.two.test(x, y)
    tibble(
      team_1 = pair[1],
      team_2 = pair[2],
      statistic = round(test$statistic, 4),
      significant = test$statistic > 0.187  # threshold for p < 0.05
    )
  })

# number of times diff
all_teams <- tibble(team.name = unique(c(results$team_1, results$team_2)))

results |> 
  filter(significant == TRUE) |> 
  pivot_longer(cols = c(team_1, team_2), values_to = "team.name") |>
  count(team.name, sort = TRUE) |> 
  right_join(all_teams, by = "team.name") |>
  mutate(n = replace_na(n, 0)) |>
  left_join(wwc_2023_rankings, by = 'team.name') |> 
  arrange( -n, -ranking) |>
  gt()

# Every significant result
results |>
  filter(significant == TRUE) |>
  gt() |>
  tab_header(title = "Watson Test: Pass Angle Distributions by Team Pair") |>
  cols_label(
    team_1 = "Team 1",
    team_2 = "Team 2",
    statistic = "Watson Statistic",
    significant = "Significant (p < 0.05)"
  )

#UNDER PRESSSSSURE ***************************************************************************

results_UP <- combn(teams, 2, simplify = FALSE) |>
  map_dfr(function(pair) {
    x <- circular(wwc_passes_UP|> filter(team.name == pair[1]) |> pull(pass_angle_degrees), units = "degrees")
    y <- circular(wwc_passes_UP |> filter(team.name == pair[2]) |> pull(pass_angle_degrees), units = "degrees")
    test <- watson.two.test(x, y)
    tibble(
      team_1 = pair[1],
      team_2 = pair[2],
      statistic = round(test$statistic, 4),
      significant = test$statistic > 0.187  # threshold for p < 0.05
    )
  })

# number of times diff
all_teams <- tibble(team.name = unique(c(results$team_1, results$team_2)))

results_UP |> 
  filter(significant == TRUE) |> 
  pivot_longer(cols = c(team_1, team_2), values_to = "team.name") |>
  count(team.name, sort = TRUE) |> 
  right_join(all_teams, by = "team.name") |>
  mutate(n = replace_na(n, 0)) |>
  left_join(wwc_2023_rankings, by = 'team.name') |> 
  arrange( -n, -ranking) |>
  # ggplot(aes(x = team.name, y = n)) + 
  # geom_point(aes(fill = ranking))
  gt()




# Every significant result
results_UP |>
  filter(significant == TRUE) |>
  arrange(-statistic) |> 
  gt() |>
  tab_header(title = "Watson Test: Pass Angle Distributions by Team Pair") |>
  cols_label(
    team_1 = "Team 1",
    team_2 = "Team 2",
    statistic = "Watson Statistic",
    significant = "Significant (p < 0.05)"
  )


#UNDER PRESSSSSURE V NOT UNDER PRESSURE FOR EACH TEAM ***************************************************************************


teams <- unique(wwc_passes$team.name)

results_pressure <- teams |>
  map_dfr(function(team) {
    up <- circular(wwc_passes_UP |> filter(team.name == team & period == 1) |> pull(pass_angle_degrees), units = "degrees")
    nup <- circular(wwc_passes_NUP |> filter(team.name == team & period == 1) |> pull(pass_angle_degrees), units = "degrees")
    test <- watson.two.test(up, nup)
    tibble(
      team.name = team,
      statistic = round(test$statistic, 4),
      significant = test$statistic > 0.187
    )
  }) |>
  arrange(desc(statistic))

results_pressure |>
  left_join(wwc_2023_rankings, by = 'team.name') |> 
  arrange(-significant, ranking) |> 
  gt() |>
  tab_header(title = "Pass Identity Change Under Pressure by Team") |>
  cols_label(
    team.name = "Team",
    statistic = "Watson Statistic",
    significant = "Identity Changes Under Pressure"
  )


results_pressure |>
  left_join(wwc_2023_rankings, by = 'team.name') |> 
  left_join(avg_dist_all, by = 'team.name') |> 
  #filter(ranking >= 9) |> 
  group_by(significant) |> 
  summarise(
    avg_games_played = mean(games_played),
    avg_wins = mean(wins),
    avg_draws = mean(draws),
    avg_gd = mean(goal_diff),
    avg_dist = mean(avg_distance)
  ) 
  

(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))



#NOT UNDER PRESSURE  ***************************************************************************

#Option 1: Two-sample KS test (are the distributions different?)
eng_angles <- wwc_passes_NUP |> filter(team.name == country1) |> pull(pass_angle_degrees)
ned_angles <- wwc_passes_NUP |> filter(team.name == country2) |> pull(pass_angle_degrees)

#UNDER PRESSURE  ***************************************************************************

eng_angles <- wwc_passes_UP |> filter(team.name == country1) |> pull(pass_angle_degrees)
ned_angles <- wwc_passes_UP |> filter(team.name == country2) |> pull(pass_angle_degrees)

eng_circ <- circular(eng_angles, units = "degrees")
ned_circ <- circular(ned_angles, units = "degrees")

watson.two.test(eng_circ, ned_circ)


#ECDF PLOT
data.frame(
  angle = c(eng_angles, ned_angles),
  team = c(rep(country1, length(eng_angles)), rep(country2, length(ned_angles)))
) |>
  ggplot(aes(x = angle, colour = team)) +
  stat_ecdf() +
  labs(x = "Pass Angle (degrees)", y = "Cumulative Proportion") +
  scale_x_continuous(breaks = seq(-180, 180, 45))

#Density Plot
data.frame(angle = c(eng_angles, ned_angles),
           team = c(rep(country1, length(eng_angles)), rep(country2, length(ned_angles)))) |>
  ggplot(aes(x = angle, fill = team)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(-180, 180, 45)) 


#BOX PLOT
data.frame(angle = c(eng_angles, ned_angles),
           team = c(rep(country1, length(eng_angles)), rep(country2, length(ned_angles)))) |>
  ggplot(aes(x = team, y = angle, fill = team)) +
  geom_boxplot(width = 0.1)

#Average distance of all passes
avg_dist_all <- wwc_passes |> 
  group_by(team.name) |> 
  summarise(
    avg_distance = mean(distance)
  )
