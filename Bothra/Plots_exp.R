
wwc_passes <- read_csv("Bothra/wwc_passes.csv")
wwc_passes_UP <- read_csv("Bothra/wwc_passes_UP.csv")
WWC_2023_rankings <- read_csv("Bothra/WWC_2023_rankings.csv")

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
  filter(team.name %in% c("Spain Women's",
                          "Vietnam Women's")) |>
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

library(sportyR)

soccer_pitch <- geom_soccer('fifa')

wwc_passes|>
  filter(team.name %in% c("England Women's",
                          "Spain Women's")) |>
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


rose_ENG <- wwc_passes_UP |>
  filter(team.name == "Colombia Women's") |>
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

rose_ESP <- wwc_passes_UP |>
  filter(team.name == "Netherlands Women's") |>
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

rose_ENG
rose_ESP

soccer_pitch + inset_element(rose_ENG, left = 0.1, bottom = 0.1, right = 0.9, top = 0.9) 

soccer_pitch + inset_element(rose_ESP, left = 0.1, bottom = 0.1, right = 0.9, top = 0.9) 


compare_angle_overlay <- wwc_passes_UP |>
  filter(team.name %in% c("Morocco Women's", "Netherlands Women's")) |>
  mutate(success = !is.na(pass.outcome.name)) |>
  ggplot(aes(x = pass_angle_degrees, fill = team.name)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10, alpha = 0.8, position = "identity") +
  coord_polar(start = -pi/2) +
  theme_void()

soccer_pitch + inset_element(compare_angle_overlay, left = 0.1, bottom = 0.1, right = 1.1, top = 0.9) 


((((((((((((((((((((((((((((((((((((((((((((((((((((()))))))))))))))))))))))))))))))))))))))))))))))))))))

teams <- unique(wwc_passes_UP$team.name)


results <- combn(teams, 2, simplify = FALSE) |>
  map_dfr(function(pair) {
    x <- wwc_passes_UP |> filter(team.name == pair[1]) |> pull(pass_angle_degrees)
    y <- wwc_passes_UP |> filter(team.name == pair[2]) |> pull(pass_angle_degrees)
    test <- ks.test(x, y)
    tibble(
      team_1 = pair[1],
      team_2 = pair[2],
      D = round(test$statistic, 4),
      p_value = round(test$p.value, 4)
    )
  }) |>
  arrange(p_value)

results |> 
  filter(p_value <= 0.05) |> 
  pivot_longer(cols = c(team_1, team_2), values_to = "team") |>
  count(team, sort = TRUE) |> 
  gt()

results |>
  filter(p_value <= 0.05 & (team_1 == "Netherlands Women's" | team_2 == "Netherlands Women's")) |>
  gt() |>
  tab_header(title = "KS Test: Pass Angle Distributions by Team Pair") |>
  cols_label(
    team_1 = "Team 1",
    team_2 = "Team 2",
    D = "D Statistic",
    p_value = "P-Value"
  )

write.csv(teams, "Bothra/teams.csv")



(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

# Empirical CDF comparison
#Option 1: Two-sample KS test (are the distributions different?)
eng_angles <- wwc_passes_UP |> filter(team.name == "Morocco Women's") |> pull(pass_angle_degrees)
ned_angles <- wwc_passes_UP |> filter(team.name == "Netherlands Women's") |> pull(pass_angle_degrees)

ks.test(eng_angles, ned_angles)


data.frame(
  angle = c(eng_angles, ned_angles),
  team = c(rep("Vietnam", length(eng_angles)), rep("Netherlands", length(ned_angles)))
) |>
  ggplot(aes(x = angle, colour = team)) +
  stat_ecdf() +
  labs(x = "Pass Angle (degrees)", y = "Cumulative Proportion") +
  scale_x_continuous(breaks = seq(-180, 180, 45))

data.frame(angle = c(eng_angles, ned_angles),
           team = c(rep("Vietnam", length(eng_angles)), rep("Netherlands", length(ned_angles)))) |>
  ggplot(aes(x = angle, fill = team)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(-180, 180, 45)) +
  geom_vline()

data.frame(angle = c(eng_angles, ned_angles),
           team = c(rep("Vietnam", length(eng_angles)), rep("Netherlands", length(ned_angles)))) |>
  ggplot(aes(x = team, y = angle, fill = team)) +
  geom_boxplot(width = 0.1)


