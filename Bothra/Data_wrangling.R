library(tidyverse)
library(circular)
library(gt)
library(readr)

wwc_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/wwc_passes.csv")
dim(wwc_passes)
colnames(wwc_passes)

wwc_passes <- wwc_passes |> 
  mutate(age_grp = case_when(minute <= 22 ~ "0-22", 
                             minute > 22 & minute <= 45 ~ "23-45", 
                             minute > 45 & minute <= 70 ~ "46-70", 
                             minute > 70 ~ "69+"))

wwc_passes |> 
  distinct(team.name) |> 
  count()

wwc_passes |> 
  filter(!is.na(under_pressure)) |> 
  count()

wwc_passes |> 
  group_by(position.name) |> 
  summarise(
    median_pos = median(location.x)
  )

wwc_passes_UP <- wwc_passes |> 
  filter(!is.na(under_pressure))

wwc_passes_UP |> 
  group_by(location.x) |> 
  summarise(
    avg_pass_angle = mean(pass.angle)
  )

wwc_passes_UP <- wwc_passes_UP |> 
  mutate(pass_angle_degrees = pass.angle * (180/pi))

wwc_passes <- wwc_passes|> 
  mutate(pass_angle_degrees = pass.angle * (180/pi))


#Success rate for long passes under pressure
wwc_passes_UP |> 
  filter(abs(location.x - pass.end_location.x) >= 30) |> 
  group_by(team.name) |> 
  summarise(
    avg_pass_angle = mean(pass_angle_degrees),
    pass_success_rate = mean(+is.na(pass.outcome.name))
  ) |> 
  arrange(-pass_success_rate) |> 
  print(n=32) 


#passes backwards vs forwards UNDER PRESSURE
wwc_passes_UP<- wwc_passes_UP |> 
  mutate(pass_direction = pass_angle_degrees <= 90 & pass_angle_degrees >= -90)

wwc_passes_UP |> 
  group_by(team.name) |> 
  summarise(
    #avg_pass_angle = mean(pass_angle_degrees),
    pass_direction_prop = mean(as.integer(pass_direction)),
    mean_angle = atan2(
      mean(sin(pass.angle), na.rm = TRUE),
      mean(cos(pass.angle), na.rm = TRUE)
    ) * 180 / pi
    #pass_success_rate = mean(+is.na(pass.outcome.name))
  ) |> 
  arrange(-pass_direction_prop) |> 
  gt()

#pass progress/ per pass
wwc_passes_UP |>
  mutate(
    x_progress = pass.end_location.x - location.x
  ) |>
  group_by(team.name) |>
  summarise(
    avg_progress = mean(x_progress)
  ) |>
  arrange(-avg_progress) |> 
  gt()


#passes backwards forwards NOT UNDER PRESSURE

wwc_passes<- wwc_passes |> 
  mutate(pass_direction = pass_angle_degrees <= 90 & pass_angle_degrees >= -90)


wwc_passes |> 
  group_by(team.name) |> 
  summarise(
    #avg_pass_angle = mean(pass_angle_degrees),
    pass_direction_prop = mean(as.integer(pass_direction)),
    mean_angle = atan2(
      mean(sin(pass.angle), na.rm = TRUE),
      mean(cos(pass.angle), na.rm = TRUE)
    ) * 180 / pi
    #pass_success_rate = mean(+is.na(pass.outcome.name))
  ) |> 
  arrange(-pass_direction_prop) |> 
  gt()




#PASS DIRECTION/ ANGLE / SUCCESS RATE/ PER PERIOD/ UNDER PRESSURE
wwc_passes_UP |> 
  filter(team.name == "Spain Women's") |> 
  group_by(period) |> 
  summarise(
    #avg_pass_angle = mean(pass_angle_degrees),
    pass_direction_prop = mean(as.integer(pass_direction)),
    mean_angle = atan2(
      mean(sin(pass.angle), na.rm = TRUE),
      mean(cos(pass.angle), na.rm = TRUE)
    ) * 180 / pi,
    pass_success_rate = mean(+is.na(pass.outcome.name))
  ) |> 
  arrange(period) |> 
  gt()


#write_csv(wwc_passes, "Bothra/wwc_passes.csv")
#write_csv(wwc_passes_UP, "Bothra/wwc_passes_UP.csv")
