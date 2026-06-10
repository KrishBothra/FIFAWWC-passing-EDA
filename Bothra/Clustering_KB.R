library(circular)
library(CircStats)
library(sportyR)
library(patchwork)
library(gt)
library(tidyverse)
library(factoextra)

wwc_passes <- read_csv("Bothra/wwc_passes.csv")
wwc_passes_UP <- read_csv("Bothra/wwc_passes_UP.csv")
wwc_passes_NUP <- read_csv("Bothra/wwc_passes_NUP.csv")
wwc_2023_rankings <- read_csv("Bothra/WWC_2023_rankings_v3.csv")

# store filtered data with both raw and scaled columns
wwc_NED <- wwc_passes_UP |> 
  filter(team.name == "Spain Women's") |>  # add & period == 1 if needed
  mutate(
    sin_pad = sin(pass.angle),
    cos_pad = cos(pass.angle),
    std_locationx = as.numeric(scale(location.x))
  )

# cluster on scaled features only
wwc_passes_UP_clean <- wwc_NED |> 
  select(sin_pad, cos_pad, std_locationx)

initial_dist <- wwc_passes_UP_clean |> dist(method = "euclidean")
wwc_complete  <- initial_dist |> hclust(method = "complete")

# silhouette
wwc_passes_UP_clean |> 
  fviz_nbclust(FUN = hcut, method = "silhouette")

# scatter plot
wwc_NED |> 
  mutate(
    cluster   = as.factor(cutree(wwc_complete, k = 10)),
    angle_deg = atan2(sin_pad, cos_pad) * 180 / pi
  ) |> 
  ggplot(aes(x = location.x, y = angle_deg, color = cluster)) +
  geom_point() +
  labs(x = "Field Position (x)", y = "Pass Angle (°)") +
  theme(legend.position = "bottom")

# summary table with actual location
wwc_NED |> 
  mutate(
    cluster   = as.factor(cutree(wwc_complete, k = 10)),
    angle_deg = atan2(sin_pad, cos_pad) * 180 / pi
  ) |> 
  group_by(cluster) |> 
  summarise(
    mean_angle     = mean(angle_deg),
    mean_locationx = mean(location.x),  # actual, not scaled
    n = n()
  ) |> 
  arrange(mean_locationx) |> 
  mutate(
    zone = case_when(
      mean_locationx < 40 ~ "Defensive Third",
      mean_locationx < 80 ~ "Middle Third",
      TRUE                ~ "Attacking Third"
    ),
    direction = case_when(
      abs(mean_angle) < 45  ~ "Forward",
      abs(mean_angle) > 135 ~ "Backward",
      mean_angle > 0        ~ "Left",
      TRUE                  ~ "Right"
    )
  ) |> 
  gt() |> 
  cols_label(
    cluster        = "Cluster",
    mean_angle     = "Mean Angle (°)",
    mean_locationx = "Field Position (x)",
    n              = "n",
    zone           = "Zone",
    direction      = "Direction"
  ) |> 
  fmt_number(columns = c(mean_angle, mean_locationx), decimals = 1) |> 
  data_color(
    columns = mean_locationx,
    colors  = scales::col_numeric(palette = c("#f7fbff", "#2171b5"), domain = c(0, 120))
  ) |> 
  data_color(
    columns = n,
    colors  = scales::col_numeric(palette = c("#fff7ec", "#d94701"), domain = NULL)
  ) |> 
  tab_header(
    title    = "Netherlands Women's Passing Clusters",
    subtitle = "Angle + Field Position"
  ) |> 
  tab_row_group(label = "Attacking Third", rows = zone == "Attacking Third") |> 
  tab_row_group(label = "Middle Third",    rows = zone == "Middle Third") |> 
  tab_row_group(label = "Defensive Third", rows = zone == "Defensive Third") |> 
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )