library(circular)
library(CircStats)
library(sportyR)
library(patchwork)
library(gt)
library(tidyverse)
library(factoextra)
library(mclust)  

wwc_passes <- read_csv("Bothra/wwc_passes.csv")
wwc_passes_UP <- read_csv("Bothra/wwc_passes_UP.csv")
wwc_passes_NUP <- read_csv("Bothra/wwc_passes_NUP.csv")
wwc_2023_rankings <- read_csv("Bothra/WWC_2023_rankings_v3.csv")

# store filtered data with both raw and scaled columns
wwc_NED <- wwc_passes |>
  filter(team.name == "Netherlands Women's" ) |>
  mutate(
    sin_pad       = sin(pass.angle),
    cos_pad       = cos(pass.angle),
    std_locationx = as.numeric(scale(location.x)),
    std_distance = as.numeric(scale(distance))
  )

# cluster on scaled features only
wwc_passes_UP_clean <- wwc_NED |>
  select(sin_pad, cos_pad, std_locationx, std_distance)

gmm_fit <- Mclust(wwc_passes_UP_clean)

# BIC plot (equivalent to silhouette elbow for GMM)
plot(gmm_fit, what = "BIC")

# scatter plot
wwc_NED |>
  mutate(
    cluster   = as.factor(gmm_fit$classification),
    angle_deg = atan2(sin_pad, cos_pad) * 180 / pi
  ) |>
  ggplot(aes(x = location.x, y = angle_deg, color = cluster)) +
  geom_point() +
  labs(x = "Field Position (x)", y = "Pass Angle (°)") +
  theme(legend.position = "bottom")

# summary table
k_chosen <- gmm_fit$G  # number of components selected by BIC

gt_wwc_ned <- wwc_NED |>
  mutate(
    cluster   = as.factor(gmm_fit$classification),
    angle_deg = atan2(sin_pad, cos_pad) * 180 / pi
  ) |>
  group_by(cluster) |>
  summarise(
    mean_angle     = mean(angle_deg),
    mean_locationx = mean(location.x),
    mean_distance  = mean(distance),
    n              = n()
  ) |>
  arrange(mean_locationx) |>
  mutate(
    zone = case_when(
      mean_locationx < quantile(mean_locationx, 0.33) ~ "Defensive Zone",
      mean_locationx < quantile(mean_locationx, 0.67) ~ "Mid Zone",
      TRUE                                             ~ "Attacking Zone"
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
    mean_distance  = "Mean Distance (yd)",
    n              = "n",
    zone           = "Zone",
    direction      = "Direction"
  ) |>
  fmt_number(columns = c(mean_angle, mean_locationx, mean_distance), decimals = 1) |>
  data_color(
    columns = mean_locationx,
    colors  = scales::col_numeric(palette = c("#f7fbff", "#2171b5"), domain = c(0, 120))
  ) |>
  data_color(
    columns = n,
    colors  = scales::col_numeric(palette = c("#fff7ec", "#d94701"), domain = NULL)
  ) |>
  tab_header(
    title    = "Netherlands Women's Passing Clusters (GMM)",
    subtitle = "Angle + Field Position — Gaussian Mixture Model"
  ) |>
  tab_row_group(label = "Attacking Zone", rows = zone == "Attacking Zone") |>
  tab_row_group(label = "Mid Zone",       rows = zone == "Mid Zone")       |>
  tab_row_group(label = "Defensive Zone", rows = zone == "Defensive Zone") |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(gt_wwc_ned, "Bothra/Plots_Tables/Netherlands-Womens-Passing-Clusters-GMM.png")