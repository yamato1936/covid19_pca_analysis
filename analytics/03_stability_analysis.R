library(tidyverse)
library(Rtsne)
library(ggplot2)

df <- read_csv("owid-covid-data.csv")

# 年ごとに変動係数を計算
df_cv <- df %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year %in% c(2020, 2021, 2022)) %>%
  group_by(location, year) %>%
  summarise(
    cases_mean = mean(new_cases_per_million, na.rm = TRUE),
    cases_sd = sd(new_cases_per_million, na.rm = TRUE),
    deaths_mean = mean(new_deaths_per_million, na.rm = TRUE),
    deaths_sd = sd(new_deaths_per_million, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cv_cases = cases_sd / cases_mean,
    cv_deaths = deaths_sd / deaths_mean
  ) %>%
  filter(is.finite(cv_cases), is.finite(cv_deaths)) %>%
  group_by(location) %>%
  summarise(
    mean_cv_cases = mean(cv_cases, na.rm = TRUE),
    mean_cv_deaths = mean(cv_deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  column_to_rownames("location")

# t-SNEで2次元に圧縮
tsne_result <- Rtsne(as.matrix(df_cv), perplexity = 30)
tsne_df <- as.data.frame(tsne_result$Y)
tsne_df$location <- rownames(df_cv)

ggplot(tsne_df, aes(x = V1, y = V2)) +
  geom_point(alpha = 0.7) +
  labs(title = "変動係数（CV）に基づく各国の分布", x = "t-SNE1", y = "t-SNE2") +
  theme_minimal()
