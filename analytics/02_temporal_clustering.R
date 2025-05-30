library(tidyverse)
library(cluster)
library(factoextra)

df <- read_csv("owid-covid-data.csv")

# 年ごとに平均値と最大値を集計
df_filtered <- df %>%
  filter(!is.na(new_cases_per_million), !is.na(new_deaths_per_million)) %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year %in% c(2020, 2021, 2022)) %>%
  group_by(location, year) %>%
  summarise(
    mean_cases = mean(new_cases_per_million, na.rm = TRUE),
    max_cases = max(new_cases_per_million, na.rm = TRUE),
    mean_deaths = mean(new_deaths_per_million, na.rm = TRUE),
    max_deaths = max(new_deaths_per_million, na.rm = TRUE),
    .groups = "drop"
  )

# 年ごとにクラスタリングとプロット
for (yr in c(2020, 2021, 2022)) {
  df_year <- df_filtered %>% filter(year == yr) %>% column_to_rownames("location")
  df_scaled <- scale(df_year[, -1])  # year列を除く

  pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)
  pca_df <- as.data.frame(pca_result$x[, 1:2])
  clusters <- kmeans(df_scaled, centers = 4)$cluster
  pca_df$cluster <- as.factor(clusters)
  pca_df$location <- rownames(df_year)

  ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point() +
    labs(title = paste(yr, "年のクラスタリング結果"), x = "PC1", y = "PC2") +
    theme_minimal()
}
