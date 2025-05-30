library(tidyverse)

df <- read_csv("owid-covid-data.csv")

# 国単位での感染・死亡状況の平均と最大値を算出
summary_df <- df %>%
  filter(!is.na(total_cases_per_million), !is.na(total_deaths_per_million)) %>%
  group_by(location) %>%
  summarise(
    mean_cases = mean(total_cases_per_million, na.rm = TRUE),
    mean_deaths = mean(total_deaths_per_million, na.rm = TRUE),
    max_cases = max(total_cases_per_million, na.rm = TRUE),
    max_deaths = max(total_deaths_per_million, na.rm = TRUE)
  )

# 平均プロット
ggplot(summary_df, aes(x = mean_cases, y = mean_deaths)) +
  geom_point(alpha = 0.6) +
  labs(title = "平均感染者数 vs 平均死亡者数", x = "感染者数（百万人あたり）", y = "死亡者数（百万人あたり）") +
  theme_minimal()

# 最大値プロット
ggplot(summary_df, aes(x = max_cases, y = max_deaths)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(title = "最大感染者数 vs 最大死亡者数", x = "感染者数（最大）", y = "死亡者数（最大）") +
  theme_minimal()
