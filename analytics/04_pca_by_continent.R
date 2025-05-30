# 必要パッケージの読み込み
library(tidyverse)
library(ggfortify)

# 1. データ読み込み
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
covid <- readr::read_csv(url, show_col_types = FALSE)

# 2. 欠損ゼロかつ感染者数が多い日を抽出
valid_covid <- covid %>%
  filter(!is.na(total_cases)) %>%
  group_by(date) %>%
  summarize(mean_cases = mean(total_cases), na_count = sum(is.na(total_cases))) %>%
  filter(na_count == 0, mean_cases > 1000)

valid_dates <- valid_covid$date

# 3. 国別 × 日別で感染者数をwide形式に変換
covid_cleaned <- covid %>%
  filter(date %in% valid_dates, !is.na(total_cases)) %>%
  group_by(location, date) %>%
  summarize(total_cases = mean(total_cases), .groups = "drop")

pca_input <- covid_cleaned %>%
  pivot_wider(names_from = date, values_from = total_cases) %>%
  drop_na()

# 4. continent情報付加（あとで色分けに使う）
meta_info <- covid %>% select(location, continent) %>% distinct()
pca_input <- left_join(pca_input, meta_info, by = "location") %>%
  drop_na(continent)

# 5. PCAのための行列と前処理（log1p変換）
mat_input <- pca_input %>%
  select(-location, -continent) %>%
  as.matrix()
rownames(mat_input) <- pca_input$location

mat_log <- log1p(mat_input)

# 6. PCAの実行と可視化（大陸で色分け）
pca_log <- prcomp(mat_log, scale. = TRUE)

autoplot(pca_log, data = pca_input, colour = "continent") +
  ggtitle("PCA (log1p transformed)") +
  theme_minimal()
