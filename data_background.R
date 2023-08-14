library(dplyr)
library(ggplot2)

# 解析対象データ全体の概要を算出####
data_background <- cleansed_utstein_data %>%
  count(年, 性別, 年齢, 全身機能カテゴリー) %>%
  group_by(年) %>%
  mutate(全体数 = sum(n)) %>%
  group_by(年, 性別, 年齢, 全身機能カテゴリー) %>%
  mutate(総年齢1 = 年齢 * n) %>%
  group_by(年, 性別) %>%
  mutate(総年齢2 = sum(総年齢1)) %>%
  mutate(総数 = sum(n)) %>%
  mutate(平均年齢 = 総年齢2 / 総数) %>%
  mutate(男女割合 = 総数 / 全体数) %>%
  mutate(年齢の標準偏差 = sd(年齢)) %>%
  mutate(生存の有無 = if_else(全身機能カテゴリー >= 1 & 全身機能カテゴリー <= 4, TRUE, FALSE)) %>%
  ungroup() %>%
  count(年, 性別,  総数, 男女割合, 平均年齢, 年齢の標準偏差) %>%
  dplyr::select(年, 性別, 総数, 男女割合, 平均年齢, 年齢の標準偏差)