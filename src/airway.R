library(dplyr)
library(ggplot2)

# 特定行為器具ごとの実施率####
implementation_rate_airway <- cleansed_utstein_data %>%
  count(波形種別, 特定行為器具使用, 特定行為器具種別) %>%
  group_by(波形種別, 特定行為器具種別) %>%
  mutate(総数 = sum(n)) %>%
  group_by(波形種別) %>%
  mutate(波形総数 = sum(n)) %>%
  mutate(実施率 = if_else(特定行為器具使用 == 0, 0, round(総数 / 波形総数 * 100, digits = 2))) %>%
  filter(!特定行為器具種別 == 0)

# 特定行為器具ごとの生存率####
survival_rate_airway <- cleansed_utstein_data %>%
  count(波形種別, 特定行為器具使用, 特定行為器具種別, 生存の有無, 機能予後, 心拍再開の有無) %>%
  group_by(波形種別, 特定行為器具使用, 特定行為器具種別) %>%
  mutate(総数 = sum(n)) %>%
  group_by(波形種別, 特定行為器具使用, 特定行為器具種別, 心拍再開の有無) %>%
  mutate(心拍再開数 = sum(n)) %>%
  mutate(心拍再開率 = if_else(心拍再開の有無 == FALSE, 0, round(心拍再開数 / 総数 * 100, digits = 2))) %>%
  group_by(波形種別, 特定行為器具使用, 特定行為器具種別, 生存の有無) %>%
  mutate(生存数 = sum(n)) %>%
  mutate(生存率 = if_else(生存の有無 == FALSE, 0, round(生存数 / 総数 * 100, digits = 2))) %>%
  group_by(波形種別, 特定行為器具使用, 特定行為器具種別, 機能予後) %>%
  mutate(機能予後良好数 = sum(n)) %>%
  mutate(機能予後率 = if_else(機能予後 == FALSE, 0, round(機能予後良好数 / 総数 * 100, digits = 2))) %>%
  ungroup() %>%
  filter(心拍再開の有無) %>%
  filter(生存の有無) %>%
  filter(機能予後) %>%
  mutate(波形種別 = ifelse(波形種別 == 3, 2, 波形種別)) %>%
  mutate(波形種別 = ifelse(波形種別 == 4, 3, 波形種別)) %>%
  dplyr::select(
    "波形種別", "特定行為器具使用", "特定行為器具種別", "総数", "心拍再開数", "心拍再開率",
    "生存数", "生存率", "機能予後良好数", "機能予後率"
  )

ggplot(implementation_rate_airway, aes(x = 特定行為器具種別, y = 実施率, fill = as.factor(波形種別))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(text = element_text(size = 15)) +
  scale_fill_brewer(palette = "Set2", labels = c("VF", "PEA", "心静止")) +
  scale_x_continuous(breaks = c(0, 1, 2, 3), labels = c("器具使用なし", "LM", "食道閉鎖式エアウェイ", "器官チューブ")) +
  labs(fill = "波形種別")

ggplot(survival_rate_airway, aes(x = 波形種別, y = 生存率, fill = as.factor(特定行為器具種別))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(text = element_text(size = 15)) +
  scale_fill_brewer(palette = "Set2", labels = c("器具使用なし", "LM", "食道閉鎖式エアウェイ", "器官チューブ")) +
  geom_text(aes(label = 生存率), size = 5, angle = -90, position = position_dodge(width = 0.9)) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("VF", "PEA", "心静止")) +
  scale_y_continuous(breaks = seq(0, 40, 5), limits = c(0, 40)) +
  labs(fill = "特定行為器具種別")
