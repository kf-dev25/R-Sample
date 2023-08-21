library(dplyr)
library(ggplot2)

# 薬剤投与実施率####
implementation_rate_epinephrine <- cleansed_utstein_data %>%
  count(波形種別, 薬剤投与) %>%
  group_by(波形種別, 薬剤投与) %>%
  mutate(総数 = sum(n)) %>%
  group_by(波形種別) %>%
  mutate(波形別総数 = sum(n)) %>%
  mutate(実施率 = if_else(薬剤投与 == 0, 0, round(総数 / 波形別総数 * 100, digits = 2)))

# 薬剤投与生存率####
survival_rate_epinephrine <- cleansed_utstein_data %>%
  count(波形種別, 薬剤投与, 生存の有無, 機能予後, 心拍再開の有無) %>%
  group_by(波形種別, 薬剤投与) %>%
  mutate(総数 = sum(n)) %>%
  group_by(波形種別, 薬剤投与, 心拍再開の有無) %>%
  mutate(心拍再開数 = sum(n)) %>%
  mutate(心拍再開率 = if_else(心拍再開の有無 == FALSE, 0, round(心拍再開数 / 総数 * 100, digits = 2))) %>%
  group_by(波形種別, 薬剤投与, 生存の有無) %>%
  mutate(生存数 = sum(n)) %>%
  mutate(生存率 = if_else(生存の有無 == FALSE, 0, round(生存数 / 総数 * 100, digits = 2))) %>%
  group_by(波形種別, 薬剤投与, 機能予後) %>%
  mutate(機能予後良好数 = sum(n)) %>%
  mutate(機能予後率 = if_else(機能予後 == FALSE, 0, round(機能予後良好数 / 総数 * 100, digits = 2))) %>%
  ungroup() %>%
  filter(心拍再開の有無) %>%
  filter(生存の有無) %>%
  filter(機能予後) %>%
  mutate(波形種別 = ifelse(波形種別 == 3, 2, 波形種別)) %>%
  mutate(波形種別 = ifelse(波形種別 == 4, 3, 波形種別)) %>%
  dplyr::select(
    "波形種別", "総数", "薬剤投与", "心拍再開数", "心拍再開率", "生存数", "生存率", "機能予後良好数", "機能予後率"
  )

# 薬剤投与生存率グラフ####
labeli <- as_labeller(c("0" = "薬剤投与なし", "1" = "薬剤投与あり"))
ggplot(survival_rate_epinephrine, aes(x = 波形種別, y = 心拍再開率, fill = as.factor(薬剤投与))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(text = element_text(size = 15)) +
  geom_text(aes(label = 生存率), size = 5, angle = -90, position = position_dodge(width = 0.9)) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("VF", "PEA", "心静止")) +
  scale_fill_brewer(palette = "Set2", labels = c("薬剤投与なし", "薬剤投与あり")) +
  scale_y_continuous(breaks = seq(0, 35, 5), limits = c(0, 35)) +
  # theme(legend.position = 'none')+
  labs(fill = "薬剤投与")

# 薬剤投与実施率グラフ####
implementation_rate_epinephrine$波形種別 <- as.factor(implementation_rate_epinephrine$波形種別)
ggplot(implementation_rate_epinephrine, aes(x = 波形種別, y = 実施率, fill = as.factor(波形種別))) +
  geom_bar(stat = "identity") +
  theme(text = element_text(size = 15)) +
  scale_fill_brewer(palette = "Set2", labels = c("VF", "PEA", "心静止")) +
  labs(fill = "波形種別")
