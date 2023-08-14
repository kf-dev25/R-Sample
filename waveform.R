library(dplyr)
library(ggplot2)

# 波形種別生存率####
survival_rate_waveform <- cleansed_utstein_data %>%
  count(波形種別, 生存の有無, 機能予後, 心拍再開の有無) %>%
  group_by(波形種別) %>%
  mutate(総数 = sum(n)) %>%
  group_by(波形種別, 心拍再開の有無) %>%
  mutate(心拍再開数 = sum(n)) %>%
  mutate(心拍再開率 = if_else(心拍再開の有無 == FALSE, 0, round(心拍再開数 / 総数 * 100, digits = 2))) %>%
  group_by(波形種別, 生存の有無) %>%
  mutate(生存数 = sum(n)) %>%
  mutate(生存率 = if_else(生存の有無 == FALSE, 0, round(生存数 / 総数 * 100, digits = 2))) %>%
  group_by(波形種別, 機能予後) %>%
  mutate(機能予後良好数 = sum(n)) %>%
  mutate(機能予後率 = if_else(機能予後 == FALSE, 0, round(機能予後良好数 / 総数 * 100, digits = 2))) %>%
  ungroup() %>%
  filter(心拍再開の有無) %>%
  filter(生存の有無) %>%
  filter(機能予後) %>%
  mutate(合計 = sum(総数)) %>%
  mutate(構成比率 = round(総数 / 合計 * 100, digits = 2)) %>%
  dplyr::select("波形種別", "総数", "構成比率", "心拍再開数", "心拍再開率", "生存数", "生存率", "機能予後良好数", "機能予後率")

# 波形種別生存率のグラフ####
survival_rate_waveform$波形種別 <- as.factor(survival_rate_waveform$波形種別)
ggplot(survival_rate_waveform, aes(x = 波形種別, y = 生存率, fill = 波形種別)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(x = 波形種別, y = 生存率), stat = "identity", position = "dodge") +
  geom_text(aes(label = 生存率), size = 5, angle = -90, position = position_dodge(width = 0.5)) +
  scale_fill_brewer(palette = "Set2", labels = c("VF", "PEA", "心静止")) +
  # scale_x_continuous(breaks = c(1,3,4), labels = c("VF","PEA","心静止"))+
  theme(text = element_text(size = 15))
# geom_text(aes(label=生存率),size=8,vjust=1.0)
# theme(legend.position = 'none')
# geom_text(aes(label=心拍再開率),size=4,position=position_dodge(width=0.9),vjust=1.2)+
# labs(fill="波形種別")
