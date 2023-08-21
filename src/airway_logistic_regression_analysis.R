library(data.table)
library(dplyr)
library(tidyr)
library(epiDisplay)

analysis_data_airway <- cleansed_utstein_data %>%
  # filter(波形種別 == 3) %>%
  filter(波形種別 == 4) %>%
  mutate(特定行為器具種別 = ifelse(特定行為器具使用 == 0 & 特定行為器具種別 == 0, "なし", 特定行為器具種別)) %>%
  mutate(特定行為器具種別 = ifelse(特定行為器具使用 == 1 & 特定行為器具種別 == 1, "ラリンゲルチューブ", 特定行為器具種別)) %>%
  mutate(特定行為器具種別 = ifelse(特定行為器具使用 == 1 & 特定行為器具種別 == 2, "食道閉鎖式エアウェイ", 特定行為器具種別)) %>%
  mutate(特定行為器具種別 = ifelse(特定行為器具使用 == 1 & 特定行為器具種別 == 3, "器官チューブ", 特定行為器具種別)) %>%
  filter(特定行為器具種別 == "なし" | 特定行為器具種別 == "ラリンゲルチューブ" | 特定行為器具種別 == "食道閉鎖式エアウェイ" | 特定行為器具種別 == "器官チューブ") %>%
  # dplyr::select(心拍再開の有無, 特定行為器具種別)
  dplyr::select(生存の有無, 特定行為器具種別)
  # dplyr::select(機能予後, 特定行為器具種別)

# logistic <- glm(心拍再開の有無 ~ 特定行為器具種別, family = binomial, data = analysis_data_airway)
logistic <- glm(生存の有無 ~ 特定行為器具種別, family = binomial, data = analysis_data_airway)
# logistic <- glm(機能予後 ~ 特定行為器具種別, family = binomial, data = analysis_data_airway)
logistic_answer_airway <- summary(logistic)$coefficient
pr <- exp(logistic_answer_airway[, 1])
prl <- exp(logistic_answer_airway[, 1] - 1.96 * logistic_answer_airway[, 2])
pru <- exp(logistic_answer_airway[, 1] + 1.96 * logistic_answer_airway[, 2])
N <- nrow(analysis_data_airway)
aic <- AIC(logistic)

result <- cbind(logistic_answer_airway, pr, prl, pru, aic, N)
result[2:nrow(result), 8:9] <- ""
colnames(result)[6:7] <- c("RR95%CI.low", "RR95%CI.up")
