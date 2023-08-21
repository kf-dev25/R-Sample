library(data.table)
library(dplyr)
library(tidyr)
library(epiDisplay)

analysis_data_epinephrine <- cleansed_utstein_data %>%
  filter(波形種別 == 4) %>%
  # mutate(心拍再開の有無 = if_else(心拍再開 == 1, TRUE, FALSE)) %>%
  # mutate(生存の有無 = if_else(全身機能カテゴリー >= 1 & 全身機能カテゴリー <= 4, TRUE, FALSE)) %>%
  mutate(機能予後 = if_else(全身機能カテゴリー >= 1 & 全身機能カテゴリー <= 2, TRUE, FALSE)) %>%
  mutate(薬剤投与 = ifelse(薬剤投与 == 0, "無し", "有り")) %>%
  # dplyr::select(心拍再開の有無, 薬剤投与)
  dplyr::select(生存の有無, 薬剤投与)
  # dplyr::select(機能予後, 薬剤投与)

# logistic <- glm(心拍再開の有無 ~ 薬剤投与, family = binomial, data = analysis_data_epinephrine)
logistic <- glm(生存の有無 ~ 薬剤投与, family = binomial, data = analysis_data_epinephrine)
# logistic <- glm(機能予後 ~ 薬剤投与, family = binomial, data = analysis_data_epinephrine)
logistic_answer_epinephrine <- summary(logistic)$coefficient
pr <- exp(logistic_answer_epinephrine[, 1])
prl <- exp(logistic_answer_epinephrine[, 1] - 1.96 * logistic_answer_epinephrine[, 2])
pru <- exp(logistic_answer_epinephrine[, 1] + 1.96 * logistic_answer_epinephrine[, 2])
N <- nrow(analysis_data_epinephrine)
aic <- AIC(logistic)

result <- cbind(logistic_answer_epinephrine, pr, prl, pru, aic, N)
result[2:nrow(result), 8:9] <- ""
colnames(result)[6:7] <- c("RR95%CI.low", "RR95%CI.up")
