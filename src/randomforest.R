library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

# データ内の生存患者と死亡患者を均等な数にする####
utstein_life <- cleansed_utstein_data %>%
  mutate(非心原性の種別 = ifelse(非心原性の種別 >= 6, 5, 非心原性の種別)) %>%
  filter(全身機能カテゴリー %in% 1:4) %>%
  mutate(生存の有無 = 0) %>%
  sample_n(size = 15000) %>%
  dplyr::select(性別, 年齢, 目撃, 波形種別, 非心原性の種別, 生存の有無)

utstein_death <- cleansed_utstein_data %>%
  mutate(非心原性の種別 = ifelse(非心原性の種別 >= 6, 5, 非心原性の種別)) %>%
  filter(全身機能カテゴリー == 5) %>%
  mutate(生存の有無 = 1) %>%
  sample_n(size = 15000) %>%
  dplyr::select(性別, 年齢, 目撃, 波形種別, 非心原性の種別, 生存の有無)

# 患者背景####
utstein_train1 <- rbind(utstein_life, utstein_death) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

utstein_train2 <- utstein_train1 %>%
  dplyr::select(性別, 年齢, 目撃, 波形種別, 非心原性の種別)

utstein_train1 <- data.frame(lapply(utstein_train1, as.factor))
utstein_train1$年齢 <- as.numeric(utstein_train1$年齢)
utstein_train1$生存の有無 <- as.numeric(utstein_train1$生存の有無)

utstein_dummy <- dummyVars(~., data = utstein_train1)
utstein_dummy_train <- as.data.frame(predict(utstein_dummy, utstein_train1))
utstein_dummy_train$生存の有無 <- as.factor(utstein_dummy_train$生存の有無)

model <- randomForest(生存の有無 ~ ., data = utstein_dummy_train)
varImpPlot(model)
plot(model)

rfTuning <- tuneRF(utstein_train2, utstein_dummy_train$生存の有無, doBest = TRUE)
