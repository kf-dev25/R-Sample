library(data.table)
library(dplyr)

# 実行の際はutstein_dataのインポートが必要

# 初期データのエラー排除と時刻データの変換####
cleansed_utstein_data <-
  utstein_data %>%
  filter(年 %in% 2005:2016) %>%
  filter(年 >= 2010) %>%
  filter(年齢 %in% 0:115) %>%
  # 18歳以下は医学的に小児患者と見なされ、成人と病態や処置方法等が異なってくるため
  filter(年齢 >= 18) %>%
  filter(救急救命士乗車 == 1) %>%
  filter(医師の乗車 == 2) %>%
  filter(全身機能カテゴリー %in% 1:5) %>%
  filter(!is.na(年齢)) %>%
  # 波形種別2は件数が少なすぎるため、また波形種別5は「その他」となっており種別の特定が不可のため除外
  filter(波形種別 == 1 | 波形種別 == 3 | 波形種別 == 4) %>%
  filter(薬剤投与 %in% 0:1) %>%
  filter(特定行為器具使用 %in% 0:1) %>%
  filter(特定行為器具種別 %in% 0:3) %>%
  filter((特定行為器具使用 == 0 & 特定行為器具種別 == 0) | (特定行為器具使用 == 1 & 特定行為器具種別 %in% 0:3)) %>%
  mutate(非心原性の種別 = ifelse(is.na(非心原性の種別), 0, 非心原性の種別)) %>%
  filter(非心原性の種別 %in% 0:5)%>%
  mutate(心拍再開の有無 = if_else(心拍再開 == 1, TRUE, FALSE)) %>%
  mutate(生存の有無 = if_else(全身機能カテゴリー >= 1 & 全身機能カテゴリー <= 4, TRUE, FALSE)) %>%
  mutate(機能予後 = if_else(全身機能カテゴリー >= 1 & 全身機能カテゴリー <= 2, TRUE, FALSE)) %>%
  
  # mutate(発生年月日 = as.POSIXct(strptime(発生年月日, "%Y/%m/%d %H:%M"))) %>%
  # mutate(目撃時刻 = as.POSIXct(strptime(目撃時刻, "%Y/%m/%d %H:%M"))) %>%
  # mutate(B_CPR開始時刻 = as.POSIXct(strptime(B_CPR開始時刻, "%Y/%m/%d %H:%M"))) %>%
  # mutate(初回除細動実施時刻 = as.POSIXct(strptime(初回除細動実施時刻, "%Y/%m/%d %H:%M"))) %>%
  # mutate(薬剤投与時刻 = as.POSIXct(strptime(薬剤投与時刻, "%Y/%m/%d %H:%M"))) %>%
  # mutate(覚知 = as.POSIXct(strptime(覚知, "%Y/%m/%d %H:%M"))) %>%
  # mutate(現着 = as.POSIXct(strptime(現着, "%Y/%m/%d %H:%M"))) %>%
  # mutate(接触 = as.POSIXct(strptime(接触, "%Y/%m/%d %H:%M"))) %>%
  # mutate(CPR開始 = as.POSIXct(strptime(CPR開始, "%Y/%m/%d %H:%M"))) %>%
  # mutate(病院収容 = as.POSIXct(strptime(病院収容, "%Y/%m/%d %H:%M"))) %>%
  # mutate(初回心拍再開時刻 = as.POSIXct(strptime(初回心拍再開時刻, "%Y/%m/%d %H:%M"))) %>%

  # 時間データを変換(空白などはNAになる)
  mutate(現着時間 = as.numeric(difftime(現着, 覚知, units = "mins"))) %>%
  mutate(病院収容時間 = as.numeric(difftime(病院収容, 現着, units = "mins"))) %>%
  filter(現着時間 >= 0, 病院収容時間 >= 0) %>%
  filter(現着時間 <= 1440, 病院収容時間 <= 1440) %>%
  filter(気道確保 == 1)
