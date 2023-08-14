# R-Sample
#### 解析対象データは47都道府県ごとに記録され、消防庁にて集計された心肺停止状態で救急搬送された全患者のデータ。  
#### 大元のデータは公開できないため、各プログラム実行後のデータを `result_data.RData` に格納。  
#### 各プログラムの役割は以下の通り。  
  
- data_cleansing.R  
データの前処理  
  
- data_background.R  
解析対象データの概要（大まかな項目ごとの集計）

- waveform.R
心臓の波形種別ごとの集計

- epinephrine.R
搬送患者に薬剤投与が行われたかどうか、また行われた後の予後の集計
  
- airway.R
搬送患者に特定医療器具による治療行為が行われたかどうかまた行われた後の予後の集計

- epinephrine_logistic_regression_analysis.R
薬剤投与実施に関するロジスティック回帰分析

- airway_logistic_regression_analysis.R
特定医療器具実施に関するロジスティック回帰分析

- randomforest.R
患者背景を元にしたランダムフォレストによる予後予測モデル
