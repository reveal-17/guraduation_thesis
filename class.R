# R6パッケージをインストールする
# install.packages("R6")
library(R6)

MissingValue <- 
  R6Class("MissingValue", 
          public = list(
            
            data = NULL,
            
            initialize = function(data) {
              self$data <- data
            },
            
            #　欠損値処理
            missing_value_fix = function(data) {
              #　基本統計量表示
              summary(data, na.rm = TRUE)
              # 欠損値の有無
              table(is.na(data))
              # 欠損値の状況確認
              data[!complete.cases(data),]
              # 欠損値対処（自宅死割合）
              data <- na.omit(data)
              # 欠損値がないことを確認
              table(is.na(data))
              # 欠損値の状況確認
              data[!complete.cases(data),]
            }
          )
        )

CorrelationCheck <- 
  R6Class("CorrelationCheck",
          public = list(
            correlation_check = function(year, elderly_ratio, per_capita_taxable_income, shienbyo_ratio, shienshin_ratio, nursing_station_ratio, home_death, year2015, year2016, year2017) {
              # 被説明変数と説明変数
              all_variables <- cbind(year, elderly_ratio, per_capita_taxable_income, shienbyo_ratio, shienshin_ratio, nursing_station_ratio, home_death, year2015, year2016, year2017)
              # 説明変数
              explanatory_variables <- cbind(year, city_code, elderly_ratio, per_capita_taxable_income, shienbyo_ratio, shienshin_ratio, nursing_station_ratio, year2015, year2016, year2017)
              # ピアソンの相関係数
              cor(all_variables)
              # スピアマンの相関係数
              cor(all_variables, method="spearman")
              # 多重共線性のチェック(VIF)
              cor.res <- cor(explanatory_variables)
              vif.res <- 1/(1-cor.res^2)
              #VIF>10のとき、多重共線性が疑われる
              round(vif.res)
            }
          )
        )

