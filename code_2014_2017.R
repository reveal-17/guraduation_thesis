#訪問看護ステーションを説明変数に加え、2014~2017年までのデータで同様の分析（頑健性の確認）

#パッケージを利用可能に
library(openxlsx)
library(plm)

# クラス読み込み
source("class.R")

#　作業ディレクトリに移動
setwd('/Users/miyazakishuhei/Desktop/graduation_thesis')

# データ読み込み
data <- read.xlsx('home_death.xlsx', sheet="R_robast_check")

# データが正しく読み込めていることを確認
head(data)

#欠損値はほんとうにランダムなのか？
#基本統計量（自宅死割合に欠損値が含まれないもの）
summary(data, na.rm = TRUE)
#基本統計量表示（自宅死割合に欠損値が含まれるもの）
data_drop <- subset(data, is.na(data$home_death))
summary(data_drop)

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

# 変数命名
year <- data$year
city_code <- data$city_code
population <- data$population
population_over65 <- data$population_over65
elderly_ratio <- data$elderly_ratio
taxable_income <- data$taxable_income
per_capita_taxable_income <- data$per_capita_taxable_income
shienbyo <- data$shienbyo
shienbyo_ratio <- data$shienbyo_ratio
shienshin <- data$shienshin
shienshin_ratio <- data$shienshin_ratio
nursing_station <- data$nursing_station
nursing_station_ratio <- data$nursing_station_ratio
home_death <- data$home_death

# 年次ダミー作成
# 2014年はomitされるので作成しない
transform(data, year2015=0)
transform(data, year2016=0)
transform(data, year2017=0)
data$year2015 <- ifelse(data$year==2015, 1, 0)
data$year2016 <- ifelse(data$year==2016, 1, 0)
data$year2017 <- ifelse(data$year==2017, 1, 0)
year2015 <- data$year2015
year2016 <- data$year2016
year2017 <- data$year2017

#　基本統計量表示
summary(data, na.rm = TRUE)

# オブジェクト数
length(data$year)
length(data$city_code)
for (i in 7:21) {
	col_name <- colnames(data)[i]
	object_num <- length(data[[i]])
	print(col_name)
	print(object_num)
}

#　標準偏差
sd(data$year)
sd(data$city_code)
for (i in 7:21) {
	col_name <- colnames(data)[i]
	sd <- sd(data[[i]])
	print(col_name)
	print(sd)
}

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

# pooledOLS
# パネルデータと認識させる
data_refine <- pdata.frame(data, index=c("city_code", "year"), drop.index=TRUE)
result1 = plm(home_death ~ elderly_ratio + per_capita_taxable_income + shienbyo_ratio + shienshin_ratio + nursing_station_ratio + year2015 + year2016 + year2017, data=data_refine, model="pooling")
summary(result1)

# 前2年と後2年で分けて分析
data_before <- subset(data, year <= 2015)
data_after <- subset(data, year >= 2016)
data_before_refine <- pdata.frame(data_before, index=c("city_code", "year"), drop.index=TRUE)
data_after_refine <- pdata.frame(data_after, index=c("city_code", "year"), drop.index=TRUE)
result1_before = plm(home_death ~ elderly_ratio + per_capita_taxable_income + shienbyo_ratio + shienshin_ratio + nursing_station_ratio + year2015 + year2016 + year2017, data=data_before_refine, model="pooling")
result1_after = plm(home_death ~ elderly_ratio + per_capita_taxable_income + shienbyo_ratio + shienshin_ratio + nursing_station_ratio + year2015 + year2016 + year2017, data=data_after_refine, model="pooling")
summary(result1_before)
summary(result1_after)

# 交差項入りも分析
data_refine <- pdata.frame(data, index=c("city_code", "year"), drop.index=TRUE)
result1 = plm(home_death ~ elderly_ratio + per_capita_taxable_income + shienbyo_ratio + shienshin_ratio + nursing_station + per_capita_taxable_income*shienbyo_ratio + year2015 + year2016 + year2017, data=data_refine, model="pooling")
summary(result9)

#LSDV推定（within推定、固定効果推定）
result2 <- update(result1, model="within")
summary(result2)

# 前2年と後2年で分けて分析
result2_before <- update(result1_before, model="within")
result2_after <- update(result1_after, model="within")
summary(result2_before)
summary(result2_after)

#F検定
pFtest(result2, result1)
#p値は非常に小さいので帰無仮説「個別効果はない」は棄却される

#GLS推定（変量効果推定）
result3 = plm(home_death ~ elderly_ratio + per_capita_taxable_income + shienbyo_ratio + shienshin_ratio + nursing_station, data=data_refine, model="random")
summary(result3)
#行列式の計算がうまくいっていない
#逆行列が求まらないから？
#値が極端に大きいものと小さいものが混在しているから？(yearダミー消したらできた)

#ハウスマン検定（個別効果と説明変数に相関がないという帰無仮説）
#yearダミーなしLSDV推定
result4 <- update(result3, model="within")
summary(result4)

#yearダミーなしLSDV(result4)とresult3でハウスマン検定
phtest(result4, result3)
#p値は極めて小さいので帰無仮説は棄却される→F検定で帰無仮説を棄却していれば固定効果での推定へ

