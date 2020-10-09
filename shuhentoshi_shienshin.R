# 2014-2017
# 都市圏のみで検証

# パッケージインストール
# install.packages("tidyverse")

#パッケージを利用可能に
library(openxlsx)
library(plm)
library(tidyverse)

#　作業ディレクトリに移動
setwd('/Users/miyazakishuhei/Desktop/graduation_thesis')



# 大都市圏・都市圏の市町村コード抽出=======================================================================================

# データ読み込み
# 合併を考慮できていない可能性
# 〇〇市△△区と冗長的になっている可能性
toshiken <- read.xlsx('toshiken.xlsx', startRow = 2)

# 大都市圏の中心市・周辺都市、都市圏の中心市・周辺都市の行のみ抽出
toshiken_fix <- toshiken[toshiken[, 3] >= 1 | toshiken[, 4] >= 3, ]

# 最初の列のみ抽出
toshiken_num <- toshiken_fix[ ,1]

# NAが含まれているので削除
is.na(toshiken_num)
toshiken_num_fix <- toshiken_num[!is.na(toshiken_num)]

# NAが削除されているか確認
head(toshiken_num_fix)
str(toshiken_num_fix)
length(toshiken_num_fix)

# 文字列なので数値に変換
toshiken_num_fix <- as.integer(toshiken_num_fix)



# データの準備=======================================================================================

# データ読み込み
data <- read.xlsx('home_death.xlsx', sheet="R_robast_check")

# データが正しく読み込めていることを確認
head(data)

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

# 欠損値の状況再確認
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



# 大都市圏・都市圏のみを対象にした分析=======================================================================================
# データから全市町村のコードを抽出
all_city_code <- data[, 3]
all_city_code <- unique(all_city_code)

# 全市町村のコードの総数1742
length(all_city_code)
print(all_city_code)

# 大都市圏・都市圏のコードの総数849
length(toshiken_num_fix)
print(toshiken_num_fix)

# 一致したコードを格納するリストを作成
match_code <- c()

# データから大都市圏・都市圏の市町村を抽出
for (i in 1:length(toshiken_num_fix)) {
  for (j in 1:length(all_city_code)) {
    if (all_city_code[j] == toshiken_num_fix[i]) {
      # 一致したコードをリストに追加
      match_code <- c(match_code, c(all_city_code[j]))		
    }
  }
}

# match_code重複なくす673
match_code <- unique(match_code)
write(match_code, file = "match_code.txt")

# match_codeを４桁と５桁で分ける
# 4桁84
match_code_less <- c()
# 5桁589
match_code_more <- c()
for (i in 1:length(match_code)) {
  if (match_code[i] < 10000) {
    match_code_less <- c(match_code_less, c(match_code[i]))
  } else if (match_code[i] >= 10000) {
    match_code_more <- c(match_code_more, c(match_code[i]))
  }
}

# 大都市圏・都市圏以外の市町村コード
shuhentoshi <- setdiff(all_city_code, match_code)

# match_codeを４桁と５桁で分ける
# 4桁
shuhentoshi_less <- c()
# 5桁
shuhentoshi_more <- c()
for (i in 1:length(shuhentoshi)) {
  if (shuhentoshi[i] < 10000) {
    shuhentoshi_less <- c(shuhentoshi_less, c(shuhentoshi[i]))
  } else if (shuhentoshi[i] >= 10000) {
    shuhentoshi_more <- c(shuhentoshi_more, c(shuhentoshi[i]))
  }
}

# 空のリスト（行番号格納）
row_num_list <- c()

# 大都市圏・都市圏以外のの行番号取得
# ここがうまくいかない　複数年取り出せない
# grepだと４桁コードが５桁のものと重複する可能性
# ４桁コードの先頭に０をつけるpaste()で文字列０を結合→allもtoshikenも文字列として扱ってrow_num(int)導出

# うまくいかないので、４桁と５桁のコードそれぞれ変数に格納して区別して操作する
for (i in 1:length(shuhentoshi_less)) {
  for (j in 1:length(data$city_code)) {
    # ４桁文字数で測る
    # ここを変える
    if (shuhentoshi_less[i] == data$city_code[j]) {
      # 行番号取得
      row_num <- grep(shuhentoshi_less[i], data$city_code)
      # リストに格納
      row_num_list <- c(row_num_list, c(row_num))
    }	
  }	
}


for (i in 1:length(shuhentoshi_more)) {
  for (j in 1:length(data$city_code)) {
    # 5桁文字数で測る
    if (shuhentoshi_more[i] == data$city_code[j]) {
      # 行番号取得
      row_num <- grep(shuhentoshi_more[i], data$city_code)
      # リストに格納
      row_num_list <- c(row_num_list, c(row_num))
    }	
  }	
}


# 重複なくす
row_num_list <- unique(row_num_list)

# 昇順に
row_num_list <- sort(row_num_list)
print(row_num_list)
length(row_num_list)

# 空のデータセットつくる
data_toshiken <- matrix(ncol=18)
head(data_toshiken)

# 変数（列名）命名
colnames(data_toshiken) <- c("year", "prefecture_code", "city_code", "population", "population_over65", "elderly_ratio", "taxable_income", "per_capita_taxable_income", "shienbyo", "shienbyo_ratio", "shienshin", "shienshin_ratio", "nursing_station", "nursing_station_ratio", "home_death", "year2015", "year2016", "year2017")

# 元データの不要列を削除
head(data)
data <- data[, -4:-6]

# 大都市圏・都市圏の市町村のみのデータにする
for (i in 1:length(row_num_list)) {
  # 空のデータセットに一致する行を格納していく
  slice_data <- slice(data, row_num_list[i])
  data_toshiken <- rbind(data_toshiken, slice_data)
}

# 重複はなくさない（パネルデータ）

# 先頭行NA削除
data_toshiken <- data_toshiken[-1, ]
head(data_toshiken)
table(data_toshiken$year)

#　基本統計量表示
summary(data_toshiken, na.rm = TRUE)

# 欠損値の有無
table(is.na(data_toshiken))

# 欠損値の状況確認
data[!complete.cases(data_toshiken),]

# 欠損値対処（自宅死割合）
data <- na.omit(data_toshiken)

# 欠損値がないことを確認
table(is.na(data_toshiken))

# 欠損値の状況再確認
data[!complete.cases(data),]

#　基本統計量表示
summary(data_toshiken, na.rm = TRUE)

# オブジェクト数
length(data_toshiken[,1])
length(data_toshiken[,3])
for (i in 4:18) {
  col_name <- colnames(data_toshiken)[i]
  object_num <- length(data_toshiken[[i]])
  print(col_name)
  print(object_num)
}

#　標準偏差
sd(data_toshiken[,1])
sd(data_toshiken[,3])
for (i in 4:18) {
  col_name <- colnames(data_toshiken)[i]
  sd <- sd(data_toshiken[[i]])
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
data_refine <- pdata.frame(data_toshiken, index=c("city_code", "year"), drop.index=TRUE)
result1 = plm(home_death ~ elderly_ratio + per_capita_taxable_income + shienshin_ratio + nursing_station_ratio + year2015 + year2016 + year2017, data=data_refine, model="pooling")
summary(result1)

#LSDV推定（within推定、固定効果推定）
result2 <- update(result1, model="within")
summary(result2)

#F検定
pFtest(result2, result1)
#p値は非常に小さいので帰無仮説「個別効果はない」は棄却される

#GLS推定（変量効果推定）
result3 = plm(home_death ~ elderly_ratio + per_capita_taxable_income + shienshin_ratio + nursing_station, data=data_refine, model="random")
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
#有意水準p<0.05とすると、この結果は統計的に有意。帰無仮説は棄却→固定効果推定を採用
