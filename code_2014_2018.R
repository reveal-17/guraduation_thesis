#excelファイル読み込みパッケージインストール
#install.packages('openxlsx')

#パッケージplmインストール
# install.packages('plm')

#パッケージを利用可能に
library(openxlsx)
library(plm)

#　作業ディレクトリに移動
setwd('/Users/miyazakishuhei/Desktop/graduation_thesis')

# データ読み込み
data <- read.xlsx('home_death.xlsx')

# データが正しく読み込めていることを確認
head(data)

#　基本統計量表示
summary(data, na.rm = TRUE)

# 要素数確認
table(data$year)
table(data$home_death)
nrow(data)
length(data$year)
length(data$home_death)

# 欠損値の有無
table(is.na(data))

# 欠損値の状況確認
data[!complete.cases(data),]

#　欠損値はほんとうにランダムなのか？
#　基本統計量（自宅死割合に欠損値が含まれないもの）
summary(data, na.rm = TRUE)
#　基本統計量表示（自宅死割合に欠損値が含まれるもの）
data_drop <- subset(data, is.na(data$home_death))
summary(data_drop)

# 自宅死割合に欠損値が生じた都道府県をヒストグラムで可視化
hist(data_drop$prefecture_code, breaks=seq(1, 47), xlab="prefecture_code", labels=TRUE, main="missing value of prefecture code", col="orange")
# x軸名が命名できず（xlabでできるはずだが...）

# 箱ひげ図を比較
# 65歳以上割合
boxplot(data$elderly_ratio, boxwex=0.25, at=1:1-0.2, main="elderly_ratio", col="yellow")
boxplot(data_drop$elderly_ratio, boxwex=0.25, at=1:1+0.2, add=TRUE, col="orange")
legend("topleft", c("values", "missing values"), fill=c("yellow", "orange"))

# 課税所得
boxplot(data$taxable_income, boxwex=0.25, at=1:1-0.2, main="taxable_income", col="yellow")
boxplot(data_drop$taxable_income, boxwex=0.25, at=1:1+0.2, add=TRUE, col="orange")
legend("topleft", c("values", "missing values"), fill=c("yellow", "orange"))

# 人口
boxplot(data$population, boxwex=0.25, at=1:1-0.2, main="population", col="yellow")
boxplot(data_drop$population, boxwex=0.25, at=1:1+0.2, add=TRUE, col="orange")
legend("topleft", c("values", "missing values"), fill=c("yellow", "orange"))

# 人口（外れ値なし）
boxplot(data$population, outline=FALSE, boxwex=0.25, at=1:1-0.2, main="population(outline=FALSE)", col="yellow")
boxplot(data_drop$population, outline=FALSE, boxwex=0.25, at=1:1+0.2, add=TRUE, col="orange")
legend("topleft", c("values", "missing values"), fill=c("yellow", "orange"))

# 一人あたり課税所得
boxplot(data$per_capita_taxable_income, boxwex=0.25, at=1:1-0.2, main="per_capita_taxable_income", col="yellow")
boxplot(data$per_capita_taxable_income, boxwex=0.25, at=1:1+0.2, add=TRUE, col="orange")
legend("topleft", c("values", "missing values"), fill=c("yellow", "orange"))

# 人口一人あたり在宅療養支援病院数
boxplot(data$shienbyo_ratio, boxwex=0.25, at=1:1-0.2, main="shienbyo_ratio", col="yellow")
boxplot(data_drop$shienbyo_ratio, boxwex=0.25, at=1:1+0.2, add=TRUE, col="orange")
legend("topleft", c("values", "missing values"), fill=c("yellow", "orange"))

# 在宅療養支援病院数
boxplot(data$shienbyo, boxwex=0.25, at=1:1-0.2, main="shienbyo", col="yellow")
boxplot(data_drop$shienbyo, boxwex=0.25, at=1:1+0.2, add=TRUE, col="orange")
legend("topleft", c("values", "missing values"), fill=c("yellow", "orange"))

# 人口一人あたり在宅療養支援診療所数
boxplot(data$shienshin_ratio, boxwex=0.25, at=1:1-0.2, main="shienshin_ratio", col="yellow")
boxplot(data_drop$shienshin_ratio, boxwex=0.25, at=1:1+0.2, add=TRUE, col="orange")
legend("topleft", c("values", "missing values"), fill=c("yellow", "orange"))

# 在宅療養支援診療所数
boxplot(data$shienshin, boxwex=0.25, at=1:1-0.2, main="shienshin", col="yellow")
boxplot(data_drop$shienshin, boxwex=0.25, at=1:1+0.2, add=TRUE, col="orange")
legend("topleft", c("values", "missing values"), fill=c("yellow", "orange"))

# 在宅療養支援診療所数（外れ値なし）
boxplot(data$shienshin, outline=FALSE, boxwex=0.25, at=1:1-0.2, main="shienshin(outline=FALSE)", col="yellow")
boxplot(data_drop$shienshin, outline=FALSE, boxwex=0.25, at=1:1+0.2, add=TRUE, col="orange")
legend("topleft", c("values", "missing values"), fill=c("yellow", "orange"))

# 欠損値対処（自宅死割合）
data <- na.omit(data)

# 欠損値がないことを確認
table(is.na(data))

# 欠損値の状況確認
data[!complete.cases(data),]

# 変数命名
year <- data$year
population <- data$population
population_over65 <- data$population_over65
elderly_ratio <- data$elderly_ratio
taxable_income <- data$taxable_income
per_capita_taxable_income <- data$per_capita_taxable_income
shienbyo <- data$shienbyo
shienbyo_ratio <- data$shienbyo_ratio
shienshin <- data$shienshin
shienshin_ratio <- data$shienshin_ratio
home_death <- data$home_death

# 年次ダミー作成
# 2014年はomitされるので作成しない
transform(data, year2015=0)
transform(data, year2016=0)
transform(data, year2017=0)
transform(data, year2018=0)
data$year2015 <- ifelse(data$year==2015, 1, 0)
data$year2016 <- ifelse(data$year==2016, 1, 0)
data$year2017 <- ifelse(data$year==2017, 1, 0)
data$year2018 <- ifelse(data$year==2018, 1, 0)
year2015 <- data$year2015
year2016 <- data$year2016
year2017 <- data$year2017
year2018 <- data$year2018

#　基本統計量表示
summary(data, na.rm = TRUE)

# オブジェクト数
length(data$year)
for (i in 7:20) {
	col_name <- colnames(data)[i]
	object_num <- length(data[[i]])
	print(col_name)
	print(object_num)
}

#　標準偏差
sd(data$year)
for (i in 7:20) {
	col_name <- colnames(data)[i]
	sd <- sd(data[[i]])
	print(col_name)
	print(sd)
}

table(data$year2015)
table(data$year2016)
table(data$year2017)
table(data$year2018)
transform(data, year2014=0)
data$year2014 <- ifelse(data$year==2014, 1, 0)
year2014 <- data$year2014


# 被説明変数と説明変数
all_variables <- cbind(year, elderly_ratio, per_capita_taxable_income, shienbyo_ratio, shienshin_ratio, home_death, year2015, year2016, year2017, year2018)

# 説明変数
explanatory_variables <- cbind(year, elderly_ratio, per_capita_taxable_income, shienbyo_ratio, shienshin_ratio, year2015, year2016, year2017, year2018)

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
result1 = plm(home_death ~ elderly_ratio + per_capita_taxable_income + shienbyo_ratio + shienshin_ratio + year2015 + year2016 + year2017 + year2018, data=data_refine, model="pooling")
summary(result1)
# 交差項入りも分析

#LSDV推定（within推定、固定効果推定）
result2 <- update(result1, model="within")
summary(result2)

#個別効果の推定値
mu = fixef(result2)
summary(mu)
#個別効果の平均値
mean(mu)
#個別効果の平均からの乖離
mu2 = fixef(result2, type="dmean")
summary(mu2)

#F検定
pFtest(result2, result1)
#p値は非常に小さいので帰無仮説「個別効果はある」は棄却されない
#個別効果がない場合、pooled OLSで推定

#GLS推定（変量効果推定）
result3 = plm(home_death ~ elderly_ratio + per_capita_taxable_income + shienbyo_ratio + shienshin_ratio, data=data_refine, model="random")
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
#p値は極めて小さいので帰無仮説は棄却されない→F検定で帰無仮説を棄却していればGLSでの推定へ
