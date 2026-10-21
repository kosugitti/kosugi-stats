# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第3章 統計環境Rの導入
## この章ではデータファイルは使わない

# R/RStudioの基本操作 ---------------------------------------------------------

## code 3.1 四則演算のコード
# 四則演算
1 + 2
2 - 4
3 * 5
6 / 3
sqrt(16)


# Rの基本操作 -----------------------------------------------------------------

## 関数の利用

## code 3.2 ヘルプのコード
# ヘルプも関数
help(sqrt)

## パッケージの利用

## code 3.3 コードからインストールする
## (一度インストールすれば二度目からは不要なので，行頭の # を外して一度だけ実行する)
# install.packages("tidyverse")

## オブジェクトの利用

## code 3.4 オブジェクトに代入
# オブジェクト
A <- 2
B <- 3
A + B
A

## code 3.5 オブジェクトは上書き
A <- 4
A + B

## code 3.6 ベクトル処理
obj1 <- 1:10
obj1 + 2
obj2 <- c(2, 3, 4, 1, 2, 3, 4, 5, 6, 7)
obj1 + obj2

## code 3.7 リスト型
obj3 <- list(
  name = c("Taro", "Hanako", "Ken"),
  gender = c("male", "female", "male"),
  height = c(170, 160),
  weight = 55
)
obj3
obj3$name

## code 3.8 データフレーム型
df <- data.frame(
  name = c("Taro", "Hanako", "Ken"),
  gender = c("male", "female", "male"),
  height = c(170, 160, 155),
  weight = c(55, 60, 43)
)
df
df$name
