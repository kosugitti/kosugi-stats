# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第10章 確率と確率分布

# 正規分布の関数 --------------------------------------------------------------

## code 10.1 確率に関する関数使用例
dnorm(x = 0, mean = 0, sd = 1)
pnorm(q = 1.0, mean = 0, sd = 1)
qnorm(p = 0.841, mean = 0, sd = 1)
rnorm(n = 10, mean = 0, sd = 1)
## rnorm は乱数なので，実行するたびに(また人ごとに)違う数字が出る
