# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第13章 検定

# χ2検定 ---------------------------------------------------------------------

## code 13.1 クロス集計表の検定結果をRで計算する
cross <- matrix(c(8, 30, 30, 77, 27, 60, 40, 28), ncol = 2, byrow = T)
print(cross)
chisq.test(cross)
