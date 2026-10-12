# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 1. Basics of R programming
## テキスト第2講「プログラミングの基礎」のコード(番号はテキストのコード番号)

# 代入操作(コード2.1) ----------------------------------------------------------
a <- 1
b <- 2
a + b
a <- 3
a + b

# さまざまなデータの型(コード2.2) ----------------------------------------------
# 数字をセットで持つvector型，matrix型
x <- c(1, 2, 5, 8, 9)
A <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2, nrow = 3)
# 型にこだわりなくなんでも収納list型
dataSet <- list(name = c("kosugi", "koji"),
                v1 = c(1, 2, 4, 5, 3, 5, 6),
                v2 = matrix(c(1, 2, 3, 4), ncol = 2))
# 矩形に整えられたlist型であるdata.frame
df <- data.frame(list(name = c("kosugi", "suzuki"),
                      V1 = c(176, 173),
                      V2 = c(83, 55)))

# 反復操作1(コード2.3) ---------------------------------------------------------
a <- 0
for (i in 1:4) {
  a <- a + 1
  print(a)
}

# 反復操作2(コード2.4) ---------------------------------------------------------
a <- 0
for (i in c(1, 3, 5, 15, 12)) {
  print(paste(a, "に", i, "を加えます"))
  a <- a + i
  print(a)
}

# 反復操作3(コード2.5) ---------------------------------------------------------
A <- matrix(1:15, ncol = 3, nrow = 5)
for (i in 1:5) {
  for (j in 1:3) {
    print(paste("Aの", i, "行", j, "列目の要素は", A[i, j]))
    A[i, j] <- A[i, j] + (i * j)
  }
}
print(A)

# 条件分岐(コード2.6) ----------------------------------------------------------
egg <- 250
if (egg < 200) {
  print("卵を買います")
} else {
  print("卵を買いません")
}

# 論理演算(コード2.7) ----------------------------------------------------------
X <- 2
X > 3
X < 3
X == 3
X != 3
X > 1 && X < 3
X < 1 || X > 3

# 関数を作る基本的な構文(コード2.8) --------------------------------------------
# 形を示すためのものなので，このままでは動かない
# function_name <- function(argument) {
#    # function body
#    return(value)
# }

# 引数を1つとる関数(コード2.9) -------------------------------------------------
add3 <- function(x) {
  x <- x + 3
  return(x)
}
# 実行例
add3(5)

# 引数を2つとる関数(コード2.10) ------------------------------------------------
add_numbers <- function(a, b) {
  sum <- a + b
  return(sum)
}
# 実行例
add_numbers(2, 5)

# 引数に既定値を与える(コード2.11) ---------------------------------------------
add_numbers2 <- function(a, b = 1) {
  sum <- a + b
  return(sum)
}
# 実行例
add_numbers2(2, 5)
add_numbers2(4)

# 複数の値を戻り値にする(コード2.12) -------------------------------------------
calculate_values <- function(a, b) {
  sum <- a + b
  diff <- a - b
  # 戻り値として名前付きリストを作成
  result <- list("sum" = sum, "diff" = diff)
  return(result)
}
# 実行例
result <- calculate_values(10, 5)
# 結果を表示
print(result)

# 戻り値の中から必要な値を抜き出す(コード2.13) ---------------------------------
result <- lm(mpg ~ wt + hp, data = mtcars)
# 一般的な結果の表示
summary(result)
# 構造の確認
str(result)
# 必要な値を抜き出す
result$coefficients
