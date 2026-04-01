rm(list = ls())
library(effsize)

## 対応のあるt検定
X <- c(34, 25, 13, 21, 13, 26, 19, 12, 19, 19)
Y <- c(35, 30, 16, 27, 20, 28, 16, 14, 18, 21)
t.test(X, Y, paired = T, alternative = "less")
cohen.d(X, Y, paired = TRUE)

## 対応のあるt検定;表記法2
dat <- data.frame(condition = c(rep(1, 10), rep(2, 10)), value = c(X, Y))
dat$condition <- factor(dat$condition, labels = c("pre", "post"))
t.test(value ~ condition, data = dat, paired = T, alternative = "less")

### 効果量
dat <- data.frame(condition = c(rep(1, 10), rep(2, 10)), value = c(X, Y))
dat$condition <- factor(dat$condition, labels = c("pre", "post"))
dat$ID <- rep(1:10, 2)
cohen.d(value ~ condition | Subject(ID), data = dat, paired = TRUE)

## 対応のないt検定
X <- c(25, 15, 24, 17, 22)
Y <- c(31, 28, 30, 19, 18)
dat <- data.frame(condition = c(rep(1, 5), rep(2, 5)), value = c(X, Y))
dat$condition <- factor(dat$condition, labels = c("control", "experimental"))
t.test(value ~ condition, data = dat)

## Welchの補正なし
t.test(value ~ condition, data = dat, var.equal = T)


## 効果量を求める
cohen.d(value ~ condition, data = dat, hedges.correction = T)

## 検定力分析
power.t.test(power = 0.8, delta = 0.7, sig.level = 0.05)

### nを固定
power.t.test(n = 5, delta = 0.7, sig.level = 0.05)


# 課題1
pre <- c(19, 38, 24, 25, 12, 28, 11, 17)
post <- c(31, 23, 28, 35, 14, 4, 4, 38)

### 事前スコアの方が事後スコアよりも低いかどうか，5\%水準で検定してください。

# 課題2
Control <- c(20, 21, 21, 20, 21, 22, 22, 22, 20, 20)
Experimental <- c(20, 18, 22, 20, 21, 18, 19, 21, 21, 21)
### 母分散が等しいと仮定したt検定を実行してください。
### 母分散が等しいという仮定をおかないt検定を実行してください。
### Hedgesのgを算出してください。

# 課題3
## 基礎実験1の授業の中で，社会的影響についてのデータを取りましたね。
## そこでは，「他者の存在の有無」により，「課題遂行量」に違いが出るのかどうか，を検証したとおもいます(他にも変数をとったと思いますが，ここでは課題遂行量にのみ注目します)。
### そのデータを使って，次の問いに答えてください。なお，計算に使用したデータ(あるいはデータファイル)も合わせて提出してください。
### 他者がいる群といない群，それぞれの標本平均値，標本分散，不偏分散をそれぞれ算出してください。
### このデータセットが正規分布に従う母集団から得られたと考えて，各群の母平均について95\%の区間推定をしてください
### このデータセットが正規分布に従う母集団から得られたと考えて，二群の平均値の差の検定をします。キム仮設と対立仮説はそれぞれどのようなものになりますか。
### 二群の平均値の差の検定をし，検定統計量とともに判断結果を報告してください。
### その検定に関して，効果量としてHedgesのgを算出してください。
