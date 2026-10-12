# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 2. Basics of stochastic programming
## テキスト第3講「データ生成モデリング」のコード(番号はテキストのコード番号)

# 乱数のコードの例(コード3.1) --------------------------------------------------
rm(list = ls())
set.seed(12345)
dnorm(0, mean = 0, sd = 1)
pnorm(0, mean = 0, sd = 1)
qnorm(0.6, mean = 0, sd = 1)
rnorm(10, mean = 0, sd = 1)
set.seed(12345)
rnorm(5, mean = 0, sd = 1)

# 乱数のコードの例2(コード3.2) -------------------------------------------------
rbinom(10, size = 1, prob = 0.5)
rbinom(10, size = 1, prob = 0.3)
rbinom(10, size = 1, prob = 0.7)
# パッケージの利用
library(extraDistr)
rbern(10, prob = 0.5)

# 乱数のコードの例3(コード3.3) -------------------------------------------------
N100 <- rbinom(n = 100, size = 1, prob = 0.5)
N1000 <- rbinom(n = 1000, size = 1, prob = 0.5)
N10000 <- rbinom(n = 10000, size = 1, prob = 0.5)
mean(N100)
mean(N1000)
mean(N10000)

# 確率分布の可視化1(コード3.4) -------------------------------------------------
x <- seq(-4, 4, 0.05)
plot(x, dnorm(x))
library(tidyverse)
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm)

# 確率分布の可視化2(コード3.5) -------------------------------------------------
N <- 1000
X <- rnorm(N, mean = 0, sd = 1) %>% as.data.frame()
ggplot(data = X, aes(x = .)) +
  geom_histogram()

# 確率分布の要約(コード3.6) ----------------------------------------------------
N <- 10000
X <- rnorm(N, mean = 0, sd = 1) %>%
  as.data.frame() %>%
  dplyr::rename(val = 1)
X %>%
  summarise(
    Exp = mean(val),
    SD = sd(val),
    Median = median(val),
    U50 = quantile(val, prob = 0.50),
    U90 = quantile(val, prob = 0.90),
    L90 = quantile(val, prob = 0.10)
  )

# MAP推定値(コード3.7) ---------------------------------------------------------
map_estimation <- function(z) {
  density(z)$x[which.max(density(z)$y)]
}
# 試してみる
X %>%
  summarise(
    MAP = map_estimation(val)
  )
