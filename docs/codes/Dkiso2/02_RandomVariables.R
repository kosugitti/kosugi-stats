# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 2.Basics of stochastic programming

rm(list = ls())
set.seed(12345)
dnorm(0, mean = 0, sd = 1)
pnorm(0, mean = 0, sd = 1)
qnorm(0.5, mean = 0, sd = 1)
rnorm(10, mean = 0, sd = 1)
set.seed(12345)
rnorm(5, mean = 0, sd = 1)

rbinom(10, size = 1, prob = 0.5)
rbinom(10, size = 1, prob = 0.3)
rbinom(10, size = 1, prob = 0.7)
# パッケージの利用
library(extraDistr)
rbern(10, prob = 0.5)

N100 <- rbinom(n = 100, size = 1, prob = 0.3)
N1000 <- rbinom(n = 1000, size = 1, prob = 0.3)
N10000 <- rbinom(n = 10000, size = 1, prob = 0.3)
mean(N100)
mean(N1000)
mean(N10000)

# 正規分布の可視化 ----------------------------------------------------------------

x <- seq(-4, 4, 0.05)
plot(x, dnorm(x))
library(tidyverse)
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm)

N <- 10000
X <- rnorm(N, mean = 0, sd = 1) %>% as.data.frame()
ggplot(data = X, aes(x = .)) +
  geom_histogram(binwidth = 0.02)


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


map_estimation <- function(z) {
  density(z)$x[which.max(density(z)$y)]
}

X %>%
  summarise(
    MAP = map_estimation(val)
  )
