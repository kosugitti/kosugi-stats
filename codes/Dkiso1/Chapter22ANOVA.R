rm(list = ls())
library(tidyverse)
source("https://riseki.cloudfree.jp/?plugin=attach&refer=ANOVA%E5%90%9B&openfile=anovakun_489.txt")


# Between -----------------------------------------------------------------
# データフレームを組み上げる
Example1 <- data.frame(
  id = rep(1:4, 3),
  condition = rep(1:3, each = 4),
  value = c(6, 6, 5, 5, 7, 4, 7, 6, 4, 3, 4, 6)
)
# Factor型にする
Example1$condition <- factor(Example1$condition,
  labels = c("control", "exp1", "exp2")
)

Example1

result.lm1 <- lm(value ~ condition, data = Example1)
summary(result.lm1)
ggplot(Example1) +
  aes(x = condition, y = value, fill = condition) +
  stat_summary(geom = "bar", fun = "mean")
anova(result.lm1)


anovakun(Example1[, 2:3], "As", 3, eps = T)


# 二要因 ---------------------------------------------------------------------
Example2 <- data.frame(
  id = 1:12,
  num = rep(1:3, 4),
  temp = rep(1:2, each = 6),
  maker = rep(rep(1:2, each = 3), 2),
  value = c(13, 11, 12, 7, 6, 8, 9, 9, 9, 13, 11, 9)
)
Example2$temp <- factor(Example2$temp,
  labels = c("Hot", "Cold")
)
Example2$maker <- factor(Example2$maker,
  label = c("A", "B")
)

Example2

result.lm2 <- lm(value ~ temp * maker, data = Example2)
summary(result.lm2)
anova(result.lm2)
anovakun(Example2[, 3:5], "ABs", 2, 2, eps = T)

# Within ------------------------------------------------------------------

Example3 <- data.frame(
  ID = 1:4,
  Time1 = c(10, 9, 4, 7),
  Time2 = c(5, 4, 2, 3),
  Time3 = c(9, 5, 3, 5)
)

Example3

anovakun(Example3[, 2:4], "sA", 3, eps = T)
