# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 7. Alternatives for ANOVA #2. Cmdstan Version

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(MASS)
## 補助関数(map_estimation・MCMCtoDF・MCMCsummary)を読み込む
## 関数集はサイトの「データ解析応用」ページの「事前準備」からダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく。Stanファイルも同じフォルダに置く
source("00_Appendix2_functions.R")





# リバース・リバースエンジニアリング -------------------------------------------------------

mu <- c(50, 50)
sd1 <- 10
sd2 <- 5
rho <- 0.7
SIG <- matrix(ncol = 2, nrow = 2)
SIG[1, 1] <- sd1 * sd1
SIG[2, 2] <- sd2 * sd2
SIG[1, 2] <- sd1 * sd2 * rho
SIG[2, 1] <- sd1 * sd2 * rho
N <- 100
X <- mvrnorm(N, mu, SIG)

dataSet <- list(N = N, X = X)
model <- cmdstanr::cmdstan_model("paired_t.stan")
model$check_syntax()
fit <- model$sample(
  data = dataSet,
  chains = 4,
  iter_sampling = 5000,
  iter_warmup = 1000,
  parallel_chains = 4
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()

# tidyなデータ ----------------------------------------------------------------

X <- mvrnorm(N, mu, SIG)
tidy_data <- X %>%
  as.data.frame() %>%
  rename(pre = V1, post = V2) %>%
  as_tibble() %>%
  rowid_to_column("ID") %>%
  pivot_longer(-ID) %>%
  mutate(cond = if_else(name == "pre", 1, 2))

dataSet <- list(
  L = NROW(tidy_data),
  N = max(tidy_data$ID),
  IDindex = tidy_data$ID,
  Condition = tidy_data$cond,
  val = tidy_data$value
)

model <- cmdstanr::cmdstan_model("paired_t_tidy.stan")
model$check_syntax()
fit <- model$sample(
  data = dataSet,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()


# 課題 --------------------------------------------------------------------

dat_raw <- data.frame(
  ID = 1:4,
  period1 = c(10, 9, 4, 7),
  period2 = c(5, 4, 2, 3),
  period3 = c(9, 5, 3, 5)
)

tidy_dat <- dat_raw %>%
  pivot_longer(-ID) %>%
  mutate(name = as.factor(name) %>% fct_relevel("period1", "period2")) %>%
  mutate(cond = as.numeric(name))
