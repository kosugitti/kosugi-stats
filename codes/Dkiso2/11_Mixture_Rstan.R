# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 11. Mixture model Rstan version.

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(bayesplot)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
## MAP関数
map_estimation <- function(z) {
  density(z)$x[which.max(density(z)$y)]
}
## MCMCサンプルをデータフレームにする関数
MCMCtoDF <- function(fit) {
  fit %>%
    rstan::extract() %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column("iter") %>%
    dplyr::select(-lp__) %>%
    tidyr::pivot_longer(-iter) -> MCMCsample
  return(MCMCsample)
}

## MCMCデータフレームを要約する関数
MCMCsummary <- function(MCMCsample) {
  MCMCsample %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      EAP = mean(value),
      MED = median(value),
      MAP = map_estimation(value),
      SD = sd(value),
      L95 = quantile(value, prob = 0.025),
      U95 = quantile(value, prob = 0.975),
      Rhat = posterior::rhat_basic(value)
    ) %>%
    mutate(across(where(is.numeric), ~ num(., digits = 3)))
}

# データの読み込み ----------------------------------------------------------------

dat <- read_csv("baseballDecade.csv")

## 分布の可視化
dat %>%
  dplyr::filter(position != "投手") %>%
  dplyr::filter(Games > 50) %>%
  dplyr::filter(team == "Tigers") %>%
  dplyr::mutate(salary.log = log(salary)) %>%
  ggplot(aes(x = salary.log, y = ..density..)) +
  geom_histogram(position = "identity", alpha = 0.8, binwidth = 0.35) +
  geom_density(aes(color = team), show.legend = F)

## 混合分布によるモデリング

dat.tmp <- dat %>%
  dplyr::filter(position != "投手") %>%
  dplyr::filter(Games > 50) %>%
  dplyr::filter(team == "Tigers") %>%
  dplyr::mutate(salary.log = log(salary)) %>%
  dplyr::select(salary.log, Name) %>%
  tibble::rowid_to_column("ID")

dataSet <- list(K = 2, L = NROW(dat.tmp), Y = dat.tmp$salary.log)
model <- rstan::stan_model("rstan/latent.stan")

## サンプリングの際に，Rhatがうまくいかないことがあれば，違うseed値でやり直してみてください。
fit <- sampling(model,
  data = dataSet,
  chains = 4,
  seed = 12345,
  iter = 6000,
  warmup = 1000
)

fitDF <- fit %>% MCMCtoDF()

fitDF %>%
  dplyr::filter(str_detect(name, c("mu|sigma"))) %>%
  MCMCsummary()

fitDF %>%
  dplyr::filter(str_detect(name, "pred_class")) %>%
  MCMCsummary()

## 所属クラスと描画
fitDF %>%
  dplyr::filter(str_detect(name, pattern = c("pred_class"))) %>%
  MCMCsummary() %>%
  mutate(Class = as.numeric(round(EAP))) %>%
  mutate(ID = str_extract(name, pattern = "\\d+") %>% as.numeric()) %>%
  arrange(ID) %>%
  left_join(dat.tmp, by = "ID") %>%
  print() %>%
  ggplot(aes(x = exp(salary.log), fill = as.factor(Class))) +
  geom_histogram(binwidth = 1000)




# ゼロ過剰ポアソン ----------------------------------------------------------------

## データの素描
dat %>%
  dplyr::filter(position == "投手") %>%
  dplyr::filter(Games > 50) %>%
  ggplot(aes(x = Save)) +
  geom_histogram()

## ゼロ過剰ポアソン分布をあてはめたモデリング
dat.tmp <- dat %>%
  dplyr::filter(position == "投手") %>%
  dplyr::filter(Games > 50) %>%
  dplyr::select(Save)

model <- rstan::stan_model("rstan/ziPoisson.stan")

dataSet <- list(L = NROW(dat.tmp), Y = dat.tmp$Save)
fit <- sampling(model,
  data = dataSet,
  chains = 4,
  seed = 12345,
  iter = 6000,
  warmup = 1000
)

fit %>%
  MCMCtoDF() %>%
  dplyr::filter(str_detect(name, c("theta|lambda"))) %>%
  MCMCsummary()

## 事後予測分布的に作ってみる
theta <- 0.61
lambda <- 14.67
N <- NROW(dat.tmp)
data.frame(FLG = rbinom(N, size = 1, prob = theta)) %>%
  rowwise() %>%
  mutate(Save = if_else(FLG == 1, rpois(1, lambda), rpois(1, 0))) %>%
  ggplot(aes(x = Save)) +
  geom_histogram(binwidth = 1)


# 課題 ----------------------------------------------------------------------

## 課題1　混合分布モデル

### Tigersのところを色々変えて，混合分布が適用できそうなチームを探そう
dat %>%
  dplyr::filter(position != "投手") %>%
  dplyr::filter(Games > 50) %>%
  dplyr::filter(team == "Tigers") %>%
  dplyr::mutate(salary.log = log(salary)) %>%
  ggplot(aes(x = salary.log, y = ..density..)) +
  geom_histogram(position = "identity", alpha = 0.8, binwidth = 0.35) +
  geom_density(aes(color = team), show.legend = F)

## チームが見つかったら，データを作って分析しよう。

dat.tmp <- dat %>%
  dplyr::filter(position != "投手") %>%
  dplyr::filter(Games > 50) %>%
  dplyr::filter(team == "XXXX") %>%
  dplyr::mutate(salary.log = log(salary)) %>%
  dplyr::select(salary.log, Name) %>%
  tibble::rowid_to_column("ID")

## 課題2　ゼロ過剰ポアソン回帰

dat.tmp <- dat %>%
  dplyr::filter(position == "投手") %>%
  dplyr::filter(Games > 50) %>%
  dplyr::select(Save, salary) %>%
  dplyr::mutate(salary = salary / 1000)

model <- rstan::stan_model("rstan/ziPoisson2.stan")
dataSet <- list(L = NROW(dat.tmp), Y = dat.tmp$Save, X = dat.tmp$salary)
fit <- sampling(model,
  data = dataSet,
  chains = 4,
  seed = 12345,
  iter = 6000,
  warmup = 1000
)
