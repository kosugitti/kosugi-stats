# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 13. Change Point models Cmdstan version.

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(cmdstanr)
library(bayesplot)
## MAP関数
map_estimation <- function(z) {
  density(z)$x[which.max(density(z)$y)]
}

## MCMCサンプルをデータフレームにする関数
MCMCtoDF <- function(fit) {
  fit$draws() %>%
    posterior::as_draws_df() %>%
    tibble::as_tibble() %>%
    dplyr::select(-lp__, -.draw, -.chain, -.iteration) %>%
    tibble::rowid_to_column("iter") %>%
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


# data plot ---------------------------------------------------------------

dat <- read_csv("weight.csv")
g <- dat %>%
  dplyr::mutate(date = as.Date(date)) %>%
  # dplyr::filter(date > "2019/01/01") %>%
  # dplyr::filter(date < "2022/01/01") %>%
  ggplot(aes(x = date, y = weight)) +
  geom_point() +
  scale_x_date(date_breaks = "6 month")
g


# 二種類混ぜる ------------------------------------------------------------------

dat1 <- dat %>%
  dplyr::filter(date > "2019/01/01") %>%
  dplyr::filter(date < "2021/01/01")

g <- dat1 %>%
  dplyr::mutate(date = as.Date(date)) %>%
  ggplot(aes(x = date, y = weight)) +
  geom_point() +
  scale_x_date(date_breaks = "2 month")
g


model <- cmdstanr::cmdstan_model("cmdstan/changePoint1.stan")
model$check_syntax()
dataSet <- list(L = NROW(dat1), W = dat1$weight)
fit <- model$sample(
  data = dataSet,
  chains = 4,
  parallel_chains = 4,
  seed = 12345,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit %>%
  MCMCtoDF() %>%
  dplyr::filter(str_detect(name, c("mu|sigma"))) %>%
  MCMCsummary()

## 時系列チェック
timeCheck <-
  fit %>%
  MCMCtoDF() %>%
  MCMCsummary() %>%
  dplyr::filter(str_detect(name, pattern = "theta")) %>%
  dplyr::mutate(time = str_extract(name, pattern = "\\d+") %>% as.numeric()) %>%
  dplyr::arrange(time) %>%
  dplyr::select(time, MAP) %>%
  dplyr::mutate(FLG = if_else(MAP > .5, 1, 0))

## プロット
g <- dat1 %>%
  bind_cols(timeCheck) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  ggplot(aes(x = date, y = weight, color = as.factor(FLG))) +
  geom_point() +
  scale_x_date(date_breaks = "2 month") +
  geom_hline(yintercept = 81.2, color = 1) +
  geom_hline(yintercept = 83.1, color = 2) +
  theme(legend.position = "none")
g


# 変化点検出 -------------------------------------------------------------------
## データプロット
g <- dat %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::filter(date > "2021/01/01") %>%
  dplyr::filter(date < "2021/11/01") %>%
  ggplot(aes(x = date, y = weight)) +
  geom_point() +
  scale_x_date(date_breaks = "2 month")
g

## データ選別
dat2 <- dat %>%
  dplyr::filter(date > "2021/01/01") %>%
  dplyr::filter(date < "2021/11/01")

model <- cmdstanr::cmdstan_model("cmdstan/changePoint2.stan")
model$check_syntax()
dataSet <- list(L = NROW(dat2), W = dat2$weight)
fit <- model$sample(
  data = dataSet,
  chains = 4,
  parallel_chains = 4,
  seed = 12345,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit %>%
  MCMCtoDF() %>%
  dplyr::filter(str_detect(name, c("mu|sigma|tau"))) %>%
  MCMCsummary()


## 変化点検出
Est <-
  fit %>%
  MCMCtoDF() %>%
  MCMCsummary() %>%
  dplyr::select(name, MAP) %>%
  pivot_wider(names_from = name, values_from = MAP) %>%
  rename(mu1 = 1, mu2 = 2, sigma = 3, tau = 4)
## いつでしょう
dat2[145, ]
## プロット
g <- dat %>%
  dplyr::filter(date > "2021/01/01") %>%
  dplyr::filter(date < "2021/11/01") %>%
  rowid_to_column("Date") %>%
  ggplot(aes(x = Date, y = weight)) +
  geom_point() +
  geom_segment(x = 1, xend = Est$tau, y = Est$mu2, yend = Est$mu2, color = 2) +
  geom_segment(x = Est$tau, xend = dataSet$L, y = Est$mu1, yend = Est$mu1, color = 2) +
  geom_vline(xintercept = Est$tau, color = 3, linewidth = 2)
g

# 折線回帰 --------------------------------------------------------------------
## データプロット
g <- dat %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::filter(date > "2016/01/01") %>%
  dplyr::filter(date < "2016/12/01") %>%
  ggplot(aes(x = date, y = weight)) +
  geom_point() +
  scale_x_date(date_breaks = "2 month")
g
## データ選別
dat3 <- dat %>%
  dplyr::filter(date > "2016/01/01") %>%
  dplyr::filter(date < "2016/12/10") %>%
  tibble::rowid_to_column("cDate")


model <- cmdstanr::cmdstan_model("cmdstan/changePoint3.stan")
model$check_syntax()
dataSet <- list(L = NROW(dat3), X = dat3$cDate, W = dat3$weight)
fit <- model$sample(
  data = dataSet,
  chains = 4,
  parallel_chains = 4,
  seed = 8931,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit %>%
  MCMCtoDF() %>%
  dplyr::filter(str_detect(name, c("beta0|beta1|sigma|tau"))) %>%
  MCMCsummary()


## 変化点はいつ？
Est2 <-
  fit %>%
  MCMCtoDF() %>%
  MCMCsummary() %>%
  dplyr::select(name, MAP) %>%
  pivot_wider(names_from = name, values_from = MAP) %>%
  rename(beta0a = 1, beta0b = 2)

## プロット
g <- ggplot() +
  geom_point(data = dat3, aes(x = cDate, y = weight)) +
  stat_function(fun = function(x) Est2$beta0a + Est2$beta1a * x, aes(color = palette()[2]), lty = 2, lwd = 2) +
  stat_function(fun = function(x) Est2$beta0b + Est2$beta1b * x, aes(color = palette()[2]), lty = 2, lwd = 2) +
  geom_vline(xintercept = Est2$tau, color = 3, lwd = 2)
g


# 折れ線ひっつけモデル ---------------------------------------------------------------


model <- cmdstanr::cmdstan_model("cmdstan/changePoint3b.stan")
model$check_syntax()
dataSet <- list(L = NROW(dat3), X = dat3$cDate, W = dat3$weight)
fit <- model$sample(
  data = dataSet,
  chains = 4,
  parallel_chains = 4,
  seed = 8931,
  iter_warmup = 1000,
  iter_sampling = 5000
)

## 折れたのはいつ？
Est2b <-
  fit %>%
  MCMCtoDF() %>%
  MCMCsummary() %>%
  dplyr::select(name, MAP) %>%
  pivot_wider(names_from = name, values_from = MAP) %>%
  rename(beta0a = 1, beta0b = 2)
## プロット
g <- ggplot() +
  geom_point(data = dat3, aes(x = cDate, y = weight)) +
  stat_function(
    xlim = c(1, Est2b$tau),
    fun = function(x) Est2b$beta0a + Est2b$beta1a * x, aes(color = palette()[2]), lty = 2, lwd = 2
  ) +
  stat_function(
    xlim = c(Est2b$tau, NROW(dat3)),
    fun = function(x) Est2b$beta0b + Est2b$beta1b * x, aes(color = palette()[2]), lty = 2, lwd = 2
  ) +
  geom_vline(xintercept = Est2b$tau, color = 3, lwd = 2) +
  theme(legend.position = "none")
g

# 課題 ----------------------------------------------------------------------

dat3.kadai <- dat %>%
  dplyr::filter(date > "2015/01/01") %>%
  dplyr::filter(date < "2015/12/10") %>%
  tibble::rowid_to_column("cDate")
g <- dat3.kadai %>%
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x = date, y = weight)) +
  geom_point() +
  scale_x_date(date_labels = "%m/%d")

g
