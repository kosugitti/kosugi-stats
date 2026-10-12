# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 14. State Space Model Rstan version.
## テキスト第16講のコード(テキストは cmdstanr で書いている。ここではそれを rstan で書き直したもの)

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(bayesplot)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
## 補助関数(map_estimation・MCMCtoDF・MCMCsummary)を読み込む
## 関数集はサイトの「データ解析応用」ページの「事前準備」からダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく。Stanファイルも同じフォルダに置く
source("00_Appendix2_functions_rstan.R")

# データ読み込み -----------------------------------------------------------------

dat <- read_csv("Weight.csv")
## 素描
g <- dat %>%
  dplyr::filter(date > "2020/01/01") %>%
  ggplot(aes(x = date, y = weight)) +
  geom_point()
g

# 状態空間モデル -----------------------------------------------------------------

dat1 <- dat %>%
  filter(date > "2020/01/01") %>%
  mutate(date = as.Date(date))

model <- rstan::stan_model("StateSpace.stan")
dataSet <- list(L = NROW(dat1), W = dat1$weight)
fit1 <- rstan::sampling(model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)

## MCMCサンプルを変数ごとに要約する(テキストのコード16.2と同じ形)
fit1.df <- fit1 %>%
  as.array() %>%
  posterior::as_draws_df() %>%
  as_tibble() %>%
  select(-.chain, -.iteration, -.draw) %>%
  rowid_to_column("iter") %>%
  pivot_longer(-iter, names_to = "Varname") %>%
  group_by(Varname) %>%
  summarise(
    EAP = mean(value),
    MED = median(value),
    MAP = map_estimation(value),
    SD = sd(value),
    L95 = quantile(value, probs = 0.025),
    L50 = quantile(value, probs = 0.25),
    U50 = quantile(value, probs = 0.75),
    U95 = quantile(value, probs = 0.975)
  )



Est1 <- fit1.df %>%
  dplyr::filter(str_detect(Varname, "mu")) %>%
  dplyr::mutate(ID = str_extract(Varname, pattern = "\\d+") %>% 
  as.numeric()) %>%
  dplyr::arrange(ID)

g <- dat1 %>%
  tibble::rowid_to_column("ID") %>%
  dplyr::left_join(Est1, by = "ID") %>%
  ggplot(aes(x = ID, y = weight, ymin = U95, ymax = L95)) +
  geom_point() +
  geom_point(aes(x = ID, y = MAP), color = palette()[2]) +
  geom_ribbon(fill = palette()[3], alpha = 0.2)
plot(g)

# 日付は連続か？ -----------------------------------------------------------------

dat1 %>%
  dplyr::mutate(lag = lag(date)) %>%
  dplyr::mutate(date = as.Date(date), lag = as.Date(lag)) %>%
  dplyr::mutate(FLG = date - lag) %>%
  dplyr::filter(FLG > 1)

fullDays <- 
  data.frame(date = as.Date("2020/01/01"):as.Date("2021/12/01")) %>%
  mutate(date = as.Date(date, origin = "1970-01-01")) %>%
  left_join(dat1, by = "date") %>%
  tidyr::replace_na(list(weight = 999, bodyFat = 999))

model <- rstan::stan_model("StateSpace2.stan")
dataSet <- list(L = NROW(fullDays), W = fullDays$weight, 
                Nmiss = sum(fullDays$weight == 999))
fit2 <- rstan::sampling(model,
  data = dataSet,
  chains = 4
)

## MCMCサンプルを変数ごとに要約する(テキストのコード16.2と同じ形)
fit2.df <- fit2 %>%
  as.array() %>%
  posterior::as_draws_df() %>%
  as_tibble() %>%
  select(-.chain, -.iteration, -.draw) %>%
  rowid_to_column("iter") %>%
  pivot_longer(-iter, names_to = "Varname") %>%
  group_by(Varname) %>%
  summarise(
    EAP = mean(value),
    MED = median(value),
    MAP = map_estimation(value),
    SD = sd(value),
    L95 = quantile(value, probs = 0.025),
    L50 = quantile(value, probs = 0.25),
    U50 = quantile(value, probs = 0.75),
    U95 = quantile(value, probs = 0.975)
  )


Est2 <- fit2.df %>%
  dplyr::filter(str_detect(Varname, "mu")) %>%
  dplyr::filter(!str_detect(Varname, "muZero")) %>%
  dplyr::mutate(ID = str_extract(Varname, pattern = "\\d+") %>% 
  as.numeric()) %>%
  dplyr::arrange(ID) %>%
  dplyr::select(ID, MAP, U95, L95)
Est2miss <- fit2.df %>%
  dplyr::filter(str_detect(Varname, "Miss_W")) %>%
  dplyr::mutate(ID = str_extract(Varname, pattern = "\\d+") %>% 
  as.numeric()) %>%
  dplyr::arrange(ID) %>%
  dplyr::select(ID, MAP, U95, L95)

### plot用の関数を準備
plotFunction <- function(fullDays, Est, MissEst) {
  tmp <- fullDays %>%
    rowid_to_column("ID") %>%
    left_join(Est, by = "ID") %>%
    rowwise() %>%
    mutate(FLG = if (weight != 999) {1} else {2})
  misJ <- 1
  tmp$weight2 <- NA
  tmp$weight2U <- NA
  tmp$weight2L <- NA
  for (i in 1:NROW(tmp)) {
    if (tmp$FLG[i] == 2) {
      tmp$weight2[i] <- MissEst$MAP[misJ]
      tmp$weight2U[i] <- MissEst$U95[misJ]
      tmp$weight2L[i] <- MissEst$L95[misJ]
      misJ <- misJ + 1
    } else {
      tmp$weight2[i] <- tmp$weight[i]
      tmp$weight2U[i] <- tmp$weight[i]
      tmp$weight2L[i] <- tmp$weight[i]
    }
  }
  return(tmp)
}

plot.tmp <- plotFunction(fullDays, Est2, Est2miss)
## プロット
g <- ggplot(data = plot.tmp) +
  geom_point(aes(x = date, y = weight2)) +
  geom_errorbar(aes(x = date, y = weight2, ymin = weight2L, ymax = weight2U, color = palette()[2])) +
  geom_point(aes(x = date, y = MAP, color = palette()[3])) +
  geom_errorbar(aes(x = date, y = MAP, ymin = L95, ymax = U95, color = palette()[4])) +
  scale_x_date(date_breaks = "1 month", limits = as.Date(c("2020-01-01", "2020-05-01"))) +
  theme(legend.position = "none")
g


# 未来も欠損じゃない？ --------------------------------------------------------------

dat1 %>%
  dplyr::mutate(lag = lag(date)) %>%
  dplyr::mutate(date = as.Date(date), lag = as.Date(lag)) %>%
  dplyr::mutate(FLG = date - lag) %>%
  dplyr::filter(FLG > 1)

fullDays <- data.frame(date = as.Date("2020/01/01"):as.Date("2021/12/31")) %>%
  dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
  dplyr::left_join(dat1, by = "date") %>%
  tidyr::replace_na(list(weight = 999))

dataSet <- list(L = NROW(fullDays), W = fullDays$weight, 
                Nmiss = sum(fullDays$weight == 999))
fit3 <- rstan::sampling(model,
  data = dataSet,
  chains = 4
)

## MCMCサンプルを変数ごとに要約する(テキストのコード16.2と同じ形)
fit3.df <- fit3 %>%
  as.array() %>%
  posterior::as_draws_df() %>%
  as_tibble() %>%
  select(-.chain, -.iteration, -.draw) %>%
  rowid_to_column("iter") %>%
  pivot_longer(-iter, names_to = "Varname") %>%
  group_by(Varname) %>%
  summarise(
    EAP = mean(value),
    MED = median(value),
    MAP = map_estimation(value),
    SD = sd(value),
    L95 = quantile(value, probs = 0.025),
    L50 = quantile(value, probs = 0.25),
    U50 = quantile(value, probs = 0.75),
    U95 = quantile(value, probs = 0.975)
  )




Est3 <- fit3.df %>%
  dplyr::filter(str_detect(Varname, "mu")) %>%
  dplyr::filter(!str_detect(Varname, "muZero")) %>%
  dplyr::mutate(ID = str_extract(Varname, pattern = "\\d+") %>% 
  as.numeric()) %>%
  dplyr::arrange(ID) %>%
  dplyr::select(ID, MAP, U95, L95)
Est3miss <- fit3.df %>%
  dplyr::filter(str_detect(Varname, "Miss_W")) %>%
  dplyr::mutate(ID = str_extract(Varname, pattern = "\\d+") %>% 
  as.numeric()) %>%
  dplyr::arrange(ID) %>%
  dplyr::select(ID, MAP, U95, L95)


plot.tmp <- plotFunction(fullDays, Est3, Est3miss)
## 予想される2021年末の体重(テキストのコード16.8)
plot.tmp %>% tail()

g <- ggplot(data = plot.tmp) +
  geom_point(aes(x = date, y = weight2)) +
  geom_errorbar(aes(x = date, y = weight2, ymin = weight2L, ymax = weight2U, color = palette()[2])) +
  geom_point(aes(x = date, y = MAP, color = palette()[3])) +
  geom_errorbar(aes(x = date, y = MAP, ymin = L95, ymax = U95, color = palette()[4])) +
  scale_x_date(date_breaks = "1 month", limits = as.Date(c("2021-05-01", "2021-12-31"))) +
  theme(legend.position = "none")
plot(g)

# 二階差分 --------------------------------------------------------------------

model <- rstan::stan_model("StateSpace3.stan")
fit4 <- rstan::sampling(
  model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)

## MCMCサンプルを変数ごとに要約する(テキストのコード16.2と同じ形)
fit4.df <- fit4 %>%
  as.array() %>%
  posterior::as_draws_df() %>%
  as_tibble() %>%
  select(-.chain, -.iteration, -.draw) %>%
  rowid_to_column("iter") %>%
  pivot_longer(-iter, names_to = "Varname") %>%
  group_by(Varname) %>%
  summarise(
    EAP = mean(value),
    MED = median(value),
    MAP = map_estimation(value),
    SD = sd(value),
    L95 = quantile(value, probs = 0.025),
    L50 = quantile(value, probs = 0.25),
    U50 = quantile(value, probs = 0.75),
    U95 = quantile(value, probs = 0.975)
  )


Est4 <- fit4.df %>%
  dplyr::filter(str_detect(Varname, "mu")) %>%
  dplyr::filter(!str_detect(Varname, "muZero")) %>%
  dplyr::mutate(ID = str_extract(Varname, pattern = "\\d+") %>% 
  as.numeric()) %>%
  dplyr::arrange(ID) %>%
  dplyr::select(ID, MAP, U95, L95)
Est4miss <- fit4.df %>%
  dplyr::filter(str_detect(Varname, "Miss_W")) %>%
  dplyr::mutate(ID = str_extract(Varname, pattern = "\\d+") %>% 
  as.numeric()) %>%
  dplyr::arrange(ID) %>%
  dplyr::select(ID, MAP, U95, L95)


plot.tmp <- plotFunction(fullDays, Est4, Est4miss)
## 2階差分トレンドで予想される2021年末の体重(テキストのコード16.10)
plot.tmp %>% tail()

g <- ggplot(data = plot.tmp) +
  geom_point(aes(x = date, y = weight2)) +
  geom_errorbar(aes(x = date, y = weight2, ymin = weight2L, ymax = weight2U, color = palette()[2])) +
  geom_point(aes(x = date, y = MAP, color = palette()[3])) +
  geom_errorbar(aes(x = date, y = MAP, ymin = L95, ymax = U95, color = palette()[4])) +
  scale_x_date(date_breaks = "1 month", limits = as.Date(c("2021-05-01", "2021-12-31"))) +
  theme(legend.position = "none")

plot(g)
