# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 10. GLMM Rstan Version

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

baseball <- read_csv("baseballDecade.csv")

dat <- baseball %>%
  filter(position == "投手") %>%
  filter(team == "Swallows") %>%
  group_by(Name) %>%
  nest() %>%
  mutate(
    n = purrr::map_dbl(data, ~ NROW(.)),
    FLG = purrr::map_lgl(data, ~ anyNA(.$Win))
  ) %>%
  filter(n > 7) %>%
  filter(!FLG) %>%
  unnest(data) %>%
  select(Year, Name, salary, Win)

# GLMM --------------------------------------------------------------------

### ポアソン分布で個体差を入れて
### データの加工
dat.tmp <- dat %>%
  dplyr::mutate(salary = salary / 1000) %>%
  dplyr::mutate(ID = as.factor(Name)) %>%
  dplyr::mutate(ID = as.numeric(ID))

dataSet <- list(
  L = NROW(dat.tmp),
  N = max(dat.tmp$ID),
  X = dat.tmp$salary,
  Y = dat.tmp$Win,
  index = dat.tmp$ID
)

### コンパイルと推定
model <- rstan::stan_model("rstan/glmm_poisson.stan")
fit <- rstan::sampling(model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)

### 結果の出力
fit %>%
  MCMCtoDF() %>%
  MCMCsummary()



# 課題1，打率のデータ整形 ------------------------------------------------------------

dat <- baseball %>%
  dplyr::filter(position != "投手") %>%
  dplyr::filter(team == "Softbank") %>%
  dplyr::filter(salary > 2500) %>%
  dplyr::select(Year, Name, salary, AtBats, Hit, Games, HR)

model <- cmdstanr::cmdstan_model("cmdstan/glmm_binomial.stan")

dat.tmp <- dat %>%
  dplyr::mutate(salary = salary / 1000) %>%
  dplyr::mutate(ID = as.factor(Name)) %>%
  dplyr::mutate(ID = as.numeric(ID))

dataSet <- list(
  L = NROW(dat.tmp),
  N = max(dat.tmp$ID),
  index = dat.tmp$ID,
  X = dat.tmp$salary,
  Y = dat.tmp$HR,
  H = dat.tmp$Hit
)

fit <- rstan::sampling(
  model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()

# 課題2，階層線形モデル -------------------------------------------------------------

pitcher <- baseball %>%
  dplyr::filter(position == "投手") %>%
  dplyr::filter(salary > 1000) %>%
  dplyr::mutate(salary = salary / 1000) %>%
  dplyr::select(Name, team, salary, Win)

dat.tmp <- pitcher %>%
  dplyr::mutate(
    NameID = as.factor(Name),
    teamID = as.factor(team)
  ) %>%
  dplyr::mutate(
    NameID = as.numeric(NameID),
    teamID = as.numeric(teamID)
  )

dataSet <- list(
  L = NROW(dat.tmp),
  G = max(dat.tmp$teamID),
  Gindex = dat.tmp$teamID,
  X = dat.tmp$salary,
  Y = dat.tmp$Win
)

model <- rstan::stan_model("rstan/hlm_poisson.stan")

fit_glmm <- rstan::sampling(
  model,
  data = dataSet,
  iter = 6000,
  warmup = 1000
)

fit %>%
  MCMCtoDF() %>%
  dplyr::filter(name %in% c("gamma0", "gamma1", "tau0", "tau1")) %>%
  MCMCsummary()
