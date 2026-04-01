# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 9. GLM Rstan Version

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


# LM ----------------------------------------------------------------------

dat <- read_csv("BaseballDecade.csv") %>%
  filter(Year == "2020年度")
dataSet <- list(N = NROW(dat), Y = dat$height, X = dat$weight)
dat %>% ggplot(aes(x = weight, y = height)) +
  geom_point() +
  geom_smooth(formula = "y~x", method = "lm", se = FALSE)

model <- rstan::stan_model("rstan/LM.stan")
fit <- sampling(model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)


## 簡易表示
print(fit, pars = c("beta0", "beta1", "sig"))
## MLと比較
fitML <- lm(height ~ weight, data = dat)
summary(fitML)

predY <- rstan::extract(fit)$predY
# 事後予測分布の描画
bayesplot::ppc_dens_overlay(y = dataSet$Y, yrep = predY[1:10, ])
bayesplot::ppc_intervals(
  y = dataSet$Y,
  yrep = predY,
  x = dataSet$X,
  prob = 0.5,
  prob_outer = 0.95
)




# ロジスティック回帰 ---------------------------------------------------------------

dat2 <- dat %>%
  dplyr::mutate(Pitcher = if_else(position == "投手", 0, 1)) %>%
  dplyr::filter(salary > 5000) %>%
  dplyr::select(Games, Pitcher) %>%
  na.omit()
dat2 %>%
  ggplot(aes(x = Games, y = Pitcher)) +
  geom_point() +
  geom_smooth(
    method = "glm", se = FALSE,
    method.args = list(family = binomial(link = "logit"))
  )

model <- rstan::stan_model("rstan/logistic.stan")
dataSet <- list(N = NROW(dat2), Y = dat2$Pitcher, X = dat2$Games)

fit.logistic <- rstan::sampling(model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)

## 簡易表示
print(fit.logistic, pars = c("beta0", "beta1"))

## プロットを考える
### 関数準備
plotFunc <- function(x, b0, b1) {
  tmp <- b0 + b1 * x
  ret <- 1 / (1 + exp(-tmp))
  return(ret)
}

result <- fit.logistic %>%
  MCMCtoDF() %>%
  MCMCsummary()

dat2 %>%
  ggplot(aes(x = Games, y = Pitcher)) +
  geom_point() +
  stat_function(
    fun = plotFunc,
    args = list(b0 = result[1, ]$MAP, b1 = result[2, ]$MAP),
    color = palette()[4], lwd = 1.5
  ) +
  stat_function(
    fun = plotFunc,
    args = list(b0 = result[1, ]$U95, b1 = result[2, ]$U95),
    color = palette()[4], lty = 2, alpha = 0.5
  ) +
  stat_function(
    fun = plotFunc,
    args = list(b0 = result[1, ]$L95, b1 = result[2, ]$L95), color = palette()[4], lty = 2, alpha = 0.5
  )



# 課題 ----------------------------------------------------------------------

dat3 <- dat %>%
  dplyr::filter(position != "投手") %>%
  dplyr::select(AtBats, Hit, salary) %>%
  na.omit() %>%
  dplyr::mutate(salary.z = (salary - mean(salary)) / sd(salary)) %>%
  dplyr::select(-salary)

dataSet <- list(N = NROW(dat3), AB = dat3$AtBats, Hit = dat3$Hit, X = dat3$salary.z)
