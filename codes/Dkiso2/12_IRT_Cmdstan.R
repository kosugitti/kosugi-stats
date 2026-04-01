# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 12. IRT models Cmdstan version.

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



# データの読み込み ----------------------------------------------------------------

dat <- read_csv("IRTsample.csv")

# Plotting functions ------------------------------------------------------

oneParameters <- function(x, b) {
  1 / (1 + exp(-1.7 * (x - b)))
}


twoParameters <- function(x, a, b) {
  1 / (1 + exp(-1.7 * a * (x - b)))
}

threeParameters <- function(x, a = 1, b = 0, c = 0) {
  c + (1 - c) / (1 + exp(-1.7 * a * (x - b)))
}


# 理論的プロット -----------------------------------------------------------------

p <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  xlab("theta")
g1 <- p +
  stat_function(fun = oneParameters, args = list(b = 1), color = palette()[2]) +
  stat_function(fun = oneParameters, args = list(b = -1), color = palette()[3]) +
  stat_function(fun = oneParameters, args = list(b = 2), color = palette()[4]) + ylab("pass") +
  ggtitle("1PL model.困難度母数はそれぞれ1，-1，2")
g2 <- p + stat_function(fun = twoParameters, args = list(a = 1, b = 1), color = palette()[2]) +
  stat_function(fun = twoParameters, args = list(a = 0.8, b = -1), color = palette()[3]) +
  stat_function(fun = twoParameters, args = list(a = 2, b = 2), color = palette()[4]) + ylab("pass") +
  ggtitle("2PL model.識別力母数はそれぞれ1，0.8，1.2")
g3 <- p + stat_function(fun = threeParameters, args = list(a = 1, b = 1, c = 0), color = palette()[2]) +
  stat_function(fun = threeParameters, args = list(a = 0.8, b = -1, c = 0.2), color = palette()[3]) +
  stat_function(fun = threeParameters, args = list(a = 1.2, b = 2, c = 0.3), color = palette()[4]) + ylab("pass") +
  ggtitle("3PL model.当て推量母数はそれぞれ0,0.2,0.3")

plot(g1)
plot(g2)
plot(g3)


# Stanによる推定 ---------------------------------------------------------------

model_1pl <- cmdstan_model("cmdstan/oneParameter.stan")
model_2pl <- cmdstan_model("cmdstan/twoParameters.stan")
model_3pl <- cmdstan_model("cmdstan/threeParameters.stan")

model_1pl$check_syntax()
model_2pl$check_syntax()
model_3pl$check_syntax()


dataSet <- list(N = NROW(dat), M = NCOL(dat), resp = as.matrix(dat))
fit1 <- model_1pl$sample(data = dataSet, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 5000)
fit2 <- model_2pl$sample(data = dataSet, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 5000)
fit3 <- model_3pl$sample(data = dataSet, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 5000)

fit1 %>%
  MCMCtoDF() %>%
  MCMCsummary()
fit2 %>%
  MCMCtoDF() %>%
  MCMCsummary()
fit3 %>%
  MCMCtoDF() %>%
  MCMCsummary()


tbl1 <-
  fit1 %>%
  MCMCtoDF() %>%
  MCMCsummary() %>%
  dplyr::select(name, EAP) %>%
  dplyr::filter(str_detect(name, pattern = "[a-b]\\[\\d+\\]")) %>%
  dplyr::filter(!str_detect(name, pattern = "theta")) %>%
  dplyr::mutate(Parameters = str_extract(name, pattern = "[ab]")) %>%
  dplyr::mutate(Qid = str_extract(name, "\\d+") %>% as.numeric()) %>%
  dplyr::select(-name) %>%
  tidyr::pivot_wider(id_cols = Qid, names_from = Parameters, values_from = EAP) %>%
  dplyr::rename(one_b = b)

tbl2 <-
  fit2 %>%
  MCMCtoDF() %>%
  MCMCsummary() %>%
  dplyr::select(name, EAP) %>%
  dplyr::filter(str_detect(name, pattern = "[a-b]\\[\\d+\\]")) %>%
  dplyr::filter(!str_detect(name, pattern = "theta")) %>%
  dplyr::mutate(Parameters = str_extract(name, pattern = "[ab]")) %>%
  dplyr::mutate(Qid = str_extract(name, "\\d+") %>% as.numeric()) %>%
  dplyr::select(-name) %>%
  tidyr::pivot_wider(id_cols = Qid, names_from = Parameters, values_from = EAP) %>%
  dplyr::rename(two_b = b, two_a = a)

tbl3 <-
  fit3 %>%
  MCMCtoDF() %>%
  MCMCsummary() %>%
  dplyr::select(name, EAP) %>%
  dplyr::filter(str_detect(name, pattern = "[a-c]\\[\\d+\\]")) %>%
  dplyr::filter(!str_detect(name, pattern = "theta")) %>%
  dplyr::mutate(Parameters = str_extract(name, pattern = "[abc]")) %>%
  dplyr::mutate(Qid = str_extract(name, "\\d+") %>% as.numeric()) %>%
  dplyr::select(-name) %>%
  tidyr::pivot_wider(id_cols = Qid, names_from = Parameters, values_from = EAP) %>%
  dplyr::rename(three_a = a, three_b = b, three_c = c)

tbl1 %>%
  dplyr::left_join(tbl2, by = "Qid") %>%
  dplyr::left_join(tbl3, by = "Qid") %>%
  dplyr::arrange(Qid) %>%
  print()

# 推定値プロット -----------------------------------------------------------------

g1 <- p <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  xlab("theta") +
  ggtitle("1PL model")
for (i in 1:10) {
  g1 <- g1 +
    stat_function(fun = oneParameters, args = list(b = tbl1$one_b[i]), color = palette()[i])
}
plot(g1)

g2 <- p <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  xlab("theta") +
  ggtitle("2PL model")
for (i in 1:10) {
  g2 <- g2 +
    stat_function(fun = twoParameters, args = list(a = tbl2$two_a[i], b = tbl2$two_b[i]), color = palette()[i])
}
plot(g2)

g3 <- p <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  xlab("theta") +
  ggtitle("3PL model")
for (i in 1:10) {
  g3 <- g3 +
    stat_function(fun = threeParameters, args = list(a = tbl3$three_a[i], b = tbl3$three_b[i], c = tbl3$three_c[i]), color = palette()[i])
}
plot(g3)

# Tidy Data ---------------------------------------------------------------

dat.tmp <- dat %>%
  tibble::rowid_to_column("Pid") %>%
  tidyr::pivot_longer(-Pid) %>%
  dplyr::mutate(Qid = str_extract(name, pattern = "\\d+") %>% as.numeric()) %>%
  dplyr::select(Pid, Qid, value)

# わざと欠測値を与える
dat.tmp$value[1] <- NA
dat.tmp$value[11:13] <- NA
# 確認
dat.tmp %>% head(20)

dat.tmp <- na.omit(dat.tmp)
dataSet <- list(
  L = NROW(dat.tmp), N = max(dat.tmp$Pid), M = max(dat.tmp$Qid),
  Pid = dat.tmp$Pid, Qid = dat.tmp$Qid,
  resp = dat.tmp$value
)

model_2pl_ver2 <- cmdstan_model("cmdstan/twoParameters2.stan")
model_2pl_ver2$check_syntax()
fit2.2 <- model_2pl_ver2$sample(
  data = dataSet,
  chains = 4,
  parallel_chains = 4
)

fit2.2 %>%
  MCMCtoDF() %>%
  dplyr::filter(str_detect(name, c("theta\\[1\\]|theta\\[2\\]|theta\\[3\\]"))) %>%
  MCMCsummary()
