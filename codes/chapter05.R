# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, MASS, patchwork, cmdstanr, posterior, bayesplot, bayestestR)

# 図5.1: 誤った回帰直線の例 -----------------------------------------------

set.seed(12345)
N <- 200
dat <- MASS::mvrnorm(N, mu = c(0, 0), Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2)) %>%
  as.data.frame() %>%
  rename(x = V1, y = V2)

cat("データの平均:\n")
cat("x:", mean(dat$x), "\n")
cat("y:", mean(dat$y), "\n")

# 図5.1: 誤った回帰直線の例（複数の不適切な直線）
g_wrong <- ggplot(dat, aes(x, y)) +
  geom_point() +
  theme_minimal() +
  geom_abline(intercept = -2, slope = 0.1, linetype = "dashed") +
  geom_abline(intercept = -0.2, slope = 3, linetype = "dotted") +
  geom_abline(intercept = 2, slope = -0.5, linetype = "dotdash")
print(g_wrong)

# 図5.2: ベイズ推定による事後分布 ------------------------------------------

# コインを10回投げて2回表が出た場合のベイズ推定
heads <- 2
tails <- 8
total <- heads + tails

# Stanモデルコード
stan_code <- "
data {
  int<lower=0> N;        // total number of flips
  int<lower=0> heads;    // number of heads
}
parameters {
  real<lower=0, upper=1> theta;  // probability of heads
}
model {
  // Prior
  theta ~ beta(1, 1);  // uniform prior

  // Likelihood
  heads ~ binomial(N, theta);
}
"

# Stanファイルの書き出し
dir.create("source/stan", recursive = TRUE, showWarnings = FALSE)
stan_file <- "source/stan/coin_flip.stan"
writeLines(stan_code, stan_file)

# モデルのコンパイル
mod <- cmdstan_model(stan_file)

# データの準備
stan_data <- list(
  N = total,
  heads = heads
)

# MCMCサンプリング実行（初回は数分かかります）
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500
)

# 事後サンプルの取得
draws <- fit$draws(format = "df")
theta_samples <- draws$theta

# 点推定値の計算
eap <- mean(theta_samples)
med <- median(theta_samples)
map_val <- map_estimate(theta_samples)$MAP
hdi_val <- hdi(theta_samples, ci = 0.95)

# 結果の出力
cat("Point estimates:\n")
cat(sprintf("EAP (Mean): %.4f\n", eap))
cat(sprintf("Median: %.4f\n", med))
cat(sprintf("MAP: %.4f\n", map_val))
cat(sprintf("95%% HDI: [%.4f, %.4f]\n", hdi_val$CI_low, hdi_val$CI_high))

# 図5.2: ベイズ推定による事後分布（EAP・MAP・95% HDI）
p_posterior <- ggplot(data.frame(theta = theta_samples), aes(x = theta)) +
  geom_density(fill = "gray80", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = eap, color = "EAP"), linewidth = 1) +
  geom_vline(aes(xintercept = med, color = "Median"), linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = map_val, color = "MAP"), linewidth = 1, linetype = "dotted") +
  geom_segment(aes(x = hdi_val$CI_low, xend = hdi_val$CI_high, y = 0, yend = 0,
                   color = "95% HDI"),
               linewidth = 3, lineend = "round") +
  scale_color_manual(name = "Estimates",
                     values = c("EAP" = "black", "Median" = "gray30",
                               "MAP" = "gray50", "95% HDI" = "gray60")) +
  labs(x = "求めるパラメータ",
       y = "密度") +
  theme_classic() +
  theme(legend.position = "right")
print(p_posterior)

# 推定サマリーの出力
fit$summary("theta")
