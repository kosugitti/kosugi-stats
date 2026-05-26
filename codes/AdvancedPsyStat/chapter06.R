# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(cmdstanr, bayestestR, ggplot2)

# 2群の平均値の差の検定（第6章のデータ）-----------------------------------

# データの入力
群1 <- c(51, 45, 33, 45, 67, 73, 55, 43, 52, 61)
群2 <- c(32, 45, 39, 39, 29, 42, 28, 34, 24, 31)

# t検定の実行
result <- t.test(群1, 群2, var.equal = TRUE)
print(result)

# 基本統計量の表示
cat("\n基本統計量:\n")
cat("群1: 平均 =", mean(群1), ", 標準偏差 =", sd(群1), "\n")
cat("群2: 平均 =", mean(群2), ", 標準偏差 =", sd(群2), "\n")
cat("平均値の差 =", mean(群1) - mean(群2), "\n")

# Stanモデルコードの作成 --------------------------------------------------

stan_code <- "
data {
  int<lower=0> N1;  // 群1のサンプルサイズ
  int<lower=0> N2;  // 群2のサンプルサイズ
  vector[N1] y1;    // 群1のデータ
  vector[N2] y2;    // 群2のデータ
}

parameters {
  real mu1;         // 群1の平均
  real mu2;         // 群2の平均
  real<lower=0> sigma1;  // 群1の標準偏差
  real<lower=0> sigma2;  // 群2の標準偏差
}

transformed parameters {
  real delta;       // 平均値の差
  delta = mu1 - mu2;
}

model {
  // 事前分布
  mu1 ~ uniform(0, 100);
  mu2 ~ uniform(0, 100);
  sigma1 ~ uniform(0, 1000);
  sigma2 ~ uniform(0, 1000);

  // 尤度
  y1 ~ normal(mu1, sigma1);
  y2 ~ normal(mu2, sigma2);
}

generated quantities {
  vector[N1] y1_rep;
  vector[N2] y2_rep;
  real d1;
  real d2;
  real d_common;

  vector[N1] log_lik1;
  vector[N2] log_lik2;

  d1 = (mu1 - mu2)/sigma1;
  d2 = (mu1 - mu2)/sigma2;
  d_common = (mu1 - mu2)/sqrt(((N1 - 1)*sigma1^2 + (N2 - 1)*sigma2^2)/(N1 + N2 - 2));

  for (n in 1:N1) {
    y1_rep[n] = normal_rng(mu1, sigma1);
    log_lik1[n] = normal_lpdf(y1[n] | mu1, sigma1);
  }
  for (n in 1:N2) {
    y2_rep[n] = normal_rng(mu2, sigma2);
    log_lik2[n] = normal_lpdf(y2[n] | mu2, sigma2);
  }
}
"

# 帰無仮説モデル
stan_code_null <- "
data {
  int<lower=0> N1;
  int<lower=0> N2;
  vector[N1] y1;
  vector[N2] y2;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  mu ~ uniform(0, 100);
  sigma ~ uniform(0, 1000);

  y1 ~ normal(mu, sigma);
  y2 ~ normal(mu, sigma);
}

generated quantities {
  vector[N1] log_lik1;
  vector[N2] log_lik2;

  for (n in 1:N1) {
    log_lik1[n] = normal_lpdf(y1[n] | mu, sigma);
  }
  for (n in 1:N2) {
    log_lik2[n] = normal_lpdf(y2[n] | mu, sigma);
  }
}
"

# Stanファイルの書き出し
dir.create("source/stan", recursive = TRUE, showWarnings = FALSE)
writeLines(stan_code, "source/stan/two_group_model.stan")
writeLines(stan_code_null, "source/stan/two_group_model_null.stan")

# モデルのコンパイル
model <- cmdstan_model("source/stan/two_group_model.stan")
model_null <- cmdstan_model("source/stan/two_group_model_null.stan")

# データの準備
stan_data <- list(
  N1 = length(群1),
  N2 = length(群2),
  y1 = 群1,
  y2 = 群2
)

# MCMCサンプリング実行（初回は数分かかります）
cat("\nMCMCサンプリング実行中...\n")
fit <- model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 10000,
  refresh = 500
)

fit_null <- model_null$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 10000,
  refresh = 500
)

# 結果の表示
cat("\n========================================\n")
cat("ベイズ推定の結果:\n")
cat("========================================\n")
fit$summary(c("mu1", "mu2", "delta", "sigma1", "sigma2"))

# deltaの事後分布
delta_samples <- fit$draws("delta", format = "matrix")
delta_quantiles <- quantile(delta_samples, probs = c(0.025, 0.5, 0.975))

cat("\n平均値の差 (delta = mu1 - mu2) の事後分布:\n")
cat("  中央値:", delta_quantiles[2], "\n")
cat("  95%信用区間: [", delta_quantiles[1], ",", delta_quantiles[3], "]\n")

if (delta_quantiles[1] > 0) {
  cat("  → 95%信用区間が0を含まない (下限 > 0) ため，群1の平均が群2より大きいと判断できる\n")
} else {
  cat("  → 95%信用区間が0を含むため，平均値の差があるとは言えない\n")
}

# deltaが0より大きい確率
prob_delta_positive <- mean(delta_samples > 0)
cat("\nP(delta > 0 | data) =", prob_delta_positive, "\n")

# pd (Probability of Direction)
pd_delta <- p_direction(as.vector(delta_samples))
cat("pd (Probability of Direction) =", pd_delta$pd, "\n")

# 事後分布の統計量計算
delta_vec <- as.vector(delta_samples)
EAP <- mean(delta_vec)
MED <- median(delta_vec)
MAP <- map_estimate(delta_vec)$MAP_Estimate
HDI <- hdi(delta_vec, ci = 0.95)
HDI_lower <- HDI$CI_low
HDI_upper <- HDI$CI_high

cat("\n========================================\n")
cat("事後分布の要約統計量:\n")
cat("========================================\n")
cat("EAP (事後平均):", EAP, "\n")
cat("MED (事後中央値):", MED, "\n")
cat("MAP (最頻値):", MAP, "\n")
cat("95% HDI: [", HDI_lower, ",", HDI_upper, "]\n")

# 図6.1: 事後分布プロット ---------------------------------------------------

df_delta <- data.frame(delta = delta_vec)

# 図6.1: δの事後分布（EAP・MED・MAP・95% HDI）
p_posterior <- ggplot(df_delta, aes(x = delta)) +
  geom_density(fill = "gray80", alpha = 0.5, color = "black", linewidth = 1) +
  geom_vline(aes(xintercept = EAP, color = "EAP"),
             linewidth = 1, linetype = "solid") +
  geom_vline(aes(xintercept = MED, color = "MED"),
             linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = MAP, color = "MAP"),
             linewidth = 1, linetype = "dotted") +
  geom_segment(aes(x = HDI_lower, xend = HDI_upper, y = 0, yend = 0,
                   color = "95% HDI"),
               linewidth = 3, lineend = "round") +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5,
             linetype = "twodash") +
  scale_color_manual(
    name = "",
    values = c("EAP" = "black", "MED" = "gray30", "MAP" = "gray50",
               "95% HDI" = "gray60"),
    labels = c(
      "EAP" = sprintf("EAP = %.2f", EAP),
      "MED" = sprintf("MED = %.2f", MED),
      "MAP" = sprintf("MAP = %.2f", MAP),
      "95% HDI" = sprintf("95%% HDI = [%.2f, %.2f]", HDI_lower, HDI_upper)
    )
  ) +
  labs(
    x = "δ",
    y = "密度"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10)
  )
print(p_posterior)

# ベイズファクターの計算 ---------------------------------------------------
log_lik1_M1 <- fit$draws("log_lik1", format = "matrix")
log_lik2_M1 <- fit$draws("log_lik2", format = "matrix")
log_lik_M1 <- cbind(log_lik1_M1, log_lik2_M1)

log_lik1_M0 <- fit_null$draws("log_lik1", format = "matrix")
log_lik2_M0 <- fit_null$draws("log_lik2", format = "matrix")
log_lik_M0 <- cbind(log_lik1_M0, log_lik2_M0)

log_lik_M1_sum <- rowSums(log_lik_M1)
log_ml_M1 <- -log(mean(exp(-log_lik_M1_sum)))

log_lik_M0_sum <- rowSums(log_lik_M0)
log_ml_M0 <- -log(mean(exp(-log_lik_M0_sum)))

cat("\n【周辺尤度 (Marginal Likelihood)】\n")
cat("log p(D|M1) ≈", log_ml_M1, "\n")
cat("log p(D|M0) ≈", log_ml_M0, "\n")

log_BF10 <- log_ml_M1 - log_ml_M0
BF10 <- exp(log_BF10)
BF01 <- 1 / BF10

cat("\n【ベイズファクター】\n")
cat("log BF10 =", log_BF10, "\n")
cat("BF10 (M1/M0) =", BF10, "\n")
cat("BF01 (M0/M1) =", BF01, "\n")
