# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, exametrika, cmdstanr, bayestestR, bayesplot, posterior, lubridate)

# 図15.1: 項目反応関数（IRT）-----------------------------------------------

# θ（能力）の範囲を設定
theta_range <- seq(-4, 4, by = 0.01)

# 3つの項目パラメータを定義
items_irt <- data.frame(
  item = c("項目1", "項目2", "項目3"),
  alpha = c(1.0, 1.5, 0.8),    # 識別力
  gamma = c(0.0, 2.0, -2.0)    # 困難度
)

# 各項目の正答確率を計算
irt_data_plot <- expand_grid(
  theta = theta_range,
  item = items_irt$item
) |>
  left_join(items_irt, by = "item") |>
  mutate(
    # 2PLモデルの正答確率
    p = 1 / (1 + exp(-1.7 * alpha * (theta - gamma)))
  )

# 図15.1: 項目反応関数（3つの項目の正答確率曲線）
p_irf <- ggplot(irt_data_plot, aes(x = theta, y = p, color = item, linetype = item)) +
  geom_line(linewidth = 0.8) +
  labs(
    x = "能力 θ",
    y = "正答確率 P(θ)"
  ) +
  scale_color_manual(
    values = c("項目1" = "black", "項目2" = "black", "項目3" = "black")
  ) +
  scale_linetype_manual(
    values = c("項目1" = "solid", "項目2" = "dashed", "項目3" = "dotted")
  ) +
  scale_x_continuous(breaks = seq(-4, 4, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
print(p_irf)

# 七人の科学者モデル -------------------------------------------------------

scientist_data <- data.frame(
  scientist = 1:7,
  measurement = c(-27.020, 3.570, 8.191, 9.808, 9.063, 9.945, 10.056)
)

# Stanモデルコード（七人の科学者）
seven_stan_code <- "
data {
  int<lower=0> N;  // 科学者の数
  vector[N] x;     // 各科学者の測定値
}

parameters {
  real mu;          // 真の測定値
  vector<lower=0>[N] sigma;  // 各科学者の測定誤差
}

model {
  mu ~ normal(0, 100);       // 事前分布（平穏）
  sigma ~ cauchy(0, 10);     // 事前分布（半コーシー）

  x ~ normal(mu, sigma);     // 尤度
}

generated quantities {
  vector[N] x_rep;
  for (n in 1:N) {
    x_rep[n] = normal_rng(mu, sigma[n]);
  }
}
"

dir.create("source/stan", recursive = TRUE, showWarnings = FALSE)
writeLines(seven_stan_code, "source/stan/seven.stan")
cat("Stanモデルファイルを書き出しました: source/stan/seven.stan\n")

# モデルのコンパイル・推定
model_seven <- cmdstan_model("source/stan/seven.stan")

stan_data_seven <- list(
  N = nrow(scientist_data),
  x = scientist_data$measurement
)

cat("MCMCサンプリング実行中...\n")
fit_seven <- model_seven$sample(
  data = stan_data_seven,
  chains = 4,
  iter_warmup = 2000,
  iter_sampling = 20000,
  parallel_chains = 4,
  seed = 12345,
  refresh = 500
)

# 診断確認
fit_seven$diagnostic_summary()

# 結果の抽出
posterior_summary_seven <- fit_seven$summary()
print(posterior_summary_seven)

cat("=== 七人の科学者モデル - 分析結果 ===\n")
mu_summary <- posterior_summary_seven |> filter(variable == "mu")
cat("真の測定値 μ の推定:\n")
cat(sprintf("  EAP (事後平均): %.3f\n", mu_summary$mean))
cat(sprintf("  95%% 信用区間: [%.3f, %.3f]\n", mu_summary$q5, mu_summary$q95))

# IRTモデル（exametrikaパッケージのサンプルデータ使用）------------------

cat("\n=== IRT分析用データの基本統計 ===\n")
irt_data <- J15S500$U %>% head(20)
cat("データ形状:", dim(irt_data)[1], "名の受検者 x", dim(irt_data)[2], "項目\n")
cat("正答率:\n")
print(colMeans(irt_data))

# Stanコード（IRTモデル）
irt_stan_code <- "
data {
  int<lower=0> N;  // 受検者数
  int<lower=0> J;  // 項目数
  matrix[N, J] Y;  // 反応データ
}

parameters {
  vector[N] theta;          // 受検者能力
  vector[J] gamma;          // 項目困難度
}

model {
  theta ~ normal(0, 1);
  gamma ~ normal(0, 1);

  for (n in 1:N) {
    for (j in 1:J) {
      Y[n, j] ~ bernoulli_logit(theta[n] - gamma[j]);
    }
  }
}
"

writeLines(irt_stan_code, "source/stan/irt_raw.stan")
cat("IRTモデルのStanファイルを書き出しました: source/stan/irt_raw.stan\n")

model_irt <- cmdstan_model("source/stan/irt_raw.stan")

N_irt <- nrow(irt_data)
J_irt <- ncol(irt_data)
Y_irt <- as.matrix(irt_data)

stan_irt_data <- list(N = N_irt, J = J_irt, Y = Y_irt)

cat("IRTモデルのMCMCサンプリング実行中...\n")
fit_irt_raw <- model_irt$sample(
  data = stan_irt_data,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  parallel_chains = 4,
  seed = 12345,
  refresh = 500
)

posterior_summary_irt <- fit_irt_raw$summary()

cat("受検者能力 θ (最初の5名):\n")
for (i in 1:5) {
  theta_row <- posterior_summary_irt |> filter(variable == paste0("theta[", i, "]"))
  if (nrow(theta_row) > 0) {
    cat(sprintf("  θ_%d = %.3f [%.3f, %.3f]\n", i, theta_row$mean, theta_row$q5, theta_row$q95))
  }
}

cat("\n項目困難度 γ (最初の5項目):\n")
for (i in 1:5) {
  gamma_row <- posterior_summary_irt |> filter(variable == paste0("gamma[", i, "]"))
  if (nrow(gamma_row) > 0) {
    cat(sprintf("  γ_%d = %.3f [%.3f, %.3f]\n", i, gamma_row$mean, gamma_row$q5, gamma_row$q95))
  }
}

cat("\n=== IRT分析完了 ===\n")

# 図15.2: 体重散布図 -------------------------------------------------------
# 注記: 元のスクリプト（20251231chap15b.R）はchap15_Weights.csvを必要とするため，
# ここでは同等の仮想データを生成します。

# 2016年の体重変化データ（仮想）
set.seed(42)
n_days <- 300
day_number <- 1:n_days

# 変化点: 第187日目
changepoint <- 187

# 変化前（減少傾向）と変化後（増加傾向）
weight <- ifelse(
  day_number < changepoint,
  81.86 - 0.04 * day_number + rnorm(n_days, 0, 0.4),
  71.67 + 0.02 * day_number + rnorm(n_days, 0, 0.4)
)

w_sim <- data.frame(day_number = day_number, weight = weight)

# 図15.2: 体重の時系列散布図
p_scatter_weight <- ggplot(w_sim, aes(x = day_number, y = weight)) +
  geom_point(color = "black", alpha = 0.6, size = 0.8) +
  scale_x_continuous(
    breaks = seq(0, 300, by = 50),
    labels = function(x) paste0(x, "日目")
  ) +
  labs(x = "", y = "体重(kg)") +
  theme_minimal()
print(p_scatter_weight)

# 変化点検出モデルのStanコード
changepoint_stan_code <- "
data {
  int<lower=0> L;   // 観測数
  vector[L] w;      // 体重データ
}

parameters {
  real<lower=1, upper=L> tau;   // 変化点
  real beta0a;                  // 変化前の切片
  real beta1a;                  // 変化前の傾き
  real beta0b;                  // 変化後の切片
  real beta1b;                  // 変化後の傾き
  real<lower=0> sigma;          // 誤差の標準偏差
}

model {
  tau ~ uniform(1, L);
  beta0a ~ normal(80, 10);
  beta1a ~ normal(0, 0.1);
  beta0b ~ normal(80, 10);
  beta1b ~ normal(0, 0.1);
  sigma ~ exponential(1);

  for (t in 1:L) {
    if (t < tau) {
      w[t] ~ normal(beta0a + beta1a * t, sigma);
    } else {
      w[t] ~ normal(beta0b + beta1b * (t - tau + 1), sigma);
    }
  }
}
"
writeLines(changepoint_stan_code, "source/stan/changepoint.stan")
cat("変化点検出モデルのStanファイルを書き出しました: source/stan/changepoint.stan\n")

# 変化点検出モデルの推定（仮想データを使用）
model_cp <- cmdstan_model("source/stan/changepoint.stan")

dataSet_cp <- list(
  L = nrow(w_sim),
  w = w_sim$weight
)

cat("変化点検出モデルのMCMCサンプリング実行中...\n")
fit_cp <- model_cp$sample(
  data = dataSet_cp,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  seed = 12345,
  refresh = 500
)

# 変化点の推定値
Xday <- bayestestR::describe_posterior(fit_cp$draws("tau"),
  centrality = c("mean", "median", "MAP"), ci = 0.95,
  ci_method = "hdi", test = NULL
)

cat("=== 変化点検出結果 ===\n")
cat("変化点 (MAP): 第", round(Xday$MAP), "日目\n")
cat("95% HDI: [", round(Xday$CI_low), ",", round(Xday$CI_high), "] 日目\n")

# 回帰係数の推定値
summary_cp <- fit_cp$summary()
tau_mean <- summary_cp[summary_cp$variable == "tau", ]$mean
beta0a_mean <- summary_cp[summary_cp$variable == "beta0a", ]$mean
beta1a_mean <- summary_cp[summary_cp$variable == "beta1a", ]$mean
beta0b_mean <- summary_cp[summary_cp$variable == "beta0b", ]$mean
beta1b_mean <- summary_cp[summary_cp$variable == "beta1b", ]$mean

changepoint_map <- round(tau_mean)

regression_data <- tibble(
  day_number = 1:max(w_sim$day_number)
) %>%
  mutate(
    predicted_weight = case_when(
      day_number < changepoint_map ~ beta0a_mean + beta1a_mean * day_number,
      day_number >= changepoint_map ~ beta0b_mean + beta1b_mean * (day_number - changepoint_map + 1)
    )
  )

# 図15.3: 体重の時系列と変化点・回帰線
p_changepoint_final <- w_sim %>%
  ggplot(aes(x = day_number, y = weight)) +
  geom_point(color = "black", alpha = 0.6, size = 0.8) +
  geom_line(
    data = regression_data,
    aes(x = day_number, y = predicted_weight),
    color = "black", linewidth = 1.2, linetype = "solid"
  ) +
  geom_vline(
    xintercept = round(Xday$MAP),
    color = "black", linetype = "dashed", linewidth = 1
  ) +
  geom_vline(
    xintercept = round(Xday$CI_low),
    color = "gray40", linetype = "dotted", linewidth = 0.7
  ) +
  geom_vline(
    xintercept = round(Xday$CI_high),
    color = "gray40", linetype = "dotted", linewidth = 0.7
  ) +
  scale_x_continuous(
    breaks = seq(0, 350, by = 50),
    labels = function(x) paste0(x, "日目")
  ) +
  labs(x = "経過日数", y = "体重 (kg)") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  )
print(p_changepoint_final)

cat("\n=== 第15章 分析完了 ===\n")
cat("生成された図版:\n")
cat("- 図15.1: 項目反応関数（IRT）\n")
cat("- 図15.2: 体重の時系列散布図\n")
cat("- 図15.3: 変化点検出結果と回帰線\n")
