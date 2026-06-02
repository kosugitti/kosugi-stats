# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材第7章「重回帰分析」の数値例を再現します。
# 学業成績を目的変数，勉強時間・通学時間・学費を説明変数とした
# ベイズ重回帰分析を行います。
# ============================================================

rm(list = ls())
pacman::p_load(MASS, brms, bayestestR, tidyverse)

# データの生成 -------------------------------------------------------------

set.seed(42)
n <- 100

# 説明変数間の相関構造
# 勉強時間と通学時間に負の相関(-0.5)を持たせる。学費は独立。
Sigma <- matrix(c( 1.0, -0.5,  0.0,
                  -0.5,  1.0,  0.0,
                   0.0,  0.0,  1.0),
                nrow = 3)

std_data <- mvrnorm(n = n, mu = c(0, 0, 0), Sigma = Sigma)

# スケール変換と範囲の制限
study_time   <- pmax(0,  pmin(10,  std_data[, 1] * 2   + 5))    # 勉強時間 0-10時間
commute_time <- pmax(0,  pmin(3,   std_data[, 2] * 0.7 + 1.5))  # 通学時間 0-3 時間
tuition      <- pmax(10, pmin(100, std_data[, 3] * 20  + 55))   # 学費    10-100万円

# 学業成績(目的変数)を生成。通学時間にも正の効果を持たせ，抑制関係を作る
academic_score <- 2.0 +
                  0.30  * study_time   +
                  0.25  * commute_time +
                  0.015 * tuition      +
                  rnorm(n, 0, 0.4)
academic_score <- pmax(0, pmin(5, academic_score))

df <- data.frame(
  academic_score = academic_score,
  study_time     = study_time,
  commute_time   = commute_time,
  tuition        = tuition
)

# データの確認(表7.1: 仮想データの一部) ------------------------------------

cat("\n=== データの最初の7行(表7.1) ===\n")
print(round(head(df, 7), 2))

# 相関行列(表7.2) ----------------------------------------------------------

cor_matrix <- cor(df)
cat("\n=== 相関行列(表7.2) ===\n")
print(round(cor_matrix, 3))

cat("\n通学時間と学業成績の単純相関:",
    round(cor(df$commute_time, df$academic_score), 3), "\n")

# ベイズ重回帰(非標準化) ---------------------------------------------------

cat("\nbrmsによるベイズ推定(非標準化)を実行中...\n")
fit <- brm(
  academic_score ~ study_time + commute_time + tuition,
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10),  class = Intercept),
    prior(normal(0, 5),   class = b),
    prior(exponential(1), class = sigma)
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  seed = 42,
  silent = 2,
  refresh = 0
)

cat("\n=== モデルの要約 ===\n")
print(summary(fit))

# 非標準化係数のEAP・事後標準偏差・95% HDI(表7.3) -------------------------

posterior_samples <- as_draws_df(fit)
params <- c("b_Intercept", "b_study_time", "b_commute_time", "b_tuition", "sigma")

eap_values <- sapply(params, function(p) mean(posterior_samples[[p]]))
sd_values  <- sapply(params, function(p) sd(posterior_samples[[p]]))
hdi_intervals <- sapply(params, function(p) {
  hdi(posterior_samples[[p]], ci = 0.95)
}, simplify = FALSE)

results_unstd <- data.frame(
  Parameter = c("切片", "勉強時間", "通学時間", "学費", "σ(残差SD)"),
  EAP      = round(eap_values, 3),
  PostSD   = round(sd_values,  3),
  HDI_low  = round(sapply(hdi_intervals, function(x) x$CI_low),  3),
  HDI_high = round(sapply(hdi_intervals, function(x) x$CI_high), 3)
)
cat("\n=== 非標準化係数のEAP・事後標準偏差・95% HDI(表7.3) ===\n")
print(results_unstd)

# ベイズ重回帰(標準化) -----------------------------------------------------

df_std <- df %>%
  mutate(
    academic_score_z = scale(academic_score)[, 1],
    study_time_z     = scale(study_time)[, 1],
    commute_time_z   = scale(commute_time)[, 1],
    tuition_z        = scale(tuition)[, 1]
  )

cat("\n標準化データでベイズ推定を実行中...\n")
fit_std <- brm(
  academic_score_z ~ study_time_z + commute_time_z + tuition_z,
  data = df_std,
  family = gaussian(),
  prior = c(
    prior(normal(0, 5),   class = Intercept),
    prior(normal(0, 2),   class = b),
    prior(exponential(1), class = sigma)
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  seed = 42,
  silent = 2,
  refresh = 0
)

# 標準化係数のMAP・95% HDI(表7.4) ------------------------------------------

posterior_samples_std <- as_draws_df(fit_std)
params_std <- c("b_study_time_z", "b_commute_time_z", "b_tuition_z")

map_estimates_std <- sapply(params_std, function(p) {
  map_estimate(posterior_samples_std[[p]])$MAP_Estimate
})
hdi_intervals_std <- sapply(params_std, function(p) {
  hdi(posterior_samples_std[[p]], ci = 0.95)
}, simplify = FALSE)

results_std <- data.frame(
  Parameter = c("勉強時間", "通学時間", "学費"),
  MAP      = round(map_estimates_std, 3),
  HDI_low  = round(sapply(hdi_intervals_std, function(x) x$CI_low),  3),
  HDI_high = round(sapply(hdi_intervals_std, function(x) x$CI_high), 3)
)
cat("\n=== 標準化係数のMAPと95% HDI(表7.4) ===\n")
print(results_std)

# 決定係数R^2 --------------------------------------------------------------

r2_bayes <- bayes_R2(fit)
cat("\n=== 決定係数 R^2 ===\n")
cat("R^2 の事後平均値:", round(r2_bayes[1, "Estimate"], 3), "\n")
cat("R^2 の95%信用区間: [",
    round(r2_bayes[1, "Q2.5"],  3), ",",
    round(r2_bayes[1, "Q97.5"], 3), "]\n")

# 抑制効果の確認 -----------------------------------------------------------

cat("\n=== 抑制効果の確認 ===\n")
cat("通学時間と学業成績の単純相関:",
    round(cor(df$commute_time, df$academic_score), 3),
    "(負の相関)\n")
cat("通学時間の偏回帰係数(非標準化)EAP:",
    round(results_unstd$EAP[3], 3),
    "(正の値)\n")
cat("→ 単純相関は負だが偏回帰係数は正となり，符号が逆転している。\n")
cat("  勉強時間との負の相関(",
    round(cor(df$study_time, df$commute_time), 3),
    ")を介して，通学時間が抑制変数として機能している。\n")
