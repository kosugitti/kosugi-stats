# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, brms, bayestestR, bayesplot, patchwork, MASS)

# 図8.1: ロジスティック関数 ------------------------------------------------

logistic <- function(x) {
  1 / (1 + exp(-x))
}

x_vals <- seq(-10, 10, length.out = 200)
y_vals <- logistic(x_vals)
df_logistic <- data.frame(x = x_vals, y = y_vals)

# 図8.1: ロジスティック関数 f(x) = 1/(1+exp(-x))
p_logistic <- ggplot(df_logistic, aes(x = x, y = y)) +
  geom_line(color = "black", linewidth = 1) +
  labs(x = expression(italic(x)),
       y = expression(italic(f(x)))) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_x_continuous(breaks = seq(-10, 10, 2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2))
print(p_logistic)

# 図8.2: ロジスティック回帰の実例 ------------------------------------------

set.seed(123)
n_lr <- 200

# 仮想データの生成（薬剤投与量と患者年齢）
dose <- rnorm(n_lr, mean = 5, sd = 2)
dose <- pmax(1, pmin(10, dose))

age <- rnorm(n_lr, mean = 55, sd = 15)
age <- pmax(30, pmin(80, age))

logit_p <- -1.0 + 0.6 * dose - 0.04 * age
prob_success <- 1 / (1 + exp(-logit_p))
treatment_success <- rbinom(n_lr, size = 1, prob = prob_success)

data_lr <- data.frame(
  treatment_success = treatment_success,
  dose = dose,
  age = age
)

cat("データの概要:\n")
print(summary(data_lr))
cat("\n治療成功率:", round(mean(data_lr$treatment_success) * 100, 1), "%\n\n")
cat("データの最初の7行:\n")
print(head(data_lr, 7))

# ベイズロジスティック回帰（brms）
cat("ベイズロジスティック回帰を実行中...\n")
fit_lr <- brm(
  treatment_success ~ dose + age,
  data = data_lr,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 5), class = "Intercept"),
    prior(normal(0, 2), class = "b")
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123,
  silent = 2,
  refresh = 0
)

cat("\nモデルの要約:\n")
print(summary(fit_lr))

# 回帰係数の詳細統計
posterior_samples <- as_draws_df(fit_lr)

intercept_post <- posterior_samples$b_Intercept
cat("\n切片 (Intercept):\n")
cat("  EAP (事後平均):", mean(intercept_post), "\n")
cat("  MAP (事後最頻値):", map_estimate(intercept_post)$MAP_Estimate, "\n")
cat("  95% HDI: [", hdi(intercept_post, ci = 0.95)$CI_low, ",",
    hdi(intercept_post, ci = 0.95)$CI_high, "]\n")

dose_post <- posterior_samples$b_dose
cat("\n薬剤投与量 (dose):\n")
cat("  EAP (事後平均):", mean(dose_post), "\n")
cat("  MAP (事後最頻値):", map_estimate(dose_post)$MAP_Estimate, "\n")
cat("  95% HDI: [", hdi(dose_post, ci = 0.95)$CI_low, ",",
    hdi(dose_post, ci = 0.95)$CI_high, "]\n")

age_post <- posterior_samples$b_age
cat("\n患者年齢 (age):\n")
cat("  EAP (事後平均):", mean(age_post), "\n")
cat("  MAP (事後最頻値):", map_estimate(age_post)$MAP_Estimate, "\n")
cat("  95% HDI: [", hdi(age_post, ci = 0.95)$CI_low, ",",
    hdi(age_post, ci = 0.95)$CI_high, "]\n")

# 投与量効果の予測
new_data_dose <- data.frame(
  dose = seq(1, 10, length.out = 100),
  age = mean(data_lr$age)
)
pred_dose <- fitted(fit_lr, newdata = new_data_dose, summary = TRUE)
new_data_dose$pred <- pred_dose[, "Estimate"]
new_data_dose$lower <- pred_dose[, "Q2.5"]
new_data_dose$upper <- pred_dose[, "Q97.5"]

new_data_age <- data.frame(
  dose = mean(data_lr$dose),
  age = seq(30, 80, length.out = 100)
)
pred_age <- fitted(fit_lr, newdata = new_data_age, summary = TRUE)
new_data_age$pred <- pred_age[, "Estimate"]
new_data_age$lower <- pred_age[, "Q2.5"]
new_data_age$upper <- pred_age[, "Q97.5"]

p_dose <- ggplot() +
  geom_ribbon(data = new_data_dose,
              aes(x = dose, ymin = lower, ymax = upper),
              fill = "gray70", alpha = 0.5) +
  geom_line(data = new_data_dose,
            aes(x = dose, y = pred),
            color = "black", linewidth = 1) +
  geom_point(data = data_lr,
             aes(x = dose, y = treatment_success),
             shape = 1, size = 2, alpha = 0.5,
             position = position_jitter(height = 0.02, width = 0)) +
  labs(x = "薬剤投与量（mg/kg）",
       y = "治療成功の確率") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

p_age_lr <- ggplot() +
  geom_ribbon(data = new_data_age,
              aes(x = age, ymin = lower, ymax = upper),
              fill = "gray70", alpha = 0.5) +
  geom_line(data = new_data_age,
            aes(x = age, y = pred),
            color = "black", linewidth = 1) +
  geom_point(data = data_lr,
             aes(x = age, y = treatment_success),
             shape = 1, size = 2, alpha = 0.5,
             position = position_jitter(height = 0.02, width = 0)) +
  labs(x = "患者年齢（歳）",
       y = "治療成功の確率") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

# 年齢層別の予測曲線
age_levels <- c(30, 50, 70)
pred_combined <- data.frame()
for (age_val in age_levels) {
  new_data_temp <- data.frame(
    dose = seq(1, 10, length.out = 100),
    age = age_val
  )
  pred_temp <- fitted(fit_lr, newdata = new_data_temp, summary = TRUE)
  new_data_temp$pred <- pred_temp[, "Estimate"]
  new_data_temp$lower <- pred_temp[, "Q2.5"]
  new_data_temp$upper <- pred_temp[, "Q97.5"]
  new_data_temp$age_group <- paste0(age_val, "歳")
  pred_combined <- rbind(pred_combined, new_data_temp)
}

# 図8.2: 年齢層別の投与量効果（3年齢層の比較）
p_combined_lr <- ggplot(pred_combined, aes(x = dose, y = pred,
                                 group = age_group,
                                 linetype = age_group)) +
  geom_line(color = "black", linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = age_group),
              alpha = 0.2) +
  labs(x = "薬剤投与量（mg/kg）",
       y = "治療成功の確率",
       linetype = "患者年齢",
       fill = "患者年齢") +
  theme_classic() +
  theme(legend.position = c(0.2, 0.8)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = c("gray80", "gray50", "gray20")) +
  scale_linetype_manual(values = c("dotted", "solid", "dashed"))
print(p_combined_lr)

# 図8.3〜8.6: カウントデータのベイズ分析 -----------------------------------

set.seed(12345)
n_count <- 200

# 友人の数を目的変数（年齢と社会活動が説明変数，過分散あり）
data_chap8 <- tibble(
  id = 1:n_count,
  age = rnorm(n_count, mean = 45, sd = 15),
  social_activity = rpois(n_count, lambda = 5)
) |>
  mutate(
    age_scaled = scale(age)[,1],
    social_activity_scaled = scale(social_activity)[,1]
  )

eta <- 2.5 - 0.4 * data_chap8$age_scaled + 0.6 * data_chap8$social_activity_scaled
lambda <- exp(eta)
size_param <- 2

data_chap8 <- data_chap8 |>
  mutate(
    friends_count = MASS::rnegbin(n_count, mu = lambda, theta = size_param)
  )

# データ確認
cat("\n友人数の記述統計:\n")
cat("平均:", mean(data_chap8$friends_count), "\n")
cat("分散:", var(data_chap8$friends_count), "\n")
cat("分散/平均 (過分散の指標):", var(data_chap8$friends_count) / mean(data_chap8$friends_count), "\n")

# 線形回帰での問題確認
lm_model <- lm(friends_count ~ age_scaled + social_activity_scaled, data = data_chap8)
data_chap8$lm_pred <- predict(lm_model)
cat("\n線形回帰の予測値の範囲:\n")
cat("最小値:", min(data_chap8$lm_pred), "\n")
cat("最大値:", max(data_chap8$lm_pred), "\n")
cat("負の予測値の数:", sum(data_chap8$lm_pred < 0), "\n")

# 図8.3: 友人数の分布
p_friends_dist <- ggplot(data_chap8, aes(x = friends_count)) +
  geom_histogram(binwidth = 1, fill = "gray70", color = "black") +
  labs(x = "友人の数", y = "度数") +
  theme_minimal()
print(p_friends_dist)

# 図8.4: 線形回帰の問題（負の予測値）
p_linear_prob <- ggplot(data_chap8, aes(x = age, y = friends_count)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "gray70", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "年齢", y = "友人の数") +
  theme_minimal()
print(p_linear_prob)

# ベイズモデル推定（ポアソン回帰）
cat("\nポアソン回帰モデルを推定中...\n")
model_poisson <- brm(
  friends_count ~ age_scaled + social_activity_scaled,
  data = data_chap8,
  family = poisson(link = "log"),
  prior = c(
    prior(normal(0, 5), class = "Intercept"),
    prior(normal(0, 2), class = "b")
  ),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  seed = 12345,
  silent = 2,
  refresh = 0,
  backend = "cmdstanr"
)

# ベイズモデル推定（負の二項回帰）
cat("\n負の二項回帰モデルを推定中...\n")
model_negbin <- brm(
  friends_count ~ age_scaled + social_activity_scaled,
  data = data_chap8,
  family = negbinomial(link = "log"),
  prior = c(
    prior(normal(0, 5), class = "Intercept"),
    prior(normal(0, 2), class = "b"),
    prior(gamma(0.01, 0.01), class = "shape")
  ),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  seed = 12345,
  silent = 2,
  refresh = 0,
  backend = "cmdstanr"
)

# モデル比較
cat("\nモデル比較（LOOIC）:\n")
loo_poisson <- loo(model_poisson)
loo_negbin <- loo(model_negbin)
loo_compare <- loo_compare(loo_poisson, loo_negbin)
print(loo_compare)

# 図8.5: 事後予測分布 vs 実測値（ポアソン回帰）
pred_samples_poisson <- posterior_predict(model_poisson, draws = 1000)
x_vals_pred <- 0:100
pred_freq_poisson <- sapply(x_vals_pred, function(x) {
  mean(colSums(pred_samples_poisson == x))
})
pred_density_poisson <- pred_freq_poisson / sum(pred_freq_poisson) / 1
pred_curve_poisson <- tibble(
  x = x_vals_pred,
  density = pred_density_poisson
)

# 図8.5: ポアソン回帰の予測分布 vs 実測値
p_pred_poisson <- ggplot() +
  geom_histogram(data = data_chap8, aes(x = friends_count, y = after_stat(density)),
                 binwidth = 1, fill = "gray80", color = "black", linewidth = 0.2, alpha = 0.5) +
  geom_line(data = pred_curve_poisson, aes(x = x, y = density),
            color = "black", linewidth = 1) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(x = "友人の数", y = "密度") +
  theme_minimal()
print(p_pred_poisson)

# 図8.6: 事後予測分布 vs 実測値（負の二項回帰）
pred_samples_negbin <- posterior_predict(model_negbin, draws = 1000)
pred_freq_negbin <- sapply(x_vals_pred, function(x) {
  mean(colSums(pred_samples_negbin == x))
})
pred_density_negbin <- pred_freq_negbin / sum(pred_freq_negbin) / 1
pred_curve_negbin <- tibble(
  x = x_vals_pred,
  density = pred_density_negbin
)

# 図8.6: 負の二項回帰の予測分布 vs 実測値
p_pred_negbin <- ggplot() +
  geom_histogram(data = data_chap8, aes(x = friends_count, y = after_stat(density)),
                 binwidth = 1, fill = "gray80", color = "black", linewidth = 0.2, alpha = 0.5) +
  geom_line(data = pred_curve_negbin, aes(x = x, y = density),
            color = "black", linewidth = 1) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(x = "友人の数", y = "密度") +
  theme_minimal()
print(p_pred_negbin)

cat("\n=== 分析完了 ===\n")
cat("主要な結果:\n")
cat("1. データの過分散: 分散/平均 =", var(data_chap8$friends_count) / mean(data_chap8$friends_count), "\n")
cat("2. モデル比較（LOOIC）:\n")
print(loo_compare)
