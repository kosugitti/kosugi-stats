# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, brms, bayestestR, bayesplot, patchwork, lme4, bridgesampling)

# データ生成 ---------------------------------------------------------------

set.seed(12345)
n_classes <- 15
n_students_per_class <- 30

class_data <- tibble(
  class_id = 1:n_classes,
  books_raw = round(rnorm(n_classes, mean = 50, sd = 20), 1)
) |>
  mutate(
    books = scale(books_raw)[,1],
    class_intercept = rnorm(n_classes, mean = 0, sd = 1.5),
    class_slope_study = rnorm(n_classes, mean = 0.3 * books, sd = 0.3)
  )

student_data <- map_df(1:n_classes, function(i) {
  n_students <- round(rnorm(1, mean = n_students_per_class, sd = 3))
  n_students <- max(25, min(35, n_students))

  tibble(
    class_id = i,
    student_id = 1:n_students,
    study_time = rnorm(n_students, mean = 10, sd = 3),
    sleep_time = rnorm(n_students, mean = 7, sd = 1)
  )
}) |>
  left_join(class_data, by = "class_id") |>
  mutate(
    achievement = 50 +
      class_intercept +
      (1.5 + class_slope_study) * study_time +
      2.0 * sleep_time +
      rnorm(n(), mean = 0, sd = 2.5)
  ) |>
  mutate(
    unique_id = paste0("C", sprintf("%02d", class_id), "S", sprintf("%02d", student_id)),
    study_time_c = scale(study_time)[,1],
    sleep_time_c = scale(sleep_time)[,1]
  )

cat("データの基本統計量:\n")
student_data |>
  select(achievement, study_time, sleep_time) |>
  summary() |>
  print()

cat("\n学級ごとの生徒数:\n")
student_data |>
  count(class_id) |>
  print()

# モデル結果抽出用の関数
extract_model_results <- function(model, params, labels, model_name) {
  posterior <- as_draws_df(model)
  results <- tibble(
    parameter = labels,
    model = model_name,
    EAP = NA_real_,
    MAP = NA_real_,
    HDI_lower = NA_real_,
    HDI_upper = NA_real_
  )
  for (i in seq_along(params)) {
    param <- params[i]
    if (param %in% names(posterior)) {
      results$EAP[i] <- mean(posterior[[param]])
      dens <- density(posterior[[param]])
      results$MAP[i] <- dens$x[which.max(dens$y)]
      hdi_vals <- hdi(posterior[[param]], ci = 0.95)
      results$HDI_lower[i] <- hdi_vals$CI_low[1]
      results$HDI_upper[i] <- hdi_vals$CI_high[1]
    }
  }
  results <- results |>
    mutate(HDI_95 = paste0("[", round(HDI_lower, 3), ", ", round(HDI_upper, 3), "]"))
  return(results)
}

# 図9.1: 学級ごとの散布図 -------------------------------------------------

# 図9.1: 学級ごとの学業成績と学習時間の関係
p_scatter <- ggplot(student_data, aes(x = study_time, y = achievement, group = class_id)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, color = "black") +
  facet_wrap(~class_id, ncol = 5) +
  labs(x = "学習時間（時間/週）", y = "学業成績") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 7)
  )
print(p_scatter)

# モデル2: 変動切片モデル --------------------------------------------------

cat("\n=== モデル2: 変動切片モデル ===\n")
model2 <- brm(
  achievement ~ study_time_c + sleep_time_c + (1 | class_id),
  data = student_data,
  family = gaussian(),
  prior = c(
    prior(normal(50, 10), class = Intercept),
    prior(normal(0, 5), class = b),
    prior(exponential(1), class = sigma),
    prior(exponential(1), class = sd)
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 12345,
  silent = 2,
  refresh = 0,
  save_pars = save_pars(all = TRUE)
)

print(summary(model2))

# 図9.2: 変動切片モデルの可視化 ------------------------------------------

fixef_model2 <- fixef(model2)
fixed_intercept <- fixef_model2["Intercept", "Estimate"]
fixed_slope_study <- fixef_model2["study_time_c", "Estimate"]

ranef_model2 <- ranef(model2)
class_intercepts <- ranef_model2$class_id[, "Estimate", "Intercept"]
class_intercepts_full <- fixed_intercept + class_intercepts

class_lines <- tibble(
  class_id = as.numeric(names(class_intercepts)),
  intercept = class_intercepts_full,
  slope = fixed_slope_study
)

# 図9.2: 変動切片モデル（各学級の切片が異なる，傾きは共通）
p_random_intercept <- ggplot(student_data, aes(x = study_time_c, y = achievement)) +
  geom_point(alpha = 0.3, size = 0.8, color = "gray50") +
  geom_abline(data = class_lines,
              aes(intercept = intercept, slope = slope),
              linetype = "dashed", linewidth = 0.4, color = "gray30") +
  geom_abline(intercept = fixed_intercept, slope = fixed_slope_study,
              linetype = "solid", linewidth = 1.2, color = "black") +
  labs(x = "学習時間（標準化）", y = "学業成績") +
  theme_minimal()
print(p_random_intercept)

# モデル2.5: 変動傾きモデル -----------------------------------------------

cat("\n=== モデル2.5: 変動傾きモデル ===\n")
model2_5 <- brm(
  achievement ~ study_time_c + sleep_time_c +
    (0 + study_time_c | class_id),
  data = student_data,
  family = gaussian(),
  prior = c(
    prior(normal(50, 10), class = Intercept),
    prior(normal(0, 5), class = b),
    prior(exponential(1), class = sigma),
    prior(exponential(1), class = sd)
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 12345,
  silent = 2,
  refresh = 0,
  save_pars = save_pars(all = TRUE)
)

print(summary(model2_5))

# 図9.3: 変動傾きモデルの可視化 ------------------------------------------

fixef_model2_5 <- fixef(model2_5)
fixed_intercept_m2_5 <- fixef_model2_5["Intercept", "Estimate"]
fixed_slope_study_m2_5 <- fixef_model2_5["study_time_c", "Estimate"]

ranef_model2_5 <- ranef(model2_5)
class_slopes_m2_5 <- ranef_model2_5$class_id[, "Estimate", "study_time_c"]

class_lines_m2_5 <- tibble(
  class_id = as.numeric(names(class_slopes_m2_5)),
  intercept = fixed_intercept_m2_5,
  slope = fixed_slope_study_m2_5 + class_slopes_m2_5
)

# 図9.3: 変動傾きモデル（傾きが異なる，切片は共通）
p_random_slope_only <- ggplot(student_data, aes(x = study_time_c, y = achievement)) +
  geom_point(alpha = 0.3, size = 0.8, color = "gray50") +
  geom_abline(data = class_lines_m2_5,
              aes(intercept = intercept, slope = slope),
              linetype = "dashed", linewidth = 0.4, color = "gray30") +
  geom_abline(intercept = fixed_intercept_m2_5, slope = fixed_slope_study_m2_5,
              linetype = "solid", linewidth = 1.2, color = "black") +
  labs(x = "学習時間（標準化）", y = "学業成績") +
  theme_minimal()
print(p_random_slope_only)

# モデル3: 変動切片・変動傾きモデル ----------------------------------------

cat("\n=== モデル3: 変動切片・変動傾きモデル ===\n")
model3 <- brm(
  achievement ~ study_time_c + sleep_time_c +
    (1 + study_time_c | class_id),
  data = student_data,
  family = gaussian(),
  prior = c(
    prior(normal(50, 10), class = Intercept),
    prior(normal(0, 5), class = b),
    prior(exponential(1), class = sigma),
    prior(exponential(1), class = sd),
    prior(lkj(2), class = cor)
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 12345,
  silent = 2,
  refresh = 0,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

print(summary(model3))

# rho（切片と傾きの相関）の抽出
rho3 <- as.vector(as_draws_df(model3)[["cor_class_id__Intercept__study_time_c"]])
rho3_hdi <- hdi(rho3, ci = 0.95)
cat(sprintf("\nrho: EAP=%.3f  MAP=%.3f  HDI=[%.2f, %.2f]\n",
  mean(rho3), as.numeric(map_estimate(rho3)), rho3_hdi$CI_low, rho3_hdi$CI_high))

# 図9.4: 変動切片・変動傾きモデルの可視化 ---------------------------------

fixef_model3 <- fixef(model3)
fixed_intercept_m3 <- fixef_model3["Intercept", "Estimate"]
fixed_slope_study_m3 <- fixef_model3["study_time_c", "Estimate"]

ranef_model3 <- ranef(model3)
class_intercepts_m3 <- ranef_model3$class_id[, "Estimate", "Intercept"]
class_slopes_m3 <- ranef_model3$class_id[, "Estimate", "study_time_c"]

class_lines_m3 <- tibble(
  class_id = as.numeric(names(class_intercepts_m3)),
  intercept = fixed_intercept_m3 + class_intercepts_m3,
  slope = fixed_slope_study_m3 + class_slopes_m3
)

# 図9.4: 変動切片・変動傾きモデル（切片も傾きも学級ごとに異なる）
p_random_slope <- ggplot(student_data, aes(x = study_time_c, y = achievement)) +
  geom_point(alpha = 0.3, size = 0.8, color = "gray50") +
  geom_abline(data = class_lines_m3,
              aes(intercept = intercept, slope = slope),
              linetype = "dashed", linewidth = 0.4, color = "gray30") +
  geom_abline(intercept = fixed_intercept_m3, slope = fixed_slope_study_m3,
              linetype = "solid", linewidth = 1.2, color = "black") +
  labs(x = "学習時間（標準化）", y = "学業成績") +
  theme_minimal()
print(p_random_slope)

# 図9.5: モデル比較（LOOIC）----------------------------------------------

cat("\n=== モデル比較（LOOIC）===\n")
loo1 <- add_criterion(brm(
  achievement ~ study_time_c + sleep_time_c,
  data = student_data,
  family = gaussian(),
  prior = c(
    prior(normal(50, 10), class = Intercept),
    prior(normal(0, 5), class = b),
    prior(exponential(1), class = sigma)
  ),
  chains = 4, iter = 2000, warmup = 1000, cores = 4,
  seed = 12345, silent = 2, refresh = 0
), "loo")
loo2 <- add_criterion(model2, "loo")
loo3 <- add_criterion(model3, "loo")

loo_compare_result <- loo_compare(loo1, loo2, loo3)
print(loo_compare_result)

# モデル比較を折れ線グラフで表示
loo_df <- as.data.frame(loo_compare_result) |>
  rownames_to_column("model")

# 図9.5: モデルごとのELPD比較
p_loo <- ggplot(loo_df, aes(x = model, y = elpd_diff)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = elpd_diff - se_diff, ymax = elpd_diff + se_diff), width = 0.2) +
  labs(x = "モデル", y = "ELPD差（参照モデルからの差）") +
  theme_minimal()
print(p_loo)

cat("\n=== 階層線形モデル分析完了 ===\n")
cat("主要な結果:\n")
cat("1. 学業成績の分散は学級間・学級内の両方で生じている\n")
cat("2. 変動切片モデルで学級ごとの平均成績の違いをモデル化\n")
cat("3. 変動傾きモデルで学習時間の効果の学級差をモデル化\n")
