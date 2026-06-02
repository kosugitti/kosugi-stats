# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の分析例を再現します。
# RStudioで実行すると、コンソールに結果が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, lavaan, MASS)

set.seed(12345)

# データ生成：パス解析用 -------------------------------------------------------
# テーマ：自己効力感 → 学習動機 → 学習時間 → 学業成績

n_path <- 200

path_cor_matrix <- matrix(c(
  1.00, 0.60, 0.40, 0.35,
  0.60, 1.00, 0.50, 0.40,
  0.40, 0.50, 1.00, 0.45,
  0.35, 0.40, 0.45, 1.00
), nrow = 4, ncol = 4)

path_data_raw <- MASS::mvrnorm(
  n = n_path,
  mu = c(4, 4, 15, 70),
  Sigma = path_cor_matrix * c(1.2, 1.1, 5, 12) %*% t(c(1.2, 1.1, 5, 12))
)

path_data <- data.frame(
  self_efficacy        = pmax(1, pmin(7, round(path_data_raw[, 1]))),
  learning_motivation  = pmax(1, pmin(7, round(path_data_raw[, 2]))),
  study_hours          = pmax(0, round(path_data_raw[, 3], 1)),
  academic_performance = pmax(20, pmin(100, round(path_data_raw[, 4])))
)

set.seed(12345) # 媒介分析用データは独立したseedで固定（再現性確保）

# データ生成：潜在変数媒介分析用 -----------------------------------------------
# テーマ：SES → ソーシャルサポート → 地域への愛着
# Cor(SES, 地域愛着) = 0.65 * 0.75 = 0.4875（完全媒介モデルと整合）

n_med <- 1000

latent_cor_matrix <- matrix(c(
  1.00,   0.65,   0.4875,
  0.65,   1.00,   0.75,
  0.4875, 0.75,   1.00
), nrow = 3, ncol = 3)

latent_scores <- MASS::mvrnorm(n = n_med, mu = c(0, 0, 0), Sigma = latent_cor_matrix)

ses_loadings        <- c(0.80, 0.75, 0.85)
support_loadings    <- c(0.82, 0.78, 0.80, 0.76)
attachment_loadings <- c(0.88, 0.83, 0.79)

ses_items <- matrix(0, nrow = n_med, ncol = 3)
for (i in 1:3) {
  ses_items[, i] <- ses_loadings[i] * latent_scores[, 1] +
    rnorm(n_med, 0, sqrt(1 - ses_loadings[i]^2))
}

support_items <- matrix(0, nrow = n_med, ncol = 4)
for (i in 1:4) {
  support_items[, i] <- support_loadings[i] * latent_scores[, 2] +
    rnorm(n_med, 0, sqrt(1 - support_loadings[i]^2))
}

attachment_items <- matrix(0, nrow = n_med, ncol = 3)
for (i in 1:3) {
  attachment_items[, i] <- attachment_loadings[i] * latent_scores[, 3] +
    rnorm(n_med, 0, sqrt(1 - attachment_loadings[i]^2))
}

latent_med_data <- data.frame(
  ses1 = pmax(1, pmin(7, round(ses_items[, 1] * 1.5 + 4))),
  ses2 = pmax(1, pmin(7, round(ses_items[, 2] * 1.5 + 4))),
  ses3 = pmax(1, pmin(7, round(ses_items[, 3] * 1.5 + 4))),
  sup1 = pmax(1, pmin(7, round(support_items[, 1] * 1.5 + 4))),
  sup2 = pmax(1, pmin(7, round(support_items[, 2] * 1.5 + 4))),
  sup3 = pmax(1, pmin(7, round(support_items[, 3] * 1.5 + 4))),
  sup4 = pmax(1, pmin(7, round(support_items[, 4] * 1.5 + 4))),
  att1 = pmax(1, pmin(7, round(attachment_items[, 1] * 1.5 + 4))),
  att2 = pmax(1, pmin(7, round(attachment_items[, 2] * 1.5 + 4))),
  att3 = pmax(1, pmin(7, round(attachment_items[, 3] * 1.5 + 4)))
)

# 表11-2・11-3：パス解析 -------------------------------------------------------
# モデル：自己効力感 → 学習動機 → 学習時間 → 学業成績

path_model <- "
  learning_motivation  ~ self_efficacy
  study_hours          ~ learning_motivation
  academic_performance ~ study_hours
"

fit_path <- sem(path_model, data = path_data)
summary(fit_path, fit.measures = TRUE, standardized = TRUE)

# 表11-4・11-5・11-6：潜在変数媒介分析（完全媒介モデル）-----------------------

full_mediation_model <- "
  SES                 =~ ses1 + ses2 + ses3
  SocialSupport       =~ sup1 + sup2 + sup3 + sup4
  CommunityAttachment =~ att1 + att2 + att3
  SocialSupport       ~ a * SES
  CommunityAttachment ~ b * SocialSupport
  indirect := a * b
"

fit_med <- sem(full_mediation_model, data = latent_med_data)
summary(fit_med, fit.measures = TRUE, standardized = TRUE)
