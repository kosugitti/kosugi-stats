# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, MASS, ggforce, psych, patchwork, ggrepel)

# 図10.1: 主成分分析の概念図 -----------------------------------------------

set.seed(123)
n_pca <- 100

# 50m走と走り幅跳びの相関のあるデータを生成
mu_pca <- c(7.5, 4.5)
sigma_pca <- matrix(c(0.5, 0.4, 0.4, 0.5), nrow = 2)

data_raw <- mvrnorm(n_pca, mu_pca, sigma_pca)
df_pca <- data.frame(
  run_50m = data_raw[, 1],
  long_jump = data_raw[, 2]
)

# 主成分分析の実行
pca_result <- prcomp(df_pca, scale. = TRUE)

pc1_vec <- pca_result$rotation[, 1]
pc2_vec <- pca_result$rotation[, 2]

center_x <- mean(df_pca$run_50m)
center_y <- mean(df_pca$long_jump)
sd_x <- sd(df_pca$run_50m)
sd_y <- sd(df_pca$long_jump)

scale_factor_pc1 <- 2.6
scale_factor_pc2 <- 1.0

pc1_x <- pc1_vec[1] * sd_x * scale_factor_pc1
pc1_y <- pc1_vec[2] * sd_y * scale_factor_pc1

pc2_x <- pc2_vec[1] * sd_x * scale_factor_pc2
pc2_y <- pc2_vec[2] * sd_y * scale_factor_pc2

x_min <- min(df_pca$run_50m)
x_max <- max(df_pca$run_50m)
y_min <- min(df_pca$long_jump)
y_max <- max(df_pca$long_jump)

# 図10.1: 主成分分析の概念図（第1・第2主成分軸）
p_pca_concept <- ggplot(df_pca, aes(x = run_50m, y = long_jump)) +
  geom_point(size = 2, alpha = 0.6, color = "gray30") +
  stat_ellipse(level = 0.95, color = "gray40", linewidth = 0.8, linetype = "solid") +
  geom_hline(yintercept = y_min - 0.4, color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = x_min - 0.15, color = "gray70", linewidth = 0.5) +
  annotate("segment",
           x = x_min, xend = x_max,
           y = y_min - 0.25, yend = y_min - 0.25,
           arrow = arrow(ends = "both", length = unit(0.2, "cm"), type = "closed"),
           color = "gray50", linewidth = 0.8) +
  annotate("segment",
           x = x_min - 0.12, xend = x_min - 0.12,
           y = y_min, yend = y_max,
           arrow = arrow(ends = "both", length = unit(0.2, "cm"), type = "closed"),
           color = "gray50", linewidth = 0.8) +
  annotate("segment",
           x = center_x - pc1_x, xend = center_x + pc1_x,
           y = center_y - pc1_y, yend = center_y + pc1_y,
           arrow = arrow(ends = "both", length = unit(0.25, "cm"), type = "closed"),
           color = "black", linewidth = 1.2) +
  annotate("segment",
           x = center_x - pc2_x, xend = center_x + pc2_x,
           y = center_y - pc2_y, yend = center_y + pc2_y,
           arrow = arrow(ends = "both", length = unit(0.25, "cm"), type = "closed"),
           color = "black", linewidth = 1.2, linetype = "dashed") +
  annotate("label", x = center_x + pc1_x + 0.15, y = center_y + pc1_y + 0.15,
           label = "PC1", fontface = "bold", size = 4,
           fill = "white", label.size = 0.3, label.padding = unit(0.2, "lines")) +
  annotate("label", x = center_x - pc2_x - 0.2, y = center_y - pc2_y - 0.15,
           label = "PC2", fontface = "bold", size = 4,
           fill = "white", label.size = 0.3, label.padding = unit(0.2, "lines")) +
  labs(x = "50m走（秒）", y = "走り幅跳び（m）") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  coord_fixed(ratio = 1)
print(p_pca_concept)

# 図10.2: 平行分析 ----------------------------------------------------------

set.seed(12345)
n_fa <- 300

# 3因子構造のデータを生成（12項目）
Lambda <- matrix(c(
  0.8, 0.1, 0.1,
  0.7, 0.2, 0.1,
  0.75, 0.1, 0.15,
  0.65, 0.2, 0.1,
  0.1, 0.8, 0.1,
  0.2, 0.7, 0.1,
  0.1, 0.75, 0.15,
  0.15, 0.65, 0.2,
  0.1, 0.1, 0.8,
  0.1, 0.2, 0.7,
  0.15, 0.1, 0.75,
  0.2, 0.1, 0.65
), nrow = 12, ncol = 3, byrow = TRUE)

communalities <- rowSums(Lambda^2)
uniquenesses <- 1 - communalities
R_fa <- Lambda %*% t(Lambda) + diag(uniquenesses)
data_fa <- mvrnorm(n = n_fa, mu = rep(0, 12), Sigma = R_fa)
colnames(data_fa) <- paste0("item", 1:12)

# 平行分析の実行
parallel_result <- fa.parallel(data_fa, fa = "fa", plot = FALSE, n.iter = 100)

fa_values <- parallel_result$fa.values
fa_sim <- parallel_result$fa.sim
n_factors <- length(fa_values)

plot_data_parallel <- tibble(
  factor_number = rep(1:n_factors, 2),
  eigenvalue = c(fa_values, fa_sim),
  type = rep(c("実データ", "平行分析（シミュレーション）"), each = n_factors)
)

# 図10.2: スクリープロットと平行分析
p_parallel <- ggplot(plot_data_parallel, aes(x = factor_number, y = eigenvalue,
                            linetype = type, shape = type)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3, fill = "white") +
  scale_linetype_manual(values = c("実データ" = "solid",
                                    "平行分析（シミュレーション）" = "dashed")) +
  scale_shape_manual(values = c("実データ" = 16,
                                 "平行分析（シミュレーション）" = 21)) +
  scale_x_continuous(breaks = 1:n_factors) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  labs(x = "因子番号", y = "固有値") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
print(p_parallel)

cat("推奨因子数:", parallel_result$nfact, "\n")

# 図10.3: 因子軸の回転 -----------------------------------------------------

n_extract <- 3

fa_none <- fa(data_fa, nfactors = n_extract, rotate = "none", fm = "ml")
fa_geomin_t <- fa(data_fa, nfactors = n_extract, rotate = "geominT", fm = "ml")
fa_geomin_q <- fa(data_fa, nfactors = n_extract, rotate = "geominQ", fm = "ml")

loadings_none <- as.data.frame(unclass(fa_none$loadings))
loadings_geomin_t <- as.data.frame(unclass(fa_geomin_t$loadings))
loadings_geomin_q <- as.data.frame(unclass(fa_geomin_q$loadings))

loadings_none$item <- paste0("項目", 1:12)
loadings_geomin_t$item <- paste0("項目", 1:12)
loadings_geomin_q$item <- paste0("項目", 1:12)

cat("\n========== 回転前の因子負荷量行列 ==========\n")
print(round(loadings_none[, 1:3], 3))

cat("\n========== geominT（直交回転）の因子負荷量行列 ==========\n")
print(round(loadings_geomin_t[, 1:3], 3))

cat("\n========== geominQ（斜交回転）の因子負荷量行列 ==========\n")
print(round(loadings_geomin_q[, 1:3], 3))

cat("\n========== geominQ 因子間相関 ==========\n")
print(round(fa_geomin_q$Phi, 3))

selected_items <- c(1, 2, 5, 6, 9, 10)

df_none <- tibble(
  item = paste0("項目", 1:12),
  F1 = loadings_none[[1]],
  F2 = loadings_none[[2]],
  group = rep(c("A", "B", "C"), each = 4)
) |>
  filter(row_number() %in% selected_items)

df_ortho <- tibble(
  item = paste0("項目", 1:12),
  F1 = loadings_geomin_t[[1]],
  F2 = loadings_geomin_t[[2]],
  group = rep(c("A", "B", "C"), each = 4)
) |>
  filter(row_number() %in% selected_items)

df_oblique <- tibble(
  item = paste0("項目", 1:12),
  F1 = loadings_geomin_q[[1]],
  F2 = loadings_geomin_q[[2]],
  group = rep(c("A", "B", "C"), each = 4)
) |>
  filter(row_number() %in% selected_items)

common_theme <- theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(5, 8, 5, 5)
  )

axis_min <- -0.5
axis_max <- 1.1
arrow_len <- 1.05

p_none <- ggplot(df_none, aes(x = F1, y = F2, shape = group)) +
  annotate("segment", x = -arrow_len, y = 0, xend = arrow_len, yend = 0,
           linewidth = 0.7, color = "black",
           arrow = arrow(length = unit(0.12, "cm"), ends = "last", type = "closed")) +
  annotate("segment", x = 0, y = -arrow_len, xend = 0, yend = arrow_len,
           linewidth = 0.7, color = "black",
           arrow = arrow(length = unit(0.12, "cm"), ends = "last", type = "closed")) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = item), size = 3,
                  box.padding = 0.3, max.overlaps = Inf, seed = 123) +
  scale_shape_manual(values = c("A" = 16, "B" = 17, "C" = 15)) +
  coord_fixed(xlim = c(axis_min, axis_max), ylim = c(axis_min, axis_max)) +
  labs(x = "因子1", y = "因子2", title = "回転前") +
  common_theme

rot_mat_ortho <- fa_geomin_t$rot.mat
theta_ortho <- atan2(rot_mat_ortho[2, 1], rot_mat_ortho[1, 1])
f1_ortho_x <- arrow_len * cos(theta_ortho)
f1_ortho_y <- arrow_len * sin(theta_ortho)
f2_ortho_x <- arrow_len * cos(theta_ortho + pi / 2)
f2_ortho_y <- arrow_len * sin(theta_ortho + pi / 2)

p_ortho <- ggplot(df_ortho, aes(x = F1, y = F2, shape = group)) +
  annotate("segment", x = -arrow_len, y = 0, xend = arrow_len, yend = 0,
           linewidth = 0.4, color = "gray65", linetype = "dashed") +
  annotate("segment", x = 0, y = -arrow_len, xend = 0, yend = arrow_len,
           linewidth = 0.4, color = "gray65", linetype = "dashed") +
  annotate("segment", x = -f1_ortho_x, y = -f1_ortho_y,
           xend = f1_ortho_x, yend = f1_ortho_y,
           linewidth = 0.7, color = "black",
           arrow = arrow(length = unit(0.12, "cm"), ends = "last", type = "closed")) +
  annotate("segment", x = -f2_ortho_x, y = -f2_ortho_y,
           xend = f2_ortho_x, yend = f2_ortho_y,
           linewidth = 0.7, color = "black",
           arrow = arrow(length = unit(0.12, "cm"), ends = "last", type = "closed")) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = item), size = 3,
                  box.padding = 0.3, max.overlaps = Inf, seed = 123) +
  scale_shape_manual(values = c("A" = 16, "B" = 17, "C" = 15)) +
  coord_fixed(xlim = c(axis_min, axis_max), ylim = c(axis_min, axis_max)) +
  labs(x = "因子1", y = "因子2", title = "直交回転後") +
  common_theme

phi <- fa_geomin_q$Phi
r12 <- phi[1, 2]
theta_oblique <- acos(r12)
half_angle <- (pi / 2 - theta_oblique) / 2
angle_f1 <- half_angle
angle_f2 <- pi/2 - half_angle
f1_oblique_x <- arrow_len * cos(angle_f1)
f1_oblique_y <- arrow_len * sin(angle_f1)
f2_oblique_x <- arrow_len * cos(angle_f2)
f2_oblique_y <- arrow_len * sin(angle_f2)

p_oblique <- ggplot(df_oblique, aes(x = F1, y = F2, shape = group)) +
  annotate("segment", x = -arrow_len, y = 0, xend = arrow_len, yend = 0,
           linewidth = 0.4, color = "gray65", linetype = "dashed") +
  annotate("segment", x = 0, y = -arrow_len, xend = 0, yend = arrow_len,
           linewidth = 0.4, color = "gray65", linetype = "dashed") +
  annotate("segment", x = -f1_oblique_x, y = -f1_oblique_y,
           xend = f1_oblique_x, yend = f1_oblique_y,
           linewidth = 0.7, color = "black",
           arrow = arrow(length = unit(0.12, "cm"), ends = "last", type = "closed")) +
  annotate("segment", x = -f2_oblique_x, y = -f2_oblique_y,
           xend = f2_oblique_x, yend = f2_oblique_y,
           linewidth = 0.7, color = "black",
           arrow = arrow(length = unit(0.12, "cm"), ends = "last", type = "closed")) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = item), size = 3,
                  box.padding = 0.3, max.overlaps = Inf, seed = 123) +
  scale_shape_manual(values = c("A" = 16, "B" = 17, "C" = 15)) +
  coord_fixed(xlim = c(axis_min, axis_max), ylim = c(axis_min, axis_max)) +
  labs(x = "因子1", y = "因子2", title = "斜交回転後") +
  common_theme

# 図10.3: 因子軸の回転（回転前・直交回転後・斜交回転後の比較）
p_rotation <- p_none + p_ortho + p_oblique +
  plot_layout(ncol = 3)
print(p_rotation)

cat("\n因子分析・主成分分析の図版作成完了\n")
