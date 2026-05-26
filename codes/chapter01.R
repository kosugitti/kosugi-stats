# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, palmerpenguins, ggrain, ggdist, patchwork, MASS, mvtnorm, ggrepel)

# 図1.1: 歪度の異なる分布 --------------------------------------------------
set.seed(12345)
n <- 1000

# 正の歪度（指数分布を変形）
positive_skew_data <- rexp(n, rate = 1)

# 負の歪度（ベータ分布を反転）
beta_data <- rbeta(n, 2, 8)
negative_skew_data <- (1 - beta_data)^0.3

df_positive <- data.frame(x = positive_skew_data)
df_negative <- data.frame(x = negative_skew_data)

p1 <- ggplot(df_positive, aes(x = x)) +
  geom_histogram(bins = 30, fill = "gray70", color = "black", alpha = 0.7) +
  labs(x = "値", y = "頻度") +
  theme_minimal()

p2 <- ggplot(df_negative, aes(x = x)) +
  geom_histogram(bins = 30, fill = "gray70", color = "black", alpha = 0.7) +
  labs(x = "値", y = "頻度") +
  theme_minimal()

# 図1.1: 歪度の比較（正の歪度 vs 負の歪度）
combined_plot <- p1 + p2
print(combined_plot)

# 歪度の実際の値を確認
skew_positive <- mean((positive_skew_data - mean(positive_skew_data))^3) / (sd(positive_skew_data)^3)
skew_negative <- mean((negative_skew_data - mean(negative_skew_data))^3) / (sd(negative_skew_data)^3)
cat("正の歪度データの実際の歪度:", round(skew_positive, 2), "\n")
cat("負の歪度データの実際の歪度:", round(skew_negative, 2), "\n")

# 図1.2: 尖度の異なる分布 --------------------------------------------------

# 低い尖度：一様分布
low_kurtosis_data <- runif(n, min = -2, max = 2)

# 高い尖度：t分布（自由度が小さい）
high_kurtosis_data <- rt(n, df = 3) * 0.5

k1 <- ggplot(data.frame(x = low_kurtosis_data), aes(x = x)) +
  geom_histogram(bins = 30, fill = "gray70", color = "black", alpha = 0.7) +
  labs(x = "値", y = "頻度") +
  xlim(-3, 3) +
  theme_minimal()

k2 <- ggplot(data.frame(x = high_kurtosis_data), aes(x = x)) +
  geom_histogram(bins = 30, fill = "gray70", color = "black", alpha = 0.7) +
  labs(x = "値", y = "頻度") +
  xlim(-3, 3) +
  theme_minimal()

# 図1.2: 尖度の比較（低い尖度 vs 高い尖度）
kurtosis_plot <- k1 + k2
print(kurtosis_plot)

# 尖度の実際の値を確認
kurt_low <- mean((low_kurtosis_data - mean(low_kurtosis_data))^4) / (sd(low_kurtosis_data)^4) - 3
kurt_high <- mean((high_kurtosis_data - mean(high_kurtosis_data))^4) / (sd(high_kurtosis_data)^4) - 3
cat("低い尖度データの実際の尖度:", round(kurt_low, 2), "\n")
cat("高い尖度データの実際の尖度:", round(kurt_high, 2), "\n")

# 図1.3: ヒストグラム（ペンギンデータ）--------------------------------------

# 図1.3: くちばしの深さのヒストグラム
p <- penguins %>%
  ggplot(aes(x = bill_depth_mm)) +
  geom_histogram(binwidth = .5) +
  labs(
    x = "くちばしの深さ (mm)",
    y = "頻度"
  ) +
  theme_classic()
print(p)

# 図1.4: ボックスプロット ----------------------------------------------------

# 図1.4: くちばしの長さのボックスプロット（外れ値追加）
p3 <- data.frame(val = c(penguins$bill_length_mm, 69, 25)) %>%
  ggplot(aes(x = val)) +
  geom_boxplot() +
  xlab("くちばしの長さ (mm)") +
  theme_classic()
print(p3)

# 図1.5: レインプロット -------------------------------------------------------
penguins_adelie <- penguins %>%
  filter(species == "Adelie") %>%
  drop_na(bill_depth_mm)

# 図1.5: くちばしの深さのレインプロット（Adelie種）
p2 <- ggplot(
  penguins_adelie,
  aes(x = bill_depth_mm, y = 0)
) +
  stat_halfeye(
    width = 0.6,
    justification = -0.5,
    alpha = 0.6,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.52,
    alpha = 0.8,
    outlier.shape = NA
  ) +
  geom_point(
    position = position_jitter(width = 0, height = 0.1),
    size = 2,
    alpha = 0.7,
    color = "black"
  ) +
  theme_classic() +
  xlab("くちばしの深さ (mm)") +
  ylab("") +
  theme(aspect.ratio = 1 / 3)
print(p2)

# 図1.6: 散布図 -------------------------------------------------------------

# 図1.6: くちばしの深さ vs 長さの散布図（種ごとに形状変更）
p4 <- penguins %>%
  ggplot(aes(y = bill_length_mm, x = bill_depth_mm, shape = species)) +
  geom_point() +
  xlab("くちばしの深さ (mm)") +
  ylab("くちばしの長さ (mm)") +
  theme_classic() +
  theme(legend.position = "none")
print(p4)

# 参考: ポリコリック相関の概念図（テキストには使われていないが参考として有用）---------
pacman::p_load(grid)

rho <- 0.6
thresholds_x <- c(-1.5, 0, 1.5)
thresholds_y <- c(-1.5, -0.3, 1.2)

x_seq <- seq(-3.5, 3.5, length.out = 100)
y_seq <- seq(-3.5, 3.5, length.out = 100)
grid_data <- expand.grid(x = x_seq, y = y_seq)

Sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
grid_data$z <- dmvnorm(as.matrix(grid_data[, 1:2]), sigma = Sigma)

theta <- seq(0, 2*pi, length.out = 200)
L <- chol(Sigma)
r <- 2.2
ellipse_data <- data.frame(
  x = r * cos(theta),
  y = r * sin(theta)
)
ellipse_transformed <- as.matrix(ellipse_data) %*% L
ellipse_df <- data.frame(x = ellipse_transformed[, 1], y = ellipse_transformed[, 2])

p_main <- ggplot() +
  geom_polygon(data = ellipse_df, aes(x = x, y = y),
               fill = "gray80", alpha = 0.8) +
  geom_path(data = ellipse_df, aes(x = x, y = y),
            color = "black", linewidth = 1.5, linetype = "solid") +
  geom_vline(xintercept = thresholds_x, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  geom_hline(yintercept = thresholds_y, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  annotate("text", x = 0.3, y = 0.5, label = "背後に想定された",
           size = 5, color = "black", angle = 30, fontface = "bold") +
  annotate("text", x = 0.5, y = -0.2, label = "相関係数を",
           size = 5, color = "black", angle = 30, fontface = "bold") +
  annotate("text", x = 0.7, y = -0.9, label = "推定する",
           size = 5, color = "black", angle = 30, fontface = "bold") +
  coord_fixed(xlim = c(-3.5, 4.2), ylim = c(-4, 3.5)) +
  theme_void()
print(p_main)

cat("\n--- ポリコリック相関の概念図（参考）を表示しました ---\n")
