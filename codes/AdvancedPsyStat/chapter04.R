# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, MASS, patchwork)

# 図4.1: 散布図と回帰直線（相関データ）--------------------------------------

set.seed(12345)
N <- 200
dat_mvn <- MASS::mvrnorm(N, mu = c(0, 0), Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2)) %>%
  as.data.frame() %>%
  rename(x = V1, y = V2)

# 図4.1: 散布図と回帰直線
g <- ggplot(dat_mvn, aes(x, y)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "black")
print(g)

# プロ野球選手の体重・身長データ（仮想データ）-------------------------------
set.seed(42)
dat <- data.frame(
  weight = rnorm(300, mean = 80, sd = 10),
  height = rnorm(300, mean = 180, sd = 6)
)
dat$height <- dat$height + 0.5 * (dat$weight - 80)

result.lm <- lm(height ~ weight, data = dat)

# 図4.2: Q-Qプロット（回帰残差の正規性確認）---------------------------------

# 図4.2: 残差のQ-Qプロット
g_qq <- ggplot(data = NULL, aes(sample = resid(result.lm))) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(x = "理論値", y = "標本値")
print(g_qq)

# 図4.3: 逆U字型の散布図 ---------------------------------------------------

set.seed(123)
x_val <- seq(-3, 3, length.out = 100)
y_val <- -x_val^2 + rnorm(300, sd = 0.5)
df_u <- data.frame(x = x_val, y = y_val)

# 図4.3: 逆U字型の散布図（非線形関係の例）
p_u <- ggplot(df_u, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(x = "説明変数", y = "目的変数")
print(p_u)

# 図4.4・4.5: 2群の実験状況の散布図 -----------------------------------------

set.seed(1234)
N_exp <- 30
x1 <- rnorm(N_exp, mean = 1)
x2 <- rnorm(N_exp)

df_exp <- data.frame(
  group = factor(rep(c("統制群", "実験群"), each = N_exp), levels = c("統制群", "実験群")),
  value = c(x2, x1)
)

# 散布図1：基本的な散布図（平均値バー付き）
p1_exp <- ggplot(df_exp, aes(x = group, y = value)) +
  geom_point(alpha = 0.6) +
  geom_segment(
    data = df_exp %>% group_by(group) %>% summarise(mean_value = mean(value)),
    aes(
      x = as.numeric(group) - 0.1, xend = as.numeric(group) + 0.1,
      y = mean_value, yend = mean_value
    ),
    color = "gray", linewidth = 1
  ) +
  theme_minimal() +
  labs(x = "群", y = "従属変数")

# 散布図3：全体平均を通る直線（平均値バー付き）
p3_exp <- ggplot(df_exp, aes(x = group, y = value)) +
  geom_point(alpha = 0.6) +
  geom_segment(
    data = df_exp %>% group_by(group) %>% summarise(mean_value = mean(value)),
    aes(
      x = as.numeric(group) - 0.1, xend = as.numeric(group) + 0.1,
      y = mean_value, yend = mean_value
    ),
    color = "gray", linewidth = 1
  ) +
  geom_hline(yintercept = mean(df_exp$value), linetype = "dashed", color = "gray", linewidth = 1) +
  theme_minimal() +
  labs(x = "群", y = "従属変数")

# 散布図2：平均値を通る回帰直線（平均値バー付き）
p2_exp <- ggplot(df_exp, aes(x = group, y = value)) +
  geom_point(alpha = 0.6) +
  geom_segment(
    data = df_exp %>% group_by(group) %>% summarise(mean_value = mean(value)),
    aes(
      x = as.numeric(group) - 0.1, xend = as.numeric(group) + 0.1,
      y = mean_value, yend = mean_value
    ),
    color = "gray", linewidth = 1
  ) +
  geom_segment(
    x = 0.5, xend = 2.5,
    y = mean(df_exp$value[df_exp$group == "統制群"]) - 0.6,
    yend = mean(df_exp$value[df_exp$group == "実験群"]) + 0.6,
    color = "gray", linewidth = 1
  ) +
  theme_minimal() +
  labs(x = "群", y = "従属変数")

p4_exp <- p3_exp + p2_exp

# 図4.4: 2群の散布図（基本）
print(p1_exp)

# 図4.5: 2群の散布図（全体平均線 + 回帰直線の比較）
print(p4_exp)
