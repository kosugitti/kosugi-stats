# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, palmerpenguins, ggrain)

# 図2.1: 正規分布曲線（3種類の正規分布）-------------------------------------

x <- seq(-6, 6, length.out = 1000)

df <- data.frame(
  x = rep(x, 3),
  y = c(
    dnorm(x, mean = 0, sd = 1),
    dnorm(x, mean = 1, sd = 2),
    dnorm(x, mean = -1, sd = sqrt(0.5))
  ),
  distribution = rep(c("平均0, SD1", "平均1, SD1", "平均-1, SD2"), each = length(x))
)

# 図2.1: 3種類の正規分布曲線（線種で区別）
g1 <- ggplot(df, aes(x = x, y = y, linetype = distribution)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted")) +
  labs(
    x = "x",
    y = "密度"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
print(g1)

# 参考: 95%区間の可視化（テキストには使われていないが参考として有用）-----------

x <- seq(-4, 4, length.out = 1000)
y <- dnorm(x)
df_normal <- data.frame(x = x, y = y)

critical_value <- 1.96
area_data <- subset(df_normal, x >= -critical_value & x <= critical_value)

p_95 <- ggplot(df_normal, aes(x = x, y = y)) +
  geom_line(size = 1) +
  geom_area(data = area_data, aes(x = x, y = y), fill = "grey80", alpha = 0.8) +
  geom_segment(aes(x = -critical_value, y = 0, xend = -critical_value,
                   yend = dnorm(-critical_value)), linetype = "dashed") +
  geom_segment(aes(x = critical_value, y = 0, xend = critical_value,
                   yend = dnorm(critical_value)), linetype = "dashed") +
  scale_x_continuous(
    breaks = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
    labels = c("-4", "-3", "-2", "-1", "0", "1", "2", "3", "4")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.42)) +
  labs(x = "標準偏差", y = "密度") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
print(p_95)
