# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, cluster, factoextra, ggplot2, patchwork, psych,
               kohonen, mclust, mixtools, exametrika)

set.seed(12345)

# ワークライフバランスデータ生成 -------------------------------------------

n <- 200
cluster_sizes <- c(70, 80, 50)

work_focused <- data.frame(
  work_hours = rnorm(cluster_sizes[1], mean = 55, sd = 8),
  family_time = rnorm(cluster_sizes[1], mean = 15, sd = 5),
  personal_time = rnorm(cluster_sizes[1], mean = 8, sd = 3),
  work_satisfaction = rnorm(cluster_sizes[1], mean = 6.5, sd = 1.2),
  life_satisfaction = rnorm(cluster_sizes[1], mean = 5.0, sd = 1.5),
  stress_level = rnorm(cluster_sizes[1], mean = 6.2, sd = 1.3),
  true_cluster = 1
)

balanced <- data.frame(
  work_hours = rnorm(cluster_sizes[2], mean = 42, sd = 6),
  family_time = rnorm(cluster_sizes[2], mean = 25, sd = 6),
  personal_time = rnorm(cluster_sizes[2], mean = 18, sd = 5),
  work_satisfaction = rnorm(cluster_sizes[2], mean = 5.8, sd = 1.0),
  life_satisfaction = rnorm(cluster_sizes[2], mean = 6.5, sd = 1.2),
  stress_level = rnorm(cluster_sizes[2], mean = 4.0, sd = 1.2),
  true_cluster = 2
)

private_focused <- data.frame(
  work_hours = rnorm(cluster_sizes[3], mean = 35, sd = 5),
  family_time = rnorm(cluster_sizes[3], mean = 30, sd = 7),
  personal_time = rnorm(cluster_sizes[3], mean = 25, sd = 6),
  work_satisfaction = rnorm(cluster_sizes[3], mean = 4.8, sd = 1.4),
  life_satisfaction = rnorm(cluster_sizes[3], mean = 6.8, sd = 1.0),
  stress_level = rnorm(cluster_sizes[3], mean = 3.5, sd = 1.1),
  true_cluster = 3
)

wlb_data <- rbind(work_focused, balanced, private_focused)
wlb_data <- wlb_data[sample(nrow(wlb_data)), ]
rownames(wlb_data) <- NULL

wlb_data$work_hours <- pmax(25, pmin(70, round(wlb_data$work_hours, 1)))
wlb_data$family_time <- pmax(5, pmin(40, round(wlb_data$family_time, 1)))
wlb_data$personal_time <- pmax(3, pmin(35, round(wlb_data$personal_time, 1)))
wlb_data$work_satisfaction <- pmax(1, pmin(7, round(wlb_data$work_satisfaction, 1)))
wlb_data$life_satisfaction <- pmax(1, pmin(7, round(wlb_data$life_satisfaction, 1)))
wlb_data$stress_level <- pmax(1, pmin(7, round(wlb_data$stress_level, 1)))

analysis_data <- wlb_data[, 1:6]
scaled_data <- scale(analysis_data)
colnames(scaled_data) <- colnames(analysis_data)

cat("データ生成完了:", nrow(wlb_data), "名のサンプル\n")

# 図12.1: デンドログラム ---------------------------------------------------

dist_matrix <- dist(scaled_data, method = "euclidean")
hclust_ward <- hclust(dist_matrix, method = "ward.D2")

# 図12.1: 階層クラスター分析のデンドログラム（Ward法）
p_dendro <- suppressWarnings(
  fviz_dend(hclust_ward, k = 3, cex = 0.7,
            palette = c("black", "darkgray", "gray50"),
            main = "") +
    theme_minimal() +
    labs(x = "サンプル", y = "距離")
)
print(p_dendro)

# 図12.2: 階層クラスター別プロフィール ------------------------------------

hierarchical_clusters <- cutree(hclust_ward, k = 3)
wlb_hierarchical <- analysis_data
wlb_hierarchical$hierarchical_cluster <- as.factor(hierarchical_clusters)

cat("階層クラスター分析：クラスター数3での分類完了\n")
cat("クラスター1:", sum(hierarchical_clusters == 1), "名\n")
cat("クラスター2:", sum(hierarchical_clusters == 2), "名\n")
cat("クラスター3:", sum(hierarchical_clusters == 3), "名\n")

hierarchical_means <- wlb_hierarchical %>%
  group_by(hierarchical_cluster) %>%
  summarise(across(work_hours:stress_level, ~ round(mean(.x), 2)), .groups = "drop")

hierarchical_profile <- hierarchical_means %>%
  pivot_longer(cols = -hierarchical_cluster, names_to = "variable", values_to = "value")

hierarchical_profile$variable_jp <- recode(hierarchical_profile$variable,
  "work_hours" = "労働時間",
  "family_time" = "家族時間",
  "personal_time" = "個人時間",
  "work_satisfaction" = "仕事満足度",
  "life_satisfaction" = "生活満足度",
  "stress_level" = "ストレス度"
)

# 図12.2: 階層クラスター別の変数プロフィール
p_hierarchical_bar_simple <- ggplot(hierarchical_profile, aes(x = variable_jp, y = value,
                                                             fill = hierarchical_cluster)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("white", "darkgray", "black"),
                    labels = c("クラスター1", "クラスター2", "クラスター3")) +
  labs(x = "変数", y = "平均値", fill = "クラスター") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(p_hierarchical_bar_simple)

# 図12.3: k-means法の動作可視化 -------------------------------------------

set.seed(12345)
kmeans_demo_data <- analysis_data[1:20, c("work_hours", "family_time")]
colnames(kmeans_demo_data) <- c("x", "y")

k <- 3
step_plots <- list()

p0 <- ggplot(kmeans_demo_data, aes(x = x, y = y)) +
  geom_point(size = 2, color = "black") +
  labs(x = "労働時間（時間/週）", y = "家族時間（時間/週）") +
  theme_minimal() +
  xlim(25, 70) + ylim(5, 40)
step_plots[[1]] <- p0

set.seed(123)
initial_centers <- data.frame(
  x = c(35, 50, 60),
  y = c(35, 20, 10),
  cluster = factor(1:3)
)

p1 <- ggplot(kmeans_demo_data, aes(x = x, y = y)) +
  geom_point(size = 2, color = "black") +
  geom_point(data = initial_centers, aes(x = x, y = y, color = cluster),
             size = 4, shape = 17) +
  scale_color_manual(values = c("black", "darkgray", "gray50")) +
  labs(x = "労働時間（時間/週）", y = "家族時間（時間/週）", color = "中心") +
  theme_minimal() +
  xlim(25, 70) + ylim(5, 40) +
  theme(legend.position = "bottom")
step_plots[[2]] <- p1

# k-means法の反復
centers <- initial_centers[, c("x", "y")]
data_with_clusters <- kmeans_demo_data
plot_index <- 3
max_iter <- 3  # デモ用に3回に制限

for (iter in 1:max_iter) {
  distances <- as.matrix(dist(rbind(centers, kmeans_demo_data)))
  distances <- distances[1:k, (k+1):(k+nrow(kmeans_demo_data))]
  cluster_assignments <- apply(distances, 2, which.min)
  data_with_clusters$cluster <- factor(cluster_assignments)

  center_data <- data.frame(centers, cluster = factor(1:3))

  line_data <- data.frame()
  for (i in 1:nrow(data_with_clusters)) {
    cluster_id <- as.numeric(data_with_clusters$cluster[i])
    line_data <- rbind(line_data, data.frame(
      x = c(data_with_clusters$x[i], center_data$x[cluster_id]),
      y = c(data_with_clusters$y[i], center_data$y[cluster_id]),
      group = i,
      cluster = data_with_clusters$cluster[i]
    ))
  }

  p_assign <- ggplot() +
    geom_line(data = line_data, aes(x = x, y = y, group = group, color = cluster),
              alpha = 0.5, linewidth = 0.5) +
    geom_point(data = data_with_clusters, aes(x = x, y = y, color = cluster), size = 2) +
    geom_point(data = center_data, aes(x = x, y = y, color = cluster),
               size = 4, shape = 17) +
    scale_color_manual(values = c("black", "darkgray", "gray50")) +
    labs(x = "労働時間（時間/週）", y = "家族時間（時間/週）", color = "クラスター") +
    theme_minimal() +
    xlim(25, 70) + ylim(5, 40) +
    theme(legend.position = "bottom")

  step_plots[[plot_index]] <- p_assign
  plot_index <- plot_index + 1

  old_centers <- centers
  for (i in 1:k) {
    cluster_points <- data_with_clusters[data_with_clusters$cluster == i, c("x", "y")]
    if (nrow(cluster_points) > 0) {
      centers[i, ] <- colMeans(cluster_points)
    }
  }

  new_center_data <- data.frame(centers, cluster = factor(1:3))
  p_new_center <- ggplot() +
    geom_point(data = data_with_clusters, aes(x = x, y = y, color = cluster), size = 2) +
    geom_point(data = new_center_data, aes(x = x, y = y, color = cluster),
               size = 4, shape = 17) +
    scale_color_manual(values = c("black", "darkgray", "gray50")) +
    labs(x = "労働時間（時間/週）", y = "家族時間（時間/週）", color = "クラスター") +
    theme_minimal() +
    xlim(25, 70) + ylim(5, 40) +
    theme(legend.position = "bottom")

  step_plots[[plot_index]] <- p_new_center
  plot_index <- plot_index + 1
}

# 使用するプロット（最初の8個）
plots_for_combination <- step_plots[1:min(8, length(step_plots))]
plots_no_legend <- lapply(plots_for_combination, function(p) {
  p + theme(legend.position = "none")
})

# 図12.3: k-means法の動作過程（ステップごとの変化）
combined_plot <- wrap_plots(plots_no_legend, nrow = 2)
print(combined_plot)

# 図12.4: SOM（自己組織化マップ）分析 ------------------------------------

som_data <- as.matrix(scaled_data)
set.seed(125)
som_grid <- somgrid(xdim = 7, ydim = 7, topo = "rectangular")
som_model <- som(som_data, grid = som_grid, rlen = 1000, alpha = c(0.05, 0.01))

cat("SOM訓練完了 -", som_model$grid$xdim, "x", som_model$grid$ydim, "グリッド\n")

som_coords <- data.frame(
  unit = 1:nrow(som_model$codes[[1]]),
  x = som_model$grid$pts[, 1],
  y = som_model$grid$pts[, 2]
)

som_weights <- as.data.frame(som_model$codes[[1]])
som_weights$unit <- 1:nrow(som_weights)
som_plot_data <- merge(som_coords, som_weights, by = "unit")

# コンポーネント平面（各変数の分布）
som_components_long <- som_plot_data %>%
  select(unit, x, y, work_hours:stress_level) %>%
  pivot_longer(cols = work_hours:stress_level,
               names_to = "variable", values_to = "value")

som_components_long$variable_jp <- recode(som_components_long$variable,
  "work_hours" = "労働時間",
  "family_time" = "家族時間",
  "personal_time" = "個人時間",
  "work_satisfaction" = "仕事満足度",
  "life_satisfaction" = "生活満足度",
  "stress_level" = "ストレス度"
)

# 図12.4: SOMコンポーネント平面（変数ごとのニューロン重み）
p_components <- ggplot(som_components_long, aes(x = x, y = y, fill = value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "white", high = "black", name = "重み") +
  facet_wrap(~ variable_jp, ncol = 3) +
  labs(x = "SOM X座標", y = "SOM Y座標") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10)) +
  coord_fixed()
print(p_components)

# 図12.5: 混合分布モデルの概念図 -------------------------------------------

set.seed(12345)
n_total_mix <- 1000

component1 <- list(mean = 9, sd = 4, weight = 0.55)
component2 <- list(mean = 13, sd = 1.3, weight = 0.45)

n1_mix <- round(n_total_mix * component1$weight)
n2_mix <- n_total_mix - n1_mix

data1_mix <- rnorm(n1_mix, mean = component1$mean, sd = component1$sd)
data2_mix <- rnorm(n2_mix, mean = component2$mean, sd = component2$sd)
mixed_data <- c(data1_mix, data2_mix)

cat("混合データ生成完了:", length(mixed_data), "サンプル\n")

single_mean <- mean(mixed_data)
single_sd <- sd(mixed_data)

cat("\n混合分布モデルでの推定中...\n")
gmm_result <- normalmixEM(mixed_data, k = 2, maxit = 1000, epsilon = 1e-08)
cat("混合分布推定完了:\n")
for (i in 1:2) {
  cat("成分", i, ": 平均 =", round(gmm_result$mu[i], 2),
      ", SD =", round(gmm_result$sigma[i], 2),
      ", 重み =", round(gmm_result$lambda[i], 3), "\n")
}

x_range_mix <- seq(min(mixed_data) - 2, max(mixed_data) + 2, length.out = 500)
single_density <- dnorm(x_range_mix, mean = single_mean, sd = single_sd)

components_data_mix <- data.frame(
  x = rep(x_range_mix, 2),
  density = c(
    gmm_result$lambda[1] * dnorm(x_range_mix, gmm_result$mu[1], gmm_result$sigma[1]),
    gmm_result$lambda[2] * dnorm(x_range_mix, gmm_result$mu[2], gmm_result$sigma[2])
  ),
  component = factor(rep(1:2, each = length(x_range_mix)))
)

plot_data_mix <- data.frame(
  value = mixed_data,
  true_component = factor(c(rep(1, n1_mix), rep(2, n2_mix)))
)

p_single <- ggplot() +
  geom_histogram(
    data = plot_data_mix, aes(x = value, y = after_stat(density)),
    bins = 30, fill = "lightgray", color = "black", alpha = 0.7
  ) +
  geom_line(
    data = data.frame(x = x_range_mix, density = single_density),
    aes(x = x, y = density), color = "black", linewidth = 2
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(range(mixed_data))

p_mixture_plot <- ggplot() +
  geom_histogram(
    data = plot_data_mix, aes(x = value, y = after_stat(density)),
    bins = 30, fill = "lightgray", color = "black", alpha = 0.7
  ) +
  geom_line(
    data = components_data_mix,
    aes(x = x, y = density, linetype = component),
    color = "black", linewidth = 1.5
  ) +
  scale_linetype_manual(values = c("solid", "solid")) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(range(mixed_data))

# 図12.5: 混合分布モデルの概念図（単一正規分布 vs 混合分布）
p_mix_combined <- p_single + p_mixture_plot
print(p_mix_combined)

# 図12.6: Mclustプロフィールプロット ----------------------------------

set.seed(12345)
mclust_result <- Mclust(scaled_data, G = 1:10)

cat("Mclust分析完了\n")
cat("最適クラスター数:", mclust_result$G, "\n")
cat("最適モデル:", mclust_result$modelName, "\n")

mclust_clusters <- mclust_result$classification
wlb_mclust <- analysis_data
wlb_mclust$mclust_cluster <- as.factor(mclust_clusters)

mclust_profile <- wlb_mclust %>%
  select(mclust_cluster, work_hours:stress_level) %>%
  group_by(mclust_cluster) %>%
  summarise(across(everything(), mean), .groups = "drop") %>%
  pivot_longer(cols = -mclust_cluster, names_to = "variable", values_to = "value")

mclust_profile$variable_jp <- recode(mclust_profile$variable,
  "work_hours" = "労働時間",
  "family_time" = "家族時間",
  "personal_time" = "個人時間",
  "work_satisfaction" = "仕事満足度",
  "life_satisfaction" = "生活満足度",
  "stress_level" = "ストレス度"
)

# 図12.6: Mclustクラスタープロフィール（折れ線グラフ）
p_mclust_profile <- ggplot(mclust_profile, aes(x = variable_jp, y = value,
                                              group = mclust_cluster,
                                              color = mclust_cluster)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("black", "darkgray", "gray50"),
                     labels = paste0("クラスター", 1:length(unique(mclust_profile$mclust_cluster)))) +
  labs(x = "変数", y = "平均値", color = "クラスター") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(p_mclust_profile)

# 図12.7: バイクラスタリング -----------------------------------------------

# exametrikaパッケージのサンプルデータを使用
ret <- GridSearch(J15S500, method = "R")

# 図12.7: バイクラスタリングの最適解（配列表示）
plot(ret$optimal_result, type = "Array")

cat("最適クラスター数:", ret$optimal_ncls, "\n")
cat("最適フィールド数:", ret$optimal_nfld, "\n")

cat("\n=== クラスター分析完了 ===\n")
