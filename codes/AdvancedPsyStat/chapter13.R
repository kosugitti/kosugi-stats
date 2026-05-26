# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, ggplot2, patchwork, smacof, cluster, ggrepel,
               igraph, ggraph, ppcor, maps, mapdata, bootnet, qgraph)

set.seed(12345)

# 図13.1: MDS分析（eurodistデータ）----------------------------------------

data(eurodist)
cat("eurodistデータ読み込み完了\n")
cat("都市数:", nrow(eurodist), "\n")

# 古典的多次元尺度法の実行
mds_result_euro <- cmdscale(eurodist, k = 2, eig = TRUE)
mds_coords_euro <- mds_result_euro$points

eigenvals <- mds_result_euro$eig[mds_result_euro$eig > 0]
prop_var <- eigenvals / sum(eigenvals) * 100
cat("第1次元:", round(prop_var[1], 1), "%\n")
cat("第2次元:", round(sum(prop_var[1:2]), 1), "%\n")

mds_data_euro <- data.frame(
  city = rownames(mds_coords_euro),
  x = mds_coords_euro[, 1],
  y = -mds_coords_euro[, 2]  # Y座標を反転
)

p_mds_euro <- ggplot(mds_data_euro, aes(x = x, y = y)) +
  geom_point(size = 4, color = "darkblue") +
  geom_text_repel(aes(label = city), size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.3,
                  point.padding = 0.3) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.text = element_blank(),
        axis.ticks = element_blank())

# ヨーロッパ地図の取得
cat("ヨーロッパ地図データ取得中...\n")
world_map <- map_data("world")
europe_map <- world_map %>%
  filter(long >= -10 & long <= 30,
         lat >= 35 & lat <= 70)

real_coords <- data.frame(
  city = c("Athens", "Barcelona", "Brussels", "Calais", "Cherbourg",
           "Cologne", "Copenhagen", "Geneva", "Gibraltar", "Hamburg",
           "Hook of Holland", "Lisbon", "Lyons", "Madrid", "Marseilles",
           "Milan", "Munich", "Paris", "Rome", "Stockholm", "Vienna"),
  longitude = c(23.73, 2.15, 4.35, 1.86, -1.64, 6.96, 12.57, 6.14, -5.35, 9.99,
                4.24, -9.13, 4.84, -3.70, 5.37, 9.19, 11.58, 2.35, 12.50, 18.06, 16.36),
  latitude = c(37.98, 41.39, 50.85, 50.95, 49.65, 50.94, 55.68, 46.20, 36.14, 53.55,
               52.00, 38.72, 45.76, 40.42, 43.30, 45.46, 48.14, 48.86, 41.90, 59.33, 48.21)
)

combined_data_euro <- mds_data_euro %>%
  left_join(real_coords, by = "city") %>%
  filter(!is.na(longitude))

p_map_euro <- ggplot() +
  geom_polygon(data = europe_map,
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white", linewidth = 0.3) +
  geom_point(data = combined_data_euro,
             aes(x = longitude, y = latitude),
             size = 4, color = "darkred") +
  geom_text_repel(data = combined_data_euro,
                  aes(x = longitude, y = latitude, label = city),
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.3,
                  point.padding = 0.3) +
  labs(x = NULL, y = NULL) +
  coord_fixed(1.3) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

# 図13.1: MDS結果 vs ヨーロッパ地図の比較
p_mds_comparison <- p_mds_euro | p_map_euro
print(p_mds_comparison)

# 図13.2: Stress値プロット -----------------------------------------------

emotions <- c("喜び", "悲しみ", "怒り", "恐れ", "驚き", "嫌悪",
              "愛情", "不安", "興奮", "リラックス", "退屈", "満足",
              "憤怒", "幸福", "憂鬱", "平静")

emotion_coords <- data.frame(
  emotion = emotions,
  valence = c(0.8, -0.8, -0.6, -0.7, 0.1, -0.9,
              0.9, -0.5, 0.7, 0.4, -0.3, 0.6,
              -0.8, 0.9, -0.9, 0.2),
  arousal = c(0.6, -0.3, 0.8, 0.4, 0.9, 0.2,
              0.2, 0.6, 0.8, -0.8, -0.7, -0.2,
              0.9, 0.4, -0.5, -0.6)
)

n_emotions <- nrow(emotion_coords)
true_dist_matrix <- matrix(0, n_emotions, n_emotions)
rownames(true_dist_matrix) <- colnames(true_dist_matrix) <- emotion_coords$emotion

for (i in 1:n_emotions) {
  for (j in 1:n_emotions) {
    if (i != j) {
      true_dist_matrix[i, j] <- sqrt((emotion_coords$valence[i] - emotion_coords$valence[j])^2 +
                       (emotion_coords$arousal[i] - emotion_coords$arousal[j])^2)
    }
  }
}

observed_similarity <- matrix(0, n_emotions, n_emotions)
rownames(observed_similarity) <- colnames(observed_similarity) <- emotion_coords$emotion
max_dist <- max(true_dist_matrix)

for (i in 1:n_emotions) {
  for (j in 1:n_emotions) {
    if (i == j) {
      observed_similarity[i, j] <- 5
    } else {
      base_similarity <- 5 - (true_dist_matrix[i, j] / max_dist) * 4
      noise <- runif(1, -1.0, 1.0)
      observed_similarity[i, j] <- pmax(1, pmin(5, base_similarity + noise))
    }
  }
}

for (i in 1:n_emotions) {
  for (j in (i+1):n_emotions) {
    if (j <= n_emotions) {
      avg_sim <- (observed_similarity[i, j] + observed_similarity[j, i]) / 2
      observed_similarity[i, j] <- observed_similarity[j, i] <- avg_sim
    }
  }
}

dissimilarity_matrix <- 6 - observed_similarity

# 次元数ごとのStress値計算
dimensions <- 1:10
stress_values <- numeric(length(dimensions))

cat("\n次元数ごとのStress値計算中...\n")
for (i in seq_along(dimensions)) {
  k <- dimensions[i]
  mds_res <- mds(dissimilarity_matrix, ndim = k, type = "ordinal")
  stress_values[i] <- mds_res$stress
  cat("次元数", k, ": Stress =", round(stress_values[i], 4), "\n")
}

stress_data_plot <- data.frame(
  dimensions = dimensions,
  stress = stress_values
)

# 図13.2: Stress値の変化（次元数選択の基準）
p_stress <- ggplot(stress_data_plot, aes(x = dimensions, y = stress)) +
  geom_line(linewidth = 1, color = "darkblue") +
  geom_point(size = 3, color = "darkblue") +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(x = "次元数", y = "Stress値") +
  scale_x_continuous(breaks = dimensions) +
  theme_minimal()
print(p_stress)

# 図13.3: MDS結果のクラスター分類 ----------------------------------------

optimal_dim <- 2
final_mds <- mds(dissimilarity_matrix, ndim = optimal_dim, type = "ordinal")

cat("最終Stress値:", round(final_mds$stress, 4), "\n")

mds_coords_emo <- final_mds$conf
rownames(mds_coords_emo) <- emotion_coords$emotion

mds_df_emo <- data.frame(
  emotion = rownames(mds_coords_emo),
  x = mds_coords_emo[, 1],
  y = mds_coords_emo[, 2]
)

mds_dist_emo <- dist(mds_coords_emo, method = "euclidean")
hc_result_emo <- hclust(mds_dist_emo, method = "ward.D2")
n_clusters_emo <- 4
cluster_assignment_emo <- cutree(hc_result_emo, k = n_clusters_emo)
mds_df_emo$cluster <- factor(cluster_assignment_emo)

cluster_shapes <- c(16, 15, 17, 18)

# 図13.3: 感情語のMDS配置（クラスター分類）
p_mds_clusters <- ggplot(mds_df_emo, aes(x = x, y = y)) +
  geom_point(aes(shape = cluster), size = 5, color = "black") +
  scale_shape_manual(values = cluster_shapes) +
  geom_text_repel(aes(label = emotion), size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.4,
                  point.padding = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.5) +
  labs(x = "第1次元", y = "第2次元") +
  theme_minimal() +
  theme(aspect.ratio = 1, legend.position = "none")
print(p_mds_clusters)

# 図13.4: 多次元展開法（food map）-----------------------------------------

data(breakfast)
cat("\nbreakfastデータ読み込み完了\n")
cat("データ構造:", nrow(breakfast), "人 x", ncol(breakfast), "種類の朝食アイテム\n")

unfold_result <- unfolding(breakfast, ndim = 2, type = "ordinal")
cat("多次元展開法完了 Stress値:", round(unfold_result$stress, 4), "\n")

people_coords_unfold <- unfold_result$conf.row
food_coords_unfold <- unfold_result$conf.col

people_df_unfold <- data.frame(
  id = 1:nrow(people_coords_unfold),
  x = people_coords_unfold[, 1],
  y = people_coords_unfold[, 2],
  type = "人"
)

food_df_unfold <- data.frame(
  id = colnames(breakfast),
  x = food_coords_unfold[, 1],
  y = food_coords_unfold[, 2],
  type = "食べ物"
)

unfold_df <- rbind(
  people_df_unfold %>% mutate(label = paste("P", id, sep="")),
  food_df_unfold %>% mutate(label = id)
)

# 図13.4: 食べ物マップ（食品アイテムと個人の選好空間）
p_food_map <- ggplot() +
  geom_point(data = filter(unfold_df, type == "人"),
             aes(x = x, y = y),
             shape = 16, size = 1.5, color = "grey60", alpha = 0.5) +
  geom_point(data = filter(unfold_df, type == "食べ物"),
             aes(x = x, y = y),
             shape = 17, size = 3.5, color = "black") +
  geom_text(data = filter(unfold_df, type == "食べ物"),
            aes(x = x, y = y, label = label), size = 2.5,
            hjust = 0, vjust = 0, nudge_x = 0.05, nudge_y = 0.05) +
  labs(x = "第1次元", y = "第2次元") +
  theme_minimal() +
  theme(aspect.ratio = 1)
print(p_food_map)

# 図13.5: INDSCAL（個人差多次元尺度法）-------------------------------------

n_people_ind <- nrow(breakfast)
n_items_ind <- ncol(breakfast)
item_names_ind <- colnames(breakfast)

dist_matrices_ind <- list()
for (i in 1:n_people_ind) {
  person_prefs <- as.numeric(breakfast[i, ])
  dist_matrix_ind <- matrix(0, n_items_ind, n_items_ind)
  for (j in 1:n_items_ind) {
    for (k in 1:n_items_ind) {
      dist_matrix_ind[j, k] <- abs(person_prefs[j] - person_prefs[k])
    }
  }
  rownames(dist_matrix_ind) <- colnames(dist_matrix_ind) <- item_names_ind
  dist_matrices_ind[[i]] <- dist_matrix_ind
}
names(dist_matrices_ind) <- paste("Person", 1:n_people_ind)

cat("\nINDSCAL分析実行中...\n")
indscal_result <- indscal(dist_matrices_ind, ndim = 2, type = "ordinal")
cat("INDSCAL分析完了 全体Stress値:", round(indscal_result$stress, 4), "\n")

group_space <- indscal_result$gspace
weight_matrices_ind <- indscal_result$cweights

individual_weights <- matrix(0, n_people_ind, 2)
for (i in 1:n_people_ind) {
  individual_weights[i, 1] <- weight_matrices_ind[[i]][1, 1]
  individual_weights[i, 2] <- weight_matrices_ind[[i]][2, 2]
}

gspace_df <- data.frame(
  item = rownames(group_space),
  x = group_space[, 1],
  y = group_space[, 2]
)

weights_df <- data.frame(
  person = 1:nrow(individual_weights),
  dim1_weight = individual_weights[, 1],
  dim2_weight = individual_weights[, 2]
)

dim1_max_person <- which.max(individual_weights[, 1])
dim2_max_person <- which.max(individual_weights[, 2])

person1_space <- group_space
person1_space[, 1] <- group_space[, 1] * sqrt(individual_weights[dim1_max_person, 1])
person1_space[, 2] <- group_space[, 2] * sqrt(individual_weights[dim1_max_person, 2])

person2_space <- group_space
person2_space[, 1] <- group_space[, 1] * sqrt(individual_weights[dim2_max_person, 1])
person2_space[, 2] <- group_space[, 2] * sqrt(individual_weights[dim2_max_person, 2])

group_df_ind <- data.frame(item = rownames(group_space), x = group_space[, 1],
                           y = group_space[, 2], space = "1. 共通空間")
person1_df_ind <- data.frame(item = rownames(person1_space), x = person1_space[, 1],
                              y = person1_space[, 2],
                              space = paste0("2. Person ", dim1_max_person, "\n(第1次元重視)"))
person2_df_ind <- data.frame(item = rownames(person2_space), x = person2_space[, 1],
                              y = person2_space[, 2],
                              space = paste0("3. Person ", dim2_max_person, "\n(第2次元重視)"))

all_spaces_df <- rbind(group_df_ind, person1_df_ind, person2_df_ind)
all_spaces_df$space <- factor(all_spaces_df$space,
                              levels = c("1. 共通空間",
                                        paste0("2. Person ", dim1_max_person, "\n(第1次元重視)"),
                                        paste0("3. Person ", dim2_max_person, "\n(第2次元重視)")))

x_range_ind <- range(all_spaces_df$x)
y_range_ind <- range(all_spaces_df$y)

# 図13.5: INDSCAL（共通空間 vs 個人空間の伸縮比較）
p_indscal_comparison <- ggplot(all_spaces_df, aes(x = x, y = y)) +
  geom_point(size = 3, color = "darkblue") +
  geom_text(aes(label = item), size = 2.5,
            hjust = 0, vjust = 0, nudge_x = 0.05, nudge_y = 0.05) +
  facet_wrap(~ space, ncol = 3, scales = "fixed") +
  labs(x = "第1次元", y = "第2次元") +
  xlim(x_range_ind) + ylim(y_range_ind) +
  theme_minimal() +
  theme(aspect.ratio = 1, strip.text = element_text(size = 10))
print(p_indscal_comparison)

# 図13.6: 相関ネットワーク vs 偏相関ネットワーク（自己完結版）-------------

# 自己完結データ生成（うつ症状の仮想データ）
set.seed(12345)
n_net <- 300
n_symptoms <- 10

# 4つのクラスターを持つ共分散行列を構築
Sigma_net <- matrix(0.2, n_symptoms, n_symptoms)
diag(Sigma_net) <- 1
# クラスター内の相関を高める
for (i in 1:3) for (j in 1:3) if (i != j) Sigma_net[i, j] <- 0.6
for (i in 4:6) for (j in 4:6) if (i != j) Sigma_net[i, j] <- 0.6
for (i in 7:9) for (j in 7:9) if (i != j) Sigma_net[i, j] <- 0.6

depression_sim <- MASS::mvrnorm(n_net, mu = rep(0, n_symptoms), Sigma = Sigma_net)
colnames(depression_sim) <- c("悲観", "無価値", "自殺念", "悲しみ", "興味", "イライラ",
                               "疲労", "睡眠", "集中", "引込")

# 相関行列と偏相関行列の計算
cor_matrix_net <- cor(depression_sim)
pcor_result_net <- pcor(depression_sim)
pcor_matrix_net <- pcor_result_net$estimate

short_names_net <- colnames(depression_sim)

# 共通レイアウト
set.seed(12345)
layout_matrix_net <- cor_matrix_net
layout_matrix_net[abs(layout_matrix_net) < 0.2] <- 0
layout_graph_net <- graph_from_adjacency_matrix(layout_matrix_net,
                                           mode = "undirected",
                                           weighted = TRUE,
                                           diag = FALSE)
V(layout_graph_net)$name <- short_names_net
layout_coords_net <- layout_with_fr(layout_graph_net)
rownames(layout_coords_net) <- short_names_net

# ネットワーク図作成関数
create_network_plot_local <- function(matrix_data, threshold, node_names, layout_coords) {
  filtered_matrix <- matrix_data
  filtered_matrix[abs(filtered_matrix) < threshold] <- 0

  graph <- graph_from_adjacency_matrix(filtered_matrix,
                                       mode = "undirected",
                                       weighted = TRUE,
                                       diag = FALSE)
  V(graph)$name <- node_names

  edge_list <- as_edgelist(graph, names = TRUE)
  if (nrow(edge_list) == 0) {
    node_df <- data.frame(name = node_names, x = layout_coords[,1], y = layout_coords[,2])
    return(ggplot() +
      geom_point(data = node_df, aes(x = x, y = y), shape = 21, size = 10,
                 fill = "white", color = "black", stroke = 1.0) +
      geom_text(data = node_df, aes(x = x, y = y, label = name), size = 2.8, fontface = "bold") +
      theme_void() + theme(legend.position = "none") + coord_fixed())
  }

  edge_weights <- E(graph)$weight
  edge_df <- data.frame(
    from = edge_list[,1], to = edge_list[,2],
    weight = edge_weights, abs_weight = abs(edge_weights),
    sign = ifelse(edge_weights >= 0, "positive", "negative")
  )

  node_df <- data.frame(name = node_names, x = layout_coords[,1], y = layout_coords[,2])

  ggplot() +
    geom_segment(data = edge_df,
                 aes(x = node_df$x[match(from, node_df$name)],
                     y = node_df$y[match(from, node_df$name)],
                     xend = node_df$x[match(to, node_df$name)],
                     yend = node_df$y[match(to, node_df$name)],
                     linewidth = abs_weight,
                     linetype = sign),
                 color = "black", alpha = 0.8, show.legend = FALSE) +
    geom_point(data = node_df, aes(x = x, y = y), shape = 21, size = 10,
               fill = "white", color = "black", stroke = 1.0) +
    geom_text(data = node_df, aes(x = x, y = y, label = name), size = 2.8, fontface = "bold") +
    scale_linewidth_continuous(range = c(0.2, 1.0)) +
    scale_linetype_manual(values = c("positive" = "solid", "negative" = "dashed")) +
    theme_void() +
    theme(legend.position = "none", plot.margin = margin(15, 15, 15, 15)) +
    coord_fixed()
}

cor_plot_net <- create_network_plot_local(cor_matrix_net, 0.3, short_names_net, layout_coords_net)
pcor_plot_net <- create_network_plot_local(pcor_matrix_net, 0.2, short_names_net, layout_coords_net)

# 図13.6: 相関ネットワーク vs 偏相関ネットワークの比較
p_cor_vs_pcor <- cor_plot_net + pcor_plot_net + plot_layout(ncol = 2)
print(p_cor_vs_pcor)

# 図13.7: ネットワーク分析の数値例 -----------------------------------------

# bootnetパッケージを使用したネットワーク推定（有意性テスト付き）
network_sig <- estimateNetwork(as.data.frame(depression_sim),
                              default = "pcor",
                              threshold = "sig",
                              alpha = 0.05)

cat("\n有意な辺の数:", sum(network_sig$graph != 0) / 2, "\n")

graph_sig <- graph_from_adjacency_matrix(network_sig$graph,
                                        mode = "undirected",
                                        weighted = TRUE,
                                        diag = FALSE)
V(graph_sig)$name <- short_names_net
E(graph_sig)$weight <- abs(E(graph_sig)$weight)

# 図13.7: 有意な辺のネットワーク図（抑うつ症状）
network_plot_sig <- ggraph(graph_sig, layout = "fr") +
  geom_edge_link(aes(width = weight),
                 color = "black", alpha = 0.7, show.legend = FALSE) +
  geom_node_point(shape = 21, size = 10, fill = "white", color = "black", stroke = 1.0) +
  geom_node_text(aes(label = name), color = "black", size = 2.5, fontface = "bold") +
  scale_edge_width_continuous(range = c(0.3, 2.0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10))
print(network_plot_sig)

# 中心性指数の計算
centrality_results_net <- centralityTable(network_sig)
strength_centrality <- centrality_results_net %>%
  filter(measure == "Strength") %>%
  arrange(desc(value))

cat("\n中心性指数（強度中心性 上位5件）:\n")
print(strength_centrality[1:5, c("node", "value")])

cat("\n=== 多次元尺度法・ネットワーク分析完了 ===\n")
