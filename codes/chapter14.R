# ============================================================
# 心理統計法特論（放送大学大学院）サポートスクリプト
# 著者: 小杉考司
# ライセンス: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed.ja)
# ============================================================
# このスクリプトは印刷教材の図を再現します。
# RStudioで実行すると、Plotsペインに図が表示されます。
# ============================================================

rm(list = ls())
pacman::p_load(tidyverse, ca, smacof, igraph, ggraph, ggrepel, patchwork, MASS)

set.seed(12345)

# 図14.1〜14.3: テキスト解析の可視化（自己完結版）--------------------------
# 注記: 元のスクリプト（20251229chap14f.R）は日本語テキストの形態素解析データに
# 依存していたため，ここでは概念的に同等の仮想データを使用します。

# 心理学分野の仮想テキストデータ生成 ---------------------------------------

fields <- c("基礎心理学", "臨床心理学", "応用心理学")
n_docs <- 15

# 各分野の特徴語（日本語）
terms_basic <- c("認知", "知覚", "記憶", "学習", "注意", "思考")
terms_clinical <- c("治療", "診断", "症状", "カウンセリング", "適応", "不安")
terms_applied <- c("評価", "尺度", "測定", "統計", "調査", "分析")
all_terms <- c(terms_basic, terms_clinical, terms_applied)
n_terms <- length(all_terms)

# 文書-単語行列の生成（分野ごとに異なるパターン）
doc_field <- rep(fields, each = 5)
doc_term_matrix <- matrix(0, nrow = n_docs, ncol = n_terms)
rownames(doc_term_matrix) <- paste0("Doc", 1:n_docs)
colnames(doc_term_matrix) <- all_terms

for (i in 1:n_docs) {
  field <- doc_field[i]
  if (field == "基礎心理学") {
    # 基礎心理学の文書は認知・知覚関連語が多い
    doc_term_matrix[i, 1:6] <- rpois(6, lambda = c(8, 6, 5, 4, 7, 4))
    doc_term_matrix[i, 7:12] <- rpois(6, lambda = c(2, 1, 2, 1, 2, 2))
    doc_term_matrix[i, 13:18] <- rpois(6, lambda = c(3, 3, 2, 4, 2, 3))
  } else if (field == "臨床心理学") {
    doc_term_matrix[i, 1:6] <- rpois(6, lambda = c(2, 2, 2, 2, 2, 1))
    doc_term_matrix[i, 7:12] <- rpois(6, lambda = c(8, 6, 7, 6, 4, 7))
    doc_term_matrix[i, 13:18] <- rpois(6, lambda = c(3, 2, 3, 3, 2, 3))
  } else {
    doc_term_matrix[i, 1:6] <- rpois(6, lambda = c(2, 2, 1, 3, 2, 2))
    doc_term_matrix[i, 7:12] <- rpois(6, lambda = c(2, 2, 3, 2, 2, 2))
    doc_term_matrix[i, 13:18] <- rpois(6, lambda = c(7, 8, 7, 6, 7, 8))
  }
}

# 分野×単語の集計表（対応分析用）
field_word_table <- matrix(0, nrow = 3, ncol = n_terms)
rownames(field_word_table) <- fields
colnames(field_word_table) <- all_terms

for (i in 1:n_docs) {
  field <- doc_field[i]
  field_idx <- which(fields == field)
  field_word_table[field_idx, ] <- field_word_table[field_idx, ] + doc_term_matrix[i, ]
}

cat("対応分析用分割表サイズ:", nrow(field_word_table), "×", ncol(field_word_table), "\n")

# 対応分析の実行 ----------------------------------------------------------
ca_result <- ca(field_word_table)

var_explained <- round(ca_result$sv^2 / sum(ca_result$sv^2) * 100, 1)
cat("第1軸の寄与率:", var_explained[1], "%\n")
cat("第2軸の寄与率:", var_explained[2], "%\n")

ca_rows <- data.frame(
  label = rownames(ca_result$rowcoord),
  type = "領域",
  x = ca_result$rowcoord[, 1],
  y = ca_result$rowcoord[, 2]
)

ca_cols <- data.frame(
  label = rownames(ca_result$colcoord),
  type = "単語",
  x = ca_result$colcoord[, 1],
  y = ca_result$colcoord[, 2]
)

ca_data <- rbind(ca_rows, ca_cols)

# 図14.3: 対応分析（領域と単語の同時布置）
p_ca <- ggplot(ca_data, aes(x = x, y = y, shape = type)) +
  geom_point(size = 3, color = "black") +
  geom_text_repel(aes(label = label), size = 3, max.overlaps = Inf) +
  scale_shape_manual(values = c("領域" = 17, "単語" = 16)) +
  labs(
    x = paste("第1軸 (", var_explained[1], "%)"),
    y = paste("第2軸 (", var_explained[2], "%)"),
    shape = "種別"
  ) +
  theme_minimal()
print(p_ca)

# MDS分析（共起行列から）-------------------------------------------------

# 共起行列の作成
cooc_matrix <- matrix(0, n_terms, n_terms)
rownames(cooc_matrix) <- colnames(cooc_matrix) <- all_terms

for (i in 1:n_docs) {
  doc_vec <- doc_term_matrix[i, ]
  present_terms <- which(doc_vec > 0)
  for (j in present_terms) {
    for (k in present_terms) {
      if (j != k) cooc_matrix[j, k] <- cooc_matrix[j, k] + 1
    }
  }
}

dissim_matrix_text <- 1 / (1 + cooc_matrix)
diag(dissim_matrix_text) <- 0

mds_result_text <- mds(dissim_matrix_text, ndim = 2, type = "ordinal")
cat("MDS Stress値:", round(mds_result_text$stress, 3), "\n")

mds_data_text <- data.frame(
  word = rownames(mds_result_text$conf),
  x = mds_result_text$conf[, 1],
  y = mds_result_text$conf[, 2]
)

# 図14.1: 改良版MDS（単語の共起構造）
p_mds_text <- ggplot(mds_data_text, aes(x = x, y = y, label = word)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3, max.overlaps = Inf) +
  labs(x = "次元1", y = "次元2") +
  theme_minimal()
print(p_mds_text)

# ネットワーク分析 -------------------------------------------------------

# 出現頻度の計算
word_frequencies_text <- colSums(doc_term_matrix)

# 共起頻度2以上のエッジ
cooc_pairs <- which(cooc_matrix >= 2, arr.ind = TRUE)
if (nrow(cooc_pairs) > 0) {
  strong_edges_text <- data.frame(
    from = rownames(cooc_matrix)[cooc_pairs[, 1]],
    to = colnames(cooc_matrix)[cooc_pairs[, 2]],
    cooccur_count = cooc_matrix[cooc_pairs]
  )
  strong_edges_text <- strong_edges_text[strong_edges_text$from < strong_edges_text$to, ]

  nodes_text <- data.frame(
    name = names(word_frequencies_text),
    frequency = as.numeric(word_frequencies_text)
  )

  g_text <- graph_from_data_frame(strong_edges_text, directed = FALSE, vertices = nodes_text)

  # 中心性指数
  centrality_text <- data.frame(
    word = V(g_text)$name,
    frequency = V(g_text)$frequency,
    degree = degree(g_text),
    betweenness = betweenness(g_text)
  )

  cat("中心性指数（媒介中心性上位10語）:\n")
  print(centrality_text[order(-centrality_text$betweenness), ][1:10, ])

  # 図14.2: 改良版ネットワーク（単語の共起ネットワーク）
  p_network_text <- g_text %>%
    ggraph(layout = "stress") +
    geom_edge_link(aes(width = cooccur_count), color = "gray60", alpha = 0.8) +
    geom_node_point(aes(size = frequency * 1.5), color = "black", alpha = 0.7) +
    geom_node_label(aes(label = name), size = 2.5, fill = "white", alpha = 0.9,
                    label.padding = unit(0.2, "lines"), label.r = unit(0.15, "lines")) +
    scale_edge_width_continuous(range = c(0.5, 3), guide = "none") +
    scale_size_identity() +
    theme_void()
  print(p_network_text)
} else {
  cat("共起頻度が十分なペアがありません。\n")
}

# 参考: LSAとLDAについて -----------------------------------------------
cat("\n===========================\n")
cat("参考: LSA（潜在意味解析）とLDA（潜在ディリクレ配分法）\n")
cat("===========================\n")
cat("第14章では，テキストマイニングの応用として以下の手法も扱います:\n")
cat("- LSA: 文書-単語行列に特異値分解（SVD）を適用し，\n")
cat("       潜在的な意味空間に文書と単語を配置する\n")
cat("- LDA: 文書がトピックの混合で生成されるとみなすベイズモデル\n")
cat("       ディリクレ分布を事前分布として，EMアルゴリズムで推定\n")
cat("※ これらの詳細な実装には別途テキストマイニング用パッケージが必要です。\n")
cat("   textmineR, topicmodels, lsa などを参照してください。\n")

cat("\n=== 第14章 テキスト解析の可視化完了 ===\n")
