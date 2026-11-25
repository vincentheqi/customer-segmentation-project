# =============================================================
# Project 3 — Step 4: K-means on the PCA scores
# Input : data/pca_scores.rds
# Output: data/clusters.rds, output/kmeans_diagnostics.rds,
#         output/kmeans_stability.rds, output/kmeans_model.rds,
#         output/figures/kmeans_diagnostics.png, kmeans_pca.png
# =============================================================

library(tidyverse)
library(cluster)

pca_scores <- readRDS("data/pca_scores.rds")

# Cluster on the 5 retained PC scores. PCA already standardized the
# variables and removed the redundancy (monetary, diversity, frequency…
# all collapse into PC1), so engagement is not counted five times.
# The scores are NOT rescaled: PC1 keeps its larger variance because it
# carries more of the information (39% vs ~8-11% for the others).
X <- pca_scores %>% select(starts_with("PC")) %>% as.matrix()

# ---- 1. Diagnostics for k = 2 to 6 ---------------------------
set.seed(167)
X_dist <- dist(X)

fit_k <- function(k) {
  model <- kmeans(X, centers = k, nstart = 50, iter.max = 50)
  sizes <- table(model$cluster)
  tibble(
    k                   = k,
    total_withinss      = model$tot.withinss,
    variation_explained = model$betweenss / model$totss,
    avg_silhouette      = mean(silhouette(model$cluster, X_dist)[, "sil_width"]),
    smallest_cluster    = min(sizes),
    smallest_pct        = min(sizes) / nrow(X)
  )
}

kmeans_diagnostics <- map_dfr(2:6, fit_k)
kmeans_diagnostics

diag_plot <- kmeans_diagnostics %>%
  select(k, total_withinss, variation_explained, avg_silhouette) %>%
  pivot_longer(-k, names_to = "diagnostic") %>%
  ggplot(aes(k, value)) +
  geom_line() + geom_point(size = 2) +
  facet_wrap(~ diagnostic, ncol = 1, scales = "free_y") +
  labs(title = "Choosing k", x = "Number of clusters, k", y = NULL) +
  theme_minimal()
diag_plot

# ---- 2. Stability --------------------------------------------
# A useful segmentation should not change much if the data change a
# little. For each k: fit on the full data (reference), then refit on 20
# random 80% subsamples, assign EVERY household to its nearest subsample
# center, and compare with the reference using the adjusted Rand index
# (ARI): 1 = identical grouping, 0 = no better than chance.
adjusted_rand <- function(a, b) {
  tab <- table(a, b)
  comb2 <- function(x) sum(choose(x, 2))
  index    <- comb2(tab)
  expected <- comb2(rowSums(tab)) * comb2(colSums(tab)) / choose(sum(tab), 2)
  maximum  <- (comb2(rowSums(tab)) + comb2(colSums(tab))) / 2
  (index - expected) / (maximum - expected)
}

nearest_center <- function(X, centers) {
  d <- sapply(seq_len(nrow(centers)),
              function(j) colSums((t(X) - centers[j, ])^2))
  max.col(-d)
}

kmeans_stability <- map_dfr(2:6, function(k) {
  set.seed(167)
  reference <- kmeans(X, centers = k, nstart = 50, iter.max = 50)$cluster
  aris <- map_dbl(1:20, function(s) {
    set.seed(900 + s)
    idx <- sample(nrow(X), round(0.8 * nrow(X)))
    sub <- kmeans(X[idx, ], centers = k, nstart = 25, iter.max = 50)
    adjusted_rand(reference, nearest_center(X, sub$centers))
  })
  tibble(k = k, mean_ari = mean(aris), min_ari = min(aris))
})
kmeans_stability

# ---- 3. Final model ------------------------------------------
# k = 3: the only k > 2 that is stable (subsample ARI ~0.97 vs ~0.7 for
# k = 4-6), its smallest cluster is still 15% of households, and the
# silhouette (0.22) is second only to k = 2. k = 2 just splits engaged vs
# not engaged, which is too coarse to allocate a budget.
k_final <- 3

set.seed(167)
kmeans_model <- kmeans(X, centers = k_final, nstart = 50, iter.max = 50)

# Cluster numbers are arbitrary. Renumber so that 1 = lowest average PC1
# (least engaged) … k = highest, which keeps numbering the same every run.
order_by_pc1 <- order(kmeans_model$centers[, "PC1"])
clusters <- pca_scores %>%
  mutate(cluster = factor(match(kmeans_model$cluster, order_by_pc1)))

clusters %>% count(cluster) %>% mutate(pct = n / sum(n))

# Cluster centers on the PC scale
clusters %>%
  group_by(cluster) %>%
  summarise(across(starts_with("PC"), mean), retention = mean(retained))

cluster_plot <- ggplot(clusters, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.45, size = 1) +
  labs(title = paste0("K-means clusters (k = ", k_final, ") on the PCA scores"),
       subtitle = "Clustering used PC1-PC5; this view shows the first two",
       x = "PC1: overall engagement", y = "PC2: fresh/value (+) vs drug & GM (-)",
       color = "Cluster") +
  theme_minimal()
cluster_plot

# ---- 4. Save -------------------------------------------------
saveRDS(clusters,           "data/clusters.rds")
saveRDS(kmeans_model,       "output/kmeans_model.rds")
saveRDS(kmeans_diagnostics, "output/kmeans_diagnostics.rds")
saveRDS(kmeans_stability,   "output/kmeans_stability.rds")
ggsave("output/figures/kmeans_diagnostics.png", diag_plot,    width = 6, height = 7, dpi = 150)
ggsave("output/figures/kmeans_pca.png",         cluster_plot, width = 8, height = 6, dpi = 150)
