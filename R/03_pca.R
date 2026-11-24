# =============================================================
# Project 3 — Step 3: PCA on household behavior
# Input : data/customer_features.rds
# Output: data/pca_scores.rds, output/pca_model.rds,
#         output/pca_variance.rds, output/pca_loadings.rds,
#         output/figures/pca_scree.png, pca_scores.png, pca_loadings.png
# =============================================================

library(tidyverse)

customer_features <- readRDS("data/customer_features.rds")

# ---- 1. Choose and transform variables -----------------------
# Dropped from PCA:
#   tenure      — almost everyone started in January (median 267 of 273 days),
#                 so it mostly repeats recency/frequency for a few late joiners
#   store_count — mostly a geography/fuel artifact, not shopping behavior
pca_vars <- c("recency", "frequency", "monetary", "avg_basket",
              "active_weeks", "product_diversity", "category_count",
              "department_count", "discount_share", "coupon_basket_share",
              "private_label_share", "fresh_share", "drug_gm_share",
              "fuel_share", "recent_spend_share")

# Counts and dollars are right-skewed (monetary: median $856, max $17,110).
# log1p() pulls in the long tail so a few heavy shoppers don't dominate.
log_vars <- c("recency", "frequency", "monetary", "avg_basket",
              "product_diversity", "category_count")

pca_input <- customer_features %>%
  select(household_id, all_of(pca_vars)) %>%
  mutate(across(all_of(log_vars), log1p))

# ---- 2. Fit PCA (standardized) -------------------------------
pca_model <- prcomp(select(pca_input, -household_id),
                    center = TRUE, scale. = TRUE)

# PCA signs are arbitrary (flipping a component just mirrors the axis).
# Flip PC1 so that HIGHER = more spending and engagement, which is easier
# to read in every later table and plot.
if (pca_model$rotation["monetary", "PC1"] < 0) {
  pca_model$rotation[, "PC1"] <- -pca_model$rotation[, "PC1"]
  pca_model$x[, "PC1"]        <- -pca_model$x[, "PC1"]
}

# ---- 3. Explained variance -----------------------------------
pca_variance <- tibble(
  component  = paste0("PC", seq_along(pca_model$sdev)),
  eigenvalue = pca_model$sdev^2,
  proportion = eigenvalue / sum(eigenvalue),
  cumulative = cumsum(proportion)
)
pca_variance %>% head(8)

# Kaiser rule: keep components with eigenvalue > 1 (they explain more
# than one original standardized variable)
n_keep <- sum(pca_variance$eigenvalue > 1)
n_keep

scree <- pca_variance %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(component, proportion, group = 1)) +
  geom_col(fill = "grey75") +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1 / length(pca_vars), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Variance explained by each principal component",
       subtitle = "Dashed line = one original variable's share (eigenvalue = 1)",
       x = NULL, y = "Proportion of variance") +
  theme_minimal()
scree

# ---- 4. Loadings ---------------------------------------------
pca_loadings <- as_tibble(pca_model$rotation[, 1:n_keep], rownames = "variable")

# Print sorted by size for each of the first three components
for (pc in paste0("PC", 1:n_keep)) {
  cat("\n", pc, "\n")
  pca_loadings %>%
    select(variable, loading = all_of(pc)) %>%
    arrange(desc(abs(loading))) %>%
    mutate(loading = round(loading, 2)) %>%
    print(n = 8)
}

# ---- 5. Scores per household ---------------------------------
pca_scores <- as_tibble(pca_model$x[, 1:n_keep]) %>%
  mutate(household_id = pca_input$household_id, .before = 1) %>%
  left_join(select(customer_features, household_id, retained), by = "household_id")

# Do the components relate to the outcome? (outcome not used to build them)
pca_scores %>%
  pivot_longer(starts_with("PC"), names_to = "component") %>%
  group_by(component) %>%
  mutate(quartile = ntile(value, 4)) %>%
  group_by(component, quartile) %>%
  summarise(retention = mean(retained), .groups = "drop") %>%
  pivot_wider(names_from = quartile, names_prefix = "Q", values_from = retention)

score_plot <- ggplot(pca_scores, aes(PC1, PC2, color = factor(retained))) +
  geom_point(alpha = 0.35, size = 1) +
  scale_color_manual(values = c("0" = "#d7301f", "1" = "#9ecae1"),
                     labels = c("Did not return", "Returned")) +
  labs(title = "Households on the first two principal components",
       subtitle = "Non-returners sit almost entirely at low PC1 (low engagement)",
       x = "PC1: overall engagement (higher = more)",
       y = "PC2: fresh/value basket (+) vs drug & general merchandise (-)",
       color = NULL) +
  theme_minimal()
score_plot

loading_plot <- pca_loadings %>%
  select(variable, PC1:PC3) %>%
  pivot_longer(-variable, names_to = "component", values_to = "loading") %>%
  mutate(variable = fct_reorder(variable, abs(loading), .fun = max)) %>%
  ggplot(aes(loading, variable, fill = loading > 0)) +
  geom_col(show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  facet_wrap(~ component, nrow = 1) +
  scale_fill_manual(values = c("TRUE" = "#2171b5", "FALSE" = "#cb181d")) +
  labs(title = "What each component is made of",
       subtitle = "Loadings for PC1-PC3; longer bars matter more",
       x = "Loading", y = NULL) +
  theme_minimal()
loading_plot

# ---- 6. Save -------------------------------------------------
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
saveRDS(pca_scores,   "data/pca_scores.rds")
saveRDS(pca_model,    "output/pca_model.rds")
saveRDS(pca_variance, "output/pca_variance.rds")
saveRDS(pca_loadings, "output/pca_loadings.rds")
ggsave("output/figures/pca_scree.png",  scree,  width = 7, height = 4, dpi = 150)
ggsave("output/figures/pca_scores.png",   score_plot,   width = 8, height = 6, dpi = 150)
ggsave("output/figures/pca_loadings.png", loading_plot, width = 10, height = 5, dpi = 150)
