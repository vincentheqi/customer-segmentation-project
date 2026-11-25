# =============================================================
# Project 3 — Step 6: Logistic regression for return
# Input : data/segments.rds
# Output: data/predictions.rds, output/model_comparison.rds,
#         output/test_metrics.rds, output/cutoff_table.rds,
#         output/segment_performance.rds, output/vif_table.rds,
#         output/final_coefficients.rds,
#         output/figures/roc_curve.png, calibration.png
# =============================================================

library(tidyverse)
library(car)       # vif(); note car masks dplyr::recode, not used here

segments <- readRDS("data/segments.rds")

# Model data: logs for the skewed counts and dollars (as in the PCA)
model_data <- segments %>%
  mutate(
    log_recency   = log1p(recency),
    log_frequency = log1p(frequency),
    log_monetary  = log1p(monetary),
    log_basket    = log1p(avg_basket),
    log_categories = log1p(category_count)
  )

# ---- Helper functions (base R, no extra packages) ------------
auc <- function(y, p) {               # probability a random returner
  r  <- rank(p)                       # gets a higher score than a
  n1 <- sum(y == 1); n0 <- sum(y == 0)  # random non-returner
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
brier <- function(y, p) mean((p - y)^2)
log_loss <- function(y, p) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  -mean(y * log(p) + (1 - y) * log(1 - p)) }

classify <- function(y, p, cutoff) {
  pred <- as.integer(p >= cutoff)     # predict "returns" if p >= cutoff
  tibble(
    cutoff      = cutoff,
    TP = sum(pred == 1 & y == 1), FN = sum(pred == 0 & y == 1),
    FP = sum(pred == 1 & y == 0), TN = sum(pred == 0 & y == 0),
    accuracy    = mean(pred == y),
    sensitivity = TP / (TP + FN),     # returners correctly predicted
    specificity = TN / (TN + FP),     # non-returners correctly caught
    precision   = TP / (TP + FP),     # predicted returners who returned
    npv         = TN / (TN + FN),     # flagged at-risk who did not return
    flagged_at_risk = mean(pred == 0)
  )
}

# ---- 1. Candidate models -------------------------------------
formulas <- list(
  # R: classic RFM only
  rfm = retained ~ log_recency + log_frequency + log_monetary,
  # A: RFM + trend + promotion
  rfm_trend = retained ~ log_recency + log_frequency + log_monetary +
    recent_spend_share + discount_share,
  # B: all raw behavior features (many are highly correlated)
  all_raw = retained ~ log_recency + log_frequency + log_monetary +
    log_basket + active_weeks + log_categories + department_count +
    discount_share + coupon_basket_share + private_label_share +
    fresh_share + drug_gm_share + fuel_share + recent_spend_share,
  # C: the five PCA scores instead of raw features
  pca = retained ~ PC1 + PC2 + PC3 + PC4 + PC5,
  # D: model A plus the business segment
  rfm_trend_segment = retained ~ log_recency + log_frequency + log_monetary +
    recent_spend_share + discount_share + segment,
  # E: segment only (how much do the segments alone explain?)
  segment_only = retained ~ segment
)

# ---- 2. Train/test split (stratified, 70/30) -----------------
# Only 141 households did not return, so stratify to keep ~6% in both.
set.seed(167)
test_ids <- model_data %>%
  group_by(retained) %>%
  slice_sample(prop = 0.3) %>%
  pull(household_id)

train <- model_data %>% filter(!household_id %in% test_ids)
test  <- model_data %>% filter( household_id %in% test_ids)
c(train = nrow(train), test = nrow(test),
  test_non_returners = sum(test$retained == 0))

# ---- 3. Compare models ---------------------------------------
# The test set has only ~42 non-returners, so one split is noisy.
# Also use repeated 10-fold cross-validation on the TRAINING data.
cv_auc <- function(f, data, folds = 10, repeats = 5) {
  map_dbl(1:repeats, function(r) {
    set.seed(r)
    fold <- sample(rep(1:folds, length.out = nrow(data)))
    p <- numeric(nrow(data))
    for (k in 1:folds) {
      fit <- glm(f, data = data[fold != k, ], family = binomial)
      p[fold == k] <- predict(fit, data[fold == k, ], type = "response")
    }
    auc(data$retained, p)
  }) %>% mean()
}

model_comparison <- imap_dfr(formulas, function(f, name) {
  fit <- glm(f, data = train, family = binomial)
  p   <- predict(fit, test, type = "response")
  tibble(model = name,
         n_parameters = length(coef(fit)),
         AIC = AIC(fit),
         cv_auc_train = cv_auc(f, train),
         test_auc = auc(test$retained, p),
         test_brier = brier(test$retained, p),
         test_log_loss = log_loss(test$retained, p))
})
model_comparison %>% arrange(desc(cv_auc_train)) %>% print(width = Inf)

# ---- 4. Multicollinearity ------------------------------------
# GVIF^(1/(2*df)) is comparable to sqrt(VIF); > ~2.2 (VIF > 5) is a concern.
vif_table <- bind_rows(
  map(c("all_raw", "pca", "rfm", "rfm_trend", "rfm_trend_segment"), function(m) {
    v <- vif(glm(formulas[[m]], data = train, family = binomial))
    if (is.matrix(v)) v <- v[, "GVIF^(1/(2*Df))"]^2   # back to VIF scale
    tibble(model = m, term = names(v), vif = unname(v))
  })
)
vif_table %>% group_by(model) %>% summarise(max_vif = max(vif), terms_over_5 = sum(vif > 5))
vif_table %>% filter(model == "all_raw") %>% arrange(desc(vif))
vif_table %>% filter(model == "rfm_trend_segment") %>% arrange(desc(vif))

# ---- 5. Final model ------------------------------------------
# Chosen: rfm (recency, frequency, monetary). Lowest AIC, best CV AUC,
# same test AUC as the 14-variable model, and acceptable VIFs (< 5).
# Adding trend, discounts, PCs, or segments does not improve prediction.
final_formula <- formulas$rfm
final_fit <- glm(final_formula, data = train, family = binomial)
summary(final_fit)

final_coefficients <- tibble(
  term       = names(coef(final_fit)),
  estimate   = coef(final_fit),
  odds_ratio = exp(coef(final_fit)),
  std_error  = sqrt(diag(vcov(final_fit))),
  p_value    = summary(final_fit)$coefficients[, 4]
)
final_coefficients

test <- test %>% mutate(p_return = predict(final_fit, test, type = "response"))

# ---- 6. Test-set evaluation ----------------------------------
test_metrics <- tibble(auc = auc(test$retained, test$p_return),
                       brier = brier(test$retained, test$p_return),
                       base_rate_brier = brier(test$retained, mean(train$retained)))
test_metrics

# Cutoff matters: at 0.5 almost everyone is predicted to return.
cutoff_table <- map_dfr(c(0.5, 0.8, 0.9, 0.95), ~ classify(test$retained, test$p_return, .x))
cutoff_table %>% print(width = Inf)

# Confusion matrix at the chosen cutoff (0.9: flag p < 0.9 as at-risk)
chosen_cutoff <- 0.9
table(predicted = ifelse(test$p_return >= chosen_cutoff, "returns", "at risk"),
      actual    = ifelse(test$retained == 1, "returned", "did not return"))

# ROC curve
roc_points <- map_dfr(sort(unique(c(0, test$p_return, 1))), function(c) {
  pred <- test$p_return >= c
  tibble(cutoff = c,
         tpr = mean(pred[test$retained == 1]),
         fpr = mean(pred[test$retained == 0]))
})
roc_plot <- ggplot(roc_points, aes(fpr, tpr)) +
  geom_path(linewidth = 1, color = "#2171b5") +
  geom_abline(linetype = "dashed") +
  coord_equal() +
  labs(title = "ROC curve on the held-out test set",
       subtitle = paste0("AUC = ", round(test_metrics$auc, 3)),
       x = "False positive rate (non-returners predicted to return)",
       y = "True positive rate (returners predicted to return)") +
  theme_minimal()
roc_plot

# ---- 7. Out-of-fold predictions for ALL households -----------
# Used for calibration, per-segment checks, and the budget (Step 7):
# every household gets a prediction from a model that never saw it.
set.seed(167)
fold <- sample(rep(1:10, length.out = nrow(model_data)))
model_data$p_return <- NA_real_
for (k in 1:10) {
  fit <- glm(final_formula, data = model_data[fold != k, ], family = binomial)
  model_data$p_return[fold == k] <- predict(fit, model_data[fold == k, ], type = "response")
}

# Calibration: within each band of predicted probability, does the
# observed return rate match the prediction?
calibration <- model_data %>%
  mutate(band = cut(p_return, c(0, 0.5, 0.8, 0.9, 0.95, 0.98, 0.99, 1),
                    include.lowest = TRUE)) %>%
  group_by(band) %>%
  summarise(households = n(), predicted = mean(p_return),
            observed = mean(retained), .groups = "drop")
calibration

calibration_plot <- ggplot(calibration, aes(predicted, observed)) +
  geom_abline(linetype = "dashed") +
  geom_line(color = "#2171b5") +
  geom_point(aes(size = households), color = "#2171b5") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = "Calibration (out-of-fold predictions)",
       subtitle = "Points on the dashed line = predicted probabilities are honest",
       x = "Mean predicted return probability", y = "Observed return rate") +
  theme_minimal()
calibration_plot

# Does the model work equally well within each segment?
segment_performance <- model_data %>%
  group_by(segment) %>%
  summarise(
    households     = n(),
    non_returners  = sum(retained == 0),
    observed       = mean(retained),
    predicted      = mean(p_return),
    auc            = if (non_returners >= 5) auc(retained, p_return) else NA_real_,
    flagged_at_risk = mean(p_return < chosen_cutoff),
    caught_non_returners = sum(p_return < chosen_cutoff & retained == 0) / non_returners,
    .groups = "drop"
  )
segment_performance

# ---- 8. Save -------------------------------------------------
predictions <- model_data %>% select(household_id, segment, retained, p_return)
saveRDS(predictions,         "data/predictions.rds")
saveRDS(model_comparison,    "output/model_comparison.rds")
saveRDS(vif_table,           "output/vif_table.rds")
saveRDS(final_coefficients,  "output/final_coefficients.rds")
saveRDS(test_metrics,        "output/test_metrics.rds")
saveRDS(cutoff_table,        "output/cutoff_table.rds")
saveRDS(calibration,         "output/calibration.rds")
saveRDS(segment_performance, "output/segment_performance.rds")
ggsave("output/figures/roc_curve.png",   roc_plot,         width = 6, height = 6, dpi = 150)
ggsave("output/figures/calibration.png", calibration_plot, width = 6, height = 6, dpi = 150)
