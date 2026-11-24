# =============================================================
# Project 3 — Step 2: Build one row per household
# Input : data/feature_tx.rds, data/households.rds,
#         data/products.rds, data/demographics.rds
# Output: data/customer_features.rds, output/feature_summary.rds,
#         output/feature_correlations.rds
# =============================================================

library(tidyverse)

feature_tx   <- readRDS("data/feature_tx.rds")
households   <- readRDS("data/households.rds")
products     <- readRDS("data/products.rds")
demographics <- readRDS("data/demographics.rds")

cutoff <- as.Date("2017-10-01")        # first day of the holdout period
q3_start <- as.Date("2017-07-01")      # last three months of the feature period

# ---- 1. Attach product information --------------------------
tx <- feature_tx %>%
  left_join(products, by = "product_id") %>%
  mutate(total_disc = retail_disc + coupon_disc + coupon_match_disc)

# Fuel is 7.5% of spend but bought by only ~40% of households, and its
# quantity is in fuel units. It would inflate "value" for people who
# simply fill up at the store, so shopping measures exclude it and
# fuel gets its own share variable.
fuel_by_hh <- tx %>%
  group_by(household_id) %>%
  summarise(
    fuel_share = sum(sales_value[department %in% "FUEL"]) / sum(sales_value),
    .groups = "drop"
  )

shop <- tx %>% filter(!department %in% "FUEL")

# Fresh departments = perishable, trip-driving categories
fresh_depts <- c("PRODUCE", "MEAT", "MEAT-PCKGD", "DELI", "SEAFOOD",
                 "SEAFOOD-PCKGD", "SALAD BAR", "PASTRY")

# ---- 2. Basket-level summary (for frequency and basket size) --
baskets <- shop %>%
  group_by(household_id, basket_id) %>%
  summarise(
    date       = min(date),
    spend      = sum(sales_value),
    has_coupon = any(coupon_disc > 0),
    .groups = "drop"
  )

rfm <- baskets %>%
  group_by(household_id) %>%
  summarise(
    recency        = as.numeric(cutoff - max(date)),   # days since last visit
    tenure         = as.numeric(cutoff - min(date)),   # days since first visit
    frequency      = n(),                              # number of baskets
    monetary       = sum(spend),                       # total spend ($)
    avg_basket     = monetary / frequency,             # $ per basket
    coupon_basket_share = mean(has_coupon),            # share of trips using a coupon
    recent_spend_share  = sum(spend[date >= q3_start]) / monetary,  # ~0.33 if steady
    .groups = "drop"
  )

# ---- 3. Product breadth, discounts, and department mix -------
breadth <- shop %>%
  group_by(household_id) %>%
  summarise(
    active_weeks      = n_distinct(week),
    store_count       = n_distinct(store_id),
    product_diversity = n_distinct(product_id),
    category_count    = n_distinct(product_category, na.rm = TRUE),
    department_count  = n_distinct(department, na.rm = TRUE),
    discount_share    = sum(total_disc) / (sum(sales_value) + sum(total_disc)),
    private_label_share = sum(sales_value[brand %in% "Private"]) / sum(sales_value),
    fresh_share       = sum(sales_value[department %in% fresh_depts]) / sum(sales_value),
    drug_gm_share     = sum(sales_value[department %in% "DRUG GM"]) / sum(sales_value),
    .groups = "drop"
  )

# ---- 4. Demographics (801 households only) -------------------
# Used to DESCRIBE segments later, not to build them.
demo <- demographics %>%
  mutate(
    family_size = as.numeric(str_remove(as.character(household_size), "\\+")),
    kids        = as.numeric(str_remove(as.character(kids_count), "\\+"))
  ) %>%
  select(household_id, age, income, family_size, kids, household_comp)

# ---- 5. Combine into one row per household -------------------
customer_features <- households %>%
  left_join(rfm,        by = "household_id") %>%
  left_join(breadth,    by = "household_id") %>%
  left_join(fuel_by_hh, by = "household_id") %>%
  left_join(demo,       by = "household_id") %>%
  mutate(has_demographics = household_id %in% demographics$household_id)

# Households whose only feature-period purchases were fuel have no
# shopping baskets; check how many and drop them.
fuel_only <- customer_features %>% filter(is.na(frequency))
nrow(fuel_only)
customer_features <- customer_features %>% filter(!is.na(frequency))

nrow(customer_features)          # households in the analysis
anyNA(select(customer_features, recency:fuel_share))   # should be FALSE

# ---- 6. Summaries for the report -----------------------------
behavior_vars <- c("recency", "tenure", "frequency", "monetary", "avg_basket",
                   "active_weeks", "store_count", "product_diversity",
                   "category_count", "department_count", "discount_share",
                   "coupon_basket_share", "private_label_share", "fresh_share",
                   "drug_gm_share", "fuel_share", "recent_spend_share")

feature_summary <- customer_features %>%
  select(all_of(behavior_vars)) %>%
  pivot_longer(everything(), names_to = "feature") %>%
  group_by(feature) %>%
  summarise(
    min = min(value), q1 = quantile(value, 0.25), median = median(value),
    mean = mean(value), q3 = quantile(value, 0.75), max = max(value)
  ) %>%
  mutate(feature = factor(feature, levels = behavior_vars)) %>%
  arrange(feature)

feature_summary

# Correlations motivate PCA: many of these measure the same "engagement"
feature_correlations <- cor(customer_features[behavior_vars])
round(feature_correlations[c("frequency", "monetary", "active_weeks",
                             "product_diversity", "category_count"),
                           c("frequency", "monetary", "active_weeks",
                             "product_diversity", "category_count")], 2)

# Quick first look: retention by feature quartile
customer_features %>%
  select(retained, recency, frequency, monetary, recent_spend_share) %>%
  pivot_longer(-retained, names_to = "feature") %>%
  group_by(feature) %>%
  mutate(quartile = ntile(value, 4)) %>%
  group_by(feature, quartile) %>%
  summarise(retention = mean(retained), .groups = "drop") %>%
  pivot_wider(names_from = quartile, names_prefix = "Q", values_from = retention)

# ---- 7. Save -------------------------------------------------
dir.create("output", showWarnings = FALSE)
saveRDS(customer_features,    "data/customer_features.rds")
saveRDS(feature_summary,      "output/feature_summary.rds")
saveRDS(feature_correlations, "output/feature_correlations.rds")
