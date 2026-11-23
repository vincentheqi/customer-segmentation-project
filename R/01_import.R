# =============================================================
# Project 3 — Step 1: Import data and define the outcome
# Output: data/feature_tx.rds, data/households.rds (+ products, demographics)
# =============================================================

library(tidyverse)
library(completejourney)

# ---- 1. Load data -------------------------------------------
# get_transactions() downloads the FULL transaction file (~1.47M rows)
# from GitHub. transactions_sample is a random 5% of *rows*, so every
# basket is only partly observed — too thin for RFM features.
transactions <- get_transactions()
data(products)
data(demographics)

dir.create("data", showWarnings = FALSE)

# ---- 2. Calendar date in the store's time zone ---------------
# Timestamps are stored in America/New_York. Plain as.Date() converts
# to UTC, which moves evening purchases to the next day (383 rows would
# jump from Sept 30 into October).
transactions <- transactions %>%
  mutate(date = as.Date(transaction_timestamp, tz = "America/New_York"))

range(transactions$date)

# ---- 3. Split into feature and holdout periods ---------------
cutoff <- as.Date("2017-10-01")   # observation: Jan 1–Sep 30; holdout: Oct 1–Dec 31

feature_tx <- transactions %>% filter(date <  cutoff)
holdout_tx <- transactions %>% filter(date >= cutoff)

# ---- 4. Outcome: did the household return in the holdout? ----
# Only households active in the feature period are eligible; households
# seen only in Oct–Dec are new customers we cannot score.
households <- feature_tx %>%
  distinct(household_id) %>%
  mutate(retained = as.integer(household_id %in% holdout_tx$household_id))

# ---- 5. Sanity checks ----------------------------------------
tibble(
  feature_rows        = nrow(feature_tx),
  holdout_rows        = nrow(holdout_tx),
  feature_households  = nrow(households),
  holdout_only_new    = n_distinct(setdiff(holdout_tx$household_id, households$household_id)),
  retention_rate      = mean(households$retained),
  with_demographics   = sum(households$household_id %in% demographics$household_id)
)

households %>% count(retained) %>% mutate(pct = n / sum(n))

# ---- 6. Save -------------------------------------------------
saveRDS(feature_tx,   "data/feature_tx.rds")
saveRDS(households,   "data/households.rds")
saveRDS(products,     "data/products.rds")
saveRDS(demographics, "data/demographics.rds")
