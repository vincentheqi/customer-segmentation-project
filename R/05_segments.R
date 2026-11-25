# =============================================================
# Project 3 — Step 5: From clusters to business segments
# Input : data/customer_features.rds, data/clusters.rds
# Output: data/segments.rds, output/segment_profile.rds,
#         output/segment_table.rds, output/segment_demographics.rds,
#         output/figures/segments_pca.png, segments_retention.png
# =============================================================

library(tidyverse)

customer_features <- readRDS("data/customer_features.rds")
clusters          <- readRDS("data/clusters.rds")

segments <- customer_features %>%
  left_join(select(clusters, household_id, cluster, PC1:PC5), by = "household_id")

# ---- 1. Business rule on top of the clusters -----------------
# A steady shopper spends ~1/3 of their 9-month total in Jul-Sep
# (recent_spend_share ~ 0.33). Below 0.2 means spending fell by roughly
# 40% or more in the last quarter.
#
# The rule is applied to Cluster 2 only. In Cluster 2 it separates a
# clearly riskier group (90.6% vs 96.9% return). In Cluster 3 "fading"
# households still return 98.3%, so the split adds no decision value.
fading_cutoff <- 0.2

segments <- segments %>%
  mutate(
    fading  = recent_spend_share < fading_cutoff,
    segment = case_when(
      cluster == "3"            ~ "Loyal high-value core",
      cluster == "2" & !fading  ~ "Steady mid-value regulars",
      cluster == "2" &  fading  ~ "Fading mid-value regulars",
      cluster == "1"            ~ "Occasional light shoppers"
    ),
    segment = factor(segment, levels = c("Loyal high-value core",
                                         "Steady mid-value regulars",
                                         "Fading mid-value regulars",
                                         "Occasional light shoppers"))
  )

# How the clusters map to segments
segments %>% count(cluster, segment)

# ---- 2. Segment profile (original units) ---------------------
segment_profile <- segments %>%
  group_by(segment) %>%
  summarise(
    cluster            = paste(unique(cluster), collapse = ", "),
    households         = n(),
    pct_households     = n() / nrow(segments),
    median_recency     = median(recency),
    median_frequency   = median(frequency),
    median_monetary    = median(monetary),
    median_avg_basket  = median(avg_basket),
    median_categories  = median(category_count),
    median_recent_share = median(recent_spend_share),
    mean_discount_share = mean(discount_share),
    mean_coupon_share  = mean(coupon_basket_share),
    mean_PC1           = mean(PC1),
    mean_PC2           = mean(PC2),
    mean_PC5           = mean(PC5),
    return_rate        = mean(retained),
    non_returners      = sum(retained == 0),
    total_spend        = sum(monetary),
    .groups = "drop"
  ) %>%
  mutate(pct_of_spend = total_spend / sum(total_spend))

segment_profile %>% print(width = Inf)

# ---- 3. Demographics (only 801 households have them) ---------
# Describes segments; NOT used to build them. Coverage itself is a
# finding: households with demographics are almost all engaged shoppers.
segment_demographics <- segments %>%
  group_by(segment) %>%
  summarise(
    households        = n(),
    with_demographics = sum(has_demographics),
    coverage          = mean(has_demographics),
    mean_family_size  = mean(family_size, na.rm = TRUE),
    pct_with_kids     = mean(kids[has_demographics] > 0),
    pct_income_under_50k = mean(as.integer(income[has_demographics]) <= 4),
    .groups = "drop"
  )
segment_demographics

# ---- 4. Segment table for the report -------------------------
segment_text <- tribble(
  ~segment, ~evidence, ~likely_needs, ~marketing_action,
  "Loyal high-value core",
  "Highest PC1; median 58 trips, $1,836 spend, last visit 3 days before cutoff; buys ~115 categories; 83% of all spending; 99.5% return",
  "Already shop here routinely; value convenience and recognition more than discounts",
  "Protect: loyalty recognition, personalized offers on categories they already buy, early access; avoid blanket discounts that give away margin",
  "Steady mid-value regulars",
  "Middle PC1; median 19 trips, ~$490 spend, steady spending through Sep; slightly higher fresh/discount mix (PC2 +); 96.9% return",
  "Shop here for part of their needs; room to buy more categories and visit more often",
  "Grow: cross-sell into categories they don't buy yet, frequency incentives (e.g. spend-and-get), tested with control groups",
  "Fading mid-value regulars",
  "Same profile as steady regulars (~$475 spend) but less than 20% of their spending in Jul-Sep vs ~33% if steady; 90.6% return",
  "Were regular shoppers but are drifting away, possibly to a competitor",
  "Win back: targeted reactivation offer triggered by declining spend; measure lift against a hold-out group",
  "Occasional light shoppers",
  "Lowest PC1; median 5 trips, $82 spend, last visit 49 days before cutoff; 15% of households but 1% of spending; 75.6% return; no demographic data",
  "Visit occasionally (fill-in trips or fuel); little evidence they see this store as their main grocer",
  "Low-touch: low-cost automated email/app reminders and seasonal promotions; no expensive outreach"
)

segment_table <- segment_profile %>%
  select(segment, cluster, households, pct_households, median_recency,
         median_frequency, median_monetary, return_rate, pct_of_spend) %>%
  left_join(segment_text, by = "segment")

segment_table %>% select(segment, households, return_rate, marketing_action)

# ---- 5. Figures ----------------------------------------------
segment_colors <- c("Loyal high-value core"     = "#2171b5",
                    "Steady mid-value regulars" = "#41ab5d",
                    "Fading mid-value regulars" = "#fd8d3c",
                    "Occasional light shoppers" = "#969696")

segments_pca <- ggplot(segments, aes(PC1, recent_spend_share, color = segment)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_hline(yintercept = fading_cutoff, linetype = "dashed") +
  scale_color_manual(values = segment_colors) +
  labs(title = "Business segments: k-means clusters plus a 'fading' rule",
       subtitle = "Dashed line = fading cutoff (applied to the middle cluster only)",
       x = "PC1: overall engagement", y = "Share of 9-month spend in Jul-Sep",
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
segments_pca

segments_retention <- segment_profile %>%
  ggplot(aes(return_rate, fct_rev(segment), fill = segment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(return_rate, accuracy = 0.1)),
            hjust = 1.1, color = "white") +
  geom_vline(xintercept = mean(segments$retained), linetype = "dashed") +
  scale_fill_manual(values = segment_colors) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Holdout return rate by segment",
       subtitle = "Dashed line = overall 94.2%; return was not used to build segments",
       x = "Returned in Oct-Dec", y = NULL) +
  theme_minimal()
segments_retention

# ---- 6. Save -------------------------------------------------
saveRDS(segments,             "data/segments.rds")
saveRDS(segment_profile,      "output/segment_profile.rds")
saveRDS(segment_table,        "output/segment_table.rds")
saveRDS(segment_demographics, "output/segment_demographics.rds")
ggsave("output/figures/segments_pca.png",       segments_pca,       width = 8, height = 6, dpi = 150)
ggsave("output/figures/segments_retention.png", segments_retention, width = 8, height = 4, dpi = 150)
