library(tidyverse)
source("./markdown/analysis/helper_functions.R")

load("./markdown/analysis/data/full_analysis_data_long.rdata")
load("./markdown/analysis/data/double_full_cors.rdata")

mean_homogeneity_overall <- double_full_cors %>%
  filter(
    method1_bin == method2_bin,
    method1_rater == "sven", method2_rater == "sven") %>%
  filter(method1_bin %in% c(5, 6)) %>%
  mutate(correlation = V1) %>%
  group_by(method1_approach, method1_type, method1_review) %>%
  summarize(
    mean = fisher_cor_mean(correlation),
    n = n(),
    quant_10 = quantile(correlation, 0.1),
    quant_90 = quantile(correlation, 0.9)
  )
