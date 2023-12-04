# library(tidyverse)
# source("./markdown/analysis/helper_functions.R")
#
# load("./markdown/analysis/data/full_analysis_data_long.rdata")
# load("./markdown/analysis/data/double_full_cors.rdata")

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

mean_homogeneity_full_info <- double_full_cors %>%
  filter(
    method1_bin == method2_bin,
    method1_rater == "sven", method2_rater == "sven") %>%
  filter(method1_bin %in% c(5, 6)) %>%
  mutate(correlation = V1) %>%
  group_by(task, method1_filter, method1_window, method1_approach, method1_type, method1_review) %>%
  summarize(
    mean = fisher_cor_mean(correlation),
    n = n(),
    quant_10 = quantile(correlation, 0.1),
    quant_90 = quantile(correlation, 0.9)
  ) %>%
  rename(
    "filter" = method1_filter,
    "window" = method1_window,
    "approach" = method1_approach,
    "type" = method1_type,
    "review" = method1_review
  )

footer_text_homogeneity <- c(
  "Values represent the average correlation of a particular extraction method with other extraction methods; corr = CORR-based algorithm; minsq = MINSQ-based algorithm; autoarea = Area latency algorithm; autopeak = Peak latency algorithm; results of the algorithms either not reviewed (none), automatically reviewed based on the fit statistic (auto), or reviewed manually (manual); filter = low-pass filter used in preprocessing (in Hz); window = measurement window used for latency extraction (narrow = 250 - 600 ms; medium = 200 - 700 ms; wide = 150 - 900ms)"
)

table_mean_homogeneity_flanker <- mean_homogeneity_full_info %>%
  ungroup() %>%
  filter(
    approach %in% c("corr", "minsq", "uninformed")
  ) %>%
  mutate(
    approach = ifelse(approach == "uninformed", paste0(type), approach),
    window = ifelse(window == "const", "medium", window)
  ) %>%
  select(-type, -n, -contains("quant")) %>%
  mutate(
    approach = fct_relevel(approach, "corr", "minsq", "autoarea", "autopeak"),
    window = fct_relevel(window, "narrow", "medium", "wide"),
    review = fct_relevel(review, "none", "auto", "manual"),
    filter = as.numeric(filter)
  ) %>%
  arrange(task, filter, approach, window, review) %>%
  pivot_wider(
    id_cols = c("task", "filter", "window"),
    names_from = c("approach", "review"),
    values_from = "mean"
  ) %>%
  arrange(task, filter, window) %>%
  filter(task == "flanker") %>%
  select(-task) %>%
  flextable() %>%
  colformat_double(j = -1, digits = 2) %>%
  separate_header() %>%
  align(align = "center", part = "all", j = -c(1:2)) %>%
  merge_v(j = 1) %>%
  # valign(j = 1, valign = "top") %>%
  hline(i = c(3, 6)) %>%
  apa_footer(footer_text_homogeneity) %>%
  line_spacing(space = 0.5, part = "all") %>%
  # set_caption("Reliability - Nback Task") %>%
  set_table_properties(layout = "autofit", width = 0.75)

table_mean_homogeneity_nback <- mean_homogeneity_full_info %>%
  ungroup() %>%
  filter(
    approach %in% c("corr", "minsq", "uninformed")
  ) %>%
  mutate(
    approach = ifelse(approach == "uninformed", paste0(type), approach),
    window = ifelse(window == "const", "medium", window)
  ) %>%
  select(-type, -n, -contains("quant")) %>%
  mutate(
    approach = fct_relevel(approach, "corr", "minsq", "autoarea", "autopeak"),
    window = fct_relevel(window, "narrow", "medium", "wide"),
    review = fct_relevel(review, "none", "auto", "manual"),
    filter = as.numeric(filter)
  ) %>%
  arrange(task, filter, approach, window, review) %>%
  pivot_wider(
    id_cols = c("task", "filter", "window"),
    names_from = c("approach", "review"),
    values_from = "mean"
  ) %>%
  arrange(task, filter, window) %>%
  filter(task == "nback") %>%
  select(-task) %>%
  flextable() %>%
  colformat_double(j = -1, digits = 2) %>%
  separate_header() %>%
  align(align = "center", part = "all", j = -c(1:2)) %>%
  merge_v(j = 1) %>%
  # valign(j = 1, valign = "top") %>%
  hline(i = c(3, 6)) %>%
  apa_footer(footer_text_homogeneity) %>%
  line_spacing(space = 0.5, part = "all") %>%
  # set_caption("Reliability - Nback Task") %>%
  set_table_properties(layout = "autofit", width = 0.75)


table_mean_homogeneity_switching <- mean_homogeneity_full_info %>%
  ungroup() %>%
  filter(
    approach %in% c("corr", "minsq", "uninformed")
  ) %>%
  mutate(
    approach = ifelse(approach == "uninformed", paste0(type), approach),
    window = ifelse(window == "const", "medium", window)
  ) %>%
  select(-type, -n, -contains("quant")) %>%
  mutate(
    approach = fct_relevel(approach, "corr", "minsq", "autoarea", "autopeak"),
    window = fct_relevel(window, "narrow", "medium", "wide"),
    review = fct_relevel(review, "none", "auto", "manual"),
    filter = as.numeric(filter)
  ) %>%
  arrange(task, filter, approach, window, review) %>%
  pivot_wider(
    id_cols = c("task", "filter", "window"),
    names_from = c("approach", "review"),
    values_from = "mean"
  ) %>%
  arrange(task, filter, window) %>%
  filter(task == "switching") %>%
  select(-task) %>%
  flextable() %>%
  colformat_double(j = -1, digits = 2) %>%
  separate_header() %>%
  align(align = "center", part = "all", j = -c(1:2)) %>%
  merge_v(j = 1) %>%
  # valign(j = 1, valign = "top") %>%
  hline(i = c(3, 6)) %>%
  apa_footer(footer_text_homogeneity) %>%
  line_spacing(space = 0.5, part = "all") %>%
  # set_caption("Reliability - Nback Task") %>%
  set_table_properties(layout = "autofit", width = 0.75)
