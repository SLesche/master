# library(tidyverse)
# source("./markdown/analysis/helper_functions.R")
#
# load("./markdown/analysis/data/full_analysis_data_long.rdata")
# load("./markdown/analysis/data/double_full_cors.rdata")

mean_manualcor_overall <- double_full_cors %>%
  filter(
    method1_approach != "jackknifemanual",
    method1_type %in% c("area", "peak"),
    method1_rater == "kathrin" & method2_rater == "sven",
  ) %>%
  filter(method2_smooth == 0) %>%
  filter(method1_filter == method2_filter, method1_bin == method2_bin) %>%
  mutate(correlation = V1) %>%
  group_by(method2_approach, method2_review, method2_type) %>%
  summarize(
    mean = fisher_cor_mean(correlation),
    n = n(),
    quant_10 = quantile(correlation, 0.1),
    quant_90 = quantile(correlation, 0.9)
  )

data_nested_cormanual <- double_full_cors %>%
  filter(
    method1_approach != "jackknifemanual",
    method1_type %in% c("area", "peak"),
    method1_rater == "kathrin" & method2_rater == "sven",
  ) %>%
  filter(method2_smooth == 0) %>%
  filter(method1_filter == method2_filter, method1_bin == method2_bin) %>%
  mutate(combination = factor(interaction(method2_approach, method2_type, method2_review)),
         correlation = V1,
         id = factor(paste0(task, method1_filter))) %>%
  nest(.by = c("method2_approach", "method2_type", "method2_review")) %>%
  mutate(
    aov = map(
      data,
      ~afex::aov_ez(
        id = "id",
        dv = "correlation",
        between = c("method2_filter"),
        within = c("method2_window"),
        data = .
      )
    )
  )

data_nested_cormanual <- data_nested_cormanual %>%
  mutate(
    omega = map(
      aov,
      ~effectsize::effectsize(.[[1]], ci = 0.95, type = "omega")
    ),
    emmeans_window = map(
      aov,
      ~emmeans::emmeans(., ~ method2_window)
    )
  )

data_nested_cormanual$omega_filter = NA
data_nested_cormanual$omega_window = NA

for (i in 1:nrow(data_nested_cormanual)){
  data_nested_cormanual$omega_filter[i] = data_nested_cormanual$omega[i][[1]][1, 2]
  data_nested_cormanual$omega_window[i] = data_nested_cormanual$omega[i][[1]][2, 2]
}

mean_manualcor_full_info <- double_full_cors %>%
  filter(
    method1_approach != "jackknifemanual",
    method1_type %in% c("area", "peak"),
    method1_rater == "kathrin" & method2_rater == "sven",
  ) %>%
  filter(method2_smooth == 0) %>%
  filter(method1_filter == method2_filter, method1_bin == method2_bin) %>%
  mutate(correlation = V1) %>%
  filter(method2_approach %in% c("corr", "minsq", "uninformed")) %>%
  group_by(task, method1_type, method2_filter, method2_window, method2_approach, method2_review, method2_type) %>%
  summarize(
    mean = fisher_cor_mean(correlation),
    n = n(),
    quant_10 = quantile(correlation, 0.1),
    quant_90 = quantile(correlation, 0.9)
  )

footer_text_cormanual <- c("Values represent the mean correlation of latency values extracted by a certain algorithm with latencies extracted by an expert ERP researcher; corr = CORR-based algorithm; minsq = MINSQ-based algorithm; autoarea = Area latency algorithm; autopeak = Peak latency algorithm; results of the algorithms either not reviewed (none), automatically reviewed based on the fit statistic (auto), or reviewed manually (manual); manual = expert researcher either used peak or area as their guideline; filter = low-pass filter used in preprocessing (in Hz); window = measurement window used for latency extraction (narrow = 250 - 600 ms; medium = 200 - 700 ms; wide = 150 - 900ms)")

table_mean_manualcor_flanker <- mean_manualcor_full_info %>%
  ungroup() %>%
  mutate(
    method2_approach = ifelse(method2_approach == "uninformed", paste0(method2_type), method2_approach),
    method2_window = ifelse(method2_window == "const", "medium", method2_window)
  ) %>%
  select(-method2_type, -n, -contains("quant")) %>%
  mutate(
    method2_approach = fct_relevel(method2_approach, "corr", "minsq", "autoarea", "autopeak"),
    method2_window = fct_relevel(method2_window, "narrow", "medium", "wide"),
    method2_review = fct_relevel(method2_review, "none", "auto", "manual"),
    method2_filter = as.numeric(method2_filter)
  ) %>%
  rename(
    "filter" = method2_filter,
    "manual_type" = method1_type,
    "window" = method2_window,
    "approach" = method2_approach,
    "review" = method2_review
  ) %>%
  arrange(task, manual_type, filter, approach, window, review) %>%
  pivot_wider(
    id_cols = c("task", "manual_type", "filter", "window"),
    names_from = c("approach", "review"),
    values_from = "mean"
  ) %>%
  arrange(task, manual_type, filter, window) %>%
  filter(task == "flanker") %>%
  select(-task) %>%
  rename(
    "manual" = "manual_type"
  ) %>%
  flextable() %>%
  colformat_double(j = -c(1:3), digits = 2) %>%
  separate_header() %>%
  align(align = "center", part = "all", j = -c(1:2)) %>%
  merge_v(j = 1) %>%
  # valign(j = 1, valign = "top") %>%
  hline(i = c(9)) %>%
  apa_footer(footer_text_cormanual) %>%
  line_spacing(space = 0.5, part = "all") %>%
  # set_caption("Reliability - Nback Task") %>%
  set_table_properties(layout = "autofit", width = 0.75)

table_mean_manualcor_switching <- mean_manualcor_full_info %>%
  ungroup() %>%
  mutate(
    method2_approach = ifelse(method2_approach == "uninformed", paste0(method2_type), method2_approach),
    method2_window = ifelse(method2_window == "const", "medium", method2_window)
  ) %>%
  select(-method2_type, -n, -contains("quant")) %>%
  mutate(
    method2_approach = fct_relevel(method2_approach, "corr", "minsq", "autoarea", "autopeak"),
    method2_window = fct_relevel(method2_window, "narrow", "medium", "wide"),
    method2_review = fct_relevel(method2_review, "none", "auto", "manual"),
    method2_filter = as.numeric(method2_filter)
  ) %>%
  rename(
    "filter" = method2_filter,
    "manual_type" = method1_type,
    "window" = method2_window,
    "approach" = method2_approach,
    "review" = method2_review
  ) %>%
  arrange(task, manual_type, filter, approach, window, review) %>%
  pivot_wider(
    id_cols = c("task", "manual_type", "filter", "window"),
    names_from = c("approach", "review"),
    values_from = "mean"
  ) %>%
  arrange(task, manual_type, filter, window) %>%
  filter(task == "switching") %>%
  select(-task) %>%
  rename(
    "manual" = "manual_type"
  ) %>%
  flextable() %>%
  colformat_double(j = -c(1:3), digits = 2) %>%
  separate_header() %>%
  align(align = "center", part = "all", j = -c(1:2)) %>%
  merge_v(j = 1) %>%
  # valign(j = 1, valign = "top") %>%
  hline(i = c(9)) %>%
  apa_footer(footer_text_cormanual) %>%
  line_spacing(space = 0.5, part = "all") %>%
  # set_caption("Reliability - Nback Task") %>%
  set_table_properties(layout = "autofit", width = 0.75)

table_mean_manualcor_nback <- mean_manualcor_full_info %>%
  ungroup() %>%
  mutate(
    method2_approach = ifelse(method2_approach == "uninformed", paste0(method2_type), method2_approach),
    method2_window = ifelse(method2_window == "const", "medium", method2_window)
  ) %>%
  select(-method2_type, -n, -contains("quant")) %>%
  mutate(
    method2_approach = fct_relevel(method2_approach, "corr", "minsq", "autoarea", "autopeak"),
    method2_window = fct_relevel(method2_window, "narrow", "medium", "wide"),
    method2_review = fct_relevel(method2_review, "none", "auto", "manual"),
    method2_filter = as.numeric(method2_filter)
  ) %>%
  rename(
    "filter" = method2_filter,
    "manual_type" = method1_type,
    "window" = method2_window,
    "approach" = method2_approach,
    "review" = method2_review
  ) %>%
  arrange(task, manual_type, filter, approach, window, review) %>%
  pivot_wider(
    id_cols = c("task", "manual_type", "filter", "window"),
    names_from = c("approach", "review"),
    values_from = "mean"
  ) %>%
  arrange(task, manual_type, filter, window) %>%
  filter(task == "nback") %>%
  select(-task) %>%
  rename(
    "manual" = "manual_type"
  ) %>%
  flextable() %>%
  colformat_double(j = -c(1:3), digits = 2) %>%
  separate_header() %>%
  align(align = "center", part = "all", j = -c(1:2)) %>%
  merge_v(j = 1) %>%
  # valign(j = 1, valign = "top") %>%
  hline(i = c(9)) %>%
  apa_footer(footer_text_cormanual) %>%
  line_spacing(space = 0.5, part = "all") %>%
  # set_caption("Reliability - Nback Task") %>%
  set_table_properties(layout = "autofit", width = 0.75)
