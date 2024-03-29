# library(tidyverse)
# source("./markdown/analysis/helper_functions.R")
#
# load("./markdown/analysis/data/full_analysis_data_long.rdata")

rel_overview <- data %>%
  filter(rater == "sven") %>%
  pivot_wider(
    id_cols = c("task", "filter", "smooth", "sub_ga",
                "component", "window", "prefix", "approach", "type",
                "review", "group", "subject"),
    names_from = "bin",
    names_prefix = "bin_",
    values_from = "latency"
  ) %>%
  group_by(task, filter, smooth, sub_ga, component, window, type, approach, review) %>%
  summarize(
    rel_1 = spearman_brown_double(cor(as.numeric(bin_1), as.numeric(bin_2), use = "pairwise.complete.obs")),
    rel_2 = spearman_brown_double(cor(as.numeric(bin_3), as.numeric(bin_4), use = "pairwise.complete.obs")),
    n = n()
  )

rel_overview_long <- rel_overview %>%
  pivot_longer(
    cols = c("rel_1", "rel_2"),
    names_to = "condition",
    values_to = "reliability"
  ) %>%
  ungroup()

mean_reliability_overall <- rel_overview_long %>%
  group_by(approach, type, review) %>%
  summarize(
    mean = mean(reliability),
    n = n(),
    quant_10 = quantile(reliability, 0.1),
    quant_90 = quantile(reliability, 0.9)
  )

# data_nested_reliability <- rel_overview_long %>%
#   mutate(id = factor(paste0(task, filter))) %>%
#   nest(.by = c("approach", "type", "review")) %>%
#   mutate(
#     aov = map(
#       data,
#       ~afex::aov_ez(
#         id = "id",
#         dv = "reliability",
#         between = c("filter"),
#         within = c("window"),
#         data = .
#       )
#     )
#   )
#
# data_nested_reliability <- data_nested_reliability %>%
#   mutate(
#     omega = map(
#       aov,
#       ~effectsize::effectsize(.[[1]], ci = 0.95, type = "omega")
#     )
#   )
#
# data_nested_reliability$omega_filter = NA
# data_nested_reliability$omega_window = NA
#
# for (i in 1:nrow(data_nested_reliability)){
#   data_nested_reliability$omega_filter[i] = data_nested_reliability$omega[i][[1]][1, 2]
#   data_nested_reliability$omega_window[i] = data_nested_reliability$omega[i][[1]][2, 2]
# }

cutoff <- 0.8
footer_text_reliability <- c(
  "Values represent the Spearman-Brown corrected split-half correlation of a particular extraction method; values greater than 0.8 are colored in green, less than 0.8 in orange; maxcor = MAXCOR-based algorithm; minsq = MINSQ-based algorithm; autoarea = Area latency algorithm; autopeak = Peak latency algorithm; results of the algorithms either not reviewed (none), automatically reviewed based on the fit statistic (auto), or reviewed manually (manual); filter = low-pass filter used in preprocessing (in Hz); window = measurement window used for latency extraction (narrow = 250 - 600 ms; medium = 200 - 700 ms; wide = 150 - 900ms)"
)

mean_reliability_task_filter <- rel_overview_long %>%
  group_by(task, filter, window, approach, type, review) %>%
  summarize(
    mean = fisher_cor_mean(reliability),
    n = n(),
    quant_10 = quantile(reliability, 0.1),
    quant_90 = quantile(reliability, 0.9)
  )

make_flextable_reliability <- function(data, cutoff){
  ncol = ncol(data)
  color_mat = ifelse(data[, 3:ncol] > cutoff, "forestgreen", "darkorange")
  flextable = data %>%
    flextable() %>%
    colformat_double(j = -1, digits = 2) %>%
    separate_header() %>%
    align(align = "center", part = "all", j = -c(1:2)) %>%
    align(align = "left", part = "all", j = c(1:2)) %>%
    merge_v(j = 1) %>%
    # valign(j = 1, valign = "top") %>%
    hline(i = c(3, 6)) %>%
    color(
      j = 3:ncol,
      color = color_mat
    ) %>%
    apa_footer(footer_text_reliability) %>%
    line_spacing(space = 0.5, part = "all") %>%
    # set_caption("Reliability - Nback Task") %>%
    set_table_properties(layout = "autofit", width = 0.75)
  return(flextable)
}

prepare_data_reliability <- function(data, chosen_task){
  prep = data %>%
    ungroup() %>%
    filter(
      approach %in% c("corr", "minsq", "uninformed")
    ) %>%
    mutate(
      approach = ifelse(approach == "uninformed", paste0(type), approach),
      window = ifelse(window == "const", "medium", window)
    ) %>%
    select(-type, -n, -contains("quant")) %>%
    mutate(approach = ifelse(approach == "corr", "maxcor", approach)) %>%
    mutate(
      approach = fct_relevel(approach, "maxcor", "minsq", "autoarea", "autopeak"),
      window = fct_relevel(window, "narrow", "medium", "wide"),
      review = fct_relevel(review, "none", "auto", "manual")
    ) %>%
    arrange(task, filter, approach, window, review) %>%
    pivot_wider(
      id_cols = c("task", "filter", "window"),
      names_from = c("approach", "review"),
      values_from = "mean"
    ) %>%
    arrange(task, filter, window) %>%
    filter(task == chosen_task) %>%
    select(-task)

  return(prep)
}
table_mean_reliability_flanker <- mean_reliability_task_filter %>%
  prepare_data_reliability(chosen_task = "flanker") %>%
  make_flextable_reliability(cutoff = cutoff)


table_mean_reliability_nback <- mean_reliability_task_filter %>%
  prepare_data_reliability(chosen_task = "nback") %>%
  make_flextable_reliability(cutoff = cutoff)


table_mean_reliability_switching <- mean_reliability_task_filter %>%
  prepare_data_reliability(chosen_task = "switching") %>%
  make_flextable_reliability(cutoff = cutoff)

