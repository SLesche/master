library(tidyverse)
source("./markdown/analysis/helper_functions.R")

load("./markdown/analysis/data/full_analysis_data_long.rdata")

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
    mean = fisher_cor_mean(reliability),
    n = n(),
    quant_10 = quantile(reliability, 0.1),
    quant_90 = quantile(reliability, 0.9)
  )

data_nested_reliability <- rel_overview_long %>%
  mutate(id = factor(paste0(task, filter))) %>%
  nest(.by = c("approach", "type", "review")) %>%
  mutate(
    aov = map(
      data,
      ~afex::aov_ez(
        id = "id",
        dv = "reliability",
        between = c("filter"),
        within = c("window"),
        data = .
      )
    )
  )

data_nested_reliability <- data_nested_reliability %>%
  mutate(
    omega = map(
      aov,
      ~effectsize::effectsize(.[[1]], ci = 0.95, type = "omega")
    )
  )

data_nested_reliability$omega_filter = NA
data_nested_reliability$omega_window = NA

for (i in 1:nrow(data_nested_reliability)){
  data_nested_reliability$omega_filter[i] = data_nested_reliability$omega[i][[1]][1, 2]
  data_nested_reliability$omega_window[i] = data_nested_reliability$omega[i][[1]][2, 2]
}
