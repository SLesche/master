library(tidyverse)
library(ggcorrplot)
source("./markdown/analysis/helper_functions.R")

load("./markdown/analysis/data/full_analysis_data_long.rdata")
load("./markdown/analysis/data/double_full_cors.rdata")

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

data_nested <- double_full_cors %>%
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

data_nested <- data_nested %>%
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

data_nested$omega_filter = NA
data_nested$omega_window = NA

for (i in 1:nrow(data_nested)){
  data_nested$omega_filter[i] = data_nested$omega[i][[1]][1, 2]
  data_nested$omega_window[i] = data_nested$omega[i][[1]][2, 2]
}
