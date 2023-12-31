library(tidyverse)
library(ggcorrplot)
source("./markdown/analysis/helper_functions.R")

load("./markdown/analysis/data/full_analysis_data_long.rdata")
load("./markdown/analysis/data/double_full_cors.rdata")


corr_overall <- double_full_cors %>%
  filter(
    method1_approach != "jackknifemanual",
    method1_type %in% c("area", "peak"),
    method1_rater == "kathrin" & method2_rater == "sven",
  ) %>%
  filter(method2_smooth == 0, method2_sub_ga == 0) %>%
  filter(method2_review %in% c("manual", "auto") | !method2_approach %in% c("corr", "minsq")) %>%
  filter(method1_filter == method2_filter, method1_bin == method2_bin) %>%
  mutate(combination = factor(interaction(method2_approach, method2_type, method2_review)),
         correlation = V1) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, correlation, .desc = TRUE), y = correlation, fill = method2_approach)
  )+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  geom_hline(yintercept = 0.9, color = "forestgreen", linetype = "dashed")+
  labs(
    x = "method"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

corr_by_task <- double_full_cors %>%
  filter(
    method1_approach != "jackknifemanual",
    method1_type %in% c("area", "peak"),
    method1_rater == "kathrin" & method2_rater == "sven",
  ) %>%
  filter(method2_smooth == 0, method2_sub_ga == 0) %>%
  filter(method2_review %in% c("manual", "auto") | !method2_approach %in% c("corr", "minsq")) %>%
  filter(method1_filter == method2_filter, method1_bin == method2_bin) %>%
  mutate(combination = factor(interaction(method2_approach, method2_type, method2_review)),
         correlation = V1) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, correlation, .desc = TRUE), y = correlation, fill = method2_approach)
  )+
  geom_boxplot()+
  facet_wrap(~task)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  geom_hline(yintercept = 0.9, color = "forestgreen", linetype = "dashed")+
  labs(
    x = "method"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

corr_by_filter <- double_full_cors %>%
  filter(
    method1_approach != "jackknifemanual",
    method1_type %in% c("area", "peak"),
    method1_rater == "kathrin" & method2_rater == "sven",
  ) %>%
  filter(method2_smooth == 0, method2_sub_ga == 0) %>%
  filter(method2_review %in% c("manual", "auto") | !method2_approach %in% c("corr", "minsq")) %>%
  filter(method1_filter == method2_filter, method1_bin == method2_bin) %>%
  mutate(combination = factor(interaction(method2_approach, method2_type, method2_review)),
         correlation = V1) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, correlation, .desc = TRUE), y = correlation, fill = method2_approach)
  )+
  geom_boxplot()+
  facet_wrap(~method2_filter)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  geom_hline(yintercept = 0.9, color = "forestgreen", linetype = "dashed")+
  labs(
    x = "method"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

cor_window_facet <- double_full_cors %>%
  filter(
    method1_approach != "jackknifemanual",
    method1_type %in% c("area", "peak"),
    method1_rater == "kathrin" & method2_rater == "sven",
  ) %>%
  filter(method2_smooth == 0, method2_sub_ga == 0) %>%
  filter(method2_review %in% c("manual", "auto") | !method2_approach %in% c("corr", "minsq")) %>%
  filter(method1_filter == method2_filter, method1_bin == method2_bin) %>%
  filter(method2_approach %in% c("minsq", "corr", "uninformed")) %>%
  mutate(combination = factor(interaction(method2_approach, method2_type, method2_review)),
         correlation = V1) %>%
  ggplot(
    aes(x = method2_window, y = correlation)
  )+
  geom_boxplot(aes(fill = method2_window))+
  facet_wrap(~interaction(task, method2_approach), ncol = 3)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

## ---- Homogeneity
homogeneity_overall <- double_full_cors %>%
  filter(
    method1_bin == method2_bin
  ) %>%
  filter(method1_bin %in% c(5, 6)) %>%
  mutate(combination = factor(interaction(method1_rater, method1_approach)),
         correlation = V1) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, correlation, .desc = TRUE), y = correlation, fill = method1_approach)
  )+
  geom_boxplot()+
  facet_wrap(~task)+
  geom_hline(yintercept = 0.7, color = "red", linetype = "dashed")+
  labs(
    x = "method"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

homogeneity_good_methods <- double_full_cors %>%
  filter(
    method1_bin == method2_bin,
    method1_rater == "sven",
    method2_rater == "sven",
    method1_type %in% c("autoarea", "matched"),
    method2_type %in% c("autoarea", "matched"),
    (method1_review != "none" | method1_type == "autoarea"),
    (method2_review != "none" | method2_type == "autoarea")
  ) %>%
  filter(method1_bin %in% c(5, 6)) %>%
  mutate(combination = factor(interaction(method1_approach, method1_review)),
         correlation = V1) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, correlation, .desc = TRUE), y = correlation, fill = method1_approach)
  )+
  geom_boxplot()+
  facet_wrap(~task)+
  geom_hline(yintercept = 0.7, color = "red", linetype = "dashed")+
  labs(
    x = "method"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))


## Analyzing effect of task ----
data_nested <- double_full_cors %>%
  filter(
    method1_approach != "jackknifemanual",
    method1_type %in% c("area", "peak"),
    method1_rater == "kathrin" & method2_rater == "sven",
  ) %>%
  filter(method2_smooth == 0) %>%
  filter(method2_review %in% c("manual", "auto") | !method2_approach %in% c("corr", "minsq")) %>%
  filter(method1_filter == method2_filter, method1_bin == method2_bin) %>%
  mutate(combination = factor(interaction(method2_approach, method2_type, method2_review)),
         correlation = V1,
         id = factor(paste0(task))) %>%
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
data_nested$omega_subga = NA

for (i in 1:nrow(data_nested)){
  data_nested$omega_filter[i] = data_nested$omega[i][[1]][1, 2]
  data_nested$omega_window[i] = data_nested$omega[i][[1]][2, 2]
  data_nested$omega_subga[i] = data_nested$omega[i][[1]][4, 2]
}

influence_filter_cor <- data_nested %>%
  ggplot(
    aes(
      x = forcats::fct_reorder(interaction(method2_approach, method2_review, method2_type), omega_filter, .desc = TRUE),
      y = omega_filter,
      fill = method2_approach
    )
  )+
  geom_bar(stat = "identity")+
  labs(
    x = "method",
    y = "partial omega squared",
    fill = "approach",
    title = "Effect of Filter Settings on Correlation with Manual Extraction"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

influence_window_cor <- data_nested %>%
  ggplot(
    aes(
      x = forcats::fct_reorder(interaction(method2_approach, method2_review, method2_type), omega_window, .desc = TRUE),
      y = omega_window,
      fill = method2_approach
    )
  )+
  geom_bar(stat = "identity")+
  labs(
    x = "method",
    y = "partial omega squared",
    fill = "approach",
    title = "Effect of Measurement Window on Correlation with Manual Extraction"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

## Correlation between CORR and MINSQ ----
overall_cor_algorithms <- double_full_cors %>%
  filter(
    method1_type == method2_type,
    method1_type == "matched",
    method1_rater == "sven" & method2_rater == "sven",
    method1_approach == "corr", method2_approach == "minsq"
  ) %>%
  filter(method2_smooth == 0) %>%
  filter(method1_filter == method2_filter, method1_bin == method2_bin, method1_bin %in% c(5, 6)) %>%
  mutate(correlation = V1,
         combination = interaction(task)) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, correlation, .desc = TRUE), y = correlation, fill = task)
  )+
  geom_boxplot()+
  geom_hline(yintercept = 0.7, color = "red", linetype = "dashed")+
  labs(
    x = "method"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

double_full_cors %>%
  filter(
    method1_type == method2_type,
    method1_type == "matched",
    method1_rater == "sven" & method2_rater == "sven",
    method1_approach == "corr", method2_approach == "minsq"
  ) %>%
  filter(method2_smooth == 0) %>%
  filter(method1_filter == method2_filter, method1_bin == method2_bin, method1_bin %in% c(5, 6)) %>%
  mutate(correlation = V1,
         combination = interaction(task)) %>%
  mutate(
    same_window = ifelse(method1_window == method2_window, 1, 0),
    same_review = ifelse(method1_review == method2_review, 1, 0)
  ) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, correlation, .desc = TRUE), y = correlation, group = interaction(task, same_window), fill = factor(same_window))
  )+
  geom_boxplot()+
  geom_hline(yintercept = 0.7, color = "red", linetype = "dashed")+
  labs(
    x = "task",
    fill = "same window"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))
