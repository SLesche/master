library(tidyverse)
source("./markdown/analysis/helper_functions.R")

load("./markdown/analysis/data/double_full_icc.rdata")

mean_manualcor_full_info <- double_full_icc %>%
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
    mean = mean(correlation),
    n = n(),
    quant_10 = quantile(correlation, 0.1),
    quant_90 = quantile(correlation, 0.9)
  ) %>%
  rename(
    "filter" = method2_filter,
    "window" = method2_window,
    "approach" = method2_approach,
    "type" = method2_type,
    "review" = method2_review
  ) %>%
  ungroup() %>%
  mutate(approach = ifelse(approach == "uninformed", type, approach)) %>%
  mutate(review = ifelse(review == "none", "auto", review)) %>%
  mutate(filter = forcats::fct_relevel(filter, "8", "16", "32")) %>%
  mutate(
    window = ifelse(window == "const", "medium", window),
    approach = ifelse(approach == "corr", "maxcor", approach)
  )

corr_overall <- mean_manualcor_full_info %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, mean, .desc = TRUE), y = mean, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~method1_type)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method",
    y = "intraclass-correlation"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

corr_by_task <- mean_manualcor_full_info %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, mean, .desc = TRUE), y = mean, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~interaction(method1_type, task), ncol = 2)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method",
    y = "intraclass-correlation"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

corr_by_filter <- mean_manualcor_full_info %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, mean, .desc = TRUE), y = mean, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~interaction(method1_type, filter), ncol = 2)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method",
    y = "intraclass-correlation"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

corr_by_window <- mean_manualcor_full_info %>%
  filter(approach %in% c("minsq", "maxcor")) %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_relevel(window, "narrow", "medium", "wide"), y = mean, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~interaction(method1_type, forcats::fct_reorder(combination, mean, .desc = TRUE)), ncol = 2)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method",
    y = "intraclass-correlation"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

corr_optimal_window <- mean_manualcor_full_info %>%
  filter(window == "medium") %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, mean, .desc = TRUE), y = mean, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~interaction(method1_type, task), ncol = 2)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method",
    y = "intraclass-correlation"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))
