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
  mutate(
    method1_approach = ifelse(method1_approach == "corr", "maxcor", method1_approach),
    method1_window = ifelse(method1_window == "const", "medium", method1_window),
    method2_approach = ifelse(method2_approach == "corr", "maxcor", method2_approach),
    method2_window = ifelse(method2_window == "const", "medium", method2_window)
  ) %>%
  mutate(
    method1_approach = ifelse(method1_approach == "individualmanual", "manual", method1_approach),
    method2_approach = ifelse(method2_approach == "individualmanual", "manual", method2_approach)
  ) %>%
  filter(
    method1_approach != "jackknifemanual",
    method2_approach != "jackknifemanual"
  ) %>%
  filter(
    !method1_approach %in% c("informedcorr", "informedminsq"),
    !(method1_approach != "uninformed" & method1_review == "none"),
    !method2_approach %in% c("informedcorr", "informedminsq"),
    !(method2_approach != "uninformed" & method2_review == "none")
  ) %>%
  mutate(
    method1_approach = ifelse(method1_approach == "uninformed", method1_type, method1_approach),
    method2_approach = ifelse(method2_approach == "uninformed", method2_type, method2_approach)
  ) %>%
  mutate(
    method1_review = ifelse(method1_review == "none", "auto", method1_review),
    method2_review = ifelse(method2_review == "none", "auto", method2_review)
    ) %>%
  filter(
    !(method1_approach == method2_approach & method1_type == method2_type)
  ) %>%
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
  ) %>%
  ungroup()

homogeneity_total <- mean_homogeneity_overall %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, mean, .desc = TRUE), y = mean, fill = approach)
  )+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed", linewidth = 1)+
  labs(
    x = "method",
    y = "homogeneity"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=15))

homogeneity_by_task <- mean_homogeneity_overall %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, mean, .desc = TRUE), y = mean, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~task)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed", linewidth = 1)+
  labs(
    x = "method",
    y = "homogeneity"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=15))

homogeneity_by_filter <- mean_homogeneity_overall %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, mean, .desc = TRUE), y = mean, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~forcats::fct_relevel(filter, "8", "16", "32"))+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed", linewidth = 1)+
  labs(
    x = "method",
    y = "homogeneity"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=15))

homogeneity_by_window <- mean_homogeneity_overall %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_relevel(window, "narrow", "medium", "wide"), y = mean, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~forcats::fct_reorder(combination, mean, .desc = TRUE))+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed", linewidth = 1)+
  labs(
    x = "method",
    y = "homogeneity"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=15))

homogeneity_medium_window <- mean_homogeneity_overall %>%
  filter(window == "medium") %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, mean, .desc = TRUE), y = mean, fill = approach)
  )+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed", linewidth = 1)+
  labs(
    x = "method",
    y = "homogeneity"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=15))

