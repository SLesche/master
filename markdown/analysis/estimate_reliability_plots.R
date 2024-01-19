library(tidyverse)
library(ggcorrplot)
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
  ungroup() %>%
  mutate(
    id = row_number()
  ) %>%
  mutate(
    window = ifelse(window == "const", "medium", window),
    approach = ifelse(approach == "corr", "maxcor", approach)
  )

rel_overview_long_good <- rel_overview_long %>%
  # filter(window == "const") %>%
  filter(sub_ga == 0) %>%
  filter(type %in% c("autoarea", "matched", "autopeak"),
         approach %in% c("maxcor", "minsq", "uninformed"),
         !(approach %in% c("maxcor", "minsq") & review == "none")) %>%
  mutate(approach = ifelse(approach == "uninformed", type, approach)) %>%
  mutate(review = ifelse(review == "none", "auto", review))

# Plot reliability over all tasks and windows and filters
rel_over_everything <- rel_overview_long_good %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, reliability, .desc = TRUE), y = reliability, fill = approach)
  )+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed", linewidth = 1)+
  labs(
    x = "method",
    y = "reliability"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=15))

rel_by_task <- rel_overview_long_good %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, reliability, .desc = TRUE), y = reliability, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~task)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed", linewidth = 1)+
  labs(
    x = "method",
    y = "reliability"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=15))


rel_by_filter <- rel_overview_long_good %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, reliability, .desc = TRUE), y = reliability, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~filter)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed", linewidth = 1)+
  labs(
    x = "method",
    y = "reliability"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=15))

rel_by_window <- rel_overview_long_good %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = forcats::fct_relevel(window, "narrow", "medium", "wide"), y = reliability, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~forcats::fct_reorder(combination, reliability, .desc = TRUE))+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed", linewidth = 1)+
  labs(
    x = "method",
    y = "reliability"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=15))



## Checking for effect of window on rel ----
rel_window_facet <- rel_overview_long_good %>%
  filter(review != "manual") %>%
  mutate(combination = factor(interaction(approach, review))) %>%
  ggplot(
    aes(x = window, y = reliability)
  )+
  geom_boxplot(aes(fill = window))+
  facet_wrap(~interaction(task, approach), ncol = 3)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method",
    y = "reliability"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))


