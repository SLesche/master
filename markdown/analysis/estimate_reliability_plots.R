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
  )

rel_overview_long_good <- rel_overview_long %>%
  filter(window == "const") %>%
  filter(sub_ga == 0) %>%
  filter(type %in% c("autoarea", "matched"),
         (approach %in% c("corr", "minsq") & review != "none") | !approach %in% c("corr", "minsq"))

lm(reliability ~ task + filter + approach,
   data = rel_overview_long_good) %>%
  summary()

# Plot reliability over all tasks and windows and filters
rel_over_everything <- rel_overview %>%
  pivot_longer(
    cols = c("rel_1", "rel_2"),
    names_to = "condition",
    values_to = "reliability"
  ) %>%
  mutate(combination = factor(interaction(approach, type, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, reliability, .desc = TRUE), y = reliability, fill = approach)
  )+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

rel_by_task <- rel_overview %>%
  filter(type %in% c("autoarea", "matched"),
         (approach %in% c("corr", "minsq") & review != "none") | !approach %in% c("corr", "minsq")) %>%
  pivot_longer(
    cols = c("rel_1", "rel_2"),
    names_to = "condition",
    values_to = "reliability"
  ) %>%
  mutate(combination = factor(interaction(approach, type, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, reliability, .desc = TRUE), y = reliability, fill = approach)
  )+
  facet_wrap(~task)+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))


rel_by_filter <- rel_overview %>%
  filter(type %in% c("autoarea", "matched"),
         (approach %in% c("corr", "minsq") & review != "none") | !approach %in% c("corr", "minsq")) %>%
  pivot_longer(
    cols = c("rel_1", "rel_2"),
    names_to = "condition",
    values_to = "reliability"
  ) %>%
  mutate(combination = factor(interaction(approach, type, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, reliability, .desc = TRUE), y = reliability, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~filter)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

failure_data <- rel_overview %>%
  pivot_longer(
    cols = c("rel_1", "rel_2"),
    names_to = "condition",
    values_to = "reliability"
  ) %>%
  ungroup() %>%
  mutate(failure = ifelse(reliability < 0.5, 1, 0)) %>%
  count(approach, type, review, failure) %>%
  filter(failure == 1)

## Checking for effect of window on rel ----
rel_window_facet <- rel_overview_long %>%
  filter(
    type %in% c("autoarea", "matched"),
         (approach %in% c("corr", "minsq") & review != "none") | !approach %in% c("corr", "minsq"),
    approach %in% c("corr", "minsq", "uninformed")
    ) %>%
  ggplot(
    aes(x = window, y = reliability)
  )+
  geom_boxplot(aes(fill = window))+
  facet_wrap(~interaction(task, approach), ncol = 3)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

rel_subga_facet <- rel_overview_long %>%
  filter(
    type %in% c("autoarea", "matched"),
    (approach %in% c("corr", "minsq") & review != "none") | !approach %in% c("corr", "minsq"),
    approach %in% c("corr", "minsq", "uninformed")
  ) %>%
  ggplot(
    aes(x = factor(sub_ga), y = reliability)
  )+
  geom_boxplot(aes(fill = factor(sub_ga)))+
  facet_wrap(~interaction(task, approach), ncol = 3)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")+
  labs(
    x = "method",
    fill = "Sub GA"
  )+
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

## Analyzing effect of task ----
data_nested <- rel_overview_long %>%
  filter(
    (approach %in% c("corr", "minsq") & review != "none") | !approach %in% c("corr", "minsq")
  ) %>%
  mutate(id = factor(paste0(task, filter))) %>%
  nest(.by = c("approach", "type", "review")) %>%
  mutate(
    aov = map(
      data,
      ~afex::aov_ez(
        id = "id",
        dv = "reliability",
        between = c("filter"),
        within = c("window", "sub_ga"),
        data = .
      )
    )
  )

data_nested <- data_nested %>%
  mutate(
    omega = map(
      aov,
      ~effectsize::effectsize(.[[1]], ci = 0.95, type = "omega")
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

influence_filter_rel <- data_nested %>%
  ggplot(
    aes(
      x = forcats::fct_reorder(interaction(approach, review, type), omega_filter, .desc = TRUE),
      y = omega_filter,
      fill = approach
    )
  )+
  geom_bar(stat = "identity")+
  labs(
    x = "method",
    y = "partial omega squared",
    fill = "approach",
    title = "Effect of Filter Settings on Split-Half Correlation"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

influence_window_rel <- data_nested %>%
  ggplot(
    aes(
      x = forcats::fct_reorder(interaction(approach, review, type), omega_window, .desc = TRUE),
      y = omega_window,
      fill = approach
    )
  )+
  geom_bar(stat = "identity")+
  labs(
    x = "method",
    y = "partial omega squared",
    fill = "approach",
    title = "Effect of Measurement Window on Split-Half Correlation"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))
