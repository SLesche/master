library(tidyverse)
library(ggcorrplot)
source("./markdown/analysis/helper_functions.R")

load("./markdown/analysis/data/full_analysis_data_long.rdata")

# Check for shifts in mean latency
run_anova <- function(data){
  anova = afex::aov_ez(id= "subject", dv= "latency", data = data, between = "group",  within = c("condition"))
}

data_nested <- data %>%
  filter(bin %in% c(5, 6)) %>%
  nest(.by = c("task", "approach", "filter", "sub_ga", "window", "rater", "review", "type")) %>%
  mutate(
    aov = map(
      data,
      ~run_anova(.)
    )
  )

data_nested <- data_nested %>%
  mutate(
    omega = map(
      aov,
      ~effectsize::effectsize(.[[1]], ci = 0.95, type = "omega")
    )
  )

data_nested$omega_task = NA
data_nested$omega_age = NA

for (i in 1:nrow(data_nested)){
  data_nested$omega_age[i] = data_nested$omega[i][[1]][1, 2]
  data_nested$omega_task[i] = data_nested$omega[i][[1]][2, 2]
}

data_nested <- data_nested %>%
  mutate(failure = ifelse(omega_age < 0.01 | omega_age > 0.8, 1, 0))

data_eff <- data_nested %>%
  mutate(
    approach = ifelse(approach == "corr", "maxcor", approach),
    window = ifelse(window == "const", "medium", window)
  ) %>%
  mutate(
    approach = ifelse(approach == "individualmanual", "manual", approach)
  ) %>%
  filter(
    approach != "jackknifemanual"
  ) %>%
  filter(
    !approach %in% c("informedcorr", "informedminsq"),
    !(approach != "uninformed" & review == "none")
  ) %>%
  mutate(
    approach = ifelse(approach == "uninformed", type, approach)
    ) %>%
  mutate(review = ifelse(review == "none", "auto", review))

eff_overall <- data_eff %>%
  mutate(combination = factor(interaction(approach, type, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, omega_age, .desc = TRUE), y = omega_age, fill = approach)
  )+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.2)+
  labs(
    x = "method",
    y = "Age Effect"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

eff_by_task <- data_eff %>%
  mutate(combination = factor(interaction(approach, type, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, omega_age, .desc = TRUE),
        y = omega_age,
        fill = approach,)
  )+
  geom_boxplot()+
  facet_wrap(~task)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  labs(
    x = "method",
    y = "Age Effect"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

eff_by_filter <- data_eff %>%
  filter(filter %in% c("8", "16", "32")) %>%
  mutate(combination = factor(interaction(approach, type, review))) %>%
  ggplot(
    aes(x = forcats::fct_reorder(combination, omega_age, .desc = TRUE),
        y = omega_age,
        fill = approach,)
  )+
  geom_boxplot()+
  facet_wrap(~filter)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  labs(
    x = "method",
    y = "Age Effect"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))


eff_by_window <- data_eff %>%
  filter(approach != "manual") %>%
  mutate(combination = factor(interaction(approach, type, review))) %>%
  ggplot(
    aes(x = forcats::fct_relevel(window, "narrow", "medium", "wide"),
        y = omega_age,
        fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~forcats::fct_reorder(combination, omega_age, .desc = TRUE))+
  geom_jitter(width = 0.1, alpha = 0.2)+
  labs(
    x = "method",
    y = "Age Effect"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))
