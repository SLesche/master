library(tidyverse)
library(ggcorrplot)
source("helper_functions.R")

load("./data/full_analysis_data_long.rdata")

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

# data_nested %>% 
#   count(approach, review, type, failure)

eff_overall <- data_nested %>% 
  filter(approach != "jackknifemanual") %>% 
  mutate(combination = factor(interaction(approach, type, review))) %>% 
  ggplot(
    aes(x = forcats::fct_reorder(combination, omega_age, .desc = TRUE), y = omega_age, fill = approach)
  )+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.2)+
  labs(
    x = "method"
  )+ 
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

eff_by_task <- data_nested %>% 
  filter(type %in% c("autoarea", "matched"),
         (approach %in% c("corr", "minsq") & review != "none") | !approach %in% c("corr", "minsq")) %>% 
  filter(approach != "jackknifemanual") %>% 
  mutate(combination = factor(interaction(approach, type, review))) %>% 
  ggplot(
    aes(x = forcats::fct_reorder(combination, omega_age, .desc = TRUE), y = omega_age, fill = approach)
  )+
  geom_boxplot()+
  facet_wrap(~task)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  labs(
    x = "method"
  )+ 
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

eff_window_facet <- data_nested %>% 
  filter(type %in% c("autoarea", "matched"),
         (approach %in% c("corr", "minsq") & review != "none") | !approach %in% c("corr", "minsq")) %>% 
  filter(approach %in% c("corr", "minsq", "uninformed")) %>% 
  mutate(combination = factor(interaction(approach, type, review))) %>% 
  ggplot(
    aes(x = window, y = omega_age)
  )+
  geom_boxplot(aes(fill = window))+
  facet_wrap(~interaction(task, approach), ncol = 3)+
  geom_jitter(width = 0.1, alpha = 0.2)+
  labs(
    x = "method"
  )+ 
  # scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()+
  theme(text=element_text(size=21),
        axis.text = element_text(size=10))

data_nested %>% 
  filter(window == "const" | window == "manualrater") %>% 
  filter(type %in% c("autoarea", "matched"),
         (approach %in% c("corr", "minsq") & review != "none") | !approach %in% c("corr", "minsq")) %>% 
  filter(approach != "jackknifemanual") %>% 
  count(task, approach, type, failure) %>% 
  filter(failure == 1)
