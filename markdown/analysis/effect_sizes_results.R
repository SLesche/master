# library(tidyverse)
# source("./markdown/analysis/helper_functions.R")
#
# load("./markdown/analysis/data/full_analysis_data_long.rdata")

# Check for shifts in mean latency
run_anova <- function(data){
  anova = afex::aov_ez(id= "subject", dv= "latency", data = data, between = "group",  within = c("condition"))
}

data_nested_effsize <- data %>%
  filter(bin %in% c(5, 6)) %>%
  nest(.by = c("task", "approach", "filter", "sub_ga", "window", "rater", "review", "type")) %>%
  mutate(
    aov = map(
      data,
      ~run_anova(.)
    )
  )

data_nested_effsize <- data_nested_effsize %>%
  mutate(
    omega = map(
      aov,
      ~effectsize::effectsize(.[[1]], ci = 0.95, type = "omega")
    )
  )

data_nested_effsize$omega_condition = NA
data_nested_effsize$omega_age = NA

for (i in 1:nrow(data_nested_effsize)){
  data_nested_effsize$omega_age[i] = data_nested_effsize$omega[i][[1]][1, 2]
  data_nested_effsize$omega_condition[i] = data_nested_effsize$omega[i][[1]][2, 2]
}

data_nested_effsize <- data_nested_effsize %>%
  mutate(failure = ifelse(omega_age < 0.01 | omega_age > 0.8, 1, 0))

mean_effsize_overall <- data_nested_effsize %>%
  group_by(approach, type, review) %>%
  summarize(
    mean = fisher_cor_mean(omega_age),
    n = n(),
    n_failures = sum(failure),
    quant_10 = quantile(omega_age, 0.1),
    quant_90 = quantile(omega_age, 0.9)
  )
