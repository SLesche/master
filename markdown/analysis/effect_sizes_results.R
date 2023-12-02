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
    mean = mean(omega_age, na.rm = TRUE),
    n = n(),
    n_failures = sum(failure)
  )

mean_effsize_fullinfo <- data_nested_effsize %>%
  group_by(task, filter, window, approach, type, review) %>%
  summarize(
    mean = mean(omega_age, na.rm = TRUE),
    n = n(),
    n_failures = sum(failure)
  )

table_mean_effsize_flanker <- mean_effsize_fullinfo %>%
  ungroup() %>%
  filter(
    approach %in% c("corr", "minsq", "uninformed")
  ) %>%
  mutate(
    approach = ifelse(approach == "uninformed", paste0(type), approach),
    window = ifelse(window == "const", "medium", window)
  ) %>%
  select(-type, -n, -contains("quant")) %>%
  mutate(
    approach = fct_relevel(approach, "corr", "minsq", "autoarea", "autopeak"),
    window = fct_relevel(window, "narrow", "medium", "wide"),
    review = fct_relevel(review, "none", "auto", "manual"),
    filter = as.numeric(filter)
  ) %>%
  arrange(task, filter, approach, window, review) %>%
  pivot_wider(
    id_cols = c("task", "filter", "window"),
    names_from = c("approach", "review"),
    values_from = "mean"
  ) %>%
  arrange(task, filter, window) %>%
  filter(task == "flanker") %>%
  select(-task) %>%
  flextable() %>%
  colformat_double(j = -1, digits = 2) %>%
  separate_header() %>%
  align(align = "center", part = "all", j = -c(1:2)) %>%
  merge_v(j = 1) %>%
  # valign(j = 1, valign = "top") %>%
  hline(i = c(3, 6)) %>%
  apa_footer("My personal note on this table.") %>%
  line_spacing(space = 0.5, part = "all") %>%
  # set_caption("Reliability - Nback Task") %>%
  set_table_properties(layout = "autofit", width = 0.75)

table_mean_effsize_switching <- mean_effsize_fullinfo %>%
  ungroup() %>%
  filter(
    approach %in% c("corr", "minsq", "uninformed")
  ) %>%
  mutate(
    approach = ifelse(approach == "uninformed", paste0(type), approach),
    window = ifelse(window == "const", "medium", window)
  ) %>%
  select(-type, -n, -contains("quant")) %>%
  mutate(
    approach = fct_relevel(approach, "corr", "minsq", "autoarea", "autopeak"),
    window = fct_relevel(window, "narrow", "medium", "wide"),
    review = fct_relevel(review, "none", "auto", "manual"),
    filter = as.numeric(filter)
  ) %>%
  arrange(task, filter, approach, window, review) %>%
  pivot_wider(
    id_cols = c("task", "filter", "window"),
    names_from = c("approach", "review"),
    values_from = "mean"
  ) %>%
  arrange(task, filter, window) %>%
  filter(task == "switching") %>%
  select(-task) %>%
  flextable() %>%
  colformat_double(j = -1, digits = 2) %>%
  separate_header() %>%
  align(align = "center", part = "all", j = -c(1:2)) %>%
  merge_v(j = 1) %>%
  # valign(j = 1, valign = "top") %>%
  hline(i = c(3, 6)) %>%
  apa_footer("My personal note on this table.") %>%
  line_spacing(space = 0.5, part = "all") %>%
  # set_caption("Reliability - Nback Task") %>%
  set_table_properties(layout = "autofit", width = 0.75)

table_mean_effsize_nback <- mean_effsize_fullinfo %>%
  ungroup() %>%
  filter(
    approach %in% c("corr", "minsq", "uninformed")
  ) %>%
  mutate(
    approach = ifelse(approach == "uninformed", paste0(type), approach),
    window = ifelse(window == "const", "medium", window)
  ) %>%
  select(-type, -n, -contains("quant")) %>%
  mutate(
    approach = fct_relevel(approach, "corr", "minsq", "autoarea", "autopeak"),
    window = fct_relevel(window, "narrow", "medium", "wide"),
    review = fct_relevel(review, "none", "auto", "manual"),
    filter = as.numeric(filter)
  ) %>%
  arrange(task, filter, approach, window, review) %>%
  pivot_wider(
    id_cols = c("task", "filter", "window"),
    names_from = c("approach", "review"),
    values_from = "mean"
  ) %>%
  arrange(task, filter, window) %>%
  filter(task == "nback") %>%
  select(-task) %>%
  flextable() %>%
  colformat_double(j = -1, digits = 2) %>%
  separate_header() %>%
  align(align = "center", part = "all", j = -c(1:2)) %>%
  merge_v(j = 1) %>%
  # valign(j = 1, valign = "top") %>%
  hline(i = c(3, 6)) %>%
  apa_footer("My personal note on this table.") %>%
  line_spacing(space = 0.5, part = "all") %>%
  # set_caption("Reliability - Nback Task") %>%
  set_table_properties(layout = "autofit", width = 0.75)
