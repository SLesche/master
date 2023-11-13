library(tidyverse)
library(ggcorrplot)
source("./markdown/analysis/helper_functions.R")

load("./markdown/analysis/data/full_analysis_data_long.rdata")

data_origin <- data %>%
  filter(review == "manual", rater == "sven")

data_origin %>%
  count(approach, origin) %>%
  group_by(approach) %>%
  mutate(n_total = sum(n)) %>%
  ungroup() %>%
  mutate(freq = n/n_total)

data_autoreview <- data %>%
  filter(review == "auto" | review == "none", approach %in% c("minsq", "corr"))

data_autoreview %>%
  count(approach, review, is.na(latency)) %>%
  group_by(approach, review) %>%
  mutate(freq = n / sum(n),
         n_total = sum(n)) %>%
  ungroup() %>%
  filter(`is.na(latency)` == TRUE)
