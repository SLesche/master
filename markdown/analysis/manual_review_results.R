# library(tidyverse)
# source("./markdown/analysis/helper_functions.R")
#
# load("./markdown/analysis/data/full_analysis_data_long.rdata")

data_manual_review <- data %>%
  filter(review == "manual", rater == "sven") %>%
  count(approach, origin) %>%
  group_by(approach) %>%
  mutate(n_total = sum(n)) %>%
  ungroup()

data_manual_review_nauto <- data_manual_review %>%
  filter(origin == "auto") %>%
  rename("n_auto" = "n") %>%
  select(approach, n_auto)

data_manual_review <- data_manual_review %>%
  left_join(., data_manual_review_nauto) %>%
  mutate(n_review = n_total - n_auto) %>%
  mutate(freq = n / n_review) %>%
  mutate(freq_reviewed = n_review/n_total)

data_autoreview <- data %>%
  filter(review == "auto" | review == "none", approach %in% c("minsq", "corr")) %>%
  count(approach, review, is.na(latency)) %>%
  group_by(approach, review) %>%
  mutate(freq = n / sum(n),
         n_total = sum(n)) %>%
  ungroup() %>%
  filter(`is.na(latency)` == TRUE)

additional_nas_autoreview_corr <- data_autoreview %>%
  filter(approach == "corr") %>%
  select(n_total, n, freq)

additional_nas_autoreview_minsq <- data_autoreview %>%
  filter(approach == "minsq") %>%
  select(n_total, n, freq)

additional_nas_autoreview_minsq <- additional_nas_autoreview_minsq[1, ] - c(0, additional_nas_autoreview_minsq[2, 2:3])
