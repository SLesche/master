library(tidyverse)
library(ggcorrplot)
source("./markdown/analysis/helper_functions.R")

load("./markdown/analysis/data/full_analysis_data_long.rdata")
load("./markdown/analysis/data/double_full_cors.rdata")

data_wide <- data %>%
  select(-contains("filepath"), -prefix, -origin) %>%
  filter(
    approach %in% c("corr", "minsq", "uninformed"),
    sub_ga == 0,
    rater == "sven",
    !((type == "matched") & !(review == "manual")),
    bin %in% c(5, 6)
  ) %>%
  pivot_wider(
    id_cols = c("task", "group", "subject", "component", "bin"),
    values_from = "latency",
    names_from = c("filter", "rater", "smooth", "sub_ga", "window", "approach", "type", "review"),
    names_sep = "_"
  )

# Separating by task
data_list <- vector(mode = "list", length = 3)

data_list[[1]]$task = "flanker"
data_list[[2]]$task = "nback"
data_list[[3]]$task = "switching"

for (i in seq_along(data_list)){
  data_list[[i]]$data = data_wide[which(data_wide$task == data_list[[i]]$task),]
}

# Computing correlation matrix
for (i in seq_along(data_list)){
  data_list[[i]]$cormat = cor(data_list[[i]]$data[, 6:ncol(data_list[[i]]$data)], use = "pairwise.complete.obs")
}

ggcorrplot(data_list[[3]]$cormat)
