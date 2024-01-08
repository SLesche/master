library(tidyverse)
library(ggcorrplot)
source("./markdown/analysis/helper_functions.R")

load("./markdown/analysis/data/full_analysis_data_long.rdata")

data_wide <- data %>%
  select(-contains("filepath"), -prefix, -origin) %>%
  filter(
    approach %in% c("corr", "minsq", "uninformed", "individualmanual"),
    bin %in% c(5, 6),
    filter %in% c(8, 16, 32)
  ) %>%
  pivot_wider(
    id_cols = c("task", "group", "subject", "component"),
    values_from = "latency",
    names_from = c("filter", "rater", "smooth", "sub_ga", "window", "approach", "type", "review", "bin"),
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
  data_list[[i]]$cormat = compute_icc_mat(data_list[[i]]$data[, 5:ncol(data_list[[i]]$data)])
  data_list[[i]]$correlations = turn_cormatrix_into_vector(data_list[[i]]$cormat)
}


# Modifying vars
for (i in seq_along(data_list)){
  data_list[[i]]$correlations = data_list[[i]]$correlations %>%
    separate(vars, c("method1", "method2"), "___") %>%
    separate(method1, paste0("method1_", c("filter", "rater", "smooth", "sub_ga", "window", "approach", "type", "review", "bin")), "_") %>%
    separate(method2, paste0("method2_", c("filter", "rater", "smooth", "sub_ga", "window", "approach", "type", "review", "bin")), "_")
}

# Getting correlation between Kathrin and other data
flanker_cors <- data_list[[1]]$correlations
nback_cors <- data_list[[2]]$correlations
switching_cors <- data_list[[3]]$correlations

flanker_cors$task = "flanker"
nback_cors$task = "nback"
switching_cors$task = "switching"

full_cors = rbind(flanker_cors, nback_cors, switching_cors)

colnames_full_cors <- colnames(full_cors)
colnames_full_cors_flipped <- str_replace(colnames_full_cors, "method1", "method3") %>%
  str_replace(., "method2", "method1") %>%
  str_replace(., "method3", "method2")
full_cors_flipped <- full_cors
colnames(full_cors_flipped) <- colnames_full_cors_flipped

double_full_icc <- rbind(full_cors, full_cors_flipped)

save(double_full_icc, file = "./markdown/analysis/data/double_full_icc.rdata")
