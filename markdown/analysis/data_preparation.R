# Library calls
library(tidyverse)
library(ggcorrplot)
library(data.table)
source("./markdown/analysis/helper_functions.R")

# reading data
erp_main_folder <- "markdown/analysis/erp/kathrin_exp23/"
tasks <- c("flanker", "nback", "switching")
groups <- c("young", "old")
filters <- paste0(c(8, 16, 32), "hz")
possible_subfolders = c()

for (task in tasks){
  for (group in groups){
    for (filter in filters){
      possible_subfolders = c(possible_subfolders, paste0(erp_main_folder,
                                                          task, "/",
                                                          group, "/",
                                                          filter, "/",
                                                          "erp/"))
    }
  }
}


get_latency_data <- function(possible_subfolders, file_pattern = ".csv"){
  latencies_exist = c()
  for (i in seq_along(possible_subfolders)){
    latencies_path = paste0(possible_subfolders[i], "latencies/")
    if (dir.exists(latencies_path)){
      latencies_exist = c(latencies_exist, latencies_path)
    }
  }

  main_latencies = vector(mode = "list", length = length(latencies_exist))

  for (i in seq_along(latencies_exist)){
    existing_files = list.files(
      latencies_exist[i],
      pattern = file_pattern,
      full.names = TRUE
    )
    main_latencies[[i]] = existing_files %>%
      map(data.table::fread)

    for (j in seq_along(main_latencies[[i]])){
      main_latencies[[i]][[j]]$filename = existing_files[j]
    }

    main_latencies[[i]] = data.table::rbindlist(main_latencies[[i]])
  }

  return(main_latencies)
}

main_data <- get_latency_data(possible_subfolders, file_pattern = "^exp23kathrin.*csv") %>%
  data.table::rbindlist() %>%
  as.data.frame()

main_data[main_data == "NaN"] = NA

data_unclean <- main_data

data <- data_unclean %>%
  rename(
    "erp_filepath" = filepath,
    "csv_filepath" = filename
  ) %>%
  # Get group, task and filter
  mutate(
    group = str_extract(erp_filepath, "young|old"),
    task = str_extract(erp_filepath, "flanker|switching|nback"),
    filter = parse_number(str_extract(erp_filepath, "\\d+hz"))
  ) %>%
  # Get Subject
  mutate(subject = parse_number(str_extract(erp_filepath, "[Flanker|Switching|Nback]\\d+"))) %>%
  # Smoothing Param
  mutate(smooth = parse_number(str_extract(csv_filepath, "smoothed_\\d{2}"))) %>%
  mutate(smooth = ifelse(is.na(smooth), 0, smooth)) %>%
  # Sub-GA size
  mutate(sub_ga = parse_number(str_extract(csv_filepath, "sub_\\d{2}"))) %>%
  mutate(sub_ga = ifelse(is.na(sub_ga), 0, sub_ga)) %>%
  # Get type
  mutate(
    type = case_when(
      str_detect(csv_filepath, "auto_peak") ~ "autopeak",
      str_detect(csv_filepath, "auto_area") ~ "autoarea",
      TRUE ~ "matched"
    )
  ) %>%
  # approach
  mutate(approach = case_when(
    str_detect(csv_filepath, "corr") & type == "matched" ~ "corr",
    str_detect(csv_filepath, "minsq") & type == "matched" ~ "minsq",
    str_detect(csv_filepath, "corr") & type != "matched" ~ "informedcorr" ,
    str_detect(csv_filepath, "minsq") & type != "matched" ~ "informedminsq",
    str_detect(csv_filepath, "uninformed") ~ "uninformed"
  )) %>%
  # Is just review method entries?
  mutate(is_method = ifelse(str_detect(csv_filepath, "review_method"), 1, 0)) %>%
  # Get review means
  mutate(csv_filepath = str_replace(csv_filepath, "auto_reviewed", "autoreviewed")) %>%
  mutate(
    review = case_when(
      !str_detect(csv_filepath, "review") ~ "none",
      str_detect(csv_filepath, "noreview") ~ "none",
      str_detect(csv_filepath, "autoreview") ~ "auto",
      str_detect(csv_filepath, "reviewed") ~ "manual",
      is_method == 1 ~ "method_entry"
    )
  ) %>%
  # Component
  mutate(
    component = case_when(
      str_detect(csv_filepath, "p3") ~ "p3",
      str_detect(csv_filepath, "n2") ~ "n2"
    )
  ) %>%
  # Window
  mutate(
    window = str_extract(csv_filepath, "wide|narrow|const")
  ) %>%
  group_by(task, group, filter, smooth, sub_ga, component, window, type, approach, review) %>%
  mutate(position = row_number()) %>%
  ungroup()

method_data <- data %>%
  filter(is_method == 1)
data <- data %>%
  filter(is_method == 0) %>%
  # Make latencies numeric
  mutate(
    across(contains("bin_"), as.numeric)
  )

data_long <- data %>%
  pivot_longer(
    cols = contains("bin"),
    names_to = "bin",
    values_to = "latency"
  ) %>%
  mutate(
    bin = parse_number(bin),
    condition = ifelse(bin %in% c(1, 2, 5), 1, 2)
  )

method_data_long <- method_data %>%
  pivot_longer(
    cols = contains("bin"),
    names_to = "bin",
    values_to = "origin"
  ) %>%
  mutate(
    bin = parse_number(bin),
    condition = ifelse(bin %in% c(1, 2, 5), 1, 2),
    review = "manual"
  ) %>%
  select(-is_method, -contains("filepath"), -prefix)

full_data_long <- data_long %>%
  left_join(., method_data_long) %>%
  group_by(group) %>%
  mutate(position = dense_rank(subject)) %>%
  mutate(subject = ifelse(group == "old", position + 30, position)) %>%
  ungroup() %>%
  mutate(origin = ifelse(is.na(origin), "auto", origin)) %>%
  select(
    task, filter, smooth, sub_ga, component, window, prefix,
    approach, type, review,
    group, subject, condition, bin,
    latency, origin,
    csv_filepath, erp_filepath
  )

# join rater data
load("./markdown/analysis/data/rater_data_long.rdata")

rater_data_join <- rater_data_long %>%
  mutate(
    smooth = 0,
    sub_ga = 0,
    component = "p3",
    window = "manualrater",
    prefix = "manualrater",
    subject = position,
    origin = NA
  ) %>%
  select(-position)


full_data_long$rater <- "sven"

join_list <- list(
  full_data_long,
  rater_data_join
)

data <- rbindlist(join_list, fill = TRUE)

save(data, file = "./markdown/analysis/data/full_analysis_data_long.rdata")
