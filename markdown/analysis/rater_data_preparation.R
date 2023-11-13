# Library calls
library(tidyverse)
library(ggcorrplot)
library(data.table)
source("./markdown/analysis/helper_functions.R")

# Reading Rater data
rater_main_folder <- "./markdown/analysis/manual_extraction_data/kathrin_exp23"
extraction_methods <- c("Individual", "Jackknife")
pick_approaches <- c("Area_picking", "Peak_picking", "Area_automated", "Peak_automated")
tasks <- c("Flanker", "Nback", "switching")

possible_subfolders <- c()

for (extraction_method in extraction_methods){
  for (pick_approach in pick_approaches){
    for (task in tasks){
      possible_subfolders = c(possible_subfolders, paste0(rater_main_folder, "/",
                                                          extraction_method, "/",
                                                          pick_approach, "/",
                                                          task, "/"))
    }
  }
}


latencies_exist = c()
for (i in seq_along(possible_subfolders)){
  latencies_path = paste0(possible_subfolders[i])
  if (dir.exists(latencies_path)){
    latencies_exist = c(latencies_exist, latencies_path)
  }
}

main_latencies = vector(mode = "list", length = length(latencies_exist))

for (i in seq_along(latencies_exist)){
  existing_files = list.files(
    latencies_exist[i],
    pattern = ".xlsx$",
    full.names = TRUE
  )
  main_latencies[[i]] = existing_files %>%
    map(readxl::read_xlsx)

  for (j in seq_along(main_latencies[[i]])){
    main_latencies[[i]][[j]]$filename = existing_files[j]
  }

  main_latencies[[i]] = data.table::rbindlist(main_latencies[[i]])
}

rater_data = main_latencies %>%
  rbindlist() %>%
  as.data.frame()

clean_rater_data = rater_data %>%
  mutate(filename = str_remove(filename, "^\\./")) %>%
  mutate(filename = str_remove(filename, ".*kathrin_exp23/")) %>%
  separate(
    filename,
    into = c("approach", "type", "task", "file"),
    sep = "/"
  ) %>%
  mutate(
    rater = "kathrin",
    type = case_when(
      type == "Area_automated" ~ "autoarea",
      type == "Area_picking" ~ "area",
      type == "Peak_automated" ~ "autopeak",
      type == "Peak_picking" ~ "peak"
    ),
    review = case_when(
      type == "peak" | type == "area" ~ "manual",
      TRUE ~ "none"
    ),
    task = tolower(task),
    filter = parse_number(file)
  ) %>%
  select(-contains("remove")) %>%
  rename(
    "position" = ID,
    "latency" = value
  ) %>%
  mutate(
    bin = ifelse(condition == 1, 5, 6),
    approach = paste0(tolower(approach), "manual"),
    latency = as.numeric(latency),
    position = ifelse(group == "old" & position <= 30, position + 30, position)
  )

rater_data_long <- clean_rater_data %>%
  select(
    task, filter, rater, approach, type, review,
    group, position, condition, bin,
    latency
  )

save(rater_data_long, file = "./markdown/analysis/data/rater_data_long.rdata")


