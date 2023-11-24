library(tidyverse)
source("./markdown/analysis/helper_functions.R")

load("./markdown/analysis/data/full_analysis_data_long.rdata")

data %>%
  mutate(combination = interaction(approach, type, review)) %>%
  filter(!(review == "none" & type == "matched"),
         !(approach == "individualmanual" & !type %in% c("area", "peak")),
         approach %in% c("corr", "minsq", "individualmanual", "uninformed")) %>%
  filter(bin %in% c(5, 6)) %>%
  ggplot(
    aes(
      x = combination,
      y = latency
    )
  )+
  geom_violin()+
  facet_wrap(~interaction(task, condition), ncol = 3)+
  labs(
    x = "method"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()

# Compare the means and standard deviations of each method within each group
distribution_data <- data %>%
  filter(!(review == "none" & type == "matched"),
         !(approach == "individualmanual" & !type %in% c("area", "peak")),
         approach %in% c("corr", "minsq", "individualmanual", "uninformed")) %>%
  filter(bin %in% c(5, 6)) %>%
  group_by(
    task, condition, group, approach, type, review
  ) %>%
  summarize(
    mean = mean(latency, na.rm = TRUE),
    median = median(latency, na.rm = TRUE),
    sd = sd(latency, na.rm = TRUE),
    iqr = IQR(latency, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(id = paste0(task, condition, group))

afex::aov_ez(
  id = "id",
  between = "task",
  within = c("approach"),
  dv = "sd",
  data = distribution_data
)


distribution_data %>%
  filter(review != "auto") %>%
  mutate(combination = interaction(approach, type, review)) %>%
  ggplot(
    aes(
      x = combination,
      y = sd
    )
  )+
  geom_boxplot()+
  facet_wrap(~task)+
  labs(
    x = "method"
  )+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme_classic()
