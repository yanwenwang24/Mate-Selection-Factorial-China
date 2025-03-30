## ------------------------------------------------------------------------
##
## Script name: 01_exclude.r
## Purpose: Exclusion strategies
## Author: Yanwen Wang
## Date Created: 2025-03-21
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Check source ----------------------------------------------------------

# Only keep respondents from 调研家社区
drop_source <- data_behavior %>%
  filter(source != "调研家社区") %>%
  select(id) %>%
  pull()

# 1 Check response duration -----------------------------------------------

# Summary statistics
duration_summary <- data_behavior %>%
  summarise(
    vignette_mean = mean(vignette_duration, na.rm = TRUE),
    vignette_median = median(vignette_duration, na.rm = TRUE),
    vignette_sd = sd(vignette_duration, na.rm = TRUE),
    total_mean = mean(total_duration, na.rm = TRUE),
    total_median = median(total_duration, na.rm = TRUE),
    total_sd = sd(total_duration, na.rm = TRUE)
  )

print(duration_summary)

# Define duration threshold
vignette_duration_threshold <- 50
total_duration_threshold <- 100

# Exclude respondents with short response duration
drop_duration <- data_behavior %>%
  filter(
    vignette_duration < vignette_duration_threshold |
      total_duration < total_duration_threshold
  ) %>%
  select(id) %>%
  pull()

# 2 Check vignettes -------------------------------------------------------

# Check straight lining (all answers the same)
drop_straight_lining <- data_answer %>%
  group_by(id) %>%
  summarise(
    same_r_date = n_distinct(r_date) == 1,
    same_r_marriage = n_distinct(r_marriage) == 1,
    .groups = "drop"
  ) %>%
  filter(same_r_date | same_r_marriage) %>%
  select(id) %>%
  pull()

# Check duplicated vignettes per respondent
duplicated_vignettes <- data_answer %>%
  group_by(
    id,
    v_age,
    v_educ,
    v_income,
    v_nativity,
    v_appearance,
    v_housework,
    v_career,
    v_emotion,
    v_fertility
  ) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

# 3 Exclude these respondents ---------------------------------------------

sample_answer <- data_answer %>%
  filter(
    !id %in% drop_source,
    !id %in% drop_duration,
    !id %in% drop_straight_lining
  )

sample_behavior <- data_behavior %>%
  filter(
    !id %in% drop_source,
    !id %in% drop_duration,
    !id %in% drop_straight_lining
  )

# Number of respondents excluded
data_answer %>%
  filter(
    id %in% drop_source |
      id %in% drop_duration |
      id %in% drop_straight_lining
  ) %>%
  select(id) %>%
  distinct() %>%
  nrow()
