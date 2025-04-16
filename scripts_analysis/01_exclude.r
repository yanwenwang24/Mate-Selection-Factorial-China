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
## Exclusion strategies
## 1. Exclude respondents from non-survey sources
## 2. Exclude respondents with response duration (vignette < 50s, total < 100s)
## 3. Exclude respondents with straight lining
## 4. Exclude respondents with duplicated vignettes
##
## Prepare samples
## 1. Sample answer
## 2. Sample behavior
## 3. Sample vignette
## 4. Sample long format (for comparing marriage and dating)
##
## ------------------------------------------------------------------------

# 1 Check source ----------------------------------------------------------

message("Selecting samples...")

# Only keep respondents from 调研家社区
drop_source <- data_behavior %>%
  filter(source != "调研家社区") %>%
  select(id) %>%
  pull()

# 2 Check response duration -----------------------------------------------

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

# 3 Check vignettes -------------------------------------------------------

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

# 4 Exclude these respondents ---------------------------------------------

# Drop source first
data_answer <- data_answer %>%
  filter(
    !id %in% drop_source
  )

data_behavior <- data_behavior %>%
  filter(
    !id %in% drop_source
  )

# Number of respondents excluded (after dropping source)
n_dropped <- data_answer %>%
  filter(
    id %in% drop_duration |
      id %in% drop_straight_lining
  ) %>%
  select(id) %>%
  distinct() %>%
  nrow()

message(sprintf("Dropped %d respondents", n_dropped))

# Make samples ------------------------------------------------------------

# Sample answer (for modeling)
sample_answer <- data_answer %>%
  filter(
    !id %in% drop_duration,
    !id %in% drop_straight_lining
  )

# Transform variables
sample_answer <- sample_answer %>%
  mutate(
    age_scale = scale(age, center = TRUE, scale = TRUE)
  ) %>%
  mutate(
    agemar = case_when(
      agemar == "27 and below" ~ "27 and below",
      agemar == "28-30" ~ "28-30",
      agemar == "31-35" ~ "31-35",
      agemar == "36-40" ~ "36 and above",
      agemar == "above 40" ~ "36 and above",
      TRUE ~ "against marriage"
    ),
    agemar = factor(agemar, levels = c(
      "27 and below",
      "28-30",
      "31-35",
      "36 and above",
      "against marriage"
    ))
  ) %>%
  mutate(
    peduc = case_when(
      peduc == "primary or less" ~ "primary or less",
      peduc == "middle school" ~ "middle school",
      peduc == "high school" ~ "high school",
      peduc == "some college" ~ "some college",
      peduc == "college" ~ "college and above",
      peduc == "graduate" ~ "college and above"
    ),
    peduc = factor(peduc, levels = c(
      "primary or less",
      "middle school",
      "high school",
      "some college",
      "college and above"
    ))
  ) %>%
  mutate(
    fulltime = case_when(
      empstat == "full-time" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    income = case_when(
      income == "below 3000" ~ "below 5000",
      income == "3001-5000" ~ "below 5000",
      income == "5001-8000" ~ "5000-8000",
      income == "8001-15000" ~ "8000-15000",
      income == "15001-25000" ~ "above 15000",
      income == "above 25000" ~ "above 15000"
    ),
    income = factor(income, levels = c(
      "below 5000",
      "5000-8000",
      "8000-15000",
      "above 15000"
    ))
  )

# Sample behavior
sample_behavior <- data_behavior %>%
  filter(
    !id %in% drop_duration,
    !id %in% drop_straight_lining
  )

# Sample vignette
sample_vignette <- sample_answer %>%
  select(
    id,
    gender,
    starts_with("v"),
    starts_with("r")
  )

response_time_df <- sample_behavior %>%
  pivot_longer(
    cols = matches("^v\\d+_duration$"),
    names_to = "vignette",
    values_to = "duration"
  ) %>%
  mutate(vignette = str_extract(vignette, "\\d+")) %>%
  select(id, vignette, duration)

sample_vignette <- merge(
  sample_vignette, response_time_df,
  by = c("id", "vignette")
)

# Transform to long format (for comparing marriage and dating)
sample_long <- sample_answer %>%
  pivot_longer(
    cols = c(r_marriage, r_date),
    names_to = "type",
    values_to = "rate"
  ) %>%
  mutate(
    type = sub("r_", "", type),
    type = factor(type, levels = c("marriage", "date"))
  )

# Save samples
saveRDS(
  list(
    sample_answer = sample_answer,
    sample_behavior = sample_behavior,
    sample_vignette = sample_vignette,
    sample_long = sample_long
  ),
  file = "data_clean/samples.rds"
)

message("✓ Samples saved.\n")
