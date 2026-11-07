##
## Script name: 04_educ.r
## Purpose: Visualize the education interaction terms of models for marriage
##          and dating, separately.
## Author: Yanwen Wang
## Date Created: 2025-03-31
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: Coefficient and CIs are extracted from education interaction terms
##        of models for marriage and dating separately. Forest plot is used for
##        visualization.
##
## ------------------------------------------------------------------------

# 1 Set levels ------------------------------------------------------------

message("Plotting education-specific effects...")

# Define vignette variables
vignette_dims <- c(
  "age",
  "education",
  "income/career",
  "local-origin",
  "appearance",
  "housework",
  "career support",
  "emotional support",
  "fertility intention"
)

# Reference levels for each vignette dimension
ref_levels <- tibble(
  dim = str_to_title(vignette_dims),
  level = c(
    "similar", # Age
    "secondary", # Education
    "average", # Income
    "local", # Local-origin
    "average", # Appearance
    "equal", # Housework
    "moderate", # Career
    "moderate", # Emotion
    "uncertain" # Fertility
  )
) %>%
  mutate(
    level = paste0("**", dim, " (ref: ", level, ")**"), # Bold
    row_num = row_number() # For ordering
  )

# 2 Prepare marriage data -------------------------------------------------

# 2.1 Men -----------------------------------------------------------------

# 2.1.1 High school -------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_men_educ_1_df <- tidy(
  mod_marriage_men_educ_1,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_men_educ_1_df <- bind_rows(
  coef_marriage_men_educ_1_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men",
    educ = "High school",
  )

# Custom labels
custom_labels <- forest_marriage_men_educ_1_df$level

# 2.1.2 Some college ------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_men_educ_2_df <- tidy(
  mod_marriage_men_educ_2,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_men_educ_2_df <- bind_rows(
  coef_marriage_men_educ_2_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men",
    educ = "Some college",
  )

# 2.1.3 College -----------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_men_educ_3_df <- tidy(
  mod_marriage_men_educ_3,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_men_educ_3_df <- bind_rows(
  coef_marriage_men_educ_3_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men",
    educ = "College",
  )

# 2.1.4 Graduate ----------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_men_educ_4_df <- tidy(
  mod_marriage_men_educ_4,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_men_educ_4_df <- bind_rows(
  coef_marriage_men_educ_4_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men",
    educ = "Graduate",
  )

# 2.2 Women ---------------------------------------------------------------

# 2.2.1 High school -------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_women_educ_1_df <- tidy(
  mod_marriage_women_educ_1,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_women_educ_1_df <- bind_rows(
  coef_marriage_women_educ_1_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Women",
    educ = "High school",
  )

# 2.2.2 Some college ------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_women_educ_2_df <- tidy(
  mod_marriage_women_educ_2,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_women_educ_2_df <- bind_rows(
  coef_marriage_women_educ_2_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Women",
    educ = "Some college",
  )

# 2.2.3 College -----------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_women_educ_3_df <- tidy(
  mod_marriage_women_educ_3,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_women_educ_3_df <- bind_rows(
  coef_marriage_women_educ_3_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Women",
    educ = "College",
  )

# 2.2.4 Graduate ----------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_women_educ_4_df <- tidy(
  mod_marriage_women_educ_4,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_women_educ_4_df <- bind_rows(
  coef_marriage_women_educ_4_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Women",
    educ = "Graduate",
  )

# 3 Prepare dating data ---------------------------------------------------

# 3.1 Men -----------------------------------------------------------------

# 3.1.1 High school -------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_men_educ_1_df <- tidy(
  mod_dating_men_educ_1,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_men_educ_1_df <- bind_rows(
  coef_dating_men_educ_1_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Dating",
    gender = "Men",
    educ = "High school",
  )

# Custom labels
custom_labels <- forest_dating_men_educ_1_df$level

# 3.1.2 Some college ------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_men_educ_2_df <- tidy(
  mod_dating_men_educ_2,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_men_educ_2_df <- bind_rows(
  coef_dating_men_educ_2_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Dating",
    gender = "Men",
    educ = "Some college",
  )

# 3.1.3 College -----------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_men_educ_3_df <- tidy(
  mod_dating_men_educ_3,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_men_educ_3_df <- bind_rows(
  coef_dating_men_educ_3_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Dating",
    gender = "Men",
    educ = "College",
  )

# 3.1.4 Graduate ----------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_men_educ_4_df <- tidy(
  mod_dating_men_educ_4,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_men_educ_4_df <- bind_rows(
  coef_dating_men_educ_4_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Dating",
    gender = "Men",
    educ = "Graduate",
  )

# 3.2 Women ---------------------------------------------------------------

# 3.2.1 High school -------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_women_educ_1_df <- tidy(
  mod_dating_women_educ_1,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_women_educ_1_df <- bind_rows(
  coef_dating_women_educ_1_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Dating",
    gender = "Women",
    educ = "High school",
  )

# 3.2.2 Some college ------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_women_educ_2_df <- tidy(
  mod_dating_women_educ_2,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_women_educ_2_df <- bind_rows(
  coef_dating_women_educ_2_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Dating",
    gender = "Women",
    educ = "Some college",
  )

# 3.2.3 College -----------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_women_educ_3_df <- tidy(
  mod_dating_women_educ_3,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_women_educ_3_df <- bind_rows(
  coef_dating_women_educ_3_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Dating",
    gender = "Women",
    educ = "College",
  )

# 3.2.4 Graduate ----------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_women_educ_4_df <- tidy(
  mod_dating_women_educ_4,
  conf.int = TRUE,
  effects = "fixed"
) %>%
  # Select vignette dimensions
  filter(grepl("^v_", term)) %>%
  # Extract dimensions and levels
  mutate(
    dim = case_when(
      grepl("^v_age", term) ~ "age",
      grepl("^v_educ", term) ~ "education",
      grepl("^v_income", term) ~ "income/career",
      grepl("^v_nativity", term) ~ "local-origin",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career support",
      grepl("^v_emotion", term) ~ "emotional support",
      grepl("^v_fertility", term) ~ "fertility intention",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income/career" ~ sub("^v_income", "", term),
      dim == "local-origin" ~ sub("^v_nativitymigrant$", "non-local", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career support" ~ sub("^v_career", "", term),
      dim == "emotional support" ~ sub("^v_emotion", "", term),
      dim == "fertility intention" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_women_educ_4_df <- bind_rows(
  coef_dating_women_educ_4_df,
  ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num) # For ordering
  ) %>%
  mutate(
    type = "Dating",
    gender = "Women",
    educ = "Graduate",
  )

# 4 Combine all data ------------------------------------------------------

# Combine marriage data
forest_educ_df <- bind_rows(
  forest_marriage_men_educ_1_df,
  forest_marriage_men_educ_2_df,
  forest_marriage_men_educ_3_df,
  forest_marriage_men_educ_4_df,
  forest_marriage_women_educ_1_df,
  forest_marriage_women_educ_2_df,
  forest_marriage_women_educ_3_df,
  forest_marriage_women_educ_4_df,
  forest_dating_men_educ_1_df,
  forest_dating_men_educ_2_df,
  forest_dating_men_educ_3_df,
  forest_dating_men_educ_4_df,
  forest_dating_women_educ_1_df,
  forest_dating_women_educ_2_df,
  forest_dating_women_educ_3_df,
  forest_dating_women_educ_4_df
) %>%
  mutate(
    educ = ifelse(is_ref, "ref", educ),
    educ = factor(
      educ,
      levels = c("ref", "High school", "Some college", "College", "Graduate")
    ),
    gender = factor(gender, levels = c("Women", "Men")),
    type = factor(type, levels = c("Marriage", "Dating")),
  )

# 5 Visualize -------------------------------------------------------------

p_educ <- ggplot(
  forest_educ_df,
  aes(
    x = estimate,
    y = fct_rev(y_pos),
    color = fct_rev(educ),
    group = fct_rev(educ)
  )
) +
  geom_point(size = 1, position = position_dodge) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high), # educ already in main aes
    position = position_dodge,
    height = 0.3
  ) +
  geom_vline(
    xintercept = 0, linetype = "dashed", color = "gray70"
  ) +
  scale_color_manual(
    values = c(
      "ref" = "white",
      "High school" = "#fd7f6f",
      "Some college" = "#7eb0d5",
      "College" = "#b2e061",
      "Graduate" = "#bd7ebe"
    ),
    breaks = c("High school", "Some college", "College", "Graduate"),
    labels = c("High school", "Some college", "College", "Graduate"),
    name = "Education"
  ) +
  scale_y_discrete(labels = rev(custom_labels)) +
  facet_grid(~ type + gender) +
  labs(
    x = "Attractivness rating",
    y = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.text.y = element_markdown(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(12, 20, 12, 10)
  )

# Save
ggsave(
  "outputs/figures/educ.pdf",
  p_educ,
  width = 8,
  height = 8
)

message("✓ Education-specific effects plotted\n")
