##
## Script name: 02_main.r
## Purpose: Visualize the main effects of vignette dimensions
## Author: Yanwen Wang
## Date Created: 2025-03-30
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: Coefficient and CIs are extracted from the models, for marriage and
##        dating separately. Forest plot is used for visualization.
##
## ------------------------------------------------------------------------

# 1 Set levels ------------------------------------------------------------

message("Plotting gender-specific effects...")

# Define vignette variables
vignette_dims <- c(
  "age",
  "education",
  "income/career",
  "nativity",
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
    "local", # Nativity
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

# 2 Prepare data ----------------------------------------------------------

# 2.1 Marriage ------------------------------------------------------------

# 2.1.1 Men ---------------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_men_df <- tidy(
  mod_marriage_men,
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
      grepl("^v_nativity", term) ~ "nativity",
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
      dim == "nativity" ~ sub("^v_nativity", "", term),
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
forest_marriage_men_df <- bind_rows(coef_marriage_men_df, ref_levels) %>%
  mutate(type = "Marriage") %>%
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
  mutate(gender = "men")

# Custom labels
custom_labels <- forest_marriage_men_df$level

# 2.1.2 Women -------------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_women_df <- tidy(
  mod_marriage_women,
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
      grepl("^v_nativity", term) ~ "nativity",
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
      dim == "nativity" ~ sub("^v_nativity", "", term),
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
forest_marriage_women_df <- bind_rows(coef_marriage_women_df, ref_levels) %>%
  mutate(type = "Marriage") %>%
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
  mutate(gender = "women")

# Combine men and women data
forest_marriage_df <- bind_rows(
  forest_marriage_men_df, forest_marriage_women_df
) %>%
  mutate(gender = ifelse(is_ref, "ref", gender))

# 2.2 Dating --------------------------------------------------------------

# 2.2.1 Men ---------------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_men_df <- tidy(
  mod_dating_men,
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
      grepl("^v_nativity", term) ~ "nativity",
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
      dim == "nativity" ~ sub("^v_nativity", "", term),
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
forest_dating_men_df <- bind_rows(coef_dating_men_df, ref_levels) %>%
  mutate(type = "Dating") %>%
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
  mutate(gender = "men")

# 2.2.2 Women -------------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_women_df <- tidy(
  mod_dating_women,
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
      grepl("^v_nativity", term) ~ "nativity",
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
      dim == "nativity" ~ sub("^v_nativity", "", term),
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
forest_dating_women_df <- bind_rows(coef_dating_women_df, ref_levels) %>%
  mutate(type = "Dating") %>%
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
  mutate(gender = "women")

# Combine men and women data
forest_dating_df <- bind_rows(
  forest_dating_men_df, forest_dating_women_df
) %>%
  mutate(gender = ifelse(is_ref, "ref", gender))

# Combine marriage and dating data
forest_df <- bind_rows(
  forest_marriage_df, forest_dating_df
) %>%
  mutate(type = factor(type, levels = c("Marriage", "Dating")))

# 3 Visualize --------------------------------------------------------------

p_gender <- ggplot(
  forest_df,
  aes(x = estimate, y = fct_rev(y_pos), color = gender, group = gender)
) +
  geom_point(size = 1, position = position_dodge) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high), # type already in main aes
    position = position_dodge,
    height = 0.3
  ) +
  geom_vline(
    xintercept = 0, linetype = "dashed", color = "gray70"
  ) +
  scale_color_manual(
    values = c(
      "ref" = "white",
      "men" = "#fd7f6f",
      "women" = "#7eb0d5"
    ),
    breaks = c("women", "men"),
    labels = c("Women", "Men"),
    name = "Gender"
  ) +
  scale_y_discrete(labels = rev(custom_labels)) +
  labs(
    x = "Attractiveness rating",
    y = NULL
  ) +
  facet_wrap(~type, scales = "free_x") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    axis.text.y = element_markdown(size = 9),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(12, 20, 12, 10)
  )

# Save
ggsave(
  "graphs/gender.png",
  p_gender,
  width = 8,
  height = 8,
  dpi = 300
)

message("✓ Gender-specific effects plotted\n")