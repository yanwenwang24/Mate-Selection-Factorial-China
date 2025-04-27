
##
## Script name: 03_gender.r
## Purpose: Visualize the gender difference in marriage and dating ratings
## Author: Yanwen Wang
## Date Created: 2025-03-30
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: Coefficient and CIs are extracted from gender interaction terms of
##        models for marriage and dating separately. Forest plot is used for
##        visualization.
##
## ------------------------------------------------------------------------

# 1 Set levels ------------------------------------------------------------

message("Plotting gender difference...")

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
  "fertility"
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

# 2 Prepare data -----------------------------------------------------------

# 2.1 Marriage -------------------------------------------------------------

coef_marriage_gender_df <- tidy(
  mod_marriage_pooled_gender,
  effects = "fixed",
  conf.int = TRUE
) %>%
  filter(str_detect(term, "^genderwomen:")) %>%
  mutate(term = str_remove(term, "^genderwomen:")) %>%
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
      grepl("^v_fertility", term) ~ "fertility",
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
      dim == "fertility" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_gender_df <- bind_rows(coef_marriage_gender_df, ref_levels) %>%
  mutate(type = "Marriage") %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num)
  )

# Custom labels
custom_labels <- forest_marriage_df$level

# 2.2 Dating ---------------------------------------------------------------

coef_dating_gender_df <- tidy(
  mod_dating_pooled_gender,
  effects = "fixed",
  conf.int = TRUE
) %>%
  filter(str_detect(term, "^genderwomen:")) %>%
  mutate(term = str_remove(term, "^genderwomen:")) %>%
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
      grepl("^v_fertility", term) ~ "fertility",
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
      dim == "fertility" ~ sub("^v_fertility", "", term),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dim = str_to_title(dim),
    row_num = row_number() + nrow(ref_levels) # For ordering
  ) %>%
  select(dim, level, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_gender_df <- bind_rows(coef_dating_gender_df, ref_levels) %>%
  mutate(type = "Dating") %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num) %>%
  mutate(row_num = row_number()) %>% # Set new row numbers
  mutate(
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num)
  )

# Combine data
forest_gender_df <- bind_rows(
  forest_marriage_gender_df, forest_dating_gender_df
) %>%
  mutate(type = ifelse(is_ref, "ref", type))

# 3 Visualize --------------------------------------------------------------

p_gender_diff <- ggplot(
  forest_gender_df,
  aes(x = estimate, y = fct_rev(y_pos), color = type, group = type)
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
      "Marriage" = "#fd7f6f",
      "Dating" = "#7eb0d5"
    ),
    breaks = c("Marriage", "Dating"),
    labels = c("Marriage", "Dating"),
    name = "Context"
  ) +
  scale_y_discrete(labels = rev(custom_labels)) +
  labs(
    x = "Gender difference in attractiveness rating (women - men)",
    y = NULL
  ) +
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
  "graphs/gender_diff.png",
  p_gender_diff,
  width = 8,
  height = 8,
  dpi = 300
)

message("✓ Gender difference plotted\n")
