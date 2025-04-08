
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

# Define vignette variables
vignette_dims <- c(
  "age",
  "education",
  "income",
  "nativity",
  "appearance",
  "housework",
  "career",
  "emotion",
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

# 2 Prepare data ----------------------------------------------------------

# 2.1 Marriage ------------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_marriage_df <- tidy(
  mod_marriage_pooled,
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
      grepl("^v_income", term) ~ "income",
      grepl("^v_nativity", term) ~ "nativity",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career",
      grepl("^v_emotion", term) ~ "emotion",
      grepl("^v_fertility", term) ~ "fertility",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income" ~ sub("^v_income", "", term),
      dim == "nativity" ~ sub("^v_nativity", "", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career" ~ sub("^v_career", "", term),
      dim == "emotion" ~ sub("^v_emotion", "", term),
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
forest_marriage_df <- bind_rows(coef_marriage_df, ref_levels) %>%
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
  )

# Custom labels
custom_labels <- forest_marriage_df$level

# 2.2 Dating --------------------------------------------------------------

# Coefficients and CIs for vignette dimensions
coef_dating_df <- tidy(
  mod_dating_pooled,
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
      grepl("^v_income", term) ~ "income",
      grepl("^v_nativity", term) ~ "nativity",
      grepl("^v_appearance", term) ~ "appearance",
      grepl("^v_housework", term) ~ "housework",
      grepl("^v_career", term) ~ "career",
      grepl("^v_emotion", term) ~ "emotion",
      grepl("^v_fertility", term) ~ "fertility",
      TRUE ~ NA_character_
    )
  ) %>%
  # Extract level by removing variable prefix
  mutate(
    level = case_when(
      dim == "age" ~ sub("^v_age", "", term),
      dim == "education" ~ sub("^v_educ", "", term),
      dim == "income" ~ sub("^v_income", "", term),
      dim == "nativity" ~ sub("^v_nativity", "", term),
      dim == "appearance" ~ sub("^v_appearance", "", term),
      dim == "housework" ~ sub("^v_housework", "", term),
      dim == "career" ~ sub("^v_career", "", term),
      dim == "emotion" ~ sub("^v_emotion", "", term),
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
forest_dating_df <- bind_rows(coef_dating_df, ref_levels) %>%
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
  )

# Combine marriage and dating data
forest_df <- bind_rows(forest_marriage_df, forest_dating_df) %>%
  mutate(type = ifelse(is_ref, "ref", type))

# 3 Visualize --------------------------------------------------------------

# 3.1 Marriage -------------------------------------------------------------

p_marriage <- ggplot(
  forest_marriage_df,
  aes(x = estimate, y = fct_rev(y_pos), color = is_ref)
) +
  geom_point(size = 1) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.3
  ) +
  geom_vline(
    xintercept = 0, linetype = "dashed", color = "gray70"
  ) +
  scale_color_manual(
    values = c("TRUE" = "white", "FALSE" = "#1f78b4")
  ) +
  scale_y_discrete(labels = rev(custom_labels)) +
  labs(
    x = "Marriage rating",
    y = NULL
  ) +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.text.y = element_markdown(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    plot.margin = margin(12, 20, 12, 10)
  )

# Save
ggsave(
  "graphs/marriage_pooled.png",
  p_marriage,
  width = 8,
  height = 12,
  dpi = 300
)

# 3.2 Dating ---------------------------------------------------------------

p_dating <- ggplot(
  forest_dating_df,
  aes(x = estimate, y = fct_rev(y_pos), color = is_ref)
) +
  geom_point(size = 1) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.3
  ) +
  geom_vline(
    xintercept = 0, linetype = "dashed", color = "gray70"
  ) +
  scale_color_manual(
    values = c("TRUE" = "white", "FALSE" = "#1f78b4")
  ) +
  scale_y_discrete(labels = rev(custom_labels)) +
  labs(
    x = "Dating rating",
    y = NULL
  ) +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.text.y = element_markdown(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    plot.margin = margin(12, 20, 12, 10)
  )

# Save
ggsave(
  "graphs/dating_pooled.png",
  p_dating,
  width = 8,
  height = 12,
  dpi = 300
)

# 3.3 Combined --------------------------------------------------------------

p_combined <- ggplot(
  forest_df,
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
    name = "Type"
  ) +
  scale_y_discrete(labels = rev(custom_labels)) +
  labs(
    x = "Rating",
    y = NULL
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.text.y = element_markdown(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    plot.margin = margin(12, 20, 12, 10)
  )

# Save
ggsave(
  "graphs/compare_pooled.png",
  p_combined,
  width = 8,
  height = 12,
  dpi = 300
)
