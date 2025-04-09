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

# 2 Prepare data -----------------------------------------------------------

# 2.1 Marriage -------------------------------------------------------------

# 2.1.1 Men ----------------------------------------------------------------

coef_marriage_men_educ_df <- tidy(
  mod_marriage_men_educ,
  effects = "fixed",
  conf.int = TRUE
) %>%
  filter(str_detect(term, ":")) %>%
  mutate(
    educ = str_extract(term, "(?<=educ).*?(?=:)")
  ) %>%
  mutate(
    term = str_extract(term, "(?<=:).*")
  ) %>%
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
  select(dim, level, educ, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_men_educ_df <- bind_rows(
  coef_marriage_men_educ_df, ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num)

unique_labels <- forest_marriage_men_educ_df %>%
  distinct(dim, level) %>%
  mutate(row_num = row_number())

forest_marriage_men_educ_df <- forest_marriage_men_educ_df %>%
  select(-row_num) %>%
  left_join(unique_labels, by = c("dim", "level")) %>%
  mutate(
    educ = ifelse(is.na(educ), "ref", educ),
    educ = factor(
      educ,
      levels = c("ref", "some college", "college", "graduate")
    ),
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men"
  )

# 2.1.2 Women -------------------------------------------------------------

coef_marriage_women_educ_df <- tidy(
  mod_marriage_women_educ,
  effects = "fixed",
  conf.int = TRUE
) %>%
  filter(str_detect(term, ":")) %>%
  mutate(
    educ = str_extract(term, "(?<=educ).*?(?=:)")
  ) %>%
  mutate(
    term = str_extract(term, "(?<=:).*")
  ) %>%
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
  select(dim, level, educ, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_marriage_women_educ_df <- bind_rows(
  coef_marriage_women_educ_df, ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num)

forest_marriage_women_educ_df <- forest_marriage_women_educ_df %>%
  select(-row_num) %>%
  left_join(unique_labels, by = c("dim", "level")) %>%
  mutate(
    educ = ifelse(is.na(educ), "ref", educ),
    educ = factor(
      educ,
      levels = c("ref", "some college", "college", "graduate")
    ),
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Women"
  )

forest_marriage_educ_df <- bind_rows(
  forest_marriage_men_educ_df,
  forest_marriage_women_educ_df
)

# 2.2 Dating --------------------------------------------------------------

# 2.2.1 Men ----------------------------------------------------------------

coef_dating_men_educ_df <- tidy(
  mod_dating_men_educ,
  effects = "fixed",
  conf.int = TRUE
) %>%
  filter(str_detect(term, ":")) %>%
  mutate(
    educ = str_extract(term, "(?<=educ).*?(?=:)")
  ) %>%
  mutate(
    term = str_extract(term, "(?<=:).*")
  ) %>%
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
  select(dim, level, educ, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_men_educ_df <- bind_rows(
  coef_dating_men_educ_df, ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num)

forest_dating_men_educ_df <- forest_dating_men_educ_df %>%
  select(-row_num) %>%
  left_join(unique_labels, by = c("dim", "level")) %>%
  mutate(
    educ = ifelse(is.na(educ), "ref", educ),
    educ = factor(
      educ,
      levels = c("ref", "some college", "college", "graduate")
    ),
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men"
  )

# 2.1.2 Women -------------------------------------------------------------

coef_dating_women_educ_df <- tidy(
  mod_dating_women_educ,
  effects = "fixed",
  conf.int = TRUE
) %>%
  filter(str_detect(term, ":")) %>%
  mutate(
    educ = str_extract(term, "(?<=educ).*?(?=:)")
  ) %>%
  mutate(
    term = str_extract(term, "(?<=:).*")
  ) %>%
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
  select(dim, level, educ, estimate, std.error, conf.low, conf.high, row_num)

# Prepare data for forest plot
forest_dating_women_educ_df <- bind_rows(
  coef_dating_women_educ_df, ref_levels
) %>%
  mutate(dim = factor(dim, levels = str_to_title(vignette_dims))) %>%
  arrange(dim, row_num)

forest_dating_women_educ_df <- forest_dating_women_educ_df %>%
  select(-row_num) %>%
  left_join(unique_labels, by = c("dim", "level")) %>%
  mutate(
    educ = ifelse(is.na(educ), "ref", educ),
    educ = factor(
      educ,
      levels = c("ref", "some college", "college", "graduate")
    ),
    is_ref = is.na(estimate), # Identify reference levels
    estimate = ifelse(is.na(estimate), 0, estimate),
    conf.low = ifelse(is.na(conf.low), 0, conf.low),
    conf.high = ifelse(is.na(conf.high), 0, conf.high),
    y_pos = factor(row_num)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Women"
  )

forest_dating_educ_df <- bind_rows(
  forest_dating_men_educ_df,
  forest_dating_women_educ_df
)

# 3 Visualize -------------------------------------------------------------

# 3.1 Marriage ------------------------------------------------------------

p_marriage_educ <- ggplot(
  forest_marriage_educ_df,
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
      "some college" = "#fd7f6f",
      "college" = "#7eb0d5",
      "graduate" = "#b2e061"
    ),
    breaks = c("some college", "college", "graduate"),
    labels = c("Some college", "College", "Graduate"),
    name = "Education"
  ) +
  scale_y_discrete(labels = rev(unique_labels$level)) +
  facet_wrap(~gender) +
  labs(
    x = "Educational difference in marriage rating",
    y = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
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
  "graphs/marriage_educ.png",
  p_marriage_educ,
  width = 8,
  height = 12,
  dpi = 300
)

# 3.2 Dating --------------------------------------------------------------

p_dating_educ <- ggplot(
  forest_dating_educ_df,
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
      "some college" = "#fd7f6f",
      "college" = "#7eb0d5",
      "graduate" = "#b2e061"
    ),
    breaks = c("some college", "college", "graduate"),
    labels = c("Some college", "College", "Graduate"),
    name = "Education"
  ) +
  scale_y_discrete(labels = rev(unique_labels$level)) +
  facet_wrap(~gender) +
  labs(
    x = "Educational difference in dating rating",
    y = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
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
  "graphs/dating_educ.png",
  p_dating_educ,
  width = 8,
  height = 12,
  dpi = 300
)
