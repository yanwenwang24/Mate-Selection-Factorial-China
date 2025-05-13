##
## Script name: 01_importance.r
## Purpose: Visualize the relative importance of vignette dimensions
## Author: Yanwen Wang
## Date Created: 2025-03-30
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: Use the semi-partial R² (Unique variance explained) to measure the
##        importance of each vignette dimension.
##
## ------------------------------------------------------------------------

# 1 Calculate R² ----------------------------------------------------------

# 1.1 Marriage ------------------------------------------------------------

message("Calculating R² for marriages (pooled)...")

r2_marriage_full <- partR2(
  mod_marriage_pooled,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer
)

r2_marriage <- r2_marriage_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage"
  )

message("✓ R² for marriages calculated\n")

# 1.2 Dating --------------------------------------------------------------

message("Calculating R² for dating (pooled)...")

r2_dating_full <- partR2(
  mod_dating_pooled,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer
)

r2_dating <- r2_dating_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating"
  )

r2_df <- bind_rows(r2_marriage, r2_dating) %>%
  mutate(
    type = factor(type, levels = c("Marriage", "Dating"))
  ) %>%
  mutate(
    term = case_when(
      term == "Educ" ~ "Education",
      term == "Housework" ~ "Housework",
      term == "Emotion" ~ "Emotional support",
      term == "Appearance" ~ "Appearance",
      term == "Income" ~ "Income/career",
      term == "Fertility" ~ "Fertility intention",
      term == "Nativity" ~ "Nativity",
      term == "Career" ~ "Career support",
      term == "Age" ~ "Age"
    ),
    term = str_to_title(term)
  )

message("✓ R² for dating calculated\n")

# 2 Visualize -------------------------------------------------------------

p_importance <- ggplot(
  r2_df,
  aes(x = reorder_within(term, estimate, type), y = estimate)
) +
  geom_col(fill = "#1f78b4") +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~type, scales = "free_y") +
  theme_bw() +
  labs(
    x = NULL,
    y = "Semi-partial R² (Unique variance explained)"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    axis.text.y = element_markdown(size = 9),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Save
ggsave(
  "graphs/importance.png",
  p_importance,
  width = 8,
  height = 6,
  dpi = 300
)

message("✓ Importance (pooled) plot saved\n")

# 3 Gender-specific importance ------------------------------------------------

# 3.1 Marriage models by gender -----------------------------------------------

message("Calculating R² for marriages by gender...")

# Men's marriage preferences
r2_marriage_men_full <- partR2(
  mod_marriage_men,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_men
)

r2_marriage_men <- r2_marriage_men_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men"
  )

# Women's marriage preferences
r2_marriage_women_full <- partR2(
  mod_marriage_women,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_women,
)

r2_marriage_women <- r2_marriage_women_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Women"
  )

message("✓ R² for marriages by gender calculated\n")

# 3.2 Dating models by gender -------------------------------------------------

message("Calculating R² for dating by gender...")

# Men's dating preferences
r2_dating_men_full <- partR2(
  mod_dating_men,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_men,
)

r2_dating_men <- r2_dating_men_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Men"
  )

# Women's dating preferences
r2_dating_women_full <- partR2(
  mod_dating_women,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_women,
)

r2_dating_women <- r2_dating_women_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Women"
  )

message("✓ R² for dating by gender calculated\n")

# 3.3 Combine and clean --------------------------------------------------------

r2_gender_df <- bind_rows(
  r2_marriage_men,
  r2_marriage_women,
  r2_dating_men,
  r2_dating_women
) %>%
  mutate(
    type = factor(type, levels = c("Marriage", "Dating")),
    gender = factor(gender, levels = c("Men", "Women"))
  ) %>%
  mutate(
    term = case_when(
      term == "Educ" ~ "Education",
      term == "Housework" ~ "Housework",
      term == "Emotion" ~ "Emotional support",
      term == "Appearance" ~ "Appearance",
      term == "Income" ~ "Income/career",
      term == "Fertility" ~ "Fertility intention",
      term == "Nativity" ~ "Nativity",
      term == "Career" ~ "Career support",
      term == "Age" ~ "Age"
    ),
    term = str_to_title(term)
  )

# 3.4 Visualize by gender ------------------------------------------------------

# Create separate plot for each group
message("Creating plots by gender...")

# Calculate max value for consistent y-axis scaling
max_r2 <- max(r2_gender_df$estimate) * 1.05 # Add 5% padding

# Common theme settings
common_theme <- theme_bw() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text.y = element_markdown(size = 9),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Men's marriage preferences plot
p_men_marriage <- ggplot(
  filter(r2_gender_df, gender == "Men" & type == "Marriage"),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#fd7f6f") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Marriage - Men evaluating women",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Women's marriage preferences plot
p_women_marriage <- ggplot(
  filter(r2_gender_df, gender == "Women" & type == "Marriage"),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#7eb0d5") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Marriage - Women evaluating men",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Men's dating preferences plot
p_men_dating <- ggplot(
  filter(r2_gender_df, gender == "Men" & type == "Dating"),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#fd7f6f") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Dating - Men evaluating women",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Women's dating preferences plot
p_women_dating <- ggplot(
  filter(r2_gender_df, gender == "Women" & type == "Dating"),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#7eb0d5") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Dating - Women evaluating men",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Combine plots
p_importance_gender <- (
  (p_women_marriage | p_men_marriage) /
    (p_women_dating | p_men_dating)
) +
  plot_annotation(caption = "Semi-partial R² (Unique variance explained)") +
  plot_layout(guides = "collect") &
  theme(plot.caption = element_text(hjust = 0.5, size = 10))

# Save combined plot
ggsave(
  "graphs/importance_gender.png",
  p_importance_gender,
  width = 10,
  height = 10,
  dpi = 300
)

message("✓ Gender-specific importance plot saved\n")
