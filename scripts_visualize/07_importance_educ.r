## ------------------------------------------------------------------------
##
## Script name: 06_importance_educ.r
## Purpose: Relative importance by gender and education
## Author: Yanwen Wang
## Date Created: 2025-05-13
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Marriage --------------------------------------------------------------

# 1.1 Men -----------------------------------------------------------------

message("Calculating R² for marriages (men)...")

# High school or less
r2_marriage_men_educ_1_full <- partR2(
  mod_marriage_men_educ_1,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_men_educ_1
)

r2_marriage_men_educ_1 <- r2_marriage_men_educ_1_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men",
    educ = "High school or less"
  )

# Some college
r2_marriage_men_educ_2_full <- partR2(
  mod_marriage_men_educ_2,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_men_educ_2
)

r2_marriage_men_educ_2 <- r2_marriage_men_educ_2_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men",
    educ = "Some college"
  )

# College
r2_marriage_men_educ_3_full <- partR2(
  mod_marriage_men_educ_3,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_men_educ_3
)

r2_marriage_men_educ_3 <- r2_marriage_men_educ_3_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men",
    educ = "College"
  )

# Graduate
r2_marriage_men_educ_4_full <- partR2(
  mod_marriage_men_educ_4,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_men_educ_4
)

r2_marriage_men_educ_4 <- r2_marriage_men_educ_4_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Men",
    educ = "Graduate"
  )

message("✓ R² for marriages (men) calculated\n")

# 1.2 Women ---------------------------------------------------------------

message("Calculating R² for marriages (women)...")

# High school or less
r2_marriage_women_educ_1_full <- partR2(
  mod_marriage_women_educ_1,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_women_educ_1,
)

r2_marriage_women_educ_1 <- r2_marriage_women_educ_1_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Women",
    educ = "High school or less"
  )

# Some college
r2_marriage_women_educ_2_full <- partR2(
  mod_marriage_women_educ_2,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_women_educ_2,
)

r2_marriage_women_educ_2 <- r2_marriage_women_educ_2_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Women",
    educ = "Some college"
  )

# College
r2_marriage_women_educ_3_full <- partR2(
  mod_marriage_women_educ_3,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_women_educ_3,
)

r2_marriage_women_educ_3 <- r2_marriage_women_educ_3_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Women",
    educ = "College"
  )

# Graduate
r2_marriage_women_educ_4_full <- partR2(
  mod_marriage_women_educ_4,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_women_educ_4,
)

r2_marriage_women_educ_4 <- r2_marriage_women_educ_4_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Marriage",
    gender = "Women",
    educ = "Graduate"
  )

message("✓ R² for marriages (women) calculated\n")

# 2 Dating ----------------------------------------------------------------

# 2.1 Men -----------------------------------------------------------------

message("Calculating R² for dating (men)...")

# High school or less
r2_dating_men_educ_1_full <- partR2(
  mod_dating_men_educ_1,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_men_educ_1
)

r2_dating_men_educ_1 <- r2_dating_men_educ_1_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Men",
    educ = "High school or less"
  )

# Some college
r2_dating_men_educ_2_full <- partR2(
  mod_dating_men_educ_2,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_men_educ_2
)

r2_dating_men_educ_2 <- r2_dating_men_educ_2_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Men",
    educ = "Some college"
  )

# College
r2_dating_men_educ_3_full <- partR2(
  mod_dating_men_educ_3,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_men_educ_3
)

r2_dating_men_educ_3 <- r2_dating_men_educ_3_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Men",
    educ = "College"
  )

# Graduate
r2_dating_men_educ_4_full <- partR2(
  mod_dating_men_educ_4,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_men_educ_4
)

r2_dating_men_educ_4 <- r2_dating_men_educ_4_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Men",
    educ = "Graduate"
  )

message("✓ R² for dating (men) calculated\n")

# 2.2 Women ---------------------------------------------------------------

message("Calculating R² for dating (women)...")

# High school or less
r2_dating_women_educ_1_full <- partR2(
  mod_dating_women_educ_1,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_women_educ_1,
)

r2_dating_women_educ_1 <- r2_dating_women_educ_1_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Women",
    educ = "High school or less"
  )

# Some college
r2_dating_women_educ_2_full <- partR2(
  mod_dating_women_educ_2,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_women_educ_2,
)

r2_dating_women_educ_2 <- r2_dating_women_educ_2_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Women",
    educ = "Some college"
  )

# College
r2_dating_women_educ_3_full <- partR2(
  mod_dating_women_educ_3,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_women_educ_3,
)

r2_dating_women_educ_3 <- r2_dating_women_educ_3_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Women",
    educ = "College"
  )

# Graduate
r2_dating_women_educ_4_full <- partR2(
  mod_dating_women_educ_4,
  partvars = grep("^v_", colnames(sample_answer), value = TRUE),
  data = sample_answer_women_educ_4,
)

r2_dating_women_educ_4 <- r2_dating_women_educ_4_full$R2 %>%
  filter(!grepl("\\+", term)) %>% # Exclude interaction terms
  filter(term != "Full") %>% # Exclude overall model R²
  arrange(desc(estimate)) %>% # Sort by importance
  mutate(
    term = sub("^v_", "", term),
    term = str_to_title(term)
  ) %>%
  mutate(
    type = "Dating",
    gender = "Women",
    educ = "Graduate"
  )

message("✓ R² for dating (women) calculated\n")

# 3 Combine and clean --------------------------------------------------------

r2_gender_educ_df <- bind_rows(
  r2_marriage_men_educ_1,
  r2_marriage_men_educ_2,
  r2_marriage_men_educ_3,
  r2_marriage_men_educ_4,
  r2_marriage_women_educ_1,
  r2_marriage_women_educ_2,
  r2_marriage_women_educ_3,
  r2_marriage_women_educ_4,
  r2_dating_men_educ_1,
  r2_dating_men_educ_2,
  r2_dating_men_educ_3,
  r2_dating_men_educ_4,
  r2_dating_women_educ_1,
  r2_dating_women_educ_2,
  r2_dating_women_educ_3,
  r2_dating_women_educ_4,
) %>%
  mutate(
    type = factor(type, levels = c("Marriage", "Dating")),
    gender = factor(gender, levels = c("Women", "Men")),
    educ = factor(educ, levels = c(
      "High school or less",
      "Some college",
      "College",
      "Graduate"
    )),
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

# 4 Visualize -------------------------------------------------------------

# Create separate plot for each group
message("Creating plots by gender and education...")

# Calculate max value for consistent y-axis scaling
max_r2 <- max(r2_gender_educ_df$estimate) * 1.05 # Add 5% padding

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

# 4.1 Marriage ------------------------------------------------------------

# 4.1.1 Men ---------------------------------------------------------------

# High school or less
p_marriage_men_educ_1 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Marriage",
    gender == "Men",
    educ == "High school or less"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#fd7f6f") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Marriage - High school men evaluating women",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Some college
p_marriage_men_educ_2 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Marriage",
    gender == "Men",
    educ == "Some college"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#fd7f6f") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Marriage - Some college men evaluating women",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# College
p_marriage_men_educ_3 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Marriage",
    gender == "Men",
    educ == "College"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#fd7f6f") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Marriage - College men evaluating women",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Graduate
p_marriage_men_educ_4 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Marriage",
    gender == "Men",
    educ == "Graduate"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#fd7f6f") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Marriage - Graduate men evaluating women",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# 4.1.2 Women -------------------------------------------------------------

# High school or less
p_marriage_women_educ_1 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Marriage",
    gender == "Women",
    educ == "High school or less"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#7eb0d5") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Marriage - High school women evaluating men",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Some college
p_marriage_women_educ_2 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Marriage",
    gender == "Women",
    educ == "Some college"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#7eb0d5") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Marriage - Some college women evaluating men",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# College
p_marriage_women_educ_3 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Marriage",
    gender == "Women",
    educ == "College"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#7eb0d5") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Marriage - College women evaluating men",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Graduate
p_marriage_women_educ_4 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Marriage",
    gender == "Women",
    educ == "Graduate"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#7eb0d5") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Marriage - Graduate women evaluating men",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# 4.2 Dating --------------------------------------------------------------

# 4.1.1 Men ---------------------------------------------------------------

# High school or less
p_dating_men_educ_1 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Dating",
    gender == "Men",
    educ == "High school or less"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#fd7f6f") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Dating - High school men evaluating women",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Some college
p_dating_men_educ_2 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Dating",
    gender == "Men",
    educ == "Some college"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#fd7f6f") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Dating - Some college men evaluating women",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# College
p_dating_men_educ_3 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Dating",
    gender == "Men",
    educ == "College"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#fd7f6f") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Dating - College men evaluating women",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Graduate
p_dating_men_educ_4 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Dating",
    gender == "Men",
    educ == "Graduate"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#fd7f6f") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Dating - Graduate men evaluating women",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# 4.1.2 Women -------------------------------------------------------------

# High school or less
p_dating_women_educ_1 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Dating",
    gender == "Women",
    educ == "High school or less"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#7eb0d5") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Dating - High school women evaluating men",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Some college
p_dating_women_educ_2 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Dating",
    gender == "Women",
    educ == "Some college"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#7eb0d5") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Dating - Some college women evaluating men",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# College
p_dating_women_educ_3 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Dating",
    gender == "Women",
    educ == "College"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#7eb0d5") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Dating - College women evaluating men",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# Graduate
p_dating_women_educ_4 <- ggplot(
  filter(
    r2_gender_educ_df,
    type == "Dating",
    gender == "Women",
    educ == "Graduate"
  ),
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#7eb0d5") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max_r2)) +
  labs(
    title = "Dating - Graduate women evaluating men",
    x = NULL,
    y = NULL # Will add a common y-axis label with patchwork
  ) +
  common_theme

# 4.3 Combine plots -------------------------------------------------------

p_importance_marriage <- (
  (p_marriage_women_educ_1 | p_marriage_men_educ_1) /
    (p_marriage_women_educ_2 | p_marriage_men_educ_2) /
    (p_marriage_women_educ_3 | p_marriage_men_educ_3) /
    (p_marriage_women_educ_4 | p_marriage_men_educ_4)
) +
  plot_annotation(caption = "Semi-partial R² (Unique variance explained)") +
  plot_layout(guides = "collect") &
  theme(plot.caption = element_text(hjust = 0.5, size = 10))

p_importance_dating <- (
  (p_dating_women_educ_1 | p_dating_men_educ_1) /
    (p_dating_women_educ_2 | p_dating_men_educ_2) /
    (p_dating_women_educ_3 | p_dating_men_educ_3) /
    (p_dating_women_educ_4 | p_dating_men_educ_4)
) +
  plot_annotation(caption = "Semi-partial R² (Unique variance explained)") +
  plot_layout(guides = "collect") &
  theme(plot.caption = element_text(hjust = 0.5, size = 10))

# Save combined plot
ggsave(
  "graphs/importance_marriage.png",
  p_importance_marriage,
  width = 10,
  height = 10,
  dpi = 300
)

ggsave(
  "graphs/importance_dating.png",
  p_importance_dating,
  width = 10,
  height = 10,
  dpi = 300
)

message("✓ Gender-education-specific importance plot saved\n")
