
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

message("Calculating R² for marriages...")

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

message("Calculating R² for dating...")

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
      term == "Fertility" ~ "Fertility",
      term == "Nativity" ~ "Nativity",
      term == "Career" ~ "Career support",
      term == "Age" ~ "Age"
    )
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
  theme_bw(base_family = "Roboto") +
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

message("✓ Importance plot saved\n")
