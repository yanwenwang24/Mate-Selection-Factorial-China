
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

# 1 Marriage --------------------------------------------------------------

r2_marriage_full <- partR2(
  mod_marriage_main,
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
  )

p_marriage <- ggplot(
  r2_marriage,
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#1f78b4") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Marriage"
  )

# 2 Dating ----------------------------------------------------------------

r2_dating_full <- partR2(
  mod_dating_main,
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
  )

p_dating <- ggplot(
  r2_dating,
  aes(x = reorder(term, estimate), y = estimate)
) +
  geom_col(fill = "#1f78b4") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Dating"
  )

p_importance <- p_marriage + p_dating +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Relative importance of vignette dimensions",
    subtitle = "Semi-partial R² (Unique variance explained)"
  )

# Save
ggsave(
  "graphs/importance.png",
  p_importance,
  width = 10,
  height = 8,
  dpi = 300
)
