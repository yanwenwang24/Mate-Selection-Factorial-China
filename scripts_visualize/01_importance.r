
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

# 1.2 Dating --------------------------------------------------------------

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
  )

# 2 Visualize -------------------------------------------------------------

p_importance <- ggplot(
  r2_df,
  aes(x = reorder_within(term, estimate, type), y = estimate)
) +
  geom_col(fill = "#1f78b4") +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~type, scales = "free_y") +
  labs(
    x = NULL,
    y = "Semi-partial R² (Unique variance explained)"
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
  "graphs/importance.png",
  p_importance,
  width = 10,
  height = 8,
  dpi = 300
)
