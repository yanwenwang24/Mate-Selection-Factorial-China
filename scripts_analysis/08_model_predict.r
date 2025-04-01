##
## Script name: 08_model_predict.r
## Purpose: Predict the rating of marriage and dating based on respondents'
##          own characteristics
## Author: Yanwen Wang
## Date Created: 2025-04-01
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: Use post-vignette assessments data to re-fit models and predict the
##        rating of marriage and dating, respectively.
##
## ------------------------------------------------------------------------

# 1 Train models ----------------------------------------------------------

# Define formula
f_marriage <- as.formula(
  "r_marriage ~ v_age + v_educ + v_income + v_nativity + v_appearance +
    v_housework + v_career + v_emotion + v_fertility +
    (1 | id)"
)

f_dating <- as.formula(
  "r_date ~ v_age + v_educ + v_income + v_nativity + v_appearance +
    v_housework + v_career + v_emotion + v_fertility +
    (1 | id)"
)

# Train model (men evaluating fictional women)
mod_marriage_men <- lmer(
  f_marriage,
  data = sample_answer %>% filter(gender == "men")
)

mod_dating_men <- lmer(
  f_dating,
  data = sample_answer %>% filter(gender == "men")
)

# Train model (women evaluating fictional men)
mod_marriage_women <- lmer(
  f_marriage,
  data = sample_answer %>% filter(gender == "women")
)

mod_dating_women <- lmer(
  f_dating,
  data = sample_answer %>% filter(gender == "women")
)

# 2 Create new data -------------------------------------------------------

# Create new data
new_df <- sample_answer %>%
  select(
    id,
    gender,
    educ,
    income,
    nativity,
    appearance,
    housework_cat,
    career_cat,
    emotion_cat,
    fertility
  ) %>%
  distinct() %>%
  mutate(
    v_age = "similar",
    v_educ = case_when(
      educ == "high school or less" ~ "secondary",
      educ == "some college" ~ "secondary",
      educ == "college" ~ "bachelor",
      educ == "graduate" ~ "graduate"
    ),
    v_income = case_when(
      income == "below 5000" ~ "below average",
      income == "5000-8000" ~ "below average",
      income == "8000-15000" ~ "average",
      income == "above 15000" ~ "above average"
    ),
    v_nativity = nativity,
    v_appearance = appearance,
    v_housework = housework_cat,
    v_career = career_cat,
    v_emotion = emotion_cat,
    v_fertility = fertility
  ) %>%
  select(id, gender, starts_with("v_"))

new_men_df <- new_df %>% filter(gender == "men")
new_women_df <- new_df %>% filter(gender == "women")

# 3 Predict ---------------------------------------------------------------

# Predicted attractiveness for real men
pred_marriage_men <- predict(
  mod_marriage_women, # Use women's model to predict men's attractiveness
  new_men_df,
  re.form = NA
)

pred_dating_men <- predict(
  mod_dating_women, # Use women's model to predict men's attractiveness
  new_men_df,
  re.form = NA
)

# Predicted attractiveness for real women
pred_marriage_women <- predict(
  mod_marriage_men, # Use men's model to predict women's attractiveness
  new_women_df,
  re.form = NA
)

pred_dating_women <- predict(
  mod_dating_men, # Use men's model to predict women's attractiveness
  new_women_df,
  re.form = NA
)

# Organize predicted values
pred_marriage_df <- bind_rows(
  tibble(
    gender = "men",
    pred = pred_marriage_men
  ),
  tibble(
    gender = "women",
    pred = pred_marriage_women
  )
)

pred_dating_df <- bind_rows(
  tibble(
    gender = "men",
    pred = pred_dating_men
  ),
  tibble(
    gender = "women",
    pred = pred_dating_women
  )
)

# 4 Visualize -------------------------------------------------------------

# Marriage
p_pred_marriage <- ggplot(
  pred_marriage_df, aes(x = gender, y = pred, fill = gender)
) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(
    title = "Marriage",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_fill_manual(
    values = c(
      "men" = "#fd7f6f",
      "women" = "#7eb0d5"
    )
  ) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Dating
p_pred_dating <- ggplot(
  pred_dating_df, aes(x = gender, y = pred, fill = gender)
) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(
    title = "Dating",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_fill_manual(
    values = c(
      "men" = "#fd7f6f",
      "women" = "#7eb0d5"
    )
  ) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

p_pred <- p_pred_marriage + p_pred_dating + plot_annotation(
  title = "Predicted rating of marriage and dating",
  subtitle = "Based on respondents' real characteristics"
)

# Save
ggsave(
  "graphs/predicted.png",
  p_pred,
  width = 10,
  height = 8,
  dpi = 300
)
