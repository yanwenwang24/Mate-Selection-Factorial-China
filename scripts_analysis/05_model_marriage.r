## ------------------------------------------------------------------------
##
## Script name: 05_model_marriage.r
## Purpose: Fit models for marriage
## Author: Yanwen Wang
## Date Created: 2025-03-21
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
## 1. Fit main models
## 2. Fit interaction models
##   - Gender
##   - Education
##
## ------------------------------------------------------------------------

# 1 Main models -----------------------------------------------------------

# 1.1 Define formula ------------------------------------------------------

f_main_1 <- as.formula(
  "r_marriage ~ v_age + v_educ + v_income + v_nativity + v_appearance +
    v_housework + v_career + v_emotion + v_fertility +
    (1 | id)"
)

f_main_2 <- as.formula(
  "r_marriage ~ v_age + v_educ + v_income + v_nativity + v_appearance +
    v_housework + v_career + v_emotion + v_fertility +
    gender + age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    (1 | id)"
)

# 1.2 Fit models -----------------------------------------------------------

# Fit model
mod_main_1 <- lmer(
  f_main_1,
  data = sample_answer
)

summ(mod_main_1)

mod_main_2 <- lmer(
  f_main_2,
  data = sample_answer
)

summ(mod_main_2)

# 1.3 Save models ---------------------------------------------------------

saveRDS(
  list(
    mod_main_1 = mod_main_1,
    mod_main_2 = mod_main_2
  ),
  file = "models/mods_marriage_main.rds"
)

# 2 Interaction models ----------------------------------------------------

# 2.1 Define formula ------------------------------------------------------

f_gender <- as.formula(
  "r_marriage ~ gender * (v_age + v_educ + v_income + v_nativity +
    v_appearance + v_housework + v_career + v_emotion + v_fertility) +
    gender + age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    (1 | id)"
)

f_educ <- as.formula(
  "r_marriage ~ educ * (v_age + v_educ + v_income + v_nativity +
    v_appearance + v_housework + v_career + v_emotion + v_fertility) +
    gender + age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    (1 | id)"
)
# 2.2 Fit models -----------------------------------------------------------

mod_gender <- lmer(
  f_gender,
  data = sample_answer
)

summ(mod_gender)

mod_educ <- lmer(
  f_educ,
  data = sample_answer
)

summ(mod_educ)

# 2.3 Save models ---------------------------------------------------------

saveRDS(
  list(
    mod_gender = mod_gender,
    mod_educ = mod_educ
  ),
  file = "models/mods_marriage_inter.rds"
)
