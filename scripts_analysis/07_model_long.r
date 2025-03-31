## ------------------------------------------------------------------------
##
## Script name: 07_model_full.r
## Purpose: Fit models for comparing marriage and dating
## Author: Yanwen Wang
## Date Created: 2025-03-31
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Models ----------------------------------------------------------------

# 1.1 Define formula ------------------------------------------------------

f_type <- as.formula(
  "rate ~ type * (v_age + v_educ + v_income + v_nativity +
    v_appearance + v_housework + v_career + v_emotion + v_fertility) +
    gender + age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    (1 | id)"
)

f_gender <- as.formula(
  "rate ~ gender * type * (v_age + v_educ + v_income + v_nativity +
    v_appearance + v_housework + v_career + v_emotion + v_fertility) +
    gender + age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    (1 | id)"
)

# 1.2 Fit models -----------------------------------------------------------

mod_type <- lmer(
  f_type,
  data = sample_long
)

summ(mod_type)

mod_gender <- lmer(
  f_gender,
  data = sample_long
)

summ(mod_gender)

# 1.3 Save models -----------------------------------------------------------

saveRDS(
  list(
    mod_type = mod_type,
    mod_gender = mod_gender
  ),
  file = "models/mods_long.rds"
)
