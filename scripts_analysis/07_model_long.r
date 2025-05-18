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
## Notes: Fit models for comparing marriage and dating
##
## ------------------------------------------------------------------------

# 1 Models ----------------------------------------------------------------

# 1.1 Define formula ------------------------------------------------------

message("Fitting models for comparing marriage and dating by gender...")

f <- as.formula(
  "rate ~ type * (v_age + v_educ + v_income + v_nativity +
    v_appearance + v_housework + v_career + v_emotion + v_fertility) +
    age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    order +
    (1 | id)"
)

# 1.2 Fit models -----------------------------------------------------------

mod_men <- lmer(
  f,
  data = sample_long %>%
    filter(gender == "men")
)

summ(mod_men)

mod_women <- lmer(
  f,
  data = sample_long %>%
    filter(gender == "women")
)

summ(mod_women)

# 1.3 Save models -----------------------------------------------------------

saveRDS(
  list(
    mod_men = mod_men,
    mod_women = mod_women
  ),
  file = "models/mods_long.rds"
)

message("✓ Models for comparing marriage and dating saved.\n")
