## ------------------------------------------------------------------------
##
## Script name: 06.1_model_dating_robust.r
## Purpose: Fit models for dating
## Author: Yanwen Wang
## Date Created: 2025-05-15
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
## Repeat models with two robustness exclusions:
## 1. Implausible vignette combinations (graduate-educated respondents whose
##    "younger" partner would be under 24).
## 2. Respondents who selected "against marriage" in agemar.
##
## ------------------------------------------------------------------------

# 1 Both genders ----------------------------------------------------------

id_vector_implausible <- sample_answer %>%
  filter(
    v_educ == "graduate",
    v_age == "younger"
  ) %>%
  filter(age - 5 < 24) %>%
  pull(id) %>%
  unique()

id_vector_agemar <- sample_answer %>%
  filter(agemar == "against marriage") %>%
  pull(id) %>%
  unique()

id_vector <- union(id_vector_implausible, id_vector_agemar)

# 1.1 Main models ---------------------------------------------------------

message("Fitting models for dating...")

# Define formula
f_pooled <- as.formula(
  "r_date ~ v_age + v_educ + v_income + v_nativity + v_appearance +
    v_housework + v_career + v_emotion + v_fertility +
    gender + age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    order +
    (1 | id)"
)

# Fit model
mod_pooled <- lmer(
  f_pooled,
  data = sample_answer %>%
    filter(!id %in% id_vector)
)

summ(mod_pooled)

# 1.2 Interaction models --------------------------------------------------

# Define formula
f_pooled_gender <- as.formula(
  "r_date ~ gender * (v_age + v_educ + v_income + v_nativity +
    v_appearance + v_housework + v_career + v_emotion + v_fertility) +
    gender + age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    order +
    (1 | id)"
)

f_pooled_educ <- as.formula(
  "r_date ~ educ * (v_age + v_educ + v_income + v_nativity +
    v_appearance + v_housework + v_career + v_emotion + v_fertility) +
    gender + age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    order +
    (1 | id)"
)

# Fit models
mod_pooled_gender <- lmer(
  f_pooled_gender,
  data = sample_answer %>%
    filter(!id %in% id_vector)
)

summ(mod_pooled_gender)

mod_pooled_educ <- lmer(
  f_pooled_educ,
  data = sample_answer %>%
    filter(!id %in% id_vector)
)

summ(mod_pooled_educ)

# 2 By gender -------------------------------------------------------------

# 2.1 Main models ---------------------------------------------------------

# Define formula
f <- as.formula(
  "r_date ~ v_age + v_educ + v_income + v_nativity + v_appearance +
    v_housework + v_career + v_emotion + v_fertility +
    age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    order +
    (1 | id)"
)

# Fit models
mod_men <- lmer(
  f,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "men")
)

mod_women <- lmer(
  f,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "women")
)

summ(mod_men, digits = 3)
summ(mod_women, digits = 3)

# 2.2 Interaction models --------------------------------------------------

f_educ <- as.formula(
  "r_date ~ educ * (v_age + v_educ + v_income + v_nativity +
    v_appearance + v_housework + v_career + v_emotion + v_fertility) +
    age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + educ + fulltime + income + sibling +
    order +
    (1 | id)"
)

# Fit models
mod_men_educ <- lmer(
  f_educ,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "men")
)

mod_women_educ <- lmer(
  f_educ,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "women")
)

summ(mod_men_educ)
summ(mod_women_educ)

# 3 By gender and education -----------------------------------------------

# Define formula
f <- as.formula(
  "r_date ~ v_age + v_educ + v_income + v_nativity + v_appearance +
    v_housework + v_career + v_emotion + v_fertility +
    age + city + nativity + romance + cohabit + agemar +
    hukou + peduc + fulltime + income + sibling +
    order +
    (1 | id)"
)

# Fit models
mod_men_educ_1 <- lmer(
  f,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "men") %>%
    filter(educ == "high school or less")
)

mod_men_educ_2 <- lmer(
  f,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "men") %>%
    filter(educ == "some college")
)

mod_men_educ_3 <- lmer(
  f,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "men") %>%
    filter(educ == "college")
)

mod_men_educ_4 <- lmer(
  f,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "men") %>%
    filter(educ == "graduate")
)

mod_women_educ_1 <- lmer(
  f,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "women") %>%
    filter(educ == "high school or less")
)

mod_women_educ_2 <- lmer(
  f,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "women") %>%
    filter(educ == "some college")
)

mod_women_educ_3 <- lmer(
  f,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "women") %>%
    filter(educ == "college")
)

mod_women_educ_4 <- lmer(
  f,
  data = sample_answer %>%
    filter(!id %in% id_vector) %>%
    filter(gender == "women") %>%
    filter(educ == "graduate")
)

# 3 Save models -----------------------------------------------------------

saveRDS(
  list(
    mod_pooled = mod_pooled,
    mod_pooled_gender = mod_pooled_gender,
    mod_pooled_educ = mod_pooled_educ,
    mod_men = mod_men,
    mod_women = mod_women,
    mod_men_educ = mod_men_educ,
    mod_women_educ = mod_women_educ,
    mod_men_educ_1 = mod_men_educ_1,
    mod_men_educ_2 = mod_men_educ_2,
    mod_men_educ_3 = mod_men_educ_3,
    mod_men_educ_4 = mod_men_educ_4,
    mod_women_educ_1 = mod_women_educ_1,
    mod_women_educ_2 = mod_women_educ_2,
    mod_women_educ_3 = mod_women_educ_3,
    mod_women_educ_4 = mod_women_educ_4
  ),
  file = "models/mods_dating_robust.rds"
)

message("✓ Models for dating saved.\n")
