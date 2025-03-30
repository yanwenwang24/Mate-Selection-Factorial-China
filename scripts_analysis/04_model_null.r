## ------------------------------------------------------------------------
##
## Script name: 04_model_null.r
## Purpose: Fit null models
## Author: Yanwen Wang
## Date Created: 2025-03-21
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Null model ------------------------------------------------------------

# Marriage
mod_marriage_null <- lmer(
  r_marriage ~ 1 + (1 | id),
  data = sample_answer
)

# Dating
mod_date_null <- lmer(
  r_date ~ 1 + (1 | id),
  data = sample_answer
)

# Save models
saveRDS(
  list(
    mod_marriage_null = mod_marriage_null,
    mod_date_null = mod_date_null
  ),
  file = "models/mods_null.rds"
)

# Read models
mods_null <- readRDS("models/mods_null.rds")
mod_marriage_null <- mods_null$mod_marriage_null
mod_date_null <- mods_null$mod_date_null

# Output
summ(mod_marriage_null)
summ(mod_date_null)
