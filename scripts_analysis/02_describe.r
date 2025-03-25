## ------------------------------------------------------------------------
##
## Script name: 02_describe.r
## Purpose: Describe the data
## Author: Yanwen Wang
## Date Created: 2025-03-21
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Sample size -----------------------------------------------------------

length(unique(sample_answer$id)) # Num of respondents
nrow(sample_answer) # Num of observations
nrow(sample_answer) / length(unique(sample_answer$id)) # Num of obs per resp

# 2 Demographic information -----------------------------------------------

sample_demo <- sample_answer %>%
  select(
    id,
    gender,
    age,
    city,
    nativity,
    romance,
    cohabit,
    agemar,
    hukou,
    peduc,
    educ,
    empstat,
    income,
    sibling
  ) %>%
  distinct()

# Gender
tabyl(sample_demo, gender)

# Age
mean(sample_demo$age)
sd(sample_demo$age)

# City
tabyl(sample_demo, city)

# Nativity
tabyl(sample_demo, nativity)

# Romance
tabyl(sample_demo, romance)

# Cohabitation
tabyl(sample_demo, cohabit)

# Age of marriage
tabyl(sample_demo, agemar)

# Education
tabyl(sample_demo, educ)

# Income
tabyl(sample_demo, income)

# Siblings
tabyl(sample_demo, sibling)

# 3 Post-vignette information ---------------------------------------------

sample_pv <- sample_answer %>%
  select(
    id,
    housework_scale,
    housework_cat,
    career_scale,
    career_cat,
    emotion_scale,
    emotion_cat,
    fertility,
    appearance
  ) %>%
  distinct()

# Housework
mean(sample_pv$housework_scale)
sd(sample_pv$housework_scale)
tabyl(sample_pv, housework_cat)

# Career
mean(sample_pv$career_scale)
sd(sample_pv$career_scale)
tabyl(sample_pv, career_cat)

# Emotion
mean(sample_pv$emotion_scale)
sd(sample_pv$emotion_scale)
tabyl(sample_pv, emotion_cat)

# Fertility
tabyl(sample_pv, fertility)

# Appearance
tabyl(sample_pv, appearance)

# 4 Vignettes -------------------------------------------------------------

sample_vignette <- sample_answer %>%
  select(
    id,
    starts_with("v"),
    starts_with("r")
  )

response_time_df <- sample_behavior %>%
  pivot_longer(
    cols = matches("^v\\d+_duration$"),
    names_to = "vignette",
    values_to = "duration"
  ) %>%
  mutate(vignette = str_extract(vignette, "\\d+")) %>%
  select(id, vignette, duration)

# Merge with sample_vignette
sample_vignette <- merge(
  sample_vignette, response_time_df,
  by = c("id", "vignette")
)

# Response time
mean(sample_vignette$duration)
sd(sample_vignette$duration)

# Rating for dating
mean(sample_vignette$r_date)
sd(sample_vignette$r_date)

# Rating for marriage
mean(sample_vignette$r_marriage)
sd(sample_vignette$r_marriage)
