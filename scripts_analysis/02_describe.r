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
## 1. Sample size
## 2. Demographic information
## 3. Post-vignette information
## 4. Vignettes (dating and marriage)
##
## ------------------------------------------------------------------------

# 1 Sample size -----------------------------------------------------------

message("Describing samples...")

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
    fulltime,
    income,
    sibling,
    oldest
  ) %>%
  distinct()

# Gender
tabyl(sample_demo, gender) %>%
  adorn_pct_formatting(2)

# Age
sample_demo %>%
  group_by(gender) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age   = sd(age, na.rm = TRUE)
  )

t.test(age ~ gender, data = sample_demo)

# City
tabyl(sample_demo, city, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$city, sample_demo$gender)

# Nativity
tabyl(sample_demo, nativity, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$nativity, sample_demo$gender)

# Romance
tabyl(sample_demo, romance, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$romance, sample_demo$gender)

# Cohabitation
tabyl(sample_demo, cohabit, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$cohabit, sample_demo$gender)

# Age of marriage
tabyl(sample_demo, agemar, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$agemar, sample_demo$gender)

# Hukou
tabyl(sample_demo, hukou, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$hukou, sample_demo$gender)

# Parental education
tabyl(sample_demo, peduc, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$peduc, sample_demo$gender)

# Education
tabyl(sample_demo, educ) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, educ, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$educ, sample_demo$gender)

# Income
tabyl(sample_demo, income, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$income, sample_demo$gender)

# Employment (full-time) status
tabyl(sample_demo, fulltime, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$fulltime, sample_demo$gender)

# Siblings
tabyl(sample_demo, sibling, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_demo$sibling, sample_demo$gender)

# 3 Post-vignette information ---------------------------------------------

sample_pv <- sample_answer %>%
  select(
    id,
    gender,
    housework_scale,
    housework_cat,
    career_scale,
    career_cat,
    emotion_scale,
    emotion_cat,
    fertility,
    appearance,
    starts_with("media")
  ) %>%
  distinct()

# Housework
sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_housework = mean(housework_scale, na.rm = TRUE),
    sd_housework = sd(housework_scale, na.rm = TRUE)
  )

tabyl(sample_pv, housework_cat, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_pv$housework_cat, sample_pv$gender)

# Career
sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_career = mean(career_scale, na.rm = TRUE),
    sd_career = sd(career_scale, na.rm = TRUE)
  )

tabyl(sample_pv, career_cat, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_pv$career_cat, sample_pv$gender)

# Emotion
sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_emotion = mean(emotion_scale, na.rm = TRUE),
    sd_emotion = sd(emotion_scale, na.rm = TRUE)
  )

tabyl(sample_pv, emotion_cat, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_pv$emotion_cat, sample_pv$gender)

# Fertility
tabyl(sample_pv, fertility, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Appearance
tabyl(sample_pv, appearance, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

chisq.test(sample_pv$appearance, sample_pv$gender)

# Media use
sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_time = mean(media_time, na.rm = TRUE),
    sd_media_time = sd(media_time, na.rm = TRUE)
  )

t.test(media_time ~ gender, data = sample_pv)

# Media comparison (income)
sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_compare_income = mean(media_compare_income, na.rm = TRUE),
    sd_media_compare_income = sd(media_compare_income, na.rm = TRUE)
  )

t.test(media_compare_income ~ gender, data = sample_pv)

# Media comparison (appearance)
sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_compare_appearance = mean(
      media_compare_appearance,
      na.rm = TRUE
    ),
    sd_media_compare_appearance = sd(media_compare_appearance, na.rm = TRUE)
  )

t.test(media_compare_appearance ~ gender, data = sample_pv)

# Media comparison (education)
sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_compare_educ = mean(media_compare_educ, na.rm = TRUE),
    sd_media_compare_educ = sd(media_compare_educ, na.rm = TRUE)
  )

t.test(media_compare_educ ~ gender, data = sample_pv)

# Media mobility (income)
sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_mobility_income = mean(media_mobility_income, na.rm = TRUE),
    sd_media_mobility_income = sd(media_mobility_income, na.rm = TRUE)
  )

t.test(media_mobility_income ~ gender, data = sample_pv)

# Media mobility (appearance)
sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_mobility_appearance = mean(
      media_mobility_appearance,
      na.rm = TRUE
    ),
    sd_media_mobility_appearance = sd(media_mobility_appearance, na.rm = TRUE)
  )

t.test(media_mobility_appearance ~ gender, data = sample_pv)

# Media mobility (education)
sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_mobility_educ = mean(media_mobility_educ, na.rm = TRUE),
    sd_media_mobility_educ = sd(media_mobility_educ, na.rm = TRUE)
  )

t.test(media_mobility_educ ~ gender, data = sample_pv)

# 4 Vignettes -------------------------------------------------------------

# Response time
mean(sample_vignette$duration, na.rm = TRUE)
sd(sample_vignette$duration, na.rm = TRUE)

# Rating for dating
sample_vignette %>%
  group_by(gender) %>%
  summarise(
    mean_r_date = mean(r_date, na.rm = TRUE),
    sd_r_date = sd(r_date, na.rm = TRUE)
  )

t.test(r_date ~ gender, data = sample_vignette)

# Rating for marriage
sample_vignette %>%
  group_by(gender) %>%
  summarise(
    mean_r_marriage = mean(r_marriage, na.rm = TRUE),
    sd_r_marriage = sd(r_marriage, na.rm = TRUE)
  )

t.test(r_marriage ~ gender, data = sample_vignette)

message("✓ Data description completed\n")
