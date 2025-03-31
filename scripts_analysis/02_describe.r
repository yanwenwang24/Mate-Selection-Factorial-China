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
mean(sample_demo$age)
sd(sample_demo$age)

sample_demo %>%
  group_by(gender) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age   = sd(age, na.rm = TRUE)
  )

ggplot(sample_demo, aes(x = age, fill = gender)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
  labs(
    x = "Age",
    y = "Count",
    fill = "Gender"
  )

# City
tabyl(sample_demo, city) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, city, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Nativity
tabyl(sample_demo, nativity) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, nativity, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Romance
tabyl(sample_demo, romance) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, romance, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Cohabitation
tabyl(sample_demo, cohabit) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, cohabit, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Age of marriage
tabyl(sample_demo, agemar) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, agemar, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Hukou
tabyl(sample_demo, hukou) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, hukou, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Parental education
tabyl(sample_demo, peduc) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, peduc, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Education
tabyl(sample_demo, educ) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, educ, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Income
tabyl(sample_demo, income) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, income, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Employment (full-time) status
tabyl(sample_demo, fulltime) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, fulltime, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Siblings
tabyl(sample_demo, sibling) %>%
  adorn_pct_formatting(2)

tabyl(sample_demo, sibling, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

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
mean(sample_pv$housework_scale)
sd(sample_pv$housework_scale)

sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_housework = mean(housework_scale, na.rm = TRUE),
    sd_housework = sd(housework_scale, na.rm = TRUE)
  )

tabyl(sample_pv, housework_cat) %>%
  adorn_pct_formatting(2)

tabyl(sample_pv, housework_cat, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Career
mean(sample_pv$career_scale)
sd(sample_pv$career_scale)

sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_career = mean(career_scale, na.rm = TRUE),
    sd_career = sd(career_scale, na.rm = TRUE)
  )

tabyl(sample_pv, career_cat) %>%
  adorn_pct_formatting(2)

tabyl(sample_pv, career_cat, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Emotion
mean(sample_pv$emotion_scale)
sd(sample_pv$emotion_scale)

sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_emotion = mean(emotion_scale, na.rm = TRUE),
    sd_emotion = sd(emotion_scale, na.rm = TRUE)
  )

tabyl(sample_pv, emotion_cat) %>%
  adorn_pct_formatting(2)

tabyl(sample_pv, emotion_cat, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Fertility
tabyl(sample_pv, fertility) %>%
  adorn_pct_formatting(2)

tabyl(sample_pv, fertility, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Appearance
tabyl(sample_pv, appearance) %>%
  adorn_pct_formatting(2)

tabyl(sample_pv, appearance, gender) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(2)

# Media use
mean(sample_pv$media_time)
sd(sample_pv$media_time)

sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_time = mean(media_time, na.rm = TRUE),
    sd_media_time = sd(media_time, na.rm = TRUE)
  )

# Media comparison (income)
mean(sample_pv$media_compare_income)
sd(sample_pv$media_compare_income)

sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_compare_income = mean(media_compare_income, na.rm = TRUE),
    sd_media_compare_income = sd(media_compare_income, na.rm = TRUE)
  )

# Media comparison (appearance)
mean(sample_pv$media_compare_appearance)
sd(sample_pv$media_compare_appearance)

sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_compare_appearance = mean(
      media_compare_appearance,
      na.rm = TRUE
    ),
    sd_media_compare_appearance = sd(media_compare_appearance, na.rm = TRUE)
  )

# Media comparison (education)
mean(sample_pv$media_compare_educ)
sd(sample_pv$media_compare_educ)

sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_compare_educ = mean(media_compare_educ, na.rm = TRUE),
    sd_media_compare_educ = sd(media_compare_educ, na.rm = TRUE)
  )

# Media mobility (income)
mean(sample_pv$media_mobility_income)
sd(sample_pv$media_mobility_income)

sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_mobility_income = mean(media_mobility_income, na.rm = TRUE),
    sd_media_mobility_income = sd(media_mobility_income, na.rm = TRUE)
  )

# Media mobility (appearance)
mean(sample_pv$media_mobility_appearance)
sd(sample_pv$media_mobility_appearance)

sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_mobility_appearance = mean(
      media_mobility_appearance,
      na.rm = TRUE
    ),
    sd_media_mobility_appearance = sd(media_mobility_appearance, na.rm = TRUE)
  )

# Media mobility (education)
mean(sample_pv$media_mobility_educ)
sd(sample_pv$media_mobility_educ)

sample_pv %>%
  group_by(gender) %>%
  summarise(
    mean_media_mobility_educ = mean(media_mobility_educ, na.rm = TRUE),
    sd_media_mobility_educ = sd(media_mobility_educ, na.rm = TRUE)
  )

# 4 Vignettes -------------------------------------------------------------

# Response time
mean(sample_vignette$duration)
sd(sample_vignette$duration)

# Rating for dating
mean(sample_vignette$r_date)
sd(sample_vignette$r_date)

sample_vignette %>%
  group_by(gender) %>%
  summarise(
    mean_r_date = mean(r_date, na.rm = TRUE),
    sd_r_date = sd(r_date, na.rm = TRUE)
  )

# Rating for marriage
mean(sample_vignette$r_marriage)
sd(sample_vignette$r_marriage)

sample_vignette %>%
  group_by(gender) %>%
  summarise(
    mean_r_marriage = mean(r_marriage, na.rm = TRUE),
    sd_r_marriage = sd(r_marriage, na.rm = TRUE)
  )

# Visualize distributions for dating
p_r_date <- ggplot(sample_vignette, aes(x = r_date, fill = gender)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
  labs(
    x = "Rating for Dating",
    y = "Count",
    fill = "Gender"
  ) +
  theme(
    legend.position = "none"
  )

# Visualize distributions for marriage
p_r_marriage <- ggplot(sample_vignette, aes(x = r_marriage, fill = gender)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
  labs(
    x = "Rating for Marriage",
    y = "Count",
    fill = "Gender"
  )

# Combine plots
p_r_date + p_r_marriage
