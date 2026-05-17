## ------------------------------------------------------------------------
##
## Script name: 03_check.r
## Purpose: Check randomization
## Author: Yanwen Wang
## Date Created: 2025-03-21
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
## 1. Check randomization (order and pairwise independence)
## 2. Check order effect
## 3. Check age/education distribution by vignette dimension
##
## ------------------------------------------------------------------------

# 1 Check randomization ---------------------------------------------------

message("Checking randomization...")

# Select dimension columns
dim_cols <- grep("^v_", colnames(sample_vignette), value = TRUE)

# Initialize storage for randomization results
randomization_results <- list()
order_dimension_results <- list()

# Check independence between vignette order and dimension
for (dim in dim_cols) {
  # Remove NA values and ensure proper factor levels
  clean_data <- sample_vignette[
    !is.na(sample_vignette$order) & !is.na(sample_vignette[[dim]]),
  ]
  clean_data$order <- factor(clean_data$order)
  clean_data[[dim]] <- factor(clean_data[[dim]])

  # Create a contingency table between vignette order and current dimension
  tab <- table(clean_data$order, clean_data[[dim]])

  # Run chi-square test
  chi_test <- chisq.test(tab)

  # Store results
  order_dimension_results[[dim]] <- list(
    contingency_table = tab,
    chi_square_test = chi_test
  )
}

# Check pairwise independence between dimensions
dim_pairs <- combn(dim_cols, 2, simplify = FALSE)
flag_pairs <- list() # to store significant pairs

for (pair in dim_pairs) {
  dim1 <- pair[1]
  dim2 <- pair[2]

  # Remove NA values and ensure proper factor levels
  clean_data <- sample_vignette[
    !is.na(sample_vignette[[dim1]]) & !is.na(sample_vignette[[dim2]]),
  ]
  clean_data[[dim1]] <- factor(clean_data[[dim1]])
  clean_data[[dim2]] <- factor(clean_data[[dim2]])

  # Construct contingency table
  tab_pair <- table(clean_data[[dim1]], clean_data[[dim2]])

  # Perform chi-square test
  chi_test_pair <- chisq.test(tab_pair)

  # Store results
  pair_name <- paste(dim1, dim2, sep = "-")
  randomization_results[[pair_name]] <- list(
    contingency_table = tab_pair,
    chi_square_test = chi_test_pair
  )

  # Check for significant p-value
  if (chi_test_pair$p.value < 0.05) {
    flag_pairs[[pair_name]] <- chi_test_pair
  }
}

# Store summary statistics
randomization_summary <- list(
  total_pairs = length(dim_pairs),
  significant_pairs = length(flag_pairs),
  significant_pair_names = names(flag_pairs)
)

map(sample_vignette %>% select(starts_with("v_")), tabyl)

# 2 Check order -----------------------------------------------------------

# Create plots but don't display them
p_order_date <- ggplot(sample_vignette, aes(x = order, y = r_date)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Dating rating",
    x = "Vignette order",
    y = "Dating rating"
  )

p_order_marriage <- ggplot(sample_vignette, aes(x = order, y = r_marriage)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Marriage rating",
    x = "Vignette order",
    y = "Marriage rating"
  )

p_check_order <- p_order_date + p_order_marriage

# Save
ggsave(
  "graphs/check_order.pdf",
  p_check_order,
  width = 12,
  height = 8
)

# ANOVA to test if vignette order predicts r_date
anova_date <- aov(r_date ~ order, data = sample_vignette)
anova_date_summary <- summary(anova_date)

# ANOVA to test if vignette order predicts r_marriage
anova_marriage <- aov(r_marriage ~ order, data = sample_vignette)
anova_marriage_summary <- summary(anova_marriage)

# Store order effect results
order_effect_results <- list(
  anova_date = anova_date,
  anova_date_summary = anova_date_summary,
  anova_marriage = anova_marriage,
  anova_marriage_summary = anova_marriage_summary
)

# 3 Check respondent characteristics --------------------------------------

# Initialize storage for respondent characteristic results
respondent_results <- list()

# Check age correlations with vignette dimensions
for (dim in dim_cols) {
  # Remove NA values
  clean_data <- sample_answer[
    !is.na(sample_answer$age) & !is.na(sample_answer[[dim]]),
  ]

  # Run ANOVA
  anova_result <- aov(age ~ get(dim), data = clean_data)
  anova_summary <- summary(anova_result)

  # Store results
  respondent_results[[paste0("age_", dim)]] <- list(
    anova = anova_result,
    anova_summary = anova_summary
  )
}

# Check education correlations with vignette dimensions
for (dim in dim_cols) {
  # Remove NA values and ensure proper factor levels
  clean_data <- sample_answer[
    !is.na(sample_answer$educ) & !is.na(sample_answer[[dim]]),
  ]
  clean_data$educ <- factor(clean_data$educ)
  clean_data[[dim]] <- factor(clean_data[[dim]])

  # Create contingency table
  tab <- table(clean_data$educ, clean_data[[dim]])

  # Run chi-square test
  chi_test <- chisq.test(tab)

  # Store results
  respondent_results[[paste0("educ_", dim)]] <- list(
    contingency_table = tab,
    chi_square_test = chi_test
  )
}

# Store all results in a single list for easy access
all_check_results <- list(
  randomization = list(
    order_dimension = order_dimension_results,
    pairwise = randomization_results,
    summary = randomization_summary
  ),
  order_effect = order_effect_results,
  respondent_characteristics = respondent_results
)

# Function to get a summary of significant findings
get_significant_findings <- function(results) {
  # Extract significant randomization pairs
  sig_pairs <- results$randomization$summary$significant_pair_names

  # Extract significant order effects
  sig_order_date <- results$order_effect$anova_date_summary[[1]][
    "order", "Pr(>F)"
  ] < 0.05
  sig_order_marriage <- results$order_effect$anova_marriage_summary[[1]][
    "order", "Pr(>F)"
  ] < 0.05

  # Extract significant respondent characteristic effects
  sig_age_effects <- names(results$respondent_characteristics)[
    grepl("^age_", names(results$respondent_characteristics)) &
      sapply(
        results$respondent_characteristics[
          grepl("^age_", names(results$respondent_characteristics))
        ],
        function(x) x$anova_summary[[1]]["get(dim)", "Pr(>F)"] < 0.05
      )
  ]

  sig_educ_effects <- names(results$respondent_characteristics)[
    grepl("^educ_", names(results$respondent_characteristics)) &
      sapply(
        results$respondent_characteristics[
          grepl("^educ_", names(results$respondent_characteristics))
        ],
        function(x) x$chi_square_test$p.value < 0.05
      )
  ]

  # Return summary
  list(
    significant_randomization_pairs = sig_pairs,
    significant_order_effects = list(
      dating = sig_order_date,
      marriage = sig_order_marriage
    ),
    significant_respondent_effects = list(
      age = sig_age_effects,
      education = sig_educ_effects
    )
  )
}

# Create summary of significant findings
significant_findings <- get_significant_findings(all_check_results)

message("✓ All checks completed\n")
