## ------------------------------------------------------------------------
##
## Script name: 03_check.r
## Purpose: Check randomization, vignette order, and response time
## Author: Yanwen Wang
## Date Created: 2025-03-21
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Check randomization ---------------------------------------------------

# Select dimension columns
dim_cols <- grep("^v_", colnames(sample_vignette), value = TRUE)
dim_cols <- setdiff(dim_cols, "vignette")

# Check independence between vignette order and dimension
for (dim in dim_cols) {
  cat("=======================================\n")
  cat("Randomization check for dimension:", dim, "\n")

  # Create a contingency table between vignette order and current dimension
  tab <- table(sample_vignette$vignette, sample_vignette[[dim]])
  print(tab)

  # Run chi-square test
  chi_test <- chisq.test(tab)
  print(chi_test)

  cat("\n\n")
}

# Check pairwise independence between dimensions
dim_pairs <- combn(dim_cols, 2, simplify = FALSE)
flag_pairs <- list() # to store significant pairs

for (pair in dim_pairs) {
  dim1 <- pair[1]
  dim2 <- pair[2]
  cat("=======================================\n")
  cat("Checking independence between:", dim1, "and", dim2, "\n")

  # Construct contingency table
  tab_pair <- table(sample_vignette[[dim1]], sample_vignette[[dim2]])
  print(tab_pair)

  # Perform chi-square test
  chi_test_pair <- chisq.test(tab_pair)
  print(chi_test_pair)

  # Check for significant p-value
  if (chi_test_pair$p.value < 0.05) {
    cat("Significant association found (p-value:", chi_test_pair$p.value, ")\n")
    flag_pairs[[paste(dim1, dim2, sep = "-")]] <- chi_test_pair
  } else {
    cat("No significant association (p-value:", chi_test_pair$p.value, ")\n")
  }

  cat("\n\n")
}

# Review significant pairs
if (length(flag_pairs) > 0) {
  cat("Summary of significant pairs with full results:\n")
  for (pair_name in names(flag_pairs)) {
    cat("***************************************\n")
    cat("Association for pair:", pair_name, "\n")
    print(flag_pairs[[pair_name]])
    cat("***************************************\n\n")
  }
} else {
  cat("No significant associations found.\n")
}

# 2 Check order -----------------------------------------------------------

# Factorize vignette order
sample_vignette$vignette <- as.factor(sample_vignette$vignette)

# Visualize by vignette order
order_date <- ggplot(sample_vignette, aes(x = vignette, y = r_date)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Dating response by vignette order",
    x = "Vignette order",
    y = "Dating response"
  ) +
  theme_minimal()

order_marriage <- ggplot(sample_vignette, aes(x = vignette, y = r_marriage)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Marriage response by vignette order",
    x = "Vignette order",
    y = "Marriage response"
  ) +
  theme_minimal()

order_response <- order_date + order_marriage

# Save
ggsave(
  "graphs/order_response.png",
  order_response,
  width = 12,
  height = 8,
  dpi = 300
)

# ANOVA to test if vignette order predicts r_date
anova_date <- aov(r_date ~ vignette, data = sample_vignette)
summary(anova_date)

# ANOVA to test if vignette order predicts r_marriage
anova_marriage <- aov(r_marriage ~ vignette, data = sample_vignette)
summary(anova_marriage)

# 3 Check response time ---------------------------------------------------

time_date <- ggplot(sample_vignette, aes(x = duration, y = r_date)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Dating response by response time",
    x = "Response time (seconds)",
    y = "Dating response"
  ) +
  theme_minimal()

time_marriage <- ggplot(sample_vignette, aes(x = duration, y = r_marriage)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Marriage response by response time",
    x = "Response time (seconds)",
    y = "Marriage response"
  ) +
  theme_minimal()

time_response <- time_date + time_marriage

# Save
ggsave(
  "graphs/time_response.png",
  time_response,
  width = 12,
  height = 8,
  dpi = 300
)

# Regression
lm_date <- lm(r_date ~ duration, data = sample_vignette)
summary(lm_date)

lm_marriage <- lm(r_marriage ~ duration, data = sample_vignette)
summary(lm_marriage)
