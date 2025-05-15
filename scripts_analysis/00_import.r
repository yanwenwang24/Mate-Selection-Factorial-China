## ------------------------------------------------------------------------
##
## Script name: 00_import.r
## Purpose: Import libraries and data
## Author: Yanwen Wang
## Date Created: 2025-03-21
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# Load packages -----------------------------------------------------------

message("Loading required libraries...")

# List of packages to check/install
packages_to_install <- c(
  "arrow",
  "janitor",
  "jtools",
  "lme4",
  "patchwork",
  "tidyverse"
)

# Install if needed
for (pkg in packages_to_install) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
    message(sprintf("Installed package: %s", pkg))
  }
}

# Load packages after installation
invisible(lapply(packages_to_install, library, character.only = TRUE))
message("Libraries loaded successfully.")

# Set theme
theme_set(theme_classic())

# Load data ---------------------------------------------------------------

message("Loading data...")
data_behavior <- read_feather("data/processed/data_behavior.arrow")
data_answer <- read_feather("data/processed/data_answer.arrow")

message("✓ Data loaded.\n")

# Source scripts -----------------------------------------------------------

message("\n--- Analyzing data... ---\n")

# Select samples
source("scripts_analysis/01_exclude.r")

# Describe samples
source("scripts_analysis/02_describe.r")

# Check randomization
source("scripts_analysis/03_check.r")

# Fit null models
source("scripts_analysis/04_model_null.r")

# Fit models for marriages
source("scripts_analysis/05_model_marriage.r")
source("scripts_analysis/05.1_model_marriage_robust.r") # Remove vignettes 5 & 6

# Fit models for dating
source("scripts_analysis/06_model_dating.r")
source("scripts_analysis/06.1_model_dating_robust.r") # Remove vignettes 5 & 6

# Fit models for comparing marriage and dating
source("scripts_analysis/07_model_long.r")

# Predict with real data
source("scripts_analysis/08_model_predict.r")

message("\n--- All analysis scripts completed successfully! ---\n")
