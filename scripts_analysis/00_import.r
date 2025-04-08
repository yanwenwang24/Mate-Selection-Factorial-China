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

# Load packages
library(arrow)
library(janitor)
library(jtools)
library(lme4)
library(patchwork)
library(tidyverse)

# Set theme
theme_set(theme_classic())

# Load data
data_behavior <- read_feather("data_clean/data_behavior.arrow")
data_answer <- read_feather("data_clean/data_answer.arrow")

cat("✓ Import completed\n")

# Source scripts
cat("Analyzing data...\n")

source("scripts_analysis/01_exclude.r")
cat("✓ Sample selection completed\n")

source("scripts_analysis/02_describe.r")
cat("✓ Data description completed\n")

source("scripts_analysis/03_check.r")
cat("✓ Data check completed\n")

source("scripts_analysis/04_model_null.r")
cat("✓ Null models completed\n")

source("scripts_analysis/05_model_marriage.r")
cat("✓ Marriage models completed\n")

source("scripts_analysis/06_model_dating.r")
cat("✓ Dating models completed\n")

source("scripts_analysis/07_model_long.r")
cat("✓ Long models completed\n")

source("scripts_analysis/08_model_predict.r")
cat("✓ Predictions completed\n")

cat("\nAll analysis scripts completed successfully!\n")
