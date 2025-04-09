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

message("✓ Import completed\n")

# Source scripts
message("\n--- Analyzing data... ---\n")

source("scripts_analysis/01_exclude.r")
message("✓ Sample selection completed\n")

source("scripts_analysis/02_describe.r")
message("✓ Data description completed\n")

source("scripts_analysis/03_check.r")
message("✓ Data check completed\n")

source("scripts_analysis/04_model_null.r")
message("✓ Null models completed\n")

source("scripts_analysis/05_model_marriage.r")
message("✓ Marriage models completed\n")

source("scripts_analysis/06_model_dating.r")
message("✓ Dating models completed\n")

source("scripts_analysis/07_model_long.r")
message("✓ Long models completed\n")

source("scripts_analysis/08_model_predict.r")
message("✓ Predictions completed\n")

message("\n--- All analysis scripts completed successfully! ---\n")
