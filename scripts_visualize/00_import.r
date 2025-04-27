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

# 1 Load packages ---------------------------------------------------------

message("Loading required libraries...")

# List of packages to check/install
packages_to_install <- c(
  "arrow",
  "broom.mixed",
  "ggeffects",
  "ggtext",
  "ggthemes",
  "janitor",
  "jtools",
  "lme4",
  "patchwork",
  "partR2",
  "tidytext",
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
position_dodge <- position_dodge(width = 0.75)

# 2 Load samples ----------------------------------------------------------

message("Loading samples...")

samples <- readRDS("data/processed/samples.rds")
sample_answer <- samples$sample_answer
sample_behavior <- samples$sample_behavior
sample_vignette <- samples$sample_vignette
sample_long <- samples$sample_long

message("✓ Samples loaded\n")

# 3 Load models -----------------------------------------------------------

message("Loading models...")

# 3.1 Marriage ------------------------------------------------------------

# Pooled models
mods_marriage <- readRDS("models/mods_marriage.rds")
mod_marriage_pooled <- mods_marriage$mod_pooled
mod_marriage_pooled_gender <- mods_marriage$mod_pooled_gender # Interaction
mod_marriage_pooled_educ <- mods_marriage$mod_pooled_educ # Interaction

# Gender models
mod_marriage_men <- mods_marriage$mod_men
mod_marriage_women <- mods_marriage$mod_women
mod_marriage_men_educ <- mods_marriage$mod_men_educ # Interaction
mod_marriage_women_educ <- mods_marriage$mod_women_educ # Interaction

# 3.2 Dating --------------------------------------------------------------

# Pooled models
mods_dating <- readRDS("models/mods_dating.rds")
mod_dating_pooled <- mods_dating$mod_pooled
mod_dating_pooled_gender <- mods_dating$mod_pooled_gender # Interaction
mod_dating_pooled_educ <- mods_dating$mod_pooled_educ # Interaction

# Gender models
mod_dating_men <- mods_dating$mod_men
mod_dating_women <- mods_dating$mod_women
mod_dating_men_educ <- mods_dating$mod_men_educ # Interaction
mod_dating_women_educ <- mods_dating$mod_women_educ # Interaction

message("✓ Models loaded\n")

# 4 Source scripts --------------------------------------------------------

message("\n--- Visualizing data... ---\n")

# Relative importance
source("scripts_visualize/01_importance.r")

# Pooled effects
source("scripts_visualize/02_pooled.r")

# Gender-specific effects
source("scripts_visualize/03_gender.r")

# Gender difference
source("scripts_visualize/04_gender_diff.r")

# Education-specific effects
source("scripts_visualize/05_educ.r")

message("\n--- Visualization completed ---\n")
