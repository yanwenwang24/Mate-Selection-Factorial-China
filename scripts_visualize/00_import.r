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

library(arrow)
library(broom.mixed)
library(ggeffects)
library(ggtext)
library(ggthemes)
library(janitor)
library(jtools)
library(lme4)
library(partR2)
library(patchwork)
library(tidytext)
library(tidyverse)

# Set theme
theme_set(theme_classic())
position_dodge <- position_dodge(width = 0.75)

# 2 Load samples ----------------------------------------------------------

samples <- readRDS("data_clean/samples.rds")
sample_answer <- samples$sample_answer
sample_behavior <- samples$sample_behavior
sample_vignette <- samples$sample_vignette
sample_long <- samples$sample_long

# 3 Load models -----------------------------------------------------------

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

# 3.3 Long model ----------------------------------------------------------

mods_long <- readRDS("models/mods_long.rds")
mod_long_gender <- mods_long$mod_gender
mod_long_type <- mods_long$mod_type

message("✓ Data and models loaded\n")

# 4 Source scripts --------------------------------------------------------

message("\n--- Visualizing data... ---\n")

source("scripts_visualize/01_importance.r")
message("✓ Relative importance visualized\n")

source("scripts_visualize/02_pooled.r")
message("✓ Pooled effects visualized\n")

source("scripts_visualize/03_gender.r")
message("✓ Gender-specific effects visualized\n")

source("scripts_visualize/04_gender_diff.r")
message("✓ Gender difference visualized\n")

source("scripts_visualize/05_educ.r")
message("✓ Education effects visualized\n")

message("\n--- Visualization completed ---\n")
