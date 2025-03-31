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
library(tidyverse)

# Set theme
theme_set(theme_light())
position_dodge <- position_dodge(width = 0.75)

# 2 Load samples ----------------------------------------------------------

samples <- readRDS("data_clean/samples.rds")
sample_answer <- samples$sample_answer
sample_behavior <- samples$sample_behavior
sample_vignette <- samples$sample_vignette
sample_full <- samples$sample_full

# 3 Load models -----------------------------------------------------------

# 3.1 Marriage ------------------------------------------------------------

# Main model
mods_marriage_main <- readRDS("models/mods_marriage_main.rds")
mod_marriage_main <- mods_marriage_main$mod_main_2

# Interaction model
mods_marriage_inter <- readRDS("models/mods_marriage_inter.rds")
mod_marriage_gender <- mods_marriage_inter$mod_gender
mod_marriage_educ <- mods_marriage_inter$mod_educ

# 3.2 Dating --------------------------------------------------------------

# Main model
mods_dating_main <- readRDS("models/mods_dating_main.rds")
mod_dating_main <- mods_dating_main$mod_main_2

# Interaction model
mods_dating_inter <- readRDS("models/mods_dating_inter.rds")
mod_dating_gender <- mods_dating_inter$mod_gender
mod_dating_educ <- mods_dating_inter$mod_educ

# 3.3 Long model ----------------------------------------------------------

mods_long <- readRDS("models/mods_long.rds")
mod_long_gender <- mods_long$mod_gender
mod_long_type <- mods_long$mod_type
