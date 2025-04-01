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

# Load samples
samples <- readRDS("data_clean/samples.rds")
sample_answer <- samples$sample_answer
sample_behavior <- samples$sample_behavior
sample_vignette <- samples$sample_vignette
sample_long <- samples$sample_long

# Source scripts
source("scripts_analysis/01_exclude.r")
source("scripts_analysis/02_describe.r")
source("scripts_analysis/03_check.r")
source("scripts_analysis/04_model_null.r")
source("scripts_analysis/05_model_marriage.r")
source("scripts_analysis/06_model_dating.r")
source("scripts_analysis/07_model_long.r")
source("scripts_analysis/08_model_predict.r")
