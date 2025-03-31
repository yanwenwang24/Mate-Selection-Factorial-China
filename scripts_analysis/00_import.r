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
