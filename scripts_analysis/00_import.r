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
library(ggeffects)
library(jtools)
library(lme4)
library(tidyverse)

# Load data
data_behavior <- read_feather("data_clean/data_behavior.arrow")
data_answer <- read_feather("data_clean/data_answer.arrow")
