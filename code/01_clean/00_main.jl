## ------------------------------------------------------------------------
##
## Script name: 00_main.jl
## Purpose: Main script for cleaning the data
## Author: Yanwen Wang
## Date Created: 2025-03-19
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# Load packages
using Arrow
using CategoricalArrays
using CSV
using DataFrames
using DataFramesMeta
using Dates
using FreqTables

# Load dictionaries and functions
include("dictionaries.jl")

# Load data
data_behavior = CSV.read("data/raw/data_behavior.csv", DataFrame)
data_answer = CSV.read("data/raw/data_answer.csv", DataFrame)

# Source scripts
include("01_behavior.jl")
include("02_answer.jl")
