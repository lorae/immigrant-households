# run-all.R
#
# This file sequentially runs all scripts necessary for the project to run.
# Be sure to follow Parts A and B of the setup instructions outlined in README.md
# prior to running this script.

# Setup
source("src/scripts/import-ipums.R") # Run time: about 30-45 minutes
source("src/scripts/process-ipums.R") # Run time: 2 mins

# Figures
source("src/figures/fig01-household-size-year-nativity-line.R")