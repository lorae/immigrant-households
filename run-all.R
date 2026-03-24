# run-all.R
#
# This file sequentially runs all scripts necessary for the project to run.
# Be sure to follow Parts A and B of the setup instructions outlined in README.md
# prior to running this script.

# Setup
source("src/scripts/import-ipums.R") # Run time: about 30-45 minutes
source("src/scripts/process-person-level-ipums.R")
source("src/scripts/process-household-level-ipums.R")

# Figures
source("src/figures/fig01-household-size-year-nativity-line.R")
source("src/figures/fig02-household-size-cohort-line.R")
source("src/figures/fig03-multifam-year-nativity-line.R")
source("src/figures/fig04-multifam-year-cohort-line.R")
source("src/figures/fig05-percent-households-with-immigrant.R")
source("src/figures/fig06-percent-households-only-immigrants.R")
source("src/figures/fig07-percent-households-only-immigrants-cohort-line.R")
source("src/figures/fig08-bedroom-year-nativity-line.R")
source("src/figures/fig09-bedroom-cohort-year-nativity-line.R")
source("src/figures/fig10-age-distribution-cohort.R")
source("src/figures/fig11-foreign-native-household-size-race-year.R")
source("src/figures/fig12-nchild-year-nativity-line.R")
source("src/figures/fig13-nchild-year-nativity-race-line.R")
source("src/figures/fig14-multifam-year-nativity-race-line.R")

# Fast facts
source("src/scripts/fast-facts.R")
