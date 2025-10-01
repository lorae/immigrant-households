# process-ipums.R
#
# This script adds bucket columns to raw data based on specifications outlined in
# CSV files in the `lookup_tables/` directory.
# It reads data from the "ipums" table in `/db/ipums-raw.duckdb` and writes processed
# data to the "ipums-bucketed" table in `/db/ipums-processed.duckdb`.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("dbplyr")

devtools::load_all("../demographr")

# ----- Step 1: Connect to the database ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums")

# For data validation: count number of rows, to ensure none are dropped later
obs_count <- ipums_db |>
  summarise(count = n()) |>
  pull()


# ----- Step 2: Add columns ----- #

ipums_final <- ipums_db |>
  mutate(
    us_born = BPL <= 120,
    decade = case_when(
      YEAR == 1970 ~ 1970,
      YEAR == 1980 ~ 1980,
      YEAR == 1990 ~ 1990,
      YEAR == 2000 ~ 2000,
      YEAR >= 2008 & YEAR <= 2012 ~ 2010,
      YEAR >= 2018 & YEAR <= 2022 ~ 2020,
      YEAR == 2023 ~ 2023
    )
  )

# ----- Step 3: Compute, save, close out the connection ----- #

# Create a new table to write processed columns to
compute(
  ipums_final,
  name = "ipums_processed",
  temporary = FALSE,
  overwrite = TRUE
)

# Validate no rows were dropped
validate_row_counts(
  db = tbl(con, "ipums_processed"),
  expected_count = obs_count,
  step_description = "ipums_processed db was created"
)

dbDisconnect(con)
