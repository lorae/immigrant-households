# process-person-level-ipums.R
#
# This script adds bucket columns to raw (person-level) data.
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
    ),
    immig_cohort = case_when(
      YRIMMIG == 0 ~ NA_character_,
      YRIMMIG <= 1919 ~ "1919 or earlier",
      YRIMMIG >= 1920 & YRIMMIG < 1930 ~ "1920s",
      YRIMMIG >= 1930 & YRIMMIG < 1940 ~ "1930s",
      YRIMMIG >= 1940 & YRIMMIG < 1950 ~ "1940s",
      YRIMMIG >= 1950 & YRIMMIG < 1960 ~ "1950s",
      YRIMMIG >= 1960 & YRIMMIG < 1970 ~ "1960s",
      YRIMMIG >= 1970 & YRIMMIG < 1980 ~ "1970s",
      YRIMMIG >= 1980 & YRIMMIG < 1990 ~ "1980s",
      YRIMMIG >= 1990 & YRIMMIG < 2000 ~ "1990s",
      YRIMMIG >= 2000 & YRIMMIG < 2010 ~ "2000s",
      YRIMMIG >= 2010 & YRIMMIG < 2020 ~ "2010s",
      YRIMMIG >= 2020 ~ "2020s"
    ),
    # Top-code at 5, since 1970 has most restrictive top-code
    n_multifam = case_when(
      NFAMS == 0 ~ 0,
      NFAMS == 1 ~ 1,
      NFAMS == 2 ~ 2,
      NFAMS == 3 ~ 3,
      NFAMS == 4 ~ 4,
      NFAMS >= 5 ~ 5
    ),
    is_multifam = case_when(
      n_multifam == 0 ~ NA,
      n_multifam == 1 ~ FALSE,
      n_multifam >= 2 ~ TRUE
    )
  )

# ----- Step 3: Compute, save, close out the connection ----- #

# Create a new table to write processed columns to
compute(
  ipums_final,
  name = "ipums_person",
  temporary = FALSE,
  overwrite = TRUE
)

# Validate no rows were dropped
validate_row_counts(
  db = tbl(con, "ipums_person"),
  expected_count = obs_count,
  step_description = "ipums_person db was created"
)

dbDisconnect(con)
