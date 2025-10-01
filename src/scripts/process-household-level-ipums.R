# process-household-level-ipums.R
#
# This script creates household-level aggregations from person-level data.
# It reads data from the "ipums_person" table in `/db/ipums.duckdb` and writes
# household-level data to the "ipums_household_level" table in the same database.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("dbplyr")
devtools::load_all("../demographr")

# ----- Step 1: Connect to the database ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# For data validation: count number of unique households
household_count <- ipums_person |>
  distinct(SERIAL, YEAR) |>
  summarise(count = n()) |>
  pull()

# ----- Step 2: Create household-level aggregations ----- #
ipums_household <- ipums_person |>
  group_by(SERIAL, YEAR) |>
  summarise(
    # Household size
    hh_size = n(),
    
    # NUMPREC (should match household size, for verification)
    NUMPREC = first(NUMPREC),
    
    # Number of families in household (take max since it's constant within household)
    n_multifam = max(n_multifam, na.rm = TRUE),
    is_multifam = max(is_multifam, na.rm = TRUE),
    
    # Household composition
    n_adults = sum(AGE >= 18, na.rm = TRUE),
    n_children = sum(AGE < 18, na.rm = TRUE),
    
    # Immigration characteristics
    n_foreign_born = sum(!us_born, na.rm = TRUE),
    pct_foreign_born = mean(as.numeric(!us_born), na.rm = TRUE),
    all_us_born = all(us_born, na.rm = TRUE),
    any_foreign_born = any(!us_born, na.rm = TRUE),
    
    # Decade (should be constant within household)
    decade = first(decade),
    
    .groups = "drop"
  )

# ----- Step 3: Compute, save, close out the connection ----- #
# Create a new table for household-level data
compute(
  ipums_household,
  name = "ipums_household",
  temporary = FALSE,
  overwrite = TRUE
)

# Validate household count matches
validate_row_counts(
  db = tbl(con, "ipums_household"),
  expected_count = household_count,
  step_description = "ipums_household db was created"
)

# Validate NUMPREC matches hh_size
household_table <- tbl(con, "ipums_household")
mismatches <- household_table |>
  filter(NUMPREC != hh_size) |>
  summarise(count = n()) |>
  pull()

if (mismatches > 0) {
  stop(
    sprintf(
      "Data validation failed: %d households have NUMPRECT != hh_size",
      mismatches
    )
  )
} else {
  message("✓ Validation passed: NUMPREC matches hh_size for all households")
}

dbDisconnect(con)