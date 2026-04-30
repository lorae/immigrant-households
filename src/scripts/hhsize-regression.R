# Regression analysis of household size (2022 cross-section)
#
# Builds models by adding one variable at a time. Poisson with log link via
# fixest::fepois; SEs clustered at the household level.
#
# Outputs:
# - output/tables/hhsize-regression.html
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("fixest")
library("modelsummary")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Build regression data ----- #

reg_data <- ipums_person |>
  filter(GQ %in% c(0, 1, 2) & YEAR == 2022) |>
  dplyr::select(NUMPREC, us_born, AGE, PERWT, STATEFIP, race_eth, YRIMMIG, SERIAL) |>
  collect() |>
  mutate(
    foreign_born = as.integer(!us_born),
    age_at_arrival = case_when(
      us_born ~ 0L,
      !us_born & YRIMMIG > 0 ~ AGE - (2022L - YRIMMIG),
      TRUE ~ NA_integer_
    ),
    race_eth = factor(
      race_eth,
      levels = c("White", "Hispanic", "Black", "AAPI", "AIAN", "Multiracial", "Other")
    )
  )

dbDisconnect(con)

# ----- Step 2: Models ----- #
# Poisson regressions via fixest::fepois. Variables added one at a time.
# Household-clustered SEs (cluster = ~SERIAL; SERIAL is unique within 2022).

# Model 1: foreign-born only
m1 <- fepois(
  NUMPREC ~ foreign_born,
  data = reg_data,
  weights = ~PERWT,
  cluster = ~SERIAL
)

# Model 2: add age
m2 <- fepois(
  NUMPREC ~ foreign_born + AGE,
  data = reg_data,
  weights = ~PERWT,
  cluster = ~SERIAL
)

# Model 3: add state fixed effects
m3 <- fepois(
  NUMPREC ~ foreign_born + AGE | STATEFIP,
  data = reg_data,
  weights = ~PERWT,
  cluster = ~SERIAL
)

# Model 4: add race/ethnicity (White omitted)
m4 <- fepois(
  NUMPREC ~ foreign_born + AGE + race_eth | STATEFIP,
  data = reg_data,
  weights = ~PERWT,
  cluster = ~SERIAL
)

# Model 5: add age-at-arrival (0 for US-born; age when FB arrived in US)
m5 <- fepois(
  NUMPREC ~ foreign_born + AGE + race_eth + age_at_arrival | STATEFIP,
  data = reg_data,
  weights = ~PERWT,
  cluster = ~SERIAL
)

models <- list(
  "Model 1" = m1,
  "Model 2" = m2,
  "Model 3" = m3,
  "Model 4" = m4,
  "Model 5" = m5
)

fe_rows <- tibble::tribble(
  ~term,      ~"Model 1", ~"Model 2", ~"Model 3", ~"Model 4", ~"Model 5",
  "State FE", "No",       "No",       "Yes",      "Yes",      "Yes"
)

# ----- Step 3: Output ----- #

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

modelsummary(
  models,
  output = "output/tables/hhsize-regression.html",
  stars = TRUE,
  add_rows = fe_rows
)
