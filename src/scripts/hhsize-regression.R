# Regression analysis of household size (2022 cross-section)
#
# Builds models by adding one variable at a time. Poisson with log link via
# fixest::fepois; SEs clustered at the household level.
#
# Outputs:
# - output/tables/hhsize-regression.docx                          (log coefs;
#                                                                  AGE and age_
#                                                                  at_arrival
#                                                                  in decades)
# - output/tables/hhsize-regression-multiplicative.docx           (exp(beta))
# - output/tables/hhsize-regression-by-decade.docx                (M5 spec, one
#                                                                  fit per
#                                                                  decade
#                                                                  sample/ACS
#                                                                  pool)
# - output/tables/hhsize-regression-by-decade-multiplicative.docx (exp(beta))
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
  dplyr::select(
    NUMPREC, 
    us_born, 
    AGE, 
    PERWT, 
    STATEFIP, 
    race_eth, 
    YRIMMIG, 
    MULTYEAR, # needed to calculate years in usa
    SERIAL
    ) |>
  collect() |>
  mutate(
    foreign_born = as.integer(!us_born),
    age_at_arrival = case_when(
      us_born ~ 0L,
      !us_born & YRIMMIG > 0 ~ AGE - (MULTYEAR - YRIMMIG),
      TRUE ~ NA_integer_
    ),
    # Convert AGE and age_at_arrival from years to decades so coefficients
    # are interpretable per decade of age, not per year.
    AGE_decade = AGE / 10,
    age_at_arrival_decade = age_at_arrival / 10,
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

# Model 2: add age at arrival
m2 <- fepois(
  NUMPREC ~ foreign_born + age_at_arrival_decade + AGE_decade,
  data = reg_data,
  weights = ~PERWT,
  cluster = ~SERIAL
)

# Model 3: add race/eth
m3 <- fepois(
  NUMPREC ~ foreign_born + age_at_arrival_decade + AGE_decade + race_eth,
  data = reg_data,
  weights = ~PERWT,
  cluster = ~SERIAL
)

# Model 4: interact race/eth with foreign born
m4 <- fepois(
  NUMPREC ~ age_at_arrival_decade + AGE_decade + race_eth*foreign_born,
  data = reg_data,
  weights = ~PERWT,
  cluster = ~SERIAL
)

# Model 5: add state FEs
m5 <- fepois(
  NUMPREC ~ age_at_arrival_decade + AGE_decade + race_eth*foreign_born | STATEFIP,
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
  "State FE", "No",       "No",       "No",      "No",      "Yes"
)

# ----- Step 3: Output ----- #

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# Only keep N in the goodness-of-fit footer; suppresses R2, AIC/BIC, RMSE,
# and the auto-generated "FE: STATEFIP" row (replaced by the manual State FE
# row in fe_rows).
gof_map <- tibble::tribble(
  ~raw,   ~clean,      ~fmt,
  "nobs", "Num. Obs.", function(x) format(x, big.mark = ",")
)

# Display labels for each coefficient. coef_map also fixes the row order and
# drops anything not listed.
coef_map <- c(
  "foreign_born"                     = "Foreign born",
  "age_at_arrival_decade"            = "Age at arrival (decade)",
  "AGE_decade"                       = "Age (decade)",
  "race_ethHispanic"                 = "Hispanic",
  "race_ethBlack"                    = "Black",
  "race_ethAAPI"                     = "AAPI",
  "race_ethAIAN"                     = "AIAN",
  "race_ethMultiracial"              = "Multiracial",
  "race_ethOther"                    = "Other",
  "race_ethHispanic:foreign_born"    = "Hispanic x Foreign born",
  "race_ethBlack:foreign_born"       = "Black x Foreign born",
  "race_ethAAPI:foreign_born"        = "AAPI x Foreign born",
  "race_ethAIAN:foreign_born"        = "AIAN x Foreign born",
  "race_ethMultiracial:foreign_born" = "Multiracial x Foreign born",
  "race_ethOther:foreign_born"       = "Other x Foreign born",
  "(Intercept)"                      = "(Intercept)"
)

# Log-coefficient table
modelsummary(
  models,
  output = "output/tables/hhsize-regression.docx",
  stars = TRUE,
  coef_map = coef_map,
  gof_map = gof_map,
  add_rows = fe_rows
)

# Multiplicative-effect table: same models, exp(beta). modelsummary applies
# delta-method SEs internally. Same coef_map / gof_map / fe_rows.
modelsummary(
  models,
  output = "output/tables/hhsize-regression-multiplicative.docx",
  stars = TRUE,
  exponentiate = TRUE,
  coef_map = coef_map,
  gof_map = gof_map,
  add_rows = fe_rows
)

# ----- Step 4: M5 fit per decade sample / ACS pool ----- #
# Run the full M5 spec separately on each of the four decennial census
# samples (1970-2000) and the two ACS pools (2010, 2020). No pooled model.
# RHS, scaling, weights, and clustering match Step 2's M5 exactly.
#
# Notes:
# - SERIAL is only unique within a YEAR, so cluster on hh_id = paste(YEAR, SERIAL).
# - MULTYEAR (per-record interview year) only exists for ACS samples; for
#   decennial census records it is NA. coalesce(MULTYEAR, YEAR) gives the
#   correct reference year for age-at-arrival in both cases.

decades_to_fit <- c(1970, 1980, 1990, 2000, 2010, 2020)

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

reg_data_by_decade <- ipums_person |>
  filter(
    GQ %in% c(0, 1, 2),
    decade %in% decades_to_fit
  ) |>
  dplyr::select(
    NUMPREC, us_born, AGE, PERWT, STATEFIP, race_eth,
    YRIMMIG, YEAR, MULTYEAR, decade, SERIAL
  ) |>
  collect() |>
  mutate(
    foreign_born = as.integer(!us_born),
    ref_year = coalesce(MULTYEAR, YEAR),
    age_at_arrival = case_when(
      us_born ~ 0L,
      !us_born & YRIMMIG > 0 ~ AGE - (ref_year - YRIMMIG),
      TRUE ~ NA_integer_
    ),
    AGE_decade            = AGE / 10,
    age_at_arrival_decade = age_at_arrival / 10,
    race_eth = factor(
      race_eth,
      levels = c("White", "Hispanic", "Black", "AAPI", "AIAN", "Multiracial", "Other")
    ),
    hh_id = paste(YEAR, SERIAL, sep = "_")
  )

dbDisconnect(con)

fit_m5_for_decade <- function(dec) {
  fepois(
    NUMPREC ~ age_at_arrival_decade + AGE_decade + race_eth * foreign_born | STATEFIP,
    data    = reg_data_by_decade |> filter(decade == dec),
    weights = ~PERWT,
    cluster = ~hh_id
  )
}

models_by_decade <- lapply(decades_to_fit, fit_m5_for_decade)
names(models_by_decade) <- as.character(decades_to_fit)

fe_rows_by_decade <- tibble::tribble(
  ~term,      ~"1970", ~"1980", ~"1990", ~"2000", ~"2010", ~"2020",
  "State FE", "Yes",   "Yes",   "Yes",   "Yes",   "Yes",   "Yes"
)

# Log-coefficient table, one column per decade
modelsummary(
  models_by_decade,
  output = "output/tables/hhsize-regression-by-decade.docx",
  stars = TRUE,
  coef_map = coef_map,
  gof_map = gof_map,
  add_rows = fe_rows_by_decade
)

# Multiplicative version
modelsummary(
  models_by_decade,
  output = "output/tables/hhsize-regression-by-decade-multiplicative.docx",
  stars = TRUE,
  exponentiate = TRUE,
  coef_map = coef_map,
  gof_map = gof_map,
  add_rows = fe_rows_by_decade
)
