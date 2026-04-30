# Per-decade regression of household size on nativity + controls.
# Six separate Poisson fits, one per decade. Outputs a regression-style
# table with one column per decade and standard errors underneath in parens.
#
# Same RHS as the pooled model in hhsize-regression-over-time.R but without
# decade interactions (since each decade gets its own fit). Full sample per
# decade (no 10% subsample).
#
# Outputs:
# - output/tables/hhsize-regression-by-decade-coefs.csv  (raw log coefs)
# - output/tables/hhsize-regression-by-decade-mult.csv   (exponentiated)

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("fixest")
library("broom")
library("tidyr")
library("readr")
library("purrr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Build regression data (full sample, four main races) ----- #

reg_data <- ipums_person |>
  filter(
    GQ %in% c(0, 1, 2),
    race_eth %in% c("White", "Hispanic", "Black", "AAPI"),
    decade %in% c(1970, 1980, 1990, 2000, 2010, 2020)
  ) |>
  dplyr::select(NUMPREC, us_born, AGE, PERWT, STATEFIP, race_eth,
                YRIMMIG, YEAR, decade, SERIAL, EDUC) |>
  collect() |>
  mutate(
    foreign_born = as.integer(!us_born),
    age_at_arrival = case_when(
      us_born ~ 0L,
      !us_born & YRIMMIG > 0 ~ AGE - (YEAR - YRIMMIG),
      TRUE ~ NA_integer_
    ),
    race_eth = factor(race_eth, levels = c("White", "Hispanic", "Black", "AAPI")),
    educ_cat_simple = factor(
      case_when(
        EDUC >= 0  & EDUC <= 6  ~ "hs_or_less",
        EDUC >= 7  & EDUC <= 9  ~ "some_college",
        EDUC >= 10 & EDUC <= 11 ~ "college_4yr+",
        TRUE                    ~ NA_character_
      ),
      levels = c("college_4yr+", "some_college", "hs_or_less")
    ),
    hh_id = paste(YEAR, SERIAL, sep = "_")
  )

dbDisconnect(con)

# ----- Step 2: Fit one Poisson per decade ----- #

decades <- c(1970, 1980, 1990, 2000, 2010, 2020)

fit_one <- function(dec) {
  fepois(
    NUMPREC ~ foreign_born + race_eth + foreign_born:race_eth +
              AGE + age_at_arrival + educ_cat_simple |
              STATEFIP,
    data    = reg_data |> filter(decade == dec),
    weights = ~PERWT,
    cluster = ~hh_id
  )
}

models <- map(decades, fit_one) |> setNames(as.character(decades))

# ----- Step 3: Tidy coefficients ----- #

all_coefs <- imap(
  models,
  ~ broom::tidy(.x) |> mutate(decade = .y)
) |> bind_rows()

# ----- Step 4: Build the table ----- #

term_labels <- c(
  "foreign_born"                  = "Foreign-born",
  "race_ethHispanic"              = "Hispanic (vs White)",
  "race_ethBlack"                 = "Black (vs White)",
  "race_ethAAPI"                  = "AAPI (vs White)",
  "foreign_born:race_ethHispanic" = "FB x Hispanic",
  "foreign_born:race_ethBlack"    = "FB x Black",
  "foreign_born:race_ethAAPI"     = "FB x AAPI",
  "AGE"                           = "Age (years)",
  "age_at_arrival"                = "Age at arrival (years)",
  "educ_cat_simplesome_college"   = "Some college (vs college 4yr+)",
  "educ_cat_simplehs_or_less"     = "HS or less (vs college 4yr+)"
)

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ ""
  )
}

build_table <- function(coefs_df, exp_form = FALSE) {
  formatted <- coefs_df |>
    filter(term %in% names(term_labels)) |>
    mutate(
      est      = if (exp_form) exp(estimate) else estimate,
      se       = if (exp_form) exp(estimate) * std.error else std.error,  # delta method
      sig      = stars(p.value),
      est_cell = sprintf("%.3f%s", est, sig),
      se_cell  = sprintf("(%.3f)", se)
    )

  est_wide <- formatted |>
    dplyr::select(term, decade, est_cell) |>
    pivot_wider(names_from = decade, values_from = est_cell)

  se_wide <- formatted |>
    dplyr::select(term, decade, se_cell) |>
    pivot_wider(names_from = decade, values_from = se_cell)

  rows <- bind_rows(
    est_wide |> mutate(label = term_labels[term], row_kind = "est"),
    se_wide  |> mutate(label = "",                row_kind = "se")
  ) |>
    mutate(term_order = match(term, names(term_labels))) |>
    arrange(term_order, match(row_kind, c("est", "se"))) |>
    dplyr::select(label, all_of(as.character(decades)))

  # Footer rows: N, fixed effects, clustering
  n_per_decade <- format(map_int(models, nobs), big.mark = ",")

  footer <- tibble(
    label = c("N (persons)", "State fixed effects", "Clustered SE (household)")
  )
  for (i in seq_along(decades)) {
    d <- as.character(decades[i])
    footer[[d]] <- c(n_per_decade[i], "Yes", "Yes")
  }

  bind_rows(rows, footer)
}

table_log <- build_table(all_coefs, exp_form = FALSE)
table_exp <- build_table(all_coefs, exp_form = TRUE)

# ----- Step 5: Save ----- #

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
write_csv(table_log, "output/tables/hhsize-regression-by-decade-coefs.csv")
write_csv(table_exp, "output/tables/hhsize-regression-by-decade-mult.csv")

cat("\n--- Log coefficients ---\n")
print(table_log)

cat("\n--- Multiplicative effects ---\n")
print(table_exp)
