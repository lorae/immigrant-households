# Fits ONE pooled Model 5 on all years with decade interactions, then reconstructs
# per-decade effects for graphing.
#
# Advantages over per-decade fits:
#  - Uses the full sample jointly → less noise, more stable estimates
#  - Permits formal tests of whether coefficients differ across decades
#  - Single model fit instead of seven
#
# Outputs:
# - output/figures/hhsize-regression-model5-over-time.jpeg
# - output/figures/hhsize-regression-model5-over-time.csv
# - output/figures/hhsize-regression-model5-over-time-exp.jpeg
# - output/figures/hhsize-regression-model5-over-time-exp.csv
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("fixest")
library("ggplot2")
library("broom")
library("tidyr")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Build regression data across all years ----- #

reg_data <- ipums_person |>
  filter(GQ %in% c(0, 1, 2)) |>
  dplyr::select(NUMPREC, us_born, AGE, PERWT, STATEFIP, race_eth, YRIMMIG, YEAR, decade, SERIAL) |>
  collect() |>
  mutate(
    foreign_born = as.integer(!us_born),
    age_at_arrival = case_when(
      us_born ~ 0L,
      !us_born & YRIMMIG > 0 ~ AGE - (YEAR - YRIMMIG),
      TRUE ~ NA_integer_
    ),
    race_eth = factor(
      race_eth,
      levels = c("White", "Hispanic", "Black", "AAPI", "AIAN", "Multiracial", "Other")
    ),
    hh_id = paste(YEAR, SERIAL, sep = "_")  # SERIAL is only unique within a YEAR
  ) |>
  filter(!is.na(decade)) |>
  mutate(decade = factor(decade))  # must be factor so coefs are per-decade, not linear

dbDisconnect(con)

# ----- Step 1b: Sample households to keep pooled design matrix in memory ----- #
# Sample 20% of HOUSEHOLDS per decade (not persons), so within-HH structure is preserved.

set.seed(42)

sampled_hh_ids <- reg_data |>
  distinct(decade, hh_id) |>
  group_by(decade) |>
  slice_sample(prop = 0.2) |>
  ungroup() |>
  pull(hh_id)

reg_data_sample <- reg_data |> filter(hh_id %in% sampled_hh_ids)

# ----- Step 2: Fit one pooled model with decade interactions ----- #

m_pooled <- fepois(
  NUMPREC ~ foreign_born + age_at_arrival + AGE + race_eth + decade +
            foreign_born:decade +
            age_at_arrival:decade +
            AGE:decade +
            race_eth:decade |
            STATEFIP,
  data = reg_data_sample,
  weights = ~PERWT,
  cluster = ~hh_id
)

# ----- Step 3: Reconstruct per-decade effects from pooled coefficients ----- #
# For each variable V and decade d:
#   effect_d = beta_V + beta_{V:decade_d}   (beta_{V:decade_d} == 0 for reference decade)

tidy_coefs <- broom::tidy(m_pooled, conf.int = TRUE)
coef_lookup <- setNames(tidy_coefs$estimate, tidy_coefs$term)

decades    <- levels(reg_data$decade)
ref_decade <- decades[1]

term_labels <- c(
  foreign_born     = "Foreign-born",
  age_at_arrival   = "Age at arrival",
  AGE              = "Age",
  race_ethHispanic = "Hispanic (vs White)",
  race_ethBlack    = "Black (vs White)",
  race_ethAAPI     = "AAPI (vs White)"
)

plot_data <- expand_grid(term = names(term_labels), decade = decades) |>
  mutate(
    main_coef        = coef_lookup[term],
    interaction_name = if_else(
      decade == ref_decade,
      NA_character_,
      paste0(term, ":decade", decade)
    ),
    interaction_coef = if_else(
      decade == ref_decade,
      0,
      unname(coef_lookup[interaction_name])
    ),
    estimate    = main_coef + interaction_coef,
    mult_effect = exp(estimate),
    term_label  = factor(unname(term_labels[term]), levels = unname(term_labels)),
    decade      = as.integer(as.character(decade))
  ) |>
  arrange(term, decade)

# ----- Step 4: Log-scale plot ----- #

fig <- ggplot(plot_data, aes(x = decade, y = estimate, color = term_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  labs(
    x = NULL,
    y = "Coefficient (log scale)",
    color = NULL,
    title = "Model 5 coefficients across decades (pooled model)",
    caption = "Pooled Poisson regression of household size on FB + age-at-arrival + AGE + race/eth,\nall interacted with decade, state FE absorbed."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8)
  )

fig

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

write_csv(
  plot_data,
  "output/figures/hhsize-regression-model5-over-time.csv"
)

ggsave(
  "output/figures/hhsize-regression-model5-over-time.jpeg",
  plot = fig,
  width = 7,
  height = 5,
  dpi = 500
)

# ----- Step 5: Exponentiated (multiplicative) plot ----- #

fig_exp <- ggplot(plot_data, aes(x = decade, y = mult_effect, color = term_label)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_y_continuous(
    breaks = c(0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(
    x = NULL,
    y = "Multiplicative effect on household size",
    color = NULL,
    title = "Model 5 multiplicative effects across decades (pooled model)",
    caption = "exp(coefficient) from a pooled Poisson regression of household size on FB + age-at-arrival + AGE + race/eth,\nall interacted with decade, state FE absorbed. 1.0 = no effect; 1.30 = 30% more household members."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8)
  )

fig_exp

write_csv(
  plot_data,
  "output/figures/hhsize-regression-model5-over-time-exp.csv"
)

ggsave(
  "output/figures/hhsize-regression-model5-over-time-exp.jpeg",
  plot = fig_exp,
  width = 7,
  height = 5,
  dpi = 500
)
