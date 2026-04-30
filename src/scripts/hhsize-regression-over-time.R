# Fits ONE pooled Model 5 on all years with foreign_born × race_eth × decade
# (triple) interaction, then reconstructs per-decade race × nativity effects for
# graphing.
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
library("patchwork")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Build regression data across all years ----- #

reg_data <- ipums_person |>
  filter(GQ %in% c(0, 1, 2)) |>
  dplyr::select(NUMPREC, us_born, AGE, PERWT, STATEFIP, race_eth, YRIMMIG, YEAR, decade, SERIAL, EDUC) |>
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
    # Education bucketing follows the scheme from household-size-demographics/
    # lookup_tables/educ/educ_buckets00.csv. EDUC 99 (N/A) -> NA, dropped by fepois.
    educ_cat = factor(
      case_when(
        EDUC >= 0  & EDUC <= 5  ~ "less_than_hs",
        EDUC == 6               ~ "hs",
        EDUC >= 7  & EDUC <= 9  ~ "some_college",
        EDUC >= 10 & EDUC <= 11 ~ "college_4yr+",
        TRUE                    ~ NA_character_
      ),
      # college_4yr+ first so it is the reference category in the regression
      levels = c("college_4yr+", "some_college", "hs", "less_than_hs")
    ),
    educ_cat_simple = factor(
      case_when(
        EDUC >= 0  & EDUC <= 6  ~ "hs_or_less",
        EDUC >= 7  & EDUC <= 9  ~ "some_college",
        EDUC >= 10 & EDUC <= 11 ~ "college_4yr+",
        TRUE                    ~ NA_character_
      ),
      # college_4yr+ first so it is the reference category in the regression
      levels = c("college_4yr+", "some_college", "hs_or_less")
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
  slice_sample(prop = 0.1) |>
  ungroup() |>
  pull(hh_id)

reg_data_sample <- reg_data |> filter(hh_id %in% sampled_hh_ids)

# ----- Step 2: Fit one pooled model with decade interactions ----- #

m_pooled <- fepois(
  NUMPREC ~ foreign_born + race_eth + decade +
            foreign_born:race_eth +
            foreign_born:decade +
            race_eth:decade +
            foreign_born:race_eth:decade +
            age_at_arrival + age_at_arrival:decade +
            AGE + AGE:decade +
            educ_cat_simple + educ_cat_simple:decade |
            STATEFIP,
  data = reg_data_sample,
  weights = ~PERWT,
  cluster = ~hh_id
)

# ----- Step 3: Reconstruct per-decade race × nativity effects ----- #
# For each (race r, foreign_born fb, decade d) cell, the effect relative to
# (White, US-born) in the same decade is the sum of all coefficients whose
# indicator activates for that cell:
#
#   effect(r, fb, d) =
#       1[fb=1]            * beta_FB
#     + 1[r!=White]         * beta_race_r
#     + 1[d!=ref]           * 0                            (decade main absorbed by baseline)
#     + 1[fb=1, r!=White]   * beta_{FB:race_r}
#     + 1[fb=1, d!=ref]     * beta_{FB:decade_d}
#     + 1[r!=White, d!=ref] * beta_{race_r:decade_d}
#     + 1[fb=1, r!=White, d!=ref] * beta_{FB:race_r:decade_d}
#
# (White, US-born) is the reference cell in every decade → effect = 0.

tidy_coefs  <- broom::tidy(m_pooled, conf.int = TRUE)
coef_lookup <- setNames(tidy_coefs$estimate, tidy_coefs$term)

get_coef <- function(term_name) {
  val <- coef_lookup[term_name]
  if (!is.na(val)) return(unname(val))
  # fixest reorders interaction parts (factors first), so try all permutations
  if (grepl(":", term_name, fixed = TRUE)) {
    parts <- strsplit(term_name, ":", fixed = TRUE)[[1]]
    perms <- if (length(parts) == 2) {
      list(parts, rev(parts))
    } else if (length(parts) == 3) {
      list(
        parts[c(1, 2, 3)], parts[c(1, 3, 2)], parts[c(2, 1, 3)],
        parts[c(2, 3, 1)], parts[c(3, 1, 2)], parts[c(3, 2, 1)]
      )
    } else {
      list(parts)
    }
    for (p in perms) {
      val <- coef_lookup[paste(p, collapse = ":")]
      if (!is.na(val)) return(unname(val))
    }
  }
  0
}

decades    <- levels(reg_data$decade)
decades    <- decades[decades != "2023"]  # exclude 2023 from plots (model still uses it)
ref_decade <- decades[1]
races      <- c("White", "Hispanic", "Black", "AAPI")
ref_race   <- "White"

plot_data <- expand_grid(
  race   = races,
  fb     = c(0L, 1L),
  decade = decades
) |>
  rowwise() |>
  mutate(
    fb_main        = if (fb == 1L)                                get_coef("foreign_born")                                    else 0,
    race_main      = if (race != ref_race)                        get_coef(paste0("race_eth", race))                          else 0,
    fb_race        = if (fb == 1L && race != ref_race)            get_coef(paste0("foreign_born:race_eth", race))             else 0,
    fb_decade      = if (fb == 1L && decade != ref_decade)        get_coef(paste0("foreign_born:decade", decade))             else 0,
    race_decade    = if (race != ref_race && decade != ref_decade) get_coef(paste0("race_eth", race, ":decade", decade))      else 0,
    fb_race_decade = if (fb == 1L && race != ref_race && decade != ref_decade) {
                       get_coef(paste0("foreign_born:race_eth", race, ":decade", decade))
                     } else 0,
    estimate    = fb_main + race_main + fb_race + fb_decade + race_decade + fb_race_decade,
    mult_effect = exp(estimate)
  ) |>
  ungroup() |>
  mutate(
    nativity    = factor(if_else(fb == 1L, "Foreign-born", "US-born"),
                         levels = c("US-born", "Foreign-born")),
    race        = factor(race, levels = races),
    group       = factor(
      paste(race, nativity, sep = " / "),
      levels = as.vector(outer(races, c("US-born", "Foreign-born"), paste, sep = " / "))
    ),
    decade      = as.integer(as.character(decade))
  ) |>
  arrange(race, fb, decade)

# ----- Step 4: Log-scale plot ----- #

race_colors <- c(
  "White"    = "grey40",
  "Hispanic" = "#E69F00",
  "Black"    = "#009E73",
  "AAPI"     = "#9370DB"
)

fig <- ggplot(plot_data, aes(x = decade, y = estimate, color = race, linetype = nativity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_color_manual(values = race_colors) +
  scale_linetype_manual(values = c("US-born" = "solid", "Foreign-born" = "22")) +
  labs(
    x = NULL,
    y = "Coefficient (log scale)",
    color = NULL,
    linetype = NULL,
    title = "Household size effects by race x nativity across decades",
    caption = "Pooled Poisson regression with foreign_born x race_eth x decade interaction.\nEffects relative to (White, US-born) within each decade. Age, age-at-arrival, state FE absorbed."
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

fig_exp <- ggplot(plot_data, aes(x = decade, y = mult_effect, color = race, linetype = nativity)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_color_manual(values = race_colors) +
  scale_linetype_manual(values = c("US-born" = "solid", "Foreign-born" = "22")) +
  scale_y_continuous(
    breaks = c(0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(
    x = NULL,
    y = "Multiplicative effect on household size",
    color = NULL,
    linetype = NULL,
    title = "Household size multiplicative effects by race x nativity across decades",
    caption = "exp(coefficient) from a pooled Poisson regression with foreign_born x race_eth x decade interaction.\nEffects relative to (White, US-born) within each decade. 1.0 = no effect; 1.30 = 30% more household members."
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

# ----- Step 6: Age and age-at-arrival coefficients across decades ----- #
# Per-year-of-age coefficients (controls in the main model). Plotted separately
# because the y-scale is much smaller than the race × nativity effects.
# Depends only on objects from Step 3 (coef_lookup, get_coef, decades, ref_decade),
# so this block can be re-run without re-fitting m_pooled.

age_term_labels <- c(
  AGE            = "Age",
  age_at_arrival = "Age at arrival"
)

plot_data_age <- expand_grid(
  term   = names(age_term_labels),
  decade = decades
) |>
  rowwise() |>
  mutate(
    main_coef        = get_coef(term),
    interaction_coef = if (decade == ref_decade) 0 else get_coef(paste0(term, ":decade", decade)),
    estimate    = main_coef + interaction_coef,
    mult_effect = exp(estimate)
  ) |>
  ungroup() |>
  mutate(
    term_label = factor(unname(age_term_labels[term]), levels = unname(age_term_labels)),
    decade     = as.integer(as.character(decade))
  ) |>
  arrange(term, decade)

fig_age <- ggplot(plot_data_age, aes(x = decade, y = estimate, color = term_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  labs(
    x = NULL,
    y = "Coefficient (log scale, per year of age)",
    color = NULL,
    title = "Age and age-at-arrival coefficients across decades",
    caption = "Per-year-of-age effects on household size from the same pooled Poisson regression.\nState FE absorbed. Positive = more HH members per additional year of age."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8)
  )

fig_age

write_csv(
  plot_data_age,
  "output/figures/hhsize-regression-age-coefs-over-time.csv"
)

ggsave(
  "output/figures/hhsize-regression-age-coefs-over-time.jpeg",
  plot = fig_age,
  width = 7,
  height = 5,
  dpi = 500
)

# ----- Step 7: Education coefficients across decades ----- #
# Effect of each educ_cat_simple level relative to college_4yr+ (reference) in
# the same decade. Depends only on Step 3 objects, so re-runnable without refit.

educ_levels    <- c("college_4yr+", "some_college", "hs_or_less")
ref_educ_level <- educ_levels[1]

plot_data_educ <- expand_grid(
  educ   = educ_levels,
  decade = decades
) |>
  rowwise() |>
  mutate(
    main_coef        = if (educ != ref_educ_level)                    get_coef(paste0("educ_cat_simple", educ))                   else 0,
    interaction_coef = if (educ != ref_educ_level && decade != ref_decade) get_coef(paste0("educ_cat_simple", educ, ":decade", decade)) else 0,
    estimate    = main_coef + interaction_coef,
    mult_effect = exp(estimate)
  ) |>
  ungroup() |>
  mutate(
    educ   = factor(educ, levels = educ_levels),
    decade = as.integer(as.character(decade))
  ) |>
  arrange(educ, decade)

fig_educ <- ggplot(plot_data_educ, aes(x = decade, y = estimate, color = educ)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  labs(
    x = NULL,
    y = "Coefficient (log scale)",
    color = NULL,
    title = "Education coefficients across decades",
    caption = "Effects relative to college_4yr+ within each decade. From the same pooled Poisson regression;\nstate FE absorbed; race, foreign-born, age, age-at-arrival controlled."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8)
  )

fig_educ

write_csv(
  plot_data_educ,
  "output/figures/hhsize-regression-educ-coefs-over-time.csv"
)

ggsave(
  "output/figures/hhsize-regression-educ-coefs-over-time.jpeg",
  plot = fig_educ,
  width = 7,
  height = 5,
  dpi = 500
)

# ----- Step 8: Combined 3-panel figure with shared y-axis ----- #
# Three panels side by side: (1) race x nativity, (2) age controls scaled to
# "per 10 years of age" so they share the same y-scale as the other effects,
# (3) education. All use the same coefficient (log) y-axis.
# Depends only on plot_data, plot_data_age, plot_data_educ — re-runnable without refit.

plot_data_age_decade <- plot_data_age |>
  mutate(
    estimate    = estimate * 10,       # per 10 years of age -> same scale as other log coefs
    mult_effect = exp(estimate)
  )

y_limits <- range(c(
  plot_data$estimate,
  plot_data_age_decade$estimate,
  plot_data_educ$estimate
))
y_limits <- y_limits + c(-0.03, 0.03) * diff(y_limits)

panel_theme <- theme_minimal() +
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 8),
    legend.box       = "vertical",
    legend.spacing.y = unit(-0.2, "cm"),
    plot.title       = element_text(size = 11)
  )

p_race <- ggplot(plot_data, aes(x = decade, y = estimate, color = race, linetype = nativity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  scale_color_manual(values = race_colors) +
  scale_linetype_manual(values = c("US-born" = "solid", "Foreign-born" = "22")) +
  scale_y_continuous(limits = y_limits) +
  labs(x = NULL, y = "Coefficient (log scale)", color = NULL, linetype = NULL,
       title = "Race x nativity (vs White US-born)") +
  panel_theme

p_age <- ggplot(plot_data_age_decade, aes(x = decade, y = estimate, color = term_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  scale_y_continuous(limits = y_limits) +
  labs(x = NULL, y = NULL, color = NULL,
       title = "Age controls (per 10 years)") +
  panel_theme

p_educ <- ggplot(plot_data_educ, aes(x = decade, y = estimate, color = educ)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  scale_y_continuous(limits = y_limits) +
  labs(x = NULL, y = NULL, color = NULL,
       title = "Education (vs college_4yr+)") +
  panel_theme

fig_combined <- p_race + p_age + p_educ +
  plot_layout(ncol = 3) +
  plot_annotation(
    title   = "Household size coefficients across decades (pooled model)",
    caption = "All panels share the same y-scale. Age controls multiplied by 10 to express effect per decade of age (exp(10*beta))."
  )

fig_combined

ggsave(
  "output/figures/hhsize-regression-coefs-combined-panels.jpeg",
  plot = fig_combined,
  width = 15,
  height = 5.5,
  dpi = 500
)
