# Average household size by arrival cohort x decade, restricted to a
# synthetic-cohort age window (people of a fixed birth-year range followed
# across decades). age_2020 = AGE + (2020 - ref_year) is the person's age
# as they would be in 2020, used as a stable cohort identifier across
# samples.
#
# Outputs: TBD

# ----- Step 0: Configuration ----- #
library("dplyr")
library("tidyr")
library("duckdb")
library("dbplyr")
library("readr")
library("ggplot2")

devtools::load_all("../demographr")

# ----- Step 1: Setup ----- #

decades_keep <- c(1970, 1980, 1990, 2000, 2010, 2020)

cohort_levels <- c(
  "1919 or earlier", "1920s", "1930s", "1940s", "1950s",
  "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s"
)

# Map each cohort to the first decade in which it can be observed as
# a "complete" arrival cohort (i.e. the decade AFTER the one in which
# they arrived). e.g. people arriving in the 2000s are first fully
# observed as a closed cohort in 2010.
cohort_first_complete_decade <- c(
  "1919 or earlier" = 1920,
  "1920s" = 1930, "1930s" = 1940, "1940s" = 1950, "1950s" = 1960,
  "1960s" = 1970, "1970s" = 1980, "1980s" = 1990, "1990s" = 2000,
  "2000s" = 2010, "2010s" = 2020, "2020s" = 2030
)

# ----- Step 2: Connect and isolate data ----- #
# MULTYEAR is the per-record interview year for ACS multi-year files and
# is NA for decennial census records, so coalesce(MULTYEAR, YEAR) gives
# the actual year the person was observed. age_2020 shifts everyone onto
# a common 2020 reference so a fixed birth cohort is identifiable across
# samples (e.g. age_2020 in [30, 39] = born 1981-1990).

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

data <- ipums_person |>
  filter(
    GQ %in% c(0, 1, 2),
    decade %in% decades_keep
  ) |>
  mutate(
    ref_year = coalesce(MULTYEAR, YEAR),
    age_2020 = AGE + (2020 - ref_year)
  )

# ----- Step 3: Define synthetic cohorts ----- #
# Each synthetic cohort is one arrival cohort restricted to a fixed
# birth-year window, named by the age window it occupied in its first
# fully-observed decade. Because age_2020 is invariant for an individual,
# a synthetic cohort is equivalently (immig_cohort, age_2020 range).
#
# Example: cohort_1970s_30s = arrived in the 1970s and aged 30-39 in 1980
# (born 1941-1950 = age_2020 in [70, 79]). This same group appears as
# 40-49 in 1990, 50-59 in 2000, 60-69 in 2010, and 70-79 in 2020.

# age_2020 window per cohort: anchor = arrival_decade + 10, age 30-39 at
# anchor → age_2020 in [2040 - arrival_decade, 2049 - arrival_decade].
# Also require decade >= anchor: April-of-anchor-year censuses capture
# early arrivers from that same decade at pre-anchor ages, which would
# otherwise contaminate the cohort with a biased fragment of itself.
data <- data |>
  mutate(
    cohort_1940s_30s = immig_cohort == "1940s" & age_2020 >= 100 & age_2020 <= 109 & decade >= 1950,
    cohort_1950s_30s = immig_cohort == "1950s" & age_2020 >= 90  & age_2020 <= 99  & decade >= 1960,
    cohort_1960s_30s = immig_cohort == "1960s" & age_2020 >= 80  & age_2020 <= 89  & decade >= 1970,
    cohort_1970s_30s = immig_cohort == "1970s" & age_2020 >= 70  & age_2020 <= 79  & decade >= 1980,
    cohort_1980s_30s = immig_cohort == "1980s" & age_2020 >= 60  & age_2020 <= 69  & decade >= 1990,
    cohort_1990s_30s = immig_cohort == "1990s" & age_2020 >= 50  & age_2020 <= 59  & decade >= 2000,
    cohort_2000s_30s = immig_cohort == "2000s" & age_2020 >= 40  & age_2020 <= 49  & decade >= 2010,
    cohort_2010s_30s = immig_cohort == "2010s" & age_2020 >= 30  & age_2020 <= 39  & decade >= 2020
  )

# ----- Step 4: Aggregate hhsize by (synth cohort, decade) ----- #
# Each person belongs to at most one synthetic cohort (mutually exclusive
# by construction), so we collapse the eight indicators to one categorical
# column and weight-average NUMPREC per (synth_cohort, decade).

synth_cohort_levels <- c("1940s", "1950s", "1960s", "1970s",
                         "1980s", "1990s", "2000s", "2010s")

cohort_data <- data |>
  mutate(
    synth_cohort = case_when(
      cohort_1940s_30s ~ "1940s",
      cohort_1950s_30s ~ "1950s",
      cohort_1960s_30s ~ "1960s",
      cohort_1970s_30s ~ "1970s",
      cohort_1980s_30s ~ "1980s",
      cohort_1990s_30s ~ "1990s",
      cohort_2000s_30s ~ "2000s",
      cohort_2010s_30s ~ "2010s"
    )
  ) |>
  filter(!is.na(synth_cohort))

agg <- crosstab_mean(
  data = cohort_data,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("synth_cohort", "decade")
)

dbDisconnect(con)

plot_data <- agg |>
  mutate(synth_cohort = factor(synth_cohort, levels = synth_cohort_levels)) |>
  arrange(synth_cohort, decade)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
write_csv(plot_data, "output/figures/fig21-synthetic-cohort-30s-hhsize.csv")

print(plot_data, n = Inf)

# ----- Step 5: Plot ----- #

fig21 <- ggplot(
  plot_data,
  aes(x = decade, y = weighted_mean, color = synth_cohort, group = synth_cohort)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = rainbow(length(synth_cohort_levels), start = 0, end = 0.85)
  ) +
  scale_x_continuous(breaks = decades_keep) +
  labs(
    x = "Year",
    y = "Persons per household",
    color = "Arrival cohort"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave(
  filename = "output/figures/fig21-synthetic-cohort-30s-hhsize.jpeg",
  plot = fig21,
  width = 7,
  height = 6,
  dpi = 500
)
