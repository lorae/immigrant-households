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

# ----- Step 3: Assign synthetic cohort + nativity ----- #
# synth_cohort labels each row with the arrival decade of its synthetic
# cohort (e.g. "1980s"). US-born controls share the cohort label of the
# foreign-born group they match on age_2020 window and observation
# decades, with nativity carrying the foreign/US distinction.
#
# age_2020 window: anchor = arrival_decade + 10, age 30-39 at anchor
# → age_2020 in [2040 - arrival_decade, 2049 - arrival_decade].
# Also require decade >= anchor: April-of-anchor-year censuses capture
# early arrivers from that same decade at pre-anchor ages, which would
# otherwise contaminate the cohort with a biased fragment of itself.

data <- data |>
  mutate(
    synth_cohort = case_when(
      (immig_cohort == "1940s" | us_born) & age_2020 >= 100 & age_2020 <= 109 & decade >= 1950 ~ "1940s",
      (immig_cohort == "1950s" | us_born) & age_2020 >= 90  & age_2020 <= 99  & decade >= 1960 ~ "1950s",
      (immig_cohort == "1960s" | us_born) & age_2020 >= 80  & age_2020 <= 89  & decade >= 1970 ~ "1960s",
      (immig_cohort == "1970s" | us_born) & age_2020 >= 70  & age_2020 <= 79  & decade >= 1980 ~ "1970s",
      (immig_cohort == "1980s" | us_born) & age_2020 >= 60  & age_2020 <= 69  & decade >= 1990 ~ "1980s",
      (immig_cohort == "1990s" | us_born) & age_2020 >= 50  & age_2020 <= 59  & decade >= 2000 ~ "1990s",
      (immig_cohort == "2000s" | us_born) & age_2020 >= 40  & age_2020 <= 49  & decade >= 2010 ~ "2000s",
      (immig_cohort == "2010s" | us_born) & age_2020 >= 30  & age_2020 <= 39  & decade >= 2020 ~ "2010s"
    ),
    nativity = if_else(us_born, "US-born", "Foreign-born")
  )

# ----- Step 4: Aggregate hhsize by (synth_cohort, decade, nativity) ----- #

arrival_levels <- c("1940s", "1950s", "1960s", "1970s",
                    "1980s", "1990s", "2000s", "2010s")

cohort_data <- data |> filter(!is.na(synth_cohort))

agg <- crosstab_mean(
  data = cohort_data,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("synth_cohort", "decade", "nativity")
)

dbDisconnect(con)

plot_data <- agg |>
  mutate(
    synth_cohort = factor(synth_cohort, levels = arrival_levels),
    nativity = factor(nativity, levels = c("Foreign-born", "US-born"))
  ) |>
  arrange(synth_cohort, nativity, decade)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
write_csv(plot_data, "output/figures/fig21-synthetic-cohort-30s-hhsize.csv")

print(plot_data, n = Inf)

# ----- Step 5: Plot ----- #

fig21 <- ggplot(
  plot_data,
  aes(
    x = decade, y = weighted_mean,
    color = synth_cohort,
    linetype = nativity, linewidth = nativity, alpha = nativity,
    group = interaction(synth_cohort, nativity)
  )
) +
  geom_line() +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(
      "1940s" = "#E88DFF",
      "1950s" = "#ff8fc8",
      "1960s" = "#ffa489",
      "1970s" = "#ffb84a",
      "1980s" = "#b5c984",
      "1990s" = "#6bd9bd",
      "2000s" = "#83daf1",
      "2010s" = "#a5c3ff"
    )
  ) +
  scale_linetype_manual(values = c("Foreign-born" = "solid",  "US-born" = "12")) +
  scale_linewidth_manual(values = c("Foreign-born" = 1.4,      "US-born" = 0.8)) +
  scale_alpha_manual(values = c("Foreign-born" = 1,            "US-born" = 1)) +
  scale_x_continuous(breaks = decades_keep) +
  labs(
    x = "Year",
    y = "Persons per household",
    color = "Arrival cohort",
    linetype = "Nativity",
    linewidth = "Nativity",
    alpha = "Nativity"
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
