# Average household size by arrival cohort x decade,
# age-standardized to US-born age distribution in 2020.
#
# Outputs:
# - output/figures/fig21-age-standardized-hhsize-by-cohort-decade.csv
# - output/figures/fig21-age-standardized-hhsize-by-cohort-decade.jpeg

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

# ----- Step 2: Connect and build lazy frames ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# Comparison: foreign-born with a known arrival cohort
comparison <- ipums_person |>
  filter(
    GQ %in% c(0, 1, 2),
    !us_born,
    decade %in% decades_keep,
    !is.na(age_bucket),
    !is.na(immig_cohort)
  )

# Reference: US-born population in 2020
reference <- ipums_person |>
  filter(GQ %in% c(0, 1, 2), us_born, decade == 2020, !is.na(age_bucket))

# ----- Step 3: Standardize by (immig_cohort, decade) ----- #
# Per-row matched reference: for each (cohort, decade) we use the US-born
# 2020 age distribution restricted to the buckets the cohort actually
# populates, renormalized to sum to 100%. This lets older cohorts (whose
# under-18 bucket is structurally empty) still be standardized against
# the buckets they have, at the cost of using a different reference per row.

# US-born 2020 reference: percentage in each age_bucket
ref_props <- crosstab_percent(
  data = reference,
  wt_col = "PERWT",
  group_by = "age_bucket",
  percent_group_by = character(0)
) |>
  select(age_bucket, ref_pct = percent)

# Foreign-born cell means by (immig_cohort, decade, age_bucket)
cell_means <- crosstab_mean(
  data = comparison,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("immig_cohort", "decade", "age_bucket"),
  every_combo = TRUE
)

# Renormalized sumprod: drop NA cells, divide by sum of used reference weights
std_hhsize <- cell_means |>
  filter(!is.na(weighted_mean)) |>
  left_join(ref_props, by = "age_bucket") |>
  group_by(immig_cohort, decade) |>
  summarize(
    std_mean = sum(weighted_mean * ref_pct) / sum(ref_pct),
    n_buckets_used = n(),
    ref_weight_used = sum(ref_pct),
    .groups = "drop"
  )

# Raw (unstandardized) mean per (immig_cohort, decade)
raw_means <- crosstab_mean(
  data = comparison,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("immig_cohort", "decade")
) |>
  select(immig_cohort, decade, raw_mean = weighted_mean, n_obs = count)

std_hhsize <- std_hhsize |>
  left_join(raw_means, by = c("immig_cohort", "decade"))

dbDisconnect(con)

# ----- Step 4: Drop incomplete / pre-arrival cohort x decade combos ----- #

plot_data <- std_hhsize |>
  filter(immig_cohort %in% cohort_levels) |>
  mutate(
    first_complete = cohort_first_complete_decade[immig_cohort],
    immig_cohort = factor(immig_cohort, levels = cohort_levels)
  ) |>
  filter(decade >= first_complete) |>
  select(-first_complete) |>
  arrange(immig_cohort, decade)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
write_csv(
  plot_data,
  "output/figures/fig21-age-standardized-hhsize-by-cohort-decade.csv"
)

print(plot_data, n = Inf)

# ----- Step 5: Plot ----- #

# Assign a segment id within each cohort so that gaps > 10 years (decades
# with no observations) break the line rather than being bridged.
plot_data_nonNA <- plot_data |>
  filter(!is.na(std_mean)) |>
  arrange(immig_cohort, decade) |>
  group_by(immig_cohort) |>
  mutate(
    gap_break  = (decade - lag(decade, default = first(decade))) > 10,
    segment_id = cumsum(gap_break)
  ) |>
  ungroup() |>
  mutate(segment = paste(immig_cohort, segment_id, sep = "_"))

fig21 <- ggplot(
  plot_data_nonNA,
  aes(x = decade, y = std_mean, color = immig_cohort, group = segment)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = rainbow(length(cohort_levels), start = 0, end = 0.85)
  ) +
  scale_x_continuous(breaks = decades_keep) +
  labs(
    x = "Year",
    y = "Persons per Household (age-standardized to US-born 2020)",
    color = "Decade of Immigration"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave(
  filename = "output/figures/fig21-age-standardized-hhsize-by-cohort-decade.jpeg",
  plot = fig21,
  width = 7,
  height = 6,
  dpi = 500
)

# ----- Step 6: Raw (unstandardized) version ----- #

fig21_raw <- ggplot(
  plot_data_nonNA,
  aes(x = decade, y = raw_mean, color = immig_cohort, group = segment)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = rainbow(length(cohort_levels), start = 0, end = 0.85)
  ) +
  scale_x_continuous(breaks = decades_keep) +
  labs(
    x = "Year",
    y = "Persons per Household (raw)",
    color = "Decade of Immigration"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave(
  filename = "output/figures/fig21b-raw-hhsize-by-cohort-decade.jpeg",
  plot = fig21_raw,
  width = 7,
  height = 6,
  dpi = 500
)
