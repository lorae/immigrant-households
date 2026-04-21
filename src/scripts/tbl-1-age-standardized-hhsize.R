# Table 1: Age-standardized household size
#
# Outputs:
# - output/tables/tbl-1-age-standardized-hhsize.csv
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("tidyr")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Age composition by nativity × decade ----- #
# Share of each (decade, us_born) group in each age bucket.

age_share <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0, 1, 2)),
  wt_col = "PERWT",
  group_by = c("decade", "us_born", "age_bucket"),
  percent_group_by = c("decade", "us_born")
) |>
  arrange(decade, us_born, age_bucket)

age_share

# ----- Step 2: Mean household size by nativity × decade × age ----- #

hhsize_by_cell <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0, 1, 2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("decade", "us_born", "age_bucket")
) |>
  arrange(decade, us_born, age_bucket)

hhsize_by_cell

# ----- Step 3: Sumprod of HH size × age share within each group ----- #

hhsize_sumprod <- hhsize_by_cell |>
  left_join(age_share, by = c("decade", "us_born", "age_bucket")) |>
  group_by(decade, us_born) |>
  summarize(
    mean_hhsize = sum(weighted_mean * percent / 100, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(decade, us_born)

hhsize_sumprod

# ----- Step 4: Age-standardize foreign-born to US-born age distribution ----- #

# US-born age distribution per decade (the reference)
us_born_age_share <- age_share |>
  filter(us_born == TRUE) |>
  dplyr::select(decade, age_bucket, us_born_percent = percent)

# Foreign-born age-specific HH sizes per decade
fb_hhsize_cell <- hhsize_by_cell |>
  filter(us_born == FALSE) |>
  dplyr::select(decade, age_bucket, fb_mean_cell = weighted_mean)

# Apply US-born age shares to FB age-specific HH sizes
fb_age_adjusted <- fb_hhsize_cell |>
  left_join(us_born_age_share, by = c("decade", "age_bucket")) |>
  group_by(decade) |>
  summarize(
    fb_mean_adjusted = sum(fb_mean_cell * us_born_percent / 100, na.rm = TRUE),
    .groups = "drop"
  )

# Combine US-born mean, FB mean (raw), FB mean (age-adjusted), and gaps
tbl1 <- hhsize_sumprod |>
  mutate(nativity = if_else(us_born, "us_born_mean", "fb_mean_raw")) |>
  dplyr::select(decade, nativity, mean_hhsize) |>
  pivot_wider(names_from = nativity, values_from = mean_hhsize) |>
  left_join(fb_age_adjusted, by = "decade") |>
  mutate(
    gap_raw         = fb_mean_raw - us_born_mean,
    gap_adjusted    = fb_mean_adjusted - us_born_mean,
    explained_pct   = (fb_mean_raw - fb_mean_adjusted) / gap_raw * 100
  ) |>
  arrange(decade) |>
  dplyr::select(decade, us_born_mean, fb_mean_raw, fb_mean_adjusted,
                gap_raw, gap_adjusted, explained_pct)

tbl1

dbDisconnect(con)

# ----- Step 5: Save ----- #

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

write_csv(tbl1, "output/tables/tbl-1-age-standardized-hhsize.csv")
