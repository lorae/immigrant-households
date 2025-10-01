# Produces a line graph of household size by year and immigration cohort
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# ----- Step 1: Graph ----- #

hhsize_year_cohort <- crosstab_mean(
  data = ipums_db |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("decade", "immig_cohort")
) |>
  filter(!is.na(immig_cohort))

# Ensure cohorts are in chronological order
cohort_levels <- c(
  "1919 or earlier", "1920s", "1930s", "1940s", "1950s",
  "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s"
)

fig_cohort <- hhsize_year_cohort |>
  mutate(
    year = decade,
    immig_cohort = factor(immig_cohort, levels = cohort_levels)
  ) |>
  ggplot(aes(x = year, y = weighted_mean, color = immig_cohort)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = rainbow(length(cohort_levels), start = 0, end = 0.85)  # red → purple
  ) +
  labs(
    x = "Year",
    y = "Average Household Size",
    color = "Decade of Immigration",
    title = "Household Size Among Immigrants by Decade of Immigration"
  ) +
  theme_minimal()

fig_cohort
  
# ----- Step 2: Save figure ----- #

# TODO