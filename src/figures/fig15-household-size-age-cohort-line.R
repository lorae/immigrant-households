# Produces a line graph of household size by age and immigration cohort
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Graph ----- #

hhsize_age_cohort <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2), !is.na(immig_cohort)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("AGE", "immig_cohort")
) |>
  arrange(immig_cohort, AGE) |>
  filter(!(count < 30))

# Ensure cohorts are in chronological order
cohort_levels <- c(
  "1919 or earlier", "1920s", "1930s", "1940s", "1950s",
  "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s"
)

fig15 <- hhsize_age_cohort |>
  mutate(
    immig_cohort = factor(immig_cohort, levels = cohort_levels)
  ) |>
  ggplot(aes(x = AGE, y = weighted_mean, color = immig_cohort)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = rainbow(length(cohort_levels), start = 0, end = 0.85)
  ) +
  labs(
    x = "Age",
    y = "Persons per Household",
    color = "Decade of Immigration",
    title = "Household Size by Age and Immigrant Cohort"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

fig15

# ----- Step 2: Save figure & data ----- #

write_csv(
  hhsize_age_cohort,
  "output/figures/fig15-household-size-age-cohort-line.csv"
)

ggsave(
  filename = "output/figures/fig15-household-size-age-cohort-line.jpeg",
  plot = fig15,
  width = 6,
  height = 6,
  dpi = 500
)
