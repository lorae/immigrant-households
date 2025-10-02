# Produces a line graph of household size by year and immigration cohort
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_person")

# ----- Step 1: Graph ----- #

bedroom_year_cohort <- crosstab_mean(
  data = ipums_db |> filter(GQ %in% c(0,1,2)),
  value = "bedroom",
  wt_col = "PERWT",
  group_by = c("decade", "immig_cohort")
) |>
  filter(!is.na(immig_cohort)) |>
  arrange(decade, immig_cohort) |>
  filter(
    # Remove some imcomplete / misleading cohorts
    !(decade == 2020 & immig_cohort == "2020s"),
    !(decade == 2010 & immig_cohort == "2010s"),
    !(decade == 2000 & immig_cohort == "2000s"),
    !(count < 30)
  )

# Ensure cohorts are in chronological order
cohort_levels <- c(
  "1919 or earlier", "1920s", "1930s", "1940s", "1950s",
  "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s"
)

fig09 <- bedroom_year_cohort |>
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
  scale_x_continuous(breaks = seq(1970, 2020, by = 10)) +
  labs(
    x = "Year",
    y = "Bedrooms in Housing Unit",
    color = "Decade of Immigration",
    title = "Number of Bedrooms in Foreign-Born Persons' Households\nby Decade of Immigration"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

fig09

# ----- Step 2: Save figure ----- #
ggsave(
  filename = "output/figures/fig09-household-size-year-immig_cohort-line.jpeg",
  plot = fig09,
  width = 6,
  height = 6,
  dpi = 500 
)