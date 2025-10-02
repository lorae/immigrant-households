# Produces a line graph of multifamily households by year and immigration cohort
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("scales")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_person")

# ----- Step 1: Graph ----- #

multifam_year_cohort <- crosstab_percent(
  data = ipums_db |> filter(GQ %in% c(0,1,2), !us_born),  # only immigrants
  wt_col = "PERWT",
  group_by = c("decade", "immig_cohort", "is_multifam"),
  percent_group_by = c("decade", "immig_cohort")
) |>
  filter(is_multifam, !is.na(immig_cohort)) |>
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

fig04 <- multifam_year_cohort |>
  mutate(
    year = decade,
    immig_cohort = factor(immig_cohort, levels = cohort_levels)
  ) |>
  ggplot(aes(x = year, y = percent/100, color = immig_cohort)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = rainbow(length(cohort_levels), start = 0, end = 0.85)  # red → purple
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_x_continuous(breaks = seq(1970, 2020, by = 10)) +
  labs(
    x = "Year",
    y = "Percent of Immigrants Living in Multifamily Households",
    color = "Decade of Immigration",
    title = "Percentage of Immigrants Living in Multifamily\nHouseholds by Decade of Immigration",
    caption = "All plotted points have at least 30 observations underlying their estimate"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

fig04

# ----- Step 2: Save figure ----- #
ggsave(
  filename = "output/figures/fig04-multifam-year-immig_cohort-line.jpeg",
  plot = fig04,
  width = 6,      # in inches
  height = 6,     # in inches
  dpi = 500
)
