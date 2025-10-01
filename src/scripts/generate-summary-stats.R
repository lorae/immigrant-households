# generate-summary-stats.R
#
# Produce basic facts about immigrants in the United States since the 1970s

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# ----- Step 1: Household size by year and foreign vs us born ----- #

hhsize_year_bpl <- crosstab_mean(
  data = ipums_db |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("decade", "us_born")
) 

hhsize_year_bpl |>
  mutate(
    year = decade,                       # rename decade to year
    us_born = ifelse(us_born, "US-born", "Foreign-born")
  ) |>
  ggplot(aes(x = year, y = weighted_mean, color = us_born)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Average Household Size",
    color = NULL,
    title = "Household Size by Nativity"
  ) +
  theme_minimal()

