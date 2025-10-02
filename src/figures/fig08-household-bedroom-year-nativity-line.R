# Produces a line graph of # bedrooms by year and nativity
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_person")

# ----- Step 1: Graph ----- #

bedroom_year_bpl <- crosstab_mean(
  data = ipums_db |> filter(GQ %in% c(0,1,2)),
  value = "bedroom",
  wt_col = "PERWT",
  group_by = c("decade", "us_born")
) 

fig08 <- bedroom_year_bpl |>
  mutate(
    year = decade,                       # rename decade to year
    us_born = ifelse(us_born, "US-born", "Foreign-born")
  ) |>
  ggplot(aes(x = year, y = weighted_mean, color = us_born)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Bedrooms per Household",
    color = NULL,
    title = "Number of Bedrooms by Nativity"
  ) +
  theme_minimal()

fig08

# ----- Step 2: Save figure & data ----- #

write_csv(
  bedroom_year_bpl,
  "output/figures/fig08-bedroom-year-nativity-line.csv"
)

ggsave(
  filename = "output/figures/fig08-bedroom-year-nativity-line.jpeg",
  plot = fig08,
  width = 6,
  height = 6,
  dpi = 500
)