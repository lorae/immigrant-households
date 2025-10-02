# Produces a line graph of household size by year and nativity
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

hhsize_year_bpl <- crosstab_mean(
  data = ipums_db |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("decade", "us_born")
) |>
  arrange(us_born, decade)

fig01 <- hhsize_year_bpl |>
  mutate(
    year = decade,
    us_born = ifelse(us_born, "US-born", "Foreign-born")
  ) |>
  ggplot(aes(x = year, y = weighted_mean, color = us_born)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = NULL,
    y = "Persons per Household",
    color = NULL,
    title = "Household Size by Nativity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal" 
  )

fig01

# ----- Step 2: Save figure & data ----- #

write_csv(
  hhsize_year_bpl,
  "output/figures/fig01-household-size-year-nativity-line.csv"
)

ggsave(
  filename = "output/figures/fig01-household-size-year-nativity-line.jpeg",
  plot = fig01,
  width = 6,
  height = 6,
  dpi = 500
)

# Version without a title
fig01_notitle <- fig01 + labs(title = NULL)

ggsave(
  filename = "output/figures/fig01-notitle-household-size-year-nativity-line.jpeg",
  plot = fig01_notitle,
  width = 6,
  height = 3.5,
  dpi = 600,
  scale = 1.5
)

