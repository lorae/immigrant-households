# Produces a line graph of household size by year and nativity
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("scales")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# ----- Step 1: Graph ----- #

multifam_year_bpl <- crosstab_percent(
  data = ipums_db |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("decade", "us_born", "is_multifam"),
  percent_group_by = c("decade", "us_born")
) |> 
  filter(is_multifam) |>
  arrange(decade, us_born)

fig03 <- multifam_year_bpl |>
  filter(is_multifam) |> 
  mutate(
    year = decade,
    us_born = ifelse(us_born, "US-born", "Foreign-born")
  ) |>
  ggplot(aes(x = year, y = percent/100, color = us_born)) +   # convert to proportion
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Percent in Multifamily Households",
    color = NULL,
    title = "Percentage of Population Living in Multifamily\nHouseholds by Nativity"
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +   # adds % sign
  theme_minimal()

fig03

# ----- Step 2: Save figure ----- #
ggsave(
  filename = "output/figures/fig03-multifam-year-nativity-line.jpeg",
  plot = fig03,
  width = 6,
  height = 5,
  dpi = 500
)