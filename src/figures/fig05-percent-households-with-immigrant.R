# Produces a line graph of the percentage of American households with at least one immigrant
# And, within those households, the percentage of occupants that are immigrants
# (do this by cohort to see if there is increased integration over time)
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
devtools::load_all("../demographr")

# ----- Step 1: Connect to the database ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_household <- tbl(con, "ipums_household")

foreign_born_in_household <- crosstab_percent(
  data = ipums_household |> filter(GQ %in% c(0,1,2)),
  wt_col = "HHWT",
  group_by = c("decade", "any_foreign_born"),
  percent_group_by = c("decade")
) |> 
  arrange(decade) |>
  filter(any_foreign_born)
  
fig05 <- foreign_born_in_household |>
  filter(any_foreign_born) |> 
  mutate(year = decade) |>
  ggplot(aes(x = year, y = percent/100)) +
  geom_line(color = "firebrick", linewidth = 1.2) +
  geom_point(color = "firebrick", size = 2) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_x_continuous(
    breaks = c(1970, 1980, 1990, 2000, 2010, 2020),
    labels = c("1970", "1980", "1990", "2000", "2010", "2020")
  ) +
  labs(
    x = "Year",
    y = "Percent of Households",
    title = "Percentage of Households with at\nLeast One Foreign-born Member"
  ) +
  theme_minimal()

fig05

# Save figure
ggsave(
  filename = "output/figures/fig05-percent-foreign-born-households.jpeg",
  plot = fig05,
  width = 6,
  height = 6,
  dpi = 500
)