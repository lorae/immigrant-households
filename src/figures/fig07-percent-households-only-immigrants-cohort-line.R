# Produces a line graph of the percentage of American immigrant households with only immigrants

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Graph ----- #

all_foreign_born_cohort <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "HHWT",
  group_by = c("decade", "immig_cohort", "any_foreign_born", "all_foreign_born"),
  percent_group_by = c("decade", "immig_cohort", "any_foreign_born")
) |> 
  arrange(decade) |>
  filter(any_foreign_born) |>
  filter(all_foreign_born)

fig06 <- all_foreign_born |>
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
    y = "Percent of Immigrant Households",
    title = "Percentage of Immigrant Households with at\nOnly Immigrant Members"
  ) +
  theme_minimal()

fig06

# ----- Step 2: Save figure ----- #

ggsave(
  filename = "output/figures/fig06-percent-foreign-born-households-only-foreign-born.jpeg",
  plot = fig06,
  width = 6,
  height = 6,
  dpi = 500
)