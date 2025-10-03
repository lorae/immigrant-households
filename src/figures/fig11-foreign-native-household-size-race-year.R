# Produces line graphs of foreign- versus native-born household sizes over the decades,
# split by race/ethnicity
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("scales")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Graph ----- #

race_nat_hhsize <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("decade", "race_eth", "us_born")
) |>
  arrange(race_eth, us_born, decade) |>
  filter(!(race_eth %in% c("Multiracial", "Other", "AAPI", "AIAN")))

fig11 <- race_nat_hhsize |>
  mutate(
    year = decade,
    us_born_label = ifelse(us_born, "US-born", "Foreign-born")
  ) |>
  ggplot(aes(x = year, y = weighted_mean,
             color = race_eth,
             linetype = us_born)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "Hispanic" = "#1f78b4",
      "Black"    = "#33a02c",
      "White"    = "coral"
    )
  ) +
  scale_linetype_manual(
    values = c("TRUE" = "dotted", "FALSE" = "solid"),
    labels = c("Foreign-born", "US-born")
  ) +
  labs(
    x = NULL,
    y = "Persons per Household",
    color = "Race/Ethnicity",
    linetype = NULL,
    title = "Household Size by Race/Ethnicity and Nativity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

fig11

# ----- Step 2: Save figure & data ----- #

write_csv(
  race_nat_hhsize,
  "output/figures/fig11-household-size-race-nat-year-line.csv"
)

ggsave(
  filename = "output/figures/fig11-household-size-race-nat-year-line.jpeg",
  plot = fig11,
  width = 6,
  height = 6,
  dpi = 500
)

# Version without a title
fig11_notitle <- fig11 + labs(title = NULL)

ggsave(
  filename = "output/figures/fig11-notitle-household-size-race-nat-year-line.jpeg",
  plot = fig11_notitle,
  width = 6,
  height = 4,
  dpi = 600,
  scale = 1.5
)

