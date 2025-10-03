# Plot the number of children by decade among US-born and immigrant 18-50 year olds,
# by race/ethnicity
# TODO: verify I am properly using NCHILD here: do we need to filter to only household
# heads (PERNUM = 1)?

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

nat_race_children <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2) & AGE >= 18 & AGE <= 50),
  value = "NCHILD",
  wt_col = "PERWT",
  group_by = c("decade", "us_born", "race_eth")
) |>
  arrange(race_eth, us_born, decade) |>
  filter(!(race_eth %in% c("Multiracial", "Other", "AAPI", "AIAN")))

fig13 <-nat_race_children |>
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
    y = "Number of children in household",
    color = "Race/Ethnicity",
    linetype = NULL,
    title = "Number of Own Children in Household among 18-50 year olds\nby Race/Ethnicity and Nativity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

fig13

# ----- Step 2: Save figure ----- #
ggsave(
  filename = "output/figures/fig13-nchild-year-nativity-race-line.jpeg",
  plot = fig13,
  width = 6,
  height = 5,
  dpi = 500,
  scale = 1.5
)