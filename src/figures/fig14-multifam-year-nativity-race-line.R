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
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Graph ----- #

multifam_bpl_race <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("decade", "us_born", "is_multifam", "race_eth"),
  percent_group_by = c("decade", "us_born", "race_eth")
) |> 
  filter(is_multifam) |>
  arrange(race_eth, decade, us_born) |>
  filter(!(race_eth %in% c("Multiracial", "Other", "AAPI", "AIAN")))

fig14 <- multifam_bpl_race |>
  mutate(
    year = decade,
    us_born_label = ifelse(us_born, "US-born", "Foreign-born")
  ) |>
  ggplot(aes(x = year, y = percent/100,
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
    y = "PErcentage Multifam",
    color = "Race/Ethnicity",
    linetype = NULL,
    title = "Percentage of Individuals in Doubled-Up Housing by Race and Nativity"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +   # adds % sign
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

fig14

# ----- Step 2: Save figure ----- #
ggsave(
  filename = "output/figures/fig14-multifam-year-nativity-race-line.jpeg",
  plot = fig14,
  width = 6,
  height = 5,
  dpi = 500,
  scale = 1.5
)