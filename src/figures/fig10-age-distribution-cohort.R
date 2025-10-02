# Produces a line graph of age distribution by immigration cohort
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

age_dist_cohort <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("decade", "immig_cohort", "age_bucket"),
  percent_group_by = c("decade", "immig_cohort")
) |> 
  arrange(immig_cohort, decade, age_bucket) |>
  filter(
    (decade == 2023 & immig_cohort == "2020s") |
      (decade == 2020 & immig_cohort == "2010s") |
      (decade == 2010 & immig_cohort == "2000s") |
      (decade == 2000 & immig_cohort == "1990s") |
      (decade == 1990 & immig_cohort == "1980s") |
      (decade == 1980 & immig_cohort == "1970s") |
      (decade == 1970 & immig_cohort == "1960s")
  )

# Define desired order and colors
age_levels <- c("17 or younger", "18-29", "30-49", "50 and older")
age_colors <- c(
  "17 or younger" = "#a6cee3",  # light blue
  "18-29"         = "#1f78b4",  # blue
  "30-49"         = "#33a02c",  # green
  "50 and older"  = "#b2df8a"   # muted green
)

fig10 <- age_dist_cohort |>
  mutate(
    year = decade,
    age_bucket = factor(age_bucket, levels = rev(age_levels))
  ) |>
  ggplot(aes(x = year, y = percent/100, fill = age_bucket)) +
  geom_area(color = "white", size = 0.2, alpha = 0.9) +
  scale_fill_manual(values = age_colors) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = "Year",
    y = "Share of Cohort (percent)",
    fill = "Age group",
    title = "Age Distribution of Immigrant Cohorts Over Time"
  ) +
  theme_minimal()

fig10

# ----- Step 2: Save figure ----- #

write_csv(
  age_dist_cohort,
  "output/figures/fig10-age-dist-cohort-sand.csv"
)


ggsave(
  filename = "output/figures/fig10-age-dist-cohort-sand.jpeg",
  plot = fig10,
  width = 6,
  height = 6,
  dpi = 500 
)