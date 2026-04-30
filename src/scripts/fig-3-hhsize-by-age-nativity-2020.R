# Figure 3: Household size by single-year age and nativity, 2018-2022 ACS 5-year pool.
#
# Outputs:
# - output/figures/fig-3-hhsize-by-age-nativity-2020.jpeg
# - output/figures/fig-3-hhsize-by-age-nativity-2020.csv
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Mean HH size by age × nativity, 2018-2022 pooled ----- #

hhsize_by_cell <- crosstab_mean(
  data = ipums_person |>
    filter(GQ %in% c(0, 1, 2), decade == 2020, AGE <= 90),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("AGE", "us_born")
) |>
  filter(count >= 30) |>
  arrange(us_born, AGE) |>
  mutate(
    nativity = factor(
      if_else(us_born, "US-born", "Foreign-born"),
      levels = c("US-born", "Foreign-born")
    )
  )

dbDisconnect(con)

# ----- Step 2: Graph ----- #

nativity_colors <- c(
  "US-born"      = "#1f78b4",
  "Foreign-born" = "#e31a1c"
)

fig3 <- ggplot(
  hhsize_by_cell,
  aes(x = AGE, y = weighted_mean, color = nativity)
) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = nativity_colors) +
  labs(
    x = "Age",
    y = "Mean household size",
    color = NULL,
    title = "Household size by age and nativity, 2018-2022"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

fig3

# ----- Step 3: Save ----- #

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

write_csv(
  hhsize_by_cell,
  "output/figures/fig-3-hhsize-by-age-nativity-2020.csv"
)

ggsave(
  "output/figures/fig-3-hhsize-by-age-nativity-2020.jpeg",
  plot = fig3,
  width = 7,
  height = 7,
  dpi = 500
)
