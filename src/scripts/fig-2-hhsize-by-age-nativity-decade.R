# Figure 2: Household size by nativity, age bucket, and decade.
#
# Outputs:
# - output/figures/fig-2-hhsize-by-age-nativity-decade.jpeg
# - output/figures/fig-2-hhsize-by-age-nativity-decade.csv
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

# ----- Step 1: Mean HH size by decade × nativity × age bucket ----- #

age_levels <- c("17 or younger", "18-29", "30-49", "50 and older")

hhsize_by_cell <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0, 1, 2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("decade", "us_born", "age_bucket")
) |>
  arrange(decade, us_born, age_bucket) |>
  mutate(
    age_bucket = factor(age_bucket, levels = age_levels),
    nativity = factor(
      if_else(us_born, "US-born", "Foreign-born"),
      levels = c("US-born", "Foreign-born")
    )
  )

dbDisconnect(con)

# ----- Step 2: Graph ----- #

age_colors <- c(
  "17 or younger" = "#ff8fc8",
  "18-29"         = "#ffb84a",
  "30-49"         = "#6bd9bd",
  "50 and older"  = "#a5c3ff"
)

fig2 <- ggplot(
  hhsize_by_cell,
  aes(
    x = decade,
    y = weighted_mean,
    color = age_bucket,
    linetype = nativity,
    group = interaction(age_bucket, nativity)
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = age_colors) +
  scale_linetype_manual(values = c("Foreign-born" = "solid", "US-born" = "12")) +
  labs(
    x = NULL,
    y = "Mean household size",
    color = "Age group",
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

fig2

# ----- Step 3: Save ----- #

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

write_csv(
  hhsize_by_cell,
  "output/figures/fig-2-hhsize-by-age-nativity-decade.csv"
)

ggsave(
  "output/figures/fig-2-hhsize-by-age-nativity-decade.jpeg",
  plot = fig2,
  width = 7,
  height = 7,
  dpi = 500
)
