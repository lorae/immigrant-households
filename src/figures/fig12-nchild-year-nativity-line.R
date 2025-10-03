# Plot the number of children by decade among US-born and immigrant 18-50 year olds
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

nat_children <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2) & AGE >= 18 & AGE <= 50),
  value = "NCHILD",
  wt_col = "PERWT",
  group_by = c("decade", "us_born")
) |>
  arrange(us_born, decade)

fig12 <- ggplot(nat_children, aes(x = decade, y = weighted_mean, color = us_born)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    name = NULL,
    values = c("TRUE" = "#1f78b4", "FALSE" = "#33a02c"),
    labels = c("Foreign-born", "U.S.-born")
  ) +
  labs(
    x = NULL,
    y = "Number of children in household",
    title = "Number of Own Children Living in Household Among 18-50\nYear Olds, by Nativity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

fig12

# ----- Step 2: Save figure & data ----- #

write_csv(
  nat_children,
  "output/figures/fig12-nchild-year-nativity-line.csv"
)

ggsave(
  filename = "output/figures/fig12-nchild-year-nativity-line.jpeg",
  plot = fig12,
  width = 6,
  height = 6,
  dpi = 500
)

# Version without a title
fig12_notitle <- fig12 + labs(title = NULL)

ggsave(
  filename = "output/figures/fig12-notitle-nchild-year-nativity-line.jpeg",
  plot = fig12_notitle,
  width = 6,
  height = 6,
  dpi = 600,
  scale = 1.5
)
