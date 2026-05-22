# Produces a line graph of household size by year and nativity
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("ggrepel")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_person")

# ----- Step 1: Graph ----- #

hhsize_year_bpl <- crosstab_mean(
  data = ipums_db |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("decade", "us_born")
) |>
  arrange(us_born, decade)

fig01 <- hhsize_year_bpl |>
  mutate(
    year = decade,
    us_born = ifelse(us_born, "US-born", "Foreign-born")
  ) |>
  ggplot(aes(x = year, y = weighted_mean, color = us_born)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = NULL,
    y = "Persons per Household",
    color = NULL,
    title = "Household Size by Nativity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal" 
  )

fig01

# ----- Step 2: Save figure & data ----- #

write_csv(
  hhsize_year_bpl,
  "output/figures/fig01-household-size-year-nativity-line.csv"
)

ggsave(
  filename = "output/figures/fig01-household-size-year-nativity-line.jpeg",
  plot = fig01,
  width = 6,
  height = 6,
  dpi = 500
)

# Version without a title
fig01_notitle <- fig01 + labs(title = NULL)

ggsave(
  filename = "output/figures/fig01-notitle-household-size-year-nativity-line.jpeg",
  plot = fig01_notitle,
  width = 6,
  height = 3.5,
  dpi = 600,
  scale = 1.5
)

# Presentation version: no title, larger fonts, legend on the right, wider.
# Adds a third line: foreign-born household size age-standardized to the
# US-born age distribution within each decade. Read precomputed values from
# tbl-1 (src/scripts/tbl-1-age-standardized-hhsize.R must be run first to
# write the CSV). Excludes 2023.

tbl1_age_adj <- read_csv(
  "output/tables/tbl-1-age-standardized-hhsize.csv",
  show_col_types = FALSE
)

fb_adj_for_plot <- tbl1_age_adj |>
  transmute(decade, weighted_mean = fb_mean_adjusted,
            group = "Foreign-born (age-adjusted)")

pres_plot_data <- hhsize_year_bpl |>
  transmute(decade, weighted_mean,
            group = if_else(us_born, "US-born", "Foreign-born")) |>
  bind_rows(fb_adj_for_plot) |>
  filter(decade != 2023) |>
  mutate(
    year  = decade,
    group = factor(
      group,
      levels = c("US-born", "Foreign-born", "Foreign-born (age-adjusted)")
    )
  )

pres_label_data <- pres_plot_data |>
  filter(year == max(year)) |>
  mutate(label = as.character(group))

fig01_presentation <- pres_plot_data |>
  ggplot(aes(x = year, y = weighted_mean, color = group, linetype = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data               = pres_label_data,
    aes(label = label),
    hjust              = 0,
    nudge_x            = 0.5,
    nudge_y            = -0.04,
    direction          = "y",
    size               = 5,
    fontface           = "bold",
    box.padding        = 0.1,
    point.padding      = 0,
    min.segment.length = Inf,
    show.legend        = FALSE,
    seed               = 1
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "US-born"                     = "#00BFC4",
      "Foreign-born"                = "#F8766D",
      "Foreign-born (age-adjusted)" = "#F8766D"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "US-born"                     = "solid",
      "Foreign-born"                = "solid",
      "Foreign-born (age-adjusted)" = "dotted"
    )
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.35))) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "Persons\nper\nHousehold"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.y    = element_text(angle = 0, vjust = 0.5),
    plot.margin     = margin(t = 10, r = 40, b = 10, l = 10)
  )

ggsave(
  filename = "output/figures/fig01-presentation-household-size-year-nativity-line.jpeg",
  plot = fig01_presentation,
  width = 11,
  height = 5,
  dpi = 600,
  scale = 1.5
)

# Same presentation styling, but only US-born and Foreign-born (no age-adjusted).

pres_plot_data_noadj <- pres_plot_data |>
  filter(group != "Foreign-born (age-adjusted)") |>
  mutate(group = droplevels(group))

pres_label_data_noadj <- pres_plot_data_noadj |>
  filter(year == max(year)) |>
  mutate(label = as.character(group))

fig01_presentation_noadj <- pres_plot_data_noadj |>
  ggplot(aes(x = year, y = weighted_mean, color = group, linetype = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data               = pres_label_data_noadj,
    aes(label = label),
    hjust              = 0,
    nudge_x            = 0.5,
    nudge_y            = -0.04,
    direction          = "y",
    size               = 5,
    fontface           = "bold",
    box.padding        = 0.1,
    point.padding      = 0,
    min.segment.length = Inf,
    show.legend        = FALSE,
    seed               = 1
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "US-born"      = "#00BFC4",
      "Foreign-born" = "#F8766D"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "US-born"      = "solid",
      "Foreign-born" = "solid"
    )
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.35))) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "Persons\nper\nHousehold"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.y    = element_text(angle = 0, vjust = 0.5),
    plot.margin     = margin(t = 10, r = 40, b = 10, l = 10)
  )

ggsave(
  filename = "output/figures/fig01-presentation-noadj-household-size-year-nativity-line.jpeg",
  plot = fig01_presentation_noadj,
  width = 11,
  height = 5,
  dpi = 600,
  scale = 1.5
)

