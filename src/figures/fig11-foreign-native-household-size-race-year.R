# Produces line graphs of foreign- versus native-born household sizes over the decades,
# split by race/ethnicity
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("scales")
library("readr")

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
  filter(
    !(race_eth %in% c("Multiracial", "Other", "AIAN")),
    decade != 2023
  )

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
      "White"    = "grey40",
      "Hispanic" = "#E69F00",
      "Black"    = "#009E73",
      "AAPI"     = "#9370DB"
    )
  ) +
  scale_linetype_manual(
    values = c("TRUE" = "solid", "FALSE" = "22"),
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

# Presentation versions ----
# Three slide-ready variants designed to read as a clean progression:
# the US-born lines occupy the same screen positions with the same line
# style on every slide; the only deliberate change is their COLOR
# (race-colored on slides 1 and 3, greyed out on slide 2 to redirect
# the audience's eye to the foreign-born lines as they appear).
# The foreign-born lines are introduced on slide 2 in their final visual
# form (dotted, slightly thicker, race-colored) and are unchanged on
# slide 3. Legends are identical across all three slides (race colors
# only) so the plot panel keeps the same aspect ratio.

race_colors_v <- c(
  "White"    = "grey40",
  "Hispanic" = "#E69F00",
  "Black"    = "#009E73",
  "AAPI"     = "#9370DB"
)

y_range_pres <- range(race_nat_hhsize$weighted_mean, na.rm = TRUE) + c(-0.1, 0.1)

# Shared styling. linewidth and linetype constants are kept in one place
# so the same values are used on every slide.
nb_linewidth <- 1.5
fb_linewidth <- 1.8
fb_linetype  <- "12"
nb_grey      <- "grey75"

pres_theme <- list(
  labs(x = NULL, y = "Persons\nper\nHousehold", color = "Race/Ethnicity"),
  scale_color_manual(values = race_colors_v),
  coord_cartesian(ylim = y_range_pres),
  theme_minimal(base_size = 20),
  theme(
    legend.position = "right",
    legend.box = "vertical",
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )
)

nb_data <- filter(race_nat_hhsize, us_born)
fb_data <- filter(race_nat_hhsize, !us_born)

# Slide 1: US-born only, race-colored solid lines.
fig11_pres_usborn <- ggplot(nb_data, aes(x = decade, y = weighted_mean, color = race_eth)) +
  geom_line(linewidth = nb_linewidth) +
  geom_point(size = 2.5) +
  pres_theme

ggsave(
  filename = "output/figures/fig11-presentation-1-usborn-only.jpeg",
  plot = fig11_pres_usborn,
  width = 11, height = 5, dpi = 600, scale = 1.5
)

# Slide 2: US-born lines unchanged in shape/position but greyed; FB lines
# introduced (dotted, race-colored, slightly thicker).
fig11_pres_emphasize_fb <- ggplot() +
  geom_line(
    data = nb_data,
    aes(x = decade, y = weighted_mean, group = race_eth),
    color = nb_grey, linewidth = nb_linewidth
  ) +
  geom_point(
    data = nb_data,
    aes(x = decade, y = weighted_mean, group = race_eth),
    color = nb_grey, size = 2.5
  ) +
  geom_line(
    data = fb_data,
    aes(x = decade, y = weighted_mean, color = race_eth),
    linetype = fb_linetype, linewidth = fb_linewidth
  ) +
  geom_point(
    data = fb_data,
    aes(x = decade, y = weighted_mean, color = race_eth),
    size = 2.8
  ) +
  pres_theme

ggsave(
  filename = "output/figures/fig11-presentation-2-emphasize-fb.jpeg",
  plot = fig11_pres_emphasize_fb,
  width = 11, height = 5, dpi = 600, scale = 1.5
)

# Slide 3 / full version: built off fig11_notitle so the original
# aes(linetype = us_born) mapping is preserved — that's what produces the
# US-born / Foreign-born linetype legend entry. Presentation styling is
# applied on top: wrapped horizontal y label, base_size 20, side legend,
# no minor gridlines, shared y range with slides 1 and 2.
fig11_presentation <- fig11_notitle +
  coord_cartesian(ylim = y_range_pres) +
  labs(y = "Persons\nper\nHousehold") +
  theme_minimal(base_size = 20) +
  theme(
    legend.position  = "right",
    legend.box       = "vertical",
    axis.title.y     = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "output/figures/fig11-presentation-3-full.jpeg",
  plot = fig11_presentation,
  width = 11, height = 5, dpi = 600, scale = 1.5
)

# Keep the original presentation filename pointing at the full version
# for backward compat.
ggsave(
  filename = "output/figures/fig11-presentation-household-size-race-nat-year-line.jpeg",
  plot = fig11_presentation,
  width = 11, height = 5, dpi = 600, scale = 1.5
)

# Hispanic-highlight variant: built on the original fig11_notitle (which uses
# aes(linetype = us_born) so the US-born/Foreign-born legend entry survives),
# with non-Hispanic race lines greyed out. Styling matches the
# fig11-presentation-3-full slide (wrapped horizontal y label, base_size 20,
# legend on the right, panel.grid.minor removed, shared y range).
# ggplot warns about replacing the color scale; that's intentional.
fig11_pres_highlight_hispanic <- fig11_notitle +
  scale_color_manual(values = c(
    "Hispanic" = "#E69F00",
    "Black"    = "grey75",
    "White"    = "grey75",
    "AAPI"     = "grey75"
  )) +
  coord_cartesian(ylim = y_range_pres) +
  labs(y = "Persons\nper\nHousehold") +
  theme_minimal(base_size = 20) +
  theme(
    legend.position  = "right",
    legend.box       = "vertical",
    axis.title.y     = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "output/figures/fig11-presentation-highlight-hispanic.jpeg",
  plot = fig11_pres_highlight_hispanic,
  width = 11, height = 5, dpi = 600, scale = 1.5
)

