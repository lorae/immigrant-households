# Average household size by country of origin x decade,
# age-standardized to US-born age distribution in 2020.
# Four-panel layout, one panel per region.
#
# Outputs:
# - output/figures/fig20-age-standardized-hhsize-by-country-decade-faceted.csv
# - output/figures/fig20-age-standardized-hhsize-by-country-decade-faceted.jpeg

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ipumsr")
library("ggplot2")
library("ggrepel")
library("readr")
library("tidyr")

devtools::load_all("../demographr")

# ----- Step 1: Country labels and target list ----- #

ddi_path <- list.files("data/ipums-microdata", pattern = "\\.xml$", full.names = TRUE)[1]
bpld_labels <- ipums_val_labels(read_ipums_ddi(ddi_path), "BPLD") |>
  as_tibble() |>
  rename(BPLD = val, country = lbl)

countries_of_interest <- c(
  "Mexico", "Guatemala", "Honduras", "El Salvador",
  "Cuba", "Dominican Republic",
  "Venezuela", "Colombia", "Brazil",
  "India", "China", "Philippines", "Korea", "Vietnam"
)

# Region lookup: Mexico and Central America, Caribbean, South America, Asia
continent_lookup <- tibble::tribble(
  ~country,             ~continent,
  "Mexico",             "Mexico and Central America",
  "Guatemala",          "Mexico and Central America",
  "Honduras",           "Mexico and Central America",
  "El Salvador",        "Mexico and Central America",
  "Cuba",               "Caribbean",
  "Dominican Republic", "Caribbean",
  "Venezuela",          "South America",
  "Colombia",           "South America",
  "Brazil",             "South America",
  # Asia ordered so India (anchor) and China (endpoint) sit at opposite
  # ends of the gradient — they were too similar when adjacent.
  "India",              "Asia",
  "Korea",              "Asia",
  "Philippines",        "Asia",
  "Vietnam",            "Asia",
  "China",              "Asia"
)

display_names <- c("Dominican Republic" = "D.R.")

# Per-country label placement overrides. label_decade picks which point on
# the line the label is anchored to; nudge_x / nudge_y push it away from that
# anchor in data units (decade for x, mean-hh-size for y).
#   nudge_x:  + = right, - = left
#   nudge_y:  + = up,    - = down
# Decade values must be in `decades_keep`.
label_position_overrides <- tibble::tribble(
  ~country,      ~label_decade,  ~nudge_x,  ~nudge_y,
  "Venezuela",   1990,           -1,         0.15,   # up + left
  "Colombia",    2000,            1,         0.15,   # up + right
  "Philippines", 2000,           -2,        -0.30,   # down + left (kept)
  "Vietnam",     1990,            1,         0.15,   # up + right
  "India",       1970,            1,        -0.15,   # down + right
  "China",       2010,            0,        -0.15    # directly below
)

target_labels <- bpld_labels |> filter(country %in% countries_of_interest)
decades_keep <- c(1970, 1980, 1990, 2000, 2010, 2020)
panel_continents <- c("Mexico and Central America", "Caribbean", "South America", "Asia")

# ----- Step 2: Connect and build lazy frames ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

comparison <- ipums_person |>
  filter(
    GQ %in% c(0, 1, 2),
    !us_born,
    decade %in% decades_keep,
    !is.na(age_bucket)
  ) |>
  inner_join(target_labels, by = "BPLD", copy = TRUE)

reference <- ipums_person |>
  filter(GQ %in% c(0, 1, 2), us_born, decade == 2020, !is.na(age_bucket))

# ----- Step 3: Standardize ----- #

std_hhsize <- standardize_mean(
  comparison_data = comparison,
  reference_data  = reference,
  value           = "NUMPREC",
  wt_col          = "PERWT",
  by              = c("country", "decade"),
  category        = "age_bucket",
  include_raw     = TRUE
)

# ----- Step 4: US-born reference line, replicated across panels ----- #

us_born_raw <- crosstab_mean(
  data = ipums_person |>
    filter(
      GQ %in% c(0, 1, 2),
      us_born,
      decade %in% decades_keep,
      !is.na(age_bucket)
    ),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = "decade"
) |>
  transmute(country = "US-born", decade, std_mean = weighted_mean)

dbDisconnect(con)

# Replicate US-born line into every panel
us_born_per_panel <- expand_grid(continent = panel_continents, us_born_raw)

# ----- Step 5: Combine and save data ----- #

plot_data <- std_hhsize |>
  select(country, decade, std_mean) |>
  left_join(continent_lookup, by = "country") |>
  bind_rows(us_born_per_panel) |>
  filter(!is.na(std_mean), decade %in% decades_keep) |>
  mutate(
    continent   = factor(continent, levels = panel_continents),
    color_group = if_else(country == "US-born", "US-born", country)
  ) |>
  arrange(continent, country, decade)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
write_csv(plot_data, "output/figures/fig20-age-standardized-hhsize-by-country-decade-faceted.csv")

# ----- Step 6: Plot ----- #

# Per-region two-point anchor: dark -> lighter/hue-shifted variant. Each
# country gets a distinct shade interpolated between these anchors, giving
# more within-region visual differentiation than mixing toward white alone.
continent_palettes <- list(
  "Mexico and Central America" = c("#668df5", "#a5c3ff"),
  "Caribbean"     = c("#00b894", "#6bd9bd"),
  "South America" = c("#ff2e93", "#ff8fc8"),
  "Asia"          = c("#ed6a2c", "#ffb84a")
)

make_country_palette <- function(anchors, n) {
  if (n == 1) return(anchors[1])
  colorRampPalette(anchors)(n)
}

country_palette <- continent_lookup |>
  group_by(continent) |>
  group_modify(\(df, key) {
    anchors <- continent_palettes[[key$continent]]
    df |> mutate(color = make_country_palette(anchors, n()))
  }) |>
  ungroup()

country_color_vec <- c(
  setNames(country_palette$color, country_palette$country),
  "US-born" = "grey50"
)

# Labels: most countries at rightmost decade with default rightward nudge;
# overrides set per-country anchor decade and nudge direction.
label_data <- plot_data |>
  filter(country != "US-born") |>
  group_by(country) |>
  mutate(default_decade = max(decade)) |>
  ungroup() |>
  left_join(label_position_overrides, by = "country") |>
  mutate(
    target_decade = coalesce(label_decade, default_decade),
    nudge_x       = coalesce(nudge_x, 1.2),
    nudge_y       = coalesce(nudge_y, 0)
  ) |>
  filter(decade == target_decade) |>
  mutate(label = coalesce(display_names[country], country))

# US-born label, first panel only
us_born_label_first_panel <- us_born_per_panel |>
  filter(continent == "Mexico and Central America", decade == 2020) |>
  mutate(
    color_group = "US-born",
    label       = "U.S.-born",
    nudge_x     = 1.2,
    nudge_y     = 0
  )

label_data <- bind_rows(label_data, us_born_label_first_panel)

# Force the panel order on BOTH frames going into ggplot. bind_rows above
# downgrades the factor to character when mixing factor + character columns,
# which causes facet_wrap to fall back to alphabetical order.
panel_levels <- c("Mexico and Central America", "Caribbean", "South America", "Asia")
plot_data$continent  <- factor(as.character(plot_data$continent),  levels = panel_levels)
label_data$continent <- factor(as.character(label_data$continent), levels = panel_levels)

fig20 <- ggplot(
  plot_data,
  aes(x = decade, y = std_mean, group = country, color = color_group)
) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_point(size = 1.3) +
  geom_text_repel(
    data = label_data,
    aes(x = decade, y = std_mean, label = label, color = color_group),
    nudge_x            = label_data$nudge_x,
    nudge_y            = label_data$nudge_y,
    direction          = "both",
    hjust              = 0,
    size               = 3.2,
    segment.color      = "grey60",
    segment.size       = 0.3,
    segment.alpha      = 0.7,
    min.segment.length = 0,
    box.padding        = 0.4,
    point.padding      = 0.2,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    seed               = 1
  ) +
  facet_wrap(~ continent, ncol = 2) +
  scale_color_manual(values = country_color_vec, guide = "none") +
  scale_x_continuous(
    breaks = decades_keep,
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "Average household size (age-standardized to US-born 2020)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    panel.spacing    = unit(2, "lines"),
    strip.text       = element_text(face = "bold", size = 12),
    plot.margin      = margin(t = 10, r = 20, b = 10, l = 10)
  )

ggsave(
  filename = "output/figures/fig20-age-standardized-hhsize-by-country-decade-faceted.jpeg",
  plot = fig20,
  width = 13,
  height = 9,
  dpi = 500
)
