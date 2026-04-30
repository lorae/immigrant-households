# Bump/step chart of country-of-origin rank within each immigrant cohort.
# Reads the long CSV produced by fig16-top-countries-by-cohort-table.R.
#
# Countries that appear in the 2020s top 10 are drawn as thick translucent
# lines so the contemporary leaders stand out against historical trajectories.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("ggplot2")
library("readr")
library("scales")

top10 <- read_csv(
  "output/figures/fig16-top-countries-by-cohort-table-long.csv",
  show_col_types = FALSE
)

# ----- Step 1: Cohort -> numeric x ----- #

cohort_levels <- c(
  "1919 or earlier", "1920s", "1930s", "1940s", "1950s",
  "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s"
)
cohort_midpoints <- c(1915, 1925, 1935, 1945, 1955, 1965, 1975, 1985, 1995, 2005, 2015, 2025)
names(cohort_midpoints) <- cohort_levels

top10 <- top10 |>
  mutate(
    immig_cohort = factor(immig_cohort, levels = cohort_levels),
    cohort_x = cohort_midpoints[as.character(immig_cohort)]
  )

# ----- Step 2: Highlight countries in the 2020s top 10 ----- #

highlight_countries <- top10 |>
  filter(immig_cohort == "2020s") |>
  pull(country) |>
  unique()

# Notable historical countries that get labels even though they're not in the
# current top 10 — drawn a step up from the dotted "other" class so they stand
# out next to their labels.
notable_past <- c("Germany", "Philippines", "Vietnam", "Korea",
                  "Canada", "Italy", "England", "Dominican Republic")

top10 <- top10 |>
  mutate(
    highlight = country %in% highlight_countries,
    line_class = factor(
      case_when(
        highlight                  ~ "Current top 10",
        country %in% notable_past  ~ "Notable past",
        TRUE                       ~ "Other previous"
      ),
      levels = c("Current top 10", "Notable past", "Other previous")
    )
  )

# ----- Step 3: Region-based palette, unique color per country ----- #

region_lookup <- tibble::tribble(
  ~country,              ~region,
  "Mexico",              "Americas",
  "Canada",              "Americas",
  "Cuba",                "Americas",
  "El Salvador",         "Americas",
  "Guatemala",           "Americas",
  "Honduras",            "Americas",
  "Brazil",              "Americas",
  "Colombia",            "Americas",
  "Venezuela",           "Americas",
  "Dominican Republic",  "Americas",
  "Jamaica",             "Americas",
  "India",               "Asia",
  "China",               "Asia",
  "Vietnam",             "Asia",
  "Korea",               "Asia",
  "Philippines",         "Asia",
  "Japan",               "Asia",
  "Italy",               "Europe",
  "Germany",             "Europe",
  "West Germany",        "Europe",
  "Poland",              "Europe",
  "Ireland",             "Europe",
  "England",             "Europe",
  "Scotland",            "Europe",
  "Austria",             "Europe",
  "Other USSR/Russia",   "Europe",
  "USSR, ns",            "Europe",
  "Czechoslovakia",      "Europe",
  "Greece",              "Europe",
  "Hungary",             "Europe",
  "Abroad, ns",          "Other"
)

top10 <- top10 |>
  left_join(region_lookup, by = "country") |>
  mutate(region = ifelse(is.na(region), "Other", region))

# Within each region, order countries by their best (lowest) historical rank
# so dominant senders get the most-saturated shade.
country_order <- top10 |>
  group_by(region, country) |>
  summarise(best_rank = min(rank), .groups = "drop") |>
  arrange(region, best_rank)

make_palette <- function(dark, light, n) {
  if (n == 1) dark else colorRampPalette(c(dark, light))(n)
}

palette_by_region <- country_order |>
  group_by(region) |>
  group_modify(\(df, key) {
    pal <- switch(
      key$region,
      "Americas" = make_palette("#668df5", "#b3c6fa", nrow(df)),  # blue -> light blue
      "Asia"     = make_palette("#ed6a2c", "#f6b596", nrow(df)),  # orange -> light orange
      "Europe"   = make_palette("#ff2e93", "#ffa3c9", nrow(df)),  # bright magenta -> light pink
      "Other"    = rep("grey60", nrow(df))
    )
    df |> mutate(color = pal)
  }) |>
  ungroup()

color_vec <- setNames(palette_by_region$color, palette_by_region$country)

# ----- Step 4: Spline-interpolate ranks for smooth curves ----- #

country_attrs <- top10 |> distinct(country, line_class, region, highlight)

top10_smooth <- top10 |>
  arrange(country, cohort_x) |>
  group_by(country) |>
  group_modify(\(df, key) {
    if (nrow(df) < 2) {
      tibble(cohort_x = df$cohort_x, rank = df$rank)
    } else {
      f <- stats::splinefun(df$cohort_x, df$rank, method = "monoH.FC")
      xs <- seq(min(df$cohort_x), max(df$cohort_x), length.out = 100)
      tibble(cohort_x = xs, rank = f(xs))
    }
  }) |>
  ungroup() |>
  left_join(country_attrs, by = "country")

# ----- Step 5: Labels at the right edge of each country's line ----- #

label_data <- top10 |>
  filter(highlight | country %in% notable_past) |>
  group_by(country) |>
  slice_max(cohort_x, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    display_label = if_else(country == "Dominican Republic", "D.R.", country),
    label_y = rank + case_when(
      country == "Philippines" ~ 0.2,
      TRUE ~ 0
    )
  )

# ----- Step 6: Plot ----- #

single_obs <- top10 |>
  group_by(country) |>
  filter(n() == 1) |>
  ungroup()

fig16b <- ggplot(top10_smooth, aes(x = cohort_x, y = rank, group = country, color = country)) +
  geom_line(aes(linetype = line_class, linewidth = line_class, alpha = line_class)) +
  geom_point(data = single_obs, size = 1.6, show.legend = FALSE) +
  geom_text(
    data = label_data,
    aes(x = cohort_x + 1.2, y = label_y, label = display_label, color = country),
    hjust = 0,
    size = 4.2,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = color_vec,
    breaks = c("Mexico", "China", "Italy"),
    labels = c("Americas", "Asia", "Europe"),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Current top 10" = "solid",
      "Notable past"   = "solid",
      "Other previous" = "dotted"
    ),
    name = "Style"
  ) +
  scale_linewidth_manual(
    values = c(
      "Current top 10" = 3,
      "Notable past"   = 0.7,
      "Other previous" = 0.4
    ),
    name = "Style"
  ) +
  scale_alpha_manual(
    values = c(
      "Current top 10" = 0.45,
      "Notable past"   = 0.45,
      "Other previous" = 1
    ),
    name = "Style"
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linetype = "solid", linewidth = 2, alpha = 1)
    ),
    linetype = guide_legend(
      order = 2,
      title = NULL,
      override.aes = list(color = "grey30")
    ),
    linewidth = guide_legend(order = 2, title = NULL),
    alpha     = guide_legend(order = 2, title = NULL)
  ) +
  scale_y_reverse(breaks = 1:10) +
  scale_x_continuous(
    breaks = cohort_midpoints,
    labels = cohort_levels,
    expand = expansion(mult = c(0.02, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "Rank within cohort"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.margin = margin(t = 15, r = 130, b = 10, l = 20)
  )

fig16b

# ----- Step 7: Save ----- #

write_csv(
  top10 |>
    select(immig_cohort, rank, country, pop, region, highlight, line_class),
  "output/figures/fig16b-rank-trajectories-by-cohort.csv"
)

ggsave(
  filename = "output/figures/fig16b-rank-trajectories-by-cohort.jpeg",
  plot = fig16b,
  width = 11,
  height = 7,
  dpi = 500
)
