# Average household size by country of origin x decade,
# age-standardized to US-born age distribution in 2020.
# Colored by continent (Americas vs Asia), country labels at right edge.
#
# Outputs:
# - output/figures/fig19-age-standardized-hhsize-by-country-decade.csv
# - output/figures/fig19-age-standardized-hhsize-by-country-decade.jpeg

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ipumsr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

# ----- Step 1: Country labels and target list ----- #

ddi_path <- list.files("data/ipums-microdata", pattern = "\\.xml$", full.names = TRUE)[1]
bpld_labels <- ipums_val_labels(read_ipums_ddi(ddi_path), "BPLD") |>
  as_tibble() |>
  rename(BPLD = val, country = lbl)

countries_of_interest <- c(
  "Mexico", "India", "Venezuela", "Cuba", "Colombia", "China",
  "Guatemala", "Honduras", "Brazil", "El Salvador",
  "Dominican Republic", "Philippines", "Korea", "Vietnam"
)

# Region lookup: N./C. America, Caribbean, South America, Asia
continent_lookup <- tibble::tribble(
  ~country,             ~continent,
  "Mexico",             "N./C. America",
  "Guatemala",          "N./C. America",
  "Honduras",           "N./C. America",
  "El Salvador",        "N./C. America",
  "Cuba",               "Caribbean",
  "Dominican Republic", "Caribbean",
  "Venezuela",          "South America",
  "Colombia",           "South America",
  "Brazil",             "South America",
  "India",              "Asia",
  "China",              "Asia",
  "Philippines",        "Asia",
  "Korea",              "Asia",
  "Vietnam",            "Asia"
)

# Display-name overrides (acronyms / short forms for the plot labels)
display_names <- c("Dominican Republic" = "D.R.")

target_labels <- bpld_labels |> filter(country %in% countries_of_interest)

# Decades of interest — drop 2023
decades_keep <- c(1970, 1980, 1990, 2000, 2010, 2020)

# ----- Step 2: Connect and build lazy frames ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# Comparison: foreign-born from target countries, country attached lazily
comparison <- ipums_person |>
  filter(
    GQ %in% c(0, 1, 2),
    !us_born,
    decade %in% decades_keep,
    !is.na(age_bucket)
  ) |>
  inner_join(target_labels, by = "BPLD", copy = TRUE)

# Reference: US-born population in 2020
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

# ----- Step 4: US-born reference line (raw mean of native-born by decade) ----- #

us_born_line <- crosstab_mean(
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
  transmute(
    country   = "US-born",
    continent = "US-born",
    decade,
    std_mean  = weighted_mean
  )

dbDisconnect(con)

# ----- Step 5: Combine and save data ----- #

plot_data <- std_hhsize |>
  select(country, decade, std_mean) |>
  left_join(continent_lookup, by = "country") |>
  bind_rows(us_born_line) |>
  filter(!is.na(std_mean), decade %in% decades_keep) |>
  arrange(continent, country, decade)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
write_csv(plot_data, "output/figures/fig19-age-standardized-hhsize-by-country-decade.csv")

# ----- Step 6: Plot ----- #

# Region colors (4-way palette, accessible)
continent_colors <- c(
  "N./C. America" = "#668df5",  # blue
  "Caribbean"     = "#00b894",  # teal/green
  "South America" = "#ff2e93",  # magenta
  "Asia"          = "#ed6a2c",  # orange
  "US-born"       = "grey50"
)

# End-of-line labels (at the rightmost decade each country appears)
label_data <- plot_data |>
  group_by(country) |>
  slice_max(decade, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(label = coalesce(display_names[country], country))

fig19 <- ggplot(
  plot_data,
  aes(x = decade, y = std_mean, group = country, color = continent)
) +
  geom_line(
    aes(linetype = continent == "US-born"),
    linewidth = 1,
    alpha = 0.85
  ) +
  geom_point(size = 1.6) +
  geom_text(
    data = label_data,
    aes(x = decade + 1.5, y = std_mean, label = label, color = continent),
    hjust = 0,
    size = 3.6,
    show.legend = FALSE
  ) +
  scale_color_manual(values = continent_colors, name = NULL) +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed"), guide = "none") +
  scale_x_continuous(
    breaks = decades_keep,
    expand = expansion(mult = c(0.02, 0))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "Average household size (age-standardized to US-born 2020)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(t = 10, r = 110, b = 10, l = 15)
  )

ggsave(
  filename = "output/figures/fig19-age-standardized-hhsize-by-country-decade.jpeg",
  plot = fig19,
  width = 11,
  height = 6.5,
  dpi = 500
)
