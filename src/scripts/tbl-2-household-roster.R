# Table 2: Household roster composition per person
#
# For each person, counts within their household:
#  - number of spouses (0 or 1)
#  - number of children (< 18)
#  - number of seniors (65+)
#  - number of other adults (not otherwise counted)
#
# Outputs:
# - output/tables/tbl-2-household-roster.csv
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# Non-institutional only, per project convention
ipums_ni <- ipums_person |> filter(GQ %in% c(0, 1, 2))

# ----- Step 1: Household-level counts by age group ----- #
# One row per household (YEAR, SERIAL) with counts of kids, seniors, and adults.

hh_counts <- ipums_ni |>
  group_by(YEAR, SERIAL) |>
  summarize(
    n_kids_hh       = sum(as.integer(AGE < 18), na.rm = TRUE),
    n_seniors_hh    = sum(as.integer(AGE >= 65), na.rm = TRUE),
    n_adults1864_hh = sum(as.integer(AGE >= 18 & AGE < 65), na.rm = TRUE),
    .groups = "drop"
  )

# ----- Step 2: Spouse's age via self-join on (YEAR, SERIAL, SPLOC → PERNUM) ----- #
# Needed so we don't double-count: if my spouse is 65+, they're already in n_seniors_hh.

spouse_ages <- ipums_ni |>
  dplyr::select(YEAR, SERIAL, PERNUM, spouse_age = AGE)

# ----- Step 3: Person-level roster ----- #
# Each person gets: n_spouses, n_children, n_seniors, n_other_adults — all EXCLUDING self.
# The four counts + self = NUMPREC by construction.

# If the materialized roster table exists, use it. Otherwise build and persist it
# so subsequent runs skip the expensive self-join.
if ("tbl2_roster" %in% DBI::dbListTables(con)) {
  roster <- tbl(con, "tbl2_roster")
} else {
  roster <- ipums_ni |>
    left_join(hh_counts, by = c("YEAR", "SERIAL")) |>
    left_join(
      spouse_ages,
      by = c("YEAR" = "YEAR", "SERIAL" = "SERIAL", "SPLOC" = "PERNUM")
    ) |>
    mutate(
      self_is_kid         = as.integer(AGE < 18),
      self_is_senior      = as.integer(AGE >= 65),
      self_is_adult1864   = as.integer(AGE >= 18 & AGE < 65),

      has_spouse          = as.integer(SPLOC > 0 & !is.na(spouse_age)),
      spouse_is_kid       = as.integer(has_spouse == 1L & spouse_age < 18),
      spouse_is_senior    = as.integer(has_spouse == 1L & spouse_age >= 65),
      spouse_is_adult1864 = as.integer(has_spouse == 1L &
                                         spouse_age >= 18 & spouse_age < 65),

      n_spouses       = has_spouse,
      n_children      = n_kids_hh       - self_is_kid       - spouse_is_kid,
      n_seniors       = n_seniors_hh    - self_is_senior    - spouse_is_senior,
      n_other_adults  = n_adults1864_hh - self_is_adult1864 - spouse_is_adult1864
    ) |>
    compute(name = "tbl2_roster", temporary = FALSE)
}

# ----- Step 4: Validation (uncomment after first successful run) ----- #
# Every row should have: n_spouses + n_children + n_seniors + n_other_adults + 1 == NUMPREC.
#
# roster |>
#   mutate(check = n_spouses + n_children + n_seniors + n_other_adults + 1L - NUMPREC) |>
#   summarize(
#     min_diff = min(check, na.rm = TRUE),
#     max_diff = max(check, na.rm = TRUE)
#   )

# ----- Step 5: Weighted means for ADULTS, by decade × race × nativity ----- #
# Restricted to adults (AGE >= 18), the four focal race/eth groups, excluding 2023.

roster_summary <- roster |>
  filter(
    decade != 2023,
    AGE >= 18,
    race_eth %in% c("White", "Black", "Hispanic", "AAPI")
  ) |>
  group_by(decade, race_eth, us_born) |>
  summarize(
    weight_total       = sum(PERWT, na.rm = TRUE),
    wsum_spouses       = sum(n_spouses * PERWT, na.rm = TRUE),
    wsum_children      = sum(n_children * PERWT, na.rm = TRUE),
    wsum_seniors       = sum(n_seniors * PERWT, na.rm = TRUE),
    wsum_other_adults  = sum(n_other_adults * PERWT, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Children     = wsum_children     / weight_total,
    Spouses      = wsum_spouses      / weight_total,
    Other_adults = wsum_other_adults / weight_total,
    Seniors      = wsum_seniors      / weight_total
  ) |>
  dplyr::select(decade, race_eth, us_born, Children, Spouses, Other_adults, Seniors) |>
  collect()

dbDisconnect(con)

# ----- Step 6: Reshape to Table 2 layout ----- #
# Rows: race × component (Children, Spouses, Other adults, Seniors)
# Columns: decade × nativity (NB, FB) — 12 value columns

library("tidyr")

tbl2 <- roster_summary |>
  pivot_longer(
    cols      = c(Children, Spouses, Other_adults, Seniors),
    names_to  = "component",
    values_to = "mean"
  ) |>
  mutate(
    nativity  = if_else(us_born, "NB", "FB"),
    col_label = paste0("d", decade, "_", nativity)
  ) |>
  pivot_wider(
    id_cols     = c(race_eth, component),
    names_from  = col_label,
    values_from = mean
  ) |>
  mutate(
    race_eth  = factor(race_eth,  levels = c("White", "Black", "Hispanic", "AAPI")),
    component = factor(component, levels = c("Children", "Spouses", "Other_adults", "Seniors"))
  ) |>
  arrange(race_eth, component) |>
  dplyr::select(
    race_eth, component,
    d1970_NB, d1970_FB,
    d1980_NB, d1980_FB,
    d1990_NB, d1990_FB,
    d2000_NB, d2000_FB,
    d2010_NB, d2010_FB,
    d2020_NB, d2020_FB
  )

tbl2

# ----- Step 7: Save ----- #

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(tbl2, "output/tables/tbl-2-household-roster.csv")

# ----- Step 8: Stacked bar chart by nativity × decade, faceted by race ----- #
# Within each race facet: bars per decade, NB and FB side-by-side (FB more translucent),
# each bar stacked by component (Children / Spouses / Other adults / Seniors).

library("ggplot2")

plot_data <- roster_summary |>
  pivot_longer(
    cols      = c(Children, Spouses, Other_adults, Seniors),
    names_to  = "component",
    values_to = "mean"
  ) |>
  mutate(
    nativity = factor(
      if_else(us_born, "US-born", "Foreign-born"),
      levels = c("US-born", "Foreign-born")
    ),
    race_eth = factor(race_eth, levels = c("AAPI", "Black", "Hispanic", "White")),
    component = factor(
      component,
      levels = c("Children", "Spouses", "Other_adults", "Seniors"),
      labels = c("Children", "Spouses", "Other adults", "Seniors")
    ),
    # Shift each decade by ±2 so NB and FB bars sit side-by-side within each decade.
    x_pos = decade + if_else(us_born, -2.2, 2.2)
  )

fig_tbl2 <- ggplot(plot_data, aes(x = x_pos, y = mean, fill = component, alpha = nativity)) +
  geom_col(width = 4, color = "grey30", linewidth = 0.15) +
  facet_wrap(~race_eth, ncol = 2) +
  scale_fill_manual(values = c(
    "Children"     = "#66C2A5",  # teal
    "Spouses"      = "#FC8D62",  # orange
    "Other adults" = "#8DA0CB",  # lavender-blue
    "Seniors"      = "#E78AC3"   # pink
  )) +
  scale_alpha_manual(values = c("US-born" = 1.0, "Foreign-born" = 0.55)) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    x = NULL,
    y = "Mean household co-residents (adults)",
    fill = NULL,
    alpha = NULL,
    title = "Household composition by race, nativity, and decade"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

fig_tbl2

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

ggsave(
  "output/figures/tbl-2-household-roster-stacked.jpeg",
  plot = fig_tbl2,
  width = 9,
  height = 7,
  dpi = 500
)

# ----- Step 9: Stacked area, normalized to 100%, faceted by race x nativity ----- #

fig_tbl2_area <- ggplot(
  plot_data,
  aes(x = decade, y = mean, fill = component)
) +
  geom_area(position = "fill") +
  facet_grid(rows = vars(nativity), cols = vars(race_eth)) +
  scale_fill_manual(values = c(
    "Children"     = "#66C2A5",
    "Spouses"      = "#FC8D62",
    "Other adults" = "#8DA0CB",
    "Seniors"      = "#E78AC3"
  )) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    x = NULL,
    y = "Share of mean household co-residents (adults)",
    fill = NULL,
    title = "Household composition shares by race, nativity, and decade"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "output/figures/tbl-2-household-roster-area.jpeg",
  plot = fig_tbl2_area,
  width = 10,
  height = 6,
  dpi = 500
)

# ----- Step 10: Exploded bar chart, foreign-born only ----- #
# Rows = component, columns = race. Bars are NOT stacked — each component
# gets its own row with a y-axis starting at 0, so you can look sideways
# along a row to compare how that component changes across races.

plot_data_exploded <- plot_data |>
  mutate(
    component = factor(
      component,
      levels = c("Children", "Spouses", "Other adults", "Seniors")
    )
  )

plot_data_nb <- plot_data_exploded |> filter(nativity == "US-born")
plot_data_fb <- plot_data_exploded |> filter(nativity == "Foreign-born")

fig_tbl2_exploded <- ggplot(mapping = aes(x = decade, y = mean, fill = component)) +
  geom_col(data = plot_data_nb, aes(alpha = nativity), width = 9,
           color = "grey30", linewidth = 0.2) +
  geom_col(data = plot_data_fb, aes(alpha = nativity), width = 5,
           color = "grey30", linewidth = 0.2) +
  facet_grid(
    rows = vars(component),
    cols = vars(race_eth),
    scales = "free_y"
  ) +
  scale_fill_manual(values = c(
    "Children"     = "#1B9E77",  # teal
    "Spouses"      = "#D95F02",  # rust
    "Other adults" = "#7570B3",  # indigo
    "Seniors"      = "#E7298A"   # magenta
  ), guide = "none") +
  scale_alpha_manual(
    values = c("US-born" = 0.3, "Foreign-born" = 0.7),
    name   = NULL,
    labels = c("US-born (wide, pale)", "Foreign-born (narrow, dark)")
  ) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    x = NULL,
    y = "Mean number of coresidents"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text.y = element_text(angle = 0),
    legend.position = "bottom"
  )

ggsave(
  "output/figures/tbl-2-household-roster-exploded.jpeg",
  plot = fig_tbl2_exploded,
  width = 12,
  height = 8,
  dpi = 500
)
