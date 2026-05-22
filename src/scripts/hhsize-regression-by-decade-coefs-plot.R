# Plots per-decade regression coefficients (multiplicative form) on a 3-panel
# layout, mirroring the pooled-model figure produced by hhsize-regression-over-time.R.
# Reads the formatted multiplicative table written by hhsize-regression-by-decade.R;
# does NOT refit the underlying regressions.
#
# Inputs:
# - output/tables/hhsize-regression-by-decade-mult.csv
#     Wide formatted table: estimate row (with stars; values are exp(beta))
#     followed by SE row in parens, plus N / FE / clustering footer rows.
#
# Outputs:
# - output/figures/fig22-hhsize-regression-by-decade-coefs-combined-panels.jpeg
# - output/figures/fig22-presentation-1-white.jpeg
# - output/figures/fig22-presentation-2-black.jpeg
# - output/figures/fig22-presentation-3-hispanic.jpeg
# - output/figures/fig22-presentation-4-aapi.jpeg
# - output/figures/fig22-presentation-5-age.jpeg
# - output/figures/fig22-presentation-6-education.jpeg
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("tidyr")
library("readr")
library("stringr")
library("ggplot2")
library("ggrepel")
library("patchwork")

# ----- Step 1: Parse the formatted table back to a long tidy frame ----- #
# Estimate rows have a non-empty label and values like "0.977***" (exp(beta)).
# SE rows immediately follow with an empty label and values like "(0.003)".
# Footer rows (N, FE, clustering) get dropped.

raw <- read_csv(
  "output/tables/hhsize-regression-by-decade-mult.csv",
  show_col_types = FALSE
)

footer_labels <- c("N (persons)", "State fixed effects", "Clustered SE (household)")

mults_long <- raw |>
  filter(!label %in% footer_labels) |>
  mutate(
    is_est = label != "",
    term   = if_else(is_est, label, NA_character_)
  ) |>
  fill(term, .direction = "down") |>
  mutate(value_type = if_else(is_est, "mult", "std.error")) |>
  select(-label, -is_est) |>
  pivot_longer(cols = -c(term, value_type), names_to = "decade", values_to = "raw_value") |>
  mutate(
    parsed = case_when(
      value_type == "mult"      ~ as.numeric(str_remove_all(raw_value, "\\*+")),
      value_type == "std.error" ~ as.numeric(str_replace_all(raw_value, "[()]", ""))
    ),
    decade = as.integer(decade)
  ) |>
  select(term, decade, value_type, parsed) |>
  pivot_wider(names_from = value_type, values_from = parsed)

mult_lookup <- function(term_name, dec) {
  v <- mults_long$mult[mults_long$term == term_name & mults_long$decade == dec]
  if (length(v) == 0) 1 else v   # default: no effect (multiplicative identity)
}

# ----- Step 2: Race x nativity panel ----- #
# Multiplicative effect relative to (White, US-born) within each decade.
# With per-decade fits, each decade is its own model, so the cell effect is
# the product of the relevant exp(beta) terms from that decade's fit:
#   (FB, race) = mult_FB * mult_race * mult_{FB:race}

races       <- c("White", "Hispanic", "Black", "AAPI")
ref_race    <- "White"
all_decades <- sort(unique(mults_long$decade))

plot_data_race <- expand_grid(
  race     = races,
  nativity = c("US-born", "Foreign-born"),
  decade   = all_decades
) |>
  rowwise() |>
  mutate(
    fb_mult     = if (nativity == "Foreign-born")            mult_lookup("Foreign-born", decade)               else 1,
    race_mult   = if (race != ref_race)                       mult_lookup(paste0(race, " (vs White)"), decade) else 1,
    fb_race_int = if (nativity == "Foreign-born" && race != ref_race)
                    mult_lookup(paste0("FB x ", race), decade)                                                 else 1,
    mult_effect = fb_mult * race_mult * fb_race_int
  ) |>
  ungroup() |>
  mutate(
    race     = factor(race,     levels = races),
    nativity = factor(nativity, levels = c("US-born", "Foreign-born"))
  )

# ----- Step 3: Age-controls panel (per 10 years of age, to share y-axis) ----- #
# Per-year multiplicative effect raised to the 10th power gives the per-decade
# effect (i.e., what household size is multiplied by for a 10-year age increase).

age_terms <- c("Age (years)" = "Age", "Age at arrival (years)" = "Age at arrival")

plot_data_age <- mults_long |>
  filter(term %in% names(age_terms)) |>
  mutate(
    term_label  = factor(unname(age_terms[term]), levels = unname(age_terms)),
    mult_effect = mult ^ 10
  )

# ----- Step 4: Education panel ----- #

educ_levels <- c("college_4yr+", "some_college", "hs_or_less")

plot_data_educ <- bind_rows(
  tibble(educ = "college_4yr+", decade = all_decades, mult_effect = 1),
  mults_long |>
    filter(term == "Some college (vs college 4yr+)") |>
    transmute(educ = "some_college", decade, mult_effect = mult),
  mults_long |>
    filter(term == "HS or less (vs college 4yr+)") |>
    transmute(educ = "hs_or_less",   decade, mult_effect = mult)
) |>
  mutate(educ = factor(educ, levels = educ_levels))

# ----- Step 5: Build the 3-panel figure ----- #

race_colors <- c(
  "White"    = "grey40",
  "Hispanic" = "#E69F00",
  "Black"    = "#009E73",
  "AAPI"     = "#9370DB"
)

y_limits <- range(c(plot_data_race$mult_effect,
                    plot_data_age$mult_effect,
                    plot_data_educ$mult_effect))
y_limits <- y_limits + c(-0.03, 0.03) * diff(y_limits)

panel_theme <- theme_minimal() +
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 8),
    legend.box       = "vertical",
    legend.spacing.y = unit(-0.2, "cm"),
    plot.title       = element_text(size = 11)
  )

mult_y_scale <- scale_y_continuous(
  limits = y_limits,
  labels = scales::number_format(accuracy = 0.01)
)

p_race <- ggplot(plot_data_race, aes(x = decade, y = mult_effect, color = race, linetype = nativity)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  scale_color_manual(values = race_colors) +
  scale_linetype_manual(values = c("US-born" = "solid", "Foreign-born" = "22")) +
  mult_y_scale +
  labs(x = NULL, y = "Multiplicative effect on household size",
       color = NULL, linetype = NULL,
       title = "Race x nativity (vs White US-born)") +
  panel_theme

p_age <- ggplot(plot_data_age, aes(x = decade, y = mult_effect, color = term_label)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  mult_y_scale +
  labs(x = NULL, y = NULL, color = NULL,
       title = "Age controls (per 10 years)") +
  panel_theme

p_educ <- ggplot(plot_data_educ, aes(x = decade, y = mult_effect, color = educ)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  mult_y_scale +
  labs(x = NULL, y = NULL, color = NULL,
       title = "Education (vs college_4yr+)") +
  panel_theme

fig_combined <- p_race + p_age + p_educ +
  plot_layout(ncol = 3) +
  plot_annotation(
    title   = "Household size multiplicative effects across decades (per-decade fits)",
    caption = "Each decade is its own Poisson regression. 1.0 = no effect; 1.30 = 30% more household members.\nAge controls raised to the 10th power to express effect per decade of age (mult^10)."
  )

# ----- Step 6: Save ----- #

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

ggsave(
  "output/figures/fig22-hhsize-regression-by-decade-coefs-combined-panels.jpeg",
  plot = fig_combined,
  width = 15,
  height = 5.5,
  dpi = 500
)

# ----- Step 7: Per-panel slide exports for presentation ----- #
# Six slides, all at the same dimensions (no legends, identical theme), so
# they can be flipped through cleanly in a deck. The race-by-nativity panel
# is split across four progression slides: each successive slide adds one
# race in color while previously-shown races fade to grey.

slide_w   <- 8
slide_h   <- 5
slide_dpi <- 500

slide_theme <- theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title      = element_text(size = 14)
  )

# All slides share the same y range (`y_limits` defined in Step 5) so the
# race, age, and education effects are visually comparable across slides.

# Helper: slide showing the focused race in color and any previously-shown
# races greyed out. The focused race's two lines (US-born + foreign-born) get
# direct labels at their right-hand endpoints in lieu of a legend.
make_race_slide <- function(focus, shown) {
  data <- plot_data_race |>
    filter(race %in% shown) |>
    mutate(color_group = if_else(race == focus, as.character(race), "grey"))

  label_data <- data |>
    filter(race == focus, decade == max(decade)) |>
    mutate(label = paste0(race, " (", nativity, ")"))

  color_values <- c(race_colors, grey = "grey75")

  ggplot(data, aes(x = decade, y = mult_effect,
                   group = interaction(race, nativity),
                   linetype = nativity, color = color_group)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    geom_text_repel(
      data           = label_data,
      aes(label = label),
      hjust          = 0,
      nudge_x        = 1.2,
      nudge_y        = -0.02,
      direction      = "y",
      size           = 4.2,
      fontface       = "bold",
      segment.color  = "grey60",
      segment.size   = 0.3,
      box.padding    = 0.3,
      show.legend    = FALSE,
      seed           = 1
    ) +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = c("US-born" = "solid", "Foreign-born" = "22")) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.22))) +
    mult_y_scale +
    labs(x = NULL, y = "Multiplicative effect on household size") +
    slide_theme
}

ggsave(
  "output/figures/fig22-presentation-1-white.jpeg",
  plot   = make_race_slide("White",    c("White")),
  width  = slide_w, height = slide_h, dpi = slide_dpi
)
ggsave(
  "output/figures/fig22-presentation-2-black.jpeg",
  plot   = make_race_slide("Black",    c("White", "Black")),
  width  = slide_w, height = slide_h, dpi = slide_dpi
)
ggsave(
  "output/figures/fig22-presentation-3-hispanic.jpeg",
  plot   = make_race_slide("Hispanic", c("White", "Black", "Hispanic")),
  width  = slide_w, height = slide_h, dpi = slide_dpi
)
ggsave(
  "output/figures/fig22-presentation-4-aapi.jpeg",
  plot   = make_race_slide("AAPI",     c("White", "Black", "Hispanic", "AAPI")),
  width  = slide_w, height = slide_h, dpi = slide_dpi
)

# Age panel — single slide on the shared y range, with direct labels.
# "Age at arrival" sits above its line; "Age" sits below (per-row nudge_y).
age_label_data <- plot_data_age |>
  filter(decade == max(decade)) |>
  mutate(
    label       = paste0(term_label, " (per 10 yrs)"),
    nudge_y_val = if_else(term_label == "Age at arrival", 0.02, -0.02)
  )

p_age_slide <- ggplot(plot_data_age, aes(x = decade, y = mult_effect, color = term_label)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text_repel(
    data           = age_label_data,
    aes(label = label),
    hjust          = 0,
    nudge_x        = 1.2,
    nudge_y        = age_label_data$nudge_y_val,
    direction      = "y",
    size           = 4.2,
    fontface       = "bold",
    segment.color  = "grey60",
    segment.size   = 0.3,
    box.padding    = 0.3,
    show.legend    = FALSE,
    seed           = 1
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  mult_y_scale +
  labs(x = NULL, y = "Multiplicative effect on household size") +
  slide_theme

ggsave(
  "output/figures/fig22-presentation-5-age.jpeg",
  plot   = p_age_slide,
  width  = slide_w, height = slide_h, dpi = slide_dpi
)

# Education panel — single slide on the shared y range, with direct labels.
educ_label_data <- plot_data_educ |>
  filter(decade == max(decade)) |>
  mutate(label = case_when(
    educ == "college_4yr+" ~ "College 4yr+",
    educ == "some_college" ~ "Some college",
    educ == "hs_or_less"   ~ "HS or less"
  ))

p_educ_slide <- ggplot(plot_data_educ, aes(x = decade, y = mult_effect, color = educ)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text_repel(
    data           = educ_label_data,
    aes(label = label),
    hjust          = 0,
    nudge_x        = 1.2,
    direction      = "y",
    size           = 4.2,
    fontface       = "bold",
    segment.color  = "grey60",
    segment.size   = 0.3,
    box.padding    = 0.3,
    show.legend    = FALSE,
    seed           = 1
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  mult_y_scale +
  labs(x = NULL, y = "Multiplicative effect on household size") +
  slide_theme

ggsave(
  "output/figures/fig22-presentation-6-education.jpeg",
  plot   = p_educ_slide,
  width  = slide_w, height = slide_h, dpi = slide_dpi
)
