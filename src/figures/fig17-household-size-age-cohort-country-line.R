# Produces figures showing hhsize by immigrant cohort and country of origin
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ipumsr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

# ----- Step 1: Get BPLD value labels from DDI ----- #

ddi_path <- list.files("data/ipums-microdata", pattern = "\\.xml$", full.names = TRUE)[1]
ddi <- read_ipums_ddi(ddi_path)

bpld_labels <- ipums_val_labels(ddi, "BPLD") |>
  as_tibble() |>
  rename(BPLD = val, country = lbl)

# ----- Step 2: Query and aggregate ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

hhsize_age_cohort <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0, 1, 2), !is.na(immig_cohort)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("AGE", "immig_cohort", "BPLD")
) |>
  arrange(BPLD, immig_cohort, AGE) |>
  filter(!(count < 30))

dbDisconnect(con)

# ----- Step 3: Attach labels ----- #

hhsize_age_cohort <- hhsize_age_cohort |>
  left_join(bpld_labels, by = "BPLD") |>
  mutate(country = ifelse(is.na(country), paste0("Unknown (", BPLD, ")"), country))

cohort_levels <- c(
  "1919 or earlier", "1920s", "1930s", "1940s", "1950s",
  "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s"
)

# ----- Step 4: Plot and save each country ----- #

plot_country <- function(data, country_name, cohort_levels) {
  slug <- tolower(gsub(" ", "-", country_name))

  p <- data |>
    filter(country == country_name) |>
    mutate(immig_cohort = factor(immig_cohort, levels = cohort_levels)) |>
    ggplot(aes(x = AGE, y = weighted_mean, color = immig_cohort)) +
    geom_line(linewidth = 1) +
    scale_color_manual(
      values = rainbow(length(cohort_levels), start = 0, end = 0.85)
    ) +
    labs(
      x = "Age",
      y = "Persons per Household",
      color = "Decade of Immigration",
      title = paste0("Household Size by Age: ", country_name, " Immigrants by Cohort")
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())

  print(p)

  ggsave(
    filename = paste0("output/figures/fig17-household-size-age-cohort-", slug, "-line.jpeg"),
    plot = p,
    width = 6,
    height = 6,
    dpi = 500
  )
}

countries <- c(
  "Mexico", "India", "Venezuela", "Cuba", "Colombia",
  "China", "Guatemala", "Honduras", "Brazil", "Philippines", "Korea"
)

for (cntry in countries) {
  plot_country(hhsize_age_cohort, cntry, cohort_levels)
}
