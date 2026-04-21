# Produces a table of the top 10 countries of origin for each immigrant cohort
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ipumsr")
library("tidyr")
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

# Sum person weights by cohort and birthplace, for immigrants only
country_cohort <- ipums_person |>
  filter(GQ %in% c(0, 1, 2), !is.na(immig_cohort)) |>
  group_by(immig_cohort, BPLD) |>
  summarise(pop = sum(PERWT, na.rm = TRUE), .groups = "drop") |>
  collect()

dbDisconnect(con)

# ----- Step 3: Attach labels and rank ----- #

country_cohort <- country_cohort |>
  left_join(bpld_labels, by = "BPLD") |>
  mutate(country = ifelse(is.na(country), paste0("Unknown (", BPLD, ")"), country))

# Rank within each cohort and keep top 10
top10 <- country_cohort |>
  group_by(immig_cohort) |>
  arrange(desc(pop)) |>
  mutate(rank = row_number()) |>
  filter(rank <= 10) |>
  ungroup()

# ----- Step 4: Pivot to wide table ----- #

cohort_levels <- c(
  "1919 or earlier", "1920s", "1930s", "1940s", "1950s",
  "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s"
)

top10_wide <- top10 |>
  mutate(immig_cohort = factor(immig_cohort, levels = cohort_levels)) |>
  arrange(immig_cohort, rank) |>
  mutate(label = paste0(rank, ". ", country)) |>
  select(immig_cohort, rank, label) |>
  pivot_wider(names_from = immig_cohort, values_from = label)

# ----- Step 5: Save ----- #

write_csv(
  top10_wide,
  "output/figures/fig16-top-countries-by-cohort-table.csv"
)

# Also save the long-form data with population counts
write_csv(
  top10 |>
    mutate(immig_cohort = factor(immig_cohort, levels = cohort_levels)) |>
    arrange(immig_cohort, rank) |>
    select(immig_cohort, rank, country, pop),
  "output/figures/fig16-top-countries-by-cohort-table-long.csv"
)
