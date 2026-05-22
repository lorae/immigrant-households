# Produces quick facts used in the paper
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("ipumsr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ------ Step 1: Facts ----- #

# "In 1970, the average person lived in a household that included x people 
# (including themselves). By 2020, this had fallen to x people, a decline of x%.
hhsize_decade <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("decade")
) 

hhsize_decade

hhsize1970 <- hhsize_decade |> filter(decade == 1970) |> pull(weighted_mean)
hhsize2020 <- hhsize_decade |> filter(decade == 2020) |> pull(weighted_mean)

((hhsize1970 - hhsize2020) / hhsize1970)*100

# We exclude individuals living in institutional settings, such as prisons and 
# nursing homes, removing between ____% and ____% of the population across years. 
in_gq_decade <- crosstab_percent(
  data = ipums_person,
  wt_col = "PERWT",
  group_by = c("decade", "GQ"),
  percent_group_by = c("decade")
) |> 
  arrange(decade, GQ) |>
  mutate(
    in_gq = if_else(GQ %in% c(0, 1, 2), FALSE, TRUE)
  ) |>
  group_by(decade, in_gq) |>
  summarize(
    percent = sum(percent, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(decade, in_gq) |>
  filter(in_gq)

min(in_gq_decade$percent)
max(in_gq_decade$percent)

# In Figure 2, we plot household size over the last half century among native and 
# foreign-born populations of Black, Hispanic, and white Americans (who, in 2020, 
# collectively accounted for _% of the native-born population and _% of the 
# foreign-born population).
race_nat <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2) & decade == 2020),
  wt_col = "PERWT",
  group_by = c("race_eth", "us_born"),
  percent_group_by = c("us_born")
)

race_nat

race_nat |> 
  filter(us_born) |> 
  filter(
    race_eth == "Hispanic" |
    race_eth == "Black" |
    race_eth == "White"
  ) |>
  pull(percent) |>
  sum()

race_nat |> 
  filter(!us_born) |> 
  filter(
    race_eth == "Hispanic" |
      race_eth == "Black" |
      race_eth == "White"
  ) |>
  pull(percent) |>
  sum()

# Between 1970 and 2020, native-born Black household size fell by __% and native-born 
# white household size declined by __%, whereas foreign-born Black household sizes 
# fell by __% and native-born white household sizes fell by __%.
hhsize_race_nat <- read_csv("output/figures/fig11-household-size-race-nat-year-line.csv")

black_usborn_1970 <- hhsize_race_nat |> 
  filter(decade == 1970 & us_born == TRUE & race_eth == "Black") |> 
  pull(weighted_mean)

black_usborn_2020 <- hhsize_race_nat |> 
  filter(decade == 2020 & us_born == TRUE & race_eth == "Black") |> 
  pull(weighted_mean)

black_usborn_1970
black_usborn_2020

(black_usborn_1970 - black_usborn_2020) / black_usborn_1970

white_usborn_1970 <- hhsize_race_nat |> 
  filter(decade == 1970 & us_born == TRUE & race_eth == "White") |> 
  pull(weighted_mean)

white_usborn_2020 <- hhsize_race_nat |> 
  filter(decade == 2020 & us_born == TRUE & race_eth == "White") |> 
  pull(weighted_mean)

white_usborn_1970
white_usborn_2020

(white_usborn_1970 - white_usborn_2020) / white_usborn_1970

# Immigrant populations in both groups show linear and modest declines in household 
# size: in this period, foreign-born Black household sizes fell by __% and native-born 
# white household sizes fell by __%.
black_immig_1970 <- hhsize_race_nat |> 
  filter(decade == 1970 & us_born == FALSE & race_eth == "Black") |> 
  pull(weighted_mean)

black_immig_2020 <- hhsize_race_nat |> 
  filter(decade == 2020 & us_born == FALSE & race_eth == "Black") |> 
  pull(weighted_mean)

black_immig_1970
black_immig_2020

(black_immig_1970 - black_immig_2020) / black_immig_1970

white_immig_1970 <- hhsize_race_nat |> 
  filter(decade == 1970 & us_born == FALSE & race_eth == "White") |> 
  pull(weighted_mean)

white_immig_2020 <- hhsize_race_nat |> 
  filter(decade == 2020 & us_born == FALSE & race_eth == "White") |> 
  pull(weighted_mean)

white_immig_1970
white_immig_2020

(white_immig_1970 - white_immig_2020) / white_immig_1970

# In Figure 20, we plot age-standardized household size for foreign-born
# residents from 14 countries of origin. Together, these 14 countries
# accounted for __% of all foreign-born residents in the 2018-22 ACS pool.
# Country labels are matched on BPLD via the IPUMS DDI, mirroring
# src/figures/fig20-age-standardized-hhsize-by-country-decade-faceted.R.
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

target_labels <- bpld_labels |> filter(country %in% countries_of_interest)

fb_total_2020 <- ipums_person |>
  filter(GQ %in% c(0, 1, 2), !us_born, decade == 2020) |>
  summarize(fb_total = sum(PERWT, na.rm = TRUE)) |>
  collect() |>
  pull(fb_total)

fb_by_country <- ipums_person |>
  filter(GQ %in% c(0, 1, 2), !us_born, decade == 2020) |>
  inner_join(target_labels, by = "BPLD", copy = TRUE) |>
  group_by(country) |>
  summarize(fb_count = sum(PERWT, na.rm = TRUE), .groups = "drop") |>
  collect() |>
  mutate(percent = 100 * fb_count / fb_total_2020) |>
  arrange(desc(percent))

fb_share_fig20 <- bind_rows(
  fb_by_country,
  tibble(
    country  = "TOTAL (14 countries)",
    fb_count = sum(fb_by_country$fb_count),
    percent  = sum(fb_by_country$percent)
  )
)

fb_share_fig20

# In Figure 3, we plot household size over the last half-century among
# native- and foreign-born populations of Black, Hispanic, AAPI, and White
# Americans. In 2020 (2018-22 ACS pool), these four groups collectively
# accounted for __% of the native-born population and __% of the
# foreign-born population.
race_nat_4grp <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0, 1, 2) & decade == 2020),
  wt_col = "PERWT",
  group_by = c("race_eth", "us_born"),
  percent_group_by = c("us_born")
)

groups_4 <- c("Black", "Hispanic", "AAPI", "White")

race_nat_4grp_breakdown <- race_nat_4grp |>
  filter(race_eth %in% groups_4) |>
  mutate(nativity = if_else(us_born, "Native-born", "Foreign-born")) |>
  dplyr::select(nativity, race_eth, percent) |>
  arrange(nativity, desc(percent))

race_nat_4grp_breakdown

race_nat_4grp_breakdown |>
  group_by(nativity) |>
  summarize(total_4_groups = sum(percent), .groups = "drop")