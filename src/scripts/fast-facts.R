# Produces quick facts used in the paper
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")

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
