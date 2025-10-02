# Produces a line graph of the percentage of American households with at least one immigrant
# And, within those households, the percentage of occupants that are immigrants
# (do this by cohort to see if there is increased integration over time)
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("dbplyr")
devtools::load_all("../demographr")

# ----- Step 1: Connect to the database ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_household <- tbl(con, "ipums_household")

foreign_born_in_household <- crosstab_percent(
  data = ipums_household |> filter(GQ %in% c(0,1,2)),
  wt_col = "HHWT",
  group_by = c("decade", "any_foreign_born"),
  percent_group_by = c("decade")
) |> 
  arrange(decade) |>
  filter(any_foreign_born)
  
