# Produces a line graph of household size by age for native-born Americans,
# colored by decade of birth
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

# ----- Step 1: Query and aggregate ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

hhsize_age_birthcohort <- crosstab_mean(
  data = ipums_person |>
    filter(GQ %in% c(0, 1, 2), us_born == TRUE) |>
    mutate(
      birth_year = YEAR - AGE,
      birth_decade = case_when(
        birth_year < 1920 ~ "Before 1920",
        birth_year >= 1920 & birth_year < 1930 ~ "1920s",
        birth_year >= 1930 & birth_year < 1940 ~ "1930s",
        birth_year >= 1940 & birth_year < 1950 ~ "1940s",
        birth_year >= 1950 & birth_year < 1960 ~ "1950s",
        birth_year >= 1960 & birth_year < 1970 ~ "1960s",
        birth_year >= 1970 & birth_year < 1980 ~ "1970s",
        birth_year >= 1980 & birth_year < 1990 ~ "1980s",
        birth_year >= 1990 & birth_year < 2000 ~ "1990s",
        birth_year >= 2000 & birth_year < 2010 ~ "2000s",
        birth_year >= 2010 ~ "2010s"
      )
    ),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("AGE", "birth_decade")
) |>
  arrange(birth_decade, AGE) |>
  filter(!(count < 30))

dbDisconnect(con)

# ----- Step 2: Graph ----- #

birth_decade_levels <- c(
  "Before 1920", "1920s", "1930s", "1940s", "1950s",
  "1960s", "1970s", "1980s", "1990s", "2000s", "2010s"
)

fig18 <- hhsize_age_birthcohort |>
  mutate(birth_decade = factor(birth_decade, levels = birth_decade_levels)) |>
  ggplot(aes(x = AGE, y = weighted_mean, color = birth_decade)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = rainbow(length(birth_decade_levels), start = 0, end = 0.85)
  ) +
  labs(
    x = "Age",
    y = "Persons per Household",
    color = "Decade of Birth",
    title = "Household Size by Age: Native-Born Americans by Birth Cohort"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

fig18

# ----- Step 3: Save figure & data ----- #

write_csv(
  hhsize_age_birthcohort,
  "output/figures/fig18-household-size-age-birth-cohort-native-line.csv"
)

ggsave(
  filename = "output/figures/fig18-household-size-age-birth-cohort-native-line.jpeg",
  plot = fig18,
  width = 6,
  height = 6,
  dpi = 500
)
