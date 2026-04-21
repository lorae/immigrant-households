# Produces summary statistics used in the paper
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# ----- Step 1: Race/ethnicity breakdown by nativity, 2022 ----- #

race_by_nativity_2022 <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0, 1, 2) & YEAR == 2022),
  wt_col = "PERWT",
  group_by = c("race_eth", "us_born"),
  percent_group_by = c("us_born")
) |>
  arrange(us_born, desc(percent))

race_by_nativity_2022

# ----- Step 2: Bar chart ----- #

race_order <- c("Hispanic", "White", "Black", "AAPI", "AIAN", "Multiracial", "Other")

plot_data <- race_by_nativity_2022 |>
  mutate(
    race_eth = factor(race_eth, levels = race_order),
    us_born_label = factor(
      ifelse(us_born, "US-born", "Foreign-born"),
      levels = c("US-born", "Foreign-born")
    )
  )

race_by_nativity_2022_bar <- ggplot(
  plot_data,
  aes(x = race_eth, y = percent / 100, fill = us_born_label)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("US-born" = "skyblue", "Foreign-born" = "forestgreen")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = "Share of population",
    fill = NULL,
    title = "Race/Ethnicity Composition by Nativity, 2022"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

race_by_nativity_2022_bar

# ----- Step 3: Age distribution by nativity, 2022 ----- #

age_levels <- c("17 or younger", "18-29", "30-49", "50 and older")

age_by_nativity_2022 <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0, 1, 2) & YEAR == 2022),
  wt_col = "PERWT",
  group_by = c("age_bucket", "us_born"),
  percent_group_by = c("us_born")
) |>
  arrange(us_born, age_bucket)

age_by_nativity_2022

age_plot_data <- age_by_nativity_2022 |>
  mutate(
    age_bucket = factor(age_bucket, levels = age_levels),
    us_born_label = factor(
      ifelse(us_born, "US-born", "Foreign-born"),
      levels = c("US-born", "Foreign-born")
    )
  )

age_by_nativity_2022_bar <- ggplot(
  age_plot_data,
  aes(x = age_bucket, y = percent / 100, fill = us_born_label)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("US-born" = "skyblue", "Foreign-born" = "forestgreen")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = "Share of population",
    fill = NULL,
    title = "Age Distribution by Nativity, 2022"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

age_by_nativity_2022_bar
