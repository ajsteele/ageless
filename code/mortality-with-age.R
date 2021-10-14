#' # How mortality varies with age
#'
#+ initialisation

source('init.R')

#' ## Read in data
#' 
#' We’re using data from the
#' [Human Mortality Database](https://www.mortality.org/),
#' a standardised and curated collection of mortality data from countries
#' around the world. For this analysis we’ll concentrate on the World Bank’s
#' ‘high-income countries’ in that database, averaged over the five most recent
#' years of data.
#' 
#+ read_data

# Load a list of the countries available in the HMD
countries_hmd <- read_csv('../data/mortality.org/codelist.csv')

# Load a list of countries from the World Bank
countries_wb <- wb_countries('en')

# Select the high-income countries in the HMD
countries <-
  countries_hmd %>%
  filter(country_iso3c %in% countries_wb$iso3c[countries_wb$income_level_iso3c == 'HIC'])

# Load the life tables and populations of the countries of interest
countries_lt <- tibble()
countries_population <- tibble()
for(country in countries$country_hmd) {
  countries_lt <-
    rbind(
      countries_lt,
      read_life_table_hmd(country)  %>%
        # This is a kludge - we want the five most recent years, but top_n
        # selects from all values, not just unique ones, so only selects the
        # most recent year because there are 111 values (0-110+) for each year.
        # Should really make a top_n_unique function buy this will do...
        top_n(5*111, year) %>%
        mutate(country = country)
    )
  
  countries_population <- 
    rbind(
      countries_population,
      read_population_hmd(country) %>%
        top_n(5*111, year) %>%
        mutate(country = country)
    )
}

# Get total population for each year
countries_population_total <-
  countries_population %>%
  group_by(country, year) %>%
  summarize(population = sum(population))

# Average life tables over years
countries_lt_avg <-
  inner_join(countries_lt, countries_population_total) %>%
  group_by(country, age) %>%
  summarise(
    risk = weighted.mean(risk, population),
    population = mean(population)
  )

# Overall population-weighted average life table for a generic HIC
avg_lt <-
  inner_join(countries_lt, countries_population, by=c('country', 'age', 'year')) %>%
  group_by(age) %>%
  summarise(
    risk = weighted.mean(risk, population)
  )

#' ## Graphs
#' 
#+ mortality_with_age_graphs

lt_gg <-
  ggplot(
    countries_lt_avg %>% filter(age <= 90),
    aes(
      x = age,
      y = risk
    )
  ) +
  geom_line(
    alpha = 0.5,
    aes(colour = country,
        size = population)
  ) +
  geom_line(
    data = avg_lt %>% filter(age <= 90),
    aes(x = age, y = risk),
    size = 3
  ) +
  statto_style()

print(lt_gg)

print(lt_gg + scale_y_log10())

#' ## Results
#' 
#' Assuming the graph looks OK, you’ve done it! And we can now calculate a few
#' numbers describing the data…
#' 
#+ numerical_results

# Mortality rate doubling time
log_fit_mortality <-
  lm(
    log2(risk) ~ age, # log2 because we want doubling time
    # Only use data for 30 to 89 year-olds
    data = avg_lt %>% filter(age >= 30 & age <= 90)
  )
mrdt <- 1/coef(log_fit_mortality)[2]

# Cumulative odds of making certain ages
avg_lt <-
  avg_lt %>%
  mutate(odds_cum = c(1, head(cumprod(1-risk), -1)))

# Safest ages
min_risk <- avg_lt %>% top_n(-3, risk) %>% arrange(-desc(risk))

# Age when risk of death exceeds that of the first year of life
age_risk_0 <- min(avg_lt$age[avg_lt$risk > avg_lt$risk[avg_lt$age == 0]])

# Age when risk of death exceeds 1%
age_risk_1pc <- min(avg_lt$age[avg_lt$risk > 0.01])

# Age when risk of death exceeds 1/6
age_risk_dice <- min(avg_lt$age[avg_lt$risk > 1/6])

#' * The mortality rate doubling time, the amount of time it takes for risk of
#'   death to double, is `r pretty_number(mrdt)` years.
#' * The safest age to be is `r min_risk$age[1]` years old. The chance of you
#'   not making your `r min_risk$age[1]+1`th birthday is
#'   `r pretty_number(min_risk$risk[1], percentage = TRUE)`, or 1 in
#'   `r pretty_number(1/min_risk$risk[1])`
#' * The next safest ages are `r min_risk$age[2]` and `r min_risk$age[3]`,
#'   with odds of death in that year of
#'   `r pretty_number(min_risk$risk[2], percentage = TRUE)` and
#'   `r pretty_number(min_risk$risk[3], percentage = TRUE)`.
#' * An 80-year-old is
#'   `r pretty_number(avg_lt$risk[avg_lt$age == 80]/avg_lt$risk[avg_lt$age == 30])`
#'   times more likely to die than a 30-year-old.
#' * Your risk of death doesn’t exceed your risk in the first year of life until
#'   you’re `r pretty_number(age_risk_0)` years old.
#' * Your risk of death exceeds 1% when you’re `r pretty_number(age_risk_1pc)`
#'   years old.
#' * Your risk of death aged `r pretty_number(age_risk_dice)` is 1 in 
#'   `r pretty_number(1/avg_lt$risk[avg_lt$age == age_risk_dice])`, worse than a
#'   dice roll.
#' 
#' Finally, here are some odds of death at various ages:
#' 

avg_lt %>%
  filter(
    age %in%
      # A selection of interesting ages
      sort(c(seq(0, 100, 10), 1, 65, age_risk_0, age_risk_1pc, age_risk_dice))
  ) %>%
  # Create pretty numbers to print
  mutate(
    `risk of death` = pretty_number(risk, percentage = TRUE),
    `1/risk` = pretty_number(1/risk),
    `odds of reaching` = pretty_number(odds_cum, percentage = TRUE)
  ) %>%
  # Remove non-pretty columns
  select(-c('risk', 'odds_cum')) %>%
  # Use kable to knit a nice table
  kable() %>%
  kable_styling()

