#' # Causes of death and disability, incidence and deaths with age
#' 
#' How does incidence of and deaths by various causes vary with age? This report
#' uses [WHO Global Burden of Disease (GBD)](http://www.healthdata.org/gbd) data
#' to work it out.
#' 
#' It currently only examines the high-income countries.
#' 
#+ initialisation

source('init.R')

# Choose a unique group of age groups - several of the GBD options overlap etc
# so these will be the ones we'll use. We also need to specify which are in
# the population versus the life tables data, because of course they're not the
# same...
age_groups <-
  tibble(
    # How they are referred to by the GBD data
    age_group_name = c(
      '<1 year', '1 to 4',
      paste(seq(5, 90, 5), 'to', paste0(seq(9, 94, 5))),
      '95 plus'
    ),
    # Numerical versions of those age groups for simplicity later
    age_group_num = c(0, 3, seq(7.5, 92.5, 5), 99),
    # How wide is this category? The first two are fewer than five years.
    # '95 plus' is considered five years wide because I ultimately worked out
    # the average five-yearly probability of death.
    age_group_width = c(1, 4, rep(5, 19))
  )

#+ calculations

# Read in the causes we're going to look at
causes_selected <- read_csv(file.path(data_dir, 'who-gbd/causes-selected.csv'))

# Make our first data frame of life tables
incidence_table <-
  file.path(
    data_dir, 'who-gbd',
    c('IHME-GBD_2016_DATA-c1d5b4ee-1.csv', 'IHME-GBD_2016_DATA-c1d5b4ee-2.csv')
  ) %>% 
  map_dfr(read_csv) %>%
  # Only retain the rows relating to the relevant age ranges, both sexes and the
  # probability of death (there are also rows with remaining life expectancy)
  filter(
    age_name %in% age_groups$age_group_name &
      cause_id %in% c(causes_selected$cause_id, 322) &
      sex_name == 'Both' &
      metric_name == 'Rate'
  ) %>%
  # Rename the age_name column because this is different to their other data
  # because of course it is
  rename(age_group_name = age_name) %>%
  # Join with the age_groups data frame because we'll be making use of this soon
  inner_join(age_groups, by=c('age_group_name')) %>%
  # And put them in order of location and ascending age for ease
  arrange(location_id, age_group_num, cause_name) %>%
  # Because the rate is per 100,000, so divide by 1000 to give a percentage
  mutate(rate_percent = val / 1e3)

#+ causes_with_age_graphs

ggplot(
  incidence_table %>%
    filter(
      location_name == 'World Bank High Income' &
        cause_id %in% c(493, 410, 494, 509, 956, 543, 587, 322) &
        measure_name == 'Incidence'
    ),
  aes(
    x = age_group_num, y = rate_percent,
    group = cause_name, colour = cause_name
  )
) +
  # Uncomment this line to check and optimise the splines
  #geom_line() +
  geom_smooth(
    method = lm, formula = y ~ splines::bs(x, 10), se= FALSE
  ) +
  coord_cartesian(ylim=c(0,10)) +
  ggtitle('Incidence') +
  statto_style() +
  theme(legend.text = element_text(size = 12))

ggplot(
  incidence_table %>%
    filter(
      location_name == 'World Bank High Income' &
        cause_id %in% c(493, 410, 494, 509, 956, 543, 587, 322) &
        measure_name == 'Deaths'
    ),
  aes(
    x = age_group_num, y = rate_percent,
    group = cause_name, colour = cause_name
  )
) +
  # Uncomment this line to check and optimise the splines
  #geom_line() +
  geom_smooth(
    method = lm, formula = y ~ splines::bs(x, 10), se= FALSE
  ) +
  coord_cartesian(ylim=c(0,10)) +
  ggtitle('Deaths') +
  statto_style() +
  theme(legend.text = element_text(size = 12))

#+ summary_text

# Oldest age category in which Alzheimer's is categorised as zero incidence
alz_max_age_zero_incidence <-
  incidence_table %>%
  filter(
    cause_id == 543 &
      location_name == 'World Bank High Income' &
      # Only use data for 55 to 89 year-olds
      val == 0 &
      measure_name == 'Incidence'
  ) %>%
  filter(age_group_num == max(age_group_num)) %>%
  select(age_group_name)

# Incidence in the just under 50 category
alz_under_60 <-
  incidence_table %>%
  filter(
    cause_id == 543 &
      location_name == 'World Bank High Income' &
      # Only use data for 55 to 89 year-olds
      age_group_name == '55 to 59' &
      measure_name == 'Incidence'
  ) %>%
  select(val)

# Alzheimer's incidence doubling time
log_fit_alz <-
  lm(
    log2(val) ~ age_group_num, # log2 because we want doubling time
    data =
      incidence_table %>%
      filter(
        cause_id == 543 &
          location_name == 'World Bank High Income' &
          # Only use data for 55 to 89 year-olds
          age_group_num > 55 & age_group_num < 90 &
          measure_name == 'Incidence'
      )
  )
alz_dt <- 1/coef(log_fit_alz)[2]

# Work how out likely an 80-year-old is to get various diseases compared to a
# 30-year-old
incidence_80_vs_30 <- tibble()
death_80_vs_30 <- tibble()
for(cause in unique(incidence_table$cause_name)) {
  incidence_30 <-
    incidence_table %>%
    filter(
      cause_name == cause &
        location_name == 'World Bank High Income' &
        age_group_name == '30 to 34' &
        measure_name == 'Incidence'
    ) %>%
    select(val)
  death_30 <-
    incidence_table %>%
    filter(
      cause_name == cause &
        location_name == 'World Bank High Income' &
        age_group_name == '30 to 34' &
        measure_name == 'Deaths'
    ) %>%
    select(val)
  incidence_80 <-
    incidence_table %>%
    filter(
      cause_name == cause &
        location_name == 'World Bank High Income' &
        age_group_name == '80 to 84' &
        measure_name == 'Incidence'
    ) %>%
    select(val)
  death_80 <-
    incidence_table %>%
    filter(
      cause_name == cause &
        location_name == 'World Bank High Income' &
        age_group_name == '80 to 84' &
        measure_name == 'Deaths'
    ) %>%
    select(val)
  
  incidence_80_vs_30 <-
    rbind(
      incidence_80_vs_30,
      tibble(
        cause,
        ratio = incidence_80$val / incidence_30$val
      )
    )
  death_80_vs_30 <-
    rbind(
      death_80_vs_30,
      tibble(
        cause,
        ratio = death_80$val / death_30$val
      )
    )
}

#' This graph shows how the incidence of and deaths caused by different
#' age-related diseases varies with age. Most of them follow an
#' exponential-ish distribution, remaining relatively flat for most of life and
#' increasing very rapidly in old age.
#' 
#' Alzheimer's has a particularly strong association with age. The WHO rates it
#' as having zero incidence up to and including the
#' `r alz_max_age_zero_incidence$age_group_name`
#' age group; its incidence in the 55 to 59 category is
#' `r pretty_number(alz_under_60$val, 2)` per 100,000; and between the ages of
#' 55 and 90 its incidence doubles every `r pretty_number(alz_dt, 3)` years.
#' 
#' This table shows how much more likely an 80-year-old is than a 30-year-old to
#' experience a certain cause of death or disability:

incidence_80_vs_30 %>%
  # Create pretty numbers to print
  mutate(
    `cause` = cause,
    `incidence ratio` = pretty_number(ratio, 3)
  ) %>%
  # Remove columns not to display
  select(-c('ratio')) %>%
  # Use kable to knit a nice table
  kable() %>%
  kable_styling()

#' Alzheimer's is infinite because the incidence at age 30 is zero. Diabetes
#' has around the same incidence because both 30 and 80 are in the tails of the
#' incidence distribution—most people get diabetes in middle age if they're
#' going to.

death_80_vs_30 %>%
  # Create pretty numbers to print
  mutate(
    `cause` = cause,
    `death ratio` = pretty_number(ratio, 3)
  ) %>%
  # Remove columns not to display
  select(-c('ratio')) %>%
  # Use kable to knit a nice table
  kable() %>%
  kable_styling()

#' Alzheimer's is infinite because the death rate at age 30 is zero, and
#' neonatal problems is not a number because they obviously affect neither
#' 30- nor 80-year-olds.