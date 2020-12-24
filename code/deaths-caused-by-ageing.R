#' # Fraction of deaths attributable to ageing
#' 
#' How many deaths are attributable to ageing globally? This report uses
#' [WHO Global Burden of Disease (GBD)](http://www.healthdata.org/gbd) data to
#' work it out.
#' 
#' We will assume that, if ageing were entirely cured, mortality would peak in
#' your late 30s and that rate of death would continue indefinitely. The excess
#' deaths over this level are thus attributable to ageing.
#' 
#' This is arguably a conservative estimate—we could suggest that mortality
#' would peak at the equivalent of a younger age if ageing were completely
#' eradicated—but relatively few 20- and 30-somethings die, so it doesn’t
#' massively alter the statistics, and I hope it makes the numbers less open
#' to criticism of arbitrarily choosing an unrealistically young age to make
#' the point.
#' 
#' This script also uses
#' [UN data for GDP per capita](https://data.un.org/Data.aspx?d=SNAAMA&f=grID%3A101%3BcurrID%3AUSD%3BpcFlag%3A1)
#' for its graphics.
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
      paste(seq(5, 105, 5), 'to', paste0(seq(9, 109, 5))),
      '110 plus', '95 plus'
    ),
    # Numerical versions of those age groups for simplicity later
    age_group_num = c(0, 3, seq(7.5, 107.5, 5), 110, 99),
    # Logical value for whether this age range appears in population data
    age_group_in_pop = c(rep(TRUE, 20), rep(FALSE, 4), TRUE),
    # How wide is this category? The first two are fewer than five years.
    # '95 plus' is considered five years wide because I ultimately worked out
    # the average five-yearly probability of death.
    age_group_width = c(1, 4, rep(5, 23))
  )

# At what age do we want to freeze death rates to simulate the end of ageing?
old_age_name <- '35 to 39'
old_age_num  <- 37.5

# Which year is this for? (Because of course the population statistics are for
# multiple years and the life tables aren't...)
year <- 2017

#+ calculations

# Make our first data frame of life tables
life_tables <-
  # Start by reading the latest abridged life tables
  read_csv(
    file.path(
      data_dir,
      'who-gbd/IHME_GBD_2017_ABRIDGED_LIFE_TABLES_2017_Y2018M11D08.CSV'
    )
  ) %>%
  # Only retain the rows relating to the relevant age ranges, both sexes and the
  # probability of death (there are also rows with remaining life expectancy)
  filter(
    age_group_name %in% age_groups$age_group_name &
      sex_name == 'Both' &
      measure_name == 'Probability of death'
  ) %>%
  # Join with the age_groups data frame because we'll be making use of this soon
  inner_join(age_groups, by=c('age_group_name')) %>%
  # And put them in order of location and ascending age for ease
  arrange(location_id, age_group_num)

# Now, because the population data stops at '95 plus' and the life tables
# delineate ages all the way to '110 plus', we need to calculate an average
# probability of death for the '95 plus' age range by combining values in each
# region...

# Loop over the unique regions in the data
for(region_id in unique(life_tables$location_id)) {
  # Grab the rows of the data frame which are relevant to this location and late
  # life... (Over 95, but named generally as this could change...)
  late_life_survival <-
    life_tables %>%
    filter(
      location_id == region_id,
      age_group_name %in%
        age_groups$age_group_name[!age_groups$age_group_in_pop]
    )
  
  # The mean survival is a mean of the survival probabilities, weighted by the
  # chance of surviving long enough to be in that age group. The probability of
  # making it into the first group is taken to be 1, hence the need for adding a
  # leading 0 probability of death, and then trimming the final value as it's
  # irrelevant (everyone is dead after the final group).
  late_life_survival_mean <-
    weighted.mean(
      late_life_survival$val,
      cumprod(1 - head(c(0, late_life_survival$val), -1))
    )
  
  # Now, we're going to insert the '95 plus' average into our data frame.
  # Start by grabbing a row from the late_life_survival data frame to act as a
  # template...
  late_life_row <- tail(late_life_survival, 1)
  # ...change the age group and value...
  late_life_row$age_group_name <- '95 plus'
  late_life_row$age_group_num <- 99
  late_life_row$val <- late_life_survival_mean
  # ...and attach it to the life_tables data frame.
  life_tables <- rbind(life_tables, late_life_row)
  
  # Finally, remove the irrelevant rows.
  life_tables <-
    life_tables %>%
    filter(
      !(location_id == region_id &
      age_group_name %in%
        age_groups$age_group_name[!age_groups$age_group_in_pop])
    )
}

# Read the population data
population <-
  read_csv(
    file.path(
      data_dir,
      'who-gbd/IHME_GBD_2017_POP_2015_2017_Y2018M11D08.CSV'
    )
  ) %>%
  # Only retain the rows relating to the relevant age ranges, both sexes, and
  # the year used in the life tables data
  filter(
    age_group_name %in% age_groups$age_group_name &
    sex_name == 'Both' &
    year_id == year
  )

# Make a new data frame combining the population and life table data for
# continued calculations...
death_tables <-
  # Join the relevant columns of life_tables and population
  inner_join(
    life_tables %>%
      select(
        'location_name', 'location_id', 'age_group_name', 'age_group_num',
        'age_group_width', 'val'
      ),
    population %>%
      select('location_id', 'age_group_name', 'val'),
    by = c('location_id', 'age_group_name')
  ) %>%
  # Put them in order of location and ascending age for ease
  arrange(location_id, age_group_num) %>%
  # Remove duplicate rows (the '<1 year' entries are there twice and I can't be
  # bothered to troubleshoot)
  distinct %>% 
  # And rename the two 'val' columns to something more meaningful now the table
  # is wide rather than long
  rename(
    prob_death = val.x,
    population = val.y
  )

# The life tables contain the probability of death in a given period, and we
# want the annual chance of death, so convert it.
death_tables <-
  death_tables %>%
    mutate(prob_annual = 1-(1-prob_death)^(1/age_group_width))

# Calculate the number of deaths in each region in each age group
death_tables <-
  death_tables %>%
  mutate(deaths = prob_annual*population)

# Load files which will allow us to narrow down the location_ids to process.
locations_hierarchy <-
  read_csv(
    file.path(
      data_dir,
      'who-gbd/IHME_GBD_2017_ALL_LOCATIONS_HIERARCHIES_Y2018M11D08.CSV'
    )
  )
# Level 3 is the countries and territories
selected_countries <-
  locations_hierarchy$location_id[locations_hierarchy$level == 3]
# There are also a handful of high-level categories (eg the world and
# World Bank income levels) specified by hand.
locations_by_hand <-
  read_csv(
    file.path(
      data_dir,
      'who-gbd/high-level-categories.csv'
    )
  )
selected_groups <- locations_by_hand$location_id

locations_selected <- c(selected_countries, selected_groups)

# Filter death_tables to contain only the locations of interest
death_tables <-
  death_tables %>%
    filter(location_id %in% locations_selected)

# Loop over the regions determining the number of deaths due to ageing in each
ageing_deaths <- tibble()
for(region_id in unique(death_tables$location_id)) {
  # Total number of deaths is just the sum of the deaths in each age group
  deaths_total <-
    sum(death_tables$deaths[death_tables$location_id == region_id])
  # Ditto population
  population_total <-
    sum(death_tables$population[death_tables$location_id == region_id])
  
  # The probability of death in each age group if we ended ageing...
  prob_annual_end_ageing <-
    c(
      # ...is the same when we're at or below the cutoff...
      death_tables$prob_annual[
        death_tables$location_id == region_id &
          death_tables$age_group_num <= old_age_num
        ],
      # ...and constant at that level thereafter. Repeat the probability enough
      # times for the values to be filled in.
      rep(
        death_tables$prob_annual[
          death_tables$location_id == region_id &
            death_tables$age_group_num == old_age_num
        ],
        sum(
          death_tables$age_group_num[death_tables$location_id == region_id] >
            old_age_num
        )
      )
    )
  
  # We can now calculate the number of deaths were ageing to be cured
  deaths_end_ageing <-
    sum(
      prob_annual_end_ageing*
        death_tables$population[death_tables$location_id == region_id]
    )
  
  # And thus calculate the percentage of deaths we can ascribe to ageing
  deaths_by_ageing <- 100*(1-deaths_end_ageing/deaths_total)
  
  # Get the region name
  region <- death_tables$location_name[death_tables$location_id == region_id][1]
  
  # Append the calculated percentage to the data frame
  ageing_deaths <-
    rbind(
      ageing_deaths,
      tibble(
        location_id = region_id,
        location_name = region,
        population = population_total,
        deaths_by_ageing
      )
    )
}

# Sort the data frame
ageing_deaths <-
  ageing_deaths %>%
    arrange(desc(deaths_by_ageing))

# Get standardised location names with the countrycode package to merge later
ageing_deaths$location_standard <-
  countrycode(ageing_deaths$location_name, 'country.name', 'country.name')

countries_filter <- ageing_deaths$location_id %in% selected_countries
groups_filter <- ageing_deaths$location_id %in% selected_groups

# Load UN GDP per capita data
gdp_pc <-
  read_csv(
    file.path(
      data_dir,
      'un/UNdata_Export_20190824_172419049.csv'
    )
  ) %>%
  filter(Year == year) %>%
  select(-`Value Footnotes`) %>%
  rename(
    location_name = `Country or Area`,
    year = Year,
    gdp_pc = Value
  )

# Get standardised location names with the countrycode package to merge
gdp_pc$location_standard <-
  countrycode(gdp_pc$location_name, 'country.name', 'country.name')

# Join GDP data with ageing deaths and population data
ageing_deaths_gdp <-
  ageing_deaths %>%
    left_join(
      gdp_pc, by=c('location_standard'),
      suffix=c('_who', '_un'), na_matches = "never"
    )

#+ plot_data

# Make a new tibble to plot, starting by choosing just the countries
ageing_deaths_plot <- ageing_deaths_gdp[countries_filter,]
# Initialise all x-positions at zero
ageing_deaths_plot$x <- 0
# y positions are given by the percentage of deaths caused by ageing
ageing_deaths_plot$y <- ageing_deaths_plot$deaths_by_ageing
# The radius of each circle is given by the population of the country
max_value <- max(sqrt(ageing_deaths_gdp$population))
max_radius <- 5
ageing_deaths_plot$r <-
  sqrt(ageing_deaths_plot$population)/max_value * max_radius

# Run the circle spreading function to stop overlaps
ageing_deaths_plot <- circle_spread(ageing_deaths_plot, padding = 0.2)

# Create some convenient fake points to act as the legend
legend_points <-
  tibble(
    x = rep(seq(30,40, length.out=3), 2),
    y = c(rep(92, 3), rep(87, 3)),
    population = c(10e6, 100e6, 1e9, rep(100e6, 3)),
    gdp_pc = c(rep(1e4, 3), 1e3, 1e4, 1e5)
  )
legend_points$r <- sqrt(legend_points$population)/max_value * max_radius

# Large graph of deaths caused by ageing in different countries
gg_ageing_deaths <-
  ggplot(ageing_deaths_plot) +
  ggtitle(
    'Percentage of deaths caused by ageing in countries around the world',
    subtitle = 'Size of circle represents country population, colour represents GDP per capita') +
  # Labelled hline for global average, underneath everything
  geom_labelled_hline(
      yintercept = 
        ageing_deaths$deaths_by_ageing[ageing_deaths$location_name == 'Global'],
      text =
        paste0(
          'Global average: ',
          round(
            ageing_deaths$deaths_by_ageing[
              ageing_deaths$location_name == 'Global'],
            1
          ), "%"
        ),
      x_text = 30
    ) +
  # Circles for data
  geom_circle(
    aes(x0 = x, y0 = y, r = r, fill = gdp_pc),
    colour = 'transparent'
  ) +
  # Circles for legend
  geom_circle(
    data = legend_points,
    aes(x0 = x, y0 = y, r = r, fill = gdp_pc),
    colour = 'transparent'
  ) +
  # Labels for right-hand side of graph (these are separated to allow a nudge
  # to be applied...nudge isn't an aesthetic so must be defined for all labels)
  geom_text_repel(
    data =
      subset(
        ageing_deaths_plot,
        ageing_deaths_plot$population > 50e6 &
          ageing_deaths_plot$direction > 0
      ),
    aes(x=x, y=y, label=location_standard, colour=gdp_pc),
    hjust = 0, direction = 'y', nudge_x=5
  ) +
  # Labels for left-hand side of graph
  geom_text_repel(
    data =
      subset(
        ageing_deaths_plot,
        ageing_deaths_plot$population > 40e6 &
          ageing_deaths_plot$direction < 0
      ),
    aes(x=x, y=y, label=location_standard, colour=gdp_pc),
    hjust = 1, direction = 'y', nudge_x=-5
  ) +
  # Define two identical scales for fill and outline colours
  scale_fill_gradientn(
    colours = c('#9b1717', '#3f931a', '#4753d8'),
    trans = "log10",
    limits = c(
      min(ageing_deaths_plot$gdp_pc, na.rm=TRUE),
      max(ageing_deaths_plot$gdp_pc, na.rm=TRUE)
    )
  ) +
  scale_colour_gradientn(
    colours = c('#9b1717', '#3f931a', '#4753d8'),
    trans = "log10",
    limits = c(
      min(ageing_deaths_plot$gdp_pc, na.rm=TRUE),
      max(ageing_deaths_plot$gdp_pc, na.rm=TRUE)
    )
  ) +
  # Set axis limits and force equal scales so circles are circular
  xlim(-20, 50) + ylim(0,100) + coord_equal(ratio=1) +
  # Apply my plot stylings
  statto_style() +
  # And remove the x-axis and legend
  theme(
    axis.line.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    legend.position = 'none'
  )

print(gg_ageing_deaths)

#+ summary_text

ageing_majority_countries <-
  sum(
    ageing_deaths$deaths_by_ageing[countries_filter] > 50
  )

total_countries <- length(selected_countries)

ageing_majority_fraction <- ageing_majority_countries / total_countries

ageing_majority_population <-
  sum(
    (ageing_deaths$deaths_by_ageing[countries_filter] > 50) *
      ageing_deaths$population[countries_filter]
  ) / sum(ageing_deaths$population[countries_filter])

# Get total number of deaths per day, and number caused by ageing
global_deaths <-
  death_tables %>%
  filter(location_name == 'Global') %>%
  mutate(deaths_total=population*prob_annual)
global_deaths_total <- sum(global_deaths$deaths_total)
# Deaths due to ageing is total multiplied by number due to ageing 
global_deaths_ageing <-
  global_deaths_total *
  ageing_deaths$deaths_by_ageing[ageing_deaths$location_name == 'Global'] / 100

#' This graph shows that most deaths in most countries are caused by ageing.
#' 
#' Every day, there are `r pretty_number(global_deaths_total/365, 4)` deaths
#' globally. Ageing is responsible for
#' `r pretty_number(global_deaths_ageing/365, 4)` of them-so
#' `r pretty_number(ageing_deaths$deaths_by_ageing[ageing_deaths$location_name == 'Global']/100, percentage = TRUE)`.
#' In the high-income countries, this percentage increases to
#' `r pretty_number(ageing_deaths$deaths_by_ageing[ageing_deaths$location_name == 'World Bank High Income']/100, percentage = TRUE)`
#' of deaths.
#' 
#' Ageing is the leading cause of death in `r ageing_majority_countries` of
#' `r total_countries` countries
#' (`r pretty_number(ageing_majority_fraction, percentage = TRUE)`).
#' `r pretty_number(ageing_majority_population, percentage = TRUE)` of people
#' live in countries where ageing is the leading cause of death.
#' 
#' This is a table of all countries and regions examined. Location names are
#' those used by the WHO.

ageing_deaths_gdp %>%
  # Turn this into a prettier data frame to print
  transmute(
    Location = location_name_who,
    `Deaths caused by ageing` =
      pretty_number(deaths_by_ageing/100, percentage = TRUE),
    Population = pretty_number(population, 5),
    `GDP per capita` = pretty_number(gdp_pc, 3)
  ) %>%
  # Use kable to knit a nice table
  kable() %>%
  kable_styling()
