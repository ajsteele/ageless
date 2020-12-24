#' # Difference between female and male life expectancy
#' 
#' How does life expectancy differ between the sexes? This report uses
#' [WHO Global Burden of Disease (GBD)](http://www.healthdata.org/gbd) data to
#' work it out.
#' 
#' You can download the exact dataset used for this script
#' [here](http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2017-permalink/665d025a2594db660eda5ba8360dbcb5).
#' 
#+ initialisation

source('init.R')

#+ calculations

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

# Import GBD LE by gender data
le_data <-
  read_csv(
    file.path(
      data_dir,
      'who-gbd/IHME-GBD_2017_DATA-4c228945-1.csv'
    )
  ) %>%
  filter(
    location_id %in% locations_selected
     & age_name == '<1 year'
  ) %>%
  # Select only the most recent data
  group_by(location_id) %>% top_n(1, year) %>%
  # Remove some columns otherwise when we spread there will be 2 rows/country
  select(-c(sex_id, upper, lower)) %>%
  # Put female and male on one row
  spread(sex_name, val) %>%
  # Rename columns to lower case
  rename(female = Female, male = Male, both = Both) %>%
  # Make a column containing the difference
  mutate(fm_diff = female - male)

#' ## Life expectancy of women vs men in differerent countries
#' 
#+ le_sex_difference_graphs, fig.width = 800/300, fig.height=800/300

gg_fm_diff <-
  ggplot(le_data, aes(x=male, y = female)) + 
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    coord_equal(ratio=1) +
    # Apply my plot stylings
    statto_style()

print(gg_fm_diff)

#' This graph shows that female life expectancy exceeds male in almost every
#' country in the world. The diagonal line is where the points would lie were
#' female and male LE equal in that country. Only in
#' `r paste(le_data$location_name[le_data$fm_diff < 0], collapse = ', ')` is
#' male life expectancy greater than female.
#' 
#' 
#' This table shows the differences in life expectancies in selected regions:
 
le_data %>%
  # Interesting/representative places
  subset(location_name %in%
           c(
             'Global',
             'World Bank High Income',
             'World Bank Low Income',
             'United Kingdom',
             'United States',
             'France',
             'Germany',
             'Italy',
             'China',
             'India',
             'Brazil',
             'Russia',
             'Nigeria',
             # All locations where it's close, or men live longer than women
             le_data$location_name[le_data$fm_diff < 1]
           )) %>%
  # Turn this into a prettier data frame to print
  transmute(
    Location = location_name,
    `Female LE - Male LE` = fm_diff
  ) %>%
  # Use kable to knit a nice table
  kable() %>%
  kable_styling()

#' ## Sex difference in life expectancy vs overall life expectancy
#+ fig.width = 800/300, fig.height=800/300

gg_diff_vs_le <-
  ggplot(le_data, aes(x = both, y = fm_diff)) +
    geom_point() +
    # Apply my plot stylings
    statto_style()

print(gg_diff_vs_le)

#' Interestingly, there isn't a correlation between the difference in life
#' expectancy between sexes and the overall life expectancy in a country.