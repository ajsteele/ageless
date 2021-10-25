#' # Global population estimates if we cured ageing (or death)
#' 
#' Could curing ageing lead to a population crisis? This is an attempt at some
#' (very) simple calculations to get some idea, based on population projections
#' by the
#' [United Nations (UN)](https://population.un.org/wpp/Download/Standard/Population/)
#' and
#' [Institute for Health Metrics and Evaluation (IHME)](http://ghdx.healthdata.org/record/ihme-data/global-population-forecasts-2017-2100).
#' 
#' These calculations support a
#' [video exploring this](https://www.youtube.com/watch?v=f1Ve0fYuZO8)
#' by [Andrew Steele](https://www.youtube.com/DrAndrewSteele), author of
#' [_Ageless: The new science of getting older without getting old_](https://andrewsteele.co.uk/ageless/).
#' 
#+ initialisation

# Global includes etc
source('init.R')

# Handy functions for this specific application

# Wrapper function to apply to IHME population and death projections to get
# global values for both sexes from provided data.
# If all_ages is FALSE (the default), then the 'All Ages' rows are excluded to
# avoid double-counting for analyses concerned with age-specific populations or
# death rates. If it's TRUE, then this just returns the All Ages combined
# figures, for simpler analysis.
simplify_ihme_projections <- function(
    x,
    measure_name = 'population',
    all_ages = FALSE
    ) {
    # Choose rows depending on whether all_ages are wanted or to be discarded
    if(all_ages) {
        select_ages <- x$age_group_name == 'All Ages'
    } else {
        select_ages <- x$age_group_name != 'All Ages'
    }
    # Operate on a tibble of raw IHME data to simplify it...
    x %>%
        filter(
            location_name == 'Global', # Just the whole world for now
            select_ages # Either remove All Ages, or only All Ages
        ) %>%
        select(
            c(
                age_group = age_group_name,
                sex,
                year = year_id,
                population = val
            )
        ) %>%
        group_by(year, age_group) %>%
        # This should work in a single line with summarise, but for some reason
        # I can't get it to, so I will kludge it with three lines for now...
        mutate(total = sum(population)) %>%
        # Just keep one sex - both now contain the same value after mutating
        filter(
            sex == 'Female' # Smash the patriarchy!
        ) %>% 
        # Simplify columns
        select(
            age_group,
            year,
            !!measure_name := total
        )
}

# Frustratingly, the most usable format of age-specific death statistics from
# the UN is distributed in an Excel spreadsheet, (the CSV life tables don't give
# you enough information to make the calculations, because the numbers are given
# over five-year periods while population for the medium variant is given
# annually, so it's not possible to exactly equate the figures provided without
# knowing the intermediate values) so import an Excel reader and get the data...
library(readxl)

read_excel_unpop <- function(
    url, sheet_name, excel_range, time_name, value_name
) {
    download_data(url, 'un-population', data_dir)
    
    filename <- basename(url)
    
    read_excel(
        file.path(data_dir, 'un-population', filename),
        sheet = sheet_name,
        # Select specified cells, fragile though that is
        range = excel_range
    ) %>%
        # Data are in wide format, so make it long and rename
        pivot_longer(c(-1)) %>%
        rename(
            !!time_name := 1, # First column is either year or period
            age_group = name,
            !!value_name := value # value is whatever this is a spreadsheet of
        )
}

#' 
#' ## Eradicating death
#' 
#' The simplest and most extreme case we’ll consider is a complete eradication
#' of death in 2025. This is obviously absurd, for a number of reasons: we can’t
#' possibly develop treatments that fast; even if we could, we couldn’t roll
#' them out universally that fast; and, even if we somehow did cure ageing and
#' roll out those treatments in just a few years, there would still be other
#' ways to die, from traffic accidents to infectious diseases.
#' 
#' Nonetheless, this provides the most extreme imaginable scenario with which to
#' benchmark our level of concern.
#' 
#' Without embarking on a complicated demographic modelling exercise, I’ll just
#' take the two best-known population projections and assume no-one dies after
#' 2025, ignoring any effect this might have on birth rates.

# Since the files used here are large and have simple permanent URLs, the script
# automatically downloads them if needed to avoid putting them in the repo.
# We'll start with the UN's population estimates and projections
download_data(
    'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv',
    'un-population', data_dir
)

population_un <-
    read_csv(
        file.path(
            data_dir,
            'un-population/WPP2019_TotalPopulationBySex.csv'
        )
    ) %>%
    filter(
        Location == 'World',
        Variant %in% c('Low', 'Medium', 'High')
    ) %>%
    select(
        year = Time,
        variant = Variant,
        population = PopTotal
    ) %>%
    mutate (
        population = population*1e3 # UN population figures are given in 000s
    )

# Period indicators for medium variant...
download_data(
    'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Period_Indicators_Medium.csv',
    'un-population', data_dir
)
# ...and other variants
download_data(
    'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Period_Indicators_OtherVariants.csv',
    'un-population', data_dir
)

all_projections_un <-
    # Bind the medium and other projections together
    bind_rows(
        read_csv(
            file.path(
                data_dir,
                'un-population/WPP2019_Period_Indicators_Medium.csv'
            )
        ),
        read_csv(
            file.path(
                data_dir,
                'un-population/WPP2019_Period_Indicators_OtherVariants.csv'
            )
        )
    )  %>%
    # Then just grab the world deaths, since all we want is global population
    filter(
        Location == 'World',
        Variant %in% c('Low', 'Medium', 'High')
    ) %>%
    select(
        year = MidPeriod,
        variant = Variant,
        deaths = Deaths
    ) %>%
    mutate (
        year = year + 2,
        deaths = deaths*1e3 # UN population figures are given in 000s
    )

all_projections_un_deathless <-
    merge(
        all_projections_un, population_un
    ) %>%
    filter(year >= 2030) %>%
    group_by(variant) %>%
    mutate(
        population_deathless = population + cumsum(deaths)
    )

# Get the population in 2020, which will be common to all projections (so
# there's not a gap between 2020 and 2025 on the chart)
# TODO make 2020 and 2025 variables
population_now <-
    population_un %>%
    filter(year == 2020, variant == 'Medium') %>%
    select(population) %>% as.numeric()
population_2025 <-
    population_un %>%
    filter(year == 2025)

# TODO make 2020 and 2025 variables
all_projections_un_deathless <-
    bind_rows(
        tibble(
            year = 2020,
            variant = c('High', 'Low', 'Medium'),
            deaths = NA,
            population = population_now,
            population_deathless = population_now
        ),
        population_2025 %>%
            mutate(
                deaths = NA,
                population_deathless = population
            ),
        all_projections_un_deathless
    )

#' ### UN
#' 
#' The UN has three headline scenarios, the high, medium and low variants, which
#' differ depending on assumptions made about birth rates. I’ve plotted these
#' below.
#' 

# Hackily pull out a few values we're going to be using to display over the next
# few paragraphs...
pop_low_2050 <-
    all_projections_un_deathless %>%
    filter(year == 2050, variant == 'Low') %>%
    select(population) %>%
    as.numeric
pop_low_2100 <-
    all_projections_un_deathless %>%
    filter(year == 2100, variant == 'Low') %>%
    select(population) %>%
    as.numeric
pop_medium_2050 <-
    all_projections_un_deathless %>%
    filter(year == 2050, variant == 'Medium') %>%
    select(population) %>%
    as.numeric
pop_medium_2100 <-
    all_projections_un_deathless %>%
    filter(year == 2100, variant == 'Medium') %>%
    select(population) %>%
    as.numeric
pop_high_2050 <-
    all_projections_un_deathless %>%
    filter(year == 2050, variant == 'High') %>%
    select(population) %>%
    as.numeric
pop_high_2100 <-
    all_projections_un_deathless %>%
    filter(year == 2100, variant == 'High') %>%
    select(population) %>%
    as.numeric
pop_medium_2050_deathless <-
    all_projections_un_deathless %>%
    filter(year == 2050, variant == 'Medium') %>%
    select(population_deathless) %>%
    as.numeric
pop_medium_2100_deathless <-
    all_projections_un_deathless %>%
    filter(year == 2100, variant == 'Medium') %>%
    select(population_deathless) %>%
    as.numeric

#' By 2050, the population would be 
#' `r pretty_number(pop_low_2050/1e9, dp=1)`bn, 
#' `r pretty_number(pop_medium_2050/1e9, dp=1)`bn
#' or
#' `r pretty_number(pop_high_2050/1e9, dp=1)`bn
#' under the low, medium and high variants, respectively. By 2100, the
#' population would be
#' `r pretty_number(pop_low_2100/1e9, dp=1)`bn, 
#' `r pretty_number(pop_medium_2100/1e9, dp=1)`bn
#' or
#' `r pretty_number(pop_high_2100/1e9, dp=1)`bn.

ggplot(
    all_projections_un_deathless,
    aes(
        x = year,
        y = population,
        colour = variant
    )
) +
    geom_line() +
    geom_line(
        data = subset(population_un, year <= 2020),
        aes(y = population), colour = 'black'
    )


#' What if no-one died? Under the medium variant, the population by 2050 would
#' be `r pretty_number(pop_medium_2050_deathless/1e9, dp=1)`bn. That’s
#' `r pretty_number(pop_medium_2050_deathless/pop_medium_2050-1, sf=2, percentage=TRUE)`
#' larger than if we’d not eradicated death.
#' 
#' By 2100, it would be
#' `r pretty_number(pop_medium_2050_deathless/1e9, dp=1)`bn:
#' `r pretty_number(pop_medium_2100_deathless/pop_medium_2100-1, sf=2, percentage=TRUE)`
#' larger.
#' 
#' That compares to a difference of
#' `r pretty_number(pop_high_2050/pop_low_2050-1, sf=2, percentage=TRUE)`
#' between the normal, death-included low and high variants in 2050, or
#' `r pretty_number(pop_high_2100/pop_low_2100-1, sf=2, percentage=TRUE)`
#' between them in 2100.
#' 
#' So, similar or even larger differences exist with plausible future variation
#' in birth rates than if we were to _literally cure death_ which, to reirerate,
#' is a rather extreme assumption.

ggplot(
    all_projections_un_deathless,
    aes(
        x = year,
        y = population,
        colour = variant
    )
) +
    geom_line() +
    geom_line(
        data = subset(all_projections_un_deathless, variant == 'Medium'),
        aes(y = population_deathless)
    ) +
    geom_line(
        data = subset(population_un, year <= 2020),
        aes(y = population), colour = 'black'
    )

#' ### IHME
#' 
#' Let’s try a similar exercise with the IHME projections. They publish a
#' number of different scenarios, with the ‘reference’ scenario as a baseline.
#' We’ll look at this, and the two most extreme in population terms:
#' ‘slower’ (which assumes slower development of poorer countries, and thus
#' higher birth rates), and ‘SDG’ (which assumes rapid development in
#' accordance with the Sustainable Development Goals, plus increased education
#' for women, slowing birth rates more rapidly.)

# Download the IHME's reference scenario, and its two most extreme scenarios
### Population ###
download_data(
    'http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_POP_2017_2100_POP_REFERENCE.zip',
    'ihme-population', data_dir
)
download_data(
    'http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_POP_2017_2100_POP_SDG.zip',
    'ihme-population', data_dir
)
download_data(
    'http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_POP_2017_2100_POP_SLOWER.zip',
    'ihme-population', data_dir
)
### Deaths ###
download_data(
    'http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_POP_2017_2100_DEATH_REFERENCE.zip',
    'ihme-population', data_dir
)
download_data(
    'http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_POP_2017_2100_DEATH_SDG.zip',
    'ihme-population', data_dir
)
download_data(
    'http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_POP_2017_2100_DEATH_SLOWER.zip',
    'ihme-population', data_dir
)

# Open all the population projection CSVs and bind them into a massive tibble
# with a column for scenario
population_projection_ihme <-
    bind_rows(
        read_csv(
            file.path(
                data_dir,
                'ihme-population/IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.CSV'
            )
        ) %>%
            simplify_ihme_projections(all_ages = TRUE) %>%
            mutate(scenario = 'reference'),
        read_csv(
            file.path(
                data_dir,
                'ihme-population/IHME_POP_2017_2100_POP_SDG_Y2020M05D01.CSV'
            )
        ) %>%
            simplify_ihme_projections(all_ages = TRUE) %>%
            mutate(scenario = 'sdg'),
        read_csv(
            file.path(
                data_dir,
                'ihme-population/IHME_POP_2017_2100_POP_SLOWER_Y2020M05D01.CSV'
            )
        ) %>%
            simplify_ihme_projections(all_ages = TRUE) %>%
            mutate(scenario = 'slower')
    )

# Open all the death projection CSVs and bind them into a massive tibble
# with a column for scenario
death_projection_ihme <-
    bind_rows(
        read_csv(
            file.path(
                data_dir,
                'ihme-population/IHME_POP_2017_2100_DEATH_REFERENCE_Y2020M05D01.CSV'
            )
        ) %>%
            simplify_ihme_projections(
                measure_name = 'deaths', all_ages = TRUE
            ) %>%
            mutate(scenario = 'reference'),
        read_csv(
            file.path(
                data_dir,
                'ihme-population/IHME_POP_2017_2100_DEATH_SDG_Y2020M05D01.CSV'
            )
        ) %>%
            simplify_ihme_projections(
                measure_name = 'deaths', all_ages = TRUE
            ) %>%
            mutate(scenario = 'sdg'),
        read_csv(
            file.path(
                data_dir,
                'ihme-population/IHME_POP_2017_2100_DEATH_SLOWER_Y2020M05D01.CSV'
            )
        ) %>%
            simplify_ihme_projections(
                measure_name = 'deaths', all_ages = TRUE
            ) %>%
            mutate(scenario = 'slower')
    )

# Merge the population and death projections
# TODO make 2025 a variable
projection_ihme <-
    merge(
        population_projection_ihme, death_projection_ihme,
        by = c('year', 'scenario')
    ) %>%
    # Remove needless age group columns
    select(-c(age_group.x, age_group.y)) %>%
    # Cure death in 2025
    filter(year >= 2025) %>%
    # Cumulatively sum avoided deaths by scenario to get population if death
    # were cured
    group_by(scenario) %>%
    mutate(
        population_deathless = population + cumsum(deaths)
    )

#' This next graph is a bit confusing but I’m hoping if you’ve read this far
#' you’l be willing to try to make sense of it! It includes all three UN
#' variants (Low, Medium and High), and the deathless UN Medium variant (which
#' is the higher line in the same colour); and it also includes the three IHME
#' scenarios (reference, sdg and slower), plus a deathless SDG scenario to see
#' if literally curing death ‘saves us’ from the risk of underpopulation by the
#' end of the century in this case.
#' 

ggplot(
    projection_ihme,
    aes(
        x = year,
        y = population,
        colour = scenario
    )
) +
    geom_line() +
    geom_line(
        data = subset(projection_ihme, scenario == 'sdg'),
        aes(y = population_deathless)
    ) +
    geom_line(
        data = subset(population_un, year <= 2020),
        aes(y = population), colour = 'black'
    ) +
    geom_line(
        data = all_projections_un_deathless,
        aes(colour = variant)
    ) +
    geom_line(
        data = subset(all_projections_un_deathless, variant == 'Medium'),
        aes(y = population_deathless, colour = variant)
    )

#' As you can see, the IHME scenarios tend to end up with lower populations than
#' the UN ones, but they’re all in the same broad range, and literally curing
#' death has an effect that’s broadly in line with the size of effect that
#' adjustments in birth rate is expected to produce.
    

#' ## ‘Curing’ ageing
#' 
#' I wanted to try something just slightly more realistic  than literally curing
#' death, so I decided to try not of eradicating all death, but just the
#' age-related fraction of it.
#' 
#' To perform these calculations, I have assumed a very simple change to death
#' rates resulting from a ‘cure’ for ageing: that everyone aged 35 or over will
#' have the same death rate as people aged between 30 and 34. This is quite an
#' extreme assumption, and I’ve made it to make the most extreme possible
#' projections again.
#' 
#' Regardless of the exact age you choose, annual death rates are very low for
#' most people in most countries under the age of 60. The low death rate of
#' young people means that
#' [the number of deaths caused by ageing](http://htmlpreview.github.io/?https://github.com/ajsteele/ageless/blob/main/output/deaths-caused-by-ageing.html)
#' is very large. When making that calculation, I made an opposite ‘extreme’
#' assumption that death rates would freeze at the _end_ the our 30s, rather
#' than the start. This was to ensure that my results would be conservative
#' in the opposite direction—if anything, underestimating deaths due to
#' ageing—to make my case as robust as possible.
#' 
#' This does mean that the ageless scenario here and the number of deaths caused
#' by ageing in those stats don’t quite tally up—but all the code for these
#' calculations is [available on GitHub](https://github.com/ajsteele/ageless/),
#' so you can play around with these scenarios, check my working, or develop
#' better ones if you’re interested!
#' 
#+ ageless_population_projections

# Reload the IHME population projection with age-specific data this time
# (I coded this before the above, so these are separate tibbles. Ideally I’d
# merge them to make the code the same.)
population_projection_ihme <-
    read_csv(
        file.path(
            data_dir,
            'ihme-population/IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.CSV'
        )
    ) %>%
    simplify_ihme_projections

population_projection_ihme_slower <-
    read_csv(
        file.path(
            data_dir,
            'ihme-population/IHME_POP_2017_2100_POP_SLOWER_Y2020M05D01.CSV'
        )
    ) %>%
    simplify_ihme_projections

population_projection_ihme_sdg <-
    read_csv(
        file.path(
            data_dir,
            'ihme-population/IHME_POP_2017_2100_POP_SDG_Y2020M05D01.CSV'
        )
    ) %>%
    simplify_ihme_projections

# At what age do we want to freeze death rates to simulate the end of ageing?
# (Age group naming convention differs between the UN and IHME...)
old_age_name_un   <- '30-34'
old_age_name_ihme <- '30 to 34'

# Names of age groups post-ageing (easiest to define manually due to
# idiosyncratic age group definitions)
old_age_groups_un <-
    c(
        '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74',
        '75-79', '80-84', '85-89', '90-94', '95+'
    )
old_age_groups_ihme <-
    c(
        '35 to 39', '40 to 44', '45 to 49', '50 to 54', '55 to 59', '60 to 64',
        '65 to 69', '70 to 74', '75 to 79', '80 to 84', '85 to 89', '90 to 94',
        '95 plus'
    )

# Note: because the UN death rates are only released for five-year periods, this
# number needs to be a multiple of 5 at the moment. Interpolation could be
# implemented but, given the other simplifications in this model, such precision
# seems unnecessary!
ageing_cured_year <- 2025

#+ calculations_un

# Get more population data by age for agelessness estimates
download_data(
    'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv',
    'un-population', data_dir
)
population_projection_un <-
    read_csv(
        file.path(
            data_dir,
            'un-population/WPP2019_PopulationByAgeSex_Medium.csv'
        )
    ) %>%
    filter(
        Location == 'World',
        Variant == 'Medium',
        Time >= 2020
    ) %>%
    select(
        year = Time,
        age_group = AgeGrp,
        population = PopTotal
    )

death_projection_un <-
    read_excel_unpop(
        'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/3_Mortality/WPP2019_MORT_F04_1_DEATHS_BY_AGE_BOTH_SEXES.xlsx',
        'MEDIUM VARIANT',
        'H17:AB33',
        'period',
        'deaths'
    )

# Loop over periods and age groups, updating death numbers
period_start <- seq(2020,2095,5)
period <- paste0(period_start, '-', period_start+5)

# Start by grouping together older ages because of course the death stats stop
# at 95+ while the population stats stop at 100+
for(update_year in unique(population_projection_un$year)) {
    population_projection_un <-
        population_projection_un %>%
        # Add a new row for 95+
        add_row(
            tibble(
                year = update_year,
                age_group = '95+',
                population = (
                    # Get this year's 95-99s and 100+s and add them together
                    population_projection_un %>%
                        filter(
                            year == update_year,
                            age_group %in% c('95-99', '100+')
                        ) %>%
                        pull(population) %>%
                        sum
                )
            )
        ) %>%
        # Delete the old 95-99 and 100+ rows, to avoid confusion later
        rows_delete(
            tibble(
                year = c(update_year, update_year),
                age_group = c('95-99', '100+'),
            ),
            by = c('year', 'age_group')
        )
}

# Make a new column which we'll update...
death_projection_un$deaths_ageless <- death_projection_un$deaths

# Loop over time periods getting the death rate in an ageless scenario in that
# period
for(i in 1:length(period_start)) {
    # If the period is after ageing has been cured, then assign new death rates
    if(period_start[i] >= ageing_cured_year) {
        # Get death rate at the age at which we're assuming rates will freeze
        death_rate_ageless <-
            # Deaths in that age group... (table gives total over the period)
            death_projection_un %>%
            filter(period == period[i], age_group == old_age_name_un) %>%
            pull(deaths) /
            # ...divided by total population in that age group over the period
            population_projection_un %>%
            filter(
                year >= period_start[i], year < period_start[i] + 5,
                age_group == old_age_name_un) %>%
            pull(population) %>%
            sum
        
        
        # Thenm loop over age groups modifying them to have the lower death rate
        for(j in 1:length(old_age_groups_un)){
            # Update the death count in the ageless scenario...
            death_projection_un$deaths_ageless[
                death_projection_un$period == period[i] &
                    death_projection_un$age_group == old_age_groups_un[j]
            ] <-
                # By multiplying the ageless death rate...
                death_rate_ageless *
                # ...by the total population of this age group over the period
                population_projection_un %>%
                filter(
                    year >= period_start[i], year < period_start[i] + 5,
                    age_group == old_age_groups_un[j]) %>%
                pull(population) %>%
                sum
        }
    } else {
        # Else, this period is before ageing has been cured, then just assign
        # the regular death rates to it
        this_period <- death_projection_un$period == period[i]
        death_projection_un$deaths_ageless[this_period] <-
            death_projection_un$deaths[this_period]
    }

}

# Aggregate death projections (which are for individual years) to get total
# numbers of deaths in a given time period
death_projection_un_all_ages <-
    death_projection_un %>%
    group_by(period) %>%
    summarise(
        deaths = sum(deaths),
        deaths_ageless = sum(deaths_ageless)
    ) %>%
    mutate(
        deaths_diff = cumsum(deaths - deaths_ageless),
        # Create a 'year' variable to merge with the population data. It's the
        # end of the period because the deaths happen during that period
        year = as.numeric(substr(period, 6, 9))
    )

projection_combined_un_5_year <-
    population_projection_un %>%
    filter(year %in% c(period_start, 2100)) %>%
    group_by(year) %>%
    summarise(population = sum(population)) %>%
    # Include all years in the population projection so the first one isn't
    # discarded
    left_join(death_projection_un_all_ages) %>%
    # Remove unnecessary columns
    select(-c(period, deaths_ageless))

# Get the population in the ageless scenario
n_periods <- nrow(projection_combined_un_5_year)
projection_combined_un_5_year$population_ageless <-
    c(
        projection_combined_un_5_year$population[1], # 2020 population remains the same
        # Population in later years is found by adding the population to the
        # cumulative deaths averted by curing ageing from the previous time
        # periods
        projection_combined_un_5_year$population[2:n_periods] + 
            projection_combined_un_5_year$deaths_diff[2:n_periods]
    )

# Get an annual projection by interpolating the ageless population stats
# (the regular projections are already provided annually)
# Use zoo for the na.spline convenience function
library(zoo)
projection_combined_un <-
    population_projection_un %>%
    group_by(year) %>%
    summarise(population = sum(population)) %>%
    left_join(
        projection_combined_un_5_year %>% select(year, population_ageless),
        by = c('year')
    ) %>%
    mutate(
        population_ageless = na.spline(population_ageless)
    )


# Finally, all the UN data are given in 000s, so multiply up to get true numbers
projection_combined_un <-
    projection_combined_un %>%
    mutate(
        population = population*1e3,
        population_ageless = population_ageless*1e3
    )
    
#' ## 
#' 
#' The other major group projecting population is the IHME. Their forecasts are
#' rather different to the UN's, predicting that global population will peak
#' mid-century and decline after that.
#+ calculations_ihme


# In addition to the population data, already downloaded to graph the
# projections above, we also need death data for them. Download that too...
download_data(
    'http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_POP_2017_2100_DEATH_REFERENCE.zip',
    'ihme-population', data_dir
)

death_projection_ihme <-
    read_csv(
        file.path(
            data_dir,
            'ihme-population/IHME_POP_2017_2100_DEATH_REFERENCE_Y2020M05D01.CSV'
        )
    ) %>%
    simplify_ihme_projections %>%
    # simplify_ihme_projections calls the column population, so rename it deaths
    rename(deaths = population)
    # Note: there's something strange about the death rates for neonates. See
    # http://www.healthdata.org/gbd/faq#Why%20are%20age-specific%20neonatal%20mortality%20rates%20often%20greater%20than%201.0?
    # However, I can't work out how to apply that information to this situation,
    # which involves numbers rather than rates? Leaving this here for reference
    # in case it's useful at some point. (Note: it doesn't work!)
    # It doesn't matter for this application because I'll only be adjusting
    # death rates at older ages.)
    # mutate(
    #     deaths =
    #         case_when(
    #             age_group == 'Early Neonatal' ~ deaths/365*7,
    #             age_group == 'Late Neonatal' ~ deaths/365*(28-7),
    #             age_group == 'Post Neonatal' ~ deaths/365*(365-28)
    #         )
    # )


# Make a wraper function for calculating ageless population sizes for the IHME
# so we can test multiple scenarios
ihme_projection_ageless <- function(
    population_projection, death_projection
){
    # Make a tibble of population and death projections combined
    projection_combined <-
        inner_join(population_projection, death_projection) %>%
        mutate(
            # Calculate the death rate
            death_rate = deaths/population,
            # The ageless scenario starts with existing death rates...
            death_rate_ageless = death_rate
        )
    
    # ...then change all deaths of all 'old' people to the age were ageing stops
    for(year in unique(projection_combined$year)) {
        if(year > ageing_cured_year) {
            projection_combined$death_rate_ageless[
                projection_combined$year == year &
                    projection_combined$age_group %in% old_age_groups_ihme
            ] <-
                projection_combined$death_rate[
                    projection_combined$year == year &
                        projection_combined$age_group == old_age_name_ihme
                ]
        }
        
    }
    
    # Get the new number of deaths by multiplying the new death rate by population
    projection_combined$deaths_ageless <-
        projection_combined$death_rate_ageless *
        projection_combined$population
    
    # Find out the difference and add it to the existing population projection
    projection_combined$ageless_diff <-
        projection_combined$deaths - projection_combined$deaths_ageless
    
    # Get total populations by summarising
    projection_combined <-
        projection_combined %>%
        select(year, age_group, population, ageless_diff) %>%
        group_by(year) %>%
        summarise(
            population = sum(population),
            ageless_diff = sum(ageless_diff)
        ) %>%
        mutate(
            population_ageless = population + cumsum(ageless_diff)
        )
}

# Calculate the IHME ageless projections using the function
projection_combined_ihme <-
    ihme_projection_ageless(population_projection_ihme, death_projection_ihme)


# Cheat the IHME projections into line with the UN ones
un_ihme_cheat <-
    projection_combined_ihme %>% filter(year == 2020) %>% pull(population) -
    projection_combined_un %>% filter(year == 2020) %>% pull(population)

projection_combined_ihme_corrected <-
    projection_combined_ihme %>%
    filter(year >= 2020) %>%
    mutate(
        population = population - un_ihme_cheat,
        population_ageless = population_ageless - un_ihme_cheat
    )

#' ### UN
#' 
#' This graph shows the UN’s medium variant (bottom line), the ageless version
#' of that (ageing completely cured in 2025, the next line up), and the
#' deathless version from before (the line above that).
#' 

# Hackily pull out a few values we're going to be using to display over the next
# few paragraphs...
pop_medium_2050_ageless <-
    projection_combined_un %>%
    filter(year == 2050) %>%
    select(population_ageless) %>%
    as.numeric
pop_medium_2100_ageless <-
    projection_combined_un %>%
    filter(year == 2100) %>%
    select(population_ageless) %>%
    as.numeric

#' After curing ageing, the population would be
#' `r pretty_number(pop_medium_2050_ageless/1e9, dp=1)`bn in 2050
#' (`r pretty_number((pop_medium_2050_ageless - pop_medium_2050)/1e9, dp=1)`, or
#' `r pretty_number(pop_medium_2050_ageless/pop_medium_2050-1, sf=2, percentage=TRUE)`
#' larger), and
#' `r pretty_number(pop_medium_2100_ageless/1e9, dp=1)`bn
#' (`r pretty_number((pop_medium_2100_ageless - pop_medium_2100)/1e9, dp=1)`, or
#' `r pretty_number(pop_medium_2100_ageless/pop_medium_2100-1, sf=2, percentage=TRUE)`
#' larger) in 2100.
#' 
#' As a reminder, that compares to a difference of
#' `r pretty_number((pop_high_2050 - pop_low_2050)/1e9, dp=1)`
#' (`r pretty_number(pop_high_2050/pop_low_2050-1, sf=2, percentage=TRUE)`)
#' between the normal, death-included low and high variants in 2050, or
#' `r pretty_number(pop_high_2100/pop_low_2100-1, sf=2, percentage=TRUE)`
#' (`r pretty_number((pop_high_2100 - pop_low_2100)/1e9, dp=1)`)
#' between them in 2100.
#' 
#' Thus (obviously!), this ‘worst-case’ scenario is made slightly less extreme
#' by only curing age-related death rather than all death. The two scenarios are
#' compared to the regular medium variant in the next graph which, from top to
#' bottom, is deathless, ageless and regular medium variant.
#' 

ggplot(
    population_un,
    aes(x = year, y = population)
) +
    geom_line(
        data = subset(population_un, year <= 2020), colour = 'black'
    ) +
    geom_line(
        data = subset(all_projections_un_deathless, variant == 'Medium'),
        aes(y = population_deathless),
        colour = 'red'
    ) +
    geom_line(
        data = projection_combined_un,
        colour = 'green'
    ) +
    geom_line(
        data = projection_combined_un,
        aes(y = population_ageless),
        colour = 'darkgreen'
    )
#' Finally, this graph shows the IHME’s reference scenario with (blue) and
#' without (dark blue) deaths due to ageing, compared to the UN medium variant
#' with (green) and without (dark green). Again, the differences here show that
#' existing variation between model variants or scenarios is smaller but not
#' massively so than _totally curing ageing in a few years’ time_. Given a more 
#' realistic timeframe, it’s fair to assume that treatments for ageing will only
#' modestly increase uncertainty on future population projections.

ggplot(
    population_un,
    aes(x = year, y = population)
    ) +
    geom_line(
        data = subset(population_un, year <= 2020), colour = 'black'
    ) +
    geom_line(
        data = projection_combined_ihme_corrected,
        colour = 'blue'
    ) +
    geom_line(
        data = projection_combined_ihme_corrected,
        aes(y = population_ageless),
        colour = 'darkblue'
    ) +
    geom_line(
        data = projection_combined_un,
        colour = 'green'
    ) +
    geom_line(
        data = projection_combined_un,
        aes(y = population_ageless),
        colour = 'darkgreen'
    )

#' ## Life expectancy assumptions in population projections
#' 
#' Nonetheless, more work is most definitely needed here: though the changes are
#' only likely to be relatively modest compared to existing scenarios, the
#' lack of interest from demographers about future trends in life expectancy is
#' striking.
#' 
#' Taking the IHME projections as an example, this is what they anticipate
#' happening in (from top to bottom) Japan, all high-income countries averaged,
#' and the US until 2100. None of the scenarios offer any real variation, and
#' they all project that life expectancy will increase more slowly now than in
#' the past: a projection which has repeatedly been proven wrong in previous
#' forecasts.

ihme_le_projections <-
    read_csv(
        file.path(
            data_dir,
            'ihme-population/IHME_POP_2017_2100_LIFE_EXPECTANCY_Y2020M05D01.CSV'
        )
    )

ggplot(
    ihme_le_projections %>%
        filter(location_name == 'High-income', sex == 'Both'),
    aes(x = year_id, y = val, colour = scenario_name)
) +
    geom_line() +
    geom_line(
        data =
            ihme_le_projections %>%
            filter(location_name == 'Low-income', sex == 'Both')
    ) +
    geom_line(
        data =
            ihme_le_projections %>%
            filter(location_name == 'Japan', sex == 'Both')
    ) +
    geom_line(
        data =
            ihme_le_projections %>%
            filter(location_name == 'United States of America', sex == 'Both')
    ) +
    statto_style() +
    labs(
        title = 'Projections of life expectancy in high-income countries',
        x = 'year',
        y = 'life expectancy'
    )

#' In some less wealthy countries (India and Nigeria here), there is a bit more
#' variation, but presumably mainly driven by mortality which is reduced through
#' development, since it’s the ‘slower met need and education’ scenario at the
#' bottom. It’s also quite surprising to me that they think these countries will
#' plateau at lower levels than the high-income countries—why wouldn’t people
#' in Nigeria and India eventually live as long as rich people do today?

ggplot(
    ihme_le_projections,
    aes(x = year_id, y = val, colour = scenario_name)
) +
    geom_line(
        data =
            ihme_le_projections %>%
            filter(location_name == 'India', sex == 'Both')
    ) +
    geom_line(
        data =
            ihme_le_projections %>%
            filter(location_name == 'Nigeria', sex == 'Both')
    ) +
    statto_style() +
    labs(
        title = 'Projections of life expectancy in India and Nigeria',
        x = 'year',
        y = 'life expectancy'
    )

#' ## To do
#' 
#' It would be straightforward to add the option to make this slightly more
#' realistic by gradually reducing age-related deaths over a few decades, rather
#' than entirely removing them instantaneously or, even better, to gradually
#' reduce them starting with countries with the highest GDP per capita. If
#' anyone would like to do this please make a pull request!
#' 
#' Obviously best of all would be some more thorough modelling of these
#' consequences, but that would require some proper demographic expertise…
