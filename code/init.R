library(tidyverse)

# Packages for generating reports
library(knitr)
library(kableExtra)

# Packages for data
library(countrycode)
library(wbstats)

# For plotting
source('stattoplot.R')

data_dir <- '../data'
output_dir <- '../output'

sig_fig <- function(x, n = 3) {
  return(formatC(signif(x, digits = n), digits = n, format="fg", flag="#"))
}

pretty_number <- function(x, sf = 3, percentage = FALSE) {
  if(percentage){
    return(paste0(sig_fig(x*100, sf),'%'))
  } else
    return (sig_fig(x, sf))
}

read_life_table_hmd <-
  function(
    country_code,
    file_prefix = file.path('..', 'data', 'mortality.org', 'bltper_1x1'),
    file_suffix = '.bltper_1x1.txt'
  ) {
  # Generate the filename based on the country code, prefix and suffix
  filename <-
    paste0(
      file.path(file_prefix, country_code),
      file_suffix
    )
  
  # Make a tibble to return
  read_fwf(
    filename,
    fwf_widths(
      c(9,9,1,12),
      # deleteme catches the + in 110+ at the end of the life tables, which
      # thankfully is in its own column
      c('year', 'age', 'deleteme', 'risk')
    ),
    skip = 3
  ) %>%
    select(-'deleteme')
  }

read_population_hmd <-
  function(
    country_code,
    file_prefix = file.path('..', 'data', 'mortality.org', 'Population'),
    file_suffix = '.Population.txt'
  ) {
    # Generate the filename based on the country code, prefix and suffix
    filename <-
      paste0(
        file.path(file_prefix, country_code),
        file_suffix
      )
    
    # Make a tibble to return
    read_fwf(
      filename,
      fwf_widths(
        c(9,9,1,20,16,16),
        # deleteme catches the + in 110+ at the end of the life tables, which
        # thankfully is in its own column
        # 'population' is called 'total' in the source data
        c('year', 'age', 'deleteme', 'female', 'male', 'population')
      ),
      skip = 3
    ) %>%
      select(-'deleteme') %>%
      # This is a kludge which needs fixing - HMD uses eg 1973-  and 1973+ to
      # denote populations before and after a border change. This causes issues
      # so currently I'm just dropping them...
      drop_na(year)
  }
