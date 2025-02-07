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

gm_mean <- function(x) {
  exp(mean(log(x)))
}

sig_fig <- function(x, n = 3) {
  return(prettyNum(signif(x, digits = n), digits = n, format="fg", flag="#"))
}

pretty_number <- function(x, sf = 3, dp = NA, percentage = FALSE) {
  if(is.na(dp)) {
    if(percentage)
      return(paste0(sig_fig(x*100, sf),'%'))
    else
      return (sig_fig(x, sf))
  } else {
    if(percentage)
      return(paste0(round(x*100, dp),'%'))
    else
      return (round(x, dp))
  }

}

# Simple automation of downloading data from a URL to a subfolder of the data
# folder, as specified. If the download is zipped, it will unzip it ready for
# use.
download_data <-
  function(url, subpath, data_dir, force_download=FALSE) {
    dest_dir <- file.path(data_dir, subpath)
    dest_file <- file.path(dest_dir, basename(url))
    if(!file.exists(dest_file) | force_download) {
      # If the folder doesn't exist, create it
      if(!dir.exists(dest_dir)){dir.create(dest_dir)}
      download.file(url, dest_file)
      # If it's a zip, unzip it. We'll keep the zip to avoid re-downloading...
      if(file_ext(url) == 'zip') {
        unzip(dest_file, exdir = dest_dir)
      }
    }
  }

read_hmd <- function(filename, columns, col_widths) {
    # Make a tibble to return
    read_fwf(
        filename,
        fwf_widths(
            col_widths,
            columns
        ),
        skip = 3,
        na = c(".")
    ) %>%
        select(-'deleteme') %>%
        # This is a kludge which needs fixing - HMD uses eg 1973-  and 1973+ to
        # denote populations before and after a border change. This causes
        # issues so currently I'm just dropping them...
        mutate(year = as.numeric(year)) %>%
        drop_na(year)
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
  read_hmd(
    filename,
    # deleteme catches the + in 110+ at the end of the life tables, which
    # thankfully is in its own column
    c('year', 'age', 'deleteme', 'risk'),
    c(9,9,1,12)
    )
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
    
    read_hmd(
        filename,
        # deleteme catches the + in 110+ at the end of the life tables, which
        # thankfully is in its own column
        # 'population' is called 'total' in the source data
        columns = c('year', 'age', 'deleteme', 'female', 'male', 'population'),
        col_widths = c(9,9,1,20,16,16)
    )
  }
