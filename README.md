# _Ageless_ data and calculations

These scripts calculate various statistics used in the book [_Ageless: The new
science of getting older without getting
old_](https://andrewsteele.co.uk/ageless/).

The data and code in this release are those that were used for the first
publication of the book, in hardback form. Other references can be found at
[andrewsteele.co.uk/ageless/references](https://andrewsteele.co.uk/ageless/references/)
and other data and calculations simple enough not to require scripting are
available in
[this Google Drive folder](https://drive.google.com/drive/u/0/folders/1j9OEYBbGQgmXCCwe_ux15fBiORe0QQWh).

The reason I had to do these calculations is that I couldn’t find estimates for
some of the specific numbers in the literature, and I’d like these scripts to
help people wanting to talk or write about ageing to have accurate values for
things like the percentage of deaths caused by ageing in their country, in a
format that makes them as easy to update as possible when new data are released.

If anything is unclear or you spot any errors, please
[contact me](https://andrewsteele.co.uk/contact/). Feel free to download and
modify these scripts for your own purposes, or make pull requests if you’d like
to contribute. I’d be keen to tidy this up and make a self-generating ageing
data report but haven’t got time right now!

## `output/`

This is the output of the scripts using the data which I used when writing the
book. Most people will be interested in the `.html` files which are the
mini-reports generated showing the numbers. There are also `.txt` files where
messages and errors during compiling are dumped.

## `code/`

### `causes-with-age.R`

This looks at how different causes of death vary with age, based on
World Health Organization Global Burden of Disease (WHO GBD) data.

### `deaths-caused-by-ageing.R`

This calculates the number of deaths that can be attributed to ageing in
different countries and around the world, using WHO GBD data.

### `le-sex-difference.R`

This calculates the difference in lifespan between women and men, in different
countries and around the world, based on

### `mortality-with-age.R`

This looks at how risk of death varies with age.

### `init.R`

Initialisation and various convenience functions. Required packages are imported
here too.

### `spin-reports.R`

Convenience script to spin all the reports. I used this because I wanted
the reports to be code- and message-free so they'd be easier to read. If you
prefer a standard spin which shows its working, feel free not to use this.

### `stattoplot.R`

Styling and convenience functions for plotting, inspired by
[bbplot](https://github.com/bbc/bbplot).

## `data/`

### `who-gbd`

These data are from the World Health Organization Global Burden of Disease.
Some files are from the preexisting
[Data Resources](http://ghdx.healthdata.org/gbd-2019), while others were
compiled using the
[GBD Results Tool](http://ghdx.healthdata.org/gbd-results-tool). The actual data
used are provided here for convenience under a [Creative Commons
Attribution-NonCommercial 4.0 International
License](https://creativecommons.org/licenses/by-nc/4.0/).

It also contains three files with the suffix `-selected.csv` that are used,
unsurprisingly, to select various categories, causes of death and impairments
for convenience.

### `mortality.org/`

This folder is mostly empty as per the
[Human Mortality Database User Agreement](https://www.mortality.org/Public/UserAgreement.php)
which requests that copies of data downloaded are not passed to other users.

It is used in `mortality-with-age.R` via the `read_life_table_hmd()` and
`read_population_hmd()` convenience functions in `init.R` which expect this
folder to be populated. You will need to
[download](https://www.mortality.org/cgi-bin/hmd/hmd_download.php) the 
‘Period data’, ‘Population estimates (January 1)’ and ‘Life Tables - Both
Sexes’, and then move the `Population` and `bltper_1x1` folders into this folder
for the script to work.

`codelist.csv` translates between the HMD-specific country codes and the ISO
equivalents (eg GBR_NP instead of GBR for the UK), and may need updating if more
countries are added and you wish to include them.

### `un/`

UN data is used for GDP per capita in `deaths-caused-by-ageing.R`. The actual
data used are reproduced here with attribution to the
[UNdata](https://data.un.org/) service under its
[terms and conditions of use](https://data.un.org/Host.aspx?Content=UNdataUse).