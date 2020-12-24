library(knitr)
library(xfun)

scripts_to_spin <-
  c(
    'mortality-with-age.R',
    'deaths-caused-by-ageing.R',
    'causes-with-age.R',
    'le-sex-difference.R'
  )

github_repo <- 'https://github.com/ajsteele/ageless/'
github_file_prepend <- 'blob/master/'
github_url <- paste0(github_repo, github_file_prepend)

pretty_spin <- function(filename, output_dir = '../output/', addendum = TRUE) {
  message('Pretty-spinning ', filename, '...', appendLF = FALSE)
  
  # Turn the script name into a 
  filename_sans_ext <- sans_ext(filename)
  
  # Write console output to a separate text file
  con <- file(file.path(output_dir, with_ext(filename_sans_ext, 'txt')))
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  # TODO - add error catching so if the spinning fails then the messages are
  # displayed in the terminal again
  
  # Don't show any of the messages in the spun document - this is for non-nerds!
  opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
  # Use figures which are 800x800 as default, save to output/figures
  dpi <- 72
  opts_chunk$set(
    dpi = dpi,  fig.width = 800/dpi, fig.height = 800/dpi,
    fig.path = paste0(file.path(output_dir, 'figures'), '/')
  )

  if(addendum) {
    # Build an addendum to put in pretty reports explaining that the code and
    # output aren't here, like a normal spun R script
    pretty_report_addendum <-
      c(
        "#' ## About this report",
        "#'",
        "#' This report was generated from an R script which performs the",
        "#' underlying calculations. The code and console output have been",
        "#' removed to make it easier to read. If you want to see the code,",
        "#' you can check out the latest version at",
        paste0("#' [GitHub](", github_url, 'code/', filename, "),"),
        "#' and if you want to view the console output, see",
        paste0(
          "#' [", with_ext(filename_sans_ext, 'txt'), "](",
          paste0(github_url, 'output/',with_ext(filename_sans_ext, 'txt')), ")."
        ),
        sep = '\n'
      )
  } else {
    pretty_report_addendum <- ''
  }
  
  script_file <- file(filename)
  script_to_spin = read_utf8(script_file)
  close(script_file)
  
  output_html <- 
    spin(
      text = c(
        script_to_spin,
        pretty_report_addendum,
        sep = '\n\n'
      )
    )
  
  write_utf8(
    output_html,
    file.path(output_dir, with_ext(filename_sans_ext, '.html'))
  )
  
  # Restore output to console
  sink()
  sink(type="message")
  
  # Restore default spin options
  opts_chunk$set(echo = TRUE, warning = TRUE, message = TRUE)
  
  message('complete!')
}

for(script_to_spin in scripts_to_spin) {
  pretty_spin(script_to_spin)
}
