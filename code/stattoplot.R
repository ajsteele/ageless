library(ggplot2)
library(scales)
library(ggrepel)
library(ggforce)

update_geom_defaults("text_repel", list(colour = "black", family = 'Roboto'))
update_geom_defaults("text", list(colour = "black", family = 'Roboto'))

statto_style <- function() {
  font <- 'Roboto'
  
  ggplot2::theme(
    
    #Text format:
    text = ggplot2::element_text(family=font,
                                 size=10,
                                 color="#222222"),
    
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family='Roboto Medium',
                                       size=16,
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family='Roboto Light',
                                          size=18,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=18,
                                        color="#222222"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=18,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}

circle_spread <- function(
  circles, padding = min(circles$r), tinypad = padding/100
) {
  #' Generates non-overlapping circles to plot
  #' 
  #' @description This function takes a load of circle positions and radii
  #' and moves the circles on the x-axis so that they no longer overlap. It does
  #' so by moving the smaller circles outwards until they no longer overlap the
  #' larger ones.
  #'
  #' It was written for this rather specific use-case and could be generalised.
  
  # Order of circles firstly by y position, then by size if they clash
  y_r_order <- order(circles$y, circles$r)
  # Order of circles from largest to smallest
  size_order <- order(-circles$r)
  # Initialise the direction that a circle will move: 1 is right, -1 is left
  circles$direction <- 1
  # Make the directions alternate by y-order to ensure balanced-looking output
  circles$direction[y_r_order] <-
    circles$direction[y_r_order] * rep_len(c(1,-1),nrow(circles))
  
  # Main loop to iteratively resolve overlaps
  overlaps <- TRUE
  while(overlaps) {
    # Go through circles in order of size, except the smallest one
    for(i in head(size_order, -1)) {
      # For a circle i, go through the smaller ones
      for(j in size_order[(which(size_order == i)+1):length(size_order)]) {
        # Check if the circles overlap
        if(
          sqrt((circles$x[j]-circles$x[i])^2 + (circles$y[j]-circles$y[i])^2) <
          circles$r[j] + circles$r[i] + padding
        ) {
          # If they do, move the smaller one far enough that they don't any more
          circles$x[j] <-
            circles$x[i] +
            circles$direction[j]*sqrt(
              (circles$r[j] + circles$r[i] + padding)^2 -
                (circles$y[j] - circles$y[i])^2
            ) +
            circles$direction[j]*tinypad
        }
      }
    }
    # Test all pairs of circles for overlaps
    overlaps <- FALSE
    for(i in 1:(nrow(circles)-1)) {
      for(j in (i+1):nrow(circles)) {
        # Check if the circles overlap
        if(
          sqrt((circles$x[j]-circles$x[i])^2 + (circles$y[j]-circles$y[i])^2) <
          circles$r[j] + circles$r[i] + padding
        )
        {
          # If there are overlaps, set overlaps to true and break out of the
          # inner loop
          overlaps <- TRUE
          break
        }
      }
      # And if you got here by breaking out of the inner loop, break out of the
      # outer one too
      if(overlaps) {break}
    }
  }
  
  # Return the generated circles
  circles
}

geom_labelled_hline <- function(
  yintercept, text, x_text = 0, colour = '#eeeeee', size = 2
  ) {
  #' A labelled horizontal line
  
  # Return a list so multiple geoms can be attached to one-another
  list(
    # Draw the line...
    geom_hline(
      yintercept = yintercept,
      colour = colour, size = size
    ),
    # ...and the text
    geom_text(
      aes(x = x_text, y = yintercept, label = text),
      vjust = 'bottom', hjust = 'left'
    )
  )
}