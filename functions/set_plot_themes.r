# Graph theme components ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

theme_glb.rpt <- function(base_size=10, base_family="") {
  colors <- ggthemes_data$few
  gray <- colors$medium['gray']
  black <- colors$dark['black']

  # I don't know why these last 3 parts are needed, but they are. List is here: http://docs.ggplot2.org/0.9.2.1/theme.html

  theme_bw(base_size=base_size, base_family=base_family) +
    theme(
      line = element_line(colour = gray),
      rect = element_rect(fill = "white", colour = NA),
      text = element_text(colour = black),
      axis.ticks.x = element_line(colour = gray),
      axis.ticks.y = element_blank(),
      legend.key = element_rect(colour = NA),
      ## Examples do not use grid lines
      panel.border = element_rect(colour = gray),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill="white", colour=NA),
      strip.text = element_text(hjust=0),
      strip.text.x = element_text(size=12),
      plot.title = element_text(hjust=0)
      #       plot.margin = unit(c(0,0))
    )
}



# Standard colours used by Sue

standard_palette <- function(type){

  switch(type,
        "incidence" = "#91A93E",        # green
        "tbhiv_incidence" = "#ED1D24",   # pinky red
        "art" = "#0091D1"  # blue
  )

}

# Palette for treatment outcomes ----
# (adapted from colourblind-friendly palettes at http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)

outcomes_palette <- function() {

   c("#009E73",   # Success (green)
     "#D55E00",   # Failure (red)
     "#E69F00",   # Died (orange)
     "#F0E442",   # Lost to follow-up (yellow)
     "#999999",   # Not evaluated (gray)
     "#CCCCCC"    # No data (light gray)
    )

  # Alternative taken from colour picker off 2016 report PDF doesn't work so well.
  # c("#008670",   # Success (green)
  #   "#ED1D24",   # Failure (red)
  #   "#F7941E",   # Died (orange)
  #   "#E5DDB3",   # Lost to follow-up (yellow)
  #   "#D1D3D4",   # Not evaluated (gray)
  #   "#CCCCCC"    # No data (light gray)
  #   )

}

# Palette for age/sex pyramids ----

agesex_palette <- function() {

 c("#EC008C",  # Females (garish pink used by Sue in 2016)
   "#F7941E"   # Males (orange)
 )

}
