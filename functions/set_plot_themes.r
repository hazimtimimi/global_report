# Graph theme components ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

theme_glb.rpt <- function(base_size=10, base_family="") {

  gray <- "#4D4D4D"
  black <- "#000000"

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
      strip.text.x = element_text(size=10),
      plot.title = element_text(hjust=0)
      #       plot.margin = unit(c(0,0))
    )
}



# Standard colours used by Sue

palette_standard <- function(type){

  switch(type,
        "incidence" = "#91A93E",        # green
        "tbhiv_incidence" = "#ED1D24",   # pinky red
        "mortality_exc_tbhiv" = "#1D91D1", # light blue
        "art" = "#0091D1"  # blue
  )

}

# Palette for treatment outcomes ----
# (adapted from colourblind-friendly palettes at http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)

palette_outcomes <- function() {

   # colours used in 2016
   # c("#009E73",   # Success (green)
   #   "#D55E00",   # Failure (red)
   #   "#E69F00",   # Died (orange)
   #   "#F0E442",   # Lost to follow-up (yellow)
   #   "#999999",   # Not evaluated (gray)
   #   "#CCCCCC"    # No data (light gray)
   #  )

  # Alternative taken from colour picker off 2016 report PDF
  c("Treatment success" = "#008670",  # Success (green)
    "Failure" = "#ED1D24",            # Failure (red)
    "Died" = "#F7941E",               # Died (orange)
    "Lost to follow-up" = "#E5DDB3",  # Lost to follow-up (yellow)
    "Not evaluated" = "#D1D3D4",      # Not evaluated (gray)
    "No data reported" = "#888888"    # No data (dark gray)
    )

}

# Alternative palette for treatment outcomes absolute numbers --
# (adapted from colourblind-friendly palettes at http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)

palette_outcomes_num <- function() {

  c("Treatment success"               = "#009E73",  # Success (green)
    "Failure/Died/Lost to follow-up"  = "#D55E00",  # Failure or others (red)
    "Not evaluated"                   = "#999999"   # Not evaluated (gray)
                      )

}


# Palette for age/sex pyramids ----

palette_agesex <- function() {

 c("Female" = "#EE1D23",  # Females (red used by Sue in 2017)
   "Male"   = "#00AAAD"   # Males   (turquoise used by Sue in 2017)
 )

}
