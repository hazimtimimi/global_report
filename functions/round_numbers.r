# Report rounding convention ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# General rounding convention:

# - 0 is written as "0"                                                                             "0"
# - values under 0.1 are written "<0.1"                                                          "<0.1"
# - from 0.1 to under 1 are rounded to 2 significant figures (2 decimal places)                  "0.NN"
# - from 1 to under 10 are rounded to 2 significant figures (1 decimal place)                     "N.N"
# - 10 to under 100 are rounded to 2 significant figures (the whole number, no decimal places)     "NN"
# - 100 to under 1000 are rounded to 3 significant figures (the whole number, no decimal places   "NNN"
# - 1000 upwards are rounded to 3 significant figures                                       "N NN0 000"
#
# - data that are not reported, but could be are represented as empty cells and should be
#   accompanied by a footnote.
# - data that cannot be calculated, either because of missing data, data was not requested,
#   or any other reason are represented with a dash.

# Variations:

# 1. When the number represents thousands, show numbers between 0.01 and 0.1:

# - values under 0.01 are written "<0.01"                                                       "<0.01"
# - values between 0.01 and under 0.1 are rounded to 2 significant figures (3 decimal places)   "0.0NN"


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Return a number as a string formated for display
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

display_num <- function(x, thousands=FALSE) {

  # Standard format for numbers
  fstring <-  ifelse(x==0, "0",
              ifelse(signif(x, 1) < 0.1, "<0.1",
              ifelse(signif(x, 2) < 1, formatC(signif(x,2), format="f", digits=2),
              ifelse(signif(x, 2) < 10, formatC(signif(x,2), format="f", digits=1),
              ifelse(signif(x, 3) < 100, formatC(signif(x, 2), big.mark=" ", format="d"),
                     formatC(signif(x, 3), big.mark=" ", format="d"))))))


  # A few exceptions to the standard rules
  # When the number represents thousands increase the precision for numbers below 1
  if (thousands==TRUE) {

    fstring <-  ifelse(x==0, "0",
                ifelse(x < 0.01, "<0.01",
                ifelse(signif(x, 2) < 0.1, formatC(signif(x,2), format="f", digits=3),
                       fstring)))
  }

  return(fstring)
}



# Paste together uncertainty intervals enclosed by parentheses to return
# a single string. Don't do this if any of best, lo, hi are NA!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

display_intervals <- function(best, lo, hi, thousands=FALSE){

  interval_string <- ifelse(is.na(best) | is.na(lo) | is.na(hi) ,
                            NA,
                            paste0("(", display_num(lo, thousands), "–", display_num(hi, thousands), ")"))

  return(interval_string)

}


# Simple rounding function that returns a string rounded to the nearest integer and
# uses a space as the thousands separator as per WHO standard.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rounder <- function(x) {

    ifelse(is.na(x), NA,
           formatC(round(x,0), big.mark=" ", format="d")
           )
}



# Calculate % using numerator and denominator, format the output and cap at 100%
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

display_cap_pct <- function(numerator, denominator) {

  pct <- ifelse(is.na(numerator) | is.na(denominator) | denominator == 0, "",
         ifelse(numerator > denominator, ">100", display_num(numerator * 100 / denominator)))

  return(pct)
}


# Calculate % change using start and end values, format the output and cap between 0.1% and 100%
# without using > and < symbols and allowing for negative numbers
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

display_change_cap_pct <- function(start_value, end_value) {

  pct <- ifelse(is.na(end_value) | is.na(start_value) | start_value == 0, NA,
                signif((end_value - start_value) * 100 / start_value, 2))

  pct <- ifelse(is.na(pct), "",
          ifelse(pct > 100, 100,
          ifelse(pct < -100, -100,
          ifelse(abs(pct) < 0.1, 0, pct))))

  return(pct)
}
