# Report rounding convention ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# General rounding convention:

# - 0 is written as "0"                                                                             "0"
# - values under 0.1 are written "<0.1"                                                          "<0.1"
# - from 0.1 to under 1 are rounded to 2 significant figures (2 decimal places)                   "0.N"
# - from 1 to 10 are rounded to 2 significant figures (1 decimal place)                           "N.N"
# - 10 to under 100 are rounded to 2 significant figures (the whole number, no decimal places)     "NN"
# - 100 to under 1000 are rounded to 3 significant figures (the whole number, no decimal places   "NNN"
# - 1000 upwards are rounded to 3 significant figures                                       "N NN0 000"
#
# - data that are not reported, but could be are represented as empty cells and should be
#   accompanied by a footnote.
# - data that cannot be calculated, either because of missing data, data was not requested,
#   or any other reason are represented with a dash.

# Variations:

# 1. When the number represents thousands increase the precision for numbers below 1:

# - values under 0.01 are written "<0.01"                                                       "<0.01"
# - values between 0.01 and 0.1 are rounded to 2 significant figures (3 decimal places)         "0.0NN"
# - values between 0.1 and under 1 are rounded to 2 significant figures (2 decimal places)       "0.NN"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Return a string formated for display
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

display_num <- function(x, thousands=FALSE) {

  # Standard format for numbers
  fstring <-  ifelse(x==0, "0",
              ifelse(signif(x, 1) < 0.1, "<0.1",
              ifelse(signif(x, 2) < 1, formatC(signif(x,1), format="f", digits=1),
              ifelse(signif(x, 2) < 10, formatC(signif(x,2), format="f", digits=1),
              ifelse(signif(x, 3) < 100, formatC(signif(x, 2), big.mark=" ", format="d"),
                     formatC(signif(x, 3), big.mark=" ", format="d"))))))


  # A few exceptions to the standard rules
  # When the number represents thousands increase the precision for numbers below 1
  if (thousands==TRUE) {

    fstring <-  ifelse(x==0, "0",
                ifelse(x < 0.01, "<0.01",
                ifelse(signif(x, 2) < 0.1, formatC(signif(x,2), format="f", digits=3),
                ifelse(signif(x, 2) < 1, formatC(signif(x,2), format="f", digits=2),
                       fstring))))
  }

  return(fstring)
}



# Paste together uncertainty intervals enclosed by parentheses to return
# a single string. Don't do this if any of best, lo, hi are NA!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# frmt_intervals <- function(best,lo,hi,rates=FALSE, thou=FALSE, thouEst=FALSE){
#
#   x <- NA
#
#   if (thou==TRUE) {
#     #format as thousands
#     x <- ifelse(is.na(best) | is.na(lo) | is.na(hi) ,
#                 NA,
#                 paste0("(", frmt(lo, thou=TRUE), "–", frmt(hi, thou=TRUE), ")"))
#   }
#
#   if (thou==TRUE & thouEst==TRUE) {
#     #format as thousands
#     x <- ifelse(is.na(best) | is.na(lo) | is.na(hi) ,
#                 NA,
#                 paste0("(", frmt(lo, thou=TRUE, thouEst=TRUE), "–", frmt(hi, thou=TRUE, thouEst=TRUE), ")"))
#   }
#
#   if (rates==TRUE) {
#     # format as rates
#     x <- ifelse(is.na(best) | is.na(lo) | is.na(hi) ,
#                 NA,
#                 paste0("(", frmt(lo, rates=TRUE), "–", frmt(hi, rates=TRUE), ")"))
#   }
#
#   return(x)
#
# }


# Simple rounding function that returns a string rounded to the nearest integer and
# uses a space as the thousands separator as per WHO standard.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rounder <- function(x) {

    ifelse(is.na(x), NA,
           formatC(round(x,0), big.mark=" ", format="d")
           )
}



