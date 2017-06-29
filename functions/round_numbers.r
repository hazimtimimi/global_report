# Report rounding convention ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Report rounding convention
# - 0 is written as "0"
# - values under 0.1 are written "<0.1"
# - from 0.1 to under 10 are rounded to 2 significant figures (2 decimal places < 1 and 1 decimal place >= 1)
# - 10 to under 1000 are written rounded to the whole number (3 significant figures).
# - 1000 upwards are rounded to 3 significant figures

# - data that are not reported, but could be are represented as empty cells and should be accompanied by a footnote.
# - data that cannot be calculated, either because of missing data, data was not requested, or any other reason are
#   represented with an en-dash.



# Return a string formated for display
# Depends on whether dealing with thousands or rates (although by 2017 the distinction had disappeared, but keeping the
# function parameter for backward compatibility.
# thouEst introduced for estimated thousands so that don't use 3 sig figs for small numbers (used for estimated number
# of TB/HIV incident cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

frmt <- function(x, rates=FALSE, thou=FALSE, thouEst=FALSE) {

  # Standard format for numbers
  fstring <-  ifelse(x==0, "0",
              ifelse(x < 0.1, "<0.1",
              ifelse(signif(x, 2) < 1, formatC(signif(x,2), format="f", digits=2),
              ifelse(signif(x, 2) < 10, formatC(signif(x,2), format="f", digits=1),
              ifelse(signif(x, 3) < 100, formatC(signif(x, 2), big.mark=" ", format="d"),
                     formatC(signif(x, 3), big.mark=" ", format="d"))))))


  # A few exceptions to the standard rules
  # Use 3 decimal places for numbers representing "thousands" that are less than 1
  if (thou=TRUE) {

    fstring <-  ifelse(x < 0.01, "<0.01",
                ifelse(signif(x, 2) < 1, formatC(signif(x,3), format="f", digits=3),
                       fstring))
  }


  # Use only 2 decimal places for numbers representing "thousands" that are between 0.1 and 1,
  # (inherits 3 decimal places below 0.1 as per the prvious if statement)
  if (thou==TRUE & thouEst==TRUE) {

    fstring <-  ifelse(signif(x, 2) < 1 & signif(x, 3) >= 0.1, formatC(signif(x,2), format="f", digits=2),
                       fstring)
  }

  return(fstring)
}



# Paste together uncertainty intervals enclosed by parentheses to return
# a single string. Don't do this if any of best, lo, hi are NA!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

frmt_intervals <- function(best,lo,hi,rates=FALSE, thou=FALSE, thouEst=FALSE){

  x <- NA

  if (thou==TRUE) {
    #format as thousands
    x <- ifelse(is.na(best) | is.na(lo) | is.na(hi) ,
                NA,
                paste0("(", frmt(lo, thou=TRUE), "–", frmt(hi, thou=TRUE), ")"))
  }

  if (thou==TRUE & thouEst==TRUE) {
    #format as thousands
    x <- ifelse(is.na(best) | is.na(lo) | is.na(hi) ,
                NA,
                paste0("(", frmt(lo, thou=TRUE, thouEst=TRUE), "–", frmt(hi, thou=TRUE, thouEst=TRUE), ")"))
  }

  if (rates==TRUE) {
    # format as rates
    x <- ifelse(is.na(best) | is.na(lo) | is.na(hi) ,
                NA,
                paste0("(", frmt(lo, rates=TRUE), "–", frmt(hi, rates=TRUE), ")"))
  }

  return(x)

}


# Simple rounding function that returns a string rounded to the nearest integer and
# uses a space as the thousands separator as per WHO standard.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rounder <- function(x) {

    ifelse(is.na(x), NA,
           formatC(round(x,0), big.mark=" ", format="d")
           )
}



