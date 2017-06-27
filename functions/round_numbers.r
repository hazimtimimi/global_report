# Report rounding convention ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Report rounding convention
# - 0 is written as "0"
# - values under 0.1 are written "<0.1"
# - from 0.1 to under 10 are written rounding 1 decimal place
# - 10 to under 100 are written rounded to the whole number.
# - 100 and above are written as 3 significant figures for rates and 2 significant figures for absolute numbers.
# - data that are not reported, but could be are represented as empty cells and should be accompanied by a footnote.
# - data that cannot be calculated, either because of missing data, data was not requested, or any other reason are represented with an en-dash (ctrl - on your keyboard).


# rounding function for absolute numbers
round.conv <- function(x) {
  ifelse(x==0, 0,
  ifelse(x < 0.1, "<0.1",
  ifelse(signif(x, 2) < 1, formatC(round(x,1), format='f', digits=1),
  ifelse(signif(x, 2) < 10, sapply(signif(x,2), sprintf, fmt="%#.2g"), signif(x, 2)))))
}

# rounding convention for rates
round.conv.rates <- function(x) {
  ifelse(x==0, 0,
  ifelse(x < 0.1, "<0.1",
  ifelse(signif(x, 2) < 10, formatC(round(x,1), format='f', digits=1),
  ifelse(signif(x, 3) < 100, signif(x, 2), signif(x, 3)))))
}

# Depends on whether dealing with thousands or rates. In general, 0 is 0, under .1 to "<0.1", then appropriate sig figs.
# Numbers < 1 are only rounded to 1 sig fig
frmt <- function(x, rates=FALSE, thou=FALSE) {
  ifelse(x==0, "0",
  ifelse(x < 0.01 & thou==TRUE, "<0.01",
  ifelse(x < 0.1 & thou==FALSE, "<0.1",
  ifelse(signif(x, 2) < 1 & thou==TRUE, formatC(signif(x,2), format='f', digits=2),
  ifelse(signif(x, 2) < 1, formatC(signif(x,1), format='f', digits=1),
  ifelse(signif(x, 2) < 10, formatC(signif(x,2), format='f', digits=1),
  ifelse(x > 1 & rates==FALSE, formatC(signif(x, 2), big.mark=" ", format='d'),
  ifelse(signif(x, 3) < 100, formatC(signif(x, 2), big.mark=" ", format='d'), formatC(signif(x, 3), big.mark=" ", format='d')))))))))
}

# Simple rounder that just adds in the thousands separator
rounder <- function(x, decimals=FALSE) {

  if (decimals==TRUE) {
    ifelse(is.na(x), NA,
    ifelse(x==0, 0,
    ifelse(x < 0.01, "<0.01",
    ifelse(round(x,2) < 0.1, formatC(round(x,2), format='f', digits=2),
    ifelse(round(x,1) < 10, formatC(round(x,1), format='f', digits=1), formatC(round(x,0), big.mark=" ", format='d') )))))

  } else {

    ifelse(is.na(x), NA,
    ifelse(x==0, 0,
    ifelse(x < 1, "< 1", formatC(round(x,0), big.mark=" ", format='d'))))
  }
}



