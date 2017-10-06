
# Better row sum ----
# This function sums rows ignoring NAs unless all are NA
# [rowSums() returns 0 instead of NA if all are NA and you use na.rm=TRUE]
# use it like this
# df$snu <- sum_of_row(df[c('new_sn', 'new_su')])
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sum_of_row <- function(x) {
  tosum <- as.matrix(x)
  summed <- rowMeans((tosum), na.rm=TRUE) * rowSums(!is.na((tosum)))
  # Flush out any NaN's
  summed <- ifelse(is.nan(summed), NA, summed)
  return(summed)
}


# Convert a null (NA) to zero
NZ <- function(x){
  x <- ifelse(is.na(x),0,x)
  return(x)
}
