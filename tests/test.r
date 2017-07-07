# All the following lines using identical() should result in TRUE.
# Alternatively use the testthat package
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# General rounding convention:

# - 0 is written as "0"                                                                             "0"
# - values that round to under 0.1 are written "<0.1"                                            "<0.1"
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

# - values that round to under 0.01 are written "<0.01"                                         "<0.01"
# - values between 0.01 and 0.1 are rounded to 2 significant figures (3 decimal places)         "0.0NN"
# - values between 0.1 and under 1 are rounded to 2 significant figures (2 decimal places)       "0.NN"



source("functions/round_numbers.r")

# Test defaults:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

identical(display_num(0),"0")
identical(display_num(0.08612),"<0.1")
identical(display_num(0.091),"<0.1")

identical(display_num(0.099),"0.1")

identical(display_num(0.100),"0.1")
identical(display_num(0.4134),"0.4")
identical(display_num(0.4934),"0.5")
identical(display_num(0.9111),"0.9")

identical(display_num(0.99),"1.0")
identical(display_num(2.4168),"2.4")
identical(display_num(2.4968),"2.5")
identical(display_num(9.91),"9.9")

identical(display_num(9.99),"10")
identical(display_num(11.1),"11")
identical(display_num(99.9),"100")

identical(display_num(125.6),"126")
identical(display_num(7813.2),"7 810")

identical(display_num(24517),"24 500")
identical(display_num(24517/1000),"25")

identical(display_num(7853109),"7 850 000")
identical(display_num(7853109/1000000),"7.9")

identical(display_num(10409543),"10 400 000")

# Test exceptions:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

identical(display_num(0, thousands=TRUE),"0")
identical(display_num(0.006, thousands=TRUE),"<0.01")
identical(display_num(0.08612, thousands=TRUE),"0.086")
identical(display_num(0.091, thousands=TRUE),"0.091")

identical(display_num(0.0999, thousands=TRUE),"0.10")

identical(display_num(0.100, thousands=TRUE),"0.10")
identical(display_num(0.4134, thousands=TRUE),"0.41")
identical(display_num(0.4934, thousands=TRUE),"0.49")
identical(display_num(0.9111, thousands=TRUE),"0.91")

identical(display_num(0.99, thousands=TRUE),"0.99")


# All of the following should give the same results as when thousands=FALSE

identical(display_num(2.4168, thousands=TRUE),"2.4")
identical(display_num(2.4968, thousands=TRUE),"2.5")
identical(display_num(9.91, thousands=TRUE),"9.9")

identical(display_num(9.99, thousands=TRUE),"10")
identical(display_num(11.1, thousands=TRUE),"11")
identical(display_num(99.9, thousands=TRUE),"100")

identical(display_num(125.6, thousands=TRUE),"126")
identical(display_num(7813.2, thousands=TRUE),"7 810")

identical(display_num(24517, thousands=TRUE),"24 500")
identical(display_num(24517/1000, thousands=TRUE),"25")

identical(display_num(361184/1000, thou=TRUE),"361")

identical(display_num(7853109, thousands=TRUE),"7 850 000")
identical(display_num(7853109/1000000, thousands=TRUE),"7.9")

identical(display_num(10409543, thousands=TRUE),"10 400 000")
identical(display_num(10409543/1000000, thousands=TRUE),"10")




