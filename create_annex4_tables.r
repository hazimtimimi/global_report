# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to produce annex 4 tables for the global TB report.
#
# Hazim Timimi, September 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Instructions ----
#
#   1. Run this script (check flags to be used in section A)
#   2. Refresh Excel tables that link to this script's output
#   3. See notes for further steps
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Clear the decks ----
rm(list=ls())



# A: SET A FEW FLAGS AND CONSTANTS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Establish the report year
report_year <- 2015

# The following are convenience variables since notification and most other data sets will run up to the
# year before the reporting year and outcomes will run up to two years before the reporting year
notification_maxyear <- (report_year - 1)
outcome_maxyear      <- (report_year - 2)


# Apply the Russian fudge ------
# flag for whether to suppress calculation of %
# of notified TB patients who knew their HIV status (applies to table 6)

russianfudge <- TRUE


# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)

# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# scripts_folder: Folder containing these scripts
# annex4_folder:  Parent folder under which CSV files will be saved
# rdata_folder:   Folder in which to find a .RData file (if available)
# rdata_name:     Name of a .RData file containing copy of database views
# use_live_db:    Flag -- if TRUE then data loaded durectly from the global TB database
#                 if FALSE then data loaded from the .RData file
#
# and, from the global TB database,
#
# n:     dataframe copy of view_TME_master_notification
# nk:    dataframe copy of view_TME_master_notification_exceptions
# tbhiv: dataframe copy of view_TME_master_TBHIV_for_aggregates
# e:     dataframe copy of view_TME_estimates_epi
# eraw:  dataframe copy of view_TME_estimates_epi_rawvalues
# f:     dataframe copy of view_TME_master_finance
# be:    dataframe copy of view_TME_master_budget_expenditure
# p:     dataframe copy of view_TME_estimates_population
# o:     dataframe copy of view_TME_master_outcomes
# s:     dataframe copy of view_TME_master_strategy
# i:     dataframe copy of view_TME_master_data_collection
# a:     dataframe copy of view_TME_aggregated_estimates_epi
# araw:  dataframe copy of view_TME_aggregated_estimates_epi_rawvalues
# d:     dataframe copy of view_TME_master_dr_surveillance
# dsvy:  dataframe copy of view_TME_master_drs
# dictionary:  dataframe copy of view_TME_data_dictionary
# datacodes:   dataframe copy of view_TME_data_codes
# emdr:  dataframe copy of view_TME_estimates_mdr
# emdrn: dataframe copy of view_TME_estimates_mdr_in_notified
# emdra: dataframe copy of view_TME_aggregated_estimates_mdr_in_notified
#
# data.date: When the source datasets were created
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

scripts_folder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616

setwd(scripts_folder)

source("set_environment.r")  # particular to each person so this file is in the ignore list

if (use_live_db==TRUE){

  # load the data directly from the global TB database
  source("load_data_from_database.r")

} else {

  # load up an .RData file containing a copy of the database views
  load(paste(rdata_folder, rdata_name, sep="/"))
}


# Create output folder (only if it doesn't yet exist), and move to it
dir.create(annex4_folder, recursive = TRUE, showWarnings = FALSE)

dir.create(file.path(annex4_folder, "linked_CSVs"), showWarnings = FALSE)

setwd(file.path(annex4_folder, "linked_CSVs"))



# B: OK, NOW GO FOR IT ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library("dplyr")


# functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# rounding convention
# Depends on whether dealing with thousands or rates. In general, 0 is 0, under .1 to "<0.1", then appropriate sig figs.
# thouEst introduced for estimated thousands so that don't use 3 sig figs for small numbers (used for estimated number of TB/HIV incident cases)

frmt <- function(x, rates=FALSE, thou=FALSE, thouEst=FALSE) {
  ifelse(x==0, "0",
  ifelse(x < 0.01 & thou==TRUE, "<0.01",
  ifelse(x < 0.1 & thou==FALSE, "<0.1",
  ifelse(signif(x, 2) < 1 & thou==TRUE & thouEst==FALSE, formatC(signif(x,3), format="f", digits=3),
  ifelse(signif(x, 3) < 0.1 & thou==TRUE & thouEst==TRUE, formatC(signif(x,3), format="f", digits=3),
  ifelse(signif(x, 2) < 1, formatC(signif(x,2), format="f", digits=2),
  ifelse(signif(x, 2) < 10, formatC(signif(x,2), format="f", digits=1),
  ifelse(x > 1 & rates==FALSE, formatC(signif(x, 2), big.mark=" ", format="d"),
  ifelse(signif(x, 3) < 100, formatC(signif(x, 2), big.mark=" ", format="d"), formatC(signif(x, 3), big.mark=" ", format="d"))))))))))
}


# paste together uncertainty intervals enclosed by parentheses
frmt_intervals <- function(best,lo,hi,rates=FALSE, thou=FALSE, thouEst=FALSE){
  x <- NA
  # Only show confidence intervals if any of best, lo, hi are not NA
  if (thou==TRUE) {
    #format as thousands
    x <- ifelse(is.na(best) | is.na(lo) | is.na(hi) , NA, paste0("(", frmt(lo, thou=TRUE), "–", frmt(hi, thou=TRUE), ")"))
  }
  if (thou==TRUE & thouEst==TRUE) {
    #format as thousands
    x <- ifelse(is.na(best) | is.na(lo) | is.na(hi) , NA, paste0("(", frmt(lo, thou=TRUE, thouEst=TRUE), "–", frmt(hi, thou=TRUE, thouEst=TRUE), ")"))
  }
  if (rates==TRUE) {
    # format as rates
    x <- ifelse(is.na(best) | is.na(lo) | is.na(hi) , NA, paste0("(", frmt(lo, rates=TRUE), "–", frmt(hi, rates=TRUE), ")"))
  }
  return(x)
}


# Simple rounder that also adds in the thousands separator
rounder <- function(x) {
  ifelse(is.na(x), NA, formatC(round(x,0), big.mark=" ", format="d"))
}

# Combine datasets ----
# Creates a table in order of countries, who regional aggregates and global aggregate, with a header row
# preceding the regional aggregates
combine_tables <- function(by_country, by_region, by_global){

  # Add a placeholder row at the end of the countries table to use as a header for the regions
  by_country[nrow(by_country)+1,1] <- "WHO regions"

  # Remove the unnecessary "WHO " text at the beginning of region names
  by_region$entity <- sub("^WHO |^WHO/PAHO ", "", by_region$entity)

  # Remove the unnecessary " Aggregate" text at the end of the global aggregate
  by_global$entity <- sub(" Aggregate", "", by_global$entity)


  a <- rbind(by_country, by_region, by_global)
  return(a)
}


# Better row sum ----
# This function sums rows ignoring NAs unless all are NA
# [rowSums() returns 0 instead of NA if all are NA and you use na.rm=TRUE]
# use it like this
# df$snu <- sum_of_row(df[c('new_sn', 'new_su')])
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sum_of_row <- function(x) {
  tosum <- as.matrix(x)
  summed <- rowMeans((tosum), na.rm=TRUE) * rowSums(!is.na((tosum)))
  return(summed)
}


# Convert a null (NA) to zero
NZ <- function(x){
  x <- ifelse(is.na(x),0,x)
  return(x)
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   mort_prev (Table 1) -----
#   Estimates of TB mortality and prevalence
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country estimates
mort_prev_c <- filter(e, year == notification_maxyear) %>%
                arrange(country) %>%
                select(country, year, e_pop_num,
                      e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_num_hi,
                      e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
                      e_mort_tbhiv_num, e_mort_tbhiv_num_lo, e_mort_tbhiv_num_hi,
                      e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi,
                      e_prev_num, e_prev_num_lo, e_prev_num_hi,
                      e_prev_100k, e_prev_100k_lo, e_prev_100k_hi) %>%
                rename(entity = country )

# Get regional estimates
mort_prev_region <- filter(a, year == notification_maxyear & group_type == "g_whoregion") %>%
                    arrange(group_name) %>%
                    select(group_description, year, e_pop_num,
                         e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_num_hi,
                         e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
                         e_mort_tbhiv_num, e_mort_tbhiv_num_lo, e_mort_tbhiv_num_hi,
                         e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi,
                         e_prev_num, e_prev_num_lo, e_prev_num_hi,
                         e_prev_100k, e_prev_100k_lo, e_prev_100k_hi) %>%
                    rename(entity = group_description )

# Got global estimates
mort_prev_global <- filter(a, year == notification_maxyear & group_type == "global") %>%
                    select(group_description, year, e_pop_num,
                           e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_num_hi,
                           e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
                           e_mort_tbhiv_num, e_mort_tbhiv_num_lo, e_mort_tbhiv_num_hi,
                           e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi,
                           e_prev_num, e_prev_num_lo, e_prev_num_hi,
                           e_prev_100k, e_prev_100k_lo, e_prev_100k_hi) %>%
                    rename(entity = group_description )

# Create combined table in order of countries then regional and global estimates
mort_prev <- combine_tables(mort_prev_c, mort_prev_region, mort_prev_global)
rm(list=c("mort_prev_c", "mort_prev_region", "mort_prev_global"))



# Format variables for output
mort_prev <- within(mort_prev, {

  # round population to millions
  e_pop_num <- ifelse(e_pop_num / 1000000 < 1, "< 1", rounder(e_pop_num / 1000000))

  # mortality excluding HIV (convert numbers to thousands)
  mort_num <- frmt(e_mort_exc_tbhiv_num / 1000, thou=TRUE)
  mort_num_lo_hi <- frmt_intervals(e_mort_exc_tbhiv_num / 1000,
                                 e_mort_exc_tbhiv_num_lo / 1000,
                                 e_mort_exc_tbhiv_num_hi / 1000, thou=TRUE)

  mort_rate <- frmt(e_mort_exc_tbhiv_100k, rates=TRUE)
  mort_rate_lo_hi <- frmt_intervals(e_mort_exc_tbhiv_100k,
                                e_mort_exc_tbhiv_100k_lo,
                                e_mort_exc_tbhiv_100k_hi, rates=TRUE)

  # mortality (HIV-positive TB) (convert numbers to thousands)
  mort_tbhiv_num <- frmt(e_mort_tbhiv_num / 1000, thou=TRUE, thouEst=TRUE)
  mort_tbhiv_num_lo_hi <- frmt_intervals(e_mort_tbhiv_num / 1000,
                                 e_mort_tbhiv_num_lo / 1000,
                                 e_mort_tbhiv_num_hi / 1000, thou=TRUE, thouEst=TRUE)

  mort_tbhiv_rate <- frmt(e_mort_tbhiv_100k, rates=TRUE)
  mort_tbhiv_rate_lo_hi <- frmt_intervals(e_mort_tbhiv_100k,
                                e_mort_tbhiv_100k_lo,
                                e_mort_tbhiv_100k_hi, rates=TRUE)

  # prevalence (convert numbers to thousands)
  prev_num <- frmt(e_prev_num / 1000, thou=TRUE)
  prev_num_lo_hi <- frmt_intervals(e_prev_num / 1000,
                                 e_prev_num_lo / 1000,
                                 e_prev_num_hi / 1000, thou=TRUE)

  prev_rate <- frmt(e_prev_100k, rates=TRUE)
  prev_rate_lo_hi <- frmt_intervals(e_prev_100k,
                                e_prev_100k_lo,
                                e_prev_100k_hi, rates=TRUE)

  # Add for blank columns
  blank <- ""
})


# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(mort_prev,
       select=c("entity", "e_pop_num", "blank", "blank",
                "mort_num", "mort_num_lo_hi", "mort_rate", "mort_rate_lo_hi", "blank",
                "mort_tbhiv_num", "mort_tbhiv_num_lo_hi", "mort_tbhiv_rate", "mort_tbhiv_rate_lo_hi", "blank",
                "prev_num", "prev_num_lo_hi", "prev_rate", "prev_rate_lo_hi")) %>%
  write.csv(file="mort_prev.csv", row.names=FALSE, na="")


# Don't leave any mess behind!
rm(mort_prev)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   inc_cdr (Table 2)  ----
#   Incidence, notification and case detection rates, all forms
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country estimates
inc_cdr_c <- filter(e, year == notification_maxyear) %>%
              arrange(country) %>%
              select(country, year, e_pop_num,
                     e_inc_num, e_inc_num_lo, e_inc_num_hi,
                     e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                     e_inc_tbhiv_num, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi,
                     e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
                     c_cdr, c_cdr_lo, c_cdr_hi) %>%
              rename(entity = country )

# get notifications and add them to the estimates
inc_cdr_c <- filter(n, year  == notification_maxyear) %>%
              select(country, c_newinc) %>%
              rename(entity = country ) %>%
              inner_join(inc_cdr_c, by = "entity") %>%
              arrange(entity)

# Get regional estimates (this view already includes c_newinc)
inc_cdr_region <- filter(a, year == notification_maxyear & group_type == "g_whoregion") %>%
                  arrange(group_name) %>%
                  select(group_description, year, e_pop_num,
                         e_inc_num, e_inc_num_lo, e_inc_num_hi,
                         e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                         e_inc_tbhiv_num, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi,
                         e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
                         c_cdr, c_cdr_lo, c_cdr_hi,
                         c_newinc) %>%
                  rename(entity = group_description )


# Got global estimates (this view already includes c_newinc)
inc_cdr_global <- filter(a, year == notification_maxyear & group_type == "global") %>%
                    select(group_description, year, e_pop_num,
                           e_inc_num, e_inc_num_lo, e_inc_num_hi,
                           e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                           e_inc_tbhiv_num, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi,
                           e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
                           c_cdr, c_cdr_lo, c_cdr_hi,
                           c_newinc) %>%
                    rename(entity = group_description )

# Create combined table in order of countries then regional and global estimates
inc_cdr <- combine_tables(inc_cdr_c, inc_cdr_region, inc_cdr_global)
rm(list=c("inc_cdr_c", "inc_cdr_region", "inc_cdr_global"))


# Format variables for output
inc_cdr <- within(inc_cdr, {

  # round population to millions
  e_pop_num <- ifelse(e_pop_num / 1000000 < 1, "< 1", rounder(e_pop_num / 1000000))

  # incidence (convert numbers to thousands)
  inc_num <- frmt(e_inc_num / 1000, thou=TRUE)
  inc_num_lo_hi <- frmt_intervals(e_inc_num / 1000,
                                 e_inc_num_lo / 1000,
                                 e_inc_num_hi / 1000, thou=TRUE)

  inc_rate <- frmt(e_inc_100k, rates=TRUE)
  inc_rate_lo_hi <- frmt_intervals(e_inc_100k,
                                e_inc_100k_lo,
                                e_inc_100k_hi, rates=TRUE)

  # TB/HIV incidence (convert numbers to thousands)
  inc_tbhiv_num <- frmt(e_inc_tbhiv_num / 1000, thou=TRUE, thouEst=TRUE)
  inc_tbhiv_num_lo_hi <- frmt_intervals(e_inc_tbhiv_num / 1000,
                                 e_inc_tbhiv_num_lo / 1000,
                                 e_inc_tbhiv_num_hi / 1000, thou=TRUE, thouEst=TRUE)

  inc_tbhiv_rate <- frmt(e_inc_tbhiv_100k, rates=TRUE)
  inc_tbhiv_rate_lo_hi <- frmt_intervals(e_inc_tbhiv_100k,
                                e_inc_tbhiv_100k_lo,
                                e_inc_tbhiv_100k_hi, rates=TRUE)

  # format c_newinc
  c_newinc <- rounder(c_newinc)

  # Case detection rate
  c_cdr <- frmt(c_cdr)
  c_cdr_lo_hi <- ifelse(is.na(c_cdr) | c_cdr==0,
                        NA,
                        paste0("(", frmt(c_cdr_lo), "–", frmt(c_cdr_hi), ")"))

  # Add for blank columns
  blank <- ""
})

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead


subset(inc_cdr,
       select=c("entity", "e_pop_num", "blank", "blank",
                "inc_num", "inc_num_lo_hi",
                "inc_rate", "inc_rate_lo_hi", "blank",
                "inc_tbhiv_num", "inc_tbhiv_num_lo_hi",
                "inc_tbhiv_rate", "inc_tbhiv_rate_lo_hi", "blank",
                "c_newinc", "blank",
                "c_cdr", "c_cdr_lo_hi")) %>%
  write.csv(file="inc_cdr.csv", row.names=FALSE, na="")


# Don't leave any mess behind!
rm(inc_cdr)


# ---------------------------------------------------------------------
stop("OK, see what we have!")
# ---------------------------------------------------------------------



