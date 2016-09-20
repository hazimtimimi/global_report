# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to produce annex 4 tables for the global TB report.
#
# Hazim Timimi
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
report_year <- 2016

# The following are convenience variables since notification and most other data sets will run up to the
# year before the reporting year and outcomes will run up to two years before the reporting year
notification_maxyear <- (report_year - 1)
outcome_maxyear      <- (report_year - 2)


# Apply the Russian fudge ------
# flag for whether to suppress calculation of %
# of notified TB patients who knew their HIV status (applies to table A4.4)

russianfudge <- TRUE

# Apply the Malawi fudge ------
# flag for whether to calculate % of patients who knew their HIV status using c_notified as the denominator

malawifudge <- TRUE

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
dir.create(file.path(annex4_folder, "linked_CSVs"), recursive = TRUE, showWarnings = FALSE)
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

# Calculate % using numerator and denominator, format the output and cap at 100%
cap_frmt_pct <- function(numerator, denominator) {

  pct <- ifelse(is.na(numerator) | NZ(denominator) == 0, "",
         ifelse((numerator * 100 / denominator) > 100, ">100", frmt(numerator * 100 / denominator)))

  return(pct)
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# stop("OK, see what we have!")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


stop("

     >>>>>>>>>>
     Stopping here so can do the rest manually!
     <<<<<<<<<<<<")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   inc_table (Table A4.1)  ----
#   Incidence of TB (all forms), TB/HIV and MDR/RR-TB, report_year - 1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country estimates
inc_country <-  estimates_epi %>%
                filter(year == notification_maxyear) %>%
                select(entity = country,
                       e_pop_num,
                       e_inc_num, e_inc_num_lo, e_inc_num_hi,
                       e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                       e_inc_tbhiv_num, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi,
                       e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi)

# Add DR-TB incidence estimates
inc_country <- estimates_drtb_rawvalues %>%
                filter(year  == notification_maxyear) %>%
                select(entity = country,
                       e_inc_rr_num,
                       e_inc_rr_num_lo,
                       e_inc_rr_num_hi) %>%
                inner_join(inc_country, by = "entity") %>%
                arrange(entity)

# Get regional estimates
inc_region <- aggregated_estimates_epi %>%
              filter(year == notification_maxyear & group_type == "g_whoregion") %>%
              select(entity = group_description,
                     group_name,
                     e_pop_num,
                     e_inc_num, e_inc_num_lo, e_inc_num_hi,
                     e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                     e_inc_tbhiv_num, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi,
                     e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi)

inc_region <- aggregated_estimates_drtb_rawvalues %>%
              select(group_name,
                     e_inc_rr_num,
                     e_inc_rr_num_lo,
                     e_inc_rr_num_hi) %>%
              inner_join(inc_region, by = "group_name") %>%
              arrange(group_name) %>%
              select(-group_name)



# Got global estimates
inc_global <- aggregated_estimates_epi %>%
              filter(year == notification_maxyear & group_type == "global") %>%
              select(entity = group_description,
                     group_name,
                     e_pop_num,
                     e_inc_num, e_inc_num_lo, e_inc_num_hi,
                     e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                     e_inc_tbhiv_num, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi,
                     e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi)

inc_global <- aggregated_estimates_drtb_rawvalues %>%
              select(group_name,
                     e_inc_rr_num,
                     e_inc_rr_num_lo,
                     e_inc_rr_num_hi) %>%
              inner_join(inc_global, by = "group_name") %>%
              select(-group_name)



# Create combined table in order of countries then regional and global estimates
inc_table <- combine_tables(inc_country, inc_region, inc_global)

# Format variables for output
inc_table <- within(inc_table, {

  # round population to millions
  pop_num <- ifelse(e_pop_num / 1000000 < 1, "< 1", rounder(e_pop_num / 1000000))

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

  # MDR/RR-TB incidence (convert numbers to thousands)
  inc_rr_num <- frmt(e_inc_rr_num / 1000, thou=TRUE, thouEst=TRUE)
  inc_rr_num_lo_hi <- frmt_intervals(e_inc_rr_num / 1000,
                                        e_inc_rr_num_lo / 1000,
                                        e_inc_rr_num_hi / 1000, thou=TRUE, thouEst=TRUE)


  # Need to calculate MDR/RR-TB incidence rates
  inc_rr_rate <- frmt( (e_inc_rr_num * 1e5 / e_pop_num), rates=TRUE)
  inc_rr_rate_lo_hi <- frmt_intervals((e_inc_rr_num * 1e5 / e_pop_num),
                                       (e_inc_rr_num_lo * 1e5 / e_pop_num),
                                       (e_inc_rr_num_hi * 1e5 / e_pop_num), rates=TRUE)

  # Add for blank columns
  blank <- ""
})

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead


subset(inc_table,
       select=c("entity", "pop_num", "blank", "blank",
                "inc_num", "inc_num_lo_hi",
                "inc_rate", "inc_rate_lo_hi", "blank",
                "inc_tbhiv_num", "inc_tbhiv_num_lo_hi",
                "inc_tbhiv_rate", "inc_tbhiv_rate_lo_hi", "blank",
                "inc_rr_num", "inc_rr_num_lo_hi",
                "inc_rr_rate", "inc_rr_rate_lo_hi"
                )) %>%
  write.csv(file="inc_table.csv", row.names=FALSE, na="")


# Clean up (remove any objects with their name starting with 'inc_')
rm(list=ls(pattern = "^inc_"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   mort_table (Table A4.2) -----
#   Estimates of TB mortality
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country estimates
mort_country <- estimates_epi %>%
                filter(year == notification_maxyear) %>%
                arrange(country) %>%
                select(entity = country,
                        e_pop_num,
                        e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_num_hi,
                        e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
                        e_mort_tbhiv_num, e_mort_tbhiv_num_lo, e_mort_tbhiv_num_hi,
                        e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi,
                        e_mort_num, e_mort_num_lo, e_mort_num_hi,
                        e_mort_100k, e_mort_100k_lo, e_mort_100k_hi)

# Get regional estimates
mort_region <-  aggregated_estimates_epi %>%
                filter(year == notification_maxyear & group_type == "g_whoregion") %>%
                arrange(group_name) %>%
                select(entity = group_description,
                        e_pop_num,
                        e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_num_hi,
                        e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
                        e_mort_tbhiv_num, e_mort_tbhiv_num_lo, e_mort_tbhiv_num_hi,
                        e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi,
                        e_mort_num, e_mort_num_lo, e_mort_num_hi,
                        e_mort_100k, e_mort_100k_lo, e_mort_100k_hi)

# Got global estimates
mort_global <-  aggregated_estimates_epi %>%
                filter(year == notification_maxyear & group_type == "global") %>%
                select(entity = group_description,
                       e_pop_num,
                       e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_num_hi,
                       e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
                       e_mort_tbhiv_num, e_mort_tbhiv_num_lo, e_mort_tbhiv_num_hi,
                       e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi,
                       e_mort_num, e_mort_num_lo, e_mort_num_hi,
                       e_mort_100k, e_mort_100k_lo, e_mort_100k_hi)

# Create combined table in order of countries then regional and global estimates
mort_table <- combine_tables(mort_country, mort_region, mort_global)

# Format variables for output
mort_table <- within(mort_table, {

  # round population to millions
  e_pop_num <- ifelse(e_pop_num / 1000000 < 1, "< 1", rounder(e_pop_num / 1000000))

  # mortality (HIV-negative TB) (convert numbers to thousands)
  mort_exc_tbhiv_num <- frmt(e_mort_exc_tbhiv_num / 1000, thou=TRUE)
  mort_exc_tbhiv_num_lo_hi <- frmt_intervals(e_mort_exc_tbhiv_num / 1000,
                                 e_mort_exc_tbhiv_num_lo / 1000,
                                 e_mort_exc_tbhiv_num_hi / 1000, thou=TRUE)

  mort_exc_tbhiv_rate <- frmt(e_mort_exc_tbhiv_100k, rates=TRUE)
  mort_exc_tbhiv_rate_lo_hi <- frmt_intervals(e_mort_exc_tbhiv_100k,
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

  # Mortality (HIV-negative and HIV-positive TB cases) (convert numbers to thousands)
  mort_num <- frmt(e_mort_num / 1000, thou=TRUE)
  mort_num_lo_hi <- frmt_intervals(e_mort_num / 1000,
                                 e_mort_num_lo / 1000,
                                 e_mort_num_hi / 1000, thou=TRUE)

  mort_rate <- frmt(e_mort_100k, rates=TRUE)
  mort_rate_lo_hi <- frmt_intervals(e_mort_100k,
                                e_mort_100k_lo,
                                e_mort_100k_hi, rates=TRUE)

  # Add for blank columns
  blank <- ""
})


# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(mort_table,
       select=c("entity", "e_pop_num", "blank", "blank",
                "mort_exc_tbhiv_num", "mort_exc_tbhiv_num_lo_hi",
                "mort_exc_tbhiv_rate", "mort_exc_tbhiv_rate_lo_hi", "blank",
                "mort_tbhiv_num", "mort_tbhiv_num_lo_hi",
                "mort_tbhiv_rate", "mort_tbhiv_rate_lo_hi", "blank",
                "mort_num", "mort_num_lo_hi", "mort_rate", "mort_rate_lo_hi")) %>%
  write.csv(file="mort_table.csv", row.names=FALSE, na="")


# Don't leave any mess behind!
# Clean up (remove any objects with their name starting with 'mort_')
rm(list=ls(pattern = "^mort_"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# mdr_rr_measured (Table A4.3) -----
# Measured percentage of TB cases with MDR/RR-TB
# Shows source of DR-TB measurements for those countries with usable survey or surveillance data
# No aggregates used
# (Was table A4.6 in 2015 report)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data

mdr_measured <- estimates_drtb %>%
                filter(year == notification_maxyear) %>%
                select(country,
                       source_new,
                       source_drs_year_new,
                       source_drs_coverage_new,
                       e_rr_pct_new, e_rr_pct_new_lo, e_rr_pct_new_hi,
                       source_ret,
                       source_drs_year_ret,
                       source_drs_coverage_ret,
                       e_rr_pct_ret, e_rr_pct_ret_lo, e_rr_pct_ret_hi)  %>%
                arrange(country)


# blank out source, estimates etc if source is not survey or surveillance
mdr_measured[mdr_measured$source_new == "Model",
             c("source_new", "source_drs_year_new", "source_drs_coverage_new",
               "e_rr_pct_new", "e_rr_pct_new_lo", "e_rr_pct_new_hi")] <- NA

mdr_measured[mdr_measured$source_ret == "Model",
             c("source_ret", "source_drs_year_ret", "source_drs_coverage_ret",
               "e_rr_pct_ret", "e_rr_pct_ret_lo", "e_rr_pct_ret_hi")] <- NA

# Format variables for output
mdr_measured <- within(mdr_measured, {

  # concatenate confidence interval variables into bracketed strings
  e_rr_pct_new_lo_hi <- frmt_intervals(e_rr_pct_new,
                                       e_rr_pct_new_lo,
                                       e_rr_pct_new_hi, rates=TRUE)

  e_rr_pct_ret_lo_hi <- frmt_intervals(e_rr_pct_ret,
                                       e_rr_pct_ret_lo,
                                       e_rr_pct_ret_hi, rates=TRUE)

  # Add for blank columns
  blank <- ""

})


# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(mdr_measured,
       select=c("country", "blank",
                "source_drs_year_new", "blank", "source_new", "blank", "source_drs_coverage_new", "blank",
                "e_rr_pct_new", "blank", "e_rr_pct_new_lo_hi", "blank",
                "source_drs_year_ret", "blank", "source_ret", "blank", "source_drs_coverage_ret", "blank",
                "e_rr_pct_ret", "blank", "e_rr_pct_ret_lo_hi")) %>%
  write.csv(file="mdr_measured.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(mdr_measured)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   notif_table (Table A4.4) ----
#   Case notifications (Was table A4.3 in 2015 report)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
notif_country <-  notification %>%
                  filter(year == notification_maxyear) %>%
                  select(entity = country,
                         g_whoregion,
                         c_notified,
                         c_newinc,
                         rdx_data_available,
                         newinc_rdx,
                         rdxsurvey_newinc_rdx,
                         rdxsurvey_newinc,
                         newrel_hivtest,
                         new_labconf,
                         new_clindx,
                         ret_rel_labconf,
                         ret_rel_clindx) %>%

                  # calculate % with rapid daignostics (only done at country level)
                  mutate(pct_rdx = ifelse(c_newinc > 0,
                                          ifelse(rdx_data_available == 60,
                                                rounder(newinc_rdx * 100 / c_newinc),
                                                ifelse(rdx_data_available == 61,
                                                       rounder(rdxsurvey_newinc_rdx * 100 / rdxsurvey_newinc),
                                                       NA)),
                                          NA)) %>%

                  # calculate % with known HIV status (use different variables for aggregates)
                  mutate(pct_hivtest = ifelse(c_newinc > 0,
                                              rounder(newrel_hivtest * 100 / c_newinc),
                                              NA))

# TEMPORARY POLITICAL SOLUTION FOR RUSSIAN FEDERATION 2010 onwards:
# DO NOT CALCULATE % tb PATIENTS WITH KNOWN HIV STATUS
# Enable or disable using flag in section A right at the top of the script.

if (isTRUE(russianfudge)) {
  notif_country$pct_hivtest <- ifelse(notif_country$entity == "Russian Federation",
                                      NA,
                                      notif_country$pct_hivtest)
}

# TEMPORARY POLITICAL SOLUTION FOR Malawi 2016:
# Use c_notified instead of c_newin to calculate % with known HIV status# Enable or disable using flag in section A right at the top of the script.

if (isTRUE(malawifudge)) {
  notif_country$pct_hivtest <- ifelse(notif_country$entity == "Malawi",
                                      rounder(notif_country$newrel_hivtest * 100 / notif_country$c_notified),
                                      notif_country$pct_hivtest)
}

# calculate sum of pulmonary among new and relapse, and sum pulmonary bacteriologically confirmed
notif_country$pulmonary <- notif_country %>%
                            select(new_labconf, new_clindx, ret_rel_labconf, ret_rel_clindx) %>%
                            sum_of_row()

notif_country$pulmonary_bact_conf <- notif_country %>%
                                    select(new_labconf, ret_rel_labconf) %>%
                                    sum_of_row()

# drop unneeded fields now that we have calculated stuff
notif_country <- notif_country %>%
                  select(entity,
                         g_whoregion,
                         c_notified,
                         c_newinc,
                         pct_rdx,
                         pct_hivtest,
                         pulmonary,
                         pulmonary_bact_conf) %>%

                  # sort by country name
                  arrange(entity)




# Calculate regional aggregates
notif_region <- notif_country %>%
                group_by(g_whoregion) %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               c_notified,
                               c_newinc,
                               pulmonary,
                               pulmonary_bact_conf) %>%
                # get rid of pesky grouping
                ungroup()

# Get tb/hiv numerator and denominator (for funny rules...)
notif_region <-  TBHIV_for_aggregates %>%
                  filter(year == notification_maxyear) %>%
                  group_by(g_whoregion) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 hivtest_pct_numerator,
                                 hivtest_pct_denominator) %>%
                  # get rid of pesky grouping
                  ungroup() %>%
                  inner_join(notif_region, by = "g_whoregion")

# merge with regional names
notif_region <- aggregated_estimates_epi %>%
                filter(year == notification_maxyear & group_type == "g_whoregion") %>%
                select(g_whoregion = group_name,
                       entity = group_description) %>%
                inner_join(notif_region, by = "g_whoregion") %>%
                arrange(g_whoregion) %>%

                # Add/remove extra variables to match structure of the country data frame
                mutate(pct_rdx = NA,

                       # calculate % with known HIV status
                       pct_hivtest = rounder(hivtest_pct_numerator * 100 / hivtest_pct_denominator)) %>%

                select(-g_whoregion,
                       -hivtest_pct_numerator,
                       -hivtest_pct_denominator)

# Calculate global aggregate
notif_global <- notif_country %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               c_notified,
                               c_newinc,
                               pulmonary,
                               pulmonary_bact_conf) %>%
                mutate(entity = "Global")

# Get tb/hiv numerator and denominator (for funny rules...)
notif_global <-  TBHIV_for_aggregates %>%
                  filter(year == notification_maxyear) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 hivtest_pct_numerator,
                                 hivtest_pct_denominator)  %>%
                  mutate(entity = "Global")%>%
                  inner_join(notif_global, by = "entity") %>%

                  # Add/remove extra variables to match structure of the country data frame
                  mutate(pct_rdx = NA,

                         # calculate % with known HIV status
                         pct_hivtest = rounder(hivtest_pct_numerator * 100 / hivtest_pct_denominator)) %>%

                  select(-hivtest_pct_numerator,
                         -hivtest_pct_denominator)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Handle Kosovo
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create a Kosovo only dataset
notif_kosovo <- notification_exceptions %>%
                filter(year == notification_maxyear) %>%
                select(entity = country,
                       new_labconf, new_clindx, new_ep,
                       ret_rel_labconf, ret_rel_clindx, ret_rel_ep, ret_nrel)


# calculate c_newinc and c_notified
notif_kosovo$c_newinc <- notif_kosovo %>%
                          select(new_labconf, new_clindx, new_ep,
                                ret_rel_labconf, ret_rel_clindx, ret_rel_ep) %>%
                          sum_of_row()

notif_kosovo$c_notified <- notif_kosovo %>%
                            select(new_labconf, new_clindx, new_ep,
                                  ret_rel_labconf, ret_rel_clindx, ret_rel_ep, ret_nrel) %>%
                            sum_of_row()

# Add/remove variables so as to match the country table
notif_kosovo <- notif_kosovo %>%
                select(entity,
                       c_notified,
                       c_newinc) %>%
                mutate(g_whoregion = "EUR",
                       pct_rdx = NA,
                       pct_hivtest = NA,
                       pulmonary = NA,
                       pulmonary_bact_conf = NA)

# Create a Serbia (without Kosovo) dataset
notif_serbia <- filter(notif_country, entity=="Serbia")
notif_serbia_minus_kosovo <- notif_kosovo

notif_vars <- c("c_newinc", "c_notified")

notif_serbia_minus_kosovo[notif_vars] <- notif_serbia[notif_vars] - notif_kosovo[notif_vars]
notif_serbia_minus_kosovo$entity <- "Serbia (without Kosovo)"


# Insert the two additional Kosovo/Serbia w/o Kosovo tables in appropriate spot.
insertpoint <- which(notif_country$entity == "Serbia" )
endpoint <- nrow(notif_country)

notif_country <- rbind(notif_country[1:insertpoint, ],
                       notif_serbia_minus_kosovo,
                       notif_kosovo,
                       notif_country[(insertpoint+1):endpoint, ] )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End of handling Kosovo section
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create combined table in order of countries then regional and global estimates

notif_country <- notif_country %>%
                  #drop g_whoregion so as to match aggregates
                  select(-g_whoregion)

notif_table <- combine_tables(notif_country, notif_region, notif_global)

# Calculate % pulmonary in new and relapse, and % bact confirmed among pulmonary (the same for all records)

notif_table <- notif_table %>%
                mutate(pct_pulm = ifelse(c_newinc > 0,
                                         rounder(pulmonary * 100 / c_newinc),
                                         NA),
                       pct_pulm_bact_conf = ifelse(pulmonary > 0,
                                                   rounder(pulmonary_bact_conf * 100 / pulmonary),
                                                   NA),

                       # and format abosulte numbers for publication
                       c_notified = rounder(c_notified),
                       c_newinc = rounder(c_newinc),

                       # and variable for blank columns
                       blank = "")


# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead


subset(notif_table,
       select=c("entity", "blank",
                "c_notified", "blank",
                "c_newinc", "blank",
                "pct_rdx", "pct_hivtest", "pct_pulm", "pct_pulm_bact_conf")) %>%
        write.csv(file="notif_table.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
# Clean up (remove any objects with their name starting with 'notif_')
rm(list=ls(pattern = "^notif_"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   agesex  (Table A4.4) -----
#   New and relapse case notification rates per 100 000 population by age and sex
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
# A. Males
agesex_country_m <- filter(n, year == notification_maxyear) %>%
                    select(country, g_whoregion,
                           rel_in_agesex_flg, starts_with("newrel_m")) %>%
                    rename(a04 = newrel_m04,
                           a514 = newrel_m514,
                           a014 = newrel_m014,
                           a1524 = newrel_m1524,
                           a2534 = newrel_m2534,
                           a3544 = newrel_m3544,
                           a4554 = newrel_m4554,
                           a5564 = newrel_m5564,
                           a65 = newrel_m65,
                           ageunk = newrel_mu,
                           a15plus = newrel_m15plus) %>%
                    mutate(sex = "Male")

# Merge with population denominators
agesex_country_m <- filter(p, year == notification_maxyear) %>%
                    select(country,starts_with("e_pop_m")) %>%
                    # Drop the 15plus field
                    select(-e_pop_m15plus) %>%
                    inner_join(agesex_country_m, by = "country") %>%
                    rename(p04 = e_pop_m04,
                           p514 = e_pop_m514,
                           p014 = e_pop_m014,
                           p1524 = e_pop_m1524,
                           p2534 = e_pop_m2534,
                           p3544 = e_pop_m3544,
                           p4554 = e_pop_m4554,
                           p5564 = e_pop_m5564,
                           p65 = e_pop_m65)


# B. Females
agesex_country_f <- filter(n, year == notification_maxyear) %>%
                    select(country, g_whoregion,
                           rel_in_agesex_flg, starts_with("newrel_f")) %>%
                    rename(a04 = newrel_f04,
                           a514 = newrel_f514,
                           a014 = newrel_f014,
                           a1524 = newrel_f1524,
                           a2534 = newrel_f2534,
                           a3544 = newrel_f3544,
                           a4554 = newrel_f4554,
                           a5564 = newrel_f5564,
                           a65 = newrel_f65,
                           ageunk = newrel_fu,
                           a15plus = newrel_f15plus) %>%
                    mutate(sex = "Female")

# Merge with population denominators
agesex_country_f <- filter(p, year == notification_maxyear) %>%
                    select(country,starts_with("e_pop_f")) %>%
                    # Drop the 15plus field
                    select(-e_pop_f15plus) %>%
                    inner_join(agesex_country_f, by = "country") %>%
                    rename(p04 = e_pop_f04,
                           p514 = e_pop_f514,
                           p014 = e_pop_f014,
                           p1524 = e_pop_f1524,
                           p2534 = e_pop_f2534,
                           p3544 = e_pop_f3544,
                           p4554 = e_pop_f4554,
                           p5564 = e_pop_f5564,
                           p65 = e_pop_f65)

# C. Age unknown
agesex_country_sexunk <- filter(n, year == notification_maxyear) %>%
                          select(country, g_whoregion,
                                 rel_in_agesex_flg, starts_with("newrel_sexunk"))

# Because very few countries report data with sex unknown, filter out the empty records
# (There are 5 newrel_sexunk* variables so empty records will have 5 NAs)
# rowSums(is.na(agesex_country_sexunk)) < 5
# Also filter out rows where the sum of all 5 newrel_sexunk* variables is zero

agesex_country_sexunk <- filter(agesex_country_sexunk,
                                NZ(sum_of_row(as.matrix(newrel_sexunk04, newrel_sexunk514, newrel_sexunk014,
                                                       newrel_sexunkageunk, newrel_sexunk15plus))) > 0 ) %>%
                          rename(a04 = newrel_sexunk04,
                                 a514 = newrel_sexunk514,
                                 a014 = newrel_sexunk014,
                                 ageunk = newrel_sexunkageunk,
                                 a15plus = newrel_sexunk15plus) %>%
                          # Add dummy variables to match structure of male and female tables,
                          # (note that we won't use newrel_sexunk04 and newrel_sexunk514 because cannot convert them to rates)
                          mutate(sex = "Unknown",
                                 a1524 = NA,
                                 a2534 = NA,
                                 a3544 = NA,
                                 a4554 = NA,
                                 a5564 = NA,
                                 a65 = NA,
                                 p04 = NA,
                                 p514 = NA,
                                 p014 = NA,
                                 p1524 = NA,
                                 p2534 = NA,
                                 p3544 = NA,
                                 p4554 = NA,
                                 p5564 = NA,
                                 p65 = NA)

# Combine the country records into one
agesex_country <-  rbind(agesex_country_m, agesex_country_f, agesex_country_sexunk) %>%
                    rename( entity = country) %>%
                    arrange(entity, sex)
rm(list=c("agesex_country_m", "agesex_country_f", "agesex_country_sexunk"))

# Calculate regional aggregates
# A. Get totals of cases by under and over 15 categories
agesex_region <- agesex_country %>%
                  group_by(g_whoregion, sex) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 a014, a15plus, ageunk)

# B. Get totals for detailed age categories and populations only for countries that have reported data
# for all of those age categories
agesex_region <- filter(agesex_country,
                        !is.na(a04) & !is.na(a514) & !is.na(a1524) & !is.na(a2534) &
                          !is.na(a3544) & !is.na(a4554) & !is.na(a5564) & !is.na(a65)) %>%
                  group_by(g_whoregion, sex) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 a04, a514, a1524, a2534,
                                   a3544, a4554, a5564, a65, starts_with("p")) %>%
                  # Merge with other totals
                  right_join(agesex_region, by = c("g_whoregion", "sex")) %>%
                  # Add dummy variable to match structure of country table
                  mutate(rel_in_agesex_flg = NA)


# C. Merge with regional names
agesex_region <- filter(a, year == notification_maxyear & group_type == "g_whoregion") %>%
                  select(group_name, group_description) %>%
                  rename(g_whoregion = group_name) %>%
                  inner_join(agesex_region, by = "g_whoregion") %>%
                  rename(entity = group_description) %>%
                  arrange(g_whoregion)


# Calculate global aggregate
# A. Get totals of cases by under and over 15 categories
agesex_global <- agesex_country %>%
                  group_by(sex) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 a014, a15plus, ageunk)

# B. Get totals for detailed age categories and populations only for countries that have reported data
# for all of those age categories
agesex_global <- filter(agesex_country,
                        !is.na(a04) & !is.na(a514) & !is.na(a1524) & !is.na(a2534) &
                          !is.na(a3544) & !is.na(a4554) & !is.na(a5564) & !is.na(a65)) %>%
                  group_by(sex) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 a04, a514, a1524, a2534,
                                 a3544, a4554, a5564, a65, starts_with("p")) %>%
                  # Merge with other totals
                  right_join(agesex_global, by = "sex") %>%
                  mutate(entity = "Global",
                         # Add dummy variables to match structure of the other two tables
                         rel_in_agesex_flg = NA,
                         g_whoregion = "" )

# Create combined table in order of countries then regional and global estimates
agesex <- combine_tables(agesex_country, agesex_region, agesex_global)
rm(list=c("agesex_country", "agesex_region", "agesex_global"))

# Convert zeros to NA in the detailed age ranges for sex unknown rows because they are not collected at country level
# (Should already be NA at country level, but not for aggregates)
agesex[!is.na(agesex$sex) &  agesex$sex == "Unknown",
       c("a1524", "a2534", "a3544", "a4554", "a5564", "a65")] <- NA

# Calculate and format variables for output
agesex <- within(agesex, {

  # Calculate notification rates per 100 000 population for the detailed age ranges
  a04 <- ifelse(NZ(p04)==0, NA, frmt(a04 * 100000 / p04, rates=TRUE))
  a514 <- ifelse(NZ(p514)==0, NA, frmt(a514 * 100000 / p514, rates=TRUE))
  a1524 <- ifelse(NZ(p1524)==0, NA, frmt(a1524 * 100000 / p1524, rates=TRUE))
  a2534 <- ifelse(NZ(p2534)==0, NA, frmt(a2534 * 100000 / p2534, rates=TRUE))
  a3544 <- ifelse(NZ(p3544)==0, NA, frmt(a3544 * 100000 / p3544, rates=TRUE))
  a4554 <- ifelse(NZ(p4554)==0, NA, frmt(a4554 * 100000 / p4554, rates=TRUE))
  a5564 <- ifelse(NZ(p5564)==0, NA, frmt(a5564 * 100000 / p5564, rates=TRUE))
  a65 <- ifelse(NZ(p65)==0, NA, frmt(a65 * 100000 / p65, rates=TRUE))

  # Format the crude age ranges for numerical output (not rates)
  a014 <- rounder(a014)
  a15plus <- rounder(a15plus)
  ageunk <- rounder(ageunk)

  # Keep entity only if sex is female or entry is "WHO Regions"
  entity <- ifelse(entity == "WHO regions" | sex == "Female", entity, NA )

  # Flag country name if relapses were not included in age/sex table
  entity <- ifelse(!is.na(rel_in_agesex_flg) & rel_in_agesex_flg==0 & sex == "Female" , paste0(entity,"*"),entity)

  # Add for blank columns
  blank <- ""

})

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(agesex,
       select=c("entity", "blank",
                "sex", "blank",
                "a014", "blank", "a15plus", "blank", "ageunk", "blank",
                "a04", "blank", "a514", "blank",
                "a1524", "blank", "a2534", "blank", "a3544", "blank", "a4554", "blank", "a5564", "blank", "a65")) %>%
  write.csv(file="agesex.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(agesex)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   outcome (Table A4.5) ----
#   Treatment outcomes, all types
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
# A. Standard chorts
outcome_country <- filter(o, year == outcome_maxyear) %>%
                    select(country, g_whoregion,
                           rel_with_new_flg,
                           newrel_coh, newrel_succ, newrel_fail,
                           newrel_died, newrel_lost, c_newrel_neval,
                           ret_nrel_coh, ret_nrel_succ,
                           tbhiv_coh, tbhiv_succ)

# B. Combine with MDR cohorts one year older than standard
outcome_country <- filter(o, year == (outcome_maxyear - 1)) %>%
                    select(country,
                           mdr_coh, mdr_succ) %>%
                    inner_join(outcome_country, by = "country") %>%
                    rename(entity = country ) %>%
                    arrange(entity)

# Calculate regional aggregates

outcome_region <- outcome_country %>%
                    group_by(g_whoregion) %>%
                    summarise_each(funs(sum(., na.rm = TRUE)),
                                   contains("newrel_"),
                                   starts_with("ret_nrel_"),
                                   starts_with("tbhiv_"),
                                   starts_with("mdr_")) %>%
                    mutate(rel_with_new_flg = NA) # dummy variable to match structure of country table

# merge with regional names
outcome_region <- filter(a, year == notification_maxyear & group_type == "g_whoregion") %>%
                    select(group_name, group_description) %>%
                    rename(g_whoregion = group_name) %>%
                    inner_join(outcome_region, by = "g_whoregion") %>%
                    rename(entity = group_description) %>%
                    arrange(g_whoregion)


# Calculate global aggregate
outcome_global <- outcome_country %>%
                    summarise_each(funs(sum(., na.rm = TRUE)),
                                   contains("newrel_"),
                                   starts_with("ret_nrel_"),
                                   starts_with("tbhiv_"),
                                   starts_with("mdr_")) %>%
                    mutate(entity = "Global") %>%
                    mutate(rel_with_new_flg = NA) %>% # dummy variable to match structure of the other two tables
                    mutate(g_whoregion = "")  # dummy variable to match structure of the other two tables



# Create combined table in order of countries then regional and global estimates
outcome <- combine_tables(outcome_country, outcome_region, outcome_global)
rm(list=c("outcome_country", "outcome_region", "outcome_global"))


# Calculate treatment success rates and format variables for output
outcome <- within(outcome, {

  # New or new+relapse
  c_newrel_tsr <- ifelse( is.na(newrel_coh), NA, rounder( newrel_succ * 100 /newrel_coh ))

  # % of other outcomes for new+relapse
  c_newrel_failr <- ifelse( is.na(newrel_coh), NA, rounder( newrel_fail * 100 /newrel_coh ))
  c_newrel_diedr <- ifelse( is.na(newrel_coh), NA, rounder( newrel_died * 100 /newrel_coh ))
  c_newrel_lostr <- ifelse( is.na(newrel_coh), NA, rounder( newrel_lost * 100 /newrel_coh ))
  c_newrel_nevalr <- ifelse( is.na(newrel_coh), NA, rounder( c_newrel_neval * 100 /newrel_coh ))

  # Retreatment or retreatment excluding relapse
  c_ret_tsr <- ifelse( is.na(ret_nrel_coh), NA, rounder( ret_nrel_succ * 100 / ret_nrel_coh ))

  # HIV-positive, all cases
  c_tbhiv_tsr <- ifelse( is.na(tbhiv_coh), NA, rounder( tbhiv_succ * 100 / tbhiv_coh ))

  # MDR
  c_mdr_tsr <- ifelse( is.na(mdr_coh), NA, rounder( mdr_succ * 100 / mdr_coh))

  # Format the cohort sizes
  newrel_coh <- rounder(newrel_coh)
  ret_nrel_coh <- rounder(ret_nrel_coh)
  tbhiv_coh <- rounder(tbhiv_coh)
  mdr_coh <- rounder(mdr_coh)

  # Flag country name if relapses were not included with new cases
  entity <- ifelse(!is.na(rel_with_new_flg) & rel_with_new_flg==0, paste0(entity,"*"),entity)

  # Add for blank columns
  blank <- ""

})

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(outcome,
       select = c("entity",
                  "newrel_coh", "blank", "c_newrel_tsr", "blank",
                  "c_newrel_failr", "blank", "c_newrel_diedr", "blank",  "c_newrel_lostr", "blank", "c_newrel_nevalr", "blank",
                  "ret_nrel_coh", "blank", "c_ret_tsr", "blank",
                  "tbhiv_coh", "blank", "c_tbhiv_tsr", "blank",
                  "mdr_coh", "blank", "c_mdr_tsr", "blank"))  %>%
  write.csv(file="outcome.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(outcome)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# dst_rrmdr (Table A4.7) -----
# Drug susceptibility testing, estimated MDR-TB among notified TB cases, RR-/MDR-TB cases detected
# and enrolments on MDR-TB treatment
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
# A. Notifications
dst_rrmdr_country <- filter(n, year == notification_maxyear) %>%
                      select(country, g_whoregion,
                             c_rrmdr, unconf_rrmdr_tx, conf_rrmdr_tx,
                             rdst_new, rdst_ret,
                             new_labconf, c_ret)

# B. Combine with routine surveillance
dst_rrmdr_country <- filter(d, year == notification_maxyear) %>%
                      select(country,
                             dst_rlt_new, xpert_new,
                             dst_rlt_ret, xpert_ret) %>%
                      inner_join(dst_rrmdr_country, by = "country")

# C. Combine with estimates among notified
dst_rrmdr_country <- filter(emdrn, year == notification_maxyear) %>%
                      select(country,
                             e_mdr_num, e_mdr_num_lo, e_mdr_num_hi) %>%
                      inner_join(dst_rrmdr_country, by = "country") %>%
                      rename(entity = country ) %>%
                      arrange(entity)

# Calculate total number tested and total number started on treatment
dst_rrmdr_country <- within(dst_rrmdr_country, {

  # New cases tested for RR/MDR, including molecular diagnostics
  dst_new <- ifelse(is.na(rdst_new) & is.na(dst_rlt_new) & is.na(xpert_new),
                    NA,
                    ifelse(NZ(rdst_new) > NZ(sum_of_row(dst_rrmdr_country[c("dst_rlt_new","xpert_new")])),
                           rdst_new,
                           sum_of_row(dst_rrmdr_country[c("dst_rlt_new","xpert_new")]))
  )

  # previously treated cases tested for RR/MDR, including molecular diagnostics
  dst_ret <- ifelse(is.na(rdst_ret) & is.na(dst_rlt_ret) & is.na(xpert_ret),
                    NA,
                    ifelse(NZ(rdst_ret) > NZ(sum_of_row(dst_rrmdr_country[c("dst_rlt_ret","xpert_ret")])),
                           rdst_ret,
                           sum_of_row(dst_rrmdr_country[c("dst_rlt_ret","xpert_ret")]))
  )

  # Number started on treatment (confirmed or unconfirmed)
  c_rrmdr_tx <- sum_of_row(dst_rrmdr_country[c("unconf_rrmdr_tx", "conf_rrmdr_tx")])
})

# Drop the variables used to derive dst_new, dst_ret and c_rrmdr_tx
dst_rrmdr_country <- select(dst_rrmdr_country,
                        -rdst_new, -dst_rlt_new, -xpert_new,
                        -rdst_ret, -dst_rlt_ret, -xpert_ret,
                        -unconf_rrmdr_tx, -conf_rrmdr_tx)


# Calculate the regional aggregates
# Note: Cannot do a simple aggregate for estimates, instead have to
#       take results of Babis's modelling from emdra data frame

# Aggregate the notifications and dst results (from dst_rrmdr_country)

dst_rrmdr_region <- dst_rrmdr_country %>%
                      group_by(g_whoregion) %>%
                      summarise_each(funs(sum(., na.rm = TRUE)),
                                     c_rrmdr,
                                     new_labconf, c_ret,
                                     dst_new, dst_ret,
                                     c_rrmdr_tx)

# Merge with regional estimates of mdr among notified
dst_rrmdr_region <- filter(emdra, year == notification_maxyear & group_type == "g_whoregion") %>%
                      select(group_name, group_description,
                             e_mdr_num, e_mdr_num_lo, e_mdr_num_hi) %>%
                      rename(g_whoregion = group_name) %>%
                      inner_join(dst_rrmdr_region, by = "g_whoregion") %>%
                      rename(entity = group_description) %>%
                      arrange(g_whoregion)


# Calculate the global aggregates using same logic as for regional aggregates
dst_rrmdr_global <- dst_rrmdr_country %>%
                      summarise_each(funs(sum(., na.rm = TRUE)),
                                     c_rrmdr,
                                     new_labconf, c_ret,
                                     dst_new, dst_ret,
                                     c_rrmdr_tx) %>%
                      mutate(entity = "Global Aggregate")

# Merge with global estimates of mdr among notified
dst_rrmdr_global <- filter(emdra, year == notification_maxyear & group_type == "global") %>%
                      select(group_name, group_description,
                             e_mdr_num, e_mdr_num_lo, e_mdr_num_hi) %>%
                      rename(entity = group_description) %>%
                      inner_join(dst_rrmdr_global, by = "entity") %>%
                      # change field name to match other tables
                      rename(g_whoregion = group_name)


# Create combined table in order of countries then regional and global estimates
dst_rrmdr <- combine_tables(dst_rrmdr_country, dst_rrmdr_region, dst_rrmdr_global)
rm(list=c("dst_rrmdr_country", "dst_rrmdr_region", "dst_rrmdr_global"))



# Calculate testing percentages, enrolment percentages and format everything for output

dst_rrmdr <- within(dst_rrmdr, {

  # % of new pulmonary lab-confirmed cases tested
  dst_new_pct <-  cap_frmt_pct(dst_new, new_labconf)

  # % of previously treated cases tested
  dst_ret_pct <- cap_frmt_pct(dst_ret, c_ret)

  # RR/MDR detected as % of total estimated MDR among notified (note that denominator is MDR only!)
  rrmdr_pct <- cap_frmt_pct(c_rrmdr, e_mdr_num)

  # number enrolled on MDR-TB treatment as % of number of RR-/MDR-TB cases detected
  rrmdr_tx_pct <- cap_frmt_pct(c_rrmdr_tx, c_rrmdr)

  # format numbers
  c_rrmdr <- rounder(c_rrmdr)
  dst_new <- rounder(dst_new)
  dst_ret <- rounder(dst_ret)
  c_rrmdr_tx <- rounder(c_rrmdr_tx)

  # format the estimates
  e_mdr_num_lo_hi <- frmt_intervals(e_mdr_num,
                                    e_mdr_num_lo,
                                    e_mdr_num_hi, thou=TRUE)
  e_mdr_num <- frmt(e_mdr_num)


  # Add for blank columns
  blank <- ""

})


# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(dst_rrmdr,
       select=c("entity", "blank",
                "dst_new", "blank", "dst_new_pct", "blank",
                "dst_ret", "blank", "dst_ret_pct", "blank",
                "e_mdr_num", "e_mdr_num_lo_hi", "blank",
                "c_rrmdr", "blank","rrmdr_pct", "blank",
                "c_rrmdr_tx", "blank", "rrmdr_tx_pct")) %>%
  write.csv(file="dst_rrmdr.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(dst_rrmdr)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# tb_hiv (Table A4.8)  -----
# HIV testing for TB patients and provision of CPT, ART and IPT
# Note that the tbhiv dataset already has rules built in for combining _p and _f
#  (final and provisional) numbers, plus adjusted variables for calculating aggregates.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# A. Get adjusted tbhiv data
tb_hiv_country <- filter(tbhiv, year == notification_maxyear)

# B. Combine with IPT from notifications
tb_hiv_country <- filter(n, year == notification_maxyear) %>%
                  select(iso2, hiv_ipt) %>%
                  inner_join(tb_hiv_country, by = "iso2") %>%
                  rename(entity = country ) %>%
                  arrange(entity)


# Calculate regional aggregates, using the _pct_numerator and _pct_denominator fields
tb_hiv_region <- tb_hiv_country %>%
                  group_by(g_whoregion) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 c_notified, hiv_ipt,
                                 matches("_pct_"))

# merge with regional names
tb_hiv_region <- filter(a, year == notification_maxyear & group_type == "g_whoregion") %>%
                  select(group_name, group_description) %>%
                  rename(g_whoregion = group_name) %>%
                  inner_join(tb_hiv_region, by = "g_whoregion") %>%
                  rename(entity = group_description) %>%
                  arrange(g_whoregion)

# Calculate global aggregate
tb_hiv_global <- tb_hiv_region %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 c_notified, hiv_ipt,
                                 matches("_pct_")) %>%
                  mutate(entity = "Global")


# Calculate percentages for countries
tb_hiv_country <- within(tb_hiv_country, {

  hivtest_prct <- ifelse(c_notified==0,NA,frmt(hivtest * 100 / c_notified ))
  hivtest_pos_prct <- ifelse(hivtest==0,NA,frmt(hivtest_pos * 100/ hivtest ))
  hiv_cpt_prct <- ifelse(hivtest_pos==0,NA,frmt(hiv_cpt * 100/ hivtest_pos ))
  hiv_art_prct <- ifelse(hivtest_pos==0,NA,frmt(hiv_art * 100/ hivtest_pos ))

  # TEMPORARY POLITICAL SOLUTION FOR RUSSIAN FEDERATION 2010 onwards:
  # DO NOT CALCULATE % tb PATIENTS WITH KNOWN HIV STATUS
  # Enable or disable using flag in section A right at the top of the script.

  if (isTRUE(russianfudge)) {
    hivtest_prct <- ifelse(entity == "Russian Federation", NA, hivtest_prct)
    hivtest_pos_prct <- ifelse(entity == "Russian Federation", NA, hivtest_pos_prct)
  }
})

# Calculate percentages for regions and globally
tb_hiv_region <- within(tb_hiv_region, {

  hivtest_prct <- frmt(hivtest_pct_numerator * 100 / hivtest_pct_denominator )
  hivtest_pos_prct <- frmt(hivtest_pos_pct_numerator * 100/ hivtest_pos_pct_denominator )
  hiv_cpt_prct <- frmt(hiv_cpt_pct_numerator * 100/ hiv_cpt_pct_denominator )
  hiv_art_prct <- frmt(hiv_art_pct_numerator * 100/ hiv_art_pct_denominator )
})

tb_hiv_global <- within(tb_hiv_global, {

  hivtest_prct <- frmt(hivtest_pct_numerator * 100 / hivtest_pct_denominator )
  hivtest_pos_prct <- frmt(hivtest_pos_pct_numerator * 100/ hivtest_pos_pct_denominator )
  hiv_cpt_prct <- frmt(hiv_cpt_pct_numerator * 100/ hiv_cpt_pct_denominator )
  hiv_art_prct <- frmt(hiv_art_pct_numerator * 100/ hiv_art_pct_denominator )
})

# Harmonise variables in each of the tables before combining them
tb_hiv_country <- select(tb_hiv_country,
                         entity, c_notified,
                         hivtest, hivtest_pos, hiv_ipt,
                         hivtest_prct, hivtest_pos_prct,
                         hiv_cpt, hiv_cpt_prct,
                         hiv_art, hiv_art_prct)

tb_hiv_region <- select(tb_hiv_region,
                        entity, c_notified,
                        hivtest_pct_numerator, hivtest_pos_pct_numerator, hiv_ipt,
                        hivtest_prct, hivtest_pos_prct, hiv_cpt_prct, hiv_art_prct,
                        hiv_cpt_pct_numerator, hiv_art_pct_numerator) %>%
                  rename(hivtest = hivtest_pct_numerator,
                         hivtest_pos = hivtest_pos_pct_numerator,
                         hiv_cpt = hiv_cpt_pct_numerator,
                         hiv_art = hiv_art_pct_numerator)

tb_hiv_global <- select(tb_hiv_global,
                        entity, c_notified,
                        hivtest_pct_numerator, hivtest_pos_pct_numerator, hiv_ipt,
                        hivtest_prct, hivtest_pos_prct, hiv_cpt_prct, hiv_art_prct,
                        hiv_cpt_pct_numerator, hiv_art_pct_numerator) %>%
                  rename(hivtest = hivtest_pct_numerator,
                         hivtest_pos = hivtest_pos_pct_numerator,
                         hiv_cpt = hiv_cpt_pct_numerator,
                         hiv_art = hiv_art_pct_numerator)

# Create combined table in order of countries then regional and global estimates
tb_hiv <- combine_tables(tb_hiv_country, tb_hiv_region, tb_hiv_global)
rm(list=c("tb_hiv_country", "tb_hiv_region", "tb_hiv_global"))


# Format the output variables
tb_hiv <- within(tb_hiv, {

  c_notified <- rounder(c_notified)
  hivtest <- rounder(hivtest)
  hivtest_pos <- rounder(hivtest_pos)
  hiv_cpt <- rounder(hiv_cpt)
  hiv_art <- rounder(hiv_art)
  hiv_ipt <- rounder(hiv_ipt)

  # Add for blank columns
  blank <- ""

})

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(tb_hiv,
       select = c("entity", "blank",
                  "c_notified", "blank", "hivtest", "blank", "hivtest_prct", "blank",
                  "hivtest_pos", "blank", "hivtest_pos_prct", "blank",
                  "hiv_cpt", "blank", "hiv_cpt_prct", "blank",
                  "hiv_art", "blank", "hiv_art_prct", "blank",
                  "hiv_ipt"))  %>%
  write.csv(file="tb_hiv.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(tb_hiv)



