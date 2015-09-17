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
dir.create(file.path(annex4_folder, recursive = TRUE, "linked_CSVs"), showWarnings = FALSE)
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
# stop("OK, see what we have!")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   mort_prev (Table 1) -----
#   Estimates of TB mortality and prevalence
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country estimates
mort_prev_country <- filter(e, year == notification_maxyear) %>%
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
mort_prev <- combine_tables(mort_prev_country, mort_prev_region, mort_prev_global)
rm(list=c("mort_prev_country", "mort_prev_region", "mort_prev_global"))



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
inc_cdr_country <- filter(e, year == notification_maxyear) %>%
              arrange(country) %>%
              select(country, year, e_pop_num,
                     e_inc_num, e_inc_num_lo, e_inc_num_hi,
                     e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                     e_inc_tbhiv_num, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi,
                     e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
                     c_cdr, c_cdr_lo, c_cdr_hi) %>%
              rename(entity = country )

# get notifications and add them to the estimates
inc_cdr_country <- filter(n, year  == notification_maxyear) %>%
              select(country, c_newinc) %>%
              rename(entity = country ) %>%
              inner_join(inc_cdr_country, by = "entity") %>%
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
inc_cdr <- combine_tables(inc_cdr_country, inc_cdr_region, inc_cdr_global)
rm(list=c("inc_cdr_country", "inc_cdr_region", "inc_cdr_global"))


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




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   notif (Table 3) ----
#   Case notifications
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data

notif_country <- filter(n, year == notification_maxyear) %>%
            arrange(country) %>%
            select(country, g_whoregion,
                   c_newinc, new_labconf, new_clindx, new_ep,
                   ret_rel_labconf, ret_rel_clindx, ret_rel_ep, ret_nrel) %>%
            rename(entity = country )


# Calculate regional aggregates

notif_region <- notif_country %>%
                group_by(g_whoregion) %>%
                summarise_each(funs(sum(., na.rm = TRUE)), c_newinc, new_labconf, new_clindx, new_ep,
                               ret_rel_labconf, ret_rel_clindx, ret_rel_ep, ret_nrel)

# merge with regional names
notif_region <- filter(a, year == notification_maxyear & group_type == "g_whoregion") %>%
                select(group_name, group_description) %>%
                rename(g_whoregion = group_name) %>%
                inner_join(notif_region, by = "g_whoregion") %>%
                rename(entity = group_description) %>%
                arrange(entity)

# Calculate global aggregate
notif_global <- notif_country %>%
                summarise_each(funs(sum(., na.rm = TRUE)), c_newinc, new_labconf, new_clindx, new_ep,
                               ret_rel_labconf, ret_rel_clindx, ret_rel_ep, ret_nrel) %>%
                mutate(entity = "Global") %>%
                mutate(g_whoregion = "")  # dummy variable to match structure of the other two tables


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Handle Kosovo
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create a Kosovo only dataset from the nk view
notif_kosovo <- filter(nk, year == notification_maxyear) %>%
                select(country,
                       new_labconf, new_clindx, new_ep,
                       ret_rel_labconf, ret_rel_clindx, ret_rel_ep, ret_nrel) %>%
                mutate(g_whoregion = "EUR") %>%
                rename(entity = country )

# calculate c_newinc
notif_kosovo$c_newinc <- select(notif_kosovo, new_labconf, new_clindx, new_ep,
                                ret_rel_labconf, ret_rel_clindx, ret_rel_ep) %>%
                          sum_of_row()


# Create a Serbia (without Kosovo) dataset
notif_serbia <- filter(notif_country, entity=="Serbia")
notif_serbia_minus_kosovo <- notif_serbia

notif_vars <- c("c_newinc", "new_labconf", "new_clindx", "new_ep", "ret_rel_labconf", "ret_rel_clindx", "ret_rel_ep", "ret_nrel")

notif_serbia_minus_kosovo[notif_vars] <- notif_serbia[notif_vars] - notif_kosovo[notif_vars]
notif_serbia_minus_kosovo$entity <- "Serbia (without Kosovo)"


# Insert the two additional Kosovo/Serbia w/o Kosovo tables in appropriate spot.
insertpoint <- which(notif_country$entity == "Serbia" )
endpoint <- nrow(notif_country)

notif_country <- rbind(notif_country[1:insertpoint, ],
                       notif_serbia_minus_kosovo,
                       notif_kosovo,
                       notif_country[(insertpoint+1):endpoint, ] )

# clean up
rm(list=c("notif_kosovo", "notif_serbia", "notif_serbia_minus_kosovo"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End of handling Kosovo section
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create combined table in order of countries then regional and global estimates
notif <- combine_tables(notif_country, notif_region, notif_global)
rm(list=c("notif_country", "notif_region", "notif_global"))



# Format variables for output
# could do series of rounder() statements, variable by variable, e.g.

#          notif$c_newinc <- rounder(notif$c_newinc)

# or do with an sapply() statement for the specific columns:

notif[ , notif_vars] <- sapply(notif[,notif_vars], rounder)


# Add for blank columns
notif$blank <- ""

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead


subset(notif,
       select=c("entity", "blank",
                "c_newinc", "blank",
                "new_labconf", "blank", "new_clindx", "blank", "new_ep", "blank",
                "ret_rel_labconf", "blank", "ret_rel_clindx", "blank", "ret_rel_ep", "blank",
                "ret_nrel")) %>%
        write.csv(file="notif.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(notif)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   agesex  (Table 4) -----
#   New and relapse case notification by age and sex
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get country data
agesex_country <- filter(n, year == notification_maxyear) %>%
                  select(country, g_whoregion,
                         rel_in_agesex_flg,
                         newrel_m014, newrel_m15plus, newrel_mu,
                         newrel_f014, newrel_f15plus, newrel_fu,
                         newrel_sexunk014, newrel_sexunk15plus, newrel_sexunkageunk) %>%
                  rename(entity = country ) %>%
                  arrange(entity)

# Calculate regional aggregates
agesex_region <- agesex_country %>%
                  group_by(g_whoregion) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 newrel_m014, newrel_m15plus, newrel_mu,
                                 newrel_f014, newrel_f15plus, newrel_fu,
                                 newrel_sexunk014, newrel_sexunk15plus, newrel_sexunkageunk) %>%
                  mutate(rel_in_agesex_flg = NA) # dummy variable to match structure of country table

# merge with regional names
agesex_region <- filter(a, year == notification_maxyear & group_type == "g_whoregion") %>%
                  select(group_name, group_description) %>%
                  rename(g_whoregion = group_name) %>%
                  inner_join(agesex_region, by = "g_whoregion") %>%
                  rename(entity = group_description) %>%
                  arrange(entity)

# Calculate global aggregate
agesex_global <- agesex_country %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 newrel_m014, newrel_m15plus, newrel_mu,
                                 newrel_f014, newrel_f15plus, newrel_fu,
                                 newrel_sexunk014, newrel_sexunk15plus, newrel_sexunkageunk) %>%
                  mutate(entity = "Global") %>%
                  mutate(rel_in_agesex_flg = NA) %>% # dummy variable to match structure of the other two tables
                  mutate(g_whoregion = "")  # dummy variable to match structure of the other two tables


# Create combined table in order of countries then regional and global estimates
agesex <- combine_tables(agesex_country, agesex_region, agesex_global)
rm(list=c("agesex_country", "agesex_region", "agesex_global"))

# Calculate Male:Female ratio
agesex$mf_ratio <- ifelse(NZ(sum_of_row(agesex[c("newrel_f014", "newrel_f15plus", "newrel_fu")])) > 0 &
                            NZ(sum_of_row(agesex[c("newrel_m014", "newrel_m15plus", "newrel_mu")])) > 0,
                          frmt( sum_of_row(agesex[c("newrel_m014", "newrel_m15plus", "newrel_mu")]) /
                                  sum_of_row(agesex[c("newrel_f014", "newrel_f15plus", "newrel_fu")]) ),
                          NA )

# Calculate % aged under 15
agesex$u15_pct <- ifelse( NZ(sum_of_row(agesex[c("newrel_m014", "newrel_f014", "newrel_sexunk014",
                                                 "newrel_m15plus", "newrel_f15plus", "newrel_sexunk15plus")])) > 0,
                          frmt( sum_of_row(agesex[c("newrel_m014", "newrel_f014", "newrel_sexunk014")]) * 100
                                /
                                  sum_of_row(agesex[c("newrel_m014", "newrel_f014", "newrel_sexunk014",
                                                      "newrel_m15plus", "newrel_f15plus", "newrel_sexunk15plus")])
                          ),
                          NA )

# Format variables for output
agesex <- within(agesex, {

  newrel_m014 <- rounder(newrel_m014)
  newrel_f014 <- rounder(newrel_f014)
  newrel_sexunk014 <- rounder(newrel_sexunk014)

  newrel_m15plus <- rounder(newrel_m15plus)
  newrel_f15plus <- rounder(newrel_f15plus)
  newrel_sexunk15plus <- rounder(newrel_sexunk15plus)

  newrel_mu <- rounder(newrel_mu)
  newrel_fu <- rounder(newrel_fu)
  newrel_sexunkageunk <- rounder(newrel_sexunkageunk)

  # Flag for whether relapses were not included in age/sex table
  rel_absent_flg <- ifelse(rel_in_agesex_flg==0, "*",NA)

  # Add for blank columns
  blank <- ""

})


# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(agesex,
       select=c("entity", "blank",
                "rel_absent_flg",
                "newrel_m014", "blank", "newrel_m15plus", "blank", "newrel_mu", "blank",
                "newrel_f014", "blank", "newrel_f15plus", "blank", "newrel_fu", "blank",
                "newrel_sexunk014", "blank", "newrel_sexunk15plus", "blank", "newrel_sexunkageunk", "blank",
                "u15_pct", "blank",
                "mf_ratio", "blank")) %>%
  write.csv(file="agesex.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(agesex)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   agesex alternative  (Table 4 alternative) -----
#   New and relapse case notification by age and sex, for ALL age groups
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
                          # Add dummy variables to match structure of country table
                          mutate(sex = "Unknown",
                                 a1524 = NA,
                                 a2534 = NA,
                                 a3544 = NA,
                                 a4554 = NA,
                                 a5564 = NA,
                                 a65 = NA)

# Combine the country records into one
agesex_country <-  rbind(agesex_country_m, agesex_country_f, agesex_country_sexunk) %>%
                    rename( entity = country) %>%
                    arrange(entity, sex)
rm(list=c("agesex_country_m", "agesex_country_f", "agesex_country_sexunk"))

# Calculate regional aggregates
agesex_region <- agesex_country %>%
                  group_by(g_whoregion, sex) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 starts_with("a")) %>%
                  mutate(rel_in_agesex_flg = NA) # dummy variable to match structure of country table

# merge with regional names
agesex_region <- filter(a, year == notification_maxyear & group_type == "g_whoregion") %>%
                  select(group_name, group_description) %>%
                  rename(g_whoregion = group_name) %>%
                  inner_join(agesex_region, by = "g_whoregion") %>%
                  rename(entity = group_description) %>%
                  arrange(entity)


# Calculate global aggregate
agesex_global <- agesex_country %>%
                  group_by(sex) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 starts_with("a")) %>%
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

# Format variables for output

age_variables = c("a04", "a514", "a014", "a1524", "a2534", "a3544", "a4554", "a5564", "a65", "ageunk", "a15plus" )

# apply the rounder() function to the age variable
agesex[ , age_variables] <- sapply(agesex[,age_variables], rounder)

agesex <- within(agesex, {

  # Keep entity only if sex is female or entiry is "WHO Regions"
  entity <- ifelse(entity == "WHO regions" | sex == "Female", entity, NA )

  # Flag for whether relapses were not included in age/sex table
  rel_absent_flg <- ifelse(rel_in_agesex_flg==0 & sex == "Female", "*",NA)

  # Add for blank columns
  blank <- ""

})

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(agesex,
       select=c("entity", "blank",
                "rel_absent_flg", "blank",
                "sex", "blank",
                "a014", "blank", "a15plus", "blank", "ageunk", "blank",
                "a04", "blank", "a514", "blank",
                "a1524", "blank", "a2534", "blank", "a3544", "blank", "a4554", "blank", "a5564", "blank", "a65", "blank")) %>%
  write.csv(file="agesex_alt.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(agesex)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# rrmdr (Table 5) -----
# Detection of rifampicin-resistant and multidrug-resistant TB (RR-/MDR-TB),
# and MDR-TB estimates
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
# A. Notifications
rrmdr_country <- filter(n, year == notification_maxyear) %>%
                  select(country, g_whoregion,
                         c_rrmdr, mdr,
                         rdst_new, rdst_ret,
                         new_labconf, c_ret)

# B. Combine with routine surveillance
rrmdr_country <- filter(d, year == notification_maxyear) %>%
                  select(country,
                         dst_rlt_new, xpert_new,
                         dst_rlt_ret, xpert_ret) %>%
                  inner_join(rrmdr_country, by = "country")

# C. Combine with estimates among notified
rrmdr_country <- filter(emdrn, year == notification_maxyear) %>%
                  select(country,
                         e_mdr_num, e_mdr_num_lo, e_mdr_num_hi,
                         e_new_mdr_num, e_new_mdr_num_lo, e_new_mdr_num_hi,
                         e_ret_mdr_num, e_ret_mdr_num_lo, e_ret_mdr_num_hi) %>%
                  inner_join(rrmdr_country, by = "country") %>%
                  rename(entity = country ) %>%
                  arrange(entity)

# Calculate total number tested
rrmdr_country <- within(rrmdr_country, {

  # New cases tested for RR/MDR, including molecular diagnostics
  dst_new <- ifelse(is.na(rdst_new) & is.na(dst_rlt_new) & is.na(xpert_new),
                    NA,
                    ifelse(NZ(rdst_new) > NZ(sum_of_row(rrmdr_country[c("dst_rlt_new","xpert_new")])),
                           rdst_new,
                           sum_of_row(rrmdr_country[c("dst_rlt_new","xpert_new")]))
  )

  # previously treated cases tested for RR/MDR, including molecular diagnostics
  dst_ret <- ifelse(is.na(rdst_ret) & is.na(dst_rlt_ret) & is.na(xpert_ret),
                    NA,
                    ifelse(NZ(rdst_ret) > NZ(sum_of_row(rrmdr_country[c("dst_rlt_ret","xpert_ret")])),
                           rdst_ret,
                           sum_of_row(rrmdr_country[c("dst_rlt_ret","xpert_ret")]))
  )
})

# Drop the variables used to derive dst_new and dst_ret
rrmdr_country <- select(rrmdr_country,
                        -rdst_new, -dst_rlt_new, -xpert_new,
                        -rdst_ret, -dst_rlt_ret, -xpert_ret)


# Calculate the regional aggregates
# Note: Cannot do a simple aggregate for estimates, instead have to
#       take results of Babis's modelling from emdra data frame

# Aggregate the notifications and dst results (from rrmdr_country)

rrmdr_region <- rrmdr_country %>%
                group_by(g_whoregion) %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               c_rrmdr, mdr,
                               new_labconf, c_ret,
                               dst_new, dst_ret)

# Merge with regional estimates of mdr among notified
rrmdr_region <- filter(emdra, year == notification_maxyear & group_type == "g_whoregion") %>%
                select(group_name, group_description,
                       e_mdr_num, e_mdr_num_lo, e_mdr_num_hi,
                       e_new_mdr_num, e_new_mdr_num_lo, e_new_mdr_num_hi,
                       e_ret_mdr_num, e_ret_mdr_num_lo, e_ret_mdr_num_hi) %>%
                rename(g_whoregion = group_name) %>%
                inner_join(rrmdr_region, by = "g_whoregion") %>%
                rename(entity = group_description) %>%
                arrange(entity)


# Calculate the global aggregates using same logic as for regional aggregates
rrmdr_global <- rrmdr_country %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               c_rrmdr, mdr,
                               new_labconf, c_ret,
                               dst_new, dst_ret) %>%
                mutate(entity = "Global Aggregate")

# Merge with global estimates of mdr among notified
rrmdr_global <- filter(emdra, year == notification_maxyear & group_type == "global") %>%
                select(group_name, group_description,
                       e_mdr_num, e_mdr_num_lo, e_mdr_num_hi,
                       e_new_mdr_num, e_new_mdr_num_lo, e_new_mdr_num_hi,
                       e_ret_mdr_num, e_ret_mdr_num_lo, e_ret_mdr_num_hi) %>%
                rename(entity = group_description) %>%
                inner_join(rrmdr_global, by = "entity") %>%
                # change field name to match other tables
                rename(g_whoregion = group_name)


# Create combined table in order of countries then regional and global estimates
rrmdr <- combine_tables(rrmdr_country, rrmdr_region, rrmdr_global)
rm(list=c("rrmdr_country", "rrmdr_region", "rrmdr_global"))



# Calculate testing percentages and format everything for output

rrmdr <- within(rrmdr, {

  # % of new pulmonary lab-confirmed cases tested
  dst_new_pct <- ifelse(is.na(dst_new) | NZ(new_labconf) == 0, "", frmt(dst_new * 100 / new_labconf));

  # % of previously treated cases tested
  dst_ret_pct <- ifelse(is.na(dst_ret) | NZ(c_ret) == 0, "", frmt(dst_ret * 100 / c_ret))

  # format numbers
  c_rrmdr <- rounder(c_rrmdr)
  mdr <- rounder(mdr)
  dst_new <- rounder(dst_new)
  dst_ret <- rounder(dst_ret)


  # format the estimates
  e_mdr_num <- frmt(e_mdr_num)
  e_mdr_num_lo_hi <- paste0("(", frmt(e_mdr_num_lo), "–", frmt(e_mdr_num_hi), ")")

  e_new_mdr_num <- frmt(e_new_mdr_num)
  e_new_mdr_num_lo_hi <- paste0("(", frmt(e_new_mdr_num_lo), "–", frmt(e_new_mdr_num_hi), ")")

  e_ret_mdr_num <- frmt(e_ret_mdr_num)
  e_ret_mdr_num_lo_hi <- paste0("(", frmt(e_ret_mdr_num_lo), "–", frmt(e_ret_mdr_num_hi), ")")

  # Add for blank columns
  blank <- ""

})


# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(rrmdr,
       select=c("entity", "blank",
                "c_rrmdr", "blank", "mdr", "blank",
                "e_mdr_num", "e_mdr_num_lo_hi", "blank",
                "e_new_mdr_num", "e_new_mdr_num_lo_hi", "dst_new", "blank", "dst_new_pct", "blank", "blank",
                "e_ret_mdr_num", "e_ret_mdr_num_lo_hi", "dst_ret", "blank", "dst_ret_pct")) %>%
  write.csv(file="rrmdr.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(rrmdr)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# tb_hiv (Table 6)  -----
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
                  arrange(entity)

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
                         hivtest_prct, hivtest_pos_prct, hiv_cpt_prct, hiv_art_prct)

tb_hiv_region <- select(tb_hiv_region,
                        entity, c_notified,
                        hivtest_pct_numerator, hivtest_pos_pct_numerator, hiv_ipt,
                        hivtest_prct, hivtest_pos_prct, hiv_cpt_prct, hiv_art_prct) %>%
  rename(hivtest = hivtest_pct_numerator,
         hivtest_pos = hivtest_pos_pct_numerator)

tb_hiv_global <- select(tb_hiv_global,
                        entity, c_notified,
                        hivtest_pct_numerator, hivtest_pos_pct_numerator, hiv_ipt,
                        hivtest_prct, hivtest_pos_prct, hiv_cpt_prct, hiv_art_prct) %>%
  rename(hivtest = hivtest_pct_numerator,
         hivtest_pos = hivtest_pos_pct_numerator)

# Create combined table in order of countries then regional and global estimates
tb_hiv <- combine_tables(tb_hiv_country, tb_hiv_region, tb_hiv_global)
rm(list=c("tb_hiv_country", "tb_hiv_region", "tb_hiv_global"))


# Format the output variables
tb_hiv <- within(tb_hiv, {

  c_notified <- rounder(c_notified)
  hivtest <- rounder(hivtest)
  hivtest_pos <- rounder(hivtest_pos)
  hiv_ipt <- rounder(hiv_ipt)

  # Add for blank columns
  blank <- ""

})

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(tb_hiv,
       select = c("entity", "blank",
                  "hivtest_prct", "blank", "hivtest", "blank", "c_notified", "blank",
                  "hivtest_pos", "blank", "hivtest_pos_prct", "blank",
                  "hiv_cpt_prct", "blank", "hiv_art_prct", "blank", "hiv_ipt"))  %>%
  write.csv(file="tb_hiv.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(tb_hiv)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   outcome (Table 7) ----
#   Treatment outcomes, all types
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
# A. Standard chorts
outcome_country <- filter(o, year == outcome_maxyear) %>%
                    select(country, g_whoregion,
                           rel_with_new_flg,
                           newrel_coh, newrel_succ,
                           ret_nrel_coh, ret_nrel_succ,
                           tbhiv_coh, tbhiv_succ)

# B. Combine with M/XDR cohorts one year older than standard
outcome_country <- filter(o, year == (outcome_maxyear - 1)) %>%
                    select(country,
                           mdr_coh, mdr_succ,
                           xdr_coh, xdr_succ) %>%
                    inner_join(outcome_country, by = "country") %>%
                    rename(entity = country ) %>%
                    arrange(entity)

# Calculate regional aggregates

outcome_region <- outcome_country %>%
                  group_by(g_whoregion) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 newrel_coh, newrel_succ,
                                 ret_nrel_coh, ret_nrel_succ,
                                 tbhiv_coh, tbhiv_succ,
                                 mdr_coh, mdr_succ,
                                 xdr_coh, xdr_succ) %>%
                  mutate(rel_with_new_flg = NA) # dummy variable to match structure of country table

# merge with regional names
outcome_region <- filter(a, year == notification_maxyear & group_type == "g_whoregion") %>%
                  select(group_name, group_description) %>%
                  rename(g_whoregion = group_name) %>%
                  inner_join(outcome_region, by = "g_whoregion") %>%
                  rename(entity = group_description) %>%
                  arrange(entity)


# Calculate global aggregate
outcome_global <- outcome_country %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 newrel_coh, newrel_succ,
                                 ret_nrel_coh, ret_nrel_succ,
                                 tbhiv_coh, tbhiv_succ,
                                 mdr_coh, mdr_succ,
                                 xdr_coh, xdr_succ) %>%
                  mutate(entity = "Global") %>%
                  mutate(rel_with_new_flg = NA) %>% # dummy variable to match structure of the other two tables
                  mutate(g_whoregion = "")  # dummy variable to match structure of the other two tables



# Create combined table in order of countries then regional and global estimates
outcome <- combine_tables(outcome_country, outcome_region, outcome_global)
rm(list=c("outcome_country", "outcome_region", "outcome_global"))


# Calculate treatment success rates and format variables for output
outcome <- within(outcome, {

  # New or new+relapse
  c_new_tsr <- ifelse( is.na(newrel_coh), NA, rounder( newrel_succ * 100 /newrel_coh ))

  # Retreatment or retreatment excluding relapse
  c_ret_tsr <- ifelse( is.na(ret_nrel_coh), NA, rounder( ret_nrel_succ * 100 / ret_nrel_coh ))

  # HIV-positive, all cases
  c_tbhiv_tsr <- ifelse( is.na(tbhiv_coh), NA, rounder( tbhiv_succ * 100 / tbhiv_coh ))

  # MDR and XDR
  c_mdr_tsr <- ifelse( is.na(mdr_coh), NA, rounder( mdr_succ * 100 / mdr_coh))
  c_xdr_tsr <- ifelse( is.na(mdr_coh), NA, rounder( xdr_succ * 100 / xdr_coh))

  # Mark countries where relapses included with new cases
  newrel_flg <- ifelse(NZ(rel_with_new_flg)==1, "*",NA)

  # Add for blank columns
  blank <- ""

})

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(outcome,
       select = c("entity", "blank",
                  "newrel_coh", "newrel_flg", "c_new_tsr", "blank",
                  "ret_nrel_coh", "newrel_flg", "c_ret_tsr", "blank",
                  "tbhiv_coh", "blank", "c_tbhiv_tsr", "blank",
                  "mdr_coh", "blank", "c_mdr_tsr", "blank",
                  "xdr_coh", "blank", "c_xdr_tsr", "blank"))  %>%
  write.csv(file="outcome.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(outcome)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# labs (Table 8) -----
# Laboratories and infection control
# Country level only, no need for aggregates
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get country data
labs <- filter(s, year == notification_maxyear) %>%
        select(country,
               c_lab_sm_100k, lab_sm_f, lab_sm_led,
               c_lab_cul_5m, c_lab_dst_5m, c_lab_lpa_5m, lab_xpert,
               sldst_avail_incntry, sldst_avail_outcntry, nrl,
               hcw_tot, hcw_tb_infected) %>%
        arrange(country)


# Calculate some variables and format for output
labs <- within(labs, {

  # % smear labs using LED microscopy
  lab_sm_led_pct <- ifelse(is.na(lab_sm_led) | is.na(lab_sm_f), '–', rounder(lab_sm_led * 100 / lab_sm_f))

  # Availability of 2nd line DST
  sldst <- ifelse(sldst_avail_incntry==1 & sldst_avail_outcntry==1, 'In and out of country',
                  ifelse(sldst_avail_incntry==1, 'In country',
                         ifelse(sldst_avail_outcntry==1, 'Out of country',
                                ifelse(sldst_avail_incntry==0 & sldst_avail_outcntry==0, 'No', NA))))

  # Convert nrl variable to its full description using the datacodes table
  nrl <- factor(nrl, levels=datacodes$option_id, labels=datacodes$optiontext_EN)

  # notification rate among health care workers
  # (ignore records where numerator = denominator!)
  hcw_100k <- ifelse(is.na(hcw_tb_infected) | NZ(hcw_tot)==0 | hcw_tb_infected == hcw_tot ,
                     "",
                     rounder(hcw_tb_infected * 100000 / hcw_tot ))

  # Add for blank columns
  blank <- ""

})

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(labs,
       select=c("country", "blank",
                "c_lab_sm_100k", "blank", "lab_sm_led_pct", "blank",
                "c_lab_cul_5m", "blank", "c_lab_dst_5m", "blank", "c_lab_lpa_5m", "blank", "lab_xpert", "blank",
                "sldst", "nrl", "blank",
                "hcw_100k")) %>%
  write.csv(file="labs.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(labs)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# mdr_measured (Table 9) -----
# Measured percentage of TB cases with MDR-TB
# Shows source of MDR-TB measurements for those countries with usable survey or surveillance data
# No aggregates used
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data

mdr_measured <- filter(emdr, year == notification_maxyear) %>%
                select(country,
                       source_mdr_new, source_drs_year_new, source_drs_coverage_new,
                       e_new_mdr_pcnt,e_new_mdr_pcnt_lo, e_new_mdr_pcnt_hi,
                       source_mdr_ret, source_drs_year_ret, source_drs_coverage_ret,
                       e_ret_mdr_pcnt, e_ret_mdr_pcnt_lo, e_ret_mdr_pcnt_hi)  %>%
                arrange(country)


# blank out source, estimates etc if source is not survey or surveillance
mdr_measured[mdr_measured$source_mdr_new != "Surveillance" & mdr_measured$source_mdr_new != "Survey",
        c("source_mdr_new", "source_drs_year_new", "source_drs_coverage_new",
          "e_new_mdr_pcnt", "e_new_mdr_pcnt_lo", "e_new_mdr_pcnt_hi")] <- NA

mdr_measured[mdr_measured$source_mdr_ret != "Surveillance" & mdr_measured$source_mdr_ret != "Survey",
        c("source_mdr_ret", "source_drs_year_ret", "source_drs_coverage_ret",
          "e_ret_mdr_pcnt", "e_ret_mdr_pcnt_lo", "e_ret_mdr_pcnt_hi")] <- NA

# Format variables for output
mdr_measured <- within(mdr_measured, {

  # concatenate confidence interval variables into bracketed strings
  e_new_mdr_pcnt_lo_hi <- frmt_intervals(e_new_mdr_pcnt,
                                        e_new_mdr_pcnt_lo,
                                        e_new_mdr_pcnt_hi, rates=TRUE)

  e_ret_mdr_pcnt_lo_hi <- frmt_intervals(e_ret_mdr_pcnt,
                                        e_ret_mdr_pcnt_lo,
                                        e_ret_mdr_pcnt_hi, rates=TRUE)

  # Add for blank columns
  blank <- ""

})


# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(mdr_measured,
       select=c("country", "blank",
                "source_drs_year_new", "blank", "source_mdr_new", "blank", "source_drs_coverage_new", "blank",
                "e_new_mdr_pcnt", "blank", "e_new_mdr_pcnt_lo_hi", "blank",
                "source_drs_year_ret", "blank", "source_mdr_ret", "blank", "source_drs_coverage_ret", "blank",
                "e_ret_mdr_pcnt", "blank", "e_ret_mdr_pcnt_lo_hi")) %>%
  write.csv(file="mdr_measured.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(mdr_measured)


