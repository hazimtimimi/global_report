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


# Convert estimated burden numbers to thousands (but don't include e_pop_num)
for(col in 4:ncol(mort_prev)){
  # check each column name to see if it contains _num
  if(grepl("_num", names(mort_prev[col]))== TRUE) {
    mort_prev[col] <- mort_prev[col] / 1000
  }
}

# Format variables for output
mort_prev <- within(mort_prev, {

  # round population to millions
  e_pop_num <- ifelse(e_pop_num / 1000000 < 1, "< 1", rounder(e_pop_num / 1000000))

  # mortality excluding HIV
  mort_num <- frmt(e_mort_exc_tbhiv_num, thou=TRUE)
  mort_num_lo_hi <- frmt_intervals(e_mort_exc_tbhiv_num,
                                 e_mort_exc_tbhiv_num_lo,
                                 e_mort_exc_tbhiv_num_hi, thou=TRUE)

  mort_rate <- frmt(e_mort_exc_tbhiv_100k, rates=TRUE)
  mort_rate_lo_hi <- frmt_intervals(e_mort_exc_tbhiv_100k,
                                e_mort_exc_tbhiv_100k_lo,
                                e_mort_exc_tbhiv_100k_hi, rates=TRUE)

  # mortality (HIV-positive TB)
  mort_tbhiv_num <- frmt(e_mort_tbhiv_num, thou=TRUE, thouEst=TRUE)
  mort_tbhiv_num_lo_hi <- frmt_intervals(e_mort_tbhiv_num,
                                 e_mort_tbhiv_num_lo,
                                 e_mort_tbhiv_num_hi, thou=TRUE, thouEst=TRUE)

  mort_tbhiv_rate <- frmt(e_mort_tbhiv_100k, rates=TRUE)
  mort_tbhiv_rate_lo_hi <- frmt_intervals(e_mort_tbhiv_100k,
                                e_mort_tbhiv_100k_lo,
                                e_mort_tbhiv_100k_hi, rates=TRUE)

  # prevalence
  prev_num <- frmt(e_prev_num, thou=TRUE)
  prev_num_lo_hi <- frmt_intervals(e_prev_num,
                                 e_prev_num_lo,
                                 e_prev_num_hi, thou=TRUE)

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


# Convert estimated burden numbers to thousands (but don't include e_pop_num)
for(col in 5:ncol(inc_cdr)){
  # check each column name to see if it contains _num
  if(grepl("^e_[^pop]*_num", names(inc_cdr[col]))== TRUE) {
    inc_cdr[col] <- inc_cdr[col] / 1000
  }
}

# Format variables for output
inc_cdr <- within(inc_cdr, {

  # round population to millions
  e_pop_num <- ifelse(e_pop_num / 1000000 < 1, "< 1", rounder(e_pop_num / 1000000))

  # incidence
  inc_num <- frmt(e_inc_num, thou=TRUE)
  inc_num_lo_hi <- frmt_intervals(e_inc_num,
                                 e_inc_num_lo,
                                 e_inc_num_hi, thou=TRUE)

  inc_rate <- frmt(e_inc_100k, rates=TRUE)
  inc_rate_lo_hi <- frmt_intervals(e_inc_100k,
                                e_inc_100k_lo,
                                e_inc_100k_hi, rates=TRUE)

  # TB/HIV incidence
  inc_tbhiv_num <- frmt(e_inc_tbhiv_num, thou=TRUE, thouEst=TRUE)
  inc_tbhiv_num_lo_hi <- frmt_intervals(e_inc_tbhiv_num,
                                 e_inc_tbhiv_num_lo,
                                 e_inc_tbhiv_num_hi, thou=TRUE, thouEst=TRUE)

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



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   notif (was Table 3) ----
#   Case notifications
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data

notif_vars <- c("c_newinc", "new_labconf", "new_sp", "new_clindx", "new_sn", "new_su", "new_ep", "new_oth", "ret_rel_labconf", "ret_rel_clindx", "ret_rel_ep", "ret_rel", "ret_nrel", "ret_taf", "ret_tad", "ret_oth", "newret_oth")

# Get data, including population to calculate case notification rate for sparklines
notif_c <- merge(subset(n, year %in% 1990:notification_maxyear, select=c("country", "year", "g_whoregion", notif_vars, "iso3")), subset(e, select=c("iso3", "year", "e_pop_num")), all.x=TRUE)

notif_c$type <- "countries"
names(notif_c)[names(notif_c)=="country"] <- "group_name"


# change data type of population estimate so that don't hit limit when calculating global aggregates
notif_c$e_pop_num <- as.numeric(notif_c$e_pop_num)

# Calculate aggregates.

# Regional aggregate
notif_agg_r <- aggregate( notif_c[5:22], by=list(iso3=notif_c$g_whoregion, year=notif_c$year), FUN="sum", na.rm=TRUE)

# Global aggregates
notif_agg_g <- aggregate(notif_c[5:22], by=list(year=notif_c$year), FUN="sum",  na.rm=TRUE)
notif_agg_g $iso3 <- "global"

# Note that I want the sum to be NA if AND ONLY IF, all values are NA, but could not see an easy way to do this
# because with na.rm=TRUE I get a 0 for the sum. However I think all aggregates should be > 0 therefore easiest
# thing to do is to assume all zeros in the resulting aggregates should be NA

notif_agg_r[4:19] <- sapply(notif_agg_r[4:19], function(x){ ifelse(x==0,NA,x)})
notif_agg_g[3:18] <- sapply(notif_agg_g[3:18], function(x){ ifelse(x==0,NA,x)})

# Combine aggregates
notif_agg <- rbind(notif_agg_r, notif_agg_g)
notif_agg$type <- "aggs"
notif_agg$group_name <- notif_agg$iso3
notif_agg$g_whoregion <- notif_agg$group_name


# combine country and aggregates and reorder
notif <- rbind(notif_agg, notif_c)
notif <- properorder(notif)
notif$order <- 1:nrow(notif)


# Calculate case notification rate for sparklines
notif$newrel_100k <- notif$c_newinc  * 100000 / notif$e_pop_num



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Handle Kosovo
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create a Kosovo only dataset from the nk view, but don't include the iso2 column
n_kosovo <- nk[nk$year <= notification_maxyear, !(names(nk) %in% "iso2")]     # could use nk[-2], but that depends on column numbering

# calculate c_newinc, depending on year
n_kosovo$c_newinc <- ifelse(n_kosovo$year < 2013,
                             sum_of_row(n_kosovo[c("new_sp", "new_sn", "new_su", "new_ep", "new_oth", "ret_rel", "newret_oth")]) ,
                             sum_of_row(n_kosovo[c("new_labconf", "new_clindx", "new_ep", "ret_rel_labconf", "ret_rel_clindx", "ret_rel_ep")])
                             )


n_kosovo$iso3 <- "XX1"
n_kosovo$g_whoregion <- "EUR"
n_kosovo$type <- "countries"
names(n_kosovo)[names(n_kosovo)=="country"] <- "group_name"


# Create a Serbia (without Kosovo)  dataset
n_serbia <- subset(notif_c, group_name=="Serbia", select=names(n_kosovo))
n_serbia_nkos <- n_serbia
n_serbia_nkos[notif_vars] <- n_serbia[notif_vars] - n_kosovo[notif_vars]
n_serbia_nkos$group_name <- "Serbia (without Kosovo)"
n_serbia_nkos$iso3 <- "XX2"


# Add empty variables to the two extra tables to match 'notif'
n_kosovo$e_pop_num <- NA
n_kosovo$newrel_100k <- NA
n_kosovo <- properorder(n_kosovo)
n_kosovo$order <- 1:nrow(n_kosovo)

n_serbia_nkos$e_pop_num <- NA
n_serbia_nkos$newrel_100k <- NA
n_serbia_nkos <- properorder(n_serbia_nkos)
n_serbia_nkos$order <- 1:nrow(n_serbia_nkos)


# Insert the two additional Kosovo/Serbia w/o Kosovo tables in appropriate spot.
insertpoint <- max(subset(notif, group_name=="Serbia", order)) + 1

notif <- rbind(notif[notif$order < insertpoint, ],
                n_serbia_nkos,
                n_kosovo,
                notif[notif$order >= insertpoint, ] )

notif$order <- 1:nrow(notif)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End of handling Kosovo section
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Calculate total new smear-neg, smear unknown, others for years prior to 2013
notif$new_snu <- ifelse(notif$year < 2013, sum_of_row(notif[c("new_sn", "new_su","new_oth")]) , NA )

# Calculate total retreatment cases excluding relapses for years prior to 2013
notif$ret_nrel <- ifelse(notif$year < 2013, sum_of_row(notif[c("ret_taf", "ret_tad", "ret_oth")]) , notif$ret_nrel )

# For years before 2013, only show new_labconf for EUR countries (others weren't reporting consistently)
notif$new_labconf <- ifelse(notif$year < 2013 & notif$g_whoregion != "EUR", NA, notif$new_labconf)

# For EUR, only show new_labconf if >= new_sp ! (wasn't true in early years collection)
notif$new_labconf <- ifelse(notif$year < 2013 & notif$new_labconf < notif$new_sp, NA, notif$new_labconf)

# Format variables for output
# could do series of rounder() statements, variable by variable, e.g.

#          notif$c_newinc <- rounder(notif$c_newinc)

# or do with an sapply() statement for the specific columns:

numeric_vars <- c("c_newinc", "new_labconf",  "new_clindx", "new_sp", "new_snu", "new_ep",  "ret_rel_labconf", "ret_rel_clindx", "ret_rel_ep", "ret_rel", "ret_nrel", "newret_oth")

notif[ , numeric_vars] <- sapply(notif[,numeric_vars], rounder)

# add conditional formatting column(s)
notif <- ddply(notif, "group_name", add_formatting)

# Add for blank columns
notif$blank <- NA

# restrict to variables of interest, re-order and export

notif <- subset(notif, select=c("year", "c_newinc", "blank", "new_labconf", "blank", "new_clindx", "blank", "new_sp", "blank", "new_snu", "blank", "new_ep", "blank", "ret_rel_labconf", "blank", "ret_rel_clindx", "blank", "ret_rel_ep", "blank", "ret_rel", "blank", "ret_nrel", "blank", "newret_oth", "blank" , "newrel_100k", "format", "group_name", "g_whoregion", "type", "order"))

notif <- notif[order(notif$order),]

write.csv(notif[notif$type=="aggs",], file="notif_agg.csv", row.names=FALSE, na="")
write.csv(notif[notif$type=="countries",], file="notif_c.csv", row.names=FALSE, na="")

# and now clear up the mess left behind
rm(list=c("notif", "notif_agg", "notif_agg_g", "notif_agg_r", "notif_c", "n_kosovo", "n_serbia", "n_serbia_nkos"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   outcome (was Table 4) ----
#   Treatment outcomes, all types
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get country data

outcome_c <- subset(o, year %in% 1995:outcome_maxyear, select=c("country", "year", "g_whoregion","iso3",
    "new_sp_coh", "new_sp_cur", "new_sp_cmplt", "new_snep_coh", "new_snep_cmplt",
	"newrel_coh", "newrel_succ",
	"ret_coh", "ret_cur", "ret_cmplt",
	"ret_nrel_coh", "ret_nrel_succ",
	"hiv_new_sp_coh", "hiv_new_sp_cur", "hiv_new_sp_cmplt",
	"hiv_new_snep_coh", "hiv_new_snep_cmplt",
	"hiv_ret_coh", "hiv_ret_cur", "hiv_ret_cmplt",
	"tbhiv_coh", "tbhiv_succ",
	"mdr_coh", "mdr_succ", "xdr_coh", "xdr_succ",
	"c_new_tsr", "c_ret_tsr", "rel_with_new_flg", "c_tbhiv_tsr", "c_mdr_tsr", "xdr_coh", "c_xdr_tsr" ))


# Remove any MDR/XDR outcomes in outcome_maxyear  (they shouldn't be published yet)

outcome_c[outcome_c$year==outcome_maxyear, c("mdr_coh", "mdr_succ", "xdr_coh", "xdr_succ", "c_mdr_tsr",  "c_xdr_tsr")]  <- sapply(outcome_c[outcome_c$year==outcome_maxyear, c("mdr_coh", "mdr_succ", "xdr_coh", "xdr_succ", "c_mdr_tsr",  "c_xdr_tsr")], function(x){ NA})

outcome_c$type <- 'countries'
names(outcome_c)[names(outcome_c)=="country"] <- "group_name"

# Assemble aggregates
outcome_agg_r <- aggregate(outcome_c[5:30], by=list(iso3=outcome_c$g_whoregion, year=outcome_c$year), FUN='sum', na.rm=T)

outcome_agg_g <- aggregate(outcome_c[5:30], by=list(year=outcome_c$year), FUN='sum', na.rm=T)
outcome_agg_g$iso3 <- 'global'

# Combine aggregates
outcome_agg <- rbind(outcome_agg_r, outcome_agg_g)
outcome_agg$type <- "aggs"
outcome_agg$group_name <- outcome_agg$iso3
outcome_agg$g_whoregion <- outcome_agg$group_name

# Calculate treatment success rates for aggregates and format variables for output
outcome_agg <- within(outcome_agg, {

  # New or new+relapse (mutually exclusive variables, so this works)
  c_new_tsr <- rounder( (new_sp_cur + new_sp_cmplt + new_snep_cmplt + newrel_succ) * 100 /
                        (new_sp_coh + new_snep_coh + newrel_coh) )

  # Retreatment or retreatment excluding relapse (mutually exclusive variables, so this works)
  c_ret_tsr <- rounder( (ret_cur + ret_cmplt + ret_nrel_succ) * 100 /
                        (ret_coh + ret_nrel_coh) )

  # HIV-positive, all cases (mutually exclusive variables, so this works)
  c_tbhiv_tsr <- rounder( (hiv_new_sp_cur + hiv_new_sp_cmplt + hiv_new_snep_cmplt + hiv_ret_cur + hiv_ret_cmplt + tbhiv_succ) * 100 /
                          (hiv_new_sp_coh + hiv_new_snep_coh + hiv_ret_coh + tbhiv_coh) )

  # MDR and XDR (variables already harmonised in the view)
  c_mdr_tsr <- rounder( mdr_succ * 100 / mdr_coh)
  c_xdr_tsr <- rounder( xdr_succ * 100 / xdr_coh)

  # Calculate and format the cohort sizes
  new_coh <- rounder(new_sp_coh + new_snep_coh + newrel_coh)
  ret_coh <- rounder(ret_coh + ret_nrel_coh)
  tbhiv_coh <- rounder(hiv_new_sp_coh + hiv_new_snep_coh + hiv_ret_coh + tbhiv_coh)
  mdr_coh <- rounder(mdr_coh)
  xdr_coh <- rounder(xdr_coh)

  # Change any zero-sized cohorts to NA for TBHIV, MDR and XDR
  tbhiv_coh <- ifelse(tbhiv_coh==0,NA,tbhiv_coh)
  mdr_coh <- ifelse(mdr_coh==0,NA,mdr_coh)
  xdr_coh <- ifelse(xdr_coh==0,NA,xdr_coh)

  blank <- NA
  newrel_flg <- NA
})
outcome_agg <- properorder(outcome_agg)
outcome_agg$order <- 1:nrow(outcome_agg)
outcome_agg <- ddply(outcome_agg, "group_name", add_formatting)

# Format the output for the country tables (do after aggregates because it rounds numbers and converts to chars)
outcome_c <- within(outcome_c, {

  # Format tsr
  c_new_tsr <- rounder(c_new_tsr)
  c_ret_tsr <- rounder(c_ret_tsr)
  c_tbhiv_tsr <- rounder(c_tbhiv_tsr)
  c_mdr_tsr <- rounder(c_mdr_tsr)
  c_xdr_tsr <- rounder(c_xdr_tsr)

  # Calculate and format the cohort sizes
  # Note from 2012 onwards using a different set of variables
  new_coh <- ifelse(year<2012,
                    rounder(sum_of_row(outcome_c[c("new_sp_coh","new_snep_coh")]) ),
                    rounder(newrel_coh))

  ret_coh <- ifelse(year<2012,
                    rounder(ret_coh),
                    rounder(ret_nrel_coh))

  newrel_flg <- ifelse(NZ(rel_with_new_flg)==1, "*",NA)

  tbhiv_coh <- rounder(sum_of_row(outcome_c[c("hiv_new_sp_coh", "hiv_new_snep_coh", "hiv_ret_coh", "tbhiv_coh")]))

  mdr_coh <- rounder(mdr_coh)
  xdr_coh <- rounder(xdr_coh)

  blank <- NA
})
outcome_c <- properorder(outcome_c)
outcome_c$order <- 1:nrow(outcome_c)
outcome_c <- ddply(outcome_c, "group_name", add_formatting)


# Then output to CSV (no reason to combine the two into one table
t4_output_vars = c("year", "blank", "new_coh", "newrel_flg", "c_new_tsr", "blank", "ret_coh", "newrel_flg", "c_ret_tsr", "blank", "tbhiv_coh", "blank", "c_tbhiv_tsr", "blank", "mdr_coh", "blank", "c_mdr_tsr", "blank", "xdr_coh", "blank", "c_xdr_tsr", "blank","format", "group_name", "g_whoregion", "iso3", "type", "order")

outcome_c <- subset(outcome_c, select=t4_output_vars)
outcome_c <- outcome_c[order(outcome_c$order),]
write.csv(outcome_c, file="outcome_c.csv", row.names=FALSE, na="")

outcome_agg <- subset(outcome_agg, select=t4_output_vars)
outcome_agg <- outcome_agg[order(outcome_agg$order),]
write.csv(outcome_agg, file="outcome_agg.csv", row.names=FALSE, na="")

# and now clear up the mess left behind
rm(list=c("t4_output_vars", "outcome_agg", "outcome_agg_g", "outcome_agg_r", "outcome_c"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   hiv_test (was Table 6)  -----
#   HIV testing and provision of CPT, ART and IPT
#	Note that dataset tbhiv already has rules built in for combining _p and _f
#	(final and provisional) numbers, plus adjusted variables for calculating aggregates.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
hiv_test_c <- merge(tbhiv, subset(n, year %in% 2003:notification_maxyear, select= c(iso3, country, year, g_whoregion, c_notified, hiv_ipt)), all.y=TRUE)

hiv_test_c$type <- 'countries'
names(hiv_test_c)[names(hiv_test_c)=="country"] <- "group_name"

# Calculate the aggregates, using the massaged variables *_pct_numerator and *_pct_denominator
# Regional aggregates
hiv_test_agg_r <- aggregate(hiv_test_c[, c(grep("_pct_|hiv_ipt|c_notified", names(hiv_test_c)))], by=list(iso3=hiv_test_c$g_whoregion, year=hiv_test_c$year), FUN='sum', na.rm=T)

# Global aggregate
hiv_test_agg_g <- aggregate(hiv_test_c[, c(grep("_pct_|hiv_ipt|c_notified", names(hiv_test_c)))], by=list( year=hiv_test_c$year), FUN='sum', na.rm=T)
hiv_test_agg_g$iso3 <- 'global'

# Combine aggregates
hiv_test_agg <- rbind(hiv_test_agg_r, hiv_test_agg_g)
hiv_test_agg$type <- "aggs"
hiv_test_agg$group_name <- hiv_test_agg$iso3
hiv_test_agg$g_whoregion <- hiv_test_agg$group_name


# Calculate and format the output variables for countries
hiv_test_c <- within(hiv_test_c, {
  hivtest_prct <- ifelse(c_notified==0,NA,frmt(hivtest * 100 / c_notified ))
  hivtest_num <- rounder(hivtest)
  notif <- rounder(c_notified)
  tbhiv_pos <- rounder(hivtest_pos)

  hivtest_pos_prct <- ifelse(hivtest==0,NA,frmt(hivtest_pos * 100/ hivtest ))
  hiv_cpt_prct <- ifelse(hivtest_pos==0,NA,frmt(hiv_cpt * 100/ hivtest_pos ))
  hiv_art_prct <- ifelse(hivtest_pos==0,NA,frmt(hiv_art * 100/ hivtest_pos ))
  hiv_ipt <- rounder(hiv_ipt)

  blank <- NA

  # TEMPORARY POLITICAL SOLUTION FOR RUSSIAN FEDERATION 2010 onwards:
  # DO NOT CALCULATE % tb PATIENTS WITH KNOWN HIV STATUS
  # Enable or disable using flag in section A right at the top of the script.

  if (isTRUE(russianfudge)) {
    hivtest_prct <- ifelse(group_name=='Russian Federation' & year>2009,NA,hivtest_prct)
    hivtest_pos_prct <- ifelse(group_name=='Russian Federation' & year>2009,NA,hivtest_pos_prct)
  }
})
hiv_test_c <- properorder(hiv_test_c)
hiv_test_c$order <- 1:nrow(hiv_test_c)
hiv_test_c <- ddply(hiv_test_c, "group_name", add_formatting)


# Calculate and format the output variables for aggregates

hiv_test_agg <- within(hiv_test_agg, {
  hivtest_prct <- frmt(hivtest_pct_numerator * 100 / hivtest_pct_denominator )
  hivtest_num <- rounder(hivtest_pct_numerator)
  notif <- rounder(c_notified)
  tbhiv_pos <- rounder(hivtest_pos_pct_numerator)

  hivtest_pos_prct <- frmt(hivtest_pos_pct_numerator * 100/ hivtest_pos_pct_denominator )
  hiv_cpt_prct <- frmt(hiv_cpt_pct_numerator * 100/ hiv_cpt_pct_denominator )
  hiv_art_prct <- frmt(hiv_art_pct_numerator * 100/ hiv_art_pct_denominator )
  hiv_ipt <- rounder(hiv_ipt)

  blank <- NA
})
hiv_test_agg <- properorder(hiv_test_agg)
hiv_test_agg$order <- 1:nrow(hiv_test_agg)
hiv_test_agg <- ddply(hiv_test_agg, "group_name", add_formatting)


# Then output to CSV (no reason to combine the two into one table !!!!)
t6_output_vars = c("year", "blank", "hivtest_prct", "blank", "hivtest_num", "blank", "notif", "blank", "tbhiv_pos", "blank", "hivtest_pos_prct", "blank", "hiv_cpt_prct", "blank", "hiv_art_prct", "blank", "hiv_ipt", "blank", "format", "group_name", "g_whoregion", "iso3", "type", "order")


hiv_test_c <- subset(hiv_test_c, select=t6_output_vars)
hiv_test_c <- hiv_test_c[order(hiv_test_c$order),]
write.csv(hiv_test_c, file="hiv_test_c.csv", row.names=FALSE, na="")

hiv_test_agg <- subset(hiv_test_agg, select=t6_output_vars)
hiv_test_agg <- hiv_test_agg[order(hiv_test_agg$order),]
write.csv(hiv_test_agg, file="hiv_test_agg.csv", row.names=FALSE, na="")

# and now clear up the mess left behind
rm(list=c("hiv_test_agg", "hiv_test_agg_g", "hiv_test_agg_r", "hiv_test_c"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   rrmdr (was Table 7) -----
#   Testing for MDR-TB and number of confirmed cases of MDR-TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get country data

rrmdr_c <- merge(subset(n, year %in% 2005:notification_maxyear, select=c("iso3", "country", "year", "g_whoregion", "mdr", "rr_nmdr", "rdst_new", "rdst_ret", "c_ret", "new_labconf", "new_bc", "new_sp")), subset(d, select=c("iso3", "country", "year", "dst_rlt_new", "xpert_new", "dst_rlt_ret", "xpert_ret")))



# Do some calculations
rrmdr_c <- within(rrmdr_c, {

  # Number of new bacteriologically-confirmed patients (this is a bit of a fudge until we can capture this variable more consistently)
  new_bact_pos <- new_labconf #default value
  new_bact_pos <- ifelse( (!is.na(new_bact_pos) & !is.na(new_sp) & new_bact_pos < new_sp) | (is.na(new_bact_pos) & !is.na(new_sp)), new_sp, new_bact_pos)
  new_bact_pos <- ifelse(!is.na(new_bc), new_bc, new_bact_pos)   # for EUR contries

  # Total rr-/mdr-tb cases found
  rrmdr <- sum_of_row(rrmdr_c[c("mdr", "rr_nmdr")])

  # New cases tested for RR/MDR, including molecular diagnostics
  dst_new <- ifelse(is.na(rdst_new) & is.na(dst_rlt_new) & is.na(xpert_new),
                    NA,
                    ifelse(NZ(rdst_new) > NZ(sum_of_row(rrmdr_c[c("dst_rlt_new","xpert_new")])),
                           rdst_new,
                           sum_of_row(rrmdr_c[c("dst_rlt_new","xpert_new")]))
                    )

  # previously treated cases tested for RR/MDR, including molecular diagnostics
  dst_ret <- ifelse(is.na(rdst_ret) & is.na(dst_rlt_ret) & is.na(xpert_ret),
                    NA,
                    ifelse(NZ(rdst_ret) > NZ(sum_of_row(rrmdr_c[c("dst_rlt_ret","xpert_ret")])),
                           rdst_ret,
                           sum_of_row(rrmdr_c[c("dst_rlt_ret","xpert_ret")]))
                    )
 })

# Get rid of unnecessary variables
rrmdr_c <- subset(rrmdr_c, select=c("iso3", "country", "year", "g_whoregion", "mdr", "rrmdr", "dst_new", "dst_ret", "new_bact_pos", "c_ret" ))


# Add estimates

rrmdr_c <- merge(rrmdr_c, subset(emdrn, year==notification_maxyear, select=c("year", "iso3", "e_mdr_num", "e_mdr_num_lo", "e_mdr_num_hi", "e_new_mdr_num", "e_new_mdr_num_lo", "e_new_mdr_num_hi", "e_ret_mdr_num", "e_ret_mdr_num_lo", "e_ret_mdr_num_hi")), all.x=TRUE)

rrmdr_c$type <- 'countries'
names(rrmdr_c)[names(rrmdr_c)=="country"] <- "group_name"

# Assemble aggregates

# Note from Hazim: Cannot do a simple aggregate for estimates.
# Take results of Babis's modelling (data frame emdra)

# First, aggregate the notifications and dst results (from rrmdr_c)

rrmdr_agg_r <- aggregate(rrmdr_c[5:10], by=list(iso3=rrmdr_c$g_whoregion, year=rrmdr_c$year), FUN='sum', na.rm=T)

rrmdr_agg_g <- aggregate(rrmdr_c[5:10], by=list(year=rrmdr_c$year), FUN='sum', na.rm=T)
rrmdr_agg_g$iso3 <- 'global'

# Combine aggregates
rrmdr_agg <- rbind(rrmdr_agg_r, rrmdr_agg_g)
rrmdr_agg$type <- "aggs"
rrmdr_agg$group_name <- rrmdr_agg$iso3
rrmdr_agg$g_whoregion <- rrmdr_agg$group_name

# Next, combine with aggregated estimates

rrmdr_agg <- merge(rrmdr_agg, subset(emdra, year==notification_maxyear, select=c("year", "group_name", "e_mdr_num", "e_mdr_num_lo", "e_mdr_num_hi",  "e_new_mdr_num", "e_new_mdr_num_lo", "e_new_mdr_num_hi", "e_ret_mdr_num", "e_ret_mdr_num_lo", "e_ret_mdr_num_hi")), all.x=TRUE)

# combine country and aggregates and reorder
rrmdr <- rbind(rrmdr_agg, rrmdr_c)
rrmdr <- properorder(rrmdr)
rrmdr$order <- 1:nrow(rrmdr)


# create calculated variables for output
# Need to use new_bact_pos as the denominator for % testing in new cases!

rrmdr <- within(rrmdr, {

  dst.new.pct <- ifelse(is.na(dst_new) | NZ(new_bact_pos) == 0, "", frmt(dst_new / NZ(new_bact_pos) * 100));

  dst.ret.pct <- ifelse(is.na(dst_ret) | NZ(c_ret) == 0, "", frmt(dst_ret / c_ret * 100))
})
# I don't understand that warning



# Format the large numeric columns (see explanation in table 3 notifications for this line of code)
numeric_vars <- c("rrmdr", "mdr",  "dst_new", "dst_ret")

rrmdr[ , numeric_vars] <- sapply(rrmdr[,numeric_vars], rounder)



# format estimates columns
rrmdr$e.m <- ifelse(rrmdr$year!=notification_maxyear, NA, ifelse(is.na(rrmdr$e_mdr_num), '–', frmt(rrmdr$e_mdr_num)))

rrmdr$e.m.i <- ifelse(rrmdr$year!=notification_maxyear | rrmdr$e.m=='–', NA, paste0('(', frmt(rrmdr$e_mdr_num_lo), '–', frmt(rrmdr$e_mdr_num_hi), ')'))

rrmdr$e.m.new <- ifelse(rrmdr$year!=notification_maxyear, NA, ifelse(is.na(rrmdr$e_new_mdr_num), '–', frmt(rrmdr$e_new_mdr_num)))

rrmdr$e.m.new.i <- ifelse(rrmdr$year!=notification_maxyear | rrmdr$e.m.new=='–', NA, paste0('(', frmt(rrmdr$e_new_mdr_num_lo), '–', frmt(rrmdr$e_new_mdr_num_hi), ')'))

rrmdr$e.m.ret <- ifelse(rrmdr$year!=notification_maxyear, NA, ifelse(is.na(rrmdr$e_ret_mdr_num), '–', frmt(rrmdr$e_ret_mdr_num)))

rrmdr$e.m.ret.i <- ifelse(rrmdr$year!=notification_maxyear | rrmdr$e.m.ret=='–', NA, paste0('(', frmt(rrmdr$e_ret_mdr_num_lo), '–', frmt(rrmdr$e_ret_mdr_num_hi), ')'))


# add conditional formatting column
rrmdr <- ddply(rrmdr, "group_name", add_formatting)

# Add this for blank columns
rrmdr$blank <- NA

# re-order and export
rrmdr <- subset(rrmdr, select=c("group_name", "year", "blank", "rrmdr", "blank", "mdr", "blank", "e.m", "e.m.i", "blank", "e.m.new", "e.m.new.i", "dst_new", "blank", "dst.new.pct", "blank", "blank", "e.m.ret", "e.m.ret.i", "dst_ret", "blank", "dst.ret.pct", "blank", "blank", "blank","format", "g_whoregion", "iso3", "type", "order", "blank"))

rrmdr <- rrmdr[order(rrmdr$order),]

write.csv(rrmdr[rrmdr$type=="aggs",], file="rrmdr_agg.csv", row.names=FALSE, na="")
write.csv(rrmdr[rrmdr$type=="countries",], file="rrmdr_c.csv", row.names=FALSE, na="")

# and now clear up the mess left behind
rm(list=c("rrmdr", "rrmdr_agg", "rrmdr_agg_g", "rrmdr_agg_r", "rrmdr_c"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   agesex  (was Table 8) -----
#   New and relapse case notification by age and sex
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get country data

agesex_c <- subset(n, year %in% 1995:notification_maxyear, select=c("iso3", "country", "year", "g_whoregion",
   "new_sp_m014", "new_sp_m1524", "new_sp_m2534", "new_sp_m3544", "new_sp_m4554", "new_sp_m5564", "new_sp_m65", "new_sp_mu",
   "new_sp_f014", "new_sp_f1524", "new_sp_f2534", "new_sp_f3544", "new_sp_f4554", "new_sp_f5564", "new_sp_f65", "new_sp_fu",
   "new_sn_m014", "new_sn_m15plus", "new_sn_mu",
   "new_sn_f014", "new_sn_f15plus", "new_sn_fu",
   "new_sn_sexunk014", "new_sn_sexunk15plus", "new_sn_sexunkageunk",
   "new_ep_m014", "new_ep_m15plus", "new_ep_mu",
   "new_ep_f014", "new_ep_f15plus", "new_ep_fu",
   "new_ep_sexunk014", "new_ep_sexunk15plus", "new_ep_sexunkageunk",
   "newrel_m014", "newrel_m15plus", "newrel_mu",
   "newrel_f014", "newrel_f15plus", "newrel_fu",
   "newrel_sexunk014", "newrel_sexunk15plus", "newrel_sexunkageunk",
   "rel_in_agesex_flg"))

agesex_c$type <- "countries"
names(agesex_c)[names(agesex_c)=="country"] <- "group_name"

# Assemble aggregates
agesex_agg_r <- aggregate(agesex_c[5:47], by=list(iso3=agesex_c$g_whoregion, year=agesex_c$year), FUN='sum', na.rm=T)

agesex_agg_g <- aggregate(agesex_c[5:47], by=list(year=agesex_c$year), FUN='sum', na.rm=T)
agesex_agg_g$iso3 <- 'global'

# Combine aggregates
agesex_agg <- rbind(agesex_agg_r, agesex_agg_g)
agesex_agg$type <- "aggs"
agesex_agg$group_name <- agesex_agg$iso3
agesex_agg$g_whoregion <- agesex_agg$group_name

# Calculate and format output variables for aggregates
agesex_agg <- within(agesex_agg, {

  # Use older variables for years before 2013
  # variables are mutually exclusive so this works:
  new_m014 <- new_sp_m014 + new_sn_m014 + new_ep_m014 + newrel_m014
  new_f014 <- new_sp_f014 + new_sn_f014 + new_ep_f014 + newrel_f014
  new_unk014  <- new_sn_sexunk014 + new_ep_sexunk014 + newrel_sexunk014

  new_m15plus <- new_sp_m1524 + new_sp_m2534 + new_sp_m3544 + new_sp_m4554 + new_sp_m5564 + new_sp_m65 +
                 new_sn_m15plus + new_ep_m15plus + newrel_m15plus
  new_f15plus <- new_sp_f1524 + new_sp_f2534 + new_sp_f3544 + new_sp_f4554 + new_sp_f5564 + new_sp_f65 +
                 new_sn_f15plus + new_ep_f15plus + newrel_f15plus
  new_unk15plus <- new_sn_sexunk15plus + new_ep_sexunk15plus + newrel_sexunk15plus

   # Age unknowns
  new_munk <- new_sp_mu + new_sn_mu + new_ep_mu + newrel_mu
  new_funk <- new_sp_fu + new_sn_fu + new_ep_fu + newrel_fu
  new_unkunk <- new_sn_sexunkageunk + new_ep_sexunkageunk + newrel_sexunkageunk

  # Male:Female ratio
  mf_ratio <- frmt( (new_m014 + new_m15plus + new_munk) / (new_f014 + new_f15plus + new_funk) )

  # % aged under 15
  u15_pct <- frmt( (new_m014 + new_f014 + new_unk014) * 100
                   /
                   (new_m014 + new_f014 + new_unk014 + new_m15plus + new_f15plus + new_unk15plus  )   )

  # Change any zero-sized numbers to NA for the sex unknown variables since they were only introduced in 2013 data collection round, and also for the age unknowns.
  new_unk014  <- ifelse(new_unk014==0,NA,new_unk014)
  new_unk15plus  <- ifelse(new_unk15plus==0,NA,new_unk15plus)
  new_unkunk  <- ifelse(new_unkunk==0,NA,new_unkunk)
  new_munk <- ifelse(new_munk==0,NA,new_munk)
  new_funk <- ifelse(new_funk==0,NA,new_funk)

  blank <- NA
  rel_flg <- NA
})

#Format aggregate numbers for output (see explanation under table 3 for explanation)
t8_numeric_vars <- c("new_m014", "new_f014",  "new_unk014", "new_m15plus", "new_f15plus", "new_unk15plus",
                     "new_munk", "new_funk", "new_unkunk")
agesex_agg[ , t8_numeric_vars] <- sapply(agesex_agg[,t8_numeric_vars], rounder)


agesex_agg <- properorder(agesex_agg)
agesex_agg$order <- 1:nrow(agesex_agg)
agesex_agg <- ddply(agesex_agg, "group_name", add_formatting)



# Calculate and format output variables for country tables
agesex_c <- within(agesex_c, {

  # Use older variables for years before 2013
  # variables are mutually exclusive so this works:
  new_m014 <- sum_of_row(agesex_c[c("new_sp_m014", "new_sn_m014", "new_ep_m014", "newrel_m014")])
  new_f014 <- sum_of_row(agesex_c[c("new_sp_f014", "new_sn_f014", "new_ep_f014", "newrel_f014")])
  new_unk014  <- sum_of_row(agesex_c[c("new_sn_sexunk014", "new_ep_sexunk014", "newrel_sexunk014")])

  new_m15plus <- sum_of_row(agesex_c[c("new_sp_m1524", "new_sp_m2534", "new_sp_m3544", "new_sp_m4554", "new_sp_m5564", "new_sp_m65",
                                     "new_sn_m15plus", "new_ep_m15plus", "newrel_m15plus")])
  new_f15plus <- sum_of_row(agesex_c[c("new_sp_f1524", "new_sp_f2534", "new_sp_f3544", "new_sp_f4554", "new_sp_f5564", "new_sp_f65",
                                     "new_sn_f15plus", "new_ep_f15plus", "newrel_f15plus")])
  new_unk15plus <- sum_of_row(agesex_c[c("new_sn_sexunk15plus", "new_ep_sexunk15plus", "newrel_sexunk15plus")])

  # Age unknowns
  new_munk <- sum_of_row(agesex_c[c("new_sp_mu", "new_sn_mu", "new_ep_mu", "newrel_mu")])
  new_funk <- sum_of_row(agesex_c[c("new_sp_fu", "new_sn_fu", "new_ep_fu", "newrel_fu")])
  new_unkunk <- sum_of_row(agesex_c[c("new_sn_sexunkageunk", "new_ep_sexunkageunk", "newrel_sexunkageunk")])


  blank <- NA
  rel_flg <- ifelse(NZ(rel_in_agesex_flg)==1, "*",NA)

})

# Calculate Male:Female ratio (for some reason it doesn't like this in the within() part above)
agesex_c$mf_ratio <- ifelse(  NZ(sum_of_row(agesex_c[c("new_f014", "new_f15plus", "new_funk")])) > 0 &
                       NZ(sum_of_row(agesex_c[c("new_m014", "new_m15plus", "new_munk")])) > 0,
                     frmt( sum_of_row(agesex_c[c("new_m014", "new_m15plus", "new_munk")]) /
                             sum_of_row(agesex_c[c("new_f014", "new_f15plus", "new_funk")]) ),
                     NA )

# Calculate % aged under 15
agesex_c$u15_pct <- ifelse( NZ(sum_of_row(agesex_c[c("new_m014", "new_f014", "new_unk014",
                                                   "new_m15plus", "new_f15plus", "new_unk15plus")])) > 0,
                            frmt( sum_of_row(agesex_c[c("new_m014", "new_f014", "new_unk014")]) * 100
                                 /
                                sum_of_row(agesex_c[c("new_m014", "new_f014", "new_unk014",
                                                    "new_m15plus", "new_f15plus", "new_unk15plus")])
                                ),
                            NA )

#Format numbers for output (see explanation under table 3 for explanation)
t8_numeric_vars <- c("new_m014", "new_f014",  "new_unk014", "new_m15plus", "new_f15plus", "new_unk15plus",
                     "new_munk", "new_funk", "new_unkunk")
agesex_c[ , t8_numeric_vars] <- sapply(agesex_c[,t8_numeric_vars], rounder)

agesex_c <- properorder(agesex_c)
agesex_c$order <- 1:nrow(agesex_c)
agesex_c <- ddply(agesex_c, "group_name", add_formatting)

# Then output to CSV (no reason to combine the two into one table
t8_output_vars = c("group_name", "year", "rel_flg", "new_m014", "blank", "new_m15plus", "blank", "new_munk", "blank", "new_f014", "blank", "new_f15plus", "blank", "new_funk", "blank", "new_unk014", "blank", "new_unk15plus", "blank", "new_unkunk", "blank", "u15_pct", "blank", "mf_ratio", "blank",  "format", "g_whoregion", "iso3", "type", "order")


agesex_agg <- subset(agesex_agg, select=t8_output_vars)
agesex_agg <- agesex_agg[order(agesex_agg$order),]
write.csv(agesex_agg, file="agesex_agg.csv", row.names=FALSE, na="")

agesex_c <- subset(agesex_c, select=t8_output_vars)
agesex_c <- agesex_c[order(agesex_c$order),]
write.csv(agesex_c, file="agesex_c.csv", row.names=FALSE, na="")


# and now clear up the mess left behind
rm(list=c("agesex_agg", "agesex_agg_g", "agesex_agg_r", "agesex_c", "t8_numeric_vars", "t8_output_vars"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   labs (was Table 9) -----
#   Laboratories, NTP services, drug management, human resources and infection control
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get country data

labs_c <- subset(s, year == notification_maxyear, select=c("iso3", "country", "year", "g_whoregion", "c_lab_sm_100k", "lab_sm_f", "lab_sm_led", "c_lab_cul_5m", "c_lab_dst_5m", "c_lab_lpa_5m", "lab_xpert", "sldst_avail_incntry", "sldst_avail_outcntry", "nrl", "free_dx", "free_fld_ntp", "hcw_tot", "hcw_tb_infected"))

labs_c$type <- 'countries'
names(labs_c)[names(labs_c)=="country"] <- "group_name"

labs_c <- labs_c[order(labs_c$g_whoregion, labs_c$group_name),]
labs_c$order <- 1:nrow(labs_c)

# Create calculated data columns

labs_c$lab_sm_led_pct <- ifelse(is.na(labs_c$lab_sm_led) | is.na(labs_c$lab_sm_f), '–', rounder(labs_c$lab_sm_led / labs_c$lab_sm_f * 100))

# NB, when calculating TB notification rate among healthcasre workers, also
# ignore records where numerator = denominator!
labs_c$hcw_100k <- ifelse(is.na(labs_c$hcw_tb_infected) | is.na(labs_c$hcw_tot) | labs_c$hcw_tot==0 | labs_c$hcw_tb_infected == labs_c$hcw_tot , "", rounder(labs_c$hcw_tb_infected / labs_c$hcw_tot * 100000))

labs_c$sldst <- ifelse(labs_c$sldst_avail_incntry==1 & labs_c$sldst_avail_outcntry==1, 'In and out of country', ifelse(labs_c$sldst_avail_incntry==1, 'In country', ifelse(labs_c$sldst_avail_outcntry==1, 'Out of country', ifelse(labs_c$sldst_avail_incntry==0 & labs_c$sldst_avail_outcntry==0, 'No', NA))))

# turn category variables into factors (now using datacodes table and not using Tom's rather convoluted factorize()
# function (in fun.r) which relied on well-formatted code lists in the datadicitonary table.


for(var in c("nrl","free_fld_ntp", "free_dx")){
  labs_c[[var]] <- factor(labs_c[[var]], levels=datacodes$option_id, labels=datacodes$optiontext_EN)
}

# add conditional formatting column
t9lineser <- function(x){
  rows <- nrow(x)
  base <- floor(rows/3)
  if(rows - base*3 > 1) base <- base + 1

  temp <- 0
  x[rows,"format"] <- "P"
  for(ro in 1:(base-1)){
    temp <- temp + 3
    x[(ro * 3),"format"] <- "L"
  }
  return(x)
}

labs_c <- ddply(labs_c, "g_whoregion", t9lineser)


# Add for blank columns
labs_c$blank <- NA

# wrap long countries (this needs separate wrapping as each country only takes one line.)

text_con <- textConnection('iso3  n1 n2 n3
                           COD  "Democratic Republic"  "of the Congo"  ""
                           BOL	"Bolivia"	"(Plurinational State of)"	""
                           BES	"Bonaire, Saint Eustatius"	"and Saba"	""
                           VCT	"Saint Vincent and"	"the Grenadines"	""
                           VEN	"Venezuela"	"(Bolivarian Republic of)"	""
                           MKD	"The Former Yugoslav"	"Republic of Macedonia"	""
                           PRK	"Democratic People\'s"	"Republic of Korea"	""
                           LAO	"Lao People\'s Democratic"	"Republic"	""
                           FSM	"Micronesia"	"(Federated States of)"	""
                           PSE  "West Bank and"	"Gaza Strip"	""')

t9_countries <- read.table(text_con, header = TRUE, as.is = TRUE)
close(text_con)

labs_c <- merge(labs_c, t9_countries, all.x=TRUE)

# add in formulas Excel can recognize for wrapping text
labs_c$group_name <- ifelse(labs_c$iso3 %in% t9_countries$iso3, paste0('=\"', labs_c$n1, '\"&CHAR(10)&\"     ', labs_c$n2, '\"'), labs_c$group_name)

# Only the UK has 3 lines, so they are exceptional
labs_c[labs_c$iso3=='GBR', 'group_name'] <- paste('=\"United Kingdom of Great\"', 'CHAR(10)', '\"     Britain and Northern\"', 'CHAR(10)', '\"     Ireland\"', sep="&")

# re-order and export
t9select <- c("group_name", "blank", "c_lab_sm_100k", "blank", "lab_sm_led_pct", "blank", "c_lab_cul_5m", "blank", "c_lab_dst_5m", "blank", "c_lab_lpa_5m", "blank", "lab_xpert", "blank","sldst", "nrl", "blank", "blank", "free_dx", "blank", "free_fld_ntp", "blank", "blank", "blank", "hcw_100k", "blank", "blank", "blank", "format", "g_whoregion", "iso3", "type", "order")
labs_c <- subset(labs_c, select=t9select)

labs_c <- labs_c[order(labs_c$order),]

write.csv(labs_c[labs_c$type=="countries",], file="labs_c.csv", row.names=FALSE, na="")

# and now clear up the mess left behind
rm(list=c("labs_c", "t9_countries", "t9lineser"))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   mdr_est (was Table 10) -----
#   Source of MDR-TB estimates (added 2013); code based on copy of code
#   for table 9 (labs, strategy etc)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data

mdr_est_c <- subset(emdr, year == notification_maxyear, select=c("iso3", "country", "year", "g_whoregion", "source_mdr_new", "source_drs_year_new", "source_drs_coverage_new","e_new_mdr_pcnt","e_new_mdr_pcnt_lo", "e_new_mdr_pcnt_hi", "source_mdr_ret", "source_drs_year_ret", "source_drs_coverage_ret", "e_ret_mdr_pcnt", "e_ret_mdr_pcnt_lo", "e_ret_mdr_pcnt_hi" ))


mdr_est_c$type <- 'countries'
names(mdr_est_c)[names(mdr_est_c)=="country"] <- "group_name"

# re-order by region, country
mdr_est_c <- mdr_est_c[order(mdr_est_c$g_whoregion, mdr_est_c$group_name),]
#add an order column
mdr_est_c$order <- 1:nrow(mdr_est_c)

# add conditional formatting column (same as for table 9)

t10lineser <- function(x){
  rows <- nrow(x)
  base <- floor(rows/3)
  if(rows - base*3 > 1) base <- base + 1

  temp <- 0
  x[rows,"format"] <- "P"
  for(ro in 1:(base-1)){
    temp <- temp + 3
    x[(ro * 3),"format"] <- "L"
  }
  return(x)
}

mdr_est_c<- ddply(mdr_est_c, "g_whoregion", t10lineser)

# Add this for blank columns
mdr_est_c$blank <- NA

# wrap long countries (this needs separate wrapping as each country only takes one line.)
text_con <- textConnection('iso3  n1 n2 n3
                           COD  "Democratic Republic"  "of the Congo"  ""
                           BOL  "Bolivia"	"(Plurinational State of)"	""
                           BES	"Bonaire, Saint Eustatius"	"and Saba"	""
                           VCT	"Saint Vincent and"	"the Grenadines"	""
                           VEN	"Venezuela"	"(Bolivarian Republic of)"	""
                           MKD	"The Former Yugoslav"	"Republic of Macedonia"	""
                           PRK	"Democratic People\'s"	"Republic of Korea"	""
                           LAO	"Lao People\'s Democratic"	"Republic"	""
                           FSM	"Micronesia"	"(Federated States of)"	""
                           PSE  "West Bank and"	"Gaza Strip"	""')

t10_countries <- read.table(text_con, header = TRUE, as.is = TRUE)
close(text_con)

mdr_est_c <- merge(mdr_est_c, t10_countries, all.x=TRUE)

# add in formulas Excel can recognize for wrapping text
mdr_est_c$group_name <- ifelse(mdr_est_c$iso3 %in% t10_countries$iso3, paste0('=\"', mdr_est_c$n1, '\"&CHAR(10)&\"     ', mdr_est_c$n2, '\"'), mdr_est_c$group_name)

# Only the UK has 3 lines, so they are exceptional
mdr_est_c[mdr_est_c$iso3=='GBR', 'group_name'] <- paste('=\"United Kingdom of Great\"', 'CHAR(10)', '\"     Britain and Northern\"', 'CHAR(10)', '\"     Ireland\"', sep="&")

# concatenate confidence interval variables into bracketed strings
mdr_est_c$e_new_mdr_pct_lo_hi <- frmt_intervals(mdr_est_c$e_new_mdr_pcnt,
												mdr_est_c$e_new_mdr_pcnt_lo,
												mdr_est_c$e_new_mdr_pcnt_hi, rates=TRUE)

mdr_est_c$e_ret_mdr_pct_lo_hi <- frmt_intervals(mdr_est_c$e_ret_mdr_pcnt,
												mdr_est_c$e_ret_mdr_pcnt_lo,
												mdr_est_c$e_ret_mdr_pcnt_hi, rates=TRUE)

# blank out source, estimates etc if source is not survey or surveillance
mdr_est_c[mdr_est_c$source_mdr_new != 'Surveillance' & mdr_est_c$source_mdr_new != 'Survey',c('source_mdr_new','source_drs_year_new','source_drs_coverage_new', 'e_new_mdr_pcnt','e_new_mdr_pct_lo_hi')] <- NA

mdr_est_c[mdr_est_c$source_mdr_ret != 'Surveillance' & mdr_est_c$source_mdr_ret != 'Survey',c('source_mdr_ret','source_drs_year_ret','source_drs_coverage_ret', 'e_ret_mdr_pcnt','e_ret_mdr_pct_lo_hi')] <- NA


# restrict to export fields
mdr_est_c <- subset(mdr_est_c, select=c("group_name", "blank", "source_drs_year_new", "blank", "source_mdr_new", "blank", "source_drs_coverage_new", "blank", "e_new_mdr_pcnt", "blank", "e_new_mdr_pct_lo_hi", "blank", "source_drs_year_ret", "blank", "source_mdr_ret", "blank", "source_drs_coverage_ret", "blank", "e_ret_mdr_pcnt", "blank", "e_ret_mdr_pct_lo_hi", "blank", "blank", "blank","blank", "format", "g_whoregion", "iso3", "type", "order"))

# re-order
mdr_est_c <- mdr_est_c[order(mdr_est_c$order),]

write.csv(mdr_est_c[mdr_est_c$type=="countries",], file="mdr_est_c.csv", row.names=FALSE, na="")

# and now clear up the mess left behind
rm(list=c("mdr_est_c","t10_countries","t10lineser"))


