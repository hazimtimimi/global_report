# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to produce CSV files to be imported into the WHO Global Health Observatory
#
# Hazim Timimi, October 2015 (dplyr'ed version of script written last year!)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!! Change in 2016 so only data for WHO-member states are included in the file sent to GHO   !!!!!!!







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



# This is needed to avoid scientific notation output. No idea what it actally means -- it must get the prize for the most confusing documentation. Ever.

options("scipen"=20)

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


# create lookup table to change our database regional and world bank group codes to the ones used by GHO
gho_group_codes <- read.table(textConnection("
group_name,location_code
global,GLOBAL
AFR,AFR
AMR,AMR
EMR,EMR
EUR,EUR
SEA,SEAR
WPR,WPR
LIC,WB_LI
LMC,WB_LMI
UMC,WB_UMI
HIC,WB_HI"), header=TRUE, sep=",", as.is = TRUE)
closeAllConnections()



# Create output folder (only if it doesn't yet exist), and move to it
dir.create(gho_folder, showWarnings = FALSE, recursive = TRUE)
setwd(gho_folder)



# B: OK, NOW GO FOR IT ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library("dplyr")
library("tidyr")


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

# Calculate % using numerator and denominator, format the output and cap at 100%
cap_frmt_pct <- function(numerator, denominator) {

  pct <- ifelse(is.na(numerator) | NZ(denominator) == 0, "",
                ifelse((numerator * 100 / denominator) > 100, ">100", frmt(numerator * 100 / denominator)))

  return(pct)
}



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# stop("OK, see what we have!")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Estimates (country-level and aggregates) -----
#   Estimates of TB mortality, prevalence and incidence, and MDR among notified
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Combine country-level and aggregate epi estimates because subsequent operations
# are identical

est_country <- filter(e, year >= 1990) %>%
                select(iso3, year,
                       e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_num_hi,
                       e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
                       e_prev_num, e_prev_num_lo, e_prev_num_hi,
                       e_prev_100k, e_prev_100k_lo, e_prev_100k_hi,
                       e_inc_num, e_inc_num_lo, e_inc_num_hi,
                       e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                       e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
                       e_inc_tbhiv_num, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi,
                       c_cdr, c_cdr_lo, c_cdr_hi) %>%
                rename(location_code = iso3)

est_agg <- filter(a, year >= 1990 & group_type %in% c("global", "g_whoregion", "g_income")) %>%
            select(group_name, year,
                   e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_num_hi,
                   e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
                   e_prev_num, e_prev_num_lo, e_prev_num_hi,
                   e_prev_100k, e_prev_100k_lo, e_prev_100k_hi,
                   e_inc_num, e_inc_num_lo, e_inc_num_hi,
                   e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                   e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
                   e_inc_tbhiv_num, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi,
                   c_cdr, c_cdr_lo, c_cdr_hi) %>%
            # convert to GHO group codes
            inner_join(gho_group_codes, by = "group_name") %>%
            # drop the group_name variable
            select(-group_name)

# Combine the two estimates tables
est <- rbind(est_country, est_agg)
rm(est_country, est_agg)

# Combine country-level and aggregate MDR in notified estimates because subsequent operations
# are identical

est_mdr_country <- filter(emdrn, year == notification_maxyear) %>%
                    select(iso3, year,
                           e_mdr_num, e_mdr_num_lo, e_mdr_num_hi) %>%
                    rename(location_code = iso3)

est_mdr_agg <- filter(emdra, year == notification_maxyear) %>%
                select(group_name, year,
                       e_mdr_num, e_mdr_num_lo, e_mdr_num_hi) %>%
                # convert to GHO group codes
                inner_join(gho_group_codes, by = "group_name") %>%
                # drop the group_name variable
                select(-group_name)

# Combine the two estimates tables
est_mdr <- rbind(est_mdr_country, est_mdr_agg)
rm(est_mdr_country, est_mdr_agg)


# Begin the processing now on the combined country and aggregate estimates

# Get best estimates
est_c_best <- select(est,
                     location_code, year,
                     e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_100k,
                     e_prev_num, e_prev_100k,
                     e_inc_num, e_inc_100k,
                     e_inc_tbhiv_100k, e_inc_tbhiv_num,
                     c_cdr)

est_c_best <- select(est_mdr, location_code, year,e_mdr_num) %>%
              right_join(est_c_best, by = c("location_code", "year"))


# rename variables to GHO versions
est_c_best <- rename(est_c_best,
                     time_period = year,
                     MDG_0000000020 = e_inc_100k,
                     TB_e_inc_num = e_inc_num,
                     MDG_0000000017 = e_mort_exc_tbhiv_100k,
                     TB_e_mort_exc_tbhiv_num = e_mort_exc_tbhiv_num,
                     MDG_0000000023 = e_prev_100k,
                     TB_e_prev_num = e_prev_num,
                     TB_e_inc_tbhiv_100k = e_inc_tbhiv_100k,
                     TB_e_inc_tbhiv_num = e_inc_tbhiv_num,
                     TB_1 = c_cdr,
                     TB_e_mdr_num = e_mdr_num)

# melt into long table needed by GHO.
# The key/value fields are called "indicator_code" and "value". Exclude location_code and time_period from the melting, so they appear in the final output to provide the reference key (syntax is a little bit confusing ....)

est_c_best <- gather(est_c_best, key="indicator_code", value="value", -location_code, -time_period)


# Now need to add the uncertainty intervals, have to process each one separately

est_c_lo <- select(est,
                   location_code, year,
                   e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_100k_lo,
                   e_prev_num_lo, e_prev_100k_lo,
                   e_inc_num_lo, e_inc_100k_lo,
                   e_inc_tbhiv_100k_lo, e_inc_tbhiv_num_lo,
                   c_cdr_lo)

est_c_lo <- select(est_mdr,
                   location_code, year,e_mdr_num_lo) %>%
            right_join(est_c_lo, by = c("location_code", "year"))


# rename variables to GHO versions
est_c_lo <- rename(est_c_lo,
                     time_period = year,
                     MDG_0000000020 = e_inc_100k_lo,
                     TB_e_inc_num = e_inc_num_lo,
                     MDG_0000000017 = e_mort_exc_tbhiv_100k_lo,
                     TB_e_mort_exc_tbhiv_num = e_mort_exc_tbhiv_num_lo,
                     MDG_0000000023 = e_prev_100k_lo,
                     TB_e_prev_num = e_prev_num_lo,
                     TB_e_inc_tbhiv_100k = e_inc_tbhiv_100k_lo,
                     TB_e_inc_tbhiv_num = e_inc_tbhiv_num_lo,
                     TB_1 = c_cdr_lo,
                     TB_e_mdr_num = e_mdr_num_lo)


# melt into long table needed by GHO.
est_c_lo <- gather(est_c_lo, key="indicator_code", value="low", -location_code, -time_period)


est_c_hi <- select(est,
                   location_code, year,
                   e_mort_exc_tbhiv_num_hi, e_mort_exc_tbhiv_100k_hi,
                   e_prev_num_hi, e_prev_100k_hi,
                   e_inc_num_hi, e_inc_100k_hi,
                   e_inc_tbhiv_100k_hi, e_inc_tbhiv_num_hi,
                   c_cdr_hi)

est_c_hi <- select(est_mdr, location_code, year,e_mdr_num_hi) %>%
            right_join(est_c_hi, by = c("location_code", "year"))


# rename variables to GHO versions
est_c_hi <- rename(est_c_hi,
                   time_period = year,
                   MDG_0000000020 = e_inc_100k_hi,
                   TB_e_inc_num = e_inc_num_hi,
                   MDG_0000000017 = e_mort_exc_tbhiv_100k_hi,
                   TB_e_mort_exc_tbhiv_num = e_mort_exc_tbhiv_num_hi,
                   MDG_0000000023 = e_prev_100k_hi,
                   TB_e_prev_num = e_prev_num_hi,
                   TB_e_inc_tbhiv_100k = e_inc_tbhiv_100k_hi,
                   TB_e_inc_tbhiv_num = e_inc_tbhiv_num_hi,
                   TB_1 = c_cdr_hi,
                   TB_e_mdr_num = e_mdr_num_hi)


# melt into long table needed by GHO.
est_c_hi <- gather(est_c_hi, key="indicator_code", value="high", -location_code, -time_period)


# Join the three data sets into GHO format  and use as a starter for a GHO mega dataframe
# (location_code, time_period, indicator_code, value, low, high)


gho <- full_join(est_c_best, est_c_lo, by = c("location_code", "time_period", "indicator_code")) %>%
          full_join(est_c_hi, by = c("location_code", "time_period", "indicator_code"))

#clean up
rm(list=c("est_c_best",  "est_c_lo", "est_c_hi", "est", "est_mdr"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Notifications -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get data
notif <- filter(n, year >= 1990) %>%
          select(iso3, year, g_whoregion, g_income,
                 c_newinc,
                 new_labconf, new_sp, new_clindx, new_sn, new_su, new_ep, new_oth,
                 ret_rel_labconf, ret_rel_clindx, ret_rel_ep, ret_rel,
                 ret_nrel, ret_taf, ret_tad, ret_oth, newret_oth)


# Calculate stuff
notif <- within(notif, {

  # Calculate total new smear-neg, smear unknown, others for years prior to 2013
  new_snsuoth <- ifelse(year < 2013, sum_of_row(notif[c("new_sn", "new_su","new_oth")]) , NA )

  # Calculate total retreatment cases excluding relapses for years prior to 2013
  ret_nrel <- ifelse(year < 2013, sum_of_row(notif[c("ret_taf", "ret_tad", "ret_oth")]) , ret_nrel )

  # For years before 2013, only show new_labconf for EUR countries (others weren't reporting consistently)
  new_labconf <- ifelse(year < 2013 & g_whoregion != "EUR", NA, new_labconf)

  # For EUR, only show new_labconf if >= new_sp ! (wasn't true in early years collection)
  new_labconf <- ifelse(year < 2013 & new_labconf < new_sp, NA, new_labconf)

  # Clean up rows where sum_of_row has returned NaN
  new_snsuoth <- ifelse(is.na(new_snsuoth),NA,new_snsuoth)
  ret_nrel <- ifelse(is.na(ret_nrel),NA,ret_nrel)

})

# Drop the uneeded variables
notif <- select(notif,
                -new_sn, -new_su, -new_oth,
                -ret_taf, -ret_tad, -ret_oth)


# Calculate aggregates
# 1. WHO regions
notif_agg_r <- notif %>%
                group_by(g_whoregion, year) %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               -iso3, -g_income) %>%
                ungroup() %>%
                rename(group_name = g_whoregion )

# 2. WB Income groups
notif_agg_i <- notif %>%
                group_by(g_income, year) %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               -iso3, -g_whoregion) %>%
                ungroup() %>%
                rename(group_name = g_income )

# 3. Global aggregates
notif_agg_g <- notif %>%
              group_by(year) %>%
              summarise_each(funs(sum(., na.rm = TRUE)),
                             -iso3, -g_whoregion, -g_income) %>%
              ungroup() %>%
              mutate(group_name = "global" )


# Combine aggregates
notif_agg <- rbind(notif_agg_r, notif_agg_i, notif_agg_g)
rm(list=c("notif_agg_r",  "notif_agg_i", "notif_agg_g"))

# Note that I want the sum to be NA if AND ONLY IF, all values are NA, but could not see an easy way to do this
# because with na.rm=TRUE I get a 0 for the sum. However I think all aggregates should be > 0 therefore easiest
# thing to do is to assume all zeros in the resulting aggregates should be NA

notif_agg[3:14] <- sapply(notif_agg[3:14], function(x){ ifelse(x==0,NA,x)})

# convert to GHO group codes
notif_agg <- inner_join(notif_agg, gho_group_codes, by = "group_name") %>%
              # drop the group_name variable
              select(-group_name)

# get rid of g_whoregion and g_income from notif
notif <- select(notif, -g_whoregion, -g_income)

# rename variables to GHO versions
notif <- rename(notif,
                location_code = iso3)

# combine country and aggregates
notif <- rbind(notif_agg, notif)

# Add TB_ to the start of all column names (easy way to get the GHO variable names)
names(notif) <- paste("TB_", names(notif), sep="")

# Then undo renaming of the two index fields
notif <- rename(notif,
                location_code = TB_location_code,
                time_period = TB_year)


# melt into long table needed by GHO
notif <- gather(notif, key="indicator_code", value="value", -location_code, -time_period)


# add empty columns for low and high
notif$low <- NA
notif$high <- NA

#combine into the mega GHO dataframe
gho <- rbind(gho, notif )

#clean up
rm(list=c("notif", "notif_agg"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Laboratories -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

labs <- s %>%
        select(iso3, year, c_lab_sm_100k, c_lab_cul_5m, c_lab_dst_5m,sldst_avail) %>%
        filter(year >= 1990)

# decode dst availability field
labs <- datacodes %>%
        select(option_id, optiontext_EN) %>%
        right_join(labs, datacodes, by = c("option_id" = "sldst_avail")) %>%
        select(-option_id)

# Add TB_ to the start of all column names (easy way to get the GHO variable names)
names(labs) <- paste("TB_", names(labs), sep="")

# rename variables to GHO versions
labs <- rename(labs,
               location_code = TB_iso3,
               time_period = TB_year,
               TB_sldst_avail = TB_optiontext_EN)

# melt into long table needed by GHO
labs <- gather(labs, key="indicator_code", value="value", -location_code, -time_period)

# add empty columns for low and high
labs$low <- NA
labs$high <- NA

#combine into the mega GHO dataframe
gho <- rbind(gho, labs)

#clean up
rm(labs)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   hiv_test  -----
#   HIV testing and provision of CPT, ART and IPT
#  Note that dataset tbhiv already has rules built in for combining _p and _f
#	(final and provisional) numbers, plus adjusted variables for calculating aggregates.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
hiv_test <- tbhiv %>%
            select(year, g_whoregion, g_income, c_notified, iso3, starts_with("hiv")) %>%
            filter(year >= 2003)


# Calculate the aggregates, using the massaged variables *_pct_numerator and *_pct_denominator
# Regional aggregates
hiv_test_agg_r <- hiv_test %>%
                  group_by(g_whoregion, year) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 contains("_pct_"), c_notified) %>%
                  ungroup() %>%
                  rename(group_name = g_whoregion )

# Income group aggregates
hiv_test_agg_i <- hiv_test %>%
                  group_by(g_income, year) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 contains("_pct_"), c_notified) %>%
                  ungroup() %>%
                  rename(group_name = g_income )

# Global aggregate
hiv_test_agg_g <- hiv_test %>%
                  group_by(year) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 contains("_pct_"), c_notified) %>%
                  ungroup() %>%
                  mutate(group_name = "global" )


# Combine aggregates
hiv_test_agg <- rbind(hiv_test_agg_r, rbind(hiv_test_agg_i,hiv_test_agg_g))
rm(list=c("hiv_test_agg_r",  "hiv_test_agg_i", "hiv_test_agg_g"))


# Note that I want the sum to be NA if AND ONLY IF, all values are NA, but could not see an easy way to do this
# because with na.rm=TRUE I get a 0 for the sum. However I think all aggregates should be > 0 therefore easiest
# thing to do is to assume all zeros in the resulting aggregates should be NA

hiv_test_agg[3:11] <- sapply(hiv_test_agg[3:11], function(x){ ifelse(x==0,NA,x)})

# convert to GHO group codes
hiv_test_agg <- inner_join(hiv_test_agg, gho_group_codes, by = "group_name") %>%
              # drop the group_name variable
              select(-group_name)


# Calculate the output variables for aggregates
hiv_test_agg <- within(hiv_test_agg, {
  hivtest_pct <- frmt(hivtest_pct_numerator * 100 / hivtest_pct_denominator )
  hivtest_pos_pct <- frmt(hivtest_pos_pct_numerator * 100/ hivtest_pos_pct_denominator )
  hiv_cpt_pct <- frmt(hiv_cpt_pct_numerator * 100/ hiv_cpt_pct_denominator )
  hiv_art_pct <- frmt(hiv_art_pct_numerator * 100/ hiv_art_pct_denominator )
})


# Restrict to the variables needed
hiv_test_agg <- hiv_test_agg  %>%
                rename(time_period = year) %>%
                select(time_period, location_code, c_notified, hivtest_pct, hivtest_pos_pct, hiv_cpt_pct, hiv_art_pct)



# Calculate and format the output variables for countries
hiv_test <- within(hiv_test, {
  hivtest_pct <- ifelse(c_notified==0,NA,frmt(hivtest * 100 / c_notified ))
  hivtest_pos_pct <- ifelse(hivtest==0,NA,frmt(hivtest_pos * 100/ hivtest ))
  hiv_cpt_pct <- ifelse(hivtest_pos==0,NA,frmt(hiv_cpt * 100/ hivtest_pos ))
  hiv_art_pct <- ifelse(hivtest_pos==0,NA,frmt(hiv_art * 100/ hivtest_pos ))

  # TEMPORARY POLITICAL SOLUTION FOR RUSSIAN FEDERATION 2010 onwards:
  # DO NOT CALCULATE % tb PATIENTS WITH KNOWN HIV STATUS
  # Enable or disable using flag in section A right at the top of the script.

  if (isTRUE(russianfudge)) {
    hivtest_pct <- ifelse(iso3=="RUS" & year>2009,NA,hivtest_pct)
    hivtest_pos_pct <- ifelse(iso3=="RUS" & year>2009,NA,hivtest_pos_pct)
  }
})

# rename variables to GHO versions
hiv_test <- hiv_test  %>%
            rename(location_code = iso3,
                   time_period = year) %>%
            #restrict to variables needed
            select(time_period, location_code, c_notified, hivtest_pct, hivtest_pos_pct, hiv_cpt_pct, hiv_art_pct)

# combine country and aggregates
hiv_test <- rbind(hiv_test_agg, hiv_test)
rm(hiv_test_agg)

# Change variable names to GHO versions
hiv_test <- hiv_test  %>%
            rename(TB_c_notified = c_notified,
                   TB_hivtest_pct = hivtest_pct,
                   TB_hivtest_pos_pct = hivtest_pos_pct,
                   TB_hiv_cpt_pct = hiv_cpt_pct,
                   TB_hiv_art_pct = hiv_art_pct)

# melt into long table needed by GHO
hiv_test <- gather(hiv_test, key="indicator_code", value="value", -location_code, -time_period)

# add empty columns for low and high
hiv_test$low <- NA
hiv_test$high <- NA

#combine into the mega GHO dataframe
gho <- rbind(hiv_test, gho )


#clean up
rm(hiv_test)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   rrmdr -----
#   Testing for MDR-TB and number of confirmed cases of MDR-TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get country data
# A. Notifications
dst_rrmdr_country <- n %>%
                    select(iso3, year, g_whoregion, g_income,
                           c_rrmdr,
                           rdst_new, new_labconf, new_sp, new_bc,
                           rdst_ret, c_ret,
                           conf_mdr_tx, unconf_mdr_tx, conf_rrmdr_tx, unconf_rrmdr_tx) %>%
                    filter( year >= 2005)

# B. Combine with routine surveillance
dst_rrmdr_country <- d %>%
                      select(iso3, year,
                             dst_rlt_new, xpert_new,
                             dst_rlt_ret, xpert_ret) %>%
                      inner_join(dst_rrmdr_country, by = c("iso3", "year"))


# Calculate new pulmonary bacteriologically-confirmed patients, total number tested and total number started on treatment
dst_rrmdr_country <- within(dst_rrmdr_country, {

  # Number of new pulmonary bacteriologically-confirmed patients
  # (this is a bit of a fudge for EUR countries before 2013)

  new_pulm_bac_conf <- new_labconf #default value

  new_pulm_bac_conf <- ifelse( (!is.na(new_pulm_bac_conf) & !is.na(new_sp) & new_pulm_bac_conf < new_sp) |
                                 (is.na(new_pulm_bac_conf) & !is.na(new_sp)),
                          new_sp,
                          new_pulm_bac_conf) # for countries pre-2013

  new_pulm_bac_conf <- ifelse(!is.na(new_bc), new_bc, new_pulm_bac_conf)   # for EUR contries pre-2013


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

  # Number started on treatment (confirmed or unconfirmed) -- note that variables changed in 2015 data collection year
  # but old ones dropped so they are mutually exclusive and can add them all up
  c_rrmdr_tx <- sum_of_row(dst_rrmdr_country[c("conf_mdr_tx", "unconf_mdr_tx", "conf_rrmdr_tx","unconf_rrmdr_tx")])
})


# Drop the variables used to derive new_pulm_bac_conf, dst_new, dst_ret and c_rrmdr_tx
dst_rrmdr_country <- dst_rrmdr_country  %>%
                      select(iso3, year, g_whoregion, g_income,
                             dst_new, new_pulm_bac_conf,
                             dst_ret, c_ret,
                             c_rrmdr,
                             c_rrmdr_tx)


# Calculate the aggregates

# A. WHO regions
dst_rrmdr_region <- dst_rrmdr_country %>%
                    group_by(g_whoregion, year) %>%
                    summarise_each(funs(sum(., na.rm = TRUE)),
                                   dst_new, new_pulm_bac_conf,
                                   dst_ret, c_ret,
                                   c_rrmdr,
                                   c_rrmdr_tx) %>%
                    ungroup() %>%
                    rename(group_name = g_whoregion )

# B. World Bank Income Groups
dst_rrmdr_income <- dst_rrmdr_country %>%
                    group_by(g_income, year) %>%
                    summarise_each(funs(sum(., na.rm = TRUE)),
                                   dst_new, new_pulm_bac_conf,
                                   dst_ret, c_ret,
                                   c_rrmdr,
                                   c_rrmdr_tx) %>%
                    ungroup() %>%
                    rename(group_name = g_income )

# C. Global aggregate
dst_rrmdr_global <- dst_rrmdr_country %>%
                    group_by(year) %>%
                    summarise_each(funs(sum(., na.rm = TRUE)),
                                   dst_new, new_pulm_bac_conf,
                                   dst_ret, c_ret,
                                   c_rrmdr,
                                   c_rrmdr_tx) %>%
                    ungroup() %>%
                    mutate(group_name = "global" )

# Combine aggregates
dst_rrmdr_agg <- rbind(dst_rrmdr_region, dst_rrmdr_income, dst_rrmdr_global)
rm(list=c("dst_rrmdr_region",  "dst_rrmdr_income", "dst_rrmdr_global"))

# convert to GHO group codes
dst_rrmdr_agg <- inner_join(dst_rrmdr_agg, gho_group_codes, by = "group_name") %>%
                  # drop the group_name variable
                  select(-group_name)


# get rid of g_whoregion and g_income from dst_rrmdr_country
dst_rrmdr_country <- dst_rrmdr_country  %>%
                      select(-g_whoregion, -g_income) %>%
                      # rename variables to GHO versions
                      rename(location_code = iso3)

# combine country and aggregates
dst_rrmdr <- rbind(dst_rrmdr_country, dst_rrmdr_agg)
rm(list=c("dst_rrmdr_country","dst_rrmdr_agg"))


# Calculate testing percentages -- cap at 100%

dst_rrmdr <- within(dst_rrmdr, {

  # % of new pulmonary lab-confirmed cases tested
  TB_c_dst_rlt_new_pct <- cap_frmt_pct(dst_new, new_pulm_bac_conf)

  # % of previously treated cases tested
  TB_c_dst_rlt_ret_pct <- cap_frmt_pct(dst_ret, c_ret)
})


# Remove unneeded variables and change remaining variable names to GHO versions
dst_rrmdr <- dst_rrmdr %>%
              select(-dst_new, -new_pulm_bac_conf, -dst_ret, -c_ret) %>%
              rename(time_period = year,
                     TB_rr_mdr = c_rrmdr,
                     TB_c_mdr_tx = c_rrmdr_tx)



# melt into long table needed by GHO
dst_rrmdr <- gather(dst_rrmdr, key="indicator_code", value="value", -location_code, -time_period)

# add empty columns for low and high
dst_rrmdr$low <- NA
dst_rrmdr$high <- NA

#combine into the mega GHO dataframe
gho <- rbind(dst_rrmdr, gho)

#clean up
rm(dst_rrmdr)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Treatment outcomes -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
outcome_country <- o %>%
                    select(iso3, year, g_whoregion, g_income,
                           new_sp_coh, new_sp_cur, new_sp_cmplt,
                           new_snep_coh, new_snep_cmplt,
                           newrel_coh, newrel_succ,
                           ret_coh, ret_cur, ret_cmplt,
                           ret_nrel_coh, ret_nrel_succ,
                           hiv_new_sp_coh, hiv_new_sp_cur, hiv_new_sp_cmplt,
                           hiv_new_snep_coh, hiv_new_snep_cmplt,
                           hiv_ret_coh, hiv_ret_cur, hiv_ret_cmplt,
                           tbhiv_coh, tbhiv_succ,
                           mdr_coh, mdr_succ,
                           xdr_coh, xdr_succ,
                           c_new_tsr, c_ret_tsr,
                           c_tbhiv_tsr, c_mdr_tsr, c_xdr_tsr) %>%
                    filter(year >= 1995)


# Calculate aggregates

# A. WHO regions
outcome_region <- outcome_country %>%
                  group_by(g_whoregion, year) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 starts_with("new"), starts_with("ret"),
                                 starts_with("hiv"), starts_with("tbhiv"),
                                 starts_with("mdr"), starts_with("xdr")) %>%
                  ungroup() %>%
                  rename(group_name = g_whoregion)

# B. World Bank Income Groups
outcome_income <- outcome_country %>%
                  group_by(g_income, year) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 starts_with("new"), starts_with("ret"),
                                 starts_with("hiv"), starts_with("tbhiv"),
                                 starts_with("mdr"), starts_with("xdr")) %>%
                  ungroup() %>%
                  rename(group_name = g_income)

# C. Global aggregate
outcome_global <- outcome_country %>%
                  group_by(year) %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 starts_with("new"), starts_with("ret"),
                                 starts_with("hiv"), starts_with("tbhiv"),
                                 starts_with("mdr"), starts_with("xdr")) %>%
                  ungroup() %>%
                  mutate(group_name = "global" )


# Combine aggregates
outcome_agg <- rbind(outcome_region, outcome_income, outcome_global)
rm(list=c("outcome_region",  "outcome_income", "outcome_global"))

# Calculate treatment success rates for aggregates
outcome_agg <- within(outcome_agg, {

  # New or new+relapse (mutually exclusive variables, so this works)
  c_new_tsr <- round( (new_sp_cur + new_sp_cmplt + new_snep_cmplt + newrel_succ) * 100 /
                          (new_sp_coh + new_snep_coh + newrel_coh) )

  # Retreatment or retreatment excluding relapse (mutually exclusive variables, so this works)
  c_ret_tsr <- round( (ret_cur + ret_cmplt + ret_nrel_succ) * 100 /
                          (ret_coh + ret_nrel_coh) )

  # HIV-positive, all cases (mutually exclusive variables, so this works)
  c_tbhiv_tsr <- ifelse((hiv_new_sp_coh + hiv_new_snep_coh + hiv_ret_coh + tbhiv_coh) > 0,
                        round( (hiv_new_sp_cur + hiv_new_sp_cmplt +
                                hiv_new_snep_cmplt +
                                hiv_ret_cur + hiv_ret_cmplt +
                                tbhiv_succ) * 100 /
                            (hiv_new_sp_coh + hiv_new_snep_coh + hiv_ret_coh + tbhiv_coh) ),
                        NA)

  # MDR and XDR (variables already harmonised in the view)
  c_mdr_tsr <- ifelse( mdr_coh > 0, round( mdr_succ * 100 / mdr_coh), NA)
  c_xdr_tsr <- ifelse( xdr_coh > 0, round( xdr_succ * 100 / xdr_coh), NA)

})

# convert to GHO group codes
outcome_agg <- inner_join(outcome_agg, gho_group_codes, by = "group_name") %>%
                # drop the group_name variable
                select(-group_name)


# get rid of g_whoregion and g_income from  outcome_country
outcome_country <- outcome_country  %>%
                    select(-g_whoregion, -g_income) %>%
                    # rename variables to GHO versions
                    rename(location_code = iso3)

# combine country and aggregates
outcome <- rbind(outcome_country, outcome_agg)
rm(list=c("outcome_country","outcome_agg"))


# Remove unneeded variables and change remaining variable names to GHO versions

outcome <- outcome %>%
            select(location_code, year,
                   c_new_tsr, c_ret_tsr, c_tbhiv_tsr,
                   c_mdr_tsr, c_xdr_tsr) %>%
            rename(time_period = year,
                   TB_c_new_tsr = c_new_tsr,
                   TB_c_ret_tsr = c_ret_tsr,
                   TB_c_tbhiv_tsr = c_tbhiv_tsr,
                   TB_c_mdr_tsr = c_mdr_tsr,
                   TB_c_xdr_tsr = c_xdr_tsr)


# melt into long table needed by GHO
outcome <- gather(outcome, key="indicator_code", value="value", -location_code, -time_period)


# add empty columns for low and high
outcome$low <- NA
outcome$high <- NA

#combine into the mega GHO dataframe
gho <- rbind(outcome, gho)

#clean up
rm(outcome)


# Export the GHO mega df to CSV
write.csv(gho, file=paste("GHO_TB_update_",Sys.Date(),".csv",sep="") , row.names=FALSE, na="")
