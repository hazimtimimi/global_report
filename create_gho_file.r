# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to produce CSV files to be imported into the WHO Global Health Observatory
#
# Hazim Timimi, November 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Clear the decks ----
rm(list=ls())



# A: SET A FEW FLAGS AND CONSTANTS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Establish the report year
report_year <- 2018

# The following are convenience variables since notification and most other data sets will run up to the
# year before the reporting year and outcomes will run up to two years before the reporting year
notification_maxyear <- (report_year - 1)
outcome_maxyear      <- (report_year - 2)


# Apply the Russian fudge ------
# Flag for whether to suppress calculation of %
# of notified TB patients who knew their HIV status
# Only for years prior to 2016

russianfudge <- TRUE

# Apply the Malawi fudge ------
# Flag for whether to calculate % of patients who knew their HIV status using c_notified as the denominator
# Only for 2015

malawifudge <- TRUE

# Apply the Swiss fudge ------
# Flag for whether to adjust denominator to calculate % of new cases tested for rifampicin resistance
# because of large number with unknown treatment history
# Only for 2015

swissfudge <- TRUE

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
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


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


# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(paste0(scripts_folder, "/functions/round_numbers.r"), encoding = "UTF-8")
source(paste0(scripts_folder, "/functions/handle_NAs.r"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   g_income -----
#   Recreate the old g_income variable which used to exist prior to 2017 data collection year
#   Note that in 2017, Cook Islands and Niue did not have a g_income assignation, therefor must
#   do a left join when combining this table with notifications, outcomes etc.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

wb_g_income <-  country_group_membership %>%
  filter(group_type == "g_income")  %>%
  select(iso2, group_type, group_name) %>%
  spread(key = group_type, value = group_name, fill = 0)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# stop("OK, see what we have!")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

stop("

     >>>>>>>>>>
     Stopping here so can do the rest manually!
     <<<<<<<<<<<<")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Estimates (country-level and aggregates) -----
#   Estimates of TB mortality and incidence, and MDR/RR among notified
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Combine country-level and aggregate epi estimates because subsequent operations
# are identical

est_country <- estimates_epi %>%
                filter(year >= 2000) %>%
                select(iso3, year,
                       e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_num_hi,
                       e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
                       e_inc_num, e_inc_num_lo, e_inc_num_hi,
                       e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                       e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
                       e_inc_tbhiv_num, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi,
                       c_cdr, c_cdr_lo, c_cdr_hi) %>%
                rename(location_code = iso3)


est_agg <-  aggregated_estimates_epi %>%
            filter(year >= 2000 & group_type %in% c("global", "g_whoregion", "g_income")) %>%
            select(group_name, year,
                   e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_num_hi,
                   e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
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

# Combine country-level and aggregate MDR/RR in notified estimates because subsequent operations
# are identical

est_rr_country <- estimates_drtb %>%
                  filter(year == notification_maxyear) %>%
                    select(iso3, year,
                           e_rr_in_notified_pulm, e_rr_in_notified_pulm_lo, e_rr_in_notified_pulm_hi) %>%
                    rename(location_code = iso3)

est_rr_agg <-  aggregated_estimates_drtb %>%
                filter(year == notification_maxyear) %>%
                select(group_name, year,
                       e_rr_in_notified_pulm, e_rr_in_notified_pulm_lo, e_rr_in_notified_pulm_hi) %>%
                # convert to GHO group codes
                inner_join(gho_group_codes, by = "group_name") %>%
                # drop the group_name variable
                select(-group_name)

# Combine the two estimates tables
est_rr <- rbind(est_rr_country, est_rr_agg)
rm(est_rr_country, est_rr_agg)


# Begin the processing now on the combined country and aggregate estimates

# Get best estimates
est_c_best <- est %>%
              select(location_code, year,
                     e_mort_exc_tbhiv_num, e_mort_exc_tbhiv_100k,
                     e_inc_num, e_inc_100k,
                     e_inc_tbhiv_100k, e_inc_tbhiv_num,
                     c_cdr)

est_c_best <- est_rr %>%
              select(location_code, year, e_rr_in_notified_pulm) %>%
              right_join(est_c_best, by = c("location_code", "year"))


# rename variables to GHO versions
est_c_best <- est_c_best %>%
              rename(time_period = year,
                     MDG_0000000020 = e_inc_100k,
                     TB_e_inc_num = e_inc_num,
                     MDG_0000000017 = e_mort_exc_tbhiv_100k,
                     TB_e_mort_exc_tbhiv_num = e_mort_exc_tbhiv_num,
                     TB_e_inc_tbhiv_100k = e_inc_tbhiv_100k,
                     TB_e_inc_tbhiv_num = e_inc_tbhiv_num,
                     TB_1 = c_cdr,
                     TB_e_rr_in_notified_pulm = e_rr_in_notified_pulm)

# melt into long table needed by GHO.
# The key/value fields are called "indicator_code" and "value". Exclude location_code and time_period from the melting, so they appear in the final output to provide the reference key (syntax is a little bit confusing ....)

est_c_best <- est_c_best %>%
              gather(key="indicator_code",
                     value="value",
                     -location_code,
                     -time_period)


# Now need to add the uncertainty intervals, have to process each one separately

est_c_lo <- est %>%
            select(location_code, year,
                   e_mort_exc_tbhiv_num_lo, e_mort_exc_tbhiv_100k_lo,
                   e_inc_num_lo, e_inc_100k_lo,
                   e_inc_tbhiv_100k_lo, e_inc_tbhiv_num_lo,
                   c_cdr_lo)

est_c_lo <- est_rr %>%
            select(location_code, year, e_rr_in_notified_pulm_lo) %>%
            right_join(est_c_lo, by = c("location_code", "year"))


# rename variables to GHO versions
est_c_lo <- est_c_lo %>%
            rename(time_period = year,
                   MDG_0000000020 = e_inc_100k_lo,
                   TB_e_inc_num = e_inc_num_lo,
                   MDG_0000000017 = e_mort_exc_tbhiv_100k_lo,
                   TB_e_mort_exc_tbhiv_num = e_mort_exc_tbhiv_num_lo,
                   TB_e_inc_tbhiv_100k = e_inc_tbhiv_100k_lo,
                   TB_e_inc_tbhiv_num = e_inc_tbhiv_num_lo,
                   TB_1 = c_cdr_lo,
                   TB_e_rr_in_notified_pulm = e_rr_in_notified_pulm_lo)


# melt into long table needed by GHO.
est_c_lo <- est_c_lo %>%
            gather(key="indicator_code",
                   value="low",
                   -location_code,
                   -time_period)


est_c_hi <- est %>%
            select(location_code, year,
                   e_mort_exc_tbhiv_num_hi, e_mort_exc_tbhiv_100k_hi,
                   e_inc_num_hi, e_inc_100k_hi,
                   e_inc_tbhiv_100k_hi, e_inc_tbhiv_num_hi,
                   c_cdr_hi)

est_c_hi <- est_rr %>%
            select(location_code, year, e_rr_in_notified_pulm_hi) %>%
            right_join(est_c_hi, by = c("location_code", "year"))


# rename variables to GHO versions
est_c_hi <- est_c_hi %>%
            rename(time_period = year,
                   MDG_0000000020 = e_inc_100k_hi,
                   TB_e_inc_num = e_inc_num_hi,
                   MDG_0000000017 = e_mort_exc_tbhiv_100k_hi,
                   TB_e_mort_exc_tbhiv_num = e_mort_exc_tbhiv_num_hi,
                   TB_e_inc_tbhiv_100k = e_inc_tbhiv_100k_hi,
                   TB_e_inc_tbhiv_num = e_inc_tbhiv_num_hi,
                   TB_1 = c_cdr_hi,
                   TB_e_rr_in_notified_pulm = e_rr_in_notified_pulm_hi)


# melt into long table needed by GHO.
est_c_hi <- est_c_hi %>%
            gather(key="indicator_code",
                   value="high",
                   -location_code,
                   -time_period)


# Join the three data sets into GHO format  and use as a starter for a GHO mega dataframe
# (location_code, time_period, indicator_code, value, low, high)


gho <- full_join(est_c_best, est_c_lo, by = c("location_code", "time_period", "indicator_code")) %>%
          full_join(est_c_hi, by = c("location_code", "time_period", "indicator_code"))


# add incidence for children (age 0-14)
est_country_014 <- estimates_agesex %>%
            filter(year == notification_maxyear &
                     measure == "inc" &
                     unit == "num" &
                     age_group == "0-14" &
                     sex == "a") %>%
            select(iso3,
                   year,
                   best, lo, hi) %>%
            mutate(indicator_code = "TB_e_inc_num_014") %>%
            rename(location_code = iso3,
                   time_period = year,
                   value = best,
                   low = lo,
                   high = hi)

est_agg_014 <- aggregated_estimates_agesex %>%
                filter(year == notification_maxyear &
                         measure == "inc" &
                         unit == "num" &
                         age_group == "0-14" &
                         sex == "a")  %>%
                select(group_name,
                       year,
                       best, lo, hi) %>%
                mutate(indicator_code = "TB_e_inc_num_014") %>%
                rename(time_period = year,
                       value = best,
                       low = lo,
                       high = hi) %>%
                # convert to GHO group codes
                inner_join(gho_group_codes, by = "group_name") %>%
                # drop the group_name variable
                select(-group_name)


#combine into the mega GHO dataframe
gho <- rbind(gho, est_country_014, est_agg_014)


#clean up
rm(list=c("est_c_best",  "est_c_lo", "est_c_hi", "est", "est_rr", "est_country_014", "est_agg_014"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Notifications -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Get data
notif <-  notification %>%
          filter(year >= 2000) %>%
          left_join(wb_g_income, by = "iso2") %>%
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
notif <- notif %>%
        select(-new_sn, -new_su, -new_oth,
               -ret_taf, -ret_tad, -ret_oth)


# Calculate aggregates
# 1. WHO regions
notif_agg_r <- notif %>%
                group_by(g_whoregion, year) %>%
                summarise_at(vars(c_newinc:new_snsuoth),
                             sum,
                             na.rm = TRUE) %>%
                ungroup() %>%
                rename(group_name = g_whoregion )

# 2. WB Income groups
notif_agg_i <- notif %>%
                group_by(g_income, year) %>%
                summarise_at(vars(c_newinc:new_snsuoth),
                             sum,
                             na.rm = TRUE) %>%
                ungroup() %>%
                rename(group_name = g_income )

# 3. Global aggregates
notif_agg_g <- notif %>%
              group_by(year) %>%
              summarise_at(vars(c_newinc:new_snsuoth),
                           sum,
                           na.rm = TRUE) %>%
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
notif_agg <-  notif_agg %>%
              inner_join(gho_group_codes, by = "group_name") %>%
              # drop the group_name variable
              select(-group_name)

# get rid of g_whoregion and g_income from notif
notif <- notif %>%
        select(-g_whoregion, -g_income) %>%
        # rename variables to GHO versions
        rename(location_code = iso3)

# combine country and aggregates
notif <- rbind(notif_agg, notif)

# Add TB_ to the start of all column names (easy way to get the GHO variable names)
names(notif) <- paste("TB_", names(notif), sep="")

# Then undo renaming of the two index fields
notif <- notif %>%
         rename(location_code = TB_location_code,
                time_period = TB_year)


# melt into long table needed by GHO
notif <- notif %>%
         gather(key="indicator_code",
                value="value",
                -location_code,
                -time_period)


# add empty columns for low and high
notif$low <- NA
notif$high <- NA

#combine into the mega GHO dataframe
gho <- rbind(gho, notif )

#clean up
rm(list=c("notif", "notif_agg"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   hiv_test  -----
#   HIV testing and provision of ART
#  Note that dataset TBHIV_for_aggregates already has rules built in for combining _p and _f
#	(final and provisional) numbers, plus adjusted variables for calculating aggregates.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
hiv_test <- TBHIV_for_aggregates %>%
            left_join(wb_g_income, by = "iso2") %>%
            select(year, g_whoregion, g_income, iso3, starts_with("hiv"), newrel_hivtest, c_notified) %>%
            filter(year >= 2003)


# Calculate the aggregates, using the massaged variables *_pct_numerator and *_pct_denominator
# Regional aggregates
hiv_test_agg_r <- hiv_test %>%
                  group_by(g_whoregion, year) %>%
                  summarise_at(vars(contains("_pct_")),
                               sum,
                               na.rm = TRUE) %>%
                  ungroup() %>%
                  rename(group_name = g_whoregion )

# Income group aggregates
hiv_test_agg_i <- hiv_test %>%
                  group_by(g_income, year) %>%
                  summarise_at(vars(contains("_pct_")),
                               sum,
                               na.rm = TRUE) %>%
                                  ungroup() %>%
                  rename(group_name = g_income )

# Global aggregate
hiv_test_agg_g <- hiv_test %>%
                  group_by(year) %>%
                  summarise_at(vars(contains("_pct_")),
                               sum,
                               na.rm = TRUE) %>%
                  ungroup() %>%
                  mutate(group_name = "global" )


# Combine aggregates
hiv_test_agg <- rbind(hiv_test_agg_r, rbind(hiv_test_agg_i,hiv_test_agg_g))
rm(list=c("hiv_test_agg_r",  "hiv_test_agg_i", "hiv_test_agg_g"))


# Note that I want the sum to be NA if AND ONLY IF, all values are NA, but could not see an easy way to do this
# because with na.rm=TRUE I get a 0 for the sum. However I think all aggregates should be > 0 therefore easiest
# thing to do is to assume all zeros in the resulting aggregates should be NA

hiv_test_agg[3:10] <- sapply(hiv_test_agg[3:10], function(x){ ifelse(x==0,NA,x)})

# convert to GHO group codes
hiv_test_agg <- inner_join(hiv_test_agg, gho_group_codes, by = "group_name") %>%
              # drop the group_name variable
              select(-group_name)


# Calculate the output variables for aggregates
hiv_test_agg <- within(hiv_test_agg, {
  hivtest_pct <- display_cap_pct(hivtest_pct_numerator, hivtest_pct_denominator )
  hivtest_pos_pct <- display_cap_pct(hivtest_pos_pct_numerator, hivtest_pos_pct_denominator )
  hiv_art_pct <- display_cap_pct(hiv_art_pct_numerator, hiv_art_pct_denominator )
})


# Restrict to the variables needed
hiv_test_agg <- hiv_test_agg  %>%
                rename(time_period = year) %>%
                select(time_period,
                       location_code,
                       hivtest_pct,
                       hivtest_pos_pct,
                       hiv_art_pct)


# Calculate and format the output variables for countries
hiv_test <- within(hiv_test, {
  hivtest_pct <- display_cap_pct(hivtest_pct_numerator, hivtest_pct_denominator )

  hivtest_pos_pct <- display_cap_pct(hivtest_pos_pct_numerator, hivtest_pos_pct_denominator )

  hiv_art_pct <- display_cap_pct(hiv_art_pct_numerator, hiv_art_pct_denominator )

  # TEMPORARY POLITICAL SOLUTION FOR RUSSIAN FEDERATION 2010-2015:
  # DO NOT CALCULATE % tb PATIENTS WITH KNOWN HIV STATUS
  # Enable or disable using flag in section A right at the top of the script.

  if (isTRUE(russianfudge)) {
    hivtest_pct <- ifelse(iso3=="RUS" & between(year, 2010, 2015),
                          NA,
                          hivtest_pct)

    hivtest_pos_pct <- ifelse(iso3=="RUS" & between(year, 2010, 2015),
                              NA,
                              hivtest_pos_pct)
  }

  # TEMPORARY SOLUTION FOR Malawi 2016:
  # Use c_notified instead of c_newinc to calculate % with known HIV status
  # Enable or disable using flag in section A right at the top of the script.

  if (isTRUE(malawifudge)) {

    hivtest_pct <- ifelse(iso3=="MWI" & year == 2015,
                          display_cap_pct(newrel_hivtest, c_notified ),
                          hivtest_pct)

  }

})

# rename variables to GHO versions
hiv_test <- hiv_test  %>%
            rename(location_code = iso3,
                   time_period = year) %>%
            #restrict to variables needed
            select(time_period,
                   location_code,
                   hivtest_pct,
                   hivtest_pos_pct,
                   hiv_art_pct)

# combine country and aggregates
hiv_test <- rbind(hiv_test_agg, hiv_test)
rm(hiv_test_agg)

# Change variable names to GHO versions
hiv_test <- hiv_test  %>%
            rename(TB_hivtest_pct = hivtest_pct,
                   TB_hivtest_pos_pct = hivtest_pos_pct,
                   TB_hiv_art_pct = hiv_art_pct)

# melt into long table needed by GHO
hiv_test <- gather(hiv_test,
                   key="indicator_code",
                   value="value",
                   -location_code,
                   -time_period)

# add empty columns for low and high
hiv_test$low <- NA
hiv_test$high <- NA

#combine into the mega GHO dataframe
gho <- rbind(hiv_test, gho )


#clean up
rm(hiv_test)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   rrmdr -----
#   Testing for MDR/RR-TB and number of confirmed cases of MDR/RR-TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get country data
# A. Notifications
dst_rrmdr_country <- notification %>%
                    left_join(wb_g_income, by = "iso2") %>%
                    inner_join(dr_derived_variables, by = c("iso3", "year")) %>%
                    select(iso3, year, g_whoregion, g_income,
                           c_rrmdr,
                           rdst_new, new_labconf, new_sp, new_bc, c_newunk,
                           rdst_ret, c_ret,
                           conf_mdr_tx, unconf_mdr_tx, conf_rrmdr_tx, unconf_rrmdr_tx) %>%
                    filter( year >= 2005)

# B. Combine with routine surveillance
dst_rrmdr_country <- dr_surveillance %>%
                      select(iso3, year,
                             dst_rlt_new, xpert_new,
                             dst_rlt_ret, xpert_ret) %>%
                      inner_join(dst_rrmdr_country, by = c("iso3", "year"))


# Calculate new pulmonary bacteriologically-confirmed patients, total number tested and total number started on treatment for years before 2015
# For 2015 onwards calculate % testing out of all notified patients

dst_rrmdr_country <- within(dst_rrmdr_country, {

  # Number of new pulmonary bacteriologically-confirmed patients
  # (this is a bit of a fudge for EUR countries before 2013)

  new_pulm_bac_conf <- new_labconf #default value

  new_pulm_bac_conf <- ifelse( (!is.na(new_pulm_bac_conf) & !is.na(new_sp) & new_pulm_bac_conf < new_sp) |
                                 (is.na(new_pulm_bac_conf) & !is.na(new_sp)),
                              new_sp,
                              new_pulm_bac_conf) # for countries pre-2013

  new_pulm_bac_conf <- ifelse(!is.na(new_bc),
                              new_bc,
                              new_pulm_bac_conf)   # for EUR contries pre-2013

  # And here comes the change for 2015 -- use all new notified cases as the denominator
  new_pulm_bac_conf <- ifelse(year >= 2015,
                              c_newunk,
                              new_pulm_bac_conf)


  # TEMPORARY SOLUTION FOR Switzerland 2016 data collection year:
	# Switzerland informed us that out of the 531 'new or treatment history unknown' cases
	# 162 had unknown previous treatment history, therefore denominator for the calculation
	# should be 531 - 162
  # Enable or disable using flag in section A right at the top of the script.

  if (isTRUE(swissfudge)) {

    new_pulm_bac_conf <- ifelse(iso3=="CHE" & year == 2015,
                                c_newunk - 162,
                                new_pulm_bac_conf)

  }


  # New cases tested for RR/MDR, including molecular diagnostics
  dst_new <- ifelse(is.na(rdst_new) & is.na(dst_rlt_new) & is.na(xpert_new),
                    NA,
                    ifelse(NZ(rdst_new) > NZ(sum_of_row(dst_rrmdr_country[c("dst_rlt_new","xpert_new")])),
                           rdst_new,
                           sum_of_row(dst_rrmdr_country[c("dst_rlt_new","xpert_new")]))
  )

  # And here comes the change for 2015 -- much simpler approach
  dst_new <- ifelse(year >= 2015,
                    rdst_new,
                    dst_new)



  # previously treated cases tested for RR/MDR, including molecular diagnostics
  dst_ret <- ifelse(is.na(rdst_ret) & is.na(dst_rlt_ret) & is.na(xpert_ret),
                    NA,
                    ifelse(NZ(rdst_ret) > NZ(sum_of_row(dst_rrmdr_country[c("dst_rlt_ret","xpert_ret")])),
                           rdst_ret,
                           sum_of_row(dst_rrmdr_country[c("dst_rlt_ret","xpert_ret")]))
  )

  # And here comes the change for 2015 -- much simpler approach
  dst_ret <- ifelse(year >= 2015,
                    rdst_ret,
                    dst_ret)


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
                    summarise_at(vars(dst_new, new_pulm_bac_conf,
                                      dst_ret, c_ret,
                                      c_rrmdr,
                                      c_rrmdr_tx),
                                 sum,
                                 na.rm = TRUE) %>%
                    ungroup() %>%
                    rename(group_name = g_whoregion )

# B. World Bank Income Groups
dst_rrmdr_income <- dst_rrmdr_country %>%
                    group_by(g_income, year) %>%
                    summarise_at(vars(dst_new, new_pulm_bac_conf,
                                      dst_ret, c_ret,
                                      c_rrmdr,
                                      c_rrmdr_tx),
                                 sum,
                                 na.rm = TRUE) %>%
                    ungroup() %>%
                    rename(group_name = g_income )

# C. Global aggregate
dst_rrmdr_global <- dst_rrmdr_country %>%
                    group_by(year) %>%
                    summarise_at(vars(dst_new, new_pulm_bac_conf,
                                      dst_ret, c_ret,
                                      c_rrmdr,
                                      c_rrmdr_tx),
                                 sum,
                                 na.rm = TRUE) %>%
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
  TB_c_dst_rlt_new_pct <- display_cap_pct(dst_new, new_pulm_bac_conf)

  # % of previously treated cases tested
  TB_c_dst_rlt_ret_pct <- display_cap_pct(dst_ret, c_ret)
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
outcome_country <-  outcomes %>%
                    left_join(wb_g_income, by = "iso2") %>%
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
                  summarise_at(vars(starts_with("new"), starts_with("ret"),
                                    starts_with("hiv"), starts_with("tbhiv"),
                                    starts_with("mdr"), starts_with("xdr")),
                               sum,
                               na.rm = TRUE) %>%
                  ungroup() %>%
                  rename(group_name = g_whoregion)

# B. World Bank Income Groups
outcome_income <- outcome_country %>%
                  group_by(g_income, year) %>%
                  summarise_at(vars(starts_with("new"), starts_with("ret"),
                                    starts_with("hiv"), starts_with("tbhiv"),
                                    starts_with("mdr"), starts_with("xdr")),
                               sum,
                               na.rm = TRUE) %>%
                  ungroup() %>%
                  rename(group_name = g_income)

# C. Global aggregate
outcome_global <- outcome_country %>%
                  group_by(year) %>%
                  summarise_at(vars(starts_with("new"), starts_with("ret"),
                                    starts_with("hiv"), starts_with("tbhiv"),
                                    starts_with("mdr"), starts_with("xdr")),
                               sum,
                               na.rm = TRUE) %>%
                  ungroup() %>%
                  mutate(group_name = "global" )


# Combine aggregates
outcome_agg <- rbind(outcome_region, outcome_income, outcome_global)
rm(list=c("outcome_region",  "outcome_income", "outcome_global"))

# Calculate treatment success rates for aggregates
outcome_agg <- within(outcome_agg, {

  # New or new+relapse (mutually exclusive variables, so this works)
  c_new_tsr <- display_cap_pct( (new_sp_cur + new_sp_cmplt + new_snep_cmplt + newrel_succ),
                                (new_sp_coh + new_snep_coh + newrel_coh) )

  # Retreatment or retreatment excluding relapse (mutually exclusive variables, so this works)
  c_ret_tsr <- display_cap_pct( (ret_cur + ret_cmplt + ret_nrel_succ),
                                (ret_coh + ret_nrel_coh) )

  # HIV-positive, all cases (mutually exclusive variables, so this works)
  c_tbhiv_tsr <- display_cap_pct( (hiv_new_sp_cur + hiv_new_sp_cmplt +
                                    hiv_new_snep_cmplt +
                                    hiv_ret_cur + hiv_ret_cmplt +
                                    tbhiv_succ),
                                  (hiv_new_sp_coh + hiv_new_snep_coh + hiv_ret_coh + tbhiv_coh) )

  # MDR and XDR (variables already harmonised in the view)
  c_mdr_tsr <- display_cap_pct( mdr_succ, mdr_coh)
  c_xdr_tsr <- display_cap_pct( xdr_succ, xdr_coh)

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


# Change request for 2016 to restrict country data to WHO member states !!!!!!!
# Will not change aggregate data so as to remain consistent with global report

entities_to_exclude <- report_country %>%
                       filter(g_whostatus != "M" | is.na(g_whostatus)) %>%
                       select(location_code = iso3) %>%
                       mutate(exclude = 1)



# Export the GHO mega df to CSV, excluding countries/territories that are not WHO member states

gho %>%
  left_join(entities_to_exclude) %>%
  filter(is.na(exclude)) %>%
  select(-exclude) %>%
  write.csv(file=paste("GHO_TB_update_",Sys.Date(),".csv",sep="") , row.names=FALSE, na="")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Sex-disaggregated incidence estimates -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# World HEalth Statistics 2019 report will analyse indicators by sex, so this is an
# extra file to be used for that

# Need to convert incidence numbers to rates, so calculate population denominators

pop <- estimates_population %>%
        filter(year == notification_maxyear) %>%
        mutate(m = e_pop_m014 + e_pop_m15plus,
               f = e_pop_f014 + e_pop_f15plus,
               age_group = "all") %>%
        select(iso2, year, age_group, m, f) %>%

        # Convert to long format similar to disaggregated incidence estimates

        gather(key = "sex", value = "population", c("m", "f"))


# Link denominators to matching numerators

estimates_sex <- estimates_agesex_rawvalues %>%
                 inner_join(pop) %>%

                 # calculate rates
                 mutate(rate_best = display_num(best * 1e5 / population),
                        rate_lo = display_num(lo * 1e5 /  population),
                        rate_hi = display_num(hi * 1e5 /  population),
                        unit = "rate") %>%

                 # format for output
                 select(country,
                        iso2,
                        iso3,
                        year,
                        measure,
                        unit,
                        age_group,
                        sex,
                        best = rate_best,
                        lo = rate_lo,
                        hi = rate_hi)

# save to csv
write.csv(x = estimates_sex,
          file=paste("TB_incidence_by_sex_",Sys.Date(),".csv",sep="") ,
          row.names=FALSE,
          na="")

# Now repeat the process for aggregated estimates (global and WHO regions)
# Note -- convert population aggregates to numeric to avoid integer overflow
#         in global aggregates.

pop_region <- estimates_population %>%
              filter(year == notification_maxyear) %>%
              mutate(m = as.numeric(e_pop_m014 + e_pop_m15plus),
                     f = as.numeric(e_pop_f014 + e_pop_f15plus)) %>%
              group_by(g_whoregion, year) %>%
              summarise_at(vars(c("m", "f")),
                           sum,
                           na.rm = TRUE) %>%
              ungroup() %>%
              mutate(age_group = "all") %>%
              rename(group_name = g_whoregion)


pop_global <- estimates_population %>%
              filter(year == notification_maxyear) %>%
              mutate(m = as.numeric(e_pop_m014 + e_pop_m15plus),
                     f = as.numeric(e_pop_f014 + e_pop_f15plus)) %>%
              group_by(year) %>%
              summarise_at(vars(c("m", "f")),
                           sum,
                           na.rm = TRUE) %>%
              ungroup() %>%
              mutate(age_group = "all",
                     group_name = "global")

# Combine the aggregates
pop_aggregate <- rbind(pop_region, pop_global)
rm(pop_region, pop_global)

# Convert to long format similar to disaggregated incidence estimates

pop_aggregate <- pop_aggregate %>%
                  gather(key = "sex", value = "population", c("m", "f"))

# Link denominators to matching numerators

estimates_sex_aggregate <- aggregated_estimates_agesex_rawvalues %>%
                            inner_join(pop_aggregate) %>%

                          # calculate rates
                          mutate(rate_best = display_num(best * 1e5 / population),
                                 rate_lo = display_num(lo * 1e5 /  population),
                                 rate_hi = display_num(hi * 1e5 /  population),
                                 unit = "rate") %>%

                          # format for output
                          select(group_type,
                                 group_name,
                                 year,
                                 measure,
                                 unit,
                                 age_group,
                                 sex,
                                 best = rate_best,
                                 lo = rate_lo,
                                 hi = rate_hi)

# save to csv
write.csv(x = estimates_sex_aggregate,
          file=paste("TB_incidence_by_sex_aggregates_",Sys.Date(),".csv",sep="") ,
          row.names=FALSE,
          na="")

