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
report_year <- 2017

# The following are convenience variables since notification and most other data sets will run up to the
# year before the reporting year and outcomes will run up to two years before the reporting year
notification_maxyear <- (report_year - 1)
outcome_maxyear      <- (report_year - 2)


# Apply the Russian fudge ------
# flag for whether to suppress calculation of %
# of notified TB patients who knew their HIV status (applies to table A4.4)

russianfudge <- FALSE

# Apply the Malawi fudge ------
# flag for whether to calculate % of patients who knew their HIV status using c_notified as the denominator

malawifudge <- FALSE

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


# Create output folder (only if it doesn't yet exist), and move to it
dir.create(file.path(annex4_folder, "linked_CSVs"), recursive = TRUE, showWarnings = FALSE)
setwd(file.path(annex4_folder, "linked_CSVs"))

# B: OK, NOW GO FOR IT ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library("dplyr")


# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(paste0(scripts_folder, "/functions/round_numbers.r"))
source(paste0(scripts_folder, "/functions/handle_NAs.r"))





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
  inc_num <- display_num(e_inc_num / 1000, thousands=TRUE)
  inc_num_lo_hi <- display_intervals(e_inc_num / 1000,
                                  e_inc_num_lo / 1000,
                                  e_inc_num_hi / 1000, thousands=TRUE)

  inc_rate <- display_num(e_inc_100k)
  inc_rate_lo_hi <- display_intervals(e_inc_100k,
                                   e_inc_100k_lo,
                                   e_inc_100k_hi)

  # TB/HIV incidence (convert numbers to thousands)
  inc_tbhiv_num <- display_num(e_inc_tbhiv_num / 1000, thousands=TRUE)
  inc_tbhiv_num_lo_hi <- display_intervals(e_inc_tbhiv_num / 1000,
                                        e_inc_tbhiv_num_lo / 1000,
                                        e_inc_tbhiv_num_hi / 1000, thousands=TRUE)

  inc_tbhiv_rate <- display_num(e_inc_tbhiv_100k)
  inc_tbhiv_rate_lo_hi <- display_intervals(e_inc_tbhiv_100k,
                                         e_inc_tbhiv_100k_lo,
                                         e_inc_tbhiv_100k_hi)

  # MDR/RR-TB incidence (convert numbers to thousands)
  inc_rr_num <- display_num(e_inc_rr_num / 1000, thousands=TRUE)
  inc_rr_num_lo_hi <- display_intervals(e_inc_rr_num / 1000,
                                        e_inc_rr_num_lo / 1000,
                                        e_inc_rr_num_hi / 1000, thousands=TRUE)


  # Need to calculate MDR/RR-TB incidence rates
  inc_rr_rate <- display_num( (e_inc_rr_num * 1e5 / e_pop_num))
  inc_rr_rate_lo_hi <- display_intervals((e_inc_rr_num * 1e5 / e_pop_num),
                                       (e_inc_rr_num_lo * 1e5 / e_pop_num),
                                       (e_inc_rr_num_hi * 1e5 / e_pop_num))

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
  mort_exc_tbhiv_num <- display_num(e_mort_exc_tbhiv_num / 1000, thousands=TRUE)
  mort_exc_tbhiv_num_lo_hi <- display_intervals(e_mort_exc_tbhiv_num / 1000,
                                 e_mort_exc_tbhiv_num_lo / 1000,
                                 e_mort_exc_tbhiv_num_hi / 1000, thousands=TRUE)

  mort_exc_tbhiv_rate <- display_num(e_mort_exc_tbhiv_100k)
  mort_exc_tbhiv_rate_lo_hi <- display_intervals(e_mort_exc_tbhiv_100k,
                                e_mort_exc_tbhiv_100k_lo,
                                e_mort_exc_tbhiv_100k_hi)

  # mortality (HIV-positive TB) (convert numbers to thousands)
  mort_tbhiv_num <- display_num(e_mort_tbhiv_num / 1000, thousands=TRUE)
  mort_tbhiv_num_lo_hi <- display_intervals(e_mort_tbhiv_num / 1000,
                                 e_mort_tbhiv_num_lo / 1000,
                                 e_mort_tbhiv_num_hi / 1000, thousands=TRUE)

  mort_tbhiv_rate <- display_num(e_mort_tbhiv_100k)
  mort_tbhiv_rate_lo_hi <- display_intervals(e_mort_tbhiv_100k,
                                e_mort_tbhiv_100k_lo,
                                e_mort_tbhiv_100k_hi)

  # Mortality (HIV-negative and HIV-positive TB cases) (convert numbers to thousands)
  mort_num <- display_num(e_mort_num / 1000, thousands=TRUE)
  mort_num_lo_hi <- display_intervals(e_mort_num / 1000,
                                 e_mort_num_lo / 1000,
                                 e_mort_num_hi / 1000, thousands=TRUE)

  mort_rate <- display_num(e_mort_100k)
  mort_rate_lo_hi <- display_intervals(e_mort_100k,
                                e_mort_100k_lo,
                                e_mort_100k_hi)

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

mdr_rr_measured <- estimates_drtb %>%
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
mdr_rr_measured[mdr_rr_measured$source_new == "Model",
             c("source_new", "source_drs_year_new", "source_drs_coverage_new",
               "e_rr_pct_new", "e_rr_pct_new_lo", "e_rr_pct_new_hi")] <- NA

mdr_rr_measured[mdr_rr_measured$source_ret == "Model",
             c("source_ret", "source_drs_year_ret", "source_drs_coverage_ret",
               "e_rr_pct_ret", "e_rr_pct_ret_lo", "e_rr_pct_ret_hi")] <- NA

# Format variables for output
mdr_rr_measured <- within(mdr_rr_measured, {

  # concatenate confidence interval variables into bracketed strings
  e_rr_pct_new_lo_hi <- display_intervals(e_rr_pct_new,
                                       e_rr_pct_new_lo,
                                       e_rr_pct_new_hi)

  e_rr_pct_ret_lo_hi <- display_intervals(e_rr_pct_ret,
                                       e_rr_pct_ret_lo,
                                       e_rr_pct_ret_hi)

  # Add for blank columns
  blank <- ""

})


# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(mdr_rr_measured,
       select=c("country", "blank",
                "source_drs_year_new", "blank", "source_new", "blank", "source_drs_coverage_new", "blank",
                "e_rr_pct_new", "blank", "e_rr_pct_new_lo_hi", "blank",
                "source_drs_year_ret", "blank", "source_ret", "blank", "source_drs_coverage_ret", "blank",
                "e_rr_pct_ret", "blank", "e_rr_pct_ret_lo_hi")) %>%
  write.csv(file="mdr_rr_measured.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
rm(mdr_rr_measured)





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
#   outcome_table (Table A4.5) ----
#   Treatment outcomes, all types
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get country data
# A. Standard cohorts
outcome_country <-  outcomes %>%
                    filter(year == outcome_maxyear) %>%
                    select(country,
                           g_whoregion,
                           rel_with_new_flg,
                           newrel_coh, newrel_succ,
                           ret_nrel_coh, ret_nrel_succ,
                           tbhiv_coh, tbhiv_succ)


# B. Combine with MDR and XDR cohorts one year older than standard
outcome_dr_country <-  outcomes %>%
                      filter(year == (outcome_maxyear - 1)) %>%
                      select(country,
                             mdr_coh, mdr_succ,
                             xdr_coh, xdr_succ)

outcome_country <-  outcome_dr_country %>%
                    inner_join(outcome_country, by = "country") %>%
                    rename(entity = country ) %>%
                    arrange(entity)


# Calculate regional aggregates
outcome_region <- outcome_country %>%
                    group_by(g_whoregion) %>%
                    summarise_each(funs(sum(., na.rm = TRUE)),
                                   ends_with("_coh"),
                                   ends_with("_succ")) %>%
                    # add dummy variable to match structure of country table
                    mutate(rel_with_new_flg = NA)

# merge with regional names
outcome_region <-   aggregated_estimates_drtb_rawvalues %>%
                    filter(year == notification_maxyear & group_type == "g_whoregion") %>%
                    select(g_whoregion = group_name,
                           entity = group_description) %>%
                    inner_join(outcome_region, by = "g_whoregion") %>%
                    arrange(g_whoregion)


# Calculate global aggregate
outcome_global <- outcome_country %>%
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 ends_with("_coh"),
                                 ends_with("_succ")) %>%
                  # add dummy variables to match structure of country table
                  mutate(entity = "Global",
                         rel_with_new_flg = NA,
                         g_whoregion = "")


# Create combined table in order of countries then regional and global estimates
outcome_table <- combine_tables(outcome_country, outcome_region, outcome_global)



# Calculate treatment success rates and format variables for output
outcome_table <- within(outcome_table, {

  # New or new+relapse
  c_newrel_tsr <- ifelse( is.na(newrel_coh), NA, rounder( newrel_succ * 100 /newrel_coh ))

  # Retreatment or retreatment excluding relapse
  c_ret_tsr <- ifelse( is.na(ret_nrel_coh), NA, rounder( ret_nrel_succ * 100 / ret_nrel_coh ))

  # HIV-positive, all cases
  c_tbhiv_tsr <- ifelse( is.na(tbhiv_coh), NA, rounder( tbhiv_succ * 100 / tbhiv_coh ))

  # MDR
  c_mdr_tsr <- ifelse( is.na(mdr_coh), NA, rounder( mdr_succ * 100 / mdr_coh))

  # XDR
  c_xdr_tsr <- ifelse( is.na(xdr_coh), NA, rounder( xdr_succ * 100 / xdr_coh))

  # Format the cohort sizes
  newrel_coh <- rounder(newrel_coh)
  ret_nrel_coh <- rounder(ret_nrel_coh)
  tbhiv_coh <- rounder(tbhiv_coh)
  mdr_coh <- rounder(mdr_coh)
  xdr_coh <- rounder(xdr_coh)

  # Flag country name if relapses were not included with new cases
  entity <- ifelse(!is.na(rel_with_new_flg) & rel_with_new_flg==0, paste0(entity,"*"),entity)

  # Add for blank columns
  blank <- ""

})

# Insert "blank" placeholders for use in the output spreadsheet before writing out to CSV
# dplyr's select statement won't repeat the blanks, hence use subset() from base r instead

subset(outcome_table,
       select = c("entity",
                  "newrel_coh", "blank", "c_newrel_tsr", "blank",
                  "ret_nrel_coh", "blank", "c_ret_tsr", "blank",
                  "tbhiv_coh", "blank", "c_tbhiv_tsr", "blank",
                  "mdr_coh", "blank", "c_mdr_tsr", "blank",
                  "xdr_coh", "blank", "c_xdr_tsr"))  %>%
  write.csv(file="outcome_table.csv", row.names=FALSE, na="")

# Don't leave any mess behind!
# Clean up (remove any objects with their name starting with 'outcome_')
rm(list=ls(pattern = "^outcome_"))





