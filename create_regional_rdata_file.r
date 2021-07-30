# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load views from the global TB database, filter them so that they are
# restricted to countries and territories in a specific WHO region
# and save them all in a single dated .Rdata file.
#
# Hazim Timimi, last updated July 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get DB connection string and path where to save the data (variable rdata_folder in set_environment.r)
source("set_environment.r")

# Set the region code to use ----
g_whoregion <- "WPR"

# Three simple functions to create the filtered records using SQL depending on how to identify the region or region's countries in the view

sql_group <- function(view_name, region_code){
   return(paste0("SELECT * FROM ", view_name, " WHERE group_name = '", region_code, "';" ))
}

sql_region <- function(view_name, region_code){
   return(paste0("SELECT * FROM ", view_name, " WHERE g_whoregion = '", region_code, "';" ))
}

sql_region_countries <- function(view_name, region_code){
   return(paste0("SELECT * FROM ", view_name, " WHERE iso2 IN ",
                 "(SELECT iso2 FROM view_TME_master_report_country WHERE g_whoregion = '", region_code, "');" ))
}


# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(RODBC)
library("dplyr")


# Get ODBC connection string from the environment file (if not already loaded)
if (exists("connection_string")==FALSE) {
   source("set_environment.r")
}

# load the views from the database, filtered by region
ch <- odbcDriverConnect(connection_string)

# load views into dataframes

aggregated_estimates_agesex           <- sqlQuery(ch, sql_group("estimates.view_aggregated_estimates", g_whoregion))
aggregated_estimates_agesex_rawvalues <- sqlQuery(ch, sql_group("estimates.view_aggregated_estimates_rawvalues", g_whoregion))
aggregated_estimates_epi              <- sqlQuery(ch, sql_group("view_TME_aggregated_estimates_epi", g_whoregion))
aggregated_estimates_epi_rawvalues    <- sqlQuery(ch, sql_group("view_TME_aggregated_estimates_epi_rawvalues", g_whoregion))
aggregated_estimates_drtb             <- sqlQuery(ch, sql_group("view_TME_aggregated_estimates_drtb", g_whoregion))
aggregated_estimates_drtb_rawvalues   <- sqlQuery(ch, sql_group("view_TME_aggregated_estimates_drtb_rawvalues", g_whoregion))
aggregated_finance_estimates          <- sqlQuery(ch, sql_group("reports.view_TME_aggregated_finance_estimates", g_whoregion))

budget_expenditure                 <- sqlQuery(ch, sql_region("view_TME_master_budget_expenditure", g_whoregion))
covid_unhlm                        <- sqlQuery(ch, sql_region("view_TME_master_covid_unhlm", g_whoregion))
data_collection                    <- sqlQuery(ch, sql_region("view_TME_master_data_collection", g_whoregion))
dr_surveillance                    <- sqlQuery(ch, sql_region("view_TME_master_dr_surveillance", g_whoregion))
drs                                <- sqlQuery(ch, sql_region("view_TME_master_drs", g_whoregion))
drs_for_estimation_new             <- sqlQuery(ch, sql_region("view_DRS_for_estimation_new", g_whoregion))
drs_for_estimation_ret             <- sqlQuery(ch, sql_region("view_DRS_for_estimation_ret", g_whoregion))
drs_for_estimation_sldst           <- sqlQuery(ch, sql_region("view_DRS_for_estimation_sldst", g_whoregion))
drs_most_recent_for_estimation     <- sqlQuery(ch, sql_region("view_DRS_most_recent_for_estimation", g_whoregion))
drs_most_recent_for_estimation_inh <- sqlQuery(ch, sql_region("view_DRS_most_recent_for_estimation_INH", g_whoregion))

estimates_agesex            <- sqlQuery(ch, sql_region("estimates.view_estimates", g_whoregion))
estimates_agesex_rawvalues  <- sqlQuery(ch, sql_region("estimates.view_estimates_rawvalues", g_whoregion))
estimates_epi               <- sqlQuery(ch, sql_region("view_TME_estimates_epi", g_whoregion))
estimates_epi_rawvalues     <- sqlQuery(ch, sql_region("view_TME_estimates_epi_rawvalues", g_whoregion))
estimates_drtb              <- sqlQuery(ch, sql_region("view_TME_estimates_drtb", g_whoregion))
estimates_drtb_rawvalues    <- sqlQuery(ch, sql_region("view_TME_estimates_drtb_rawvalues", g_whoregion))
estimates_ltbi              <- sqlQuery(ch, sql_region("view_TME_estimates_ltbi", g_whoregion))
estimates_population        <- sqlQuery(ch, sql_region("view_TME_estimates_population", g_whoregion))
estimates_population_5yr    <- sqlQuery(ch, sql_region("view_TME_estimates_population_5yr", g_whoregion))

finance                     <- sqlQuery(ch, sql_region("view_TME_master_finance", g_whoregion))
finance_cleaned             <- sqlQuery(ch, sql_region("view_TME_master_finance_cleaned", g_whoregion))
notification                <- sqlQuery(ch, sql_region("view_TME_master_notification", g_whoregion))
notification_exceptions     <- sqlQuery(ch, sql_region("view_TME_master_notification_exceptions", g_whoregion))
outcomes                    <- sqlQuery(ch, sql_region("view_TME_master_outcomes", g_whoregion))
report_country              <- sqlQuery(ch, sql_region("view_TME_master_report_country", g_whoregion))
strategy                    <- sqlQuery(ch, sql_region("view_TME_master_strategy", g_whoregion))
TBHIV_for_aggregates        <- sqlQuery(ch, sql_region("view_TME_master_TBHIV_for_aggregates", g_whoregion))

catastrophic_costs_survey      <- sqlQuery(ch, sql_region_countries("survey.view_catastrophic_costs_survey", g_whoregion))
country_group_membership       <- sqlQuery(ch, sql_region_countries("view_country_group_membership", g_whoregion))
dr_derived_variables           <- sqlQuery(ch, sql_region_countries("view_dr_derived_variables", g_whoregion))

prevalence_survey              <- sqlQuery(ch, sql_region_countries("survey.view_prevalence_survey", g_whoregion))
prevalence_survey_cases        <- sqlQuery(ch, sql_region_countries("survey.view_prevalence_survey_cases", g_whoregion))
prevalence_survey_estimates    <- sqlQuery(ch, sql_region_countries("survey.view_prevalence_survey_estimates", g_whoregion))

external_indicator_data        <- sqlQuery(ch, sql_region_countries("external_indicators.view_indicator_data", g_whoregion))
external_indicator_defs        <- sqlQuery(ch, sql_region_countries("external_indicators.view_indicator_definition", g_whoregion))

# And also load reference tables in full
country_group_types     <- sqlFetch(ch, "view_country_group_types")
country_groups          <- sqlFetch(ch, "view_country_groups")

data_codes              <- sqlFetch(ch, "view_TME_data_codes")
data_dictionary         <- sqlFetch(ch, "view_TME_data_dictionary")


close(ch)

# format dr_surveillance to play nicer with the other data.frames
# - Removing records that aren't explicitly covering a whole country
# - Adding in past years so the number of records is identical with notification (with NAs filled in for missing data.)
# - Adding in grouping variables and other metadata (1:6 in notification)

dr_surveillance <- merge(notification[notification$year>=min(dr_surveillance$year), 1:6],
                         dr_surveillance[dr_surveillance$all_areas_covered==1 |
                                            is.na(dr_surveillance$all_areas_covered),],
                         all.x=TRUE)

# Add an internal date'n'time stamp
data.date <- Sys.time()


# set e_pop_num to numeric to avoid integer overflow error
estimates_population$e_pop_num <- as.numeric(estimates_population$e_pop_num)


# Save the data frames
save (
   aggregated_estimates_agesex,
   aggregated_estimates_agesex_rawvalues,
   aggregated_estimates_epi,
   aggregated_estimates_epi_rawvalues,
   aggregated_estimates_drtb,
   aggregated_estimates_drtb_rawvalues,
   aggregated_finance_estimates,

   budget_expenditure,
   covid_unhlm,
   data_collection,
   dr_surveillance,
   drs,
   drs_for_estimation_new,
   drs_for_estimation_ret,
   drs_for_estimation_sldst,
   drs_most_recent_for_estimation,
   drs_most_recent_for_estimation_inh,

   estimates_agesex,
   estimates_agesex_rawvalues,
   estimates_epi,
   estimates_epi_rawvalues,
   estimates_drtb,
   estimates_drtb_rawvalues,
   estimates_ltbi,
   estimates_population,
   estimates_population_5yr,

   finance,
   finance_cleaned,
   notification,
   notification_exceptions,
   outcomes,
   report_country,
   strategy,
   TBHIV_for_aggregates,

   catastrophic_costs_survey,
   country_group_membership,
   dr_derived_variables,

   prevalence_survey,
   prevalence_survey_cases,
   prevalence_survey_estimates,

   external_indicator_data,
   external_indicator_defs,

   country_group_types,
   country_groups,

   data_codes,
   data_dictionary,

   data.date,
   file=paste0(rdata_folder, "data_", g_whoregion, "_", Sys.Date(),".Rdata")
   )


