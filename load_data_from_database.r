# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Connect to the global TB database and copy views to a standard set
# of data frames. Each data frame is named according to the underlying
# SQL-Server view, but without the leading "view_TME_" or "view_TME_master_" bits, so
#
#
# notification:       dataframe copy of view_TME_master_notification
# estimates_epi:      dataframe copy of view_TME_estimates_epi
# ... etc.
#
# data.date: When the source datasets were created
#
# Hazim Timimi July 2015, based on a original from Tom
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)

# Get ODBC connection string from the environment file (if not already loaded)
if (exists("connection_string")==FALSE) {
  source("set_environment.r")
}

# connect to the database
library(RODBC)
ch <- odbcDriverConnect(connection_string)

# load views into dataframes
aggregated_estimates_agesex <- sqlFetch(ch, "estimates.view_aggregated_estimates", na.strings = "")
aggregated_estimates_agesex_rawvalues <-  sqlFetch(ch, "estimates.view_aggregated_estimates_rawvalues", na.strings = "")
aggregated_estimates_epi   <- sqlFetch(ch, "view_TME_aggregated_estimates_epi", na.strings = "")
aggregated_estimates_epi_rawvalues   <- sqlFetch(ch, "view_TME_aggregated_estimates_epi_rawvalues", na.strings = "")
aggregated_estimates_drtb  <- sqlFetch(ch, "view_TME_aggregated_estimates_drtb", na.strings = "")
aggregated_estimates_drtb_rawvalues <- sqlFetch(ch, "view_TME_aggregated_estimates_drtb_rawvalues", na.strings = "")

aggregated_finance_estimates <- sqlFetch(ch, "reports.view_TME_aggregated_finance_estimates", na.strings = "")

budget_expenditure         <- sqlFetch(ch, "view_TME_master_budget_expenditure", na.strings = "")

catastrophic_costs_survey  <- sqlFetch(ch, "survey.view_catastrophic_costs_survey", na.strings = "")

country_group_membership   <- sqlFetch(ch, "view_country_group_membership", na.strings = "")
country_group_types        <- sqlFetch(ch, "view_country_group_types", na.strings = "")
country_groups             <- sqlFetch(ch, "view_country_groups", na.strings = "")

data_codes                 <- sqlFetch(ch, "view_TME_data_codes", na.strings = "")
data_collection            <- sqlFetch(ch, "view_TME_master_data_collection", na.strings = "")
data_dictionary            <- sqlFetch(ch, "view_TME_data_dictionary", na.strings = "")

dr_derived_variables       <- sqlFetch(ch, "view_dr_derived_variables", na.strings = "")
dr_surveillance            <- sqlFetch(ch, "view_TME_master_dr_surveillance", na.strings = "")
drs                        <- sqlFetch(ch, "view_TME_master_drs", na.strings = "")

drs_for_estimation_new     <- sqlFetch(ch, "view_DRS_for_estimation_new", na.strings = "")
drs_for_estimation_ret     <- sqlFetch(ch, "view_DRS_for_estimation_ret", na.strings = "")
drs_for_estimation_sldst   <- sqlFetch(ch, "view_DRS_for_estimation_sldst", na.strings = "")

drs_most_recent_for_estimation <- sqlFetch(ch, "view_DRS_most_recent_for_estimation", na.strings = "")
drs_most_recent_for_estimation_inh  <- sqlFetch(ch, "view_DRS_most_recent_for_estimation_INH", na.strings = "")



estimates_agesex           <- sqlFetch(ch, "estimates.view_estimates", na.strings = "")
estimates_agesex_rawvalues <- sqlFetch(ch, "estimates.view_estimates_rawvalues", na.strings = "")
estimates_epi              <- sqlFetch(ch, "view_TME_estimates_epi", na.strings = "")
estimates_epi_rawvalues    <- sqlFetch(ch, "view_TME_estimates_epi_rawvalues", na.strings = "")
estimates_drtb             <- sqlFetch(ch, "view_TME_estimates_drtb", na.strings = "")
estimates_drtb_rawvalues   <- sqlFetch(ch, "view_TME_estimates_drtb_rawvalues", na.strings = "")
estimates_ltbi             <- sqlFetch(ch, "view_TME_estimates_ltbi", na.strings = "")
estimates_population       <- sqlFetch(ch, "view_TME_estimates_population", na.strings = "")
estimates_population_5yr   <- sqlFetch(ch, "view_TME_estimates_population_5yr", na.strings = "")

finance                    <- sqlFetch(ch, "view_TME_master_finance", na.strings = "")
finance_cleaned            <- sqlFetch(ch, "view_TME_master_finance_cleaned", na.strings = "")

notification               <- sqlFetch(ch, "view_TME_master_notification", na.strings = "")
notification_exceptions    <- sqlFetch(ch, "view_TME_master_notification_exceptions", na.strings = "")
outcomes                   <- sqlFetch(ch, "view_TME_master_outcomes", na.strings = "")

prevalence_survey          <- sqlFetch(ch, "survey.view_prevalence_survey", na.strings = "")
prevalence_survey_cases    <- sqlFetch(ch, "survey.view_prevalence_survey_cases", na.strings = "")
prevalence_survey_estimates<- sqlFetch(ch, "survey.view_prevalence_survey_estimates", na.strings = "")

report_country             <- sqlFetch(ch, "view_TME_master_report_country", na.strings = "")
strategy                   <- sqlFetch(ch, "view_TME_master_strategy", na.strings = "")
tpt                        <- sqlFetch(ch, "view_TME_master_contacts_tpt", na.strings = "")
TBHIV_for_aggregates       <- sqlFetch(ch, "view_TME_master_TBHIV_for_aggregates", na.strings = "")

# external data views
external_indicator_data    <- sqlFetch(ch, "external_indicators.view_indicator_data", na.strings = "")
external_indicator_defs    <- sqlFetch(ch, "external_indicators.view_indicator_definition", na.strings = "")

# One-off view for 2020 data collection on impact of COVID-19 on TB services and on
# country reponses to targets set by the 2018 UN High-Level Meeting on TB
covid_unhlm                <- sqlFetch(ch, "view_TME_master_covid_unhlm", na.strings = "")

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


# That's it folks ----

