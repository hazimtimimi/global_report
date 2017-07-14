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

.fixnamibia <- function(df){
  # make sure Namibia's iso2 code is not interpreted as R's NA (null)
  df$iso2 <- ifelse(df$country=="Namibia", "NA", as.character(df$iso2))
  return(df)
}


# Get ODBC connection string from the environment file (if not already loaded)
if (exists("connection_string")==FALSE) {
  source("set_environment.r")
}

# connect to the database
library(RODBC)
ch <- odbcDriverConnect(connection_string)

# load views into dataframes
aggregated_estimates_epi   <- sqlFetch(ch, "view_TME_aggregated_estimates_epi")
aggregated_estimates_epi_rawvalues   <- sqlFetch(ch, "view_TME_aggregated_estimates_epi_rawvalues")
aggregated_estimates_mdr_in_notified <- sqlFetch(ch, "view_TME_aggregated_estimates_mdr_in_notified")
aggregated_estimates_drtb  <- sqlFetch(ch, "view_TME_aggregated_estimates_drtb")
aggregated_estimates_drtb_rawvalues <- sqlFetch(ch, "view_TME_aggregated_estimates_drtb_rawvalues")
budget_expenditure         <- .fixnamibia(sqlFetch(ch, "view_TME_master_budget_expenditure"))

country_group_membership   <- .fixnamibia(sqlFetch(ch, "view_country_group_membership"))
country_group_types        <- sqlFetch(ch, "view_country_group_types")
country_groups             <- sqlFetch(ch, "view_country_groups")

data_codes                 <- sqlFetch(ch, "view_TME_data_codes")
data_collection            <- .fixnamibia(sqlFetch(ch, "view_TME_master_data_collection"))
data_dictionary            <- sqlFetch(ch, "view_TME_data_dictionary")

dr_derived_variables       <- .fixnamibia(sqlFetch(ch, "view_dr_derived_variables"))
dr_surveillance            <- .fixnamibia(sqlFetch(ch, "view_TME_master_dr_surveillance"))
drs                        <- .fixnamibia(sqlFetch(ch, "view_TME_master_drs"))

estimates_epi              <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_epi"))
estimates_epi_rawvalues    <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_epi_rawvalues"))
estimates_drtb             <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_drtb"))
estimates_drtb_rawvalues   <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_drtb_rawvalues"))
estimates_mdr              <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_mdr"))
estimates_mdr_in_notified  <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_mdr_in_notified"))
estimates_population       <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_population"))

finance                    <- .fixnamibia(sqlFetch(ch, "view_TME_master_finance"))
notification               <- .fixnamibia(sqlFetch(ch, "view_TME_master_notification"))
notification_exceptions    <- sqlFetch(ch, "view_TME_master_notification_exceptions")
outcomes                   <- .fixnamibia(sqlFetch(ch, "view_TME_master_outcomes"))

prevalence_survey          <- .fixnamibia(sqlFetch(ch, "survey.view_prevalence_survey"))
prevalence_survey_cases    <- .fixnamibia(sqlFetch(ch, "survey.view_prevalence_survey_cases"))
prevalence_survey_estimates<- .fixnamibia(sqlFetch(ch, "survey.view_prevalence_survey_estimates"))

report_country             <- .fixnamibia(sqlFetch(ch, "view_TME_master_report_country"))
strategy                   <- .fixnamibia(sqlFetch(ch, "view_TME_master_strategy"))
TBHIV_for_aggregates       <- .fixnamibia(sqlFetch(ch, "view_TME_master_TBHIV_for_aggregates"))

# external data views
external_indicator_defs    <- sqlFetch(ch, "external_indicators.view_indicator_definition")
external_indicator_data    <- .fixnamibia(sqlFetch(ch, "external_indicators.view_indicator_data"))

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

