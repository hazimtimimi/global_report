# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load views from the global TB database, filter them so that they are
# restricted to countries and territories in the WHO Western Pacific Region (WPR)
# and save them all in a single dated .Rdata file.
#
# Hazim Timimi, October 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Clear the decks ----
rm(list=ls())

# Get DB connection string and path where to save the data (variable rdata_folder in set_environment.r)
source("set_environment.r")

# Load the views directly from the global TB database
source("load_data_from_database.r")

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library("dplyr")

# Get the list of WPR country codes
wpr_iso2 <- country_group_membership %>%
            filter(group_type == "g_whoregion" & group_name == "WPR") %>%
            select(iso2)



# Filter to restrict to WPR ----
aggregated_estimates_epi            <- filter(aggregated_estimates_epi, group_name == "WPR")
aggregated_estimates_epi_rawvalues  <- filter(aggregated_estimates_epi_rawvalues, group_name == "WPR")
aggregated_estimates_drtb           <- filter(aggregated_estimates_drtb, group_name == "WPR")
aggregated_estimates_drtb_rawvalues <- filter(aggregated_estimates_drtb_rawvalues, group_name == "WPR")

budget_expenditure         <- filter(budget_expenditure, g_whoregion == "WPR")

country_group_membership   <- country_group_membership %>% inner_join(wpr_iso2)

data_collection            <- filter(data_collection, g_whoregion == "WPR")

dr_derived_variables       <- dr_derived_variables %>% inner_join(wpr_iso2)
dr_surveillance            <- filter(dr_surveillance, g_whoregion == "WPR")
drs                        <- filter(drs, g_whoregion == "WPR")

estimates_epi              <- filter(estimates_epi, g_whoregion == "WPR")
estimates_epi_rawvalues    <- filter(estimates_epi_rawvalues, g_whoregion == "WPR")
estimates_drtb             <- estimates_drtb %>% inner_join(wpr_iso2)
estimates_drtb_rawvalues   <- estimates_drtb_rawvalues %>% inner_join(wpr_iso2)
estimates_population       <- filter(estimates_population, g_whoregion == "WPR")

finance                    <- filter(finance, g_whoregion == "WPR")
notification               <- filter(notification, g_whoregion == "WPR")
outcomes                   <- filter(outcomes, g_whoregion == "WPR")
report_country             <- filter(report_country, g_whoregion == "WPR")
strategy                   <- filter(strategy, g_whoregion == "WPR")
TBHIV_for_aggregates       <- filter(TBHIV_for_aggregates, g_whoregion == "WPR")



setwd(rdata_folder)

# Save the data frames
save (aggregated_estimates_epi,
      aggregated_estimates_epi_rawvalues,
      aggregated_estimates_drtb,
      aggregated_estimates_drtb_rawvalues,

      budget_expenditure,

      country_group_membership,
      country_group_types,
      country_groups,

      data_codes,
      data_collection,
      data_dictionary,

      dr_derived_variables,
      dr_surveillance,
      drs,

      estimates_epi,
      estimates_epi_rawvalues,
      estimates_drtb,
      estimates_drtb_rawvalues,
      estimates_population,

      finance,
      notification,
      outcomes,
      report_country,
      strategy,
      TBHIV_for_aggregates,

      data.date,
      file=paste0("data_WPR_",Sys.Date(),".Rdata"))

# Clear the decks ----
rm(list=ls())
