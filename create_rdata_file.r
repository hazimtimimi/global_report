# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load views from the global TB database and save them all in a
# single dated .Rdata file.
# Run by Hazim in Geneva who saves the .Rdata in a shared dropbox folder
# so that Tom can get access to the data from Manilla.
#
# Hazim Timimi, based on a original from Tom
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Clear the decks ----
rm(list=ls())

# Get DB connection string and path where to save the data (variable rdata_folder in set_environment.r)
source("set_environment.r")

# Load the views directly from the global TB database
source("load_data_from_database.r")


setwd(rdata_folder)

# Save the data frames
save (aggregated_estimates_agesex,
      aggregated_estimates_epi,
      aggregated_estimates_epi_rawvalues,
      aggregated_estimates_drtb,
      aggregated_estimates_drtb_rawvalues,
      aggregated_finance_estimates,

      budget_expenditure,

      catastrophic_costs_survey,

      country_group_membership,
      country_group_types,
      country_groups,

      data_codes,
      data_collection,
      data_dictionary,

      dr_derived_variables,
      dr_surveillance,
      drs,

      drs_for_estimation_new,
      drs_for_estimation_ret,
      drs_for_estimation_sldst,
      drs_most_recent_for_estimation,

      estimates_agesex,
      estimates_epi,
      estimates_epi_rawvalues,
      estimates_drtb,
      estimates_drtb_rawvalues,
      estimates_ltbi,
      estimates_population,

      finance,
      finance_cleaned,

      notification,
      notification_exceptions,
      outcomes,

      prevalence_survey,
      prevalence_survey_cases,
      prevalence_survey_estimates,

      report_country,
      strategy,
      TBHIV_for_aggregates,

      external_indicator_data,
      external_indicator_defs,

      data.date,
      file=paste0("data_",Sys.Date(),".Rdata"))

# Clear the decks ----
rm(list=ls())
