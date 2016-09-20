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

scripts_folder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616
setwd(scripts_folder)


# Get DB connection string and path where to save the data (variable rdata_folder in set_environment.r)
source("set_environment.r")

# Load the views directly from the global TB database
source("load_data_from_database.r")


setwd(rdata_folder)

# Save the data frames
save (aggregated_estimates_epi,
      aggregated_estimates_epi_rawvalues,
      aggregated_estimates_mdr_in_notified,
      aggregated_estimates_drtb,
      aggregated_estimates_drtb_rawvalues,
      budget_expenditure,
      data_codes,
      data_collection,
      data_dictionary,
      dr_surveillance,
      drs,
      estimates_epi,
      estimates_epi_rawvalues,
      estimates_drtb,
      estimates_drtb_rawvalues,
      estimates_mdr,
      estimates_mdr_in_notified,
      estimates_population,
      finance,
      notification,
      notification_exceptions,
      outcomes,
      prevalence_survey,
      report_country,
      strategy,
      TBHIV_for_aggregates,
      data.date,
      file=paste0("data_",Sys.Date(),".Rdata"))

# Clear the decks ----
rm(list=ls())
