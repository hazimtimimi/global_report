# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to send CSV files of un-rounded estimates to the Global Fund
#
# Hazim Timimi, November 2022
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Establish the report year
report_year <- 2022


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


# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library("dplyr")


# Create incidence and mortality estimates file ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

estimates_epi_rawvalues %>%
  select(country, year, iso2, iso3,
         e_inc_num,
         e_inc_num_lo,
         e_inc_num_hi,
         e_inc_tbhiv_num,
         e_inc_tbhiv_num_lo,
         e_inc_tbhiv_num_hi,
         e_mort_exc_tbhiv_num,
         e_mort_exc_tbhiv_num_lo,
         e_mort_exc_tbhiv_num_hi,
         e_mort_tbhiv_num,
         e_mort_tbhiv_num_lo,
         e_mort_tbhiv_num_hi,
         e_mort_num,
         e_mort_num_lo,
         e_mort_num_hi) %>%

  write.csv(file=paste0(gf_folder, "GF_TB_incidence_mortality_",Sys.Date(),".csv"),
            row.names=FALSE,
            na="")


# Create RR-TB incidence estimates file ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

estimates_drtb_rawvalues %>%
  select(country, year, iso2, iso3,
         e_inc_rr_num,
         e_inc_rr_num_lo,
         e_inc_rr_num_hi) %>%

  write.csv(file=paste0(gf_folder, "GF_RR-TB_incidence_",Sys.Date(),".csv"),
            row.names=FALSE,
            na="")


# Create disaggregated incidence estimates file ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

estimates_agesex_rawvalues %>%
  select(-se) %>%
  write.csv(file=paste0(gf_folder, "GF_TB_incidence_agesex_riskfactors_",Sys.Date(),".csv"),
            row.names=FALSE,
            na="")

# Create LTBI estimates file ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

estimates_ltbi %>%
  select(country, year, iso2, iso3,
         e_prevtx_eligible,
         e_prevtx_eligible_lo,
         e_prevtx_eligible_hi) %>%
  filter(year==report_year-1) %>%
  write.csv(file=paste0(gf_folder, "GF_LTBI_estimates_",Sys.Date(),".csv"),
            row.names=FALSE,
            na="")






