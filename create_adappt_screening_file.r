# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to produce CSV files for adappt to use in
# the TB screening app (based on code for the TB report app)
#
# Hazim Timimi, July 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# A: SET A FEW FLAGS AND CONSTANTS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Establish the report year
report_year <- 2022

# The following are convenience variables since notification and most other data sets will run up to the
# year before the reporting year
notification_maxyear <- (report_year - 1)


# Establish starting year for historical data that will not go all the way back to 2000
hist_start_year <- 2015

# This is needed to avoid scientific notation output. No idea what it actually means -- it must get the prize for the most confusing documentation. Ever.

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



# Create output folder (only if it doesn't yet exist)
dir.create(adappt_folder, showWarnings = FALSE, recursive = TRUE)




# B: OK, NOW GO FOR IT ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library("dplyr")
library("tidyr")
library("stringr")
library("rlang")


# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(paste0(scripts_folder, "/functions/round_numbers.r"), encoding = "UTF-8")
source(paste0(scripts_folder, "/functions/handle_NAs.r"))
source(paste0(scripts_folder, "/functions/get_long_data.r"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Estimates (country) -----
#   Not the most elegant way, but having this explicit helps
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Late in profile production was asked to change case fatality ratio to be shown as %
# I didn;t change the database so easiest way here is to create a new varibale and use that
# in the data sent to Adappt


adappt_est <-
  rbind(get_estimates(estimates_epi,"e_inc_num", starting_year = report_year - 1)) |>
  rbind(get_estimates(estimates_epi,"e_inc_100k", starting_year = report_year - 1))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   SDG indicators -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The following is a bit clunky considering the source table is already in long format, but wanted the
# flexibility here to be able to chop and change as needed, including if we needed to change the output
# variable name

adappt_sdg <-
  # Here are the indicators for which previously we only wanted the latest available year
  # But by end of 2020 version of the app we changed to all years

  # HIV prevalence
  rbind(get_external_indicators(external_indicator_data,
                                indicator_filter = "MDG_0000000029",
                                # keep the ID used in 2019 so adappt doesnt need to change their code
                                output_var_name = "SH.DYN.AIDS.ZS",
                                flg_latest_year = TRUE,
                                round_sig_fig=2)) |>

  # undernourishment
  rbind(get_external_indicators(external_indicator_data,
                                indicator_filter = "SN.ITK.DEFC.ZS",
                                flg_latest_year = TRUE,
                                round_sig_fig=2)) |>

  # diabetes, female and male separately
  rbind(get_external_indicators(external_indicator_data,
                                indicator_filter = "NCD_GLUC_04",
                                sex_filter = "f",
                                # keep the ID used in 2019 so adappt doesnt need to change their code
                                output_var_name = "NCD_GLUC_04FMLE",
                                flg_latest_year = TRUE,
                                round_sig_fig=2)) |>
  rbind(get_external_indicators(external_indicator_data,
                                indicator_filter = "NCD_GLUC_04",
                                sex_filter = "m",
                                # keep the ID used in 2019 so adappt doesnt need to change their code
                                output_var_name = "NCD_GLUC_04MLE",
                                flg_latest_year = TRUE,
                                round_sig_fig=2)) |>

  # alcohol, female and male separately
  rbind(get_external_indicators(external_indicator_data,
                                indicator_filter = "SA_0000001462",
                                sex_filter = "f",
                                # keep the ID used in 2019 so adappt doesnt need to change their code
                                output_var_name = "SA_0000001462FMLE",
                                flg_latest_year = TRUE,
                                round_sig_fig=2)) |>
  rbind(get_external_indicators(external_indicator_data,
                                indicator_filter = "SA_0000001462",
                                sex_filter = "m",
                                # keep the ID used in 2019 so adappt doesnt need to change their code
                                output_var_name = "SA_0000001462MLE",
                                flg_latest_year = TRUE,
                                round_sig_fig=2)) |>

  # smoking, female and male separately
  rbind(get_external_indicators(external_indicator_data,
                                indicator_filter = "M_Est_smk_curr_std",
                                sex_filter = "f",
                                # keep the ID used in 2019 so adappt doesnt need to change their code
                                output_var_name = "SH.PRV.SMOK.FE",
                                flg_latest_year = TRUE,
                                round_sig_fig=2)) |>
  rbind(get_external_indicators(external_indicator_data,
                                indicator_filter = "M_Est_smk_curr_std",
                                sex_filter = "m",
                                # keep the ID used in 2019 so adappt doesnt need to change their code
                                output_var_name = "SH.PRV.SMOK.MA",
                                flg_latest_year = TRUE,
                                round_sig_fig=2))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Country-reported data -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


adappt_data <-

  # In 2020 we dropped rr_sldst from the notification view and replaced it with indicator rr_dst_rlt_fq from the dr_surveillance view
  # However, to avoid adappt having to recode the app, we decided to provide the new variable using the old
  # variable name.

  get_vars_and_aggregates(strategy, "newinc_con",
                          starting_year = report_year - 1,
                          ending_year = notification_maxyear ) |>

  rbind(get_vars_and_aggregates(estimates_population, "e_pop_num" ,
                                starting_year = report_year - 1,
                                ending_year = notification_maxyear ))




# Combine files with value only and add empty lo, hi fields and then combine with the estimates file
adappt_output <-
  rbind(adappt_sdg, adappt_data) %>%
  mutate(lo = NA,
         hi = NA) %>%
  rbind(adappt_est)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Exclude disbanded entities -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# restrict data to the 215 countries and areas from which we collected data
# in the reporting year (this deals with Serbia & Montenegro, Netherlands Antilles etc)

adappt_location_list <-
  data_collection |>
  filter(datcol_year == report_year) |>
  select(location_code = iso3)

# And don't forget to add the codes for WHO regions and globally
# Create a list of the regiona and aggregate names
# group_codes <- read.table(textConnection("
# location_code,g_whoregion,location_name, location_name_FR, location_name_ES, location_name_RU, show_flag
# global,,Global, Global, Mundial, Глобальный,
# AFR,,Africa, Afrique, África, Африка,
# AMR,,Americas, Amériques, Las Américas, Северные и Южная Америка,
# EMR,,Eastern Mediterranean, Méditerranée orientale, Mediterráneo oriental, Восточное Средиземноморье,
# EUR,,Europe, Europe, Europa, Европа,
# SEA,,South-East Asia, Asie du Sud-Est, Asia Sudoriental, Юго-Восточная Азия,
# WPR,,Western Pacific, Pacifique occidental, Pacífico occidental, Западная часть Тихого океана, "), header=TRUE, sep=",", as.is = TRUE)
# closeAllConnections()

#Add the group location codes to the adappt list

# adappt_location_list <-
#   select(group_codes, location_code) |>
#   rbind(adappt_location_list)

adappt_output <-
  adappt_output %>%
  filter(location_code %in% adappt_location_list$location_code)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Save final dataset to CSV -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(x = adappt_output,
          file = paste(adappt_folder, "adappt_TB_screening_data_",Sys.Date(),".csv",sep="") ,
          quote = FALSE,
          row.names = FALSE,
          na = "")


