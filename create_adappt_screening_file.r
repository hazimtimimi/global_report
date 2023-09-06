# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to produce CSV files for adappt to use in
# the TB screening app (based on code for the TB report app)
#
# Hazim Timimi, September 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# A: SET A FEW FLAGS AND CONSTANTS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Establish the report year and limiting date for the incidence estimates
report_year <- 2022
limiting_date <- '2022-12-31'

# The following are convenience variables since notification and most other data sets will run up to the
# year before the reporting year
notification_maxyear <- (report_year - 1)


# This is needed to avoid scientific notation output. No idea what it actually means -- it must get the prize for the most confusing documentation. Ever.
options("scipen"=20)

# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)

# Get DB connection string and path where to save the data (variable adappt_folder in set_environment.r)
source(here::here("set_environment.r"))

# connect to the database
library(RODBC)
channel <- odbcDriverConnect(connection_string)

# load views into dataframes
sql <- "
  SELECT iso3, country
  FROM   view_TME_master_data_collection
  WHERE  datcol_year = @year"
sql <- gsub('@year', report_year, sql, ignore.case = TRUE)

cty <- sqlQuery(channel, sql, na.strings = "")

sql <- "
  SELECT iso3, year, sex, indicator_id, value
  FROM   external_indicators.view_indicator_data
  WHERE  year <= @year AND sex = 'a'"
sql <- gsub('@year', report_year - 1, sql, ignore.case = TRUE)

sdg  <- sqlQuery(channel, sql, na.strings = "")

sql <- "
  SELECT iso3, year, e_pop_num
  FROM   view_TME_estimates_population
  WHERE  year = @year"
sql <- gsub('@year', report_year - 1, sql, ignore.case = TRUE)

pop  <- sqlQuery(channel, sql, na.strings = "")

sql <- "
  SELECT iso3, year, e_hh_contacts
  FROM   view_TME_estimates_ltbi
  WHERE  year = @year"
sql <- gsub('@year', report_year - 1, sql, ignore.case = TRUE)

ltbi  <- sqlQuery(channel, sql, na.strings = "")


# Retrieve the incidence for the relevant report

sql <- "
  SELECT iso3, year, e_inc_num, e_inc_100k
  FROM   epi_estimates_rawvalues_at_date(CAST('@limiting_date' AS DATE))
  WHERE  year = @year"

sql <- gsub('@limiting_date', as.character(limiting_date), sql, ignore.case = TRUE)
sql <- gsub('@year', report_year - 1, sql, ignore.case = TRUE)

inc <- sqlQuery(channel, sql, na.strings = "")

odbcClose(channel)

detach(package:RODBC)



# Create output folder (only if it doesn't yet exist)
dir.create(adappt_folder, showWarnings = FALSE, recursive = TRUE)


# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library("dplyr")
library("tidyr")
library("stringr")
library("rlang")


# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#source(paste0(scripts_folder, "/functions/round_numbers.r"), encoding = "UTF-8")
source(here::here("functions/handle_NAs.r"))
source(here::here("functions/get_long_data.r"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Estimates (country) -----
#   Not the most elegant way, but having this explicit helps
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output <-
  get_var(pop,"e_pop_num", starting_year = report_year - 1) |>
  rbind(get_var(inc,"e_inc_num", starting_year = report_year - 1)) |>
  rbind(get_var(inc,"e_inc_100k", starting_year = report_year - 1)) |>
  rbind(get_var(ltbi,"e_hh_contacts", starting_year = report_year - 1)) |>

  # HIV prevalence
  rbind(get_external_indicators(sdg,
                                indicator_filter = "MDG_0000000029",
                                output_var_name = "hiv",
                                flg_latest_year = TRUE)) |>

  # undernourishment
  rbind(get_external_indicators(sdg,
                                indicator_filter = "SN.ITK.DEFC.ZS",
                                output_var_name = "und",
                                flg_latest_year = TRUE)) |>

  # diabetes
  rbind(get_external_indicators(sdg,
                                indicator_filter = "NCD_GLUC_04",
                                output_var_name = "dia",
                                flg_latest_year = TRUE)) |>


  # alcohol
  rbind(get_external_indicators(sdg,
                                indicator_filter = "SA_0000001462",
                                output_var_name = "alc",
                                flg_latest_year = TRUE)) |>


  # smoking
  rbind(get_external_indicators(sdg,
                                indicator_filter = "M_Est_smk_curr_std",
                                output_var_name = "smk",
                                flg_latest_year = TRUE)) |>


  # restrict data to the 215 countries and areas from which we collected data
  # in the reporting year (this deals with Serbia & Montenegro, Netherlands Antilles etc)

  filter(location_code %in% cty$iso3)



# TBD: Add very rough TB prevalence estimate and also HIV estimates disaggregated by
# < 15 years, 15+ on ART and 15+ not on ART (see if on GHO or AidsInfo)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Save final dataset to CSV -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(x = output,
          file = paste(adappt_folder, "adappt_TB_screening_data_",Sys.Date(),".csv", sep="") ,
          quote = FALSE,
          row.names = FALSE,
          na = "")

# And save the locations file
cty |>
  select(location_code = iso3,
         location_name = country) |>
  write.csv(file = paste(adappt_folder, "locations_",Sys.Date(),".csv", sep="") ,
            quote = FALSE,
            row.names = FALSE,
            na = "")

