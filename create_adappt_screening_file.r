# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to produce CSV files for adappt to use in
# the TB screening app (based on code for the TB report app)
#
# Hazim Timimi, September 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# A: SET A FEW FLAGS AND CONSTANTS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Establish the report year and limiting date for the incidence estimates
report_year <- 2023
limiting_date <- '2023-12-31'

# The following are convenience variables since notification and most other data sets will run up to the
# year before the reporting year
notification_maxyear <- (report_year - 1)

# This is needed to avoid scientific notation output. No idea what it actually means -- it must get the prize for the most confusing documentation. Ever.
options("scipen"=20)

# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)

# Get DB connection string and path where to save the data (variable adappt_folder in set_environment.r)
source(here::here("set_environment.r"))

# Create output folder (only if it doesn't yet exist)
dir.create(adappt_folder, showWarnings = FALSE, recursive = TRUE)




# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library("dplyr")
library("tidyr")
library("stringr")
library("rlang")
library("RODBC")
library("ghost")

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here::here("functions/handle_NAs.r"))
source(here::here("functions/get_long_data.r"))


# Load data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Load prevalence estimates (file location from set_environment.r)
prev <- read.csv(prev_estimates_file, na.strings = "")


# connect to the database
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
  SELECT iso3, year, e_pop_m014 + e_pop_f014 AS e_pop_014_num, e_pop_num
  FROM   view_TME_estimates_population
  WHERE  year BETWEEN 2000 AND @year"
sql <- gsub('@year', report_year, sql, ignore.case = TRUE)

pop  <- sqlQuery(channel, sql, na.strings = "")

sql <- "
  SELECT iso3, year, e_hh_contacts, e_prevtx_eligible AS e_hh_contacts_04
  FROM   view_TME_estimates_ltbi
  WHERE  year = @year"
sql <- gsub('@year', report_year - 1, sql, ignore.case = TRUE)

ltbi  <- sqlQuery(channel, sql, na.strings = "")


odbcClose(channel)

detach(package:RODBC)


# Get HIV indicators from the GHO

hiv_indicator_codes <-
  c(
    "HIV_0000000001", # https://www.who.int/data/gho/data/indicators/indicator-details/GHO/estimated-number-of-people--living-with-hiv
    "HIV_0000000023", # https://www.who.int/data/gho/data/indicators/indicator-details/GHO/estimated-number-of-children-needing-antiretroviral-therapy-based-on-who-methods
    "HIV_0000000009", # https://www.who.int/data/gho/data/indicators/indicator-details/GHO/reported-number-of-people-receiving-antiretroviral-therapy
    "HIV_ARTCOVERAGE",# https://www.who.int/data/gho/data/indicators/indicator-details/GHO/estimated-antiretroviral-therapy-coverage-among-people-living-with-hiv-(-)
    "HIV_0000000011", # https://www.who.int/data/gho/data/indicators/indicator-details/GHO/reported-number-of-children-receiving-antiretroviral-therapy
    "HIV_0000000024"  # https://www.who.int/data/gho/data/indicators/indicator-details/GHO/estimated-antiretroviral-therapy-coverage-among-children
    )

hiv_indicators <- gho_data(hiv_indicator_codes,
                           query = paste("$filter=TimeDim eq", report_year - 1, "and SpatialDimType eq 'COUNTRY'")
                           ) |>

  # Simplify the data frame
  select(indicator_code = IndicatorCode,
         location_code = SpatialDim,
         year = TimeDim,
         value = NumericValue) |>
  mutate(indicator_code = case_when(
    indicator_code == "HIV_0000000001" ~ "hiv_num",
    indicator_code == "HIV_0000000023" ~ "hiv_014_num",
    indicator_code == "HIV_0000000009" ~ "hiv_art_num",
    indicator_code == "HIV_ARTCOVERAGE" ~ "hiv_art_pct",
    indicator_code == "HIV_0000000011" ~ "hiv_art_014_num",
    indicator_code == "HIV_0000000024" ~ "hiv_art_014_pct"
  ))




# Calculate % from numbers and numbers from % ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Prevalence as absolute numbers
prev <- prev |>
  inner_join(pop, by = c("iso3", "year")) |>
  mutate(e_prev_num = as.integer(round(prev * e_pop_num / 1e5))) |>
  select(iso3,
         year,
         e_prev_100k = prev,
         e_prev_num)



# PLHIV as % of population (all ages)
hiv_indicators <- hiv_indicators |>
  filter(indicator_code == "hiv_num") |>
  # Join to the population estimates
  inner_join(pop, by = c("location_code" = "iso3", "year")) |>
  # Calculate % based on total population
  mutate(value = value * 100 / e_pop_num,
         indicator_code = "hiv_pct") |>
  # Append numbers to the existing data frame
  select(indicator_code,
         location_code,
         year,
         value) |>
  rbind(hiv_indicators)

# PLHIV as % of population (aged 0-14 years)
hiv_indicators <- hiv_indicators |>
  filter(indicator_code == "hiv_014_num") |>
  # Join to the population estimates
  inner_join(pop, by = c("location_code" = "iso3", "year")) |>
  # Calculate % based on population aged 0-14 years
  mutate(value = value * 100 / e_pop_014_num,
         indicator_code = "hiv_014_pct") |>
  # Append numbers to the existing data frame
  select(indicator_code,
         location_code,
         year,
         value) |>
  rbind(hiv_indicators)



# Use SDG indicators for the risk factors and also
# calculate numbers with risk factors from % of population ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sdg_indicators <-
  # undernourishment
  get_external_indicators(sdg,
                          indicator_filter = "SN.ITK.DEFC.ZS",
                          output_var_name = "und_pct",
                          flg_latest_year = TRUE) |>

  # diabetes
  rbind(get_external_indicators(sdg,
                                indicator_filter = "NCD_GLUC_04",
                                output_var_name = "dia_pct",
                                flg_latest_year = TRUE)) |>


  # alcohol
  rbind(get_external_indicators(sdg,
                                indicator_filter = "SA_0000001462",
                                output_var_name = "alc_pct",
                                flg_latest_year = TRUE)) |>


  # smoking
  rbind(get_external_indicators(sdg,
                                indicator_filter = "M_Est_smk_curr_std",
                                output_var_name = "smk_pct",
                                flg_latest_year = TRUE))

# Add undernourishment numbers
sdg_indicators <- sdg_indicators |>
  filter(indicator_code == "und_pct") |>
  # Join to the population estimates
  inner_join(pop, by = c("location_code" = "iso3", "year")) |>
  # Calculate number based on total population
  mutate(value = as.integer(round(value * e_pop_num / 100)),
         indicator_code = "und_num") |>
  # Append numbers to the existing data frame
  select(indicator_code,
         location_code,
         year,
         value) |>
  rbind(sdg_indicators)

# Add diabetics numbers
sdg_indicators <- sdg_indicators |>
  filter(indicator_code == "dia_pct") |>
  # Join to the population estimates
  inner_join(pop, by = c("location_code" = "iso3", "year")) |>
  # Calculate number based on population of 15+ years (a slight overestimate as indicator is for 18+ years
  mutate(value = as.integer(round(value * (e_pop_num - e_pop_014_num)/ 100)),
         indicator_code = "dia_num") |>
  # Append numbers to the existing data frame
  select(indicator_code,
         location_code,
         year,
         value) |>
  rbind(sdg_indicators)

# Add alcohol numbers
sdg_indicators <- sdg_indicators |>
  filter(indicator_code == "alc_pct") |>
  # Join to the population estimates
  inner_join(pop, by = c("location_code" = "iso3", "year")) |>
  # Calculate number based on population of 15+ years
  mutate(value = as.integer(round(value * (e_pop_num - e_pop_014_num)/ 100)),
         indicator_code = "alc_num") |>
  # Append numbers to the existing data frame
  select(indicator_code,
         location_code,
         year,
         value) |>
  rbind(sdg_indicators)

# Add smoker numbers
sdg_indicators <- sdg_indicators |>
  filter(indicator_code == "smk_pct") |>
  # Join to the population estimates
  inner_join(pop, by = c("location_code" = "iso3", "year")) |>
  # Calculate number based on population of 15+ years
  mutate(value = as.integer(round(value * (e_pop_num - e_pop_014_num)/ 100)),
         indicator_code = "smk_num") |>
  # Append numbers to the existing data frame
  select(indicator_code,
         location_code,
         year,
         value) |>
  rbind(sdg_indicators)






# Create the output file ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output <-
  get_var(pop,"e_pop_num", starting_year = report_year - 1) |>
  rbind(get_var(pop,"e_pop_014_num", starting_year = report_year - 1)) |>
  # Restrict population estimates to report_year - 1
  filter(year == report_year - 1) |>
  rbind(get_var(prev,"e_prev_num", starting_year = report_year - 1)) |>
  rbind(get_var(prev,"e_prev_100k", starting_year = report_year - 1)) |>
  rbind(get_var(ltbi,"e_hh_contacts", starting_year = report_year - 1)) |>
  rbind(get_var(ltbi,"e_hh_contacts_04", starting_year = report_year - 1)) |>

  # hiv variables (already in the correct format)
  rbind(hiv_indicators) |>

  # risk factors already calculated from SDG indicators
  rbind(sdg_indicators) |>

  # restrict data to the 215 countries and areas from which we collected data
  # in the reporting year (this deals with Serbia & Montenegro, Netherlands Antilles etc)

  filter(location_code %in% cty$iso3)


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

