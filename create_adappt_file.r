# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to produce CSV files for adappt to use in a mobile app
# (Based on GHO export code)
#
# Hazim Timimi, July 2019
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Clear the decks ----
rm(list=ls())


# A: SET A FEW FLAGS AND CONSTANTS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Establish the report year
report_year <- 2019

# The following are convenience variables since notification and most other data sets will run up to the
# year before the reporting year and outcomes will run up to two years before the reporting year
notification_maxyear <- (report_year - 1)
outcome_maxyear      <- (report_year - 2)


# This is needed to avoid scientific notation output. No idea what it actally means -- it must get the prize for the most confusing documentation. Ever.

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
# stop("OK, see what we have!")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

stop("

     >>>>>>>>>>
     Stopping here so can do the rest manually!
     <<<<<<<<<<<<")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Estimates (country and aggregates) -----
#   Not the most elegant way, but having this explicit helps
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Late in profile production was asked to change case fatality ratio to be shown as %
# I didn;t change the database so easiest way here is to create a new varibale and use that
# in the data sent to Adappt


estimates_epi$c_cfr_pct <- estimates_epi$cfr * 100
estimates_epi$c_cfr_pct_lo <- estimates_epi$cfr_lo * 100
estimates_epi$c_cfr_pct_hi <- estimates_epi$cfr_hi * 100

aggregated_estimates_epi$c_cfr_pct <- aggregated_estimates_epi$cfr * 100
aggregated_estimates_epi$c_cfr_pct_lo <- aggregated_estimates_epi$cfr_lo * 100
aggregated_estimates_epi$c_cfr_pct_hi <- aggregated_estimates_epi$cfr_hi * 100


adappt_est <-
        get_estimates(estimates_epi,"e_mort_exc_tbhiv_num") %>%
  rbind(get_estimates(estimates_epi,"e_mort_exc_tbhiv_100k")) %>%
  rbind(get_estimates(estimates_epi,"e_mort_tbhiv_num")) %>%
  rbind(get_estimates(estimates_epi,"e_mort_tbhiv_100k")) %>%
  rbind(get_estimates(estimates_epi,"e_inc_num")) %>%
  rbind(get_estimates(estimates_epi,"e_inc_100k")) %>%
  rbind(get_estimates(estimates_epi,"e_inc_tbhiv_num")) %>%
  rbind(get_estimates(estimates_epi,"e_inc_tbhiv_100k")) %>%
  rbind(get_estimates(estimates_epi,"c_cfr_pct", starting_year = notification_maxyear)) %>%
  rbind(get_estimates(estimates_epi,"c_cdr", starting_year = notification_maxyear)) %>%
  rbind(get_estimates(estimates_drtb,"e_inc_rr_num", starting_year = notification_maxyear)) %>%
  rbind(get_estimates(estimates_drtb,"e_inc_rr_100k", starting_year = notification_maxyear)) %>%
  rbind(get_estimates(estimates_drtb,"e_rr_pct_new", starting_year = notification_maxyear)) %>%
  rbind(get_estimates(estimates_drtb,"e_rr_pct_ret", starting_year = notification_maxyear)) %>%

  # and now add the same indicators for aggregates

  rbind(get_estimates(aggregated_estimates_epi,"e_mort_exc_tbhiv_num")) %>%
  rbind(get_estimates(aggregated_estimates_epi,"e_mort_exc_tbhiv_100k")) %>%
  rbind(get_estimates(aggregated_estimates_epi,"e_mort_tbhiv_num")) %>%
  rbind(get_estimates(aggregated_estimates_epi,"e_mort_tbhiv_100k")) %>%
  rbind(get_estimates(aggregated_estimates_epi,"e_inc_num")) %>%
  rbind(get_estimates(aggregated_estimates_epi,"e_inc_100k")) %>%
  rbind(get_estimates(aggregated_estimates_epi,"e_inc_tbhiv_num")) %>%
  rbind(get_estimates(aggregated_estimates_epi,"e_inc_tbhiv_100k")) %>%
  rbind(get_estimates(aggregated_estimates_epi,"c_cfr_pct", starting_year = notification_maxyear)) %>%
  rbind(get_estimates(aggregated_estimates_epi,"c_cdr", starting_year = notification_maxyear)) %>%
  rbind(get_estimates(aggregated_estimates_drtb,"e_inc_rr_num", starting_year = notification_maxyear)) %>%
  rbind(get_estimates(aggregated_estimates_drtb,"e_inc_rr_100k", starting_year = notification_maxyear)) %>%
  rbind(get_estimates(aggregated_estimates_drtb,"e_rr_pct_new", starting_year = notification_maxyear)) %>%
  rbind(get_estimates(aggregated_estimates_drtb,"e_rr_pct_ret", starting_year = notification_maxyear)) %>%


# Now add records from the age/sex/risk factor table which is already in long format an needs to be
# queried in a slightly different way


  rbind(get_estimates_agesex(estimates_agesex, sex_filter = "f", output_var_name = "e_inc_num_f")) %>%
  rbind(get_estimates_agesex(estimates_agesex, sex_filter = "m", output_var_name = "e_inc_num_m")) %>%
  rbind(get_estimates_agesex(estimates_agesex, age_group_filter = "0-14", output_var_name = "e_inc_num_014")) %>%
  rbind(get_estimates_agesex(estimates_agesex, age_group_filter = "15plus", output_var_name = "e_inc_num_15plus")) %>%
  rbind(get_estimates_agesex(estimates_agesex, age_group_filter = "15plus", risk_factor_filter = "alc", output_var_name = "e_inc_num_alc")) %>%
  rbind(get_estimates_agesex(estimates_agesex, age_group_filter = "15plus", risk_factor_filter = "dia", output_var_name = "e_inc_num_dia")) %>%
  rbind(get_estimates_agesex(estimates_agesex, risk_factor_filter = "hiv", output_var_name = "e_inc_num_hiv")) %>%
  rbind(get_estimates_agesex(estimates_agesex, age_group_filter = "15plus", risk_factor_filter = "smk", output_var_name = "e_inc_num_smk")) %>%
  rbind(get_estimates_agesex(estimates_agesex, risk_factor_filter = "und", output_var_name = "e_inc_num_und")) %>%

  # and now add the same indicators for aggregates

  rbind(get_estimates_agesex(aggregated_estimates_agesex, sex_filter = "f", output_var_name = "e_inc_num_f")) %>%
  rbind(get_estimates_agesex(aggregated_estimates_agesex, sex_filter = "m", output_var_name = "e_inc_num_m")) %>%
  rbind(get_estimates_agesex(aggregated_estimates_agesex, age_group_filter = "0-14", output_var_name = "e_inc_num_014")) %>%
  rbind(get_estimates_agesex(aggregated_estimates_agesex, age_group_filter = "15plus", output_var_name = "e_inc_num_15plus")) %>%
  rbind(get_estimates_agesex(aggregated_estimates_agesex, age_group_filter = "15plus", risk_factor_filter = "alc", output_var_name = "e_inc_num_alc")) %>%
  rbind(get_estimates_agesex(aggregated_estimates_agesex, age_group_filter = "15plus", risk_factor_filter = "dia", output_var_name = "e_inc_num_dia")) %>%
  rbind(get_estimates_agesex(aggregated_estimates_agesex, risk_factor_filter = "hiv", output_var_name = "e_inc_num_hiv")) %>%
  rbind(get_estimates_agesex(aggregated_estimates_agesex, age_group_filter = "15plus", risk_factor_filter = "smk", output_var_name = "e_inc_num_smk")) %>%
  rbind(get_estimates_agesex(aggregated_estimates_agesex, risk_factor_filter = "und", output_var_name = "e_inc_num_und")) %>%

  # and now add catastrophic cost survey results for countries that have them

  rbind(get_catatrophic_costs(catastrophic_costs_survey, output_var_name = "catast_pct")) %>%

  # add the TPT covereage for children < 5 for countries only
  rbind(get_estimates(estimates_ltbi,"e_prevtx_kids_pct", starting_year = notification_maxyear))

# remove uncertainty intervals from e_prevtx_kids_pct if value, lo and hi are all 100
adappt_est <-
  adappt_est %>%
  mutate(lo = ifelse(indicator_code == "e_prevtx_kids_pct" & value == 100 & lo == 100 & hi == 100,
                        NA,
                        lo),
         hi = ifelse(indicator_code == "e_prevtx_kids_pct" & value == 100 & lo == 100 & hi == 100,
                        NA,
                        hi))


# Need to calculate aggregate values for e_prevtx_kids_pct
# Use the method from the regional and global profiles:
#
# Use the normal approximation to calculate the confidence intervals for e_prevtx_eligible for regional aggregates.
# The intervals for a given country is simply best +/- 1.96 * SQRT(variance)
# Therefore variance for a given country is POWER(((e_prevtx_eligible_hi - e_prevtx_eligible) / 1.96) ,2)
# The variance of the aggregate is the sum of the variance of each country
# Therefore the lower limit of the aggregate, for example, becomes
# SUM(e_prevtx_eligible) - 1.96 * SQRT(SUM(POWER(((e_prevtx_eligible_hi - e_prevtx_eligible) / 1.96) ,2))) AS e_prevtx_eligible_lo
#
# For the numerator of numbers reported on LTBI treatment, Yohhei only counted cases reported via routine
# surveillance and ignored those from surveys when he did table 5.2, so I will do the same here although I
# don;t think it is quite right.


# SELECT	ISNULL(SUM(newinc_con04_prevtx),0) AS kids_prevtx,
# 		SUM(e_prevtx_eligible) AS e_prevtx_eligible,
# 		SUM(e_prevtx_eligible) - 1.96 * SQRT(SUM(POWER( ((e_prevtx_eligible_hi - e_prevtx_eligible) / 1.96) ,2))) AS e_prevtx_eligible_lo,
# 		SUM(e_prevtx_eligible) + 1.96 * SQRT(SUM(POWER( ((e_prevtx_eligible_hi - e_prevtx_eligible) / 1.96) ,2))) AS e_prevtx_eligible_hi
#
# FROM dbo.view_TME_estimates_ltbi
# 		INNER JOIN view_TME_master_report_country
# 			ON view_TME_estimates_ltbi.iso2 = view_TME_master_report_country.iso2


adappt_temp <-
  estimates_ltbi %>%
  select(iso3, year, e_prevtx_eligible, e_prevtx_eligible_lo, e_prevtx_eligible_hi, newinc_con04_prevtx) %>%
  inner_join(select(report_country, iso3, g_whoregion)) %>%
  filter(year == notification_maxyear)

adappt_temp_r <-
  adappt_temp %>%
  group_by(g_whoregion, year) %>%
  summarise(kids = sum(newinc_con04_prevtx, na.rm=TRUE),
            eligible_best = sum(e_prevtx_eligible, na.rm=TRUE),
            eligible_lo = sum(e_prevtx_eligible, na.rm=TRUE) - 1.96 * sqrt(sum( ((e_prevtx_eligible_hi - e_prevtx_eligible) / 1.96)^2, na.rm = TRUE)),
            eligible_hi = sum(e_prevtx_eligible, na.rm=TRUE) + 1.96 * sqrt(sum( ((e_prevtx_eligible_hi - e_prevtx_eligible) / 1.96)^2, na.rm = TRUE))) %>%
  ungroup() %>%
  rename(location_code = g_whoregion )

adappt_temp_g <-
  adappt_temp %>%
  group_by(year) %>%
  summarise(kids = sum(newinc_con04_prevtx, na.rm=TRUE),
            eligible_best = sum(e_prevtx_eligible, na.rm=TRUE),
            eligible_lo = sum(e_prevtx_eligible, na.rm=TRUE) - 1.96 * sqrt(sum( ((e_prevtx_eligible_hi - e_prevtx_eligible) / 1.96)^2, na.rm = TRUE)),
            eligible_hi = sum(e_prevtx_eligible, na.rm=TRUE) + 1.96 * sqrt(sum( ((e_prevtx_eligible_hi - e_prevtx_eligible) / 1.96)^2, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(location_code = "global" )

# combine the aggregates
adappt_temp <-
  rbind(adappt_temp_r, adappt_temp_g)

rm(adappt_temp_r, adappt_temp_g)

# Calculate the percent coverage
adappt_temp <-
  adappt_temp %>%
  mutate(value = display_cap_pct(kids, eligible_best),
         lo = display_cap_pct(kids, eligible_hi),
         hi = display_cap_pct(kids, eligible_lo),
         indicator_code = "e_prevtx_kids_pct") %>%
  # remove any "> 100" values
  mutate(value = ifelse(value==">100", 100, value),
         lo = ifelse(lo==">100", NA, lo),
         hi = ifelse(hi==">100", NA, hi)) %>%
  mutate(lo = ifelse(value==100 & lo==100 & hi==100, NA, lo),
         hi = ifelse(value==100 & lo==100 & hi==100, NA, hi)) %>%
  select(indicator_code, location_code, year, value, lo, hi)

# Add the result to the estimates file

adappt_est <- rbind(adappt_est, adappt_temp)

rm(adappt_temp)

# And that, folks, concludes the estimates section.


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   SDG indicators -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# THe following is a bt clunky considering the source table is already in long format, but wanted the
# flexibility here to be able to chop and change as needed, including if we needed to change the output
# variable name

adappt_sdg <-
  # Here are the indicators for which we only want the latest available year
  # % below poverty line
        get_external_indicators(external_indicator_data, indicator_filter = "SI_POV_DAY1", round_sig_fig=2) %>%
  # social protection
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "per_allsp.cov_pop_tot", round_sig_fig=2)) %>%
  # HIV prevalence
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "SH.DYN.AIDS.ZS", round_sig_fig=2)) %>%
  # UHC index
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "UHC_INDEX_REPORTED")) %>%
  # access to clean fuel
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "EG.CFT.ACCS.ZS", round_sig_fig=2)) %>%
  # GINI index
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "SI.POV.GINI", round_sig_fig=2)) %>%
  # urban population in slums
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "EN.POP.SLUM.UR.ZS", round_sig_fig=2)) %>%

  # And here are the indicators for which we only want all years
  # undernourishment
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "SN.ITK.DEFC.ZS", flg_latest_year = FALSE, round_sig_fig=2)) %>%
  # diabetes, female and male separately
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "NCD_GLUC_04FMLE", flg_latest_year = FALSE, round_sig_fig=2)) %>%
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "NCD_GLUC_04MLE", flg_latest_year = FALSE, round_sig_fig=2)) %>%
  # alcohol, female and male separately
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "SA_0000001462FMLE", flg_latest_year = FALSE, round_sig_fig=2)) %>%
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "SA_0000001462MLE", flg_latest_year = FALSE, round_sig_fig=2)) %>%
  # smoking, female and male separately
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "SH.PRV.SMOK.FE", flg_latest_year = FALSE, round_sig_fig=2)) %>%
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "SH.PRV.SMOK.MA", flg_latest_year = FALSE, round_sig_fig=2)) %>%
  # health expenditure per capita
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "SH.XPD.CHEX.PP.CD", flg_latest_year = FALSE, round_whole_digit=TRUE)) %>%
  # > 10% total expenditure on health
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "FINPROTECTION_CATA_TOT_10_POP", flg_latest_year = FALSE, round_sig_fig=2)) %>%
  # GDP per capita
  rbind(get_external_indicators(external_indicator_data, indicator_filter = "NY.GDP.PCAP.PP.KD", flg_latest_year = FALSE, round_whole_digit=TRUE))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Country-reported data -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# make sure budget and population are treated as numeric, otherwise global estimates will be too big to hold ..

budget_expenditure$budget_tot <- as.numeric(budget_expenditure$budget_tot)
estimates_population$e_pop_num <- as.numeric(estimates_population$e_pop_num)


adappt_data <-
        get_vars_and_aggregates(notification, c("c_newinc", "c_notified")) %>%
  rbind(get_vars_and_aggregates(notification, c("newrel_hivpos",
                                                "newrel_art",
                                                "rr_sldst",
                                                "conf_rrmdr",
                                                "all_conf_xdr",
                                                "conf_xdr_tx"), starting_year = notification_maxyear )) %>%
  rbind(get_vars_and_aggregates(budget_expenditure, "budget_tot" , starting_year = report_year )) %>%
  rbind(get_vars_and_aggregates(estimates_population, "e_pop_num" , starting_year = notification_maxyear, ending_year = notification_maxyear ))



# Convert budget total and e_pop_num to millions; show numbers < 1 million to one decimal place
adappt_data <-
  adappt_data %>%
  mutate(value = ifelse(indicator_code %in% c("budget_tot", "e_pop_num"),
                        ifelse(value > 1e6,
                               round(value / 1e6, 0),
                               round(value / 1e6, 1)),
                        value))



# Calculate sum for MDR cases put on treatment
adappt_temp <-
  notification %>%
  select(iso3, year, g_whoregion, unconf_rrmdr_tx, conf_rrmdr_tx) %>%
  filter(year == notification_maxyear)

adappt_temp$mdr_tx <- sum_of_row(adappt_temp[c("unconf_rrmdr_tx", "conf_rrmdr_tx")])

adappt_data <-
  adappt_data %>%
  rbind(get_vars_and_aggregates(adappt_temp, "mdr_tx", starting_year = notification_maxyear))

rm(adappt_temp)


# Calculate percentages

adappt_calc <-
        get_pct(notification,
                numerator_vars = "newinc_rdx",
                denominator_vars = "c_newinc",
                starting_year = notification_maxyear,
                output_var_name = "c_rdx_pct") %>%
  # for HIV testing use TBHIV_for_aggregates dataset
  rbind(get_pct(TBHIV_for_aggregates,
                numerator_vars = "hivtest_pct_numerator",
                denominator_vars = "hivtest_pct_denominator",
                starting_year = notification_maxyear,
                output_var_name = "c_hivtest_pct")) %>%
  rbind(get_pct(notification,
                numerator_vars = c("new_labconf", "new_clindx", "ret_rel_labconf", "ret_rel_clindx"),
                denominator_vars = "c_newinc",
                starting_year = notification_maxyear,
                output_var_name = "c_pulm_pct")) %>%
  rbind(get_pct(notification,
                numerator_vars = c("new_labconf", "ret_rel_labconf"),
                denominator_vars = c("new_labconf", "new_clindx", "ret_rel_labconf", "ret_rel_clindx"),
                starting_year = notification_maxyear,
                output_var_name = "c_pulm_labconf_pct")) %>%

  rbind(get_pct(notification,
                numerator_vars = c("newrel_f15plus", "newrel_fu"),
                denominator_vars = c("c_new_014", "newrel_f15plus", "newrel_fu", "newrel_m15plus", "newrel_mu"),
                starting_year = notification_maxyear,
                output_var_name = "c_women_pct")) %>%
  rbind(get_pct(notification,
                numerator_vars = c("newrel_m15plus", "newrel_mu"),
                denominator_vars = c("c_new_014", "newrel_f15plus", "newrel_fu", "newrel_m15plus", "newrel_mu"),
                starting_year = notification_maxyear,
                output_var_name = "c_men_pct")) %>%

  # TB.HIV
  #
  rbind(get_pct(TBHIV_for_aggregates,
                numerator_vars = "hivtest_pos_pct_numerator",
                denominator_vars = "hivtest_pos_pct_denominator",
                starting_year = notification_maxyear,
                output_var_name = "c_hivpos_pct")) %>%
  rbind(get_pct(TBHIV_for_aggregates,
                numerator_vars = "hiv_art_pct_numerator",
                denominator_vars = "hiv_art_pct_denominator",
                starting_year = notification_maxyear,
                output_var_name = "c_art_pct")) %>%

  # preventive therapy
  rbind(get_pct(TBHIV_for_aggregates,
                numerator_vars = "hiv_ipt_pct_numerator",
                denominator_vars = "hiv_ipt_pct_denominator",
                starting_year = notification_maxyear,
                output_var_name = "c_prevtx_hiv_pct")) %>%


  # DST
  rbind(get_pct(dr_surveillance,
                numerator_vars = "r_rlt_new",
                denominator_vars = "pulm_labconf_new",
                starting_year = notification_maxyear,
                output_var_name = "c_rdst_new_pct")) %>%
  rbind(get_pct(dr_surveillance,
                numerator_vars = "r_rlt_ret",
                denominator_vars = "pulm_labconf_ret",
                starting_year = notification_maxyear,
                output_var_name = "c_rdst_ret_pct")) %>%

  # treatment outcomes -- recalculate them so as to get the aggregates too
  # For new/relapse cases start the time series at 2000; the old and new variables
  # are mutually exclusive so we can simplay take the sum of them
  rbind(get_pct(outcomes,
                numerator_vars = c("new_sp_cur", "new_sp_cmplt", "new_snep_cmplt", "newrel_succ"),
                denominator_vars = c("new_sp_coh", "new_snep_coh", "newrel_coh"),
                starting_year = 2000,
                output_var_name = "c_new_tsr")) %>%
  rbind(get_pct(outcomes,
                numerator_vars = "ret_nrel_succ",
                denominator_vars = "ret_nrel_coh",
                starting_year = outcome_maxyear,
                output_var_name = "c_ret_tsr")) %>%
  rbind(get_pct(outcomes,
                numerator_vars = "tbhiv_succ",
                denominator_vars = "tbhiv_coh",
                starting_year = outcome_maxyear,
                output_var_name = "c_tbhiv_tsr")) %>%
  # MDR and XDR outcomes are one year earlier -- need to filter out the final year, bit clunky
  rbind(get_pct(outcomes,
                numerator_vars = "mdr_succ",
                denominator_vars = "mdr_coh",
                starting_year = (outcome_maxyear - 1),
                output_var_name = "c_mdr_tsr") %>%  filter(year == (outcome_maxyear - 1) )) %>%

  # Finance
  rbind(get_pct(budget_expenditure,
                numerator_vars = "cf_tot_domestic",
                denominator_vars = "budget_tot",
                starting_year = report_year,
                output_var_name = "c_f_domestic_pct")) %>%
  rbind(get_pct(budget_expenditure,
                numerator_vars = c("cf_tot_gf", "cf_tot_usaid", "cf_tot_grnt"),
                denominator_vars = "budget_tot",
                starting_year = report_year,
                output_var_name = "c_f_international_pct")) %>%
  rbind(get_pct(budget_expenditure,
                numerator_vars = "gap_tot",
                denominator_vars = "budget_tot",
                starting_year = report_year,
                output_var_name = "c_f_unfunded_pct"))


# To avoid accumulation of rounding errors and questions about why rounded
# percentages don't add up to 100, calculate % children as 100 - %men - %women. Yeah, bit of a bodge, but easier
# to implement in SQL and JS than faffing about redistributing residuals ...
# Now, because I have data in long format have to jump through a couple of hoops.

adappt_temp_women <-
  filter(adappt_calc, indicator_code=="c_women_pct") %>%
  select(location_code, year, c_women_pct = value)

adappt_temp_men <-
  filter(adappt_calc, indicator_code=="c_men_pct")%>%
   select(location_code, year, c_men_pct = value)

adappt_temp_kids <-
  adappt_temp_women %>%
  inner_join(adappt_temp_men) %>%
  mutate(c_014_pct = ifelse(c_women_pct != "" & c_men_pct != "",
                           100 - as.numeric(c_women_pct) - as.numeric(c_men_pct),
                           NA))

# this next bit is necessary to avoid a false 0%
adappt_temp_kids_num <-
  notification %>%
  filter(year == notification_maxyear) %>%
  select(location_code = iso3,
         year,
         c_new_014)

adappt_temp_kids <-
  adappt_temp_kids %>%
  left_join(adappt_temp_kids_num) %>%
  mutate(value = ifelse(c_014_pct == 0 & c_new_014 > 0,
                           "<1",
                           c_014_pct),
         indicator_code = "c_014_pct") %>%
  select(indicator_code,
         location_code,
         year,
         value)

adappt_calc <-
  adappt_calc %>%
  rbind(adappt_temp_kids)

rm(list=ls(pattern = "^adappt_temp"))



# Calculate case notification rate
adappt_temp <-
  notification %>%
  select(iso3, g_whoregion, year, c_newinc) %>%
  filter(year >= 2000)

adappt_temp <-
  estimates_population %>%
  select(iso3, g_whoregion, year, e_pop_num) %>%
  inner_join(adappt_temp)

adappt_calc <-
  adappt_calc %>%
  rbind(get_rate(adappt_temp, numerator_var = "c_newinc", population_var = "e_pop_num", output_var_name = "c_newinc_100k"))

rm(adappt_temp)



# Need to replace aggregate budgets and percentages with cleaned version in dataset aggregated_finance_estimates


# First, remove the aggregate finance from the earlier datasets
adappt_data <-
  adappt_data %>%
  filter( !(indicator_code %in% c("budget_tot", "c_f_domestic_pct", "c_f_international_pct", "c_f_unfunded_pct") &
           location_code %in% c("AFR", "AMR", "EMR", "EUR", "SEA", "WPR", "global")))

adappt_calc <-
  adappt_calc %>%
  filter( !(indicator_code %in% c("budget_tot", "c_f_domestic_pct", "c_f_international_pct", "c_f_unfunded_pct") &
           location_code %in% c("AFR", "AMR", "EMR", "EUR", "SEA", "WPR", "global")))


# Now get the cleaned aggregates

adappt_fin_agg <-
  aggregated_finance_estimates %>%
  filter(year == report_year) %>%
  mutate(c_f_domestic_pct = display_cap_pct(domestic_funding, budget_total),
         c_f_international_pct = display_cap_pct(international_funding, budget_total),
         c_f_unfunded_pct = display_cap_pct(unfunded_gap, budget_total)) %>%

  # Convert aggregate budget total to millions
  mutate(budget_total = ifelse(budget_total > 1e6,
                               round(budget_total / 1e6, 0),
                               round(budget_total / 1e6, 1))) %>%

  select(location_code = group_name,
         year,
         c_f_domestic_pct,
         c_f_international_pct,
         c_f_unfunded_pct,
         budget_total) %>%

  # melt into long format
  gather(key="indicator_code",
         value="value",
         -location_code,
         -year)


# Combine files with value only and add empty lo, hi fields and then combine with the estimates file
adappt_output <-
  rbind(adappt_calc, adappt_data, adappt_sdg, adappt_fin_agg) %>%
  mutate(lo = NA,
         hi = NA) %>%
  rbind(adappt_est)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Exclude disbanded entities -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# restrict data to the 216 countries and areas from whom we collected data
# in 2019 (this deals with Serbia & Montenegro, Netherlands Antilles etc)

adappt_location_list <-
  data_collection %>%
  filter(datcol_year == report_year) %>%
  select(location_code = iso3)

# And don't forget to add the codes for WHO regions and globally
# Create a list of the regiona and aggregate names
group_codes <- read.table(textConnection("
location_code,g_whoregion,location_name, show_flag
global,,Global,
AFR,,WHO African Region,
AMR,,WHO/PAHO Region of the Americas,
EMR,,WHO Eastern Mediterranean Region,
EUR,,WHO European Region,
SEA,,WHO South-East Asia Region,
WPR,,WHO Western Pacific Region,"), header=TRUE, sep=",", as.is = TRUE)
closeAllConnections()

#Add the group location codes to the adappt list

adappt_location_list <-
  select(group_codes, location_code) %>%
  rbind(adappt_location_list)


adappt_output <-
  adappt_output %>%
  filter(location_code %in% adappt_location_list$location_code)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Save final dataset to CSV -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(x = adappt_output,
          file = paste(adappt_folder, "adappt_TB_data_",Sys.Date(),".csv",sep="") ,
          quote = FALSE,
          row.names = FALSE,
          na = "")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Produce and save location codes -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# Produce a list of location names

# Combine with official country names
adappt_locations <-
  report_country %>%
  mutate(show_flag = ifelse(g_whostatus == "M", TRUE, NA)) %>%
  select(location_code = iso3,
         g_whoregion,
         location_name = country,
         show_flag) %>%
  # restrict to countries reporting during the latest data collection round
  filter(location_code %in% adappt_location_list$location_code) %>%
  rbind(group_codes)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Save location list and names to CSV -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(x = adappt_locations,
          file = paste(adappt_folder, "adappt_location_",Sys.Date(),".csv",sep="") ,
          quote = 3,
          row.names = FALSE,
          na = "")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Data disaggregated by age and sex -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get the incidence estimates
adappt_agesex_agg <-
  aggregated_estimates_agesex %>%
  filter(measure == "inc" &
           unit == "num" &
           risk_factor== "all" &
           sex %in% c("m", "f") &
           year == notification_maxyear) %>%
  select(location_code = group_name,
         year,
         age_group,
         sex,
         e_inc_num = best)


adappt_agesex_country <-
  estimates_agesex %>%
  filter(measure == "inc" &
           unit == "num" &
           risk_factor== "all" &
           sex %in% c("m", "f") &
           year == notification_maxyear) %>%
  select(location_code = iso3,
         year,
         age_group,
         sex,
         e_inc_num = best)

adappt_agesex_inc <- rbind(adappt_agesex_country, adappt_agesex_agg)

rm(adappt_agesex_agg, adappt_agesex_country)

# Get the notifications

agesex_vars <- c("newrel_m04", "newrel_m514", "newrel_m014",
                 "newrel_m1524", "newrel_m2534", "newrel_m3544",
                 "newrel_m4554", "newrel_m5564", "newrel_m65",
                 "newrel_m15plus",
                 "newrel_f04", "newrel_f514", "newrel_f014",
                 "newrel_f1524", "newrel_f2534", "newrel_f3544",
                 "newrel_f4554", "newrel_f5564", "newrel_f65",
                 "newrel_f15plus"
                 )

adappt_agesex_notifs <-
  get_vars_and_aggregates(df = notification,
                          vars = agesex_vars,
                          starting_year = notification_maxyear,
                          ending_year = notification_maxyear,
                          flg_long = FALSE)

# Identify entities that don't have disaggegregated age groups for adults, only the 15 plus group

adappt_agesex_notifs <-
  adappt_agesex_notifs %>%
  mutate(flg_15plus_only = ifelse(is.na(newrel_m1524) &
                                    is.na(newrel_m2534) &
                                    is.na(newrel_m2534) &
                                    is.na(newrel_m3544) &
                                    is.na(newrel_m4554) &
                                    is.na(newrel_m5564) &
                                    is.na(newrel_m65) &
                                    is.na(newrel_f1524) &
                                    is.na(newrel_f2534) &
                                    is.na(newrel_f3544) &
                                    is.na(newrel_f4554) &
                                    is.na(newrel_f5564) &
                                    is.na(newrel_f65) &
                                    (
                                    !is.na(newrel_m15plus) | !is.na(newrel_f15plus)
                                    ),
                                  TRUE,
                                  FALSE
                                    ))

# Identify entities that don't have disaggegregated age groups for children, only the 0-14  group
adappt_agesex_notifs <-
  adappt_agesex_notifs %>%
  mutate(flg_014_only = ifelse(is.na(newrel_m04) &
                                is.na(newrel_m514) &
                                is.na(newrel_f04) &
                                is.na(newrel_f514) &
                                (
                                !is.na(newrel_m014) | !is.na(newrel_f014)
                                ),
                              TRUE,
                              FALSE
                                ))

# Flip to long format. Need to vary the operation based on whether we have disaggregated adults and children
# notifications or not

# 1. Fully disaggregated

addapt_agesex_notifs_disag <-
  adappt_agesex_notifs %>%
  filter(flg_15plus_only == FALSE & flg_014_only == FALSE) %>%
  # drop unnecessary fields
  select(-contains("15plus"), -contains("014")) %>%
  # rename the 65 age group to 65plus
  rename(newrel_m65plus = newrel_m65,
         newrel_f65plus = newrel_f65) %>%

  # now switch to long format
  pivot_longer(cols = starts_with("newrel_"),
             names_to = c("sex", "age_group"),
             # thanks to Hadley, help on pivot_longer icludes
             # and example of doing this with TB variables!
             names_pattern = "newrel_(.)(.*)",
             values_to = "notifs")

# 2. CHildren disaggregated, adults aggregated
#  (For 2019 report these were Gambia, Mozambique, Niue and Senegal)

addapt_agesex_notifs_adultsagg <-
  adappt_agesex_notifs %>%
  filter(flg_15plus_only == TRUE & flg_014_only == FALSE) %>%
  # drop unnecessary fields
  select(-contains("014"),
         -contains("1524"),
         -contains("2534"),
         -contains("3544"),
         -contains("4554"),
         -contains("5564"),
         -contains("65"),
         -contains("flg")) %>%

  # now switch to long format
  pivot_longer(cols = starts_with("newrel_"),
             names_to = c("sex", "age_group"),
             # thanks to Hadley, help on pivot_longer icludes
             # and example of doing this with TB variables!
             names_pattern = "newrel_(.)(.*)",
             values_to = "notifs")


# 3. CHildren aggregated, adults disaggregated
#  (For 2019 report these were Algeria, Turkmenistan and Yemen)

addapt_agesex_notifs_kidsagg <-
  adappt_agesex_notifs %>%
  filter(flg_15plus_only == FALSE & flg_014_only == TRUE) %>%
  # drop unnecessary fields
  select(-contains("04"),
         -contains("514"),
         -contains("15plus"),
         -contains("flg")) %>%
  # rename the 65 age group to 65plus
  rename(newrel_m65plus = newrel_m65,
         newrel_f65plus = newrel_f65)  %>%

  # now switch to long format
  pivot_longer(cols = starts_with("newrel_"),
             names_to = c("sex", "age_group"),
             # thanks to Hadley, help on pivot_longer icludes
             # and example of doing this with TB variables!
             names_pattern = "newrel_(.)(.*)",
             values_to = "notifs")

# 4. Children and adults disaggregated -- no entities with this combination in 2019 report

# Combine the long datasets

adappt_agesex_notifs <- rbind(addapt_agesex_notifs_disag,
                              addapt_agesex_notifs_adultsagg,
                              addapt_agesex_notifs_kidsagg)

# clean uo
rm(addapt_agesex_notifs_disag, addapt_agesex_notifs_adultsagg,addapt_agesex_notifs_kidsagg)

# Tweak strings in age_group for the notifications so they match those of the estimates

str_replace_pattern <- c("04" = "0-4",
                         "514" = "5-14",
                         "014" = "0-14",
                         "1524" = "15-24",
                         "2534" = "25-34",
                         "3544" = "35-44",
                         "4554" = "45-54",
                         "5564" = "55-64")

adappt_agesex_notifs <-
  adappt_agesex_notifs %>%
  mutate(age_group = str_replace_all(age_group, str_replace_pattern))

# Now join notifications to estimates -- an inner join should restrict estimates
# to the reported notification age groups

adappt_agesex <-
  adappt_agesex_notifs %>%
  inner_join(adappt_agesex_inc, by = c("location_code", "year", "age_group", "sex"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Save the disaggregation by age and sex to CSV -----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write.csv(x = adappt_agesex,
          file = paste(adappt_folder, "adappt_agesex_",Sys.Date(),".csv",sep="") ,
          quote = 3,
          row.names = FALSE,
          na = "")
