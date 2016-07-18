# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Maps in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Tom Hiatt
# 6 July 2012, updated July 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Links ------
#
# Link GTBR2015 map numbers to code sections
#
# 2.4  HIV prevalence in new cases: 2_4_hivprev_map
# 2.5  Incidence rates: 2_5_inc_map
# 2.12 Mortality from vital registration: 2_12_mort_src_map
# 2.13 Mortality rates: 2_13_mort_map
# 2.16 Availability of national electronic case-based databases: 2_16_err_map
#
# 4.10 Countries that had used bedaquiline: 4_10_bdq_map
#
# 6.2  Percentage of notified TB patients with known HIV status by country: 6_2_hivtest_map
# 6.5  Percentage of HIV-positive TB patients enrolled on ART: 6_5_HIVart_map
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This script does not use Philippe's who mapping package (see https://github.com/glaziou/whomap)
# instead it uses Tom's original version which allows us to add 'Not applicable' and 'No estimate'
# options to the legends, etc.
# Have therefore not switched to the simpler whomap package (yet)

source(file.path(scripts_folder, "WHO_map_functions.r"))

# Contrasting colors for easier re-layout
con.col <- c('red', 'blue', 'orange', 'green', 'purple', 'violet', 'sienna', 'dark orange')

ignore_for_now <- TRUE


if(!ignore_for_now) {
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 2 ------
# The burden of disease caused by TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2_1_sb_map -------------------------------------------------
# Countries which have completed the standards and benchmarks

mna <- e.t %>% filter(year==report_year-1, iso2 %in% c("BW","BI","CI","ET","GH","KE","LS","MG","MW","MZ","NG","RW","ZA","SS","SZ","UG","TZ","ZM","ZW","AF","EG","PK","SA", "AM","BY", "GE","TM","UA","UZ","BD","KR","ID","MM","NP","TH","MN","PH","VN")) %>% select(country, iso3, g_hbc22)

mcnt <- table(mna$g_hbc22)

mna$cat <- factor(mna$g_hbc22, levels=c("high", "low"), labels=c(paste0("High-burden countries (", mcnt[1], ")"), paste0("Other countries (", mcnt[2], ")")))

# map
sb_map <- WHOmap.print(mna,
"Figure 2.1 Countries that had completed a systematic assessment
of TB surveillance using the WHO TB surveillance checklist of
standards and benchmarks by August 2015", "",
                            na.label="",
                            copyright=FALSE,
                            colors=c('red', 'green'),
                            show=FALSE)

figsave(sb_map, mna, "f2_1_sb_map")


# 2_5_hivprev_map -------------------------------------------------
# HIV prevalence in new cases

mga <- subset(e.t, year==report_year-1, select=c('country', 'iso3', 'e_tbhiv_prct'))

mga$cat <- cut(mga$e_tbhiv_prct,
               c(0,5,20,50,Inf),
               c('0-4', '5-19', '20-49', '>=50'),
               right=FALSE)

# map
hivprev_map <- WHOmap.print(mga, paste("Figure 2.5 Estimated HIV prevalence in new and relapse TB cases,", report_year-1),
                            'HIV prevalence \nin new TB cases, \nall ages (%)',
                            na.label="No estimate",
                            copyright=FALSE,
                            colors=c('red', 'blue', 'orange', 'green'),
                            show=FALSE)

figsave(hivprev_map, mga, "f2_5_hivprev_map")


# 2_6_inc_map -------------------------------------------------
# Incidence rates

mfa <- subset(e.t, year==report_year-1, select=c('country', 'iso3', 'e_inc_100k'))

mfa$cat <- cut(round(mfa$e_inc_100k),
               c(0,10,20,50,125,300,500,Inf),
               c('0–9.9', '10–19', '20–49', '50–124', '125–299', '300–499', '>=500'),
               right=FALSE)

# map
inc_map <- WHOmap.print(mfa, paste("Figure 2.6 Estimated TB incidence rates,", report_year-1),
                        "Estimated new TB \ncases (all forms) per \n100 000 population \nper year",
                        na.label="No estimate",
                        copyright=FALSE,
                        colors=c('red', 'blue', 'orange', 'green', 'purple', 'violet', 'sienna'),
                        show=FALSE)

figsave(inc_map, mfa, "f2_6_inc_map")



# 2_2_inc_src_map -------------------------------------------------
# Incidence from country consultations

mfa <- subset(e.t, year==report_year-1, select=c("g_whoregion", 'country', 'iso3', 'source_inc'))

#mfa$var <- mfa$source_inc %in% c("Capture-recapture","High income","Survey")
mfa$var <- mfa$source_inc
mfa$var[mfa$iso3 %in% c('CHN', 'GMB', 'IDN', 'MMR', 'PAK', 'PHL', 'RWA', 'VNM')] <- 'Prevalence survey'
mfa$var[mfa$var=='Survey'] <- 'Prevalence survey'
mfa$var[!mfa$var %in% c('Prevalence survey', 'Capture-recapture', 'High income')] <- 'Case notifications'
mfa$var[mfa$iso3 %in% c('FRA', 'RUS')]  <- 'High income'
mfa$var[mfa$iso3 %in% c('EGY', 'NLD')]  <- 'Capture-recapture'
mfa$cat <- factor(mfa$var, levels=c('Case notifications','Prevalence survey', 'High income','Capture-recapture'), labels=c('Case notifications', 'Prevalence survey', 'High-income','Capture-recapture'))


# map
inc_src_map <- WHOmap.print(mfa,
"Figure 2.2. Main method used to estimate TB incidence(a)",
                             "Main method",
colors=con.col[1:4],
                             copyright=FALSE,
                             show=FALSE)

figsave(inc_src_map, mfa, "f2_2_inc_src_map")


# 2_15_mort_src_map -------------------------------------------------
# Mortality from vital registration

mea <- subset(e.t, year==report_year-1, select=c("g_whoregion", 'country', 'iso3', 'source_mort'))

# Mortality with VR data
meb <- subset(mea, source_mort != "indirect", select=c("g_whoregion", "country", "iso3", "source_mort"))
meb$cat <- factor("Estimated with \nVR data")

# map
mort_src_map <- WHOmap.print(meb,
                             paste0(
"Figure 2.15 Countries (in ", mort.color, ") for which TB
mortality is estimated using measurements from vital registration
systems (n=", nrow(meb)-2, ") and/or mortality surveys (n=2)"),
                             "[remove legend]",
                             low.color=mort.color,
                             copyright=FALSE,
                             show=FALSE)

figsave(mort_src_map, meb, "f2_15_mort_src_map")


# 2_17_mort_map -------------------------------------------------
# Mortality rates

mia <- subset(e.t, year==report_year-1, select=c('country', 'iso3', 'e_mort_exc_tbhiv_100k'))

mia$cat <- cut(mia$e_mort_exc_tbhiv_100k,
               c(0,1,4,10,20,40,Inf),
               c('0-0.9', '1-3.9', '4-9.9', '10-19', '20-39', '>=40'),
               right=FALSE)

# map
mort_map <- WHOmap.print(mia,
                         paste(
"Figure 2.17 Estimated TB mortality rates excluding TB deaths among
HIV-positive \npeople,", report_year-1),
                         "Estimated TB \ndeaths per \n100 000 population",
                         na.label="No estimate",
                         copyright=FALSE,
                         colors=c('red', 'blue', 'orange', 'green', 'purple', 'brown'),
                         show=FALSE)

figsave(mort_map, mia, "f2_17_mort_map")


# 2_xx_err_map -------------------------------------------------
# Electronic recording and reporting

mha <- subset(s, year %in% (report_year-1):(report_year-2), select=c('country', 'year', 'iso3', 'caseb_err_nat'))

# Take last year's answer if unreported
for(cnty in unique(mha$country)) {
  if(is.na(mha[mha$country==cnty & mha$year==report_year-1, 'caseb_err_nat'])) mha[mha$country==cnty & mha$year==report_year-1, 'caseb_err_nat'] <- mha[mha$country==cnty & mha$year==report_year-2, 'caseb_err_nat']
}

mhb <- subset(mha, year==report_year-1)

# Western europe fixes
if(report_year==2013){
  mhb[mhb$iso3 %in% c('AUT', 'CHE', 'CYP', 'DNK', 'GRC', 'GRL', 'ITA', 'LUX', 'UZB', 'KOR'), 'caseb_err_nat'] <- 42
  mhb[mhb$iso3 %in% c('KGZ'), 'caseb_err_nat'] <- 0
  mhb[mhb$iso3 %in% c('NPL'), 'caseb_err_nat'] <- 43
}


mhb$cat <- factor(mhb$caseb_err_nat, levels=c(42, 43, 0), labels=c('All TB patients', 'MDR-TB patients only', 'None' ))

# map
mhc <- WHOmap.print(mhb,
                    paste("Availability of national electronic case-based databases of TB patients,", report_year-1),
                    '',
                    colors=c('dark green', 'light green', 'white'),
                    copyright=FALSE,
                    show=FALSE)

figsave(mhc, mhb, "f2_xx_err_map")

# 2_21_age_map -------------------------------------------------
# Countries able to report age disaggregated data

# find countries which did not report yet this year.
mjc <- n.t %>% filter(year==report_year-1, is.na(c_newinc)) %>% select(iso3)

mja <- n.t %>%
  # Get the last two years
  filter(year %in% c(report_year-2,report_year-1), !is.na(c_newinc)) %>%
  # Get all the age-sex variables
  select(country, year, iso3, rel_in_agesex_flg, matches("newrel_[m|f|sex]")) %>%
  # Remove the variables with age unknown
  select(-newrel_mu, -newrel_fu, -newrel_sexunkageunk) %>%
  # Rename values of last two years (for reshaping later)
  mutate(year=ifelse(year==report_year-1, "latest", "previous")) %>%
  # Replace with previous year if latest year missing.
  gather(vari, value, rel_in_agesex_flg:newrel_sexunk15plus) %>% spread(year, value) %>%
  # but only if in the list of non-reporters (to exclude countries which reported the previous year, but not the latest)
  mutate(value=ifelse(is.na(latest) & iso3 %in% mjc$iso3, previous, latest)) %>% select(-latest, -previous) %>%
  # Get back to normal with only one row per country
  spread(variable, value)

# Check if different variables are added or removed in the future.
if(ncol(mja)!=27) warning("For map 2.21, the age sex variables have changed. Check select() code.")

# Check if all age vars are missing
mja$age <- sum_of_row(mja[4:ncol(mja)])

mja$cat <- factor(ifelse(is.na(mja$age), "No age disaggregation", ifelse(mja$rel_in_agesex_flg==1, "Age disaggregation (new and relapse)", "Age disaggregation (new only)")))

mjb <- mja %>% select(country, iso3, cat)

# map
age_map <- WHOmap.print(mjb,
                    paste("Figure 2.21 Reporting of new and relapse TB case notifications disaggregated by age,", report_year-1),
                    '',
                    colors=c('dark green', 'light green', 'purple'),
                    copyright=FALSE,
                    show=FALSE)

figsave(age_map, mjb, "f2_21_age_map")

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Diagnosis and treatment of TB, HIV-associated TB and drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 4_2_pct_children_map -------------------------------------------------

kids_data <- notification %>%
             filter(year == report_year - 1) %>%
             select(iso3,
                    c_new_014,
                    newrel_m15plus,
                    newrel_mu,
                    newrel_f15plus,
                    newrel_sexunk15plus,
                    newrel_fu)

#calculate % of kids in agesex data
kids_data$c_agesex_tot <- kids_data %>%
                          select(c_new_014:newrel_fu) %>%
                          sum_of_row()

kids_data$kids_pct <- ifelse(is.na(kids_data$c_new_014) | NZ(kids_data$c_agesex_tot) == 0, NA,
                              kids_data$c_new_014 * 100 / kids_data$c_agesex_tot)



kids_data$cat <- cut(kids_data$kids_pct,
                     c(0, 2.0, 5.0, 10.0, Inf),
                     c('0-1.9%', '2-4.9%', '5-9.9%', '>=10%'),
               right=FALSE)

# produce the map
kids_map <- WHOmap.print(kids_data,
                        paste("Figure 4.2 Percentage new and relapse TB cases that were children,", report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           show=FALSE)

figsave(kids_map,
        select(kids_data,
                         iso3,
                         kids_pct,
                         cat),
        "f4_2_pct_children_map")

# Clean up (remove any objects with their name beginning with 'agesex')
rm(list=ls(pattern = "^kids"))



# 4_3_pct_rapid_dx_map -------------------------------------------------

rdx_data <- notification %>%
             filter(year == report_year - 1) %>%
             select(iso3,
                    rdx_data_available,
                    newinc_rdx,
                    c_newinc,
                    rdxsurvey_newinc_rdx,
                    rdxsurvey_newinc)

#calculate % of cases which had rapid diagnostics
rdx_data <- within(rdx_data, {

  rdx_pct <- NA

  # Check if said from routine surveillance
  rdx_pct <- ifelse(NZ(rdx_data_available)==60,
                    ifelse((is.na(newinc_rdx) | NZ(c_newinc) == 0),
                             NA,
                             newinc_rdx * 100 / c_newinc),
                    rdx_pct)


  # Check if said from survey of patient records
  rdx_pct <- ifelse(NZ(rdx_data_available)==61,
                    ifelse((is.na(rdxsurvey_newinc_rdx) | NZ(rdxsurvey_newinc) == 0),
                             NA,
                             rdxsurvey_newinc_rdx * 100 / rdxsurvey_newinc),
                    rdx_pct)

})


rdx_data$cat <- cut(rdx_data$rdx_pct,
                     c(0, 25, 50, 75, Inf),
                     c('0-24.9%', '25-49.9%', '50-74.9%', '>=75%'),
               right=FALSE)

# produce the map
rdx_map <- WHOmap.print(rdx_data,
                        paste("Figure 4.3 Percentage of new and relapse TB cases tested\nusing WHO-approved rapid diagnostics at the time of diagnosis,", report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           show=FALSE)

figsave(rdx_map,
        select(rdx_data,
                         iso3,
                         rdx_pct,
                         cat),
        "f4_3_pct_rapid_dx_map")

# Clean up (remove any objects with their name beginning with 'rdx')
rm(list=ls(pattern = "^rdx"))


# 4_5_pct_bacconf_map -------------------------------------------------


bacconf_data <- notification %>%
                filter(year == report_year - 1) %>%
                select(iso3,
                      new_labconf, new_clindx,
                      ret_rel_labconf, ret_rel_clindx)


#calculate % of pulmonary cases with bac confirmation
bacconf_data$pulm_tot <- bacconf_data %>%
                          select(new_labconf:ret_rel_clindx)%>%
                          sum_of_row()

bacconf_data$pulm_bacconf_tot <- bacconf_data %>%
                                  select(new_labconf, ret_rel_labconf) %>%
                                  sum_of_row()


bacconf_data$bacconf_pct <- ifelse(is.na(bacconf_data$pulm_bacconf_tot) | NZ(bacconf_data$pulm_tot) == 0, NA,
                                  bacconf_data$pulm_bacconf_tot * 100 / bacconf_data$pulm_tot)




bacconf_data$cat <- cut(bacconf_data$bacconf_pct,
                     c(0, 25, 50, 75, Inf),
                     c('0-24.9%', '25-49.9%', '50-74.9%', '>=75%'),
               right=FALSE)

# produce the map
bacconf_map <- WHOmap.print(bacconf_data,
                        paste("Figure 4.5 Percentage of new and relapse pulmonary TB cases with bacteriological confirmation,", report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           show=FALSE)

figsave(bacconf_map,
        select(bacconf_data,
                         iso3,
                         bacconf_pct,
                         cat),
        "f4_5_pct_bacconf_map")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))



# 4_7_pct_HIV_status_map -------------------------------------------------


hivstatus_data <- notification %>%
                  filter(year == report_year - 1) %>%
                  select(iso3,
                         c_newinc,
                         newrel_hivtest) %>%

                  # Calculate % with known HIV status
                  mutate(hivstatus_pct = ifelse(is.na(newrel_hivtest) | NZ(c_newinc) == 0, NA,
                                                newrel_hivtest * 100 / c_newinc))



hivstatus_data$cat <- cut(hivstatus_data$hivstatus_pct,
                     c(0, 25, 50, 75, Inf),
                     c('0-24.9%', '25-49.9%', '50-74.9%', '>=75%'),
               right=FALSE)


# produce the map
hivstatus_map <- WHOmap.print(hivstatus_data,
                        paste("Figure 4.7 Percentage of new and relapse TB cases with documented HIV status,", report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           show=FALSE)

figsave(hivstatus_map,
        select(hivstatus_data,
                         iso3,
                         hivstatus_pct,
                         cat),
        "f4_7_pct_HIV_status_map")

# Clean up (remove any objects with their name beginning with 'hivstatus')
rm(list=ls(pattern = "^hivstatus"))



# 4_15_pct_BMU_community_map -------------------------------------------------
# 4.15 Percentage of BMUs with community referral or treatment adherence support, 2015

comm_data <- strategy %>%
              filter(year==report_year - 1) %>%
              select(iso3,
                     bmu,
                     bmu_community_impl,
                     community_data_available) %>%
              mutate(comm_pct = ifelse(is.na(bmu_community_impl) | NZ(bmu) == 0,
                                       NA,
                                       bmu_community_impl * 100 / bmu))


comm_data$cat <- cut(comm_data$comm_pct,
                     c(0, 25, 50, 75, Inf),
                     c('0-24.9%', '25-49.9%', '50-74.9%', '>=75%'),
               right=FALSE)


# produce the map
comm_map <- WHOmap.print(comm_data,
                        paste("Figure 4.15 Percentage of BMUs with community referral or treatment adherence support,", report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           show=FALSE)

figsave(comm_map,
        select(comm_data,
               iso3,
               comm_pct,
               cat),
        "f4_15_pct_BMU_community_map")



# Try an alternative map focussing on data availability
comm_availability <- data_collection %>%
                      filter(datcol_year == report_year) %>%
                      select(iso3,
                             dc_engage_community_display) %>%
                      inner_join(comm_data) %>%
                      select(-cat)


comm_availability <- within(comm_availability, {

  cat <- ifelse(dc_engage_community_display == 0,
                "notapp",
                NA)


  cat <- ifelse(is.na(cat) & (NZ(bmu) == 0 | NZ(bmu_community_impl) == 0),
                "nodata",
                cat)

  cat <- ifelse(is.na(cat) & community_data_available == 1,
                "detaileddata",
                cat)

  cat <- ifelse(is.na(cat) &
                  bmu > 0 &
                  bmu_community_impl > 0 &
                  bmu_community_impl * 100.00 / bmu > 50,
                "nodetails_51_100",
                cat)

  cat <- ifelse(is.na(cat) &
                  bmu > 0 &
                  bmu_community_impl > 0 &
                  bmu_community_impl * 100.00 / bmu <= 50,
                "nodetails_0_50",
                cat)

  cat <- factor(cat,
                levels = c("detaileddata",
                           "nodetails_51_100",
                           "nodetails_0_50",
                           "nodata",
                           "notapp"),
                labels = c("Detailed data available",
                           "No details, activity in\n51 - 100% of BMUs",
                           "No details, activity in\n0 - 50% of BMUs",
                           "No data",
                           "Not applicable"))

})


# produce the map
comm_map <- WHOmap.print(comm_availability,
                        paste("Figure 4.15 (alternative) Availability of data on BMUs with community referral or treatment adherence support,", report_year-1),
                           "Availability",
                           copyright=FALSE,
                           colors=c('green', 'blue', 'yellow', 'white', 'grey75'),
                           show=FALSE)


figsave(comm_map,
        select(comm_data,
               iso3,
               comm_pct,
               cat),
        "f4_15alt_pct_BMU_community_map")


# Clean up (remove any objects with their name beginning with 'comm')
rm(list=ls(pattern = "^comm"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# TB prevention services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.1 Availability of data on the number of children aged <5 years who were
# household contacts of bacteriologically confirmed TB cases and were started on
# TB preventive treatment, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


kids_data <-  strategy %>%
              filter(year==report_year - 1) %>%
              select(iso3,
                     prevtx_data_available)

# Assign categories
kids_data <- within(kids_data, {

  cat <- ifelse(prevtx_data_available==0,"Number not available", NA)
  cat <- ifelse(prevtx_data_available==60,"Number available from routine surveillance", cat)
  cat <- ifelse(prevtx_data_available==61,"Number estimated from a survey", cat)
  cat <- factor(cat)

})


# produce the map
kids_map <- WHOmap.print(kids_data,
                        paste("Figure 5.1 Availability of data on the number number of children aged <5 years who were,",
                              "\nhousehold contacts of bacteriologically confirmed TB cases and were started on",
                              "\nTB preventive treatment,",
                              report_year-1),
                           legend.title = "Country response",
                           copyright=FALSE,
                           colors=c('green', 'blue', 'yellow'),
                           na.label="No response",
                           show=FALSE)

figsave(kids_map,
        kids_data,
        "f5_1_kids_prevtx_map")


# Clean up (remove any objects with their name beginning with 'kids')
rm(list=ls(pattern = "^kids"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.2 Incident (notification?) rate ratio of TB among healthcare workers compared with the general population, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

hcw_notif_hcw <-  strategy %>%
                  filter(year == report_year - 1) %>%
                  select(iso3,
                         hcw_tb_infected,
                         hcw_tot)

hcw_pop_adults <- estimates_population %>%
                  filter(year == report_year - 1) %>%
                  select(iso3,
                         e_pop_15plus)

hcw_notif_adults <- notification %>%
                    filter(year == report_year - 1) %>%
                    mutate(c_15plus = NZ(newrel_f15plus) +
                                      NZ(newrel_m15plus) +
                                      NZ(newrel_sexunk15plus)) %>%
                    select(iso3,
                           c_15plus) %>%
                    inner_join(hcw_pop_adults)


hcw_data <-  hcw_notif_adults %>%
                    inner_join(hcw_notif_hcw) %>%

                    # Calculate notification rate ratio
                    # Use as.numeric() to avoid integer overflow
                    # Exclude Kenya this year because of implausible denominator
                    mutate(nrr = ifelse(NZ(hcw_tot) > 0 &
                                        NZ(c_15plus) > 0 &
                                        iso3 != 'KEN',
                                        (as.numeric(hcw_tb_infected) * as.numeric(e_pop_15plus))
                                        /
                                        (as.numeric(hcw_tot) * c_15plus),
                                        NA)) %>%
                    select(iso3,
                           nrr)


hcw_data$cat <- cut(hcw_data$nrr,
                     c(0, 1, 2, 3, Inf),
                     c('0-0.9', '1-1.9%', '2-2.9%', '>=3'),
               right=FALSE)


# produce the map
hcw_map <- WHOmap.print(hcw_data,
                        paste("Figure 5.2 Notification rate ratio of TB among healthcare workers\ncompared with the general population,", report_year-1),
                           "Notification rate ratio",
                           copyright=FALSE,
                           colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           show=FALSE)

figsave(hcw_map,
        hcw_data,
        "f5_2_hcw_notf_rate_ratio")

# Clean up (remove any objects with their name beginning with 'hcw')
rm(list=ls(pattern = "^hcw"))

stop("

     >>>>>>>>>>
     Stopping here so can do the rest manually!
     <<<<<<<<<<<<")




# 3_7_ltbipolicy_map -------------------------------------------------
# Countries with national LTBI policy

ltbipolicy <- readWorksheetFromFile(file.path(rdata_folder, "Extra data", "YH", "LTBI_page_maps2207..xlsx"), sheet="policy") %>%
  mutate(cat=factor(Availability.on.national.policy.on.LTBI,
                    levels=c("National policy on LTBI available ",
                             "No national policy on LTBI",
                             "No data",
                             "Estimated TB incidence>=100 or low/lower middle income"),
                    labels=c("National policy on LTBI \navailable",
                             "No national policy on LTBI",
                             "No data",
                             "High-burden countries"))
  )


ltbipolicy_map <- WHOmap.print(ltbipolicy,
                               paste("Reported national policies on LTBI,", report_year-1),
                               "",
                               colors=con.col[1:4],
                               copyright=FALSE,
                               show=FALSE)

figsave(ltbipolicy_map, ltbipolicy, "f3_7_ltbipolicy_map")


# 3_6_ltbisurvey_map -------------------------------------------------
# Countries included in the LTBI survey

ltbisurvey <- readWorksheetFromFile(file.path(rdata_folder, "Extra data", "YH", "LTBI_page_maps.xlsx"), sheet=1) %>%
  mutate(cat=factor(primary_target,
                    levels=c("Estimated TB incidence<100 and high/upper-middle income"),
                    labels=c("Estimated TB incidence<100 or \nhigh/upper-middle income"))
  )


ltbisurvey_map <- WHOmap.print(ltbisurvey,
                                 "The 113 upper-middle-income and high-income countries with \nan estimated incidence rate of less than \n100 per 100 000 population that are the \nprimary audience for 2015 WHO guidelines on the \nmanagement of latent TB infection",
                                 "[Remove legend]",
                                 copyright=FALSE,
                                 show=FALSE)

figsave(ltbisurvey_map, ltbisurvey, "f3_6_ltbisurvey_map")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 4_10_bdq_map -------------------------------------------------
# Countries using bedaquiline

mia <- subset(n, year %in% (report_year-1):(report_year-2), select=c('country', 'year', 'iso3', 'mdrxdr_bdq_used'))

# Take last year's answer if unreported
for(cnty in unique(mia$country)) {
  if(is.na(mia[mia$country==cnty & mia$year==report_year-1, 'mdrxdr_bdq_used'])) mia[mia$country==cnty & mia$year==report_year-1, 'mdrxdr_bdq_used'] <- mia[mia$country==cnty & mia$year==report_year-2, 'mdrxdr_bdq_used']
}

mib <- subset(mia, year==report_year-1)

#  fixes
if(report_year==2014){
  mib[mib$iso3 %in% c('BEL', "ITA"), 'mdrxdr_bdq_used'] <- 1
  mib[mib$iso3 %in% c('ARG'), 'mdrxdr_bdq_used'] <- 0
  warning("some hard coding going on with the BDQ map.")
}

# Leave off unknown category
mib$cat1 <- ifelse(mib$mdrxdr_bdq_used==3, NA, mib$mdrxdr_bdq_used)

mib$cat <- factor(mib$cat1, levels=c(1, 0, 3), labels=c('Yes', 'No', 'Unknown' ))

# map
bdq_map <- WHOmap.print(mib,
                        paste("Countries that had used bedaquiline for the treatment of M/XDR-TB as part of expanded access, \ncompassionate use or under normal programmatic conditions by the end of", report_year-1),
                        '',
                        colors=c('dark orange', 'green', 'blue'),
                        copyright=FALSE,
                        show=FALSE)

figsave(bdq_map, mib, "f4_10_bdq_map")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# Diagnostics and laboratory strengthening
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 5_1_dst_map -------------------------------------------------
# dstality rates

dst_dta <- readWorksheetFromFile(file.path(rdata_folder, "Extra data", "WvG", "andrewistheman.xls"), sheet=1) %>% select(iso3, value) %>% filter(value!=5) %>% mutate(cat=factor(value, labels=c("1st- and 2nd-line DST", "1st-line DST only", "Xpert MTB/RIF only", "No capacity"))) %>% inner_join(subset(n, year==report_year-1, c(iso3, country)))

# map
dst_map <- WHOmap.print(dst_dta, "Figure 5.1 Global capacity for drug-susceptibility testing (DST), 2014(a)",
                        "", na.label = "No data",
                        copyright=FALSE,
                        colors=c('red', 'blue', 'green', 'purple'),
                        show=FALSE)

figsave(dst_map, dst_dta, "f5_1_dst_map")



# 5_2_xpertcart_map -------------------------------------------------
# numbers of xpert cartridges procured

xpert_cart <- readWorksheetFromFile(file.path(rdata_folder, "Extra data", "WvG", "Figure 5.2 accompanying data.xlsx"), sheet=1) %>%
  mutate(cart1=ifelse(is.na(cartridges), 0, cartridges), # NA same as 0
         cart2=as.numeric(cart1)/1000,
         cat=cut(cart2,
                   c(0, 5, 50, 100, 300, Inf),
                   c("0-4", "5-49", "50-99", "100-299", ">=300"),
                   right=FALSE)
         )


xpert_cart_map <- WHOmap.print(xpert_cart,
                        paste("Xpert MTB/RIF cartridge procurements in", report_year-1, "at concessional prices"),
                        "Xpert MTB/RIF \ncartridges \nprocured in 2014 \n(thousands)",
                        colors=con.col[1:5],
                        copyright=FALSE,
                        na.label="Not eligible for \npreferential pricing",
                        show=FALSE)

figsave(xpert_cart_map, xpert_cart, "f5_2_xpertcart_map")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 6 ------
# Addressing the co-epidemics of TB and HIV
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 6_2_hivtest_map -------------------------------------------------

mc <- subset(tbhiv, year==report_year-1, select=c(country, iso2, iso3, g_whoregion, hivtest_pct_numerator, hivtest_pct_denominator))
mc$hivtest_prct <- round(mc$hivtest_pct_numerator/mc$hivtest_pct_denominator * 100)

# Fix and footnote for Russian Federation
mc[mc$country=='Russian Federation', 'hivtest_prct'] <- round(subset(n, iso2=='RU' & year==report_year-1, hivtest_p) / subset(n, iso2=='RU' & year==report_year-1, c_newunk) * 100)

warning("Russian Federation modification for the HIV test map is still in place. Delete this message when no longer applicable.")


mc$cat <- NA
mc$cat[mc$hivtest_prct >= 75] <- ">=75"
mc$cat[mc$hivtest_prct < 75] <- '50-74'
mc$cat[mc$hivtest_prct < 50] <- '15-49'
mc$cat[mc$hivtest_prct < 15] <- '0-14'

mc$cat <- as.factor(mc$cat)
mc$cat <- factor(mc$cat, levels = c("0-14", "15-49",  "50-74", ">=75"))

# HIV testing map (ugly colors are for easier design)

hivtest_map <- WHOmap.print(mc,
                            paste("Percentage of notified TB patients with known HIV status by country,", report_year-1, "(a)"),
                            "Percentage of \nnotified TB patients",
                            copyright=FALSE,
                            colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                            show=FALSE)

# Footnote: Data for the Russian Federation are for new TB patients in the civilian sector only.

figsave(hivtest_map, mc, "f6_2_hivtest_map")


# 6_5_HIVart_map -------------------------------------------------

# Changed denominator in 2014 report from hiv_art_pct_denominator to e_inc_tbhiv_num

mca <- subset(tbhiv, year==report_year-1, select=c(country, iso2, iso3, g_whoregion, hiv_art_pct_numerator, hiv_art_pct_denominator))

mca <- merge(mca, subset(e.t, year==report_year-1, select=c('country', 'year', 'e_inc_tbhiv_num')))

mca$hivart_prct <- round(mca$hiv_art_pct_numerator/mca$e_inc_tbhiv_num * 100)

mca$cat <- cut(mca$hivart_prct,
               c(0, 25, 50, 75, Inf),
               c('0-24', '25-49', '50-74', '75-100'),
               right=FALSE)

# map

HIVart_map <- WHOmap.print(mca,
                           paste("Number of HIV-positive TB patients on ART as a percentage of estimated HIV-positive incident \nTB cases,", report_year-1, "(a)"),
                           "Percentage",
                           copyright=FALSE,
                           colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           show=FALSE)

# Footnote: The numerator (i.e. all notified HIV-positive TB cases on ART) includes all notified new, relapse and
# non-relapse retreatment cases. The denominator (i.e.
# estimated HIV-positive incident TB cases) includes new and relapse cases only.

figsave(HIVart_map, mca, "f6_5_HIVart_map")


# END ======================
