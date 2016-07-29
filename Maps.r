# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Maps in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Tom Hiatt
# 6 July 2012,
# Last updated Hazim Timimi, July 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This script does not use Philippe's who mapping package (see https://github.com/glaziou/whomap)
# instead it uses Tom's original version which allows us to add 'Not applicable' and 'No estimate'
# options to the legends, etc.
# Have therefore not switched to the simpler whomap package (yet)

source(file.path(scripts_folder, "WHO_map_functions.r"))

# Contrasting colors for easier re-layout
con.col <- c('red', 'blue', 'orange', 'green', 'purple', 'violet', 'sienna', 'dark orange')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Diagnosis and treatment of TB, HIV-associated TB and drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.2  ------
# Percentage new and relapse TB cases that were children (aged < 15), 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


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
                        paste("Figure 4.2 Percentage new and relapse TB cases that were children (aged < 15),", report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

figsave(kids_map,
        select(kids_data,
                         iso3,
                         kids_pct,
                         cat),
        "f4_2_pct_children_map")

# Clean up (remove any objects with their name beginning with 'agesex')
rm(list=ls(pattern = "^kids"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.3  ------
# Percentage of new and relapse TB cases tested using WHO-approved rapid diagnostics at the time of diagnosis, 2015
# Drop?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


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
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

figsave(rdx_map,
        select(rdx_data,
                         iso3,
                         rdx_pct,
                         cat),
        "f4_3_pct_rapid_dx_map")

# Clean up (remove any objects with their name beginning with 'rdx')
rm(list=ls(pattern = "^rdx"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.5  ------
# Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
                     c(0, 50, 65, 80, Inf),
                     c('0-49.9%', '50-64.9%', '65-79.9%', '>=80%'),
               right=FALSE)

# produce the map
bacconf_map <- WHOmap.print(bacconf_data,
                        paste("Figure 4.5 Percentage of new and relapse pulmonary TB cases with bacteriological confirmation,", report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

figsave(bacconf_map,
        select(bacconf_data,
                         iso3,
                         bacconf_pct,
                         cat),
        "f4_5_pct_bacconf_map")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.7  ------
# Percentage of new and relapse TB cases with documented HIV status,", 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


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
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

figsave(hivstatus_map,
        select(hivstatus_data,
                         iso3,
                         hivstatus_pct,
                         cat),
        "f4_7_pct_HIV_status_map")

# Clean up (remove any objects with their name beginning with 'hivstatus')
rm(list=ls(pattern = "^hivstatus"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.15  ------
# Percentage of BMUs with community referral or treatment adherence support, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


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
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
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
# Figure 5.1 -------
# Availability of data on the number of children aged <5 years who were
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
# Figure 5.2 ---------
# Notification rate ratio of TB among healthcare workers compared with the general population, 2015
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
                        paste("Figure 5.2 Notification rate ratio of TB among healthcare workers\ncompared with the general adult population,", report_year-1),
                           "Notification rate ratio",
                           copyright=FALSE,
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

figsave(hcw_map,
        hcw_data,
        "f5_2_hcw_notf_rate_ratio")

# Clean up (remove any objects with their name beginning with 'hcw')
rm(list=ls(pattern = "^hcw"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 6 ------
# Universal health coverage, social protection and social determinants
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 6.1 ---------
# Government spending on health, as a percentage of gross domestic product (GDP), 2014
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get data from external spreadsheet supplied by Ines
govspend_data <- readWorksheetFromFile(file.path(rdata_folder,
                                            "Extra data",
                                            "IGB",
                                            "Fig6_IGB_28 Jul 2016.xlsx"),
                                  sheet="6_1_map_data")

# re-calculate the categories
govspend_data$cat <- cut(govspend_data$prop,
                     c(0, 4, 6, 10, Inf),
                     c('<4%', '4-5.9%', '6-9.9%', '>=10%'),
               right=FALSE)


# produce the map
govspend_map <- WHOmap.print(govspend_data,
                        paste("Figure 6.1 Government spending on health, as a percentage of gross domestic product (GDP),", report_year-2),
                           "Percentage\nof GDP",
                           copyright=FALSE,
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

figsave(govspend_map,
        select(govspend_data,
               iso3,
               prop,
               cat),
        "f6_1_pct_gov_health_spend_map")

# Clean up (remove any objects with their name beginning with 'govspend')
rm(list=ls(pattern = "^govspend"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 6.3 ---------
# Out-of-pocket expenditures as a percentage of total health expenditures, 2014
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get data from external spreadsheet supplied by Ines
oop_data <- readWorksheetFromFile(file.path(rdata_folder,
                                            "Extra data",
                                            "IGB",
                                            "Fig6_IGB_28 Jul 2016.xlsx"),
                                  sheet="6_3_map_data")

# re-calculate the categories
oop_data$cat <- cut(oop_data$percentage,
                     c(0, 15.5, 30, 45, Inf),
                     c('<=15%', '16-29%', '30-44%', '>=45%'),
               right=FALSE)

# produce the map
oop_map <- WHOmap.print(oop_data,
                        paste("Figure 6.3 Out-of-pocket expenditures as a percentage of total health expenditures,", report_year-2),
                           "Percentage",
                           copyright=FALSE,
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

figsave(oop_map,
        select(oop_data,
               iso3,
               percentage,
               cat),
        "f6_3_pct_oop_map")

# Clean up (remove any objects with their name beginning with 'oop')
rm(list=ls(pattern = "^oop"))



stop("

     >>>>>>>>>>
     Stopping here so can do the rest manually!
     <<<<<<<<<<<<")

