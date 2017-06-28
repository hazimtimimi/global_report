# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Maps in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Tom Hiatt
# 6 July 2012,
# Last updated Hazim Timimi, July 2017
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
# Figure 4.3  ------
# Percentage new and relapse TB cases that were children (aged < 15), 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


kids_data <- notification %>%
             filter(year >= report_year - 2) %>%
             select(iso3,
                    year,
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

# Find the countries with empty data for latest year and see if there are data for the previous year
kids_prev_year_data <- kids_data %>%
                       filter(year == report_year - 1 & is.na(kids_pct)) %>%
                       select(iso3) %>%
                       inner_join(filter(kids_data, year == report_year - 2)) %>%
                       filter(!is.na(kids_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available

kids_data_combined <- kids_data %>%
                       filter(year == report_year - 1) %>%
                        anti_join(kids_prev_year_data, by= "iso3") %>%
                        rbind(kids_prev_year_data)


# produce the map
kids_map <- WHOmap.print(kids_data_combined,
                        paste0("Figure 4.3 Percentage new and relapse TB cases that were children (aged < 15), ",
                               report_year-1,
                               "(a)"),
                           "Percentage",
                           copyright=FALSE,
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

# Add footnote about using earlier data for some countries
kids_foot <- paste("(a)",
                    report_year - 2,
                    "data were used for ",
                    nrow(kids_prev_year_data),
                    "countries")



kids_map <- arrangeGrob(kids_map, bottom = textGrob(kids_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))



figsave(kids_map,
        select(kids_data_combined,
                         iso3,
                         kids_pct,
                         cat),
        "f4_3_pct_children_map")

# Clean up (remove any objects with their name beginning with 'kids')
rm(list=ls(pattern = "^kids"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.5  ------
# Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bacconf_data <- notification %>%
                filter(year  >= report_year - 2) %>%
                select(iso3,
                       year,
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


# Find the countries with empty data for latest year and see if there are data for the previous year
bacconf_prev_year_data <- bacconf_data %>%
                           filter(year == report_year - 1 & is.na(bacconf_pct)) %>%
                           select(iso3) %>%
                           inner_join(filter(bacconf_data, year == report_year - 2)) %>%
                           filter(!is.na(bacconf_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
bacconf_data_combined <- bacconf_data %>%
                          filter(year == report_year - 1) %>%
                          anti_join(bacconf_prev_year_data, by= "iso3") %>%
                          rbind(bacconf_prev_year_data)


# produce the map
bacconf_map <- WHOmap.print(bacconf_data_combined,
                        paste0("Figure 4.5 Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, ",
                              report_year-1,
                               "(a)"),
                           "Percentage",
                           copyright=FALSE,
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

# Add footnote about using earlier data for some countries
bacconf_foot <- paste("(a)",
                      report_year - 2,
                      "data were used for ",
                      nrow(bacconf_prev_year_data),
                      "countries.")


bacconf_map <- arrangeGrob(bacconf_map, bottom = textGrob(bacconf_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))



figsave(bacconf_map,
        select(bacconf_data_combined,
                         iso3,
                         bacconf_pct,
                         cat),
        "f4_5_pct_bacconf_map")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.6  (NEW MAP FOR 2017 REPORT) ------
# Percentage of new and relapse TB cases tested using a WHO-recommended
# rapid diagnostic as the initial diagnostic test, 2016

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.7  (NEW MAP FOR 2017 REPORT) ------
# Percentage of extrapulmonary cases among new and relapse TB cases, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.9  ------
# Percentage of new and relapse TB cases with documented HIV status, 2016
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
                        paste("Figure 4.9 Percentage of new and relapse TB cases with documented HIV status(a),", report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)


# Add footnote about Russia
hivstatus_foot <- "(a) Data for the Russian Federation are for new TB patients in the civilian sector only"



hivstatus_map <- arrangeGrob(hivstatus_map, bottom = textGrob(hivstatus_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


figsave(hivstatus_map,
        select(hivstatus_data,
                         iso3,
                         hivstatus_pct,
                         cat),
        "f4_9_pct_HIV_status_map")

# Clean up (remove any objects with their name beginning with 'hivstatus')
rm(list=ls(pattern = "^hivstatus"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.12  (TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Percentage of bacteriologically confirmed TB cases tested for RR-TB, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.15  (NEW MAP FOR 2017 REPORT) ------
# Percentage of MDR/RR-TB cases tested for susceptibility to second-line drugs, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.16  (TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Number of patients with laboratory-confirmed XDR–TB started on treatment, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.27  (TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Countries that had used shorter MDR–TB treatment regimens by the end of 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.28  (TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Countries that had used bedaquiline for the treatment of M/XDR–TB as part of expanded access,
# compassionate use or under normal programmatic conditions by the end of 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.29  (TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Countries that had used delamanid for the treatment of M/XDR–TB as part of expanded access,
# compassionate use or under normal programmatic conditions by the end of 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 4.3.1  ------
# Percentage of basic management units in which there is community engagement
# or provision of treatment adherence support, 2016
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
                        paste("Figure Box 4.3.1 Percentage of basic management units in which there is community engagement",
                              "\nor provision of treatment adherence support,",
                              report_year-1),
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
        "f4_box_4_3_1_pct_BMU_community_map")



# Clean up (remove any objects with their name beginning with 'comm')
rm(list=ls(pattern = "^comm"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# TB prevention services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.1 -------
# Availability of data on the number of children aged <5 years who were
# household contacts of bacteriologically confirmed pulmonary TB cases and were started on
# TB preventive treatment, 2016
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
                              "\nhousehold contacts of bacteriologically confirmed pulmonary TB cases and were started on",
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
# Figure 5.3 ---------
# Notification rate ratio of TB among healthcare workers compared with the general population, 2016
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
                        paste("Figure 5.3 Notification rate ratio of TB among healthcare workers\ncompared with the general adult population,", report_year-1),
                           "Notification rate ratio",
                           copyright=FALSE,
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

figsave(hcw_map,
        hcw_data,
        "f5_3_hcw_notf_rate_ratio")

# Clean up (remove any objects with their name beginning with 'hcw')
rm(list=ls(pattern = "^hcw"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 6 ------
# Universal health coverage, social protection and
# and addressing social determinants: Implications for TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 6.2 ---------
# Government spending on health, as a percentage of gross domestic product (GDP), 2015
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
                        paste("Figure 6.2 Government spending on health, as a percentage of gross domestic product (GDP),", report_year-2),
                           "Percentage\nof GDP",
                           copyright=FALSE,
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

figsave(govspend_map,
        select(govspend_data,
               iso3,
               prop,
               cat),
        "f6_2_pct_gov_health_spend_map")

# Clean up (remove any objects with their name beginning with 'govspend')
rm(list=ls(pattern = "^govspend"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 6.3 ---------
# Out-of-pocket expenditures as a percentage of total health expenditures, 2015
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

