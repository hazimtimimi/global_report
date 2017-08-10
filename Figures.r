# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figures in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Last updated by Hazim Timimi, July 2017
#
# Note for maps:
# This script does not use Philippe's who mapping package (see https://github.com/glaziou/whomap)
# instead it uses Tom's original version which allows us to add 'Not applicable' and 'No estimate'
# options to the legends, etc. These are in script ~/functions/make_who_maps.r
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 3 ------
# TB disease burden
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 3.1 (d) (Map)  ------
# Countries with national case-based surveillance as of July 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

cb_data <- strategy %>%
           filter(year >= report_year - 2) %>%
           select(iso3,
                  country,
                  year,
                  caseb_err_nat)

# Make sure all ECDC countries are marked as having case-based surveillance for all TB cases
cb_ecdc <- data_collection %>%
           filter(datcol_year == report_year & dc_ecdc == 1) %>%
           select(iso3)

cb_data <- cb_data %>%
           mutate(caseb_err_nat = ifelse(iso3 %in% cb_ecdc$iso3, 42, caseb_err_nat))


# Override some country responses based on what we know from epi reviews
# Angola, Guinea, Mali, Chad, Cameroon, Burkina Faso, Niger

cb_data <- cb_data %>%
           mutate(caseb_err_nat = ifelse(iso3 %in% c("AGO", "GIN", "MLI", "TCD",
                                                     "CMR", "BFA", "NER"),
                                         0, caseb_err_nat))


# Add classifications
cb_data <- cb_data %>%
           mutate(cat = ifelse(caseb_err_nat == 42, "All TB patients",
                        ifelse(caseb_err_nat == 43, "MDR-TB patients only",
                        ifelse(caseb_err_nat == 0,  "None", NA))))

cb_data$cat <-factor(cb_data$cat, levels=c("None", "MDR-TB patients only", "All TB patients" ))


# Find the countries with empty data for latest year and see if there are data for the previous year
cb_prev_year_data <- cb_data %>%
                     filter(year == report_year - 1 & is.na(caseb_err_nat)) %>%
                     select(iso3) %>%
                     inner_join(filter(cb_data, year == report_year - 2)) %>%
                     filter(!is.na(caseb_err_nat))

# Now combine into one dataframe, with previous data used if latest year's data are not available
cb_data_combined <- cb_data %>%
                    filter(year == report_year - 1) %>%
                    anti_join(cb_prev_year_data, by= "iso3") %>%
                    rbind(cb_prev_year_data)

# produce the map
cb_map<- WHOmap.print(cb_data_combined,
                      "Figure 3.1\nCountries with national case-based surveillance as of July 2017 (a)",
                         legend.title = "Country response",
                         copyright=FALSE,
                         brewer.pal(3, "Blues"),
                         na.label="No response",
                         show=FALSE)

# Add footnote about using earlier data for some countries
cb_foot <- paste("(a) Responses from ",
                  report_year -1,
                  " were used for ",
                  nrow(cb_prev_year_data),
                  "countries")

cb_map <- arrangeGrob(cb_map, bottom = textGrob(cb_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


figsave(cb_map,
        select(cb_data_combined,
                iso3,
                country,
                cat),
        "f3_1_casebased_map")


# Clean up (remove any objects with their name beginning with 'cb')
rm(list=ls(pattern = "^cb"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Diagnosis and treatment of TB, HIV-associated TB and drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1   ------
# Case notification rates (new and relapse cases, all forms) (black) compared with estimated TB incidence rates (green),
# 2000–2016, globally and for WHO regions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

inc_data <- aggregated_estimates_epi_rawvalues %>%
            filter( year >= 2000 & group_type == "g_whoregion") %>%
            select(year,
                   g_whoregion = group_name,
                   e_inc_100k,
                   e_inc_100k_lo,
                   e_inc_100k_hi,
                   c_newinc,
                   e_pop_num) %>%
            mutate(c_newinc_100k = c_newinc * 1e5 / e_pop_num) %>%

            # merge with regional names
            inner_join(who_region_names, by = "g_whoregion") %>%
            select(-g_whoregion)


inc_global <- aggregated_estimates_epi_rawvalues %>%
              filter( year >= 2000 & group_type == "global") %>%
              select(year,
                     e_inc_100k,
                     e_inc_100k_lo,
                     e_inc_100k_hi,
                     c_newinc,
                     e_pop_num) %>%
              mutate(c_newinc_100k = c_newinc * 1e5 / e_pop_num,
                     entity = "Global")

# Add global to the regional aggregates
inc_data <- rbind(inc_data, inc_global)

# Change the order
inc_data$entity <- factor(inc_data$entity,
                             levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))


# Plot as lines
inc_plot <- inc_data %>%
            ggplot(aes(x=year, y=c_newinc_100k, ymin=0)) +
            geom_line(size=1) +
            geom_ribbon(aes(x=year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi),
                        fill=standard_palette("incidence"),
                        alpha=0.4) +
            geom_line(aes(year, e_inc_100k),
                      size=1,
                      colour=standard_palette("incidence")) +

            facet_wrap( ~ entity, ncol = 4, scales="free_y") +
            scale_y_continuous(name = "Rate per 100 000 population per year") +
            xlab("Year") +

            ggtitle(paste0("Figure 4.1\nCase notification rates (new and relapse cases, all forms) (black) compared with estimated TB incidence rates (green),\n2000 - ",
                         report_year-1,
                         ", globally and for WHO regions. Shaded areas represent uncertainty bands.")) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsave(inc_plot, inc_data, "f4_1_inc_plot_aggregates")

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.2  ------
# New and relapse TB case notification rates by age and sex in 2016, globally and for WHO regions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get data
agesex_notifs <- filter(notification, year == (report_year - 1)) %>%
                 select(iso2,
                        g_whoregion,
                        newrel_m014, newrel_m1524, newrel_m2534, newrel_m3544, newrel_m4554, newrel_m5564, newrel_m65,
                        newrel_f014, newrel_f1524, newrel_f2534, newrel_f3544, newrel_f4554, newrel_f5564, newrel_f65)


agesex_pop    <- filter(estimates_population, year == (report_year - 1)) %>%
                 select(iso2,
                        e_pop_m014, e_pop_m1524, e_pop_m2534, e_pop_m3544, e_pop_m4554, e_pop_m5564, e_pop_m65,
                        e_pop_f014, e_pop_f1524, e_pop_f2534, e_pop_f3544, e_pop_f4554, e_pop_f5564, e_pop_f65)

agesex        <- merge(agesex_notifs, agesex_pop)


# Drop numbers from countries who didn't report cases in the 10-year age intervals for ages 15 and above

agesex$tot_notified <- sum_of_row(agesex[c("newrel_m1524", "newrel_m2534", "newrel_m3544", "newrel_m4554", "newrel_m5564", "newrel_m65",
                                           "newrel_f1524", "newrel_f2534", "newrel_f3544", "newrel_f4554", "newrel_f5564", "newrel_f65")] )

agesex_filtered_global <- agesex %>%
                      filter(!is.na(tot_notified)) %>%
                      summarise_each(funs(sum(.,na.rm = TRUE)),
                                     newrel_m014:tot_notified) %>%
                      mutate(entity = "Global")

agesex_filtered_regional <- agesex %>%
                      filter(!is.na(tot_notified)) %>%
                      group_by(g_whoregion) %>%
                      summarise_each(funs(sum(.,na.rm = TRUE)),
                                     newrel_m014:tot_notified)  %>%
                      # merge with regional names
                      inner_join(who_region_names, by = "g_whoregion") %>%
                      select(-g_whoregion)

agesex_filtered_agg <- rbind(agesex_filtered_regional, agesex_filtered_global) %>%

                      # Calculate rate per 100 000 population for each age/sex group
                      mutate(m014 = newrel_m014 * 1e5 / e_pop_m014,
                             m1524 = newrel_m1524 * 1e5 / e_pop_m1524,
                             m2534 = newrel_m2534 * 1e5 / e_pop_m2534,
                             m3544 = newrel_m3544 * 1e5 / e_pop_m3544,
                             m4554 = newrel_m4554 * 1e5 / e_pop_m4554,
                             m5564 = newrel_m5564 * 1e5 / e_pop_m5564,
                             m65 = newrel_m65 * 1e5 / e_pop_m65,

                             f014 = newrel_f014 * 1e5 / e_pop_f014,
                             f1524 = newrel_f1524 * 1e5 / e_pop_f1524,
                             f2534 = newrel_f2534 * 1e5 / e_pop_f2534,
                             f3544 = newrel_f3544 * 1e5 / e_pop_f3544,
                             f4554 = newrel_f4554 * 1e5 / e_pop_f4554,
                             f5564 = newrel_f5564 * 1e5 / e_pop_f5564,
                             f65 = newrel_f65 * 1e5 / e_pop_f65
                             ) %>%

                      # select only the rates
                      select(entity,
                             m014, m1524, m2534, m3544, m4554, m5564, m65,
                             f014, f1524, f2534, f3544, f4554, f5564, f65)



# Switch to long ('tidy') format to plot
agesex_agg_long <- agesex_filtered_agg %>%
                      gather("group", "rate", 2:15)


# Extract sex and agegroup from the group field
agesex_agg_long$sex <- str_extract(agesex_agg_long$group, "f|m")
agesex_agg_long$sex <- factor(agesex_agg_long$sex,
                                   levels=c("f", "m"),
                                   labels=c("Female", "Male"))
agesex_agg_long$agegroup <- str_extract(agesex_agg_long$group, "014|1524|2534|3544|4554|5564|65")
agesex_agg_long$agegroup <- factor(agesex_agg_long$agegroup,
                                   levels=c("014", "1524", "2534", "3544", "4554", "5564", "65"),
                                   labels=c("0\u201314", "15\u201324", "25\u201334", "35\u201344", "45\u201354", "55\u201364", ">=65"))

agesex_agg_long <- agesex_agg_long %>% select(-group)


# Change the order
agesex_agg_long$entity <- factor(agesex_agg_long$entity,
                              levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))



# Now plot the aggregates as pyramids
# See first code example at https://github.com/JuanGaleano/Population-pyramids-ggplot2/blob/master/POPULATION%20PYRAMID%20GGPLOT2.R

agesex_plot <- agesex_agg_long %>%
                # Multiply all the female rates by -1
                mutate(rate = ifelse(sex=="Female", rate * -1, rate )) %>%
                ggplot(aes(x=agegroup, y=rate, fill=sex)) +
                geom_bar(stat="identity",
                         size=.3,
                         colour="black",
                         position="identity") +
                scale_fill_manual(values=agesex_palette()) +
                scale_y_continuous(name = "TB case notification rate per 100 000 population per year",
                                   #Set labels manually to avoid showing negative numbers!
                                   breaks=c(-200,-100,0,100,200,300),
                                   labels=abs(c(-200,-100,0,100,200,300))) +
                scale_x_discrete("Age group (years)",
                                 labels=levels(agesex_agg_long$agegroup)) +
                coord_flip() +
                facet_wrap( ~ entity, ncol = 4) +
                ggtitle(paste0("Figure 4.2\nNew and relapse TB case notification rates by age and sex(a) in ",
                                report_year - 1,
                                ", globally and for WHO regions")) +
                theme_glb.rpt() +
                theme(legend.position="top",
                      legend.title=element_blank())


# Add footnote
agesex_pcnt <- rounder(sum(agesex$tot_notified, na.rm=TRUE)  * 100
                       /
                       sum(notification[notification$year==report_year -1, "c_newinc"], na.rm=TRUE))

agesex_foot <- paste0("(a) Countries not reporting cases in these categories are excluded. Cases included make up ",
                      agesex_pcnt,
                      "% of reported cases.")



agesex_plot <- arrangeGrob(agesex_plot, bottom = textGrob(agesex_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plots
figsave(agesex_plot, agesex_agg_long, "f4_2_agesex")

# Clean up (remove any objects with their name beginning with 'agesex')
rm(list=ls(pattern = "^agesex"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.3 (map) ------
# Percentage new and relapse TB cases that were children (aged < 15), 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


kids_data <- notification %>%
             filter(year >= report_year - 2) %>%
             select(iso3,
                    country,
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
                     c('0-1.9', '2-4.9', '5-9.9', '>=10'),
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
                        paste0("Figure 4.3\nPercentage of new and relapse TB cases that were children (aged < 15), ",
                               report_year-1,
                               "(a)"),
                           "Percentage",
                           copyright=FALSE,
                           #colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           colors=brewer.pal(4, "Blues"),
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
                         country,
                         kids_pct,
                         cat),
        "f4_3_pct_children_map")

# Clean up (remove any objects with their name beginning with 'kids')
rm(list=ls(pattern = "^kids"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.4  ------
# Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, 2000-2016, globally and for WHO regions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


bacconf_data <- notification %>%
                filter(year >= 2000) %>%
                select(iso3,
                       year,
                       g_whoregion,
                       # old variables pre-2013
                       new_sp,
                       new_sn,
                       new_su,
                       # new variables
                       new_labconf, new_clindx,
                       ret_rel_labconf, ret_rel_clindx) %>%
                group_by(year, g_whoregion) %>%
                summarise_each(funs(sum(.,na.rm = TRUE)),
                               new_sp:ret_rel_clindx) %>%

                #calculate % of pulmonary cases with bac confirmation
                # a bit tricky for years before 2013, so do for new only by smear only
                mutate(bacconf_pct_numerator = ifelse(year < 2013 & g_whoregion != 'EUR',
                                                      # old variables, do for new only outside EUR
                                                      new_sp,
                                                      # new variables
                                                     (new_labconf + ret_rel_labconf)),
                       bacconf_pct_denominator = ifelse(year < 2013 & g_whoregion != 'EUR',
                                                      # old variables, do for new only outside EUR
                                                      (new_sp + new_sn + new_su),
                                                      # new variables
                                                      (new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx))) %>%

                # Adjust calculation for EUR pre-2013 (applies to years 2002 - 2012)
                 mutate(bacconf_pct_numerator = ifelse(between(year, 2002, 2012) & g_whoregion == 'EUR',
                                                      # old variables, but using new_labconf
                                                      new_labconf,
                                                      # otherwise keep calculation from previous step
                                                      bacconf_pct_numerator),
                        bacconf_pct_denominator = ifelse(between(year, 2002, 2012) & g_whoregion == 'EUR',
                                                      # old variables
                                                      (new_sp + new_sn + new_su),
                                                      # otherwise keep calculation from previous step
                                                      bacconf_pct_denominator),

                        # Finally deal with EUR 2000 and 2001 numerator
                        bacconf_pct_numerator = ifelse(between(year, 2000, 2001) & g_whoregion == 'EUR',
                                                      # old variables
                                                      new_sp,
                                                      # otherwise keep calculation from previous step
                                                      bacconf_pct_numerator),
                        bacconf_pct_denominator = ifelse(between(year, 2000, 2001) & g_whoregion == 'EUR',
                                                      # old variables
                                                      (new_sp + new_sn + new_su),
                                                      # otherwise keep calculation from previous step
                                                      bacconf_pct_denominator)
                        ) %>%

                # merge with regional names
                inner_join(who_region_names, by = "g_whoregion") %>%
                select(-g_whoregion) %>%

                # get rid of extra variables
                select(entity,
                       year,
                       bacconf_pct_numerator,
                       bacconf_pct_denominator) %>%

                # get rid of the oh-so-pesky grouping variables within the dataframe
                ungroup()

bacconf_global <- bacconf_data %>%
                  group_by(year) %>%
                  summarise_each(funs(sum(.,na.rm = TRUE)),
                                bacconf_pct_numerator:bacconf_pct_denominator) %>%
                  mutate(entity = 'Global')

# Add global to the regional aggregates
bacconf_data <- rbind(bacconf_data, bacconf_global) %>%

                # Calculate the percentages
                mutate(bacconf_pct = bacconf_pct_numerator * 100 / bacconf_pct_denominator)

# Change the order
bacconf_data$entity <- factor(bacconf_data$entity,
                              levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))




# Plot as lines
bacconf_plot <- bacconf_data %>%
                ggplot(aes(x=year, y=bacconf_pct)) +
                  geom_line(size=1) +
                  scale_y_continuous(name = "% bacteriologically confirmed") +
                  expand_limits(y=c(0,100)) +
                  xlab("Year") +
                  facet_wrap( ~ entity, ncol = 4) +
                  ggtitle(paste0("Figure 4.4\nPercentage of new and relapse(a) pulmonary TB cases with bacteriological confirmation, 2000 - ",
                               report_year-1,
                               ", globally\nand for WHO regions.")) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
bacconf_foot <- "(a) The calculation is for new pulmonary cases in years prior to 2013 based on smear results, except for the European Region\nwhere data on confirmation by culture was also available for the period 2002 - 2012."

bacconf_plot <- arrangeGrob(bacconf_plot, bottom = textGrob(bacconf_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(bacconf_plot, bacconf_data, "f4_4_bacconf_plot")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.5  (map) ------
# Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bacconf_data <- notification %>%
                filter(year  >= report_year - 2) %>%
                select(iso3,
                       country,
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
                     c('0-49.9', '50-64.9', '65-79.9', '>=80'),
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
                        paste0("Figure 4.5\nPercentage of new and relapse pulmonary TB cases with bacteriological confirmation, ",
                              report_year-1,
                               "(a)"),
                           "Percentage",
                           copyright=FALSE,
                           #colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           colors=brewer.pal(4, "PuRd"),
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
                         country,
                         bacconf_pct,
                         cat),
        "f4_5_pct_bacconf_map")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.6  (Map) ------
# Percentage of extrapulmonary cases among new and relapse TB cases, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


ep_data <- notification %>%
                filter(year  >= report_year - 2) %>%
                select(iso3,
                       country,
                       year,
					  new_labconf, new_clindx, new_ep,
                      ret_rel_labconf, ret_rel_clindx, ret_rel_ep)


#calculate % of extrapulmonary cases
ep_data$newrel_tot <- ep_data %>%
                          select(new_labconf:ret_rel_ep)%>%
                          sum_of_row()

ep_data$ep_tot <- ep_data %>%
                                  select(new_ep, ret_rel_ep) %>%
                                  sum_of_row()


ep_data$ep_pct <- ifelse(is.na(ep_data$ep_tot) | NZ(ep_data$newrel_tot) == 0, NA,
                                  ep_data$ep_tot * 100 / ep_data$newrel_tot)




ep_data$cat <- cut(ep_data$ep_pct,
                     c(0, 10, 20, 30, Inf),
                     c('0-9.9', '10-19.9', '20-29.9', '>=30'),
               right=FALSE)


# Find the countries with empty data for latest year and see if there are data for the previous year
ep_prev_year_data <- ep_data %>%
                           filter(year == report_year - 1 & is.na(ep_pct)) %>%
                           select(iso3) %>%
                           inner_join(filter(ep_data, year == report_year - 2)) %>%
                           filter(!is.na(ep_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
ep_data_combined <- ep_data %>%
                          filter(year == report_year - 1) %>%
                          anti_join(ep_prev_year_data, by= "iso3") %>%
                          rbind(ep_prev_year_data)


# produce the map
ep_map <- WHOmap.print(ep_data_combined,
                        paste0("Figure 4.6\nPercentage of extrapulmonary cases among new and relapse TB cases, ",
                              report_year-1,
                               "(a)"),
                           "Percentage",
                           copyright=FALSE,
                           colors=brewer.pal(4, "YlOrRd"),
                           show=FALSE)

# Add footnote about using earlier data for some countries
ep_foot <- paste("(a)",
                      report_year - 2,
                      "data were used for ",
                      nrow(ep_prev_year_data),
                      "countries.")


ep_map <- arrangeGrob(ep_map, bottom = textGrob(ep_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))



figsave(ep_map,
        select(ep_data_combined,
                         iso3,
                         country,
                         ep_pct,
                         cat),
        "f4_6_pct_ep_map")

# Clean up (remove any objects with their name beginning with 'ep')
rm(list=ls(pattern = "^ep"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.7  ------
# Percentage of new and relapse TB cases with documented HIV status, 2004-2016, globally and for WHO regions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


hivstatus_data <- TBHIV_for_aggregates %>%
                  filter(year >= 2004) %>%
                  select(g_whoregion,
                         year,
                         hivtest_pct_numerator,
                         hivtest_pct_denominator) %>%
                  group_by(year, g_whoregion) %>%
                  summarise_each(funs(sum(.,na.rm = TRUE)),
                                 hivtest_pct_numerator:hivtest_pct_denominator) %>%

                  # merge with regional names
                  inner_join(who_region_names, by = "g_whoregion") %>%
                  select(-g_whoregion) %>%

                  # squash that darn grouping stuff
                  ungroup()

hivstatus_global <- TBHIV_for_aggregates %>%
                    filter(year >= 2004) %>%
                    select(year,
                           hivtest_pct_numerator,
                           hivtest_pct_denominator) %>%
                    group_by(year) %>%
                    summarise_each(funs(sum(.,na.rm = TRUE)),
                                   hivtest_pct_numerator:hivtest_pct_denominator) %>%
                    mutate(entity = "Global")

hivstatus_data <- rbind(hivstatus_data, hivstatus_global) %>%

                 # Calculate % with known HIV status
                  mutate(hivstatus_pct = hivtest_pct_numerator * 100
                                        / hivtest_pct_denominator)

# Change the order
hivstatus_data$entity <- factor(hivstatus_data$entity,
                              levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))



# Plot as lines
hivstatus_plot <- hivstatus_data %>%
                  ggplot(aes(x=year, y=hivstatus_pct)) +
                  geom_line(size=1) +
                  scale_y_continuous(name = "% with documented status") +
                  expand_limits(y=c(0,100)) +
                  xlab("Year") +
                  #scale_x_discrete(name = "Year") +
                  facet_wrap( ~ entity, ncol = 4) +
                  ggtitle(paste0("Figure 4.7\nPercentage of new and relapse(a) TB cases with documented HIV status, 2004 - ",
                               report_year-1,
                               ", globally and for WHO regions")) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
hivstatus_foot <- "(a) The calculation is for all cases in years prior to 2015."

hivstatus_plot <- arrangeGrob(hivstatus_plot, bottom = textGrob(hivstatus_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(hivstatus_plot, hivstatus_data, "f4_7_hivstatus_plot")

# Clean up (remove any objects with their name beginning with 'hivstatus')
rm(list=ls(pattern = "^hivstatus"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.8  (map) ------
# Percentage of new and relapse TB cases with documented HIV status, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


hivstatus_data <- notification %>%
                  filter(year >= report_year - 2) %>%
                  select(iso3,
                         country,
                         year,
                         c_newinc,
                         newrel_hivtest) %>%

                  # Calculate % with known HIV status
                  mutate(hivstatus_pct = ifelse(is.na(newrel_hivtest) | NZ(c_newinc) == 0, NA,
                                                newrel_hivtest * 100 / c_newinc))


hivstatus_data$cat <- cut(hivstatus_data$hivstatus_pct,
                     c(0, 25, 50, 75, Inf),
                     c('0-24.9', '25-49.9', '50-74.9', '>=75'),
               right=FALSE)


# Find the countries with empty data for latest year and see if there are data for the previous year
hivstatus_prev_year_data <- hivstatus_data %>%
                           filter(year == report_year - 1 & is.na(hivstatus_pct)) %>%
                           select(iso3) %>%
                           inner_join(filter(hivstatus_data, year == report_year - 2)) %>%
                           filter(!is.na(hivstatus_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
hivstatus_data_combined <- hivstatus_data %>%
                          filter(year == report_year - 1) %>%
                          anti_join(hivstatus_prev_year_data, by= "iso3") %>%
                          rbind(hivstatus_prev_year_data)


# produce the map
hivstatus_map <- WHOmap.print(hivstatus_data_combined,
                        paste("Figure 4.8\nPercentage of new and relapse TB cases with documented HIV status,", report_year-1, "(a)"),
                           "Percentage",
                           copyright=FALSE,
                           colors=brewer.pal(4, "BuGn"),
                           show=FALSE)


# Add footnote about using earlier data for some countries
hivstatus_foot <- paste("(a)",
                      report_year - 2,
                      "data were used for ",
                      nrow(hivstatus_prev_year_data),
                      "countries.")



hivstatus_map <- arrangeGrob(hivstatus_map, bottom = textGrob(hivstatus_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


figsave(hivstatus_map,
        select(hivstatus_data_combined,
                         iso3,
                         country,
                         hivstatus_pct,
                         cat),
        "f4_8_pct_HIV_status_map")

# Clean up (remove any objects with their name beginning with 'hivstatus')
rm(list=ls(pattern = "^hivstatus"))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.9   ------
# Global numbers of notified new and relapse cases known to be HIV-positive, number started on ART
# and estimated number of incident HIV-positive TB cases, 2004 - 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tbhiv_data <- TBHIV_for_aggregates %>%
              filter(year >= 2004) %>%
              select(year,
                     hivtest_pos,
                     hiv_art,
                     # new variables from 2015 onwards
                     newrel_hivpos,
                     newrel_art) %>%
              group_by(year) %>%
              summarise_each(funs(sum(.,na.rm = TRUE)),
                             hivtest_pos:newrel_art) %>%

              # Convert to millions and merge pre/post 2014 variables
              mutate(hivtest_pos = ifelse(year < 2015,
                                          hivtest_pos / 1e6,
                                          newrel_hivpos / 1e6),
                     hiv_art = ifelse(year < 2015,
                                      hiv_art / 1e6,
                                      newrel_art / 1e6)) %>%
              select(year,
                     hivtest_pos,
                     hiv_art)


inctbhiv_data <- aggregated_estimates_epi_rawvalues %>%
                  filter( year >= 2004 & group_name == 'global') %>%
                  select(year,
                         e_inc_tbhiv_num,
                         e_inc_tbhiv_num_lo,
                         e_inc_tbhiv_num_hi) %>%
                  mutate(e_inc_tbhiv_num = e_inc_tbhiv_num / 1e6,
                         e_inc_tbhiv_num_lo = e_inc_tbhiv_num_lo / 1e6,
                         e_inc_tbhiv_num_hi = e_inc_tbhiv_num_hi / 1e6) %>%

                  # Use a right-join so can see the data for the final year even in the absence of estimates
                  right_join(tbhiv_data)


# Plot as lines
inctbhiv_plot <- inctbhiv_data %>%
                  ggplot(aes(x=year, y=hivtest_pos, ymin=0)) +
                  geom_line(size=1) +
                  geom_ribbon(aes(x=year, ymin=e_inc_tbhiv_num_lo, ymax=e_inc_tbhiv_num_hi),
                              fill=standard_palette("tbhiv_incidence"),
                              alpha=0.4) +
                  geom_line(aes(year, e_inc_tbhiv_num),
                            size=1,
                            colour=standard_palette("tbhiv_incidence")) +

                  geom_line(aes(year, hiv_art),
                            size=1,
                            colour=standard_palette("art")) +

                  scale_y_continuous(name = "New and relapse cases per year (millions)") +
                  xlab("Year") +

                  ggtitle(paste0("Figure 4.9\nGlobal numbers of notified new and relapse cases(a) known to be HIV-positive (black),\nnumber started on antiretroviral therapy (blue) and estimated number of incident HIV-positive TB cases (red), 2004 - ",
                               report_year-1,
                         ".\nShaded areas represent uncertainty bands.")) +

                  theme_glb.rpt()


# Add footnote
inctbhiv_foot <- "(a) The calculation is for all cases in years prior to 2015."

inctbhiv_plot <- arrangeGrob(inctbhiv_plot, bottom = textGrob(inctbhiv_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(inctbhiv_plot, inctbhiv_data, "f4_9_inctbhiv_plot_global")

# Clean up (remove any objects with their name containing 'tbhiv')
rm(list=ls(pattern = "tbhiv"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.10 ------
# Percentage of bacteriologically confirmed TB cases tested for RR–TB, globally and for WHO regions, 2009–2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This one is a bit of a mess. For years up to 2014 (i.e. reporting year 2015),
# we took the highest value of DST coverage in the surveillance or notification pages.
# For years 2015 onwards (2016 data collection year onwards) we only use the notification value

dst_notif_data <- notification %>%
                  filter(year >= 2009) %>%
                  select(year,
                         iso3,
                         g_whoregion,
                         new_labconf,
                         c_ret,
                         rdst_new,
                         rdst_ret) %>%
                  mutate(
                        # denominator is a bit of a fudge: new_labconf + c_ret
                         dst_denom = ifelse(is.na(new_labconf) & is.na(c_ret), NA,
                                           (NZ(new_labconf) + NZ(c_ret))),
                        # numerator
                        dst_notif_num = ifelse(is.na(rdst_new) & is.na(rdst_ret), NA,
                                           (NZ(rdst_new) + NZ(rdst_ret)))
                        )

dst_drs_data <- dr_surveillance %>%
                filter(year >= 2009 & year < 2015) %>%
                select(year,
                       iso3,
                       dst_rlt_new,
                       dst_rlt_ret) %>%
                mutate(
                      # numerator
                      dst_drs_num = ifelse(is.na(dst_rlt_new) & is.na(dst_rlt_ret), NA,
                                           (NZ(dst_rlt_new) + NZ(dst_rlt_ret)))
                      )

# Link the two data sets

dst_data <- dst_notif_data %>%
            left_join(dst_drs_data) %>%

            # To calculate the percentage DST coverage we need to identify the greater of the two numerators
            # Set numerator to NA if the denominator is NA for a country-year
            mutate(
                  dst_num = ifelse(NZ(dst_denom) == 0, NA,
                            ifelse((is.na(dst_notif_num) & !is.na(dst_drs_num)) |
                                     (NZ(dst_drs_num) >= NZ(dst_notif_num)),
                                   dst_drs_num,
                            ifelse((!is.na(dst_notif_num) & is.na(dst_drs_num)) |
                                     (NZ(dst_notif_num) >= NZ(dst_drs_num)),
                                   dst_notif_num, NA )))
            ) %>%

            # Drop unwanted variables
            select(iso3,
                   year,
                   g_whoregion,
                   dst_num,
                   dst_denom) %>%

            # Drop rows with empty numerators
            filter(!is.na(dst_num))


dst_global <- dst_data %>%
              group_by(year) %>%
              summarise_each(funs(sum(.,na.rm = TRUE)),
                             dst_num:dst_denom) %>%
              mutate(entity = "Global") %>%
              ungroup()

dst_regional <- dst_data %>%
                group_by(g_whoregion, year) %>%
                summarise_each(funs(sum(.,na.rm = TRUE)),
                               dst_num:dst_denom)  %>%
                # merge with regional names
                inner_join(who_region_names, by = "g_whoregion") %>%
                ungroup() %>%
                select(-g_whoregion)


dst_agg <- rbind(dst_regional, dst_global) %>%

           # OK, now you can calculate the percentage coverage
           mutate( dst_pcnt = dst_num * 100 / dst_denom) %>%

           # Add a footnote call to Africa
           mutate( entity = ifelse(entity == "Africa", "Africa(b)", entity))

# Change the order
dst_agg$entity <- factor(dst_agg$entity,
                             levels = c("Africa(b)", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))


# Plot as lines
dst_plot <- dst_agg %>%
            ggplot(aes(x=year, y=dst_pcnt, ymin=0)) +
            geom_line(size=1) +

            facet_wrap( ~ entity, ncol = 4, scales="fixed") +
            scale_y_continuous(name = "% of cases") +
            expand_limits(y=c(0,100)) +
            xlab("Year") +

            ggtitle(paste0("Figure 4.10\nPercentage of bacteriologically confirmed TB cases tested for RR–TB, globally and for WHO regions, 2009 - ",
                         report_year-1,
                         "(a)")) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Add explanatory footnotes
dst_footnote <- "(a) Among new laboratory confirmed and retreatment cases; test results in cases with unknown previous history are not included.\n(b) The spike in coverage in the African Region in 2015 is due to the reporting of laboratory results for many cases in South Africa differentiated by treatment history."

dst_plot <- arrangeGrob(dst_plot, bottom = textGrob(dst_footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 9)))

# Save the plot
figsave(dst_plot, dst_agg, "f4_10_dst_aggregates")

# Clean up (remove any objects with their name containing 'dst_')
rm(list=ls(pattern = "dst_"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.11  (Map) ------
# Percentage of bacteriologically confirmed TB cases tested for RR-TB, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


dst_data <- notification %>%
            filter(year >= report_year - 2) %>%
            select(iso3,
                   country,
                   year,
                   new_labconf,
                   c_ret,
                   rdst_new,
                   rdst_ret) %>%

              # Calculate coverage of DST percentages
              mutate(# percent DST among lab-confirmed cases is a bit of a fudge:
                     # numerator rdst_new + rdst_ret  (ignore cases with unknown treatment history)
                     # denominator is a bit of a fudge: new_labconf + c_ret
                     dst_pct = ifelse((NZ(new_labconf) + NZ(c_ret)) == 0 |
                                         is.na(rdst_new) & is.na(rdst_ret), NA,
                                       (NZ(rdst_new) + NZ(rdst_ret)) * 100 /
                                                     (NZ(new_labconf) + NZ(c_ret)))
                     )

dst_data$cat <- cut(dst_data$dst_pct,
                     c(0, 10, 40, 80, Inf),
                     c('0-9', '10-39', '40-79', '>=80'),
               right=FALSE)

# Find the countries with empty data for latest year and see if there are data for the previous year
dst_prev_year_data <- dst_data %>%
                      filter(year == report_year - 1 & is.na(dst_pct)) %>%
                      select(iso3) %>%
                      inner_join(filter(dst_data, year == report_year - 2)) %>%
                      filter(!is.na(dst_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
dst_data_combined <- dst_data %>%
                     filter(year == report_year - 1) %>%
                     anti_join(dst_prev_year_data, by= "iso3") %>%
                     rbind(dst_prev_year_data)

# produce the map
dst_map <- WHOmap.print(dst_data_combined,
                        paste("Figure 4.11\nPercentage of bacteriologically confirmed TB cases tested for RR-TB,(a),", report_year-1),
                         "Percentage",
                         copyright=FALSE,
                         colors=brewer.pal(4, "YlOrBr"),
                         show=FALSE)


# Add footnote about testing and also about using earlier data for some countries
dst_foot <- paste("(a) Among new laboratory confirmed and previously treated cases; cases with unknown previous treatment history are not included.",
                  report_year - 2,
                  "data were used for ",
                  nrow(dst_prev_year_data),
                  "countries")



dst_map <- arrangeGrob(dst_map, bottom = textGrob(dst_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 8)))


figsave(dst_map,
        select(dst_data_combined,
               iso3,
               country,
               dst_pct,
               cat),
        "f4_11_dst_map")

# Clean up (remove any objects with their name beginning with 'dst')
rm(list=ls(pattern = "^dst"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.12 ------
# Global number of MDR/RR-TB cases detected (pink) and number enrolled on MDR-TB treatment (green) 2009–2016,
# compared with estimate for 2016 of the number of incident cases of MDR/RR-TB (uncertainty interval shown in blue)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rr_data <- dr_derived_variables %>%
           filter(year >= 2009) %>%
           select(iso3,
                  year,
                  c_rrmdr)

rr_txdata <- notification %>%
             filter(year >= 2009) %>%
             select(iso3,
                    year,
                    conf_mdr_tx,
                    unconf_mdr_tx,
                    conf_rrmdr_tx,
                    unconf_rrmdr_tx)

# calculate total enrolled on treatment
rr_txdata$enrolled <- rr_txdata %>%
                      select(unconf_mdr_tx, conf_mdr_tx, unconf_rrmdr_tx, conf_rrmdr_tx) %>%
                      sum_of_row()

# Link the two and drop unneeded variables
rr_data <- rr_data %>%
           inner_join(rr_txdata) %>%
           select(iso3,
                  year,
                  c_rrmdr,
                  enrolled)

# Calculate global aggregates and convert sums to thousands
rr_global <- rr_data %>%
             group_by(year) %>%
             summarise_each(funs(sum(.,na.rm = TRUE)/1000),
                           c_rrmdr:enrolled) %>%
             ungroup()

# Get the estimated incident cases
rr_global_inc <- aggregated_estimates_drtb_rawvalues %>%
                  filter(year == report_year - 1 & group_name == "global") %>%
                  select(year,
                         e_inc_rr_num,
                         e_inc_rr_num_lo,
                         e_inc_rr_num_hi) %>%
                  mutate(e_inc_rr_num    = e_inc_rr_num    / 1000,
                         e_inc_rr_num_lo = e_inc_rr_num_lo / 1000,
                         e_inc_rr_num_hi = e_inc_rr_num_hi / 1000)

rr_global <- rr_global %>%
             left_join(rr_global_inc)


# Plot as lines
rr_plot <-  rr_global %>%
            ggplot(aes(x=year, y=c_rrmdr, ymin=0)) +
            geom_line(size=1, colour = "Pink") +
            geom_line(aes(year, enrolled),
                      size=1,
                      colour="Green") +

            # Add estimated incidence
            geom_errorbar(aes(ymin=e_inc_rr_num_lo,
                              ymax=e_inc_rr_num_hi),
                          width=0.2, size=1, color="blue") +

            geom_point(aes(x=year, y=e_inc_rr_num),
                       shape = 21, colour = "blue", fill = "white", size = 5, stroke = 2) +

            scale_y_continuous(name = "Number of cases (thousands)") +
            xlab("Year") +

            ggtitle(paste0("Figure 4.12\nGlobal number of MDR/RR-TB cases detected (pink) and number enrolled on MDR-TB treatment (green), 2009 - ",
                         report_year-1,
                         ",\ncompared with estimate for ",
                          report_year-1,
                         " of the number of incident cases of MDR/RR-TB (uncertainty interval shown in blue)")) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsave(rr_plot, rr_global, "f4_12_drtb_detect_enroll_global")

# Clean up (remove any objects with their name beginning with 'rr_')
rm(list=ls(pattern = "^rr_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.13 ------
# Number of MDR/RR-TB cases detected (pink) and enrolled on MDR-TB treatment (green) 2009–2016,
# 30 high MDR-TB burden countries
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rr_30hbc <- country_group_membership %>%
               filter(group_type == "g_hb_mdr" & group_name == 1) %>%
               select(iso2)


rr_data <- dr_derived_variables %>%
           filter(year >= 2009) %>%
           select(country,
                  iso2,
                  year,
                  c_rrmdr)

rr_txdata <- notification %>%
             filter(year >= 2009) %>%
             select(iso2,
                    year,
                    conf_mdr_tx,
                    unconf_mdr_tx,
                    conf_rrmdr_tx,
                    unconf_rrmdr_tx)  %>%
             # restrict to the 30 high burden countries
             inner_join(rr_30hbc)

# calculate total enrolled on treatment
rr_txdata$enrolled <- rr_txdata %>%
                      select(unconf_mdr_tx, conf_mdr_tx, unconf_rrmdr_tx, conf_rrmdr_tx) %>%
                      sum_of_row()

# Link the two and drop unneeded variables
rr_data <- rr_data %>%
           inner_join(rr_txdata) %>%
           select(country,
                  year,
                  c_rrmdr,
                  enrolled) %>%

            # shorten long country names
            get_names_for_tables( col = "country")

# Plot as lines
rr_plot <-  rr_data %>%
            ggplot(aes(x=year, y=c_rrmdr, ymin=0)) +
            geom_line(size=1, colour = "Pink") +
            geom_line(aes(year, enrolled),
                      size=1,
                      colour="Green") +

            facet_wrap( ~ country, scales="free_y", ncol = 5) +

            scale_y_continuous(name = "Number of cases") +
            xlab("Year") +

            ggtitle(paste0("Figure 4.13\nNumber of MDR/RR-TB cases detected (pink) and enrolled on MDR-TB treatment\n(green), 2009 - ",
                         report_year-1,
                         ", 30 high MDR-TB burden countries")) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsave(rr_plot, rr_data, "f4_13_drtb_detect_enroll_hbc", width=7, height=11)

# Clean up (remove any objects with their name beginning with 'rr_')
rm(list=ls(pattern = "^rr_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.14  (map) ------
# Percentage of MDR/RR-TB cases tested for susceptibility to second-line drugs, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sldst_data <- notification %>%
              filter(year >= report_year - 2) %>%
              select(iso3,
                     country,
                     year,
                     conf_rrmdr,
                     rr_sldst) %>%

              # Calculate percentage RR cases with 2nd line DST
              mutate(pcnt_sldst = ifelse(NZ(conf_rrmdr)> 0,
                                         rr_sldst * 100 / conf_rrmdr, NA)
                     )

sldst_data$cat <- cut(sldst_data$pcnt_sldst,
                          c(0, 25, 50, 75, Inf),
                          c('0-24.9', '25-49.9', '50-74.9', '>=75'),
                          right=FALSE)

# Find the countries with empty data for latest year and see if there are data for the previous year
sldst_prev_year_data <- sldst_data %>%
                         filter(year == report_year - 1 & is.na(pcnt_sldst)) %>%
                         select(iso3) %>%
                         inner_join(filter(sldst_data, year == report_year - 2)) %>%
                         filter(!is.na(pcnt_sldst))

# Now combine into one dataframe, with previous data used if latest year's data are not available
sldst_data_combined <- sldst_data %>%
                        filter(year == report_year - 1) %>%
                        anti_join(sldst_prev_year_data, by= "iso3") %>%
                        rbind(sldst_prev_year_data)


# produce the map
sldst_map <- WHOmap.print(sldst_data_combined,
                          paste0("Figure 4.14\nPercentage of MDR/RR-TB cases tested for susceptibility to second-line drugs, ",
                                    report_year-1,
                                  "(a)"),
                                 "Percentage",
                                 copyright=FALSE,
                                 colors=brewer.pal(4, "Blues"),
                                 show=FALSE)

# Add footnote about using earlier data for some countries
sldst_foot <- paste("(a)",
                      report_year - 2,
                      "data were used for ",
                      nrow(sldst_prev_year_data),
                      "countries.")

sldst_map <- arrangeGrob(sldst_map, bottom = textGrob(sldst_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


figsave(sldst_map,
        select(sldst_data_combined,
                         iso3,
                         country,
                         pcnt_sldst,
                         cat),
        "f4_14_pct_sldst_map")

# Clean up (remove any objects with their name beginning with 'sldst')
rm(list=ls(pattern = "^sldst"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.15   ------
# Case notification rates (new and relapse cases, all forms) (black) compared with estimated TB incidence rates,
# (green), 2000-2016, 30 high TB burden countries
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

newinc_30hbc <- country_group_membership %>%
                filter(group_type == "g_hb_tb" & group_name == 1) %>%
                select(iso2)

newinc_data <- notification %>%
                filter(year >= 2000) %>%
                inner_join(newinc_30hbc) %>%
                select(year,
                       iso2,
                       country,
                       c_newinc) %>%
                #add markers for India footnote
                mutate(country = ifelse(country == "India", "India(a)", country))

inc_data <- estimates_epi_rawvalues %>%
            filter(year >= 2000) %>%
            inner_join(newinc_30hbc) %>%
            select(year,
                   iso2,
                   e_pop_num,
                   e_inc_100k,
                   e_inc_100k_lo,
                   e_inc_100k_hi) %>%

            # Use a right-join so can see the data for the final year even in the absence of estimates
            right_join(newinc_data) %>%

            # Calculate case notification rate
            mutate(c_newinc_100k = c_newinc * 1e5 / e_pop_num) %>%

            # shorten long country names
            get_names_for_tables( col = "country")

# Plot as lines
inc_plot <- inc_data %>%
            ggplot(aes(x=year, y=c_newinc_100k, ymin=0)) +
            geom_line(size=1) +
            geom_ribbon(aes(x=year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi),
                        fill=standard_palette("incidence"),
                        alpha=0.4) +
            geom_line(aes(year, e_inc_100k),
                      size=1,
                      colour=standard_palette("incidence")) +

            scale_y_continuous(name = "Rate per 100 000 population per year") +
            xlab("Year") +

            facet_wrap( ~ country,
                        scales = "free_y",
                        ncol = 5) +

            ggtitle(paste0("Figure 4.15\nCase notification rates (new and relapse cases, all forms) (black)\ncompared with estimated TB incidence rates (green), 2000 - ",
                         report_year-1,
                         ",\n30 high TB burden countries. Shaded areas represent uncertainty bands.")) +
            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  strip.text.x = element_text(size=8))  #reduce font size of labels above each panel

# Add Bangladesh and India footnotes
inc_plot <- arrangeGrob(inc_plot,
                        bottom = textGrob(paste("(a)",
                                                 india_footnote),
                                          x = 0.02,
                                          just = "left",
                                          gp = gpar(fontsize = 7)))


# Save the plot
figsave(inc_plot, inc_data, "f4_15_inc_plot_hbc", width=7, height=11)

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.16   ------
# Estimated TB treatment coverage (new and relapse patients as a percentage of estimated TB incidence)
# in 2016, 30 high TB burden countries, WHO regions and globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

coverage_inc_country <- estimates_epi_rawvalues %>%
                         filter(year == report_year - 1) %>%
                         select(entity = country,
                                iso2,
                                e_inc_num,
                                e_inc_num_lo,
                                e_inc_num_hi) %>%
                         # shorten long country names
                         get_names_for_tables( col = "entity")

# Filter the country list down to high burden ones
coverage_30hbc <- country_group_membership %>%
                  filter(group_type == "g_hb_tb" & group_name == 1) %>%
                  select(iso2)

coverage_inc_country <- coverage_inc_country %>%
                        inner_join(coverage_30hbc) %>%
                       #add marker for India footnote
                       mutate(entity = ifelse(entity == "India", "India(a)", entity))


coverage_country <- notification %>%
                     filter(year == report_year - 1) %>%
                     select(iso2,
                            c_newinc) %>%
                     inner_join(coverage_inc_country) %>%
                     select(-iso2) %>%
                     mutate(c_cdr = c_newinc * 100 / e_inc_num,
                            c_cdr_lo = c_newinc * 100  / e_inc_num_hi,
                            c_cdr_hi = c_newinc * 100  / e_inc_num_lo,
                            # highlight countries with no data
                            entity = ifelse(is.na(c_newinc), paste0(entity, "*"), entity )) %>%
                     select(entity,
                            c_cdr,
                            c_cdr_lo,
                            c_cdr_hi) %>%
                     arrange(desc(c_cdr))

# Calculate how many countries highlighted as having no data
coverage_nodata_count <- coverage_country %>%
                          filter(grepl("[*]$", entity)) %>%
                          nrow()

coverage_region <- aggregated_estimates_epi_rawvalues %>%
                   filter(year == report_year - 1 & group_type == "g_whoregion") %>%
                   select(g_whoregion = group_name,
                          c_cdr,
                          c_cdr_lo,
                          c_cdr_hi) %>%
                   # merge with regional names and simplify to match structure of country table
                   inner_join(who_region_names, by = "g_whoregion") %>%
                   select(-g_whoregion) %>%
                   arrange(desc(c_cdr))

coverage_global <- aggregated_estimates_epi_rawvalues %>%
                   filter(year == report_year - 1 & group_type == "global") %>%
                   select(c_cdr,
                          c_cdr_lo,
                          c_cdr_hi) %>%
                   # Add dummy variable to match structure of country table
                   mutate(entity = "Global")

# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
coverage_dummy1 <- data.frame(entity = "-----", c_cdr = NA, c_cdr_lo = 0, c_cdr_hi = 100)
coverage_dummy2 <- data.frame(entity = "------", c_cdr = NA, c_cdr_lo = 0, c_cdr_hi = 100)


# Create combined dataframe in order of countries then regional and global estimates
coverage_data <- rbind(coverage_country, coverage_dummy1, coverage_region, coverage_dummy2, coverage_global)


# The dataframe is in the order I want, so make entity an ordered factor based on
# what I already have. That way ggplot will not reorder by entity name
# But I need to reverse order for plotting

coverage_data$entity <- factor(coverage_data$entity, levels = rev(coverage_data$entity))


# plot as horizontal error bars
coverage_plot <- coverage_data %>%
                  ggplot(aes(x=entity,
                             y=c_cdr)) +
                  geom_point() +
                  labs(x="",
                       y="Treatment coverage (%)",
                       title=paste("Figure 4.16\nEstimated TB treatment coverage (new and relapse patients as a percentage of estimated TB incidence)\n in",
                                   report_year - 1,
                                   ", 30 high TB burden countries, WHO regions and globally")) +
                  geom_pointrange(aes(ymin=c_cdr_lo,
                                      ymax=c_cdr_hi)) +
                  theme_glb.rpt() +
                  theme(plot.title = element_text(hjust = 0)) +
                  expand_limits(y=0) +
                  coord_flip()

# Add footnotes
coverage_footnote <- paste("(a)",
                           india_footnote)
# If there are countries with no data then mention it in the footnotes
if (coverage_nodata_count > 0)
  {
  coverage_footnote <- paste("* No data\n",
                             coverage_footnote)
  }

coverage_plot <- arrangeGrob(coverage_plot,
                             bottom = textGrob(coverage_footnote,
                                               x = 0.02,
                                               just = "left",
                                               gp = gpar(fontsize = 8)))


# Save the plot
figsave(coverage_plot, coverage_data, "f4_16_txcoverage_tb")

# Clean up (remove any objects with their name starting with 'coverage')
rm(list=ls(pattern = "^coverage"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.18   ------
# Number of new and relapse cases known to be HIV-positive (black)
# and number started on ART (blue) compared with estimated number of incident HIV-positive TB cases (red),
# 2004-2016, 30 high TB/HIV burden countries
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tbhiv_30hbc <- country_group_membership %>%
               filter(group_type == "g_hb_tbhiv" & group_name == 1) %>%
               select(iso2)

tbhiv_data <- TBHIV_for_aggregates %>%
              filter(year >= 2004) %>%
              inner_join(tbhiv_30hbc) %>%
              select(year,
                     iso2,
                     country,
                     hivtest_pos,
                     hiv_art,
                     # new variables from 2015 onwards
                     newrel_hivpos,
                     newrel_art) %>%

              # Convert to thousands and merge pre/post 2014 variables
              mutate(hivtest_pos = ifelse(year < 2015,
                                          hivtest_pos / 1e3,
                                          newrel_hivpos / 1e3),
                     hiv_art = ifelse(year < 2015,
                                      hiv_art / 1e3,
                                      newrel_art / 1e3)) %>%
              select(year,
                     iso2,
                     country,
                     hivtest_pos,
                     hiv_art)

inctbhiv_data <- estimates_epi_rawvalues %>%
                  filter(year >= 2004) %>%
                  inner_join(tbhiv_30hbc) %>%
                  select(year,
                         iso2,
                         e_inc_tbhiv_num,
                         e_inc_tbhiv_num_lo,
                         e_inc_tbhiv_num_hi) %>%
                  mutate(e_inc_tbhiv_num = e_inc_tbhiv_num / 1e3,
                         e_inc_tbhiv_num_lo = e_inc_tbhiv_num_lo / 1e3,
                         e_inc_tbhiv_num_hi = e_inc_tbhiv_num_hi / 1e3) %>%

                  # Use a right-join so can see the data for the final year even in the absence of estimates
                  right_join(tbhiv_data) %>%

                  # change to shortened country names
                  get_names_for_tables()


# Plot as lines
inctbhiv_plot <- inctbhiv_data %>%
                  ggplot(aes(x=year, y=hivtest_pos, ymin=0)) +
                  geom_line(size=1) +
                  geom_ribbon(aes(x=year, ymin=e_inc_tbhiv_num_lo, ymax=e_inc_tbhiv_num_hi),
                              fill=standard_palette("tbhiv_incidence"),
                              alpha=0.4) +
                  geom_line(aes(year, e_inc_tbhiv_num),
                            size=1,
                            colour=standard_palette("tbhiv_incidence")) +

                  geom_line(aes(year, hiv_art),
                            size=1,
                            colour=standard_palette("art")) +

                  scale_y_continuous(name = "New and relapse cases per year (thousands)") +
                  xlab("Year") +

                  facet_wrap( ~ country,
                              scales = "free_y",
                              ncol = 5) +

                  ggtitle(paste0("Figure 4.18\nNumber of new and relapse cases(a) known to be HIV-positive (black) and\nnumber started on ART (blue) compared with estimated number of \nincident HIV-positive TB cases (red), 2004 - ",
                               report_year-1,
                               ", 30 high TB/HIV burden countries")) +

                  theme_glb.rpt() +
                  theme(strip.text.x = element_text(size=8))  #reduce font size of labels above each panel

# Add footnote
inctbhiv_foot <- "(a) The calculation is for all cases in years prior to 2015."

inctbhiv_plot <- arrangeGrob(inctbhiv_plot, bottom = textGrob(inctbhiv_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 8)))


# Save the plot
figsave(inctbhiv_plot, inctbhiv_data, "f4_18_inctbhiv_plot_hbc", width=7, height=11)

# Clean up (remove any objects with their name containing 'tbhiv')
rm(list=ls(pattern = "tbhiv"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.19   ------
# Estimated ART treatment coverage for HIV-positive TB cases
# (HIV-positive TB patients on ART as a percentage of the estimated incidence of HIV-positive TB)
# in 2016, 30 high TB/HIV burden countries, WHO regions and globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

coveragehiv_inc_country <- estimates_epi_rawvalues %>%
                           filter(year == report_year - 1) %>%
                           select(entity = country,
                                  iso2,
                                  e_inc_tbhiv_num,
                                  e_inc_tbhiv_num_lo,
                                  e_inc_tbhiv_num_hi)  %>%
                           # shorten long country names
                           get_names_for_tables( col = "entity")

# Filter the country list down to high burden ones
coveragehiv_30hbc <- country_group_membership %>%
                     filter(group_type == "g_hb_tbhiv" & group_name == 1) %>%
                     select(iso2)

coveragehiv_inc_country <- coveragehiv_inc_country %>%
                            inner_join(coveragehiv_30hbc)

coveragehiv_country <- TBHIV_for_aggregates %>%
                       filter(year == report_year - 1) %>%
                       select(iso2,
                              newrel_art) %>%
                       inner_join(coveragehiv_inc_country) %>%
                       select(-iso2) %>%
                       mutate(c_art = newrel_art * 100 / e_inc_tbhiv_num,
                              c_art_lo = newrel_art * 100  / e_inc_tbhiv_num_hi,
                              c_art_hi = newrel_art * 100  / e_inc_tbhiv_num_lo,
                              # highlight countries with no data
                              entity = ifelse(is.na(newrel_art), paste0(entity, "*"), entity )) %>%
                       select(entity,
                              c_art,
                              c_art_lo,
                              c_art_hi) %>%
                       arrange(desc(c_art))

# Calculate how many countries highlighted as having no data
coveragehiv_nodata_count <- coveragehiv_country %>%
                            filter(grepl("[*]$", entity)) %>%
                            nrow()

coveragehiv_inc_region <- aggregated_estimates_epi_rawvalues %>%
                          filter(year == report_year - 1 & group_type == "g_whoregion") %>%
                          select(g_whoregion = group_name,
                                  e_inc_tbhiv_num,
                                  e_inc_tbhiv_num_lo,
                                  e_inc_tbhiv_num_hi)

coveragehiv_region <- TBHIV_for_aggregates %>%
                      filter(year == report_year - 1) %>%
                      group_by(g_whoregion) %>%
                      summarise_each(funs(sum(., na.rm = TRUE)),
                                     hiv_art_pct_numerator) %>%

                      # merge with estimates and calculate ART treatment coverage
                      inner_join(coveragehiv_inc_region) %>%
                      mutate( c_art = hiv_art_pct_numerator * 100 / e_inc_tbhiv_num,
                              c_art_lo = hiv_art_pct_numerator * 100 / e_inc_tbhiv_num_hi,
                              c_art_hi = hiv_art_pct_numerator * 100 / e_inc_tbhiv_num_lo) %>%

                      # merge with regional names and simplify
                      inner_join(who_region_names, by = "g_whoregion") %>%
                      select(entity,
                            c_art,
                            c_art_lo,
                            c_art_hi)  %>%
                       arrange(desc(c_art))


coveragehiv_inc_global <- aggregated_estimates_epi_rawvalues %>%
                          filter(year == report_year - 1 & group_type == "global") %>%
                          select(e_inc_tbhiv_num,
                                  e_inc_tbhiv_num_lo,
                                  e_inc_tbhiv_num_hi)%>%
                          mutate(entity = "Global")


coveragehiv_global <- TBHIV_for_aggregates %>%
                      filter(year == report_year - 1) %>%
                      summarise_each(funs(sum(., na.rm = TRUE)),
                                     hiv_art_pct_numerator) %>%
                      mutate(entity = "Global") %>%

                      # merge with estimates and calculate ART treatment coverage
                      inner_join(coveragehiv_inc_global) %>%
                      mutate( c_art = hiv_art_pct_numerator * 100 / e_inc_tbhiv_num,
                              c_art_lo = hiv_art_pct_numerator * 100 / e_inc_tbhiv_num_hi,
                              c_art_hi = hiv_art_pct_numerator * 100 / e_inc_tbhiv_num_lo) %>%
                      select(entity,
                            c_art,
                            c_art_lo,
                            c_art_hi)

# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
coveragehiv_dummy1 <- data.frame(entity = "-----", c_art = NA, c_art_lo = 0, c_art_hi = 100)
coveragehiv_dummy2 <- data.frame(entity = "------", c_art = NA, c_art_lo = 0, c_art_hi = 100)

# Create combined dataframe in order of countries then regional and global estimates
coveragehiv_data <- rbind(coveragehiv_country, coveragehiv_dummy1, coveragehiv_region, coveragehiv_dummy2, coveragehiv_global)

# The dataframe is in the order I want, so make entity an ordered factor based on
# what I already have. That way ggplot will not reorder by entity name
# But I need to reverse order for plotting

coveragehiv_data$entity <- factor(coveragehiv_data$entity, levels = rev(coveragehiv_data$entity))


# plot as horizontal error bars
coveragehiv_plot <- coveragehiv_data %>%
                    ggplot(aes(x=entity,
                               y=c_art)) +
                    geom_point() +
                    labs(x="",
                         y="Treatment coverage (%)",
                         title=paste0("Figure 4.19\nEstimated ART treatment coverage for HIV-positive TB cases\n",
                                     "(HIV-positive TB patients on ART as a percentage of the estimated incidence of HIV-positive TB) in ",
                                     report_year - 1,
                                     ",\n30 high TB/HIV burden countries, WHO Regions and globally")) +
                    geom_pointrange(aes(ymin=c_art_lo,
                                        ymax=c_art_hi)) +
                    theme_glb.rpt() +
                    theme(plot.title = element_text(hjust = 0)) +
                    expand_limits(y=0) +
                    coord_flip()

# If there are countries with no data then add a footnote
if (coveragehiv_nodata_count > 0)
  {
  coveragehiv_plot <- arrangeGrob(coveragehiv_plot,
                                bottom = textGrob("* No data",
                                               x = 0,
                                               hjust = -0.1,
                                               vjust=0,
                                               gp = gpar(fontsize = 10)))
  }

# Save the plot
figsave(coveragehiv_plot, coveragehiv_data, "f4_19_txcoverage_tbhiv")

# Clean up (remove any objects with their name starting with 'coveragehiv')
rm(list=ls(pattern = "^coveragehiv"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.20  ------
# Estimated MDR/RR-TB treatment coverage for MDR/RR-TB
# (patients started on treatment for MDR-TB as a percentage of the estimated incidence of MDR/RR-TB)
# in 2016, 30 high MDR-TB burden countries, WHO regions and globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


coveragerr_inc_country <- estimates_drtb_rawvalues %>%
                          filter(year == report_year - 1) %>%
                          select(entity = country,
                                  iso2,
                                  e_inc_rr_num,
                                  e_inc_rr_num_lo,
                                  e_inc_rr_num_hi)  %>%
                          # shorten long country names
                          get_names_for_tables( col = "entity")

# Filter the country list down to high burden ones
coveragerr_30hbc <-  country_group_membership %>%
                     filter(group_type == "g_hb_mdr" & group_name == 1) %>%
                     select(iso2)


coveragerr_inc_country <- coveragerr_inc_country %>%
                          inner_join(coveragerr_30hbc)

coveragerr_country <-  notification %>%
                       filter(year == report_year - 1) %>%
                       select(iso2,
                              unconf_rrmdr_tx,
                              conf_rrmdr_tx)

coveragerr_country$rrmdr_tx <- sum_of_row(coveragerr_country[c("unconf_rrmdr_tx", "conf_rrmdr_tx")] )



coveragerr_country <- coveragerr_country %>%
                       inner_join(coveragerr_inc_country) %>%
                       mutate(c_rr_coverage = rrmdr_tx * 100 / e_inc_rr_num,
                              c_rr_coverage_lo = rrmdr_tx * 100  / e_inc_rr_num_hi,
                              c_rr_coverage_hi = rrmdr_tx * 100  / e_inc_rr_num_lo,
                              # highlight countries with no data
                              entity = ifelse(is.na(rrmdr_tx), paste0(entity, "*"), entity )) %>%
                       select(entity,
                              c_rr_coverage,
                              c_rr_coverage_lo,
                              c_rr_coverage_hi) %>%
                       arrange(desc(c_rr_coverage))

# Calculate how many countries highlighted as having no data
coveragerr_nodata_count <- coveragerr_country %>%
                            filter(grepl("[*]$", entity)) %>%
                            nrow()


coveragerr_inc_region <- aggregated_estimates_drtb_rawvalues %>%
                          filter(year == report_year - 1 & group_type == "g_whoregion") %>%
                          select(g_whoregion = group_name,
                                  e_inc_rr_num,
                                  e_inc_rr_num_lo,
                                  e_inc_rr_num_hi)

coveragerr_region <- notification %>%
                      filter(year == report_year - 1) %>%
                      group_by(g_whoregion) %>%
                      summarise_each(funs(sum(., na.rm = TRUE)),
                                     unconf_rrmdr_tx, conf_rrmdr_tx) %>%
                      mutate(rrmdr_tx = unconf_rrmdr_tx +  conf_rrmdr_tx) %>%

                      # merge with estimates and calculate treatment coverage
                      inner_join(coveragerr_inc_region) %>%
                       mutate(c_rr_coverage = rrmdr_tx * 100 / e_inc_rr_num,
                              c_rr_coverage_lo = rrmdr_tx * 100  / e_inc_rr_num_hi,
                              c_rr_coverage_hi = rrmdr_tx * 100  / e_inc_rr_num_lo) %>%

                      # merge with regional names and simplify
                      inner_join(who_region_names, by = "g_whoregion") %>%
                      select(entity,
                              c_rr_coverage,
                              c_rr_coverage_lo,
                              c_rr_coverage_hi)  %>%
                      arrange(desc(c_rr_coverage))


coveragerr_inc_global <- aggregated_estimates_drtb_rawvalues %>%
                         filter(year == report_year - 1 & group_type == "global") %>%
                         select(e_inc_rr_num,
                                e_inc_rr_num_lo,
                                e_inc_rr_num_hi)%>%
                         mutate(entity = "Global")


coveragerr_global <- notification %>%
                      filter(year == report_year - 1) %>%
                      summarise_each(funs(sum(., na.rm = TRUE)),
                                     unconf_rrmdr_tx, conf_rrmdr_tx) %>%
                      mutate(rrmdr_tx = unconf_rrmdr_tx +  conf_rrmdr_tx,
                             entity = "Global") %>%

                      # merge with estimates and calculate treatment coverage
                      inner_join(coveragerr_inc_global) %>%
                      mutate(c_rr_coverage = rrmdr_tx * 100 / e_inc_rr_num,
                              c_rr_coverage_lo = rrmdr_tx * 100  / e_inc_rr_num_hi,
                              c_rr_coverage_hi = rrmdr_tx * 100  / e_inc_rr_num_lo) %>%
                      select(entity,
                              c_rr_coverage,
                              c_rr_coverage_lo,
                              c_rr_coverage_hi)

# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
coveragerr_dummy1 <- data.frame(entity = "-----", c_rr_coverage = NA, c_rr_coverage_lo = 0, c_rr_coverage_hi = 100)
coveragerr_dummy2 <- data.frame(entity = "------", c_rr_coverage = NA, c_rr_coverage_lo = 0, c_rr_coverage_hi = 100)

# Create combined dataframe in order of countries then regional and global estimates
coveragerr_data <- rbind(coveragerr_country, coveragerr_dummy1, coveragerr_region, coveragerr_dummy2, coveragerr_global)

# The dataframe is in the order I want, so make entity an ordered factor based on
# what I already have. That way ggplot will not reorder by entity name
# But I need to reverse order for plotting

coveragerr_data$entity <- factor(coveragerr_data$entity, levels = rev(coveragerr_data$entity))


# plot as horizontal error bars
coveragerr_plot <- coveragerr_data %>%
                    ggplot(aes(x=entity,
                               y=c_rr_coverage)) +
                    geom_point() +
                    labs(x="",
                         y="Treatment coverage (%)",
                         title=paste0("Figure 4.20\nEstimated MDR/RR-TB treatment coverage for MDR/RR-TB\n",
                                     "(patients started on treatment for MDR-TB as a percentage of the estimated incidence of MDR/RR-TB) in ",
                                     report_year - 1,
                                     ",\n30 high MDR-TB burden countries, WHO Regions and globally")) +
                    geom_pointrange(aes(ymin=c_rr_coverage_lo,
                                        ymax=c_rr_coverage_hi)) +
                    theme_glb.rpt() +
                    theme(plot.title = element_text(hjust = 0)) +
                    expand_limits(y=0) +
                    coord_flip()

# If there are countries with no data then add a footnote
if (coveragerr_nodata_count > 0)
  {
  coveragerr_plot <- arrangeGrob(coveragerr_plot,
                                bottom = textGrob("* No data",
                                               x = 0,
                                               hjust = -0.1,
                                               vjust=0,
                                               gp = gpar(fontsize = 10)))
  }

# Save the plot
figsave(coveragerr_plot, coveragerr_data, "f4_20_txcoverage_drtb")

# Clean up (remove any objects with their name starting with 'coveragerr')
rm(list=ls(pattern = "^coveragerr"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.21   ------
# Treatment outcomes for new and relapse TB cases in 2015,
# 30 high TB burden countries, WHO regions and globally
#
#
# NOTE CHANGE FOR 2017 REPORT: SORT BY TSR, NOT COUNTRY NAME!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


txout_country  <- outcomes %>%
                  filter(year==report_year - 2) %>%
                  select(country,
                         iso2,
                         g_whoregion,
                         rel_with_new_flg,
                         contains("newrel_"))  %>%
                  # shorten long country names
                  get_names_for_tables() %>%
                  rename(entity = country ) %>%

                  # Add an asterisk to the name if country did not include
                  #relapse cases in the outcomes cohort

                  mutate(entity = ifelse(!is.na(rel_with_new_flg) & rel_with_new_flg == 0,
                                         paste0(entity, "*"),
                                         entity)) %>%
                  select(-rel_with_new_flg)


# Calculate regional aggregates
txout_region <- txout_country %>%
                group_by(g_whoregion) %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               contains("newrel_")) %>%

                  # merge with regional names and simplify to match structure of country table
                inner_join(who_region_names, by = "g_whoregion") %>%
                select(-g_whoregion)

# Calculate global aggregate
txout_global <- txout_country %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               contains("newrel_")) %>%
                # Add dummy variable to match structure of country table
                mutate(rel_with_new_flg = NA) %>%

                # Add dummy variable and simplify to match structure of country table
                mutate(entity = "Global") %>%
                select(-rel_with_new_flg)


# Filter the country list down to high burden ones
txout_30hbc <- country_group_membership %>%
               filter(group_type == "g_hb_tb" & group_name == 1) %>%
               select(iso2)

txout_country <- txout_country %>%
                  inner_join(txout_30hbc) %>%
                  #remove the iso2 field to match regional and countries aggregates
                  select(-iso2,
                         -g_whoregion)


# Check if any of the countries have no data so that we can add a 'No data reported' option
txout_nodata_count <- txout_country %>%
                      filter(is.na(newrel_coh)) %>%
                      nrow()

# Calculate outcome proportions for plotting as stacked bars
txout_country <- txout_country %>%
                 calculate_outcomes_pct("newrel_")


# Sort in descending order of success rate
txout_country <- txout_country %>%
                 arrange(desc(`Treatment success`))


# Calculate outcome proportions for regional aggregates
txout_region <- txout_region %>%
                calculate_outcomes_pct("newrel_")

# Sort regions in descending order of success rate
txout_region <- txout_region %>%
                 arrange(desc(`Treatment success`))

# Calculate outcome proportions for global aggregates
txout_global <- txout_global %>%
                calculate_outcomes_pct("newrel_")

# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
txout_dummy1 <- data.frame(entity = "-----", coh = NA, succ = NA, fail = NA,
                           died = NA, lost = NA, c_neval = NA,
                           Failure = NA, Died = NA)

# Had to use mutate to create the next 3 fields because data.frame converted spaces to dots. Grrr
txout_dummy1 <- txout_dummy1 %>%
                mutate(`Treatment success` = NA,
                       `Lost to follow-up` = NA,
                       `Not evaluated` = NA)

txout_dummy2 <- txout_dummy1 %>% mutate(entity = "------")



# Add a 'no data' option so non-reporters are highlighted in the output
# (but only if we have at least one country with no data)
if (txout_nodata_count > 0 )
  {
  txout_country <- txout_country %>%
                    mutate(`No data reported` = ifelse(is.na(coh) & substring(entity,1,2) != "--" ,100,0))

  txout_region <- txout_region %>% mutate(`No data reported` = NA)
  txout_global <- txout_global %>% mutate(`No data reported` = NA)
  txout_dummy1 <- txout_dummy1 %>% mutate(`No data reported` = NA)
  txout_dummy2 <- txout_dummy2 %>% mutate(`No data reported` = NA)
  }


# Create combined table in order of countries then regional and global estimates
txout <- rbind(txout_country, txout_dummy1, txout_region, txout_dummy2, txout_global)

txout <- txout %>%
          # Keep record of current order (in reverse) so plot comes out as we want it
          mutate(entity = factor(entity, levels=rev(entity))) %>%
          # Drop the actual numbers and keep percentages
          select(-coh, -succ, -fail, -died, -lost, -c_neval)


# Flip into long mode for stacked bar plotting
txout_long <- melt(txout, id=1)


# stacking order changed with upgrade of ggplot to version 2.2. GRRRRRRRR
# Why GRRRRRR? Because, of course, this broke existing code that was working!
# Finally figured out the solution -- use geom_col with the following parameter
# geom_col(position = position_stack(reverse = TRUE))
#
# It also helped to have a named list for the colour palette.
# See http://ggplot2.tidyverse.org/reference/geom_bar.html  and
#     http://ggplot2.tidyverse.org/reference/scale_manual.html

# Plot as stacked bars
txout_plot <- txout_long %>%
              ggplot(aes(entity,
                         value,
                         fill = variable)) +

                      geom_col(position = position_stack(reverse = TRUE)) +
                      coord_flip() +

                      theme_glb.rpt() +
                      scale_fill_manual("", values = outcomes_palette()) +
                      labs(x="", y="Percentage of cohort (%)") +

                      theme(legend.position="bottom",
                            panel.grid=element_blank()) +

                      expand_limits(c(0,0)) +

                      ggtitle(paste0("Figure 4.21\nTreatment outcomes for new and relapse TB cases in ",
                                     report_year - 2,
                                     ",\n30 high TB burden countries, WHO regions and globally"))

txout_plot <- arrangeGrob(txout_plot,
                          bottom = textGrob(paste0("* Treatment outcomes are for new cases only.",
                                                   "\nNOTE FOR SUE: PLEASE ADD SUCCESS RATE NUMBER ON EACH LINE AS WAS DONE\nIN LAST YEAR'S FIGURE 4.20"),
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0,
                                         gp = gpar(fontsize = 10)))


figsave(txout_plot, txout, "f4_21_outcomes_tb", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txout')
rm(list=ls(pattern = "^txout"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.22  ------
# Treatment outcomes for new and relapse TB cases (absolute numbers), 2000 - 2015,
# globally and for WHO regions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


txoutnum_regional <- outcomes %>%
                      filter(year >= 2000 & year <= report_year - 2) %>%
                      select(year,
                             g_whoregion,
                             new_sp_coh,
                             new_sp_cur,
                             new_sp_cmplt,
                             c_new_sp_neval,
                             new_snep_coh,
                             new_snep_cmplt,
                             c_new_snep_neval,
                             newrel_coh,
                             newrel_succ,
                             c_newrel_neval)%>%
                      group_by(year, g_whoregion) %>%
                      summarise_each(funs(sum(.,na.rm = TRUE)),
                                     new_sp_coh:c_newrel_neval)  %>%

                      # merge with regional names
                      inner_join(who_region_names, by = "g_whoregion") %>%
                      select(-g_whoregion) %>%
                      ungroup()

txoutnum_global <- outcomes %>%
                    filter(year >= 2000 & year <= report_year - 2) %>%
                    select(year,
                           g_whoregion,
                           new_sp_coh,
                           new_sp_cur,
                           new_sp_cmplt,
                           c_new_sp_neval,
                           new_snep_coh,
                           new_snep_cmplt,
                           c_new_snep_neval,
                           newrel_coh,
                           newrel_succ,
                           c_newrel_neval)%>%
                    group_by(year) %>%
                    summarise_each(funs(sum(.,na.rm = TRUE)),
                                   new_sp_coh:c_newrel_neval)  %>%
                    mutate(entity = "Global") %>%
                    ungroup()

#Combine regional and global data and reorganise
txoutnum_data <- rbind(txoutnum_regional, txoutnum_global)

txoutnum_data$entity <- factor(txoutnum_data$entity,
                            levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))


# Simplify the data for plotting

txoutnum_data <- txoutnum_data %>%
                  mutate(success = (new_sp_cur + new_sp_cmplt + new_snep_cmplt + newrel_succ) / 1e6,
                         neval = (c_new_sp_neval + c_new_snep_neval + c_newrel_neval) / 1e6,
                         coh = (new_sp_coh + new_snep_coh + newrel_coh) / 1e6) %>%
                  mutate(fail_other = coh - success - neval) %>%
                  select(entity,
                         year,
                         `Treatment success` = success,
                         `Failure/Died/Lost to follow-up` = fail_other,
                         `Not evaluated` = neval)


# Flip into long mode for stacked bar plotting
# (see http://stackoverflow.com/a/35500964 for why I had to use as.data.frame() )
txoutnum_long <- melt(as.data.frame(txoutnum_data), id=c("entity","year"))


# Plot as stacked bars, with global separate from regions so can use different scales
# otherwise AMR, EMR and EUR are hard to see

txoutnum_plot_reg <- txoutnum_long %>%
                  filter(entity!="Global") %>%
                  ggplot(aes(year, value, fill=variable)) +
                  geom_col(position = position_stack(reverse = TRUE)) +

                  facet_wrap( ~ entity) +

                  theme_glb.rpt() +
                  scale_fill_manual("", values = outcomes_num_palette()) +
                  labs(x="", y="Number of cases (millions)") +

                  theme(legend.position="bottom",
                        panel.grid=element_blank()) +
                  expand_limits(c(0,0))

txoutnum_plot_glob <- txoutnum_long %>%
                  filter(entity=="Global") %>%
                  ggplot(aes(year, value, fill=variable)) +
                  geom_col(position = position_stack(reverse = TRUE)) +

                  facet_wrap( ~ entity, nrow = 3, ncol = 2) +

                  theme_glb.rpt() +
                  scale_fill_manual("", values = outcomes_num_palette()) +
                  labs(x="", y="Number of cases (millions)") +

                  theme(legend.position="bottom",
                        panel.grid=element_blank()) +

                  expand_limits(c(0,0))


txoutnum_plot <- arrangeGrob(txoutnum_plot_glob,
                             txoutnum_plot_reg,
                             nrow = 2,
                             ncol = 1,
                             top = textGrob(label = paste0("Figure 4.22\nTreatment outcomes for new and relapse TB cases(a) (absolute numbers), 2000 - ",
                           report_year-2,
                           ", globally\nand for WHO regions."),
                                             x = 0.02,
                                             just = "left",
                                             gp = gpar(fontsize = 10)),

                           bottom = textGrob("(a) Cohorts before 2012 included new cases only.",
                                             x = 0.02,
                                             just = "left",
                                             gp = gpar(fontsize = 10)))


# Save the plot
figsave(txoutnum_plot, txoutnum_data, "f4_22_outcomes_absolute", width=7, height=11)

# Clean up (remove any objects with their name starting 'txout')
rm(list=ls(pattern = "^txoutnum"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.24   ------
# Treatment outcomes for new and relapse TB/HIV cases in 2015,
# 30 high TB/HIV burden countries, WHO regions and globally
#
# NOTE CHANGE FOR 2017 REPORT: SORT BY TSR, NOT COUNTRY NAME!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

txtbhivout_country  <-  outcomes %>%
                        filter(year==report_year - 2) %>%
                        select(country,
                               iso2,
                               g_whoregion,
                               contains("tbhiv_")) %>%
                        # remove un-needed variables
                        select(-c_tbhiv_tsr) %>%
                        # shorten long country names
                        get_names_for_tables() %>%
                        rename(entity = country ) %>%
                        arrange(entity)

# Calculate regional aggregates
txtbhivout_region <- txtbhivout_country %>%
                group_by(g_whoregion) %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               contains("tbhiv_")) %>%

                # merge with regional names and simplify to match structure of country table
                inner_join(who_region_names, by = "g_whoregion") %>%
                select(-g_whoregion)


# Calculate global aggregate
txtbhivout_global <- txtbhivout_country %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               contains("tbhiv_")) %>%

                # Add dummy variable and simplify to match structure of country table
                mutate(entity = "Global")

# Filter the country list down to high burden ones
txtbhivout_30hbc <- country_group_membership %>%
                    filter(group_type == "g_hb_tbhiv" & group_name == 1) %>%
                    select(iso2)

txtbhivout_country <- txtbhivout_country %>%
                  inner_join(txtbhivout_30hbc) %>%
                  #remove the iso2 field to match regional and countries aggregates
                  select(-iso2,
                         -g_whoregion)

# Check if any of the countries have no data so that we can add a 'No data reported' option
# (This includes countries reporting a cohort of zero as this would be a data entry error for such countries!)
txtbhivout_nodata_count <- txtbhivout_country %>%
                            filter(is.na(tbhiv_coh) | tbhiv_coh == 0) %>%
                            nrow()

# Calculate outcome proportions for plotting as stacked bars
txtbhivout_country <- txtbhivout_country %>%
                      calculate_outcomes_pct("tbhiv_")

# Sort in descending order of success rate
txtbhivout_country <- txtbhivout_country %>%
                      arrange(desc(`Treatment success`))

# Calculate outcome proportions for regional aggregates
txtbhivout_region <- txtbhivout_region %>%
                     calculate_outcomes_pct("tbhiv_")

# Sort regions in descending order of success rate
txtbhivout_region <- txtbhivout_region %>%
                     arrange(desc(`Treatment success`))

# Calculate outcome proportions for global aggregates
txtbhivout_global <- txtbhivout_global %>%
                     calculate_outcomes_pct("tbhiv_")

# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
txtbhivout_dummy1 <- data.frame(entity = "-----", coh = NA, succ = NA, fail = NA,
                                 died = NA, lost = NA, c_neval = NA,
                                 Failure = NA, Died = NA)

# Had to use mutate to create the next 3 fields because data.frame converted spaces to dots. Grrr
txtbhivout_dummy1 <- txtbhivout_dummy1 %>%
                      mutate(`Treatment success` = NA,
                             `Lost to follow-up` = NA,
                             `Not evaluated` = NA)

txtbhivout_dummy2 <- txtbhivout_dummy1 %>% mutate(entity = "------")


# Add a 'no data' option so non-reporters are highlighted in the output
# (but only if we have at least one country with no data)
if (txtbhivout_nodata_count > 0 )
  {
  txtbhivout_country <- txtbhivout_country %>%
                    mutate(`No data reported` = ifelse((is.na(coh) | coh == 0) & substring(entity,1,2) != "--" ,100,0))

  txtbhivout_region <- txtbhivout_region %>% mutate(`No data reported` = NA)
  txtbhivout_global <- txtbhivout_global %>% mutate(`No data reported` = NA)
  txtbhivout_dummy1 <- txtbhivout_dummy1 %>% mutate(`No data reported` = NA)
  txtbhivout_dummy2 <- txtbhivout_dummy2 %>% mutate(`No data reported` = NA)
}


# Create combined table in order of countries then regional and global estimates
txtbhivout <- rbind(txtbhivout_country, txtbhivout_dummy1, txtbhivout_region, txtbhivout_dummy2, txtbhivout_global)


txtbhivout <- txtbhivout %>%
          # Keep record of current order (in reverse) so plot comes out as we want it
          mutate(entity = factor(entity, levels=rev(entity))) %>%
          # Drop the actual numbers and keep percentages
          select(-coh, -succ, -fail, -died, -lost, -c_neval)


# Flip into long mode for stacked bar plotting
txtbhivout_long <- melt(txtbhivout, id=1)

# Plot as stacked bars
txtbhivout_plot <- txtbhivout_long %>%
              ggplot(aes(entity,
                         value,
                         fill = variable)) +

                      geom_col(position = position_stack(reverse = TRUE)) +
                      coord_flip() +

                      theme_glb.rpt() +
                      scale_fill_manual("", values = outcomes_palette()) +
                      labs(x="", y="Percentage of cohort (%)") +

                      theme(legend.position="bottom",
                            panel.grid=element_blank()) +

                      expand_limits(c(0,0)) +

                      ggtitle(paste0("Figure 4.24\nTreatment outcomes for new and relapse TB/HIV cases in\n",
                                     report_year - 2,
                                     ", 30 high TB/HIV burden countries, WHO regions and globally"))


txtbhivout_plot <- arrangeGrob(txtbhivout_plot,
                          bottom = textGrob("NOTE FOR SUE: PLEASE ADD SUCCESS RATE NUMBER ON EACH LINE AS WAS DONE\nIN LAST YEAR'S FIGURE 4.22",
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0,
                                         gp = gpar(fontsize = 10)))



figsave(txtbhivout_plot, txtbhivout, "f4_24_outcomes_tbhiv", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txtbhivout')
rm(list=ls(pattern = "^txtbhivout"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.25   ------
# Treatment outcomes for rifampicin-resistant TB cases started on treatment in 2014,
# 30 high MDR-TB burden countries, WHO regions and globally
#
# NOTE CHANGE FOR 2017 REPORT: SORT BY TSR, NOT COUNTRY NAME!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


txmdrout_country  <- outcomes %>%
                  filter(year==report_year - 3) %>%
                  select(country,
                         iso2,
                         g_whoregion,
                         contains("mdr_")) %>%
                  # drop old cured/completed and the tsr fields
                  select(-mdr_cur, -mdr_cmplt, -c_mdr_tsr) %>%
                  # shorten long country names
                  get_names_for_tables() %>%
                  rename(entity = country ) %>%
                  arrange(entity)

# Calculate regional aggregates
txmdrout_region <- txmdrout_country %>%
                group_by(g_whoregion) %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               contains("mdr_")) %>%

                  # merge with regional names and simplify to match structure of country table
                inner_join(who_region_names, by = "g_whoregion") %>%
                select(-g_whoregion)


# Calculate global aggregate
txmdrout_global <- txmdrout_country %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               contains("mdr_")) %>%

                # Add dummy variable and simplify to match structure of country table
                mutate(entity = "Global")

# Filter the country list down to high burden ones
txmdrout_30hbc <- country_group_membership %>%
                  filter(group_type == "g_hb_mdr" & group_name == 1) %>%
                  select(iso2)

txmdrout_country <- txmdrout_country %>%
                  inner_join(txmdrout_30hbc) %>%
                  #remove the iso2 field to match regional and countries aggregates
                  select(-iso2,
                         -g_whoregion)

# Check if any of the countries have no data so that we can add a 'No data reported' option
# (This includes countries reporting a cohort of zero as this would be a data entry error for such countries!)
txmdrout_nodata_count <- txmdrout_country %>%
                            filter(is.na(mdr_coh) | mdr_coh == 0) %>%
                            nrow()

# Calculate outcome proportions for plotting as stacked bars
txmdrout_country <- txmdrout_country %>%
                    calculate_outcomes_pct("mdr_")

# Sort in descending order of success rate
txmdrout_country <- txmdrout_country %>%
                    arrange(desc(`Treatment success`))

# Calculate outcome proportions for regional aggregates
txmdrout_region <- txmdrout_region %>%
                   calculate_outcomes_pct("mdr_")

# Sort regions in descending order of success rate
txmdrout_region <- txmdrout_region %>%
                   arrange(desc(`Treatment success`))

# Calculate outcome proportions for global aggregates
txmdrout_global <- txmdrout_global %>%
                   calculate_outcomes_pct("mdr_")



# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
txmdrout_dummy1 <- data.frame(entity = "-----", coh = NA, succ = NA, fail = NA,
                                 died = NA, lost = NA, c_neval = NA,
                                 Failure = NA, Died = NA)

# Had to use mutate to create the next 3 fields because data.frame converted spaces to dots. Grrr
txmdrout_dummy1 <- txmdrout_dummy1 %>%
                    mutate(`Treatment success` = NA,
                           `Lost to follow-up` = NA,
                           `Not evaluated` = NA)

txmdrout_dummy2 <- txmdrout_dummy1 %>% mutate(entity = "------")

# Add a 'no data' option so non-reporters are highlighted in the output
# (but only if we have at least one country with no data)
if (txmdrout_nodata_count > 0 )
  {
  txmdrout_country <- txmdrout_country %>%
                    mutate(`No data reported` = ifelse((is.na(coh) | coh == 0) & substring(entity,1,2) != "--" ,100,0))

  txmdrout_region <- txmdrout_region %>% mutate(`No data reported` = NA)
  txmdrout_global <- txmdrout_global %>% mutate(`No data reported` = NA)
  txmdrout_dummy1 <- txmdrout_dummy1 %>% mutate(`No data reported` = NA)
  txmdrout_dummy2 <- txmdrout_dummy2 %>% mutate(`No data reported` = NA)
}


# Create combined table in order of countries then regional and global estimates
txmdrout <- rbind(txmdrout_country, txmdrout_dummy1, txmdrout_region, txmdrout_dummy2, txmdrout_global)


txmdrout <- txmdrout %>%
          # Keep record of current order (in reverse) so plot comes out as we want it
          mutate(entity = factor(entity, levels=rev(entity))) %>%
          # Drop the actual numbers and keep percentages
          select(-coh, -succ, -fail, -died, -lost, -c_neval)


# Flip into long mode for stacked bar plotting
txmdrout_long <- melt(txmdrout, id=1)

# Plot as stacked bars
txmdrout_plot <- txmdrout_long %>%
              ggplot(aes(entity,
                         value,
                         fill = variable)) +

                      geom_col(position = position_stack(reverse = TRUE)) +
                      coord_flip() +

                      theme_glb.rpt() +
                      scale_fill_manual("", values = outcomes_palette()) +
                      labs(x="", y="Percentage of cohort (%)") +

                      theme(legend.position="bottom",
                            panel.grid=element_blank()) +

                      expand_limits(c(0,0)) +

                      ggtitle(paste0("Figure 4.25\nTreatment outcomes for rifampicin-resistant TB cases\nstarted on treatment in ",
                                     report_year - 3,
                                     ",\n30 high MDR-TB burden countries, WHO regions and globally"))


txmdrout_plot <- arrangeGrob(txmdrout_plot,
                          bottom = textGrob("NOTE FOR SUE: PLEASE ADD SUCCESS RATE NUMBER ON EACH LINE AS WAS DONE\nIN LAST YEAR'S FIGURE 4.23",
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0,
                                         gp = gpar(fontsize = 10)))



figsave(txmdrout_plot, txmdrout, "f4_25_outcomes_mdr", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txmdrout')
rm(list=ls(pattern = "^txmdrout"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.23 ------
# Treatment outcomes for new and relapse TB cases (2012-2015), new and relapse cases among people living with HIV (2012-2015) and rifampicin-resistant TB cases (2012-2014), globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

out_tb_data <- outcomes %>%
                filter(between(year, 2012, report_year - 2)) %>%
                select(iso2,
                       year,
                       contains("newrel_")) %>%

                # calculate global aggregates
                group_by(year) %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                            newrel_coh:c_newrel_neval) %>%
                ungroup()%>%

                # Calculate outcome proportions for plotting as stacked bars
                calculate_outcomes_pct("newrel_") %>%

                # Drop the actual numbers and keep percentages
                select(-coh, -succ, -fail, -died, -lost, -c_neval) %>%

                # Add tx group type
                mutate(subgroup = "New and relapse TB cases")



out_hiv_data <- outcomes %>%
                filter(between(year, 2012, report_year - 2)) %>%
                select(iso2,
                       year,
                       contains("tbhiv_")) %>%

                # calculate global aggregates
                group_by(year) %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                                tbhiv_coh:c_tbhiv_neval) %>%
                ungroup() %>%

                # Calculate outcome proportions for plotting as stacked bars
                calculate_outcomes_pct("tbhiv_") %>%

                # Drop the actual numbers and keep percentages
                select(-coh, -succ, -fail, -died, -lost, -c_neval) %>%

                # Add tx group type
                mutate(subgroup = "New and relapse TB/HIV cases")



out_mdr_data <- outcomes %>%
                filter(between(year, 2012, report_year - 2)) %>%
                select(iso2,
                       year,
                       contains("mdr_")) %>%


                # calculate global aggregates
                group_by(year) %>%
                summarise_each(funs(sum(., na.rm = TRUE)),
                               mdr_coh:c_mdr_neval) %>%
                ungroup() %>%

                # Calculate outcome proportions for plotting as stacked bars
                calculate_outcomes_pct("mdr_") %>%

                # Drop the actual numbers and keep percentages, plu sother unwanted variables
                select(-coh, -succ, -fail, -died, -lost, -c_neval, -cur, -cmplt) %>%

                # Add tx group type
                mutate(subgroup = "Rifampicin-resistant TB cases")

# Combine the three data frames
out_data <- rbind(out_tb_data, out_hiv_data, out_mdr_data)


# Flip into long mode for stacked bar plotting
# Rather bizerly, if I don';'t do the next line I get an error message
# See https://stackoverflow.com/a/35500964

out_data <- as.data.frame(out_data)

out_data_long <- melt(out_data, id=c(1,7))

# Plot as stacked bars
out_plot <- out_data_long %>%
            ggplot(aes(year,
                       value,
                       fill = variable)) +

                    geom_col(position = position_stack(reverse = TRUE)) +
                    facet_wrap( ~ subgroup, ncol = 3) +
                    coord_flip() +

                    theme_glb.rpt() +
                    scale_fill_manual("", values = outcomes_palette()) +

                    labs(x="Year started on treatment", y="Percentage of cohort (%)") +

                    theme(legend.position="bottom",
                          panel.grid=element_blank()) +

                    expand_limits(c(0,0)) +

                    ggtitle(paste0("Figure 4.23\nTreatment outcomes for new and relapse TB cases, new and relapse cases among people living with HIV,\nand rifampicin-resistant TB cases, 2012 - ",
                                   report_year - 2,
                                   " globally"))

out_plot <- arrangeGrob(out_plot,
                        bottom = textGrob("NOTE FOR SUE: PLEASE ADD SUCCESS RATE NUMBER ON EACH LINE AS FOR THE OTHER OUTCOME FIGURES",
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0,
                                         gp = gpar(fontsize = 10)))

figsave(out_plot, out_data_long, "f4_23_outcomes_tb_hiv_mdr") # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'out_')
rm(list=ls(pattern = "^out_"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.26  (map) ------
# Countries that had used shorter MDR–TB treatment regimens by the end of 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

short_data <- notification %>%
              filter(year == report_year - 1) %>%
              select(country,
                     iso3,
                     mdr_shortreg_used) %>%
              mutate(cat = ifelse(mdr_shortreg_used == 1, "Shorter MDR–TB\ntreatment regimens used",
                            ifelse(mdr_shortreg_used == 0, "Not used",
                            ifelse(mdr_shortreg_used == 3, "Don't know", NA))))

short_data$cat <-factor(short_data$cat)

# produce the map
short_map<- WHOmap.print(short_data,
                        paste0("Figure 4.26\nCountries that had used shorter MDR–TB treatment regimens by the end of ", report_year - 1),
                           legend.title = "Country\nresponse",
                           copyright=FALSE,
                           brewer.pal(3, "Greens"),
                           na.label="No response",
                           show=FALSE)

figsave(short_map,
        short_data,
        "f4_26_short_regimen_map")


# Clean up (remove any objects with their name beginning with 'short')
rm(list=ls(pattern = "^short"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.27a  (map) ------
# Countries that had used bedaquiline for the treatment of M/XDR–TB as part of expanded access,
# compassionate use or under normal programmatic conditions by the end of June 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bdq_data <- read.csv(file = paste0(rdata_folder, "bdq.dlm.maps.csv"))

bdq_data <- bdq_data %>%
            mutate(cat = ifelse(mdrxdr_bdq_used == 1, "Bedaquiline used",
                         ifelse(mdrxdr_bdq_used == 0, "Bedaquiline not used", NA))) %>%

            # drop unnecessary variables
            select(country, iso3, cat)


bdq_data$cat <- factor(bdq_data$cat)

# produce the map
bdq_map<- WHOmap.print(bdq_data,
                        "Figure 4.27a\nCountries that had used bedaquiline for the treatment of M/XDR–TB as part of expanded access,\ncompassionate use or under normal programmatic conditions by the end of June 2017",
                           legend.title = "",
                           copyright=FALSE,
                           colors=c("lightgreen", "darkgreen"),
                           na.label="No response",
                           show=FALSE)

bdq_map <- arrangeGrob(bdq_map,
                       bottom = textGrob("Data shown reflects country reporting supplemented with additional information from pharmaceutical manuacturers.",
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0,
                                         gp = gpar(fontsize = 10)))

figsave(bdq_map,
        bdq_data,
        "f4_27a_bdq_map")


# Clean up (remove any objects with their name beginning with 'bdq')
rm(list=ls(pattern = "^bdq"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.27b  (map) ------
# Countries that had used delamanid for the treatment of M/XDR–TB as part of expanded access,
# compassionate use or under normal programmatic conditions by the end of June 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


dlm_data <- read.csv(file = paste0(rdata_folder, "bdq.dlm.maps.csv"))

dlm_data <- dlm_data %>%
            mutate(cat = ifelse(mdrxdr_dlm_used == 1, "Delamanid used",
                         ifelse(mdrxdr_dlm_used == 0, "Delamanid not used", NA))) %>%

            # drop unnecessary variables
            select(country, iso3, cat)


dlm_data$cat <- factor(dlm_data$cat)

# produce the map
dlm_map<- WHOmap.print(dlm_data,
                        "Figure 4.27b\nCountries that had used delamanid for the treatment of M/XDR–TB as part of expanded access,\ncompassionate use or under normal programmatic conditions by the end of June 2017",
                           legend.title = "",
                           copyright=FALSE,
                           colors=c("lightblue", "darkblue"),
                           na.label="No response",
                           show=FALSE)

dlm_map <- arrangeGrob(dlm_map,
                       bottom = textGrob("Data shown reflects country reporting supplemented with additional information from pharmaceutical manuacturers.",
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0,
                                         gp = gpar(fontsize = 10)))

figsave(dlm_map,
        dlm_data,
        "f4_27b_dlm_map")


# Clean up (remove any objects with their name beginning with 'dlm')
rm(list=ls(pattern = "^dlm"))






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 4.5.1  (Map) ------
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
                        paste("Figure Box 4.5.1\nPercentage of basic management units in which there is community engagement",
                              "\nor provision of treatment adherence support,",
                              report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)


comm_map <- arrangeGrob(comm_map,
                       bottom = textGrob("Data only requested from 114 countries.",
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0,
                                         gp = gpar(fontsize = 10)))

figsave(comm_map,
        select(comm_data,
               iso3,
               comm_pct,
               cat),
        "f4_box_4_5_1_pct_BMU_community_map")



# Clean up (remove any objects with their name beginning with 'comm')
rm(list=ls(pattern = "^comm"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# TB prevention services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.1 (Map) -------
# Availability of data on the number of children aged <5 years who were
# household contacts of bacteriologically confirmed pulmonary TB cases and were started on
# TB preventive treatment, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


kids_data <-  strategy %>%
              filter(year==report_year - 1) %>%
              select(iso3,
                     country,
                     prevtx_data_available,
                     newinc_con04_prevtx,
                     ptsurvey_newinc,
                     ptsurvey_newinc_con04_prevtx) %>%

              # Assign categories
              mutate(cat =
                        ifelse(prevtx_data_available==0 |
                                prevtx_data_available==60 & is.na(newinc_con04_prevtx) |
                                prevtx_data_available==61 & (is.na(ptsurvey_newinc) | is.na(ptsurvey_newinc_con04_prevtx)),
                              "Number not available",
                        ifelse(prevtx_data_available==60,"Number available from routine surveillance",
                        ifelse(prevtx_data_available==61,"Number estimated from a survey" ,NA)))) %>%

              # drop unnecessary variables
              select(country,
                     iso3,
                     cat)


kids_data$cat <- factor(kids_data$cat)



# produce the map
kids_map <- WHOmap.print(kids_data,
                        paste("Figure 5.1\nAvailability of data on the number of children aged <5 years who were\nhousehold contacts of bacteriologically confirmed pulmonary TB cases and were started on",
                              "\nTB preventive treatment,",
                              report_year-1),
                           legend.title = "Country response",
                           copyright=FALSE,
                           colors=c('#91A93E', '#0066B3', '#E5DDB3'),
                           na.label="No response",
                           show=FALSE)

figsave(kids_map,
        kids_data,
        "f5_1_kids_prevtx_map")


# Clean up (remove any objects with their name beginning with 'kids')
rm(list=ls(pattern = "^kids"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.4 (Map) ---------
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

                    # filter out dodgy data where the number of HCWs in a country is clearly too low
                    # Check if logic below still applies in future years !!!!!
                    mutate(nrr = ifelse(hcw_tot < 1000 &
                                       e_pop_15plus > 1e6 &
                                       hcw_tb_infected > 0, NA, nrr)) %>%

                    select(iso3,
                           nrr)


hcw_data$cat <- cut(hcw_data$nrr,
                     c(0, 1, 2, 3, Inf),
                     c('0-0.9', '1-1.9%', '2-2.9%', '>=3'),
               right=FALSE)


# produce the map
hcw_map <- WHOmap.print(hcw_data,
                        paste("Figure 5.4\nNotification rate ratio of TB among healthcare workers\ncompared with the general adult population,", report_year-1),
                           "Notification\nrate ratio",
                           copyright=FALSE,
                           #colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                           show=FALSE)

figsave(hcw_map,
        hcw_data,
        "f5_4_hcw_notf_rate_ratio")

# Clean up (remove any objects with their name beginning with 'hcw')
rm(list=ls(pattern = "^hcw"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 7 ------
# Universal health coverage, social protection and
# and addressing social determinants: Implications for TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 7.4 (Map) ---------
# Out-of-pocket expenditures as a percentage of total health expenditures, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get data from external data views (data downloaded from the World Bank repository)
oop_data <- external_indicator_data %>%
            filter(indicator_id == "SH.XPD.OOPC.TO.ZS" )

# Get the most recent year for which there are data
oop_year <- oop_data %>%
            select(year) %>%
            max()

# Filter the oop data for the latest year
oop_data <- oop_data %>%
            filter(year == oop_year) %>%
            # boil down to bar minimum
            select(country,
                   iso3,
                   value)

# Add the categories
oop_data$cat <- cut(oop_data$value,
                     c(0, 15.5, 30, 45, Inf),
                     c('<=15%', '16-29%', '30-44%', '>=45%'),
               right=FALSE)

# produce the map
oop_map <- WHOmap.print(oop_data,
                        paste("Figure 7.4\nOut-of-pocket expenditures as a percentage of total health expenditures,", oop_year),
                           "Percentage",
                           copyright=FALSE,
                           colors=brewer.pal(4, "OrRd"),
                           show=FALSE)

figsave(oop_map,
        select(oop_data,
               country,
               iso3,
               value,
               cat),
        "f7_4_pct_oop_map")

# Clean up (remove any objects with their name beginning with 'oop')
rm(list=ls(pattern = "^oop"))


