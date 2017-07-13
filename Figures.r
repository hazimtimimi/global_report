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
                                   labels=c("0\u201314", "15\u201324", "25\u201334", "35\u201344", "45\u201354", "55\u201364", "\u226565"))

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
                        paste0("Figure 4.3\nPercentage new and relapse TB cases that were children (aged < 15), ",
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

                # Adjust calculation for EUR pre-2013
                 mutate(bacconf_pct_numerator = ifelse(year < 2013 & g_whoregion == 'EUR',
                                                      # old variables, but using new_labconf
                                                      new_labconf,
                                                      # otherwise keep calculation from previous step
                                                      bacconf_pct_numerator),
                        bacconf_pct_denominator = ifelse(year < 2013 & g_whoregion == 'EUR',
                                                      # old variables
                                                      (new_sp + new_sn + new_su),
                                                      # otherwise keep calculation from previous step
                                                      bacconf_pct_denominator)) %>%

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
                  expand_limits(y=c(20,80)) +
                  xlab("Year") +
                  facet_wrap( ~ entity, ncol = 4) +
                  ggtitle(paste0("Figure 4.4\nPercentage of new and relapse(a) pulmonary TB cases with bacteriological confirmation, 2000 - ",
                               report_year-1,
                               ", globally\nand for WHO regions.")) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
bacconf_foot <- "(a) The calculation is for new pulmonary cases in years prior to 2013 based on smear results, except for the European Region\nwhere data on confirmation by culture was also available."

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
                         bacconf_pct,
                         cat),
        "f4_5_pct_bacconf_map")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.6  (Map) ------
# Percentage of new and relapse TB cases tested using a WHO-recommended
# rapid diagnostic as the initial diagnostic test, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rdxpolicy_data <- notification %>%
                    filter(year == report_year - 1) %>%
                    select(iso3,
                           c_newinc,
                           rdx_data_available,
                           newinc_rdx,
                           rdxsurvey_newinc,
                           rdxsurvey_newinc_rdx) %>%

                    # Calculate percente using WRD as initial diagnostic test
                    mutate(pcnt_wrd = ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                                             newinc_rdx * 100 /c_newinc,
                                      ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                             rdxsurvey_newinc_rdx * 100 /rdxsurvey_newinc, NA)))

rdxpolicy_data$cat <- cut(rdxpolicy_data$pcnt_wrd,
                          c(0, 25, 50, 75, Inf),
                          c('0-24.9', '25-49.9', '50-74.9', '>=75'),
                          right=FALSE)


# produce the map
rdxpolicy_map <- WHOmap.print(rdxpolicy_data,
                              paste0("Figure 4.6\nPercentage of new and relapse TB cases tested using a WHO-recommended rapid diagnostic\nas the initial diagnostic test, ",
                                    report_year-1),
                                 "Percentage",
                                 copyright=FALSE,
                                 colors=brewer.pal(4, "Greens"),
                                 show=FALSE)

figsave(rdxpolicy_map,
        select(rdxpolicy_data,
                         iso3,
                         pcnt_wrd,
                         cat),
        "f4_6_pct_wrd_map")

# Clean up (remove any objects with their name beginning with 'ep')
rm(list=ls(pattern = "^rdxpolicy"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.7  (Map) ------
# Percentage of extrapulmonary cases among new and relapse TB cases, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


ep_data <- notification %>%
                filter(year  >= report_year - 2) %>%
                select(iso3,
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
                        paste0("Figure 4.7\nPercentage of extrapulmonary cases among new and relapse TB cases, ",
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
                         ep_pct,
                         cat),
        "f4_7_pct_ep_map")

# Clean up (remove any objects with their name beginning with 'ep')
rm(list=ls(pattern = "^ep"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.8  ------
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
                  ggtitle(paste0("Figure 4.8\nPercentage of new and relapse(a) TB cases with documented HIV status, 2004 - ",
                               report_year-1,
                               ", globally and for WHO regions")) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
hivstatus_foot <- "(a) The calculation is for all cases in years prior to 2015."

hivstatus_plot <- arrangeGrob(hivstatus_plot, bottom = textGrob(hivstatus_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(hivstatus_plot, hivstatus_data, "f4_8_hivstatus_plot")

# Clean up (remove any objects with their name beginning with 'hivstatus')
rm(list=ls(pattern = "^hivstatus"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.9  (map) ------
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
                     c('0-24.9', '25-49.9', '50-74.9', '>=75'),
               right=FALSE)


# produce the map
hivstatus_map <- WHOmap.print(hivstatus_data,
                        paste("Figure 4.9\nPercentage of new and relapse TB cases with documented HIV status(a),", report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           colors=brewer.pal(4, "BuGn"),
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
# Figure 4.10   ------
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

                  ggtitle(paste0("Figure 4.10\nGlobal numbers of notified new and relapse cases(a) known to be HIV-positive (black),\nnumber started on antiretroviral therapy (blue) and estimated number of incident HIV-positive TB cases (red), 2004 - ",
                               report_year-1,
                         ".\nShaded areas represent uncertainty bands.")) +

                  theme_glb.rpt()


# Add footnote
inctbhiv_foot <- "(a) The calculation is for all cases in years prior to 2015."

inctbhiv_plot <- arrangeGrob(inctbhiv_plot, bottom = textGrob(inctbhiv_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(inctbhiv_plot, inctbhiv_data, "f4_10_inctbhiv_plot_global")

# Clean up (remove any objects with their name containing 'tbhiv')
rm(list=ls(pattern = "tbhiv"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.11  (TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Percentage of bacteriologically confirmed TB cases tested for RR–TB, globally and for WHO regions, 2009–2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.12  (Map) ------
# Percentage of bacteriologically confirmed TB cases tested for RR-TB, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


dst_data <- notification %>%
            filter(year >= report_year - 2) %>%
            select(iso3,
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
dst_map <- WHOmap.print(dst_data,
                        paste("Figure 4.12\nPercentage of bacteriologically confirmed TB cases tested for RR-TB,(a),", report_year-1),
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
        select(dst_data,
               iso3,
               dst_pct,
               cat),
        "f4_12_dst_map")

# Clean up (remove any objects with their name beginning with 'dst')
rm(list=ls(pattern = "^dst"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.13  (TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Global number of MDR/RR-TB cases detected (pink) and number enrolled on MDR-TB treatment (green) 2009–2016,
# compared with estimates for 2016 of the number of incident cases of MDR/RR-TB (uncertainty interval shown in blue)
# and the number of MDR/RR-TB cases among notified pulmonary cases (uncertainty interval shown in black)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.14  (TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Number of MDR/RR-TB cases detected (pink) and enrolled on MDR-TB treatment (green) 2009–2016 compared with estimated
# number of MDR/RR-TB cases among notified pulmonary TB cases in 2016 (uncertainty interval shown in red),
# 30 high MDR-TB burden countries
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.15  (map) ------
# Percentage of MDR/RR-TB cases tested for susceptibility to second-line drugs, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sldst_data <- notification %>%
              filter(year == report_year - 1) %>%
              select(iso3,
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


# produce the map
sldst_map <- WHOmap.print(sldst_data,
                          paste0("Figure 4.15\nPercentage of MDR/RR-TB cases tested for susceptibility to second-line drugs, ",
                                    report_year-1),
                                 "Percentage",
                                 copyright=FALSE,
                                 colors=brewer.pal(4, "Blues"),
                                 show=FALSE)

figsave(sldst_map,
        select(sldst_data,
                         iso3,
                         pcnt_sldst,
                         cat),
        "f4_15_pct_sldst_map")

# Clean up (remove any objects with their name beginning with 'sldst')
rm(list=ls(pattern = "^sldst"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.16   ------
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
                #add markers for Bangladesh and India footnotes
                mutate(country = ifelse(country == "Bangladesh", "Bangladesh(a)",
                                        ifelse(country == "India", "India(b)", country)))

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

            ggtitle(paste0("Figure 4.16\nCase notification rates (new and relapse cases, all forms) (black) compared with estimated TB incidence rates  (green),\n2000 - ",
                         report_year-1,
                         ", 30 high TB burden countries. Shaded areas represent uncertainty bands.")) +
            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Add Bangladesh and India footnotes
inc_plot <- arrangeGrob(inc_plot,
                        bottom = textGrob(paste("(a)",
                                                 bangladesh_footnote,
                                                 "\n(b)",
                                                 india_footnote),
                                          x = 0.02,
                                          just = "left",
                                          gp = gpar(fontsize = 8)))


# Save the plot
figsave(inc_plot, inc_data, "f4_16_inc_plot_hbc", width=7, height=11)

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.17   ------
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
                       #add markers for Bangladesh and India footnotes
                       mutate(entity = ifelse(entity == "Bangladesh", "Bangladesh(a)",
                                              ifelse(entity == "India", "India(b)", entity)))


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
                       title=paste("Figure 4.17\nEstimated TB treatment coverage (new and relapse patients as a percentage of estimated TB incidence)\n in",
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
                           bangladesh_footnote,
                           "\n(b)",
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
figsave(coverage_plot, coverage_data, "f4_17_txcoverage_tb")

# Clean up (remove any objects with their name starting with 'coverage')
rm(list=ls(pattern = "^coverage"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.19   ------
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

                  ggtitle(paste0("Figure 4.19\nNumber of new and relapse cases(a) known to be HIV-positive (black) and\nnumber started on ART (blue) compared with estimated number of incident HIV-positive TB cases (red),\n2004 - ",
                               report_year-1,
                               ", 30 high TB/HIV burden countries")) +
                  theme_glb.rpt()


# Add footnote
inctbhiv_foot <- "(a) The calculation is for all cases in years prior to 2015."

inctbhiv_plot <- arrangeGrob(inctbhiv_plot, bottom = textGrob(inctbhiv_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(inctbhiv_plot, inctbhiv_data, "f4_19_inctbhiv_plot_hbc", width=7, height=11)

# Clean up (remove any objects with their name containing 'tbhiv')
rm(list=ls(pattern = "tbhiv"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.20   ------
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
                         title=paste0("Figure 4.20\nEstimated ART treatment coverage for HIV-positive TB cases\n",
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
figsave(coveragehiv_plot, coveragehiv_data, "f4_20_txcoverage_tbhiv")

# Clean up (remove any objects with their name starting with 'coveragehiv')
rm(list=ls(pattern = "^coveragehiv"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.21   ------
# Estimated MDR/RR-TB treatment coverage for MDR/RR-TB
# (patients started on treatment for MDR-TB as a percentage of the estimated number of MDR/RR-TB cases among notified pulmonary TB cases)
# in 2016, 30 high MDR-TB burden countries, WHO regions and globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


coveragerr_inc_country <- estimates_drtb_rawvalues %>%
                          filter(year == report_year - 1) %>%
                          select(entity = country,
                                  iso2,
                                  e_rr_in_notified_pulm,
                                  e_rr_in_notified_pulm_lo,
                                  e_rr_in_notified_pulm_hi)  %>%
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
                       mutate(c_rr_coverage = rrmdr_tx * 100 / e_rr_in_notified_pulm,
                              c_rr_coverage_lo = rrmdr_tx * 100  / e_rr_in_notified_pulm_hi,
                              c_rr_coverage_hi = rrmdr_tx * 100  / e_rr_in_notified_pulm_lo,
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
                                  e_rr_in_notified_pulm,
                                  e_rr_in_notified_pulm_lo,
                                  e_rr_in_notified_pulm_hi)

coveragerr_region <- notification %>%
                      filter(year == report_year - 1) %>%
                      group_by(g_whoregion) %>%
                      summarise_each(funs(sum(., na.rm = TRUE)),
                                     unconf_rrmdr_tx, conf_rrmdr_tx) %>%
                      mutate(rrmdr_tx = unconf_rrmdr_tx +  conf_rrmdr_tx) %>%

                      # merge with estimates and calculate treatment coverage
                      inner_join(coveragerr_inc_region) %>%
                       mutate(c_rr_coverage = rrmdr_tx * 100 / e_rr_in_notified_pulm,
                              c_rr_coverage_lo = rrmdr_tx * 100  / e_rr_in_notified_pulm_hi,
                              c_rr_coverage_hi = rrmdr_tx * 100  / e_rr_in_notified_pulm_lo) %>%

                      # merge with regional names and simplify
                      inner_join(who_region_names, by = "g_whoregion") %>%
                      select(entity,
                              c_rr_coverage,
                              c_rr_coverage_lo,
                              c_rr_coverage_hi)  %>%
                      arrange(desc(c_rr_coverage))


coveragerr_inc_global <- aggregated_estimates_drtb_rawvalues %>%
                         filter(year == report_year - 1 & group_type == "global") %>%
                         select(e_rr_in_notified_pulm,
                                e_rr_in_notified_pulm_lo,
                                e_rr_in_notified_pulm_hi)%>%
                         mutate(entity = "Global")


coveragerr_global <- notification %>%
                      filter(year == report_year - 1) %>%
                      summarise_each(funs(sum(., na.rm = TRUE)),
                                     unconf_rrmdr_tx, conf_rrmdr_tx) %>%
                      mutate(rrmdr_tx = unconf_rrmdr_tx +  conf_rrmdr_tx,
                             entity = "Global") %>%

                      # merge with estimates and calculate treatment coverage
                      inner_join(coveragerr_inc_global) %>%
                      mutate(c_rr_coverage = rrmdr_tx * 100 / e_rr_in_notified_pulm,
                              c_rr_coverage_lo = rrmdr_tx * 100  / e_rr_in_notified_pulm_hi,
                              c_rr_coverage_hi = rrmdr_tx * 100  / e_rr_in_notified_pulm_lo) %>%
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
                         title=paste0("Figure 4.21 Estimated MDR/RR-TB treatment coverage for MDR/RR-TB\n",
                                     "(patients started on treatment for MDR-TB as a percentage of the estimated number of MDR/RR-TB cases\namong notified pulmonary TB cases) in ",
                                     report_year - 1,
                                     ", 30 high MDR-TB burden countries, WHO Regions and globally")) +
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
figsave(coveragerr_plot, coveragerr_data, "f4_21_txcoverage_drtb")

# Clean up (remove any objects with their name starting with 'coveragerr')
rm(list=ls(pattern = "^coveragerr"))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.22   ------
# Treatment outcomes for new and relapse TB cases in 2015,
# 30 high TB burden countries, WHO regions and globally
#
#
#  !!!! NOTE CHANGE FOR 2017 REPORT: SORT BY TSR, NOT COUNTRY NAME!
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
                  select(-rel_with_new_flg) %>%
                  arrange(entity)

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


# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
txout_dummy1 <- data.frame(entity = "-----", newrel_coh = NA, newrel_succ = NA, newrel_fail = NA,
                           newrel_died = NA, newrel_lost = NA, c_newrel_neval = NA)
txout_dummy2 <- data.frame(entity = "------", newrel_coh = NA, newrel_succ = NA, newrel_fail = NA,
                           newrel_died = NA, newrel_lost = NA, c_newrel_neval = NA)



# Create combined table in order of countries then regional and global estimates
txout <- rbind(txout_country, txout_dummy1, txout_region, txout_dummy2, txout_global)

# Calculate outcome proportions for plotting as stacked bars
txout <- txout %>%
          mutate(`Treatment success` = ifelse(NZ(newrel_coh) > 0,
                                              newrel_succ * 100 / newrel_coh,
                                              NA),
                 Failure = ifelse(NZ(newrel_coh) > 0,
                                      newrel_fail * 100 / newrel_coh,
                                      NA),
                 Died = ifelse(NZ(newrel_coh) > 0,
                                  newrel_died * 100 / newrel_coh,
                                  NA),
                 `Lost to follow-up` = ifelse(NZ(newrel_coh) > 0,
                                              newrel_lost * 100 / newrel_coh,
                                              NA),
                 `Not evaluated` = ifelse(NZ(newrel_coh) > 0,
                                          c_newrel_neval * 100 / newrel_coh,
                                          NA))

# Add a 'no data' option so non-reporters are highlighted in the output
# (but only if we have at least one country with no data)
if (txout_nodata_count > 0 )
  {
  txout <- txout %>%
            mutate(`No data reported` = ifelse(is.na(newrel_coh) & substring(entity,1,2) != "--" ,100,0))
  }


txout <- txout %>%
          # Keep record of current order (in reverse) so plot comes out as we want it
          mutate(entity = factor(entity, levels=rev(entity))) %>%
          # Drop the actual numbers and keep percentages
          select(-contains("newrel"))


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

                      ggtitle(paste0("Figure 4.22\nTreatment outcomes for new and relapse TB cases in ",
                                     report_year - 2,
                                     ",\n30 high TB burden countries, WHO regions and globally"))

txout_plot <- arrangeGrob(txout_plot,
                          bottom = textGrob("* Treatment outcomes are for new cases only.",
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0,
                                         gp = gpar(fontsize = 10)))


figsave(txout_plot, txout, "f4_22_outcomes_tb", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txout')
rm(list=ls(pattern = "^txout"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.23  ------
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
                             top = textGrob(label = paste0("Figure 4.23\nTreatment outcomes for new and relapse TB cases(a) (absolute numbers), 2000 - ",
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
figsave(txoutnum_plot, txoutnum_data, "f4_23_outcomes_absolute", width=7, height=11)

# Clean up (remove any objects with their name starting 'txout')
rm(list=ls(pattern = "^txoutnum"))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.24   ------
# Treatment outcomes for new and relapse TB/HIV cases in 2015,
# 30 high TB/HIV burden countries, WHO regions and globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#
#
#  !!!! NOTE CHANGE FOR 2017 REPORT: SORT BY TSR, NOT COUNTRY NAME!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


txtbhivout_country  <- outcomes %>%
                  filter(year==report_year - 2) %>%
                  select(country,
                         iso2,
                         g_whoregion,
                         contains("tbhiv_")) %>%
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


# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
txtbhivout_dummy1 <- data.frame(entity = "-----", tbhiv_coh = NA, tbhiv_succ = NA, tbhiv_fail = NA,
                           tbhiv_died = NA, tbhiv_lost = NA, c_tbhiv_neval = NA, c_tbhiv_tsr = NA)
txtbhivout_dummy2 <- data.frame(entity = "------", tbhiv_coh = NA, tbhiv_succ = NA, tbhiv_fail = NA,
                           tbhiv_died = NA, tbhiv_lost = NA, c_tbhiv_neval = NA, c_tbhiv_tsr = NA)



# Create combined table in order of countries then regional and global estimates
txtbhivout <- rbind(txtbhivout_country, txtbhivout_dummy1, txtbhivout_region, txtbhivout_dummy2, txtbhivout_global)

# Calculate outcome proportions for plotting as stacked bars
txtbhivout <- txtbhivout %>%
          mutate(`Treatment success` = ifelse(NZ(tbhiv_coh) > 0,
                                              tbhiv_succ * 100 / tbhiv_coh,
                                              NA),
                 Failure = ifelse(NZ(tbhiv_coh) > 0,
                                      tbhiv_fail * 100 / tbhiv_coh,
                                      NA),
                 Died = ifelse(NZ(tbhiv_coh) > 0,
                                  tbhiv_died * 100 / tbhiv_coh,
                                  NA),
                 `Lost to follow-up` = ifelse(NZ(tbhiv_coh) > 0,
                                              tbhiv_lost * 100 / tbhiv_coh,
                                              NA),
                 `Not evaluated` = ifelse(NZ(tbhiv_coh) > 0,
                                          c_tbhiv_neval * 100 / tbhiv_coh,
                                          NA))

# Add a 'no data' option so non-reporters are highlighted in the output
# (but only if we have at least one country with no data)
if (txtbhivout_nodata_count > 0 )
  {
  txtbhivout <- txtbhivout %>%
                mutate(`No data reported` = ifelse((is.na(tbhiv_coh) | tbhiv_coh == 0) & substring(entity,1,2) != "--" ,100,0))
  }


txtbhivout <- txtbhivout %>%
          # Keep record of current order (in reverse) so plot comes out as we want it
          mutate(entity = factor(entity, levels=rev(entity))) %>%
          # Drop the actual numbers and keep percentages
          select(-contains("tbhiv"))


#tsr_table$area <- factor(tsr_table$area, levels=rev(tsr_table$area))


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


figsave(txtbhivout_plot, txtbhivout, "f4_24_outcomes_tbhiv", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txtbhivout')
rm(list=ls(pattern = "^txtbhivout"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.25   ------
# Treatment outcomes for rifampicin-resistant TB cases started on treatment in 2014,
# 30 high MDR-TB burden countries, WHO regions and globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


#
#
#  !!!! NOTE CHANGE FOR 2017 REPORT: SORT BY TSR, NOT COUNTRY NAME!
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



txmdrout_country  <- outcomes %>%
                  filter(year==report_year - 3) %>%
                  select(country,
                         iso2,
                         g_whoregion,
                         contains("mdr_")) %>%
                  # drop old cured/completed fields
                  select(-mdr_cur, -mdr_cmplt) %>%
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


# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
txmdrout_dummy1 <- data.frame(entity = "-----", mdr_coh = NA, mdr_succ = NA, mdr_fail = NA,
                           mdr_died = NA, mdr_lost = NA, c_mdr_neval = NA, c_mdr_tsr = NA)
txmdrout_dummy2 <- data.frame(entity = "------", mdr_coh = NA, mdr_succ = NA, mdr_fail = NA,
                           mdr_died = NA, mdr_lost = NA, c_mdr_neval = NA, c_mdr_tsr = NA)



# Create combined table in order of countries then regional and global estimates
txmdrout <- rbind(txmdrout_country, txmdrout_dummy1, txmdrout_region, txmdrout_dummy2, txmdrout_global)

# Calculate outcome proportions for plotting as stacked bars
txmdrout <- txmdrout %>%
          mutate(`Treatment success` = ifelse(NZ(mdr_coh) > 0,
                                              mdr_succ * 100 / mdr_coh,
                                              NA),
                 Failure = ifelse(NZ(mdr_coh) > 0,
                                      mdr_fail * 100 / mdr_coh,
                                      NA),
                 Died = ifelse(NZ(mdr_coh) > 0,
                                  mdr_died * 100 / mdr_coh,
                                  NA),
                 `Lost to follow-up` = ifelse(NZ(mdr_coh) > 0,
                                              mdr_lost * 100 / mdr_coh,
                                              NA),
                 `Not evaluated` = ifelse(NZ(mdr_coh) > 0,
                                          c_mdr_neval * 100 / mdr_coh,
                                          NA))

# Add a 'no data' option so non-reporters are highlighted in the output
# (but only if we have at least one country with no data)
if (txmdrout_nodata_count > 0 )
  {
  txmdrout <- txmdrout %>%
                mutate(`No data reported` = ifelse((is.na(mdr_coh) | mdr_coh == 0) & substring(entity,1,2) != "--" ,100,0))
  }


txmdrout <- txmdrout %>%
          # Keep record of current order (in reverse) so plot comes out as we want it
          mutate(entity = factor(entity, levels=rev(entity))) %>%
          # Drop the actual numbers and keep percentages
          select(-contains("mdr"))


#tsr_table$area <- factor(tsr_table$area, levels=rev(tsr_table$area))


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


figsave(txmdrout_plot, txmdrout, "f4_25_outcomes_mdr", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txmdrout')
rm(list=ls(pattern = "^txmdrout"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.26  (NEW suggested by Dennis and Annabel) ------
# Treatment outcomes for new and relapse TB cases (2012-2015), new and relapse cases among people living with HIV (2012-2015) and rifampicin-resistant TB cases (2011-2014), globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.27  (MAP TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Countries that had used shorter MDR–TB treatment regimens by the end of 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.28a  (MAP TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Countries that had used bedaquiline for the treatment of M/XDR–TB as part of expanded access,
# compassionate use or under normal programmatic conditions by the end of June 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.28b  (MAP TRANSFERRED OVER FROM DENNIS FOR THE 2017 REPORT) ------
# Countries that had used delamanid for the treatment of M/XDR–TB as part of expanded access,
# compassionate use or under normal programmatic conditions by the end of June 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#  !!!!!!  TO BE DONE !!!!!!!




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 4.6.1  (Map) ------
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
                        paste("Figure Box 4.6.1\nPercentage of basic management units in which there is community engagement",
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
        "f4_box_4_6_1_pct_BMU_community_map")



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
                        paste("Figure 5.1\nAvailability of data on the number number of children aged <5 years who were\nhousehold contacts of bacteriologically confirmed pulmonary TB cases and were started on",
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
                    select(iso3,
                           nrr)


hcw_data$cat <- cut(hcw_data$nrr,
                     c(0, 1, 2, 3, Inf),
                     c('0-0.9', '1-1.9%', '2-2.9%', '>=3'),
               right=FALSE)


# produce the map
hcw_map <- WHOmap.print(hcw_data,
                        paste("Figure 5.4\nNotification rate ratio of TB among healthcare workers\ncompared with the general adult population,", report_year-1),
                           "Notification rate ratio",
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


