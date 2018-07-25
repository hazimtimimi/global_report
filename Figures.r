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
                      "Figure 3.1\nCountries with national case-based surveillance as of July 2018 (a)",
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
# Figure 3.3  (map with bubbles) ------
# Estimated TB incidence in 2016, for countries with at least 100 000 incident cases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

topinc_data <- estimates_epi_rawvalues %>%
  filter(year == report_year - 1 & e_inc_num >= 1e5) %>%
  select(iso3,
         country,
         bubble_size = e_inc_num
  )

# Plot incidence numbers as bubbles

topinc_map <- who_bubble_map(topinc_data,
                             paste0("Figure 3.3\nEstimated TB incidence in ",
                                    report_year - 1,
                                    ", for countries with at least 100 000 incident cases"),
                             " Number of \nincident cases",
                             bubble_colour = "orange",
                             scale_breaks = c(100000,500000,1000000,2500000),
                             scale_limits = c(100000,3000000),
                             scale_labels = c("100 000","500 000","1 000 000","2 500 000"),
                             #Set number of countries need to be labeled with names
                             bubble_label_show_number = 8)
# Save the plot
figsave(topinc_map,
        select(topinc_data,
               iso3,
               country,
               bubble_size),
        "f3_3_inc_bubble_map")

# Clean up (remove any objects with their name beginning with 'topinc')
rm(list=ls(pattern = "^topinc"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 3.22  (map with bubbles) ------
# Estimated incidence of MDR/RR-TB in 2016, for countries with at least 1000 incident cases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rrnum_data <- estimates_drtb_rawvalues %>%
              filter(year == report_year - 1 & e_inc_rr_num >= 1000) %>%
              select(country,
                     iso3,
                     bubble_size = e_inc_rr_num
                     )


# Plot incidence numbers as bubbles

rrnum_map <- who_bubble_map(rrnum_data,
                            paste0("Figure 3.22\n",
                                   "Estimated incidence of MDR/RR-TB in ",
                                   report_year - 1,
                                   ", for countries with at least 1000 incident cases"),
                            " Number of \nincident cases",
                            bubble_colour = "red",
                            scale_breaks = c(1000,10000,100000,150000),
                            scale_limits = c(1000,150000),
                            scale_labels = c("1000","10 000","100 000","150 000"),
                            #Set number of countries need to be labeled with names
                            bubble_label_show_number = 3)


# Save the plot
figsave(rrnum_map,
        select(rrnum_data,
               iso3,
               country,
               bubble_size),
        "f3_22_rr_inc_bubble_map")

# Clean up (remove any objects with their name beginning with 'rrnum')
rm(list=ls(pattern = "^rrnum"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Diagnosis and treatment of TB, HIV-associated TB and drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1   ------
# Case notification rates (new and relapse cases, all forms) (black) compared with estimated TB incidence rates (green),
# 2000–2017, globally and for WHO regions
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
  scale_x_continuous(name="Year",
                     breaks = c(2000, 2005, 2010, 2015, report_year-1)) +
  scale_y_continuous(name = "Rate per 100 000 population per year") +
  ggtitle(paste0("Figure 4.1\nCase notification rates (new and relapse cases, all forms) (black) compared with estimated TB incidence rates (green),\n2000 - ",
                 report_year-1,
                 ", globally and for WHO regions. Shaded areas represent uncertainty bands.")) +
  
  theme_glb.rpt() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.text.x = element_text(size=5))

# Save the plot
figsave(inc_plot, inc_data, "f4_1_inc_plot_aggregates")

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.2  ------
# New and relapse TB case notification rates by age and sex in 2017, globally and for WHO regions
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
	                      summarise_at(vars(newrel_m014:tot_notified),
	                                   sum,
	                                   na.rm = TRUE) %>%
	                      mutate(entity = "Global")

agesex_filtered_regional <- agesex %>%
                      filter(!is.na(tot_notified)) %>%
                      group_by(g_whoregion) %>%
                      summarise_at(vars(newrel_m014:tot_notified),
                                   sum,
                                   na.rm = TRUE) %>%
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

agesex_foot <- paste0("(a) Countries not reporting cases in these categories are excluded. Cases included account for ",
                      agesex_pcnt,
                      "% of reported cases.")



agesex_plot <- arrangeGrob(agesex_plot, bottom = textGrob(agesex_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plots
figsave(agesex_plot, agesex_agg_long, "f4_2_agesex")

# Clean up (remove any objects with their name beginning with 'agesex')
rm(list=ls(pattern = "^agesex"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.3 (map) ------
# Percentage new and relapse TB cases that were children (aged <15), 2017
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
                         paste0("Figure 4.3\nPercentage of new and relapse TB cases that were children (aged <15), ",
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
                    "data were used for",
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
# Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, 2000-2017, globally and for WHO regions
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
                summarise_at(vars(new_sp:ret_rel_clindx),
                             sum,
                             na.rm = TRUE) %>%
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
                  summarise_at(vars(bacconf_pct_numerator:bacconf_pct_denominator),
                               sum,
                               na.rm = TRUE) %>%
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
                  scale_x_continuous(name="Year",
                                     breaks = c(2000, 2005, 2010, 2015, report_year-1)) +
                  scale_y_continuous(name = "Percentage bacteriologically confirmed") +
                  expand_limits(y=c(0,100)) +
                  facet_wrap( ~ entity, ncol = 4) +
                  ggtitle(paste0("Figure 4.4\nPercentage of new and relapse(a) pulmonary TB cases with bacteriological confirmation, 2000-",
                               report_year-1,
                               ", globally\nand for WHO regions.")) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
bacconf_foot <- "(a) The calculation is for new pulmonary cases in years prior to 2013 based on smear results, except for the European Region\nwhere data on confirmation by culture was also available for the period 2002-2012."

bacconf_plot <- arrangeGrob(bacconf_plot, bottom = textGrob(bacconf_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(bacconf_plot, bacconf_data, "f4_4_bacconf_plot")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.5  (map) ------
# Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, 2017
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
                        c('0-49', '50-64', '65-79', '>=80'),
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
                      "data were used for",
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
# Percentage of extrapulmonary cases among new and relapse TB cases, 2017
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
                   c('0-9.9', '10-19', '20-29', '>=30'),
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
                 "data were used for",
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
# Percentage of new and relapse TB cases with documented HIV status, 2004-2017 globally and for WHO regions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


hivstatus_data <- TBHIV_for_aggregates %>%
                  filter(year >= 2004) %>%
                  select(g_whoregion,
                         year,
                         hivtest_pct_numerator,
                         hivtest_pct_denominator) %>%
                  group_by(year, g_whoregion) %>%
                  summarise_at(vars(hivtest_pct_numerator:hivtest_pct_denominator),
                               sum,
                               na.rm = TRUE) %>%
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
                    summarise_at(vars(hivtest_pct_numerator:hivtest_pct_denominator),
                                 sum,
                                 na.rm = TRUE) %>%
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
                  scale_x_continuous(name= "Year", breaks = c(2005, 2009, 2013, report_year-1)) +
                  scale_y_continuous(name = "Percentage with documented status") +
                  expand_limits(y=c(0,100)) +
                  #scale_x_discrete(name = "Year") +
                  facet_wrap( ~ entity, ncol = 4) +
                  ggtitle(paste0("Figure 4.7\nPercentage of new and relapse(a) TB cases with documented HIV status, 2004-",
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
# Percentage of new and relapse TB cases with documented HIV status, 2017
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
                          c('0-24', '25-49', '50-74', '>=75'),
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
                        "data were used for",
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
# and estimated number of incident HIV-positive TB cases, 2004-2017
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
  summarise_at(vars(hivtest_pos:newrel_art),
               sum,
               na.rm = TRUE) %>%
  
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
  
  scale_x_continuous(name="Year",
                     breaks = c(2004,2006,2008,2010,2012,2014,2016, report_year-1)) +
  
  scale_y_continuous(name = "New and relapse cases per year (millions)") +
  xlab("Year") +
  
  ggtitle(paste0("Figure 4.9\nGlobal numbers of notified new and relapse cases(a) known to be HIV-positive (black),\nnumber started on antiretroviral therapy (blue) and estimated number of incident HIV-positive TB cases (red), 2004-",
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
# Percentage of bacteriologically confirmed TB cases tested for RR–TB, globally and for WHO regions, 2009–2017
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
              summarise_at(vars(dst_num:dst_denom),
                           sum,
                           na.rm = TRUE) %>%
              mutate(entity = "Global") %>%
              ungroup()

dst_regional <- dst_data %>%
                group_by(g_whoregion, year) %>%
                summarise_at(vars(dst_num:dst_denom),
                             sum,
                             na.rm = TRUE) %>%

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
            scale_x_continuous(breaks = c(2009, 2013, report_year-1)) +
            scale_y_continuous(name = "Percentage of cases") +
            expand_limits(y=c(0,100)) +
            xlab("Year") +

            ggtitle(paste0("Figure 4.10\nPercentage of bacteriologically confirmed TB cases tested for RR–TB, globally and for WHO regions, 2009-",
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
# Percentage of bacteriologically confirmed TB cases tested for RR-TB, 2017
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
                    c(0, 10, 40, 70, Inf),
                    c('0-9.9', '10-39', '40-69', '>=70'),
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
                        paste0("Figure 4.11\nPercentage of bacteriologically confirmed TB cases tested for RR-TB, ", report_year-1, "(a)"),
                        "Percentage",
                        copyright=FALSE,
                        colors=brewer.pal(4, "YlOrBr"),
                        show=FALSE)


# Add footnote about testing and also about using earlier data for some countries
dst_foot <- paste("(a) Among new laboratory confirmed and previously treated cases; cases with unknown previous treatment history are not included.",
                  report_year - 2,
                  "data were used for",
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
# Global number of MDR/RR-TB cases detected (pink) and number enrolled on MDR-TB treatment (green) 2009–2017,
# compared with estimate for 2016 of the number of incident cases of MDR/RR-TB (uncertainty interval shown in blue)
# and the number of MDR/RR-TB cases among notified pulmonary cases (uncertainty interval shown in black)
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
             summarise_at(vars(c_rrmdr:enrolled),
                          funs(sum(.,na.rm = TRUE)/1000)) %>%
             ungroup()

# Get the estimated incident cases
rr_global_inc <- aggregated_estimates_drtb_rawvalues %>%
                  filter(year == report_year - 1 & group_name == "global") %>%
                  select(year,
                         e_inc_rr_num,
                         e_inc_rr_num_lo,
                         e_inc_rr_num_hi,
                         e_rr_in_notified_pulm,
                         e_rr_in_notified_pulm_lo,
                         e_rr_in_notified_pulm_hi) %>%
                  mutate(e_inc_rr_num    = e_inc_rr_num    / 1000,
                         e_inc_rr_num_lo = e_inc_rr_num_lo / 1000,
                         e_inc_rr_num_hi = e_inc_rr_num_hi / 1000,
                         e_rr_in_notified_pulm = e_rr_in_notified_pulm / 1000,
                         e_rr_in_notified_pulm_lo = e_rr_in_notified_pulm_lo / 1000,
                         e_rr_in_notified_pulm_hi = e_rr_in_notified_pulm_hi / 1000)

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


            # Add estimated cases among notified pulmonary cases
            geom_errorbar(aes(ymin=e_rr_in_notified_pulm_lo,
                              ymax=e_rr_in_notified_pulm_hi),
                          width=0.2, size=1, color="black") +

            geom_point(aes(x=year, y=e_rr_in_notified_pulm),
                       shape = 21, colour = "black", fill = "white", size = 5, stroke = 2) +

            scale_x_continuous(name = "Year", breaks = c(2009, 2011, 2013, 2015, report_year-1)) +
            scale_y_continuous(name = "Number of cases (thousands)") +


            ggtitle(paste0("Figure 4.12\nGlobal number of MDR/RR-TB cases detected (pink) and number enrolled on MDR-TB treatment (green), 2009-",
                         report_year-1,
                         ",\ncompared with estimate for ",
                          report_year-1,
                         " of the number of incident cases of MDR/RR-TB (uncertainty interval shown in blue)\n",
                         "and the number of MDR/RR-TB cases among notified pulmonary cases (uncertainty interval shown in black)")) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsave(rr_plot, rr_global, "f4_12_drtb_detect_enroll_global")

# Clean up (remove any objects with their name beginning with 'rr_')
rm(list=ls(pattern = "^rr_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.13 ------
# Number of MDR/RR-TB cases detected (pink) and enrolled on MDR-TB treatment (green) 2009–2017,
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

            scale_x_continuous(name = "Year", breaks = c(2009, 2013, report_year-1)) +
            scale_y_continuous(name = "Number of cases") +


            ggtitle(paste0("Figure 4.13\nNumber of MDR/RR-TB cases detected (pink) and enrolled on MDR-TB treatment\n(green), 2009-",
                         report_year-1,
                         ", 30 high MDR-TB burden countries")) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  strip.text.x = element_text(size=7))  #reduce font size of labels above each panel


# Save the plot
figsave(rr_plot, rr_data, "f4_13_drtb_detect_enroll_hbc", width=7, height=11)

# Clean up (remove any objects with their name beginning with 'rr_')
rm(list=ls(pattern = "^rr_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.14  (map) ------
# Percentage of MDR/RR-TB cases tested for susceptibility to second-line drugs, 2017
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
                      c('0-24', '25-49', '50-74', '>=75'),
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
                    "data were used for",
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
# (green), 2000-2017, 30 high TB burden countries
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

            ggtitle(paste0("Figure 4.15\nCase notification rates (new and relapse cases, all forms) (black)\ncompared with estimated TB incidence rates (green), 2000-",
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
# in 2017, 30 high TB burden countries, WHO regions and globally
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
                       title=paste0("Figure 4.16\nEstimated TB treatment coverage (new and relapse patients as a percentage of estimated TB incidence)\n in ",
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
# Figure 4.17  (map with bubbles) ------
# The ten countries with the largest gaps between notifications of new and relapse
# (incident) TB cases and the best estimates of TB incidence, 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

gap_data <- estimates_epi_rawvalues %>%
  filter(year == report_year - 1) %>%
  select(iso3,
         year,
         e_inc_num
  ) %>%
  
  # Link to notifications
  inner_join(notification) %>%
  select(iso3,
         country,
         e_inc_num,
         c_newinc) %>%
  
  # Calculate the gap and use that for the bubble sizes
  mutate(bubble_size = e_inc_num - c_newinc) %>%
  
  # Modify long names of countries which will be shown as labels in map
  mutate(country = recode(country, "Democratic Republic of the Congo"="DR Congo","United Republic of Tanzania"="UR Tanzania")) %>%
  
  # limit to the top 10 by size of gap
  top_n(10, bubble_size)

# Plot the gaps as bubbles

gap_map <- who_bubble_map(gap_data,
                          paste0("Figure 4.17\n",
                                 "The ten countries with the largest gaps between notifications of new and relapse\n",
                                 "(incident) TB cases and the best estimates of TB incidence, ",
                                 report_year - 1,"(a)"),
                          bubble_colour = "purple",
                          scale_breaks = c(100000,500000,1000000),
                          scale_limits = c(100000,1100000),
                          scale_labels = c("100 000","500 000","1 000 000"),
                          bubble_label_show_number = 10)                        

# Generate names for footnote
gap_ten_countries_name_by_rank <- gap_data  %>%
  arrange(desc(bubble_size))   %>%
  select(country) 

gap_map <- arrangeGrob(gap_map,
                       bottom = textGrob(paste0("(a) The ten countries ranked in order of the size of the gap between notified cases and the best estimates of TB incidence in ",report_year-1," are \n",
                                                sapply(gap_ten_countries_name_by_rank,paste, collapse=","),".","\n",india_incidence_footnote,"\n",pending_incidence_footnote),
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0.4,
                                         gp = gpar(fontsize = 10)))                       

# Save the plot
figsave(gap_map,
        select(gap_data,
               iso3,
               country,
               bubble_size),
        "f4_17_top_10_gap_map")

# Clean up (remove any objects with their name beginning with 'gap')
rm(list=ls(pattern = "^gap"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.18   ------
# Number of new and relapse cases known to be HIV-positive (black)
# and number started on ART (blue) compared with estimated number of incident HIV-positive TB cases (red),
# 2004-2017, 30 high TB/HIV burden countries
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

                  ggtitle(paste0("Figure 4.18\nNumber of new and relapse cases(a) known to be HIV-positive (black) and\nnumber started on ART (blue) compared with estimated number of \nincident HIV-positive TB cases (red), 2004-",
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
# in 2017, 30 high TB/HIV burden countries, WHO regions and globally
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
                      summarise_at(vars(hiv_art_pct_numerator),
                                   sum,
                                   na.rm = TRUE) %>%
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
                      summarise_at(vars(hiv_art_pct_numerator),
                                   sum,
                                   na.rm = TRUE) %>%
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
                                     ",\n30 high TB/HIV burden countries, WHO regions and globally")) +
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
# Estimated treatment coverage for MDR/RR-TB
# (patients started on treatment for MDR-TB as a percentage of the estimated incidence of MDR/RR-TB)
# in 2017, 30 high MDR-TB burden countries, WHO regions and globally
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
                      summarise_at(vars(unconf_rrmdr_tx, conf_rrmdr_tx),
                                   sum,
                                   na.rm = TRUE) %>%
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
                      summarise_at(vars(unconf_rrmdr_tx, conf_rrmdr_tx),
                                   sum,
                                   na.rm = TRUE) %>%
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
                         title=paste0("Figure 4.20\nEstimated treatment coverage for MDR/RR-TB\n",
                                     "(patients started on treatment for MDR-TB as a percentage of the estimated incidence of MDR/RR-TB) in ",
                                     report_year - 1,
                                     ",\n30 high MDR-TB burden countries, WHO regions and globally")) +
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
# Figure 4.21  (map with bubbles) ------
# The ten countries with the largest gaps between the number of patients started
# on treatment for MDR-TB and the best estimates of MDR/RR-TB incidence, 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

drgap_data <- estimates_drtb_rawvalues %>%
              filter(year == report_year - 1) %>%
              select(iso3,
                     year,
                     e_inc_rr_num
                     ) %>%

              # Link to notifications
              inner_join(notification) %>%
              select(iso3,
                     country,
                     e_inc_rr_num,
                     unconf_rrmdr_tx,
                     conf_rrmdr_tx) %>%

              # Filter out countries that have not reported anything yet for the latest year
              filter(!is.na(unconf_rrmdr_tx) & !is.na(conf_rrmdr_tx)) %>%

              # Calculate the gap and use that for the bubble sizes
              mutate(bubble_size = e_inc_rr_num - (NZ(unconf_rrmdr_tx) + NZ(conf_rrmdr_tx))) %>%
  
              # Modify long names of countries which will be shown as labels in map
              mutate(country = recode(country, "Democratic Republic of the Congo"="DR Congo")) %>%
  
              # limit to the top 10 by size of gap
              top_n(10, bubble_size)

# Plot the gaps as bubbles

drgap_map <- who_bubble_map(drgap_data,
                            paste0("Figure 4.21\n",
                                   "The ten countries with the largest gaps between the number of patients started\n",
                                   "on treatment for MDR-TB and the best estimates of MDR/RR-TB incidence, ",
                                   report_year - 1,"(a)"),
                            bubble_colour = "green",
                            scale_breaks = c(10000,50000,100000),
                            scale_limits = c(9500,130000),
                            scale_labels = c("10 000","50 000","100 000"),
                            bubble_label_show_number = 10)

# Generate names for footnote
drgap_ten_countries_name_by_rank <- drgap_data  %>%
                                    arrange(desc(bubble_size))   %>%
                                    select(country) 

drgap_map <- arrangeGrob(drgap_map,
                       bottom = textGrob(paste0("(a) The ten countries ranked in order of the size of the gap between the number of patients started on MDR-TB treatment and the best estimate of MDR/RR-TB \nincidence in ",report_year-1," are ",
                                                sapply(gap_ten_countries_name_by_rank,paste, collapse=","),"."),
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0.4,
                                         gp = gpar(fontsize = 10)))  

# Save the plot
figsave(drgap_map,
        select(drgap_data,
               iso3,
               country,
               bubble_size),
        "f4_21_top_10_dr_gap_map")

# Clean up (remove any objects with their name beginning with 'drgap')
rm(list=ls(pattern = "^drgap"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.22   ------
# Treatment outcomes for new and relapse TB cases in 2016,
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
                summarise_at(vars(contains("newrel_")),
                             sum,
                             na.rm = TRUE) %>%
                  # merge with regional names and simplify to match structure of country table
                inner_join(who_region_names, by = "g_whoregion") %>%
                select(-g_whoregion)

# Calculate global aggregate
txout_global <- txout_country %>%
                summarise_at(vars(contains("newrel_")),
                             sum,
                             na.rm = TRUE) %>%
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

			  ggtitle(paste0("Figure 4.22\nTreatment outcomes for new and relapse TB cases in ",
			                 report_year - 2,
			                 ",\n30 high TB burden countries, WHO regions and globally")) +

			  geom_text(data=subset(txout_long,variable=="Treatment success"),aes(label = floor(value)),
			            position = position_stack(reverse = TRUE), size=3,hjust=1.5,color="white")

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
# Figure 4.23 ------
# Treatment outcomes for new and relapse TB cases,
# new and relapse cases HIV-positive TB cases, and
# MDR/RR-TB cases, 2012-2016 globally
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

out_tb_data <- outcomes %>%
                filter(between(year, 2012, report_year - 2)) %>%
                select(iso2,
                       year,
                       contains("newrel_")) %>%

                # calculate global aggregates
                group_by(year) %>%
                summarise_at(vars(newrel_coh:c_newrel_neval),
                             sum,
                             na.rm = TRUE) %>%
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
                summarise_at(vars(tbhiv_coh:c_tbhiv_neval),
                             sum,
                             na.rm = TRUE) %>%
                ungroup() %>%

                # Calculate outcome proportions for plotting as stacked bars
                calculate_outcomes_pct("tbhiv_") %>%

                # Drop the actual numbers and keep percentages
                select(-coh, -succ, -fail, -died, -lost, -c_neval) %>%

                # Add tx group type
                mutate(subgroup = "New and relapse HIV-positive TB cases")



out_mdr_data <- outcomes %>%
                filter(between(year, 2012, report_year - 2)) %>%
                select(iso2,
                       year,
                       contains("mdr_")) %>%


                # calculate global aggregates
                group_by(year) %>%
                summarise_at(vars(mdr_coh:c_mdr_neval),
                             sum,
                             na.rm = TRUE) %>%
                ungroup() %>%

                # Calculate outcome proportions for plotting as stacked bars
                calculate_outcomes_pct("mdr_") %>%

                # Drop the actual numbers and keep percentages, plu sother unwanted variables
                select(-coh, -succ, -fail, -died, -lost, -c_neval, -cur, -cmplt) %>%

                # Add tx group type
                mutate(subgroup = "MDR/RR-TB cases")

# Combine the three data frames
out_data <- rbind(out_tb_data, out_hiv_data, out_mdr_data)


# Flip into long mode for stacked bar plotting
# Rather bizerly, if I don';'t do the next line I get an error message
# See https://stackoverflow.com/a/35500964

out_data <- as.data.frame(out_data)

out_data_long <- melt(out_data, id=c(1,7))

# Determine the order of subgroup for plotting
out_data_long$subgroup <- factor(out_data_long$subgroup,
                                 levels = c("New and relapse TB cases",
                                            "New and relapse HIV-positive TB cases",
                                            "MDR/RR-TB cases"))

# Plot as stacked bars
out_plot <- out_data_long %>%
			  ggplot(aes(year,
			             value,
			             fill = variable)) +

			  geom_col(position = position_stack(reverse = TRUE)) +
			  facet_wrap( ~ subgroup, nrow = 3) +
			  coord_flip() +

			  theme_glb.rpt() +
			  scale_fill_manual("", values = outcomes_palette()) +

			  labs(x="Year started on treatment", y="Percentage of cohort (%)") +

			  theme(legend.position="bottom",
			        panel.grid=element_blank()) +

			  expand_limits(c(0,0)) +

			  ggtitle(paste0("Figure 4.23\nTreatment outcomes for new and relapse TB cases,\nnew and relapse HIV-positive TB cases,\nand MDR/RR-TB cases, 2012-",
			                 report_year - 2,
			                 " globally(a)")) +

			  geom_text(data=subset(out_data_long,variable=="Treatment success"),aes(label = floor(value)),
			            position = position_stack(reverse = TRUE), size=3,hjust=1.5,color="white")

out_plot <- arrangeGrob(out_plot,
                        bottom = textGrob("(a) MDR/RR-TB annual treatment cohorts are reported one year later than other TB cohorts.",
                                          x = 0,
                                          hjust = -0.1,
                                          vjust=0,
                                          gp = gpar(fontsize = 10)))

figsave(out_plot, out_data_long, "f4_23_outcomes_tb_hiv_mdr", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'out_')
rm(list=ls(pattern = "^out_"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.24  ------
# Treatment outcomes for new and relapse TB cases (absolute numbers), 2000 - 2016,
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
                      summarise_at(vars(new_sp_coh:c_newrel_neval),
                                   sum,
                                   na.rm = TRUE) %>%
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
                    summarise_at(vars(new_sp_coh:c_newrel_neval),
                                 sum,
                                 na.rm = TRUE) %>%
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

                  theme(legend.position="none",
                        panel.grid=element_blank()) +

                  expand_limits(c(0,0))


txoutnum_plot <- arrangeGrob(txoutnum_plot_glob,
                             txoutnum_plot_reg,
                             nrow = 2,
                             ncol = 1,
                             top = textGrob(label = paste0("Figure 4.24\nTreatment outcomes for new and relapse TB cases(a) (absolute numbers), 2000-",
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
figsave(txoutnum_plot, txoutnum_data, "f4_24_outcomes_absolute", width=7, height=11)

# Clean up (remove any objects with their name starting 'txout')
rm(list=ls(pattern = "^txoutnum"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.25   ------
# Treatment outcomes for new and relapse HIV-positive TB cases in 2016,
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
                        dplyr::rename(entity = country ) %>%
                        arrange(entity)

# Calculate regional aggregates
txtbhivout_region <- txtbhivout_country %>%
		                group_by(g_whoregion) %>%
		                summarise_at(vars(contains("tbhiv_")),
		                             sum,
		                             na.rm = TRUE) %>%
		                # merge with regional names and simplify to match structure of country table
		                inner_join(who_region_names, by = "g_whoregion") %>%
		                select(-g_whoregion)


# Calculate global aggregate
txtbhivout_global <- txtbhivout_country %>%
                      summarise_at(vars(contains("tbhiv_")),
                                   sum,
                                   na.rm = TRUE) %>%
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

					  ggtitle(paste0("Figure 4.25\nTreatment outcomes for new and relapse HIV-positive TB cases in\n",
					                 report_year - 2,
					                 ", 30 high TB/HIV burden countries, WHO regions and globally")) +

					  geom_text(data=subset(txtbhivout_long,variable=="Treatment success"),aes(label = floor(value)),
					            position = position_stack(reverse = TRUE), size=3,hjust=1.5,color="white")


# Save the plot
figsave(txtbhivout_plot, txtbhivout, "f4_25_outcomes_tbhiv", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txtbhivout')
rm(list=ls(pattern = "^txtbhivout"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.26   ------
# Treatment outcomes for rifampicin-resistant TB cases started on treatment in 2015,
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
					  dplyr::rename(entity = country ) %>%
					  arrange(entity)

# Calculate regional aggregates
txmdrout_region <- txmdrout_country %>%
					  group_by(g_whoregion) %>%
					  summarise_at(vars(contains("mdr_")),
					               sum,
					               na.rm = TRUE) %>%
					  # merge with regional names and simplify to match structure of country table
					  inner_join(who_region_names, by = "g_whoregion") %>%
					  select(-g_whoregion)


# Calculate global aggregate
txmdrout_global <- txmdrout_country %>%
					  summarise_at(vars(contains("mdr_")),
					               sum,
					               na.rm = TRUE) %>%
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

				  ggtitle(paste0("Figure 4.26\nTreatment outcomes for rifampicin-resistant TB cases\nstarted on treatment in ",
				                 report_year - 3,
				                 ",\n30 high MDR-TB burden countries, WHO regions and globally")) +

				  geom_text(data=subset(txmdrout_long,variable=="Treatment success"),aes(label = floor(value)),
				            position = position_stack(reverse = TRUE), size=3,hjust=1.5,color="white")

figsave(txmdrout_plot, txmdrout, "f4_26_outcomes_mdr", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txmdrout')
rm(list=ls(pattern = "^txmdrout"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.27  (map) ------
# Countries that had used shorter MDR–TB treatment regimens by the end of 2017
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
                         paste0("Figure 4.27\nCountries that had used shorter MDR–TB treatment regimens by the end of ", report_year - 1),
                         legend.title = "Country\nresponse",
                         copyright=FALSE,
                         brewer.pal(3, "Greens"),
                         na.label="No response",
                         show=FALSE)

figsave(short_map,
        short_data,
        "f4_27_short_regimen_map")


# Clean up (remove any objects with their name beginning with 'short')
rm(list=ls(pattern = "^short"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.28  (map) ------
# Countries that had used bedaquiline for the treatment of M/XDR–TB as part of expanded access,
# compassionate use or under normal programmatic conditions by the end of 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bdq_data <- notification %>%
            filter(year == report_year - 1) %>%
            select(country,
                   iso3,
                   mdrxdr_bdq_used) %>%
            mutate(cat = ifelse(mdrxdr_bdq_used == 1, "Bedaquiline used",
                                ifelse(mdrxdr_bdq_used == 0, "Bedaquiline not used",
                                       ifelse(mdrxdr_bdq_used == 3, "Don't know", NA)))) %>%

            # drop unnecessary variables
            select(country, iso3, cat)


bdq_data$cat <- factor(bdq_data$cat)

# produce the map
bdq_map<- WHOmap.print(bdq_data,
                       paste0("Figure 4.28\nCountries that had used bedaquiline for the treatment of M/XDR–TB as part of expanded access,\ncompassionate use or under normal programmatic conditions by the end of ", report_year - 1),
                       legend.title = "",
                       copyright=FALSE,
                       colors=c("lightgreen", "darkgreen", "lightyellow"),
                       na.label="No response",
                       show=FALSE)

figsave(bdq_map,
        bdq_data,
        "f4_28_bdq_map")


# Clean up (remove any objects with their name beginning with 'bdq')
rm(list=ls(pattern = "^bdq"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.29  (map) ------
# Countries that had used delamanid for the treatment of M/XDR–TB as part of expanded access,
# compassionate use or under normal programmatic conditions by the end of 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


dlm_data <- notification %>%
            filter(year == report_year - 1) %>%
            select(country,
                   iso3,
                   mdrxdr_dlm_used) %>%
            mutate(cat = ifelse(mdrxdr_dlm_used == 1, "Delamanid used",
                                ifelse(mdrxdr_dlm_used == 0, "Delamanid not used",
                                       ifelse(mdrxdr_dlm_used == 3, "Don't know", NA)))) %>%

            # drop unnecessary variables
            select(country, iso3, cat)

dlm_data$cat <- factor(dlm_data$cat)

# produce the map
dlm_map<- WHOmap.print(dlm_data,
                       paste0("Figure 4.29\nCountries that had used delamanid for the treatment of M/XDR–TB as part of expanded access,\ncompassionate use or under normal programmatic conditions by the end of ", report_year - 1),
                       legend.title = "",
                       copyright=FALSE,
                       colors=c("lightgreen", "darkgreen", "lightyellow"),
                       na.label="No response",
                       show=FALSE)


figsave(dlm_map,
        dlm_data,
        "f4_29_dlm_map")


# Clean up (remove any objects with their name beginning with 'dlm')
rm(list=ls(pattern = "^dlm"))






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 4.4.1  (Map) ------
# Percentage of basic management units in which there is community contribution to new case finding
# and/or to treatment adherence support, 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get the variable for requesting countries for those commutnity indicator or not.
comm_datarequest <- data_collection %>%
  filter(datcol_year==report_year) %>%
  select(country,
         dc_engage_community_display)

comm_bmu <- strategy %>%
			  filter(year==report_year - 1) %>%
			  select(iso3,
			         country,
			         bmu,
			         bmu_community_impl,
			         community_data_available)%>%
			  mutate(comm_pct = ifelse(is.na(bmu_community_impl) | NZ(bmu) == 0,
			                           NA,
			                           bmu_community_impl * 100 / bmu))

# Define countries which has not been requested for data as their persentage=-1, just to creat a new catogory
comm_data <- merge(comm_datarequest, comm_bmu, by= "country")%>%
  mutate(comm_pct=replace(comm_pct,(dc_engage_community_display == 0),-1))


comm_data$cat <- cut(comm_data$comm_pct,
                     c(-1,0, 25, 50, 75, Inf),
                     c('Data not requested','0-24', '25-49', '50-74', '>=75'),
                     right=FALSE)

# produce the map
comm_map <- WHOmap.print(comm_data,
                         paste("Figure Box 4.4.1\nPercentage of basic management units in which there is community contribution",
                               "\nto new case finding and/or to treatment adherence support,",
                               report_year-1),
                         "Percentage",
                         copyright=FALSE,
                         #colors=c('lightgreen','greyblue', 'lightblue', 'Blue', 'darkblue'),
                         colors=c('#edf8e9','#bdd7e7', '#6baed6', '#3182bd', '#08519c'),
                         show=FALSE)
comm_map <- arrangeGrob(comm_map,
                        bottom = textGrob("Data only requested from 111 countries.",
                                          x = 0,
                                          hjust = -0.1,
                                          vjust=0,
                                          gp = gpar(fontsize = 10)))

figsave(comm_map,
        select(comm_data,
               country,
               iso3,
               comm_pct,
               cat),
        "f4_box_4_4_1_pct_BMU_community_map")



# Clean up (remove any objects with their name beginning with 'comm')
rm(list=ls(pattern = "^comm"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 4.4.2 ------
# Number of countries reporting on WHO community engagement indicators, 2013-2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Before 2015, the report collection form did not include variable "community_data_available"
#Thus define the variable before 2015 as having reported either "bmu_ref_data" or "bmu_rxsupport_data" or both.
commureport_from2013 <- strategy %>%
  filter(year >= 2013) %>%
  select(year,
         g_whoregion,
         country,
         bmu_ref_data,
         bmu_rxsupport_data,
         community_data_available,
         notified_ref,
         notified_ref_community,
         rxsupport_community_coh,
         rxsupport_community_succ)%>%
  mutate(community_data_available2=ifelse((notified_ref>0 & notified_ref_community>=0) | (rxsupport_community_coh>0 & rxsupport_community_succ>=0),1,NA))

commureport_sumsince2013 <- commureport_from2013 %>%
  filter(year >= 2013 & year <= report_year) %>%
  select(year,
         g_whoregion,
         community_data_available2)%>%
  group_by(year) %>%
  summarise_at(vars(community_data_available2),
               sum,
               na.rm = TRUE) %>%
  mutate(entity = "Global") %>%
  ungroup()

# Also Lana said numbers has been collected in 2013(data of 2012) by herself through Email so not in the database,
# Thus we need to creat a new row for year 2012 = 13 countries manully.
commureport_2012 <- data.frame(year=2012, entity="Global", community_data_available2=13)

commureport_global <-rbind(commureport_2012,commureport_sumsince2013)
# After Lana comes back, I will ask her for list of those 13 countries and introduc them into the data frame.

commureport_plot_glob <- commureport_global %>%
  ggplot(aes(x=year, y=community_data_available2)) +
  geom_bar(stat="identity", fill="darkgreen",width = 0.5) +
  #This breaks setting need to be changed afterwards, it is simply because only 6 years data were used
  #which makes the bar too wide, to control the width, it changed X axis breaks label.
  scale_x_continuous(breaks = c(2012,2013, 2014, 2015, 2016, report_year-1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 70)) +
  theme_glb.rpt() +
  labs(x="", y="Number of countries") +
  theme(panel.grid=element_blank()) +
  expand_limits(c(0,0)) +
  ggtitle(paste0("Figure 4.4.2\nNumber of countries reporting on WHO community engagement indicators, 2012-",
                 report_year-1))

# Save the plot
figsave(commureport_plot_glob, commureport_global, "f4_box_4_4_2_community_indicator_reporting", width=7, height=11)

# Clean up (remove any objects with their name starting 'commureport')
rm(list=ls(pattern = "^commureport"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# TB prevention services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.1 (Map) -------
# Coverage of TB preventive treatment among eligible children aged under 5 years, 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Need LTBI views which were not in the RData files, so import them now and add to RData later
options(stringsAsFactors=FALSE)

.fixnamibia <- function(df){
  # make sure Namibia's iso2 code is not interpreted as R's NA (null)
  df$iso2 <- ifelse(df$country=="Namibia", "NA", as.character(df$iso2))
  return(df)
}

library(RODBC)
ch <- odbcDriverConnect(connection_string)

# load views into dataframes
estimates_ltbi   <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_ltbi"))

close(ch)


prevtx_kids_data <-  estimates_ltbi %>%
              filter(year==report_year - 1) %>%
              select(iso3,
                     country,
                     e_prevtx_kids_pct)

# We have a list of exclusions based on feedback from countries compiled by Lele
prev_tx_footnote <- paste0("Estimated coverage was not calculated because the numerator includes contacts aged 5-7 years (DPR Korea), those aged 5-6 years (Nigeria),",
                           "\nis predominantly contacts of household contacts (Indonesia), and includes household contacts of bacteriologically confirmed or clinically diagnosed",
                           "\nTB cases (Malawi and Phillipines).")

prev_tx_footnote_countries = c("PRK", "NGA", "IDN", "MWI", "PHL")

# Remove the coverage calculations for the excluded countries
prevtx_kids_data <- prevtx_kids_data %>%
                    mutate(e_prevtx_kids_pct = ifelse(iso3 %in% prev_tx_footnote_countries,
                                                      NA,
                                                      e_prevtx_kids_pct))

# Assign categories
prevtx_kids_data$cat <- cut(prevtx_kids_data$e_prevtx_kids_pct,
                            c(0, 25, 50, 75, Inf),
                            c('0-24', '25-49', '50-74', '>=75'),
                            right = FALSE)


# produce the map
prevtx_kids_map <- WHOmap.print(prevtx_kids_data,
                                paste("Figure 5.1\nCoverage of TB preventive treatment among eligible children aged under 5 years(a), ",
                                      report_year-1),
                                legend.title = "Coverage (%)",
                                copyright=FALSE,
                                colors=brewer.pal(4, "Blues"),
                                na.label="Not estimated",
                                show=FALSE)

# add footnote
prevtx_kids_map <- arrangeGrob(prevtx_kids_map, bottom = textGrob(paste0("(a) Children aged <5 years who were household contacts of bacteriologically confirmed pulmonary TB cases.",
                                                                         "\n",
                                                                         prev_tx_footnote),
                                                                  x = 0,
                                                                  hjust = -0.1,
                                                                  vjust=0.1,
                                                                  gp = gpar(fontsize = 10)))

figsave(prevtx_kids_map,
        prevtx_kids_data,
        "f5_1_prevtx_kids_map")


# Clean up (remove any objects with their name beginning with 'prevtx_kids')
rm(list=ls(pattern = "^prevtx_kids"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.2 ---------
# Provision of TB preventive treatment to people living with HIV, 2015–2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ipt <- notification %>%
  filter(year >= 2005) %>%
  select(iso3,
         country,
         year,
         g_whoregion,
         hiv_ipt,
         hiv_ipt_reg_all) %>%  
  mutate(hiv_ipt=ifelse(!is.na(hiv_ipt),hiv_ipt,hiv_ipt_reg_all))

#remove data from Kenya, Zambia and Eritrea in 2016 and add data from Russia"
#   ipt[ipt$iso3 %in% c("ZMB", "KEN", "ERI")&ipt$year==2016,"hiv_ipt"]<-NA
#   ipt[ipt$iso3=="RUS"&ipt$year==2016,"hiv_ipt"]<-19611

#checked with Annabele, last year those coUntries were removed because of data discrepancies(also removed from sever database)
#might need to do the same thing this year later.
#Also, she confirmed we can keep data that way in 2016 since those countries still could not provide clean data for 2016


ipt$area <- ifelse(ipt$iso3 %in% c("ZAF"), "South Africa", 
                   ifelse(ipt$g_whoregion=="AFR", "Rest of AFR", 
                          "Rest of world"))
ipt_b <- aggregate(ipt[5], by=list(year=ipt$year, area=ipt$area), sum, na.rm=T)
ipt_b1 <- aggregate(ipt[5], by=list(year=ipt$year), FUN=sum, na.rm=T) 
ipt_b1$area <- "Global" 
ipt_b2 <- rbind(ipt_b, ipt_b1)
library(reshape2)
ipt_c <- melt(ipt_b2, id=1:2)
ipt_c$value <- ipt_c$value/1000 
ipt_c$area <- factor(ipt_c$area, levels=c( "Rest of world", "Rest of AFR", "South Africa", "Global")) 


hiv_ipt_plot <- ggplot(ipt_c, aes(year, value, color=area)) + geom_line(size=1)+ 
  scale_y_continuous("Number of people living with HIV (thousands)") + 
  theme_glb.rpt() + scale_x_continuous(name="", breaks=c(min(ipt_c$year):max(ipt_c$year))) + 
  scale_color_manual(values=c("#0070C0", "#77933C","orange", "red")) + guides(color = guide_legend(reverse = TRUE))+
  ggtitle(paste("Figure 5.2\nProvision of TB preventive treatment to people living with HIV, 2005", report_year-1, sep="\u2013"))+
  annotate("text", x=2016, y=900, label="Global", size=4)+
  annotate("text", x=2016, y=350, label="South Africa", size=4)+
  annotate("text", x=2016, y=520, label="Rest of Africa", size=4)+
  annotate("text", x=2016, y=120, label="Rest of World", size=4)+
  theme(legend.position="none")

# Save the plot
figsave(hiv_ipt_plot, ipt_c, "f5_2_IPT_in_HIV_patients", width=7, height=5)

# Clean up (remove any objects with their name starting 'commureport')
rm(list=ls(pattern = "^ipt"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.3   ------
# Gaps in TB prevention and TB detection for people 
# who were newly enrolled in HIV care in 2017, selected countries


# Annabele confirmed only when country reported same number for hiv_reg_new and hiv_reg_new2 should this country be selected,
# othervise it would mean they use different denominator for reporting hiv_ipt and hiv_tbdetect which is unacceptable for this plot

ipt_gap_country  <- notification %>%
  filter(year==report_year - 1 & hiv_tbdetect>=0 & hiv_ipt>=0 & (hiv_reg_new>0 | hiv_reg_new2>0) & hiv_reg_new == hiv_reg_new2) %>%
  select(country,
         iso2,
         g_whoregion,
         hiv_reg_new,
         hiv_tbdetect,
         hiv_ipt)  %>%
  # shorten long country names
  get_names_for_tables() 

# Filter the country list down to high TB/TBHIV burden ones
ipt_gap_tbhivhbc <- country_group_membership %>%
  filter(group_type == "g_hb_tb" | group_type == "g_hb_tbhiv") %>%
  select(iso2) %>%
  unique()

ipt_gap_country <- ipt_gap_country %>%
  inner_join(ipt_gap_tbhivhbc) %>%
  #remove the iso2 field to match regional and countries aggregates
  select(-iso2,
         -g_whoregion)%>%
  #Zimbabwe reported an unreasonal high hiv_tbdect persentage, remove it after Annabel checked with them
  filter(country!="Zimbabwe")

# Calculate proportions for plotting as stacked bars
ipt_gap_country <- ipt_gap_country %>%
  mutate(ipt_pct = hiv_ipt * 100 / hiv_reg_new,
         tbdetect_pct = hiv_tbdetect * 100 / hiv_reg_new,
         gap_pct = (hiv_reg_new - hiv_ipt - hiv_tbdetect) * 100 / hiv_reg_new) %>%
  # Sort in descending order of started on IPT persentage
  arrange(desc(ipt_pct)) %>%
  # Keep record of current order (in reverse) so plot comes out as we want it
  mutate(country = factor(country, levels=rev(country))) %>%
  # Drop the actual numbers and keep percentages
  select(-hiv_reg_new,-hiv_tbdetect,-hiv_ipt)

# Flip into long mode for stacked bar plotting
ipt_gap_long <- melt(ipt_gap_country, id=1)


# stacking order changed with upgrade of ggplot to version 2.2. GRRRRRRRR
# Why GRRRRRR? Because, of course, this broke existing code that was working!
# Finally figured out the solution -- use geom_col with the following parameter
# geom_col(position = position_stack(reverse = TRUE))
#
# It also helped to have a named list for the colour palette.
# See http://ggplot2.tidyverse.org/reference/geom_bar.html  and
#     http://ggplot2.tidyverse.org/reference/scale_manual.html

# Plot as stacked bars

ipt_gap_plot <- ipt_gap_long %>%
  ggplot(aes(country,
             value,
             fill = variable)) +
  
  geom_col(position = position_stack(reverse = TRUE),width = 0.5) +
  
  theme_glb.rpt() +
  scale_fill_manual("", values = c("lightblue","orange",  "lightgreen"),
                    labels=c("Started on preventive treatment", "Detected and notified with active TB disease", "Gap in TB detection and TB prevention(b)")) +
  labs(x="", y="Percentage(%)") +
  
  theme(legend.position="bottom",
        #Set font for country names to avoid overlapping especialy because this year we got long names as PNG 
        axis.text=element_text(size=7),
        #Set margins to give enough space for those long long footnotes.
        legend.box.margin=margin(-20,-20,-20,-20),
        plot.margin = margin(2, 10, 80, 10),
        panel.grid=element_blank()) +
  
  expand_limits(c(0,0)) +
  
  ggtitle(paste0("Figure 5.3\nGaps in TB prevention and TB detection for people who were newly enrolled in HIV care in ",
                 report_year - 1,
                 ",selected countries(a)"))

# Add explanatory footnotes
ipt_gap_footnote <- "(a) The selected countries are high TB or TB/HIV burden countries that reported on all three of the following: the number of people newly enrolled on HIV care; the number of \n      TB cases detected among people newly enrolled on HIV care; and the number of people newly enrolled on HIV care who were started on TB preventive treatment. In high \n      TB burden countries, testing for LTBI is not a requirement for initiation of TB preventive treatment, such that all those without active TB disease are eligible for TB preventive \n                              treatment.\n     (b) The gap represents people living with HIV who should have undergone complete evaluation for TB disease or TB preventive treatment."
ipt_gap_plot <- arrangeGrob(ipt_gap_plot,
                            bottom = textGrob(ipt_gap_footnote,
                                              x = 0,
                                              hjust = -0.1,
                                              vjust=-0.7,
                                              gp = gpar(fontsize = 8)))

figsave(ipt_gap_plot, ipt_gap_country, "f5_3_gaps_for_ipt_in_new_hiv_patients", width=11, height=7) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'ipt_gap')
rm(list=ls(pattern = "^ipt_gap"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.4 (Map) ---------
# Notification rate ratio of arrangeGrob()TB among healthcare workers compared with the general population, 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

hcw_notif_hcw <-  strategy %>%
                  filter(year == report_year - 1) %>%
                  select(iso3,
                         hcw_tb_infected,
                         hcw_tot)

# Get the total adult population aged between 15 and 64 (exclude those aged 65 and above)
hcw_pop_adults <- estimates_population %>%
                  filter(year == report_year - 1) %>%

                  mutate(e_pop_adult = NZ(e_pop_m1524) + NZ(e_pop_m2534) + NZ(e_pop_m3544) + NZ(e_pop_m4554) + NZ(e_pop_m5564) +
                                       NZ(e_pop_f1524) + NZ(e_pop_f2534) + NZ(e_pop_f3544) + NZ(e_pop_f4554) + NZ(e_pop_f5564) ) %>%

                  select(iso3,
                         e_pop_adult)

# Get the total notifications among adults aged between 15 and 64 (exclude those aged 65 and above)
hcw_notif_adults <- notification %>%
                    filter(year == report_year - 1) %>%
                    mutate(notif_adult = NZ(newrel_m1524) + NZ(newrel_m2534) + NZ(newrel_m3544) + NZ(newrel_m4554) + NZ(newrel_m5564) +
                                         NZ(newrel_f1524) + NZ(newrel_f2534) + NZ(newrel_f3544) + NZ(newrel_f4554) + NZ(newrel_f5564)) %>%
                    select(iso3,
                           notif_adult) %>%

                    inner_join(hcw_pop_adults)


hcw_data <-  hcw_notif_adults %>%
                    inner_join(hcw_notif_hcw) %>%

                    # Calculate notification rate ratio
                    # Use as.numeric() to avoid integer overflow
                    mutate(nrr = ifelse(NZ(hcw_tot) > 0 &
                                        NZ(notif_adult) > 0,
                                        (as.numeric(hcw_tb_infected) * as.numeric(e_pop_adult))
                                        /
                                        (as.numeric(hcw_tot) * as.numeric(notif_adult)),
                                        NA))

# filter out counrties where the number of HCWs in a country is too low
# for now, removing any countries reporting less than 1000 HCw because that would
# make the notification rate jump around too much
hcw_filtered_out <- hcw_data %>% filter(hcw_tot < 1000) %>% nrow()

hcw_data <- hcw_data %>%
            mutate(nrr = ifelse(hcw_tot < 1000, NA, nrr)) %>%

                      select(iso3,
                           nrr)


hcw_data$cat <- cut(hcw_data$nrr,
                    c(0, 1, 2, 3, Inf),
                    c('0-0.9', '1-1.9', '2-2.9', '>=3'),
                    right=FALSE)


# produce the map
hcw_map <- WHOmap.print(hcw_data,
                        paste("Figure 5.4\nNotification rate ratio of TB among healthcare workers\ncompared with the general adult population,", report_year-1),
                        "Notification\nrate ratio",
                        copyright=FALSE,
                        colors=c('lightblue', 'orange', 'red', 'darkred'),
                        #colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                        show=FALSE)


# Add footnote about filtering out countries
hcw_foot <- paste("Data from ",
                  hcw_filtered_out,
                  " countries were excluded where the number of health care workers reported was less than 1 000.")

hcw_map <- arrangeGrob(hcw_map, bottom = textGrob(hcw_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))

figsave(hcw_map,
        hcw_data,
        "f5_4_hcw_notf_rate_ratio")

# Clean up (remove any objects with their name beginning with 'hcw')
rm(list=ls(pattern = "^hcw"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.5 (Map) ---------
# BCG vaccination policy by country
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bcg_policy_data <- read.csv(paste0(rdata_folder, "/BCG_Atlas_Data _2018-07-19.csv"),
                            na.strings = "")

# filter out empty lines and restrict to the variables we need
bcg_policy_data <- bcg_policy_data %>%
                   select(iso3, Country, BCG.Policy.Type) %>%
                   filter(!is.na(iso3))

bcg_policy_data$cat <- as.factor(bcg_policy_data$BCG.Policy.Type)

bcg_policy_map <- WHOmap.print(bcg_policy_data,
                               "Figure 5.5\nBCG vaccination policy by country",
                               "Policy",
                               copyright=FALSE,
                               colors=c("darkgreen", "lightgreen", "purple"),
                               show=FALSE)

# Add footnote
bcg_policy_map <- arrangeGrob(bcg_policy_map,
                             bottom = textGrob("\nSource: The BCG World Atlas 2nd Edition, http://www.bcgatlas.org/, accessed 19 July 2018",
                                               x = 0,
                                               hjust = -0.1,
                                               vjust=0,
                                               gp = gpar(fontsize = 10)))

figsave(bcg_policy_map,
        bcg_policy_data,
        "f5_5_BCG_policy_map")

# Clean up (remove any objects with their name beginning with 'bcg_policy')
rm(list=ls(pattern = "^bcg_policy"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.6 (Map) ---------
# Coverage of BCG vaccination, 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Download the latest data from WHO immunization department
bcg_cov_source <-"http://www.who.int/entity/immunization/monitoring_surveillance/data/coverage_series.xls"
bcg_cov_page <- "http://apps.who.int/immunization_monitoring/globalsummary/timeseries/tscoveragebcg.html"

download.file(bcg_cov_source,
              paste0(rdata_folder, "coverage_series", Sys.Date(), ".xls"),
              mode="wb")

# Read the BCG coverage sheet for the latest year
bcg_cov_data <- read_xls(path = paste0(rdata_folder, "coverage_series", Sys.Date(), ".xls"),
                         sheet = "BCG",
                         range = "B1:E178",
                         col_names = TRUE
                         )

# Merge with list of all countries and remove unwanted variables
bcg_cov_data <- report_country %>%
                select(country, iso3) %>%
                left_join(bcg_cov_data, by = c("iso3" = "ISO_code")) %>%
                select(country,
                       iso3,
                       coverage = as.character(report_year - 1))


bcg_cov_data$cat <- cut(bcg_cov_data$coverage,
                        c(0, 50, 90, 100),
                        c('0-49', "50-89", "90-100"),
                        right=FALSE)


# produce the map
bcg_cov_map <- WHOmap.print(bcg_cov_data,
                            paste("Figure 5.6\nCoverage of BCG vaccination,", report_year-1),
                            "Percentage",
                            copyright=FALSE,
                            colors=brewer.pal(3, "Greens"),
                            show=FALSE)

# Add footnote
bcg_cov_map <- arrangeGrob(bcg_cov_map,
                           bottom = textGrob(paste0("The target population of BCG coverage varies depending on national policy, but is typically for the number of live births in the year of reporting.",
                                                    "\nSource: ", bcg_cov_page, ", accessed ", Sys.Date()),
                                             x = 0,
                                             hjust = -0.1,
                                             vjust=0,
                                             gp = gpar(fontsize = 10)))


figsave(bcg_cov_map,
        bcg_cov_data,
        "f5_6_BCG_coverage_map")

# Clean up (remove any objects with their name beginning with 'bcg_cov')
rm(list=ls(pattern = "^bcg_cov"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 6 ------
# Financing for TB prevention, diagnosis and treatment
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 6.3.1 (Map) ---------
# Cost per patient treated for drug-susceptible TB or MDR-TB: current availability
# of unit cost data from independent costing studies, 1990-2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# get data from CSV file obtained by Ines
ghcc_data <- read.csv(paste0(rdata_folder,"/GHCC_box_map_data_2018-07-12.csv"))

# merge the data with the master list of countries
ghcc_data <- report_country %>%
             select(country, iso3) %>%
             left_join(ghcc_data, by = "iso3")

ghcc_data$cat <- cut(ghcc_data$N,
                     c(0, 2, 10, 30, Inf),
                     c("1", "2-9", "10-29", ">=30"),
                     right=FALSE)


ghcc_map <- WHOmap.print(ghcc_data,
                         "Fig B6.3.1\nCost per patient treated for drug-susceptible TB or MDR-TB:\ncurrent availability of unit cost data from independent costing studies, 1990-2017",
                         "Number\navailable",
                         copyright=FALSE,
                         colors=brewer.pal(4, "Reds"),
                         show=FALSE)

# Add footnote for the source of the data
ghcc_map <- arrangeGrob(ghcc_map,
                        bottom = textGrob("Source: Unit Cost Study Repository, https://ghcosting.org/pages/data/ucsr/app/index, accessed 19 July 2018",
                                          x = 0,
                                          hjust = -0.1,
                                          vjust=0,
                                          gp = gpar(fontsize = 10)))

figsave(ghcc_map,
        ghcc_data,
        "f6_box_6_3_1_unit_cost_data_map")

# Clean up (remove any objects with their name beginning with 'ghcc')
rm(list=ls(pattern = "^ghcc"))

