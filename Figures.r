# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figures in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Hazim Timimi, July 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Diagnosis and treatment of TB, HIV-associated TB and drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1  ------
# New and relapse TB case notification rates by age group and sex,  all WHO regions and global, 2015
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
                scale_y_continuous(name = "TB case notification rate per 100 000 population per year",
                                   #Set labels manually to avoid showing negative numbers!
                                   breaks=c(-200,-100,0,100,200,300),
                                   labels=abs(c(-200,-100,0,100,200,300))) +
                scale_x_discrete("Age group (years)",
                                 labels=levels(agesex_agg_long$agegroup)) +
                coord_flip() +
                facet_wrap( ~ entity, ncol = 4) +
                ggtitle(paste0("Figure 4.1 New and relapse TB case notification rates by age group and sex, globally and for all WHO regions, ",
                               report_year-1,
                               "(a)")) +
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
figsave(agesex_plot, agesex_agg_long, "f4_1_agesex")

# Clean up (remove any objects with their name beginning with 'agesex')
rm(list=ls(pattern = "^agesex"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.4  ------
# Percentage of new and relapse pulmonary TB cases with bacteriological confirmation,  all WHO regions and global, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


bacconf_data <- notification %>%
                filter(year >= 2009) %>%
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
                  #scale_x_discrete(name = "Year") +
                  facet_wrap( ~ entity) +
                  ggtitle(paste0("Figure 4.4 Percentage of new and relapse(a) pulmonary TB cases with bacteriological confirmation,\nglobally and for all WHO regions, 2009 - ",
                               report_year-1)) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
bacconf_foot <- "(a) The calculation is for new pulmonary cases only in years prior to 2013 based on smear results only, except for the European Region\nwhere data on confirmation by culture was also available."

bacconf_plot <- arrangeGrob(bacconf_plot, bottom = textGrob(bacconf_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(bacconf_plot, bacconf_data, "f4_4_bacconf_plot")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.6  ------
# Percentage of new and relapse TB cases with documented HIV status,  all WHO regions and global, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


hivstatus_data <- TBHIV_for_aggregates %>%
                  filter(year >= 2009) %>%
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
                    filter(year >= 2009) %>%
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



# Plot as lines
hivstatus_plot <- hivstatus_data %>%
                  ggplot(aes(x=year, y=hivstatus_pct)) +
                  geom_line(size=1) +
                  scale_y_continuous(name = "% with documented status") +
                  expand_limits(y=c(0,100)) +
                  xlab("Year") +
                  #scale_x_discrete(name = "Year") +
                  facet_wrap( ~ entity) +
                  ggtitle(paste0("Figure 4.6 Percentage of new and relapse(a) TB cases with documented HIV status, globally and for all WHO regions, 2009 - ",
                               report_year-1)) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
hivstatus_foot <- "(a) The calculation is for all cases in years prior to 2015."

hivstatus_plot <- arrangeGrob(hivstatus_plot, bottom = textGrob(hivstatus_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(hivstatus_plot, hivstatus_data, "f4_6_hivstatus_plot")

# Clean up (remove any objects with their name beginning with 'hivstatus')
rm(list=ls(pattern = "^hivstatus"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.10i   ------
# Number of new and relapse cases notified and estimated number of TB incidence cases, global, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

newinc_data <- notification %>%
                filter(year >= 2009) %>%
                select(year, c_newinc) %>%
                group_by(year) %>%
                summarise_each(funs(sum(.,na.rm = TRUE)),
                               c_newinc) %>%
                mutate(c_newinc = c_newinc / 1e6)

inc_data <- aggregated_estimates_epi_rawvalues %>%
            filter( year >= 2009 & group_name == 'global') %>%
            select(year,
                   e_inc_num,
                   e_inc_num_lo,
                   e_inc_num_hi) %>%
            mutate(e_inc_num = e_inc_num / 1e6,
                   e_inc_num_lo = e_inc_num_lo / 1e6,
                   e_inc_num_hi = e_inc_num_hi / 1e6) %>%
            # Use a right-join so can see the data for the final year even in the absence of estimates
            right_join(newinc_data)

# Plot as lines
inc_plot <- inc_data %>%
            ggplot(aes(x=year, y=c_newinc, ymin=0)) +
            geom_line(size=1) +
            geom_ribbon(aes(x=year, ymin=e_inc_num_lo, ymax=e_inc_num_hi),
                        fill=I('#00FF33'),
                        alpha=0.4) +
            geom_line(aes(year, e_inc_num),
                      size=1,
                      colour=I('#00FF33')) +

            scale_y_continuous(name = "New and relapse cases per year (millions)") +
            xlab("Year") +

            ggtitle(paste0("Figure 4.10.i Number of new and relapse cases notified (black) and estimated number of TB incidence cases (green), global, 2009 - ",
                         report_year-1,
                         ".\nShaded areas represent uncertainty bands.")) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsave(inc_plot, inc_data, "f4_10i_inc_plot")

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.10ii   ------
# Number of new and relapse cases  known to be HIV-positive,
# number started on ART and estimated number of incident HIV-positive TB cases, global, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tbhiv_data <- TBHIV_for_aggregates %>%
              filter(year >= 2009) %>%
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
                  filter( year >= 2009 & group_name == 'global') %>%
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
                              fill="red",
                              alpha=0.4) +
                  geom_line(aes(year, e_inc_tbhiv_num),
                            size=1,
                            colour="red") +

                  geom_line(aes(year, hiv_art),
                            size=1,
                            colour="blue") +

                  scale_y_continuous(name = "New and relapse cases per year (millions)") +
                  xlab("Year") +

                  ggtitle(paste0("Figure 4.10.ii Number of new and relapse cases(a) known to be HIV-positive (black),\nnumber started on ART (blue) and estimated number of incident HIV-positive TB cases (red), global, 2009 - ",
                               report_year-1,
                         ".\nShaded areas represent uncertainty bands.")) +

                  theme_glb.rpt()


# Add footnote
inctbhiv_foot <- "(a) The calculation is for all cases in years prior to 2015."

inctbhiv_plot <- arrangeGrob(inctbhiv_plot, bottom = textGrob(inctbhiv_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(inctbhiv_plot, inctbhiv_data, "f4_10ii_inctbhiv_plot")

# Clean up (remove any objects with their name containing 'tbhiv')
rm(list=ls(pattern = "tbhiv"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.11   ------
# Number of new and relapse cases notified compared with estimated number of incident TB cases,
# 30 high TB burden countries, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

newinc_30hbc <- report_country %>%
                filter(g_hb_tb==1) %>%
                select(iso2)

newinc_data <- notification %>%
                filter(year >= 2009) %>%
                inner_join(newinc_30hbc) %>%
                select(year,
                       iso2,
                       country,
                       c_newinc) %>%
                mutate(c_newinc = c_newinc / 1e3)

inc_data <- estimates_epi_rawvalues %>%
            filter(year >= 2009) %>%
            inner_join(newinc_30hbc) %>%
            select(year,
                   iso2,
                   e_inc_num,
                   e_inc_num_lo,
                   e_inc_num_hi) %>%
            mutate(e_inc_num = e_inc_num / 1e3,
                   e_inc_num_lo = e_inc_num_lo / 1e3,
                   e_inc_num_hi = e_inc_num_hi / 1e3) %>%

            # Use a right-join so can see the data for the final year even in the absence of estimates
            right_join(newinc_data) %>%

            # shorten long country names
            .shortnames( col = "country")

# Plot as lines
inc_plot <- inc_data %>%
            ggplot(aes(x=year, y=c_newinc, ymin=0)) +
            geom_line(size=1) +
            geom_ribbon(aes(x=year, ymin=e_inc_num_lo, ymax=e_inc_num_hi),
                        fill=I('#00FF33'),
                        alpha=0.4) +
            geom_line(aes(year, e_inc_num),
                      size=1,
                      colour=I('#00FF33')) +

            scale_y_continuous(name = "New and relapse cases per year (thousands)") +
            xlab("Year") +

            facet_wrap( ~ country,
                        scales = "free_y",
                        ncol = 5) +

            ggtitle(paste0("Figure 4.11 Number of new and relapse cases notified (black) compared with estimated number of incident TB cases (green),\n30 high TB burden countries, 2009 - ",
                         report_year-1)) +
            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsave(inc_plot, inc_data, "f4_11_inc_plot_hbc")

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.12   ------
# Estimated number of missed cases in top-ten countries, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

missing_inc <- estimates_epi_rawvalues %>%
               filter(year == report_year - 1) %>%
               select(country,
                      iso2,
                      e_inc_num,
                      e_inc_num_lo,
                      e_inc_num_hi) %>%
                .shortnames()

missing_data <- notification %>%
                 filter(year == report_year - 1) %>%
                 select(iso2,
                        c_newinc) %>%
                 inner_join(missing_inc) %>%
                 mutate(missing_best = (e_inc_num - c_newinc) / 1e6,
                        missing_lo = (e_inc_num_lo - c_newinc) / 1e6,
                        missing_hi = (e_inc_num_hi - c_newinc) / 1e6) %>%
                 arrange(desc(missing_best)) %>%
                 slice(1:10)

# plot as horizontal error bars
missing_plot <- missing_data %>%
                ggplot(aes(x=reorder(country, missing_best),
                           y=missing_best)) +
                geom_point() +
                labs(x="",
                     y="Millions",
                     title=paste("Figure 4.12 Estimated number of missed cases in top-ten countries,",
                                 report_year - 1)) +
                geom_pointrange(aes(ymin=missing_lo,
                                    ymax=missing_hi)) +
                theme_glb.rpt() +
                theme(plot.title = element_text(hjust = 0)) +
                expand_limits(y=0) +
                coord_flip()

# Save the plot
figsave(missing_plot, missing_data, "f4_12_missing_plot")

# Clean up (remove any objects with their name starting with 'missing')
rm(list=ls(pattern = "^missing"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.13   ------
# Number of new and relapse cases known to be HIV-positive,
# number started on ART and estimated number of incident HIV-positive TB cases,
# 30 high TB/HIV burden countries, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tbhiv_30hbc <- report_country %>%
                filter(g_hb_tbhiv==1) %>%
                select(iso2)

tbhiv_data <- TBHIV_for_aggregates %>%
              filter(year >= 2009) %>%
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
                  filter(year >= 2009) %>%
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
                  .shortnames()


# Plot as lines
inctbhiv_plot <- inctbhiv_data %>%
                  ggplot(aes(x=year, y=hivtest_pos, ymin=0)) +
                  geom_line(size=1) +
                  geom_ribbon(aes(x=year, ymin=e_inc_tbhiv_num_lo, ymax=e_inc_tbhiv_num_hi),
                              fill="red",
                              alpha=0.4) +
                  geom_line(aes(year, e_inc_tbhiv_num),
                            size=1,
                            colour="red") +

                  geom_line(aes(year, hiv_art),
                            size=1,
                            colour="blue") +

                  scale_y_continuous(name = "New and relapse cases per year (thousands)") +
                  xlab("Year") +

                  facet_wrap( ~ country,
                              scales = "free_y",
                              ncol = 5) +

                  ggtitle(paste0("Figure 4.13 Number of new and relapse cases(a) known to be HIV-positive (black),\nnumber started on ART (blue) and estimated number of incident HIV-positive TB cases (red),\n30 high TB/HIV burden countries, 2009 - ",
                               report_year-1)) +
                  theme_glb.rpt()


# Add footnote
inctbhiv_foot <- "(a) The calculation is for all cases in years prior to 2015."

inctbhiv_plot <- arrangeGrob(inctbhiv_plot, bottom = textGrob(inctbhiv_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(inctbhiv_plot, inctbhiv_data, "f4_13_inctbhiv_plot_hbc")

# Clean up (remove any objects with their name containing 'tbhiv')
rm(list=ls(pattern = "tbhiv"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.16a   ------
# Percentage coverage of treatment for TB,
# (new and relapse patients as % of estimated incidence)
# 30 high TB burden countries, WHO Regions and globally, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

coverage_inc_country <- estimates_epi_rawvalues %>%
                         filter(year == report_year - 1) %>%
                         select(entity = country,
                                iso2,
                                e_inc_num,
                                e_inc_num_lo,
                                e_inc_num_hi) %>%
                         # shorten long country names
                         .shortnames( col = "entity")

# Filter the country list down to high burden ones
coverage_30hbc <- report_country %>%
                  filter(g_hb_tb==1) %>%
                  select(iso2)

coverage_inc_country <- coverage_inc_country %>%
                        inner_join(coverage_30hbc)


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
                       title=paste("Figure 4.16a Estimated percentage coverage of treatment for TB\n",
                                   "(new and relapse patients as % of estimated incidence)\n",
                                   "30 high TB burden countries, WHO Regions and globally,",
                                   report_year - 1)) +
                  geom_pointrange(aes(ymin=c_cdr_lo,
                                      ymax=c_cdr_hi)) +
                  theme_glb.rpt() +
                  theme(plot.title = element_text(hjust = 0)) +
                  expand_limits(y=0) +
                  coord_flip()

coverage_plot <- arrangeGrob(coverage_plot,
                              bottom = textGrob("* No data",
                                             x = 0,
                                             hjust = -0.1,
                                             vjust=0,
                                             gp = gpar(fontsize = 10)))

# Save the plot
figsave(coverage_plot, coverage_data, "f4_16a_txcoverage_tb")

# Clean up (remove any objects with their name starting with 'coverage')
rm(list=ls(pattern = "^coverage"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.16b   ------
# Percentage coverage of treatment for TB/HIV
# (patients started on antiretroviral therapy as % of estimated TB/HIV incidence number)
# 30 high TB/HIV burden countries, WHO Regions and globally, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

coveragehiv_inc_country <- estimates_epi_rawvalues %>%
                           filter(year == report_year - 1) %>%
                           select(entity = country,
                                  iso2,
                                  e_inc_tbhiv_num,
                                  e_inc_tbhiv_num_lo,
                                  e_inc_tbhiv_num_hi)  %>%
                           # shorten long country names
                           .shortnames( col = "entity")

# Filter the country list down to high burden ones
coveragehiv_30hbc <- report_country %>%
                      filter(g_hb_tbhiv==1) %>%
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
                         title=paste("Figure 4.16b Estimated percentage coverage of treatment for TB/HIV\n",
                                     "(patients started on antiretroviral therapy as % of estimated TB/HIV incidence number)\n",
                                     "30 high TB/HIV burden countries, WHO Regions and globally,",
                                     report_year - 1)) +
                    geom_pointrange(aes(ymin=c_art_lo,
                                        ymax=c_art_hi)) +
                    theme_glb.rpt() +
                    theme(plot.title = element_text(hjust = 0)) +
                    expand_limits(y=0) +
                    coord_flip()

coveragehiv_plot <- arrangeGrob(coveragehiv_plot,
                              bottom = textGrob("* No data",
                                             x = 0,
                                             hjust = -0.1,
                                             vjust=0,
                                             gp = gpar(fontsize = 10)))

# Save the plot
figsave(coveragehiv_plot, coveragehiv_data, "f4_16b_txcoverage_tbhiv")

# Clean up (remove any objects with their name starting with 'coveragehiv')
rm(list=ls(pattern = "^coveragehiv"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.16c   ------
# Percentage coverage of treatment for MDR/RR-TB,
# (patients enrolled on treatment as % of estimated incidence of MDR/RR-TB)
# 30 high MDR-TB burden countries, WHO Regions and globally, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


coveragerr_inc_country <- estimates_drtb_rawvalues %>%
                          filter(year == report_year - 1) %>%
                          select(entity = country,
                                  iso2,
                                  e_inc_rr_num,
                                  e_inc_rr_num_lo,
                                  e_inc_rr_num_hi)  %>%
                          # shorten long country names
                          .shortnames( col = "entity")

# Filter the country list down to high burden ones
coveragerr_30hbc <- report_country %>%
                    filter(g_hb_mdr==1) %>%
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
                         title=paste("Figure 4.16c Estimated percentage coverage of treatment for MDR/RR-TB\n",
                                     "(patients started on treatment for MDR-TB as % of estimated MDR/RR-TB incidence number)\n",
                                     "30 high MDR-TB burden countries, WHO Regions and globally,",
                                     report_year - 1)) +
                    geom_pointrange(aes(ymin=c_rr_coverage_lo,
                                        ymax=c_rr_coverage_hi)) +
                    theme_glb.rpt() +
                    theme(plot.title = element_text(hjust = 0)) +
                    expand_limits(y=0) +
                    coord_flip()

coveragerr_plot <- arrangeGrob(coveragerr_plot,
                              bottom = textGrob("* No data",
                                             x = 0,
                                             hjust = -0.1,
                                             vjust=0,
                                             gp = gpar(fontsize = 10)))
# Save the plot
figsave(coveragerr_plot, coveragerr_data, "f4_16c_txcoverage_drtb")

# Clean up (remove any objects with their name starting with 'coveragehiv')
rm(list=ls(pattern = "^coveragerr"))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.20.a   ------
# Treatment outcomes for new and relapse TB cases,
# for 30 high TB burden countries, 6 WHO regions and globally, 2014
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


txout_country  <- outcomes %>%
                  filter(year==report_year - 2) %>%
                  select(country,
                         iso2,
                         g_whoregion,
                         rel_with_new_flg,
                         contains("newrel_"))  %>%
                  # shorten long country names
                  .shortnames() %>%
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
txout_30hbc <- report_country %>%
                filter(g_hb_tb==1) %>%
                select(iso2)

txout_country <- txout_country %>%
                  inner_join(txout_30hbc) %>%
                  #remove the iso2 field to match regional and countries aggregates
                  select(-iso2,
                         -g_whoregion)

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
                                          NA),
                 # Add a 'no data' option so non-reporters are highlighted in the output
                 `No data reported` = ifelse(is.na(newrel_coh) & substring(entity,1,2) != "--" ,100,0)
                 ) %>%
          # Keep record of current order (in reverse) so plot comes out as we want it
          mutate(entity = factor(entity, levels=rev(entity))) %>%
          # Drop the actual numbers and keep percentages
          select(-contains("newrel"))


#tsr_table$area <- factor(tsr_table$area, levels=rev(tsr_table$area))


# Flip into long mode for stacked bar plotting
txout_long <- melt(txout, id=1)




# Plot as stacked bars
txout_plot <- txout_long %>%
              ggplot(aes(entity, value, fill=variable)) +
                      geom_bar(stat="identity",
                               position="stack") +
                      coord_flip() +

                      theme_glb.rpt() +
                      scale_fill_manual("", values = outcomes_palette) +
                      labs(x="", y="Percentage of cohort (%)") +

                      theme(legend.position="bottom",
                            panel.grid=element_blank()) +

                      expand_limits(c(0,0)) +

                      ggtitle(paste0("Figure 4.20.a Treatment outcomes for new and relapse TB cases,\nfor 30 high TB burden countries,\n6 WHO regions and globally, ", report_year - 2))

txout_plot <- arrangeGrob(txout_plot,
                          bottom = textGrob("* Treatment outcomes are for new cases only.",
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0,
                                         gp = gpar(fontsize = 10)))


figsave(txout_plot, txout, "f4_20a_outcomes_tb", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txout')
rm(list=ls(pattern = "^txout"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.20.b   ------
# Treatment outcomes for new and relapse TB/HIV cases
# for 30 high TB/HIV burden countries, 6 WHO regions and globally, 2014
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


txtbhivout_country  <- outcomes %>%
                  filter(year==report_year - 2) %>%
                  select(country,
                         iso2,
                         g_whoregion,
                         contains("tbhiv_")) %>%
                  # shorten long country names
                  .shortnames() %>%
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
txtbhivout_30hbc <- report_country %>%
                filter(g_hb_tbhiv==1) %>%
                select(iso2)

txtbhivout_country <- txtbhivout_country %>%
                  inner_join(txtbhivout_30hbc) %>%
                  #remove the iso2 field to match regional and countries aggregates
                  select(-iso2,
                         -g_whoregion)

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
                                          NA),
                 # Add a 'no data' option so non-reporters are highlighted in the output
                 `No data reported` = ifelse(is.na(tbhiv_coh) & substring(entity,1,2) != "--" ,100,0)
                 ) %>%
          # Keep record of current order (in reverse) so plot comes out as we want it
          mutate(entity = factor(entity, levels=rev(entity))) %>%
          # Drop the actual numbers and keep percentages
          select(-contains("tbhiv"))


#tsr_table$area <- factor(tsr_table$area, levels=rev(tsr_table$area))


# Flip into long mode for stacked bar plotting
txtbhivout_long <- melt(txtbhivout, id=1)



# Plot as stacked bars
txtbhivout_plot <- txtbhivout_long %>%
              ggplot(aes(entity, value, fill=variable)) +
                      geom_bar(stat="identity",
                               position="stack") +
                      coord_flip() +

                      theme_glb.rpt() +
                      scale_fill_manual("", values = outcomes_palette) +
                      labs(x="", y="Percentage of cohort (%)") +

                      theme(legend.position="bottom",
                            panel.grid=element_blank()) +

                      expand_limits(c(0,0)) +

                      ggtitle(paste0("Figure 4.20.b Treatment outcomes for new and relapse\nTB/HIV cases, for 30 high TB/HIV burden countries,\n6 WHO regions and globally, ", report_year - 2))


figsave(txtbhivout_plot, txtbhivout, "f4_20b_outcomes_tbhiv", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txtbhivout')
rm(list=ls(pattern = "^txtbhivout"))

stop("

     >>>>>>>>>>
     Stopping here so can do the rest manually!
     <<<<<<<<<<<<")

