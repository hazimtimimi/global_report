# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figures for Annex 2 and 3 of the global report (country and regional profiles)
# Called from create_tables_figures.r which sets up the necessary dependencies
# Last updated by Hazim Timimi, July 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Profiles for 30 HBCs and 6 regions --------
# Incidence (and notifications) and mortality graphs
# For graphic designer to use in the printed report
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Countries incidence ----
# Get HBCs case notification and merge with their estimates (similar to fig 4.14 below)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

profile30hbc <- report_country %>%
                filter(g_hb_tb==1) %>%
                select(iso2)

newinc_data <- notification %>%
                filter(year >= 2000) %>%
                inner_join(profile30hbc) %>%
                select(year,
                       iso2,
                       country,
                       c_newinc)

inc_data <- estimates_epi_rawvalues %>%
            filter(year >= 2000) %>%
            inner_join(profile30hbc) %>%
            select(year,
                   iso2,
                   e_pop_num,
                   e_inc_100k,
                   e_inc_100k_lo,
                   e_inc_100k_hi,
                   e_inc_tbhiv_100k,
                   e_inc_tbhiv_100k_lo,
                   e_inc_tbhiv_100k_hi) %>%

            # Use a right-join so can see the data for the final year even in the absence of estimates
            right_join(newinc_data) %>%

            # Calculate case notification rate
            mutate(c_newinc_100k = c_newinc * 1e5 / e_pop_num)


# Plot
inc_plot <- inc_data %>%
            ggplot(aes(x=year, y=c_newinc_100k, ymin=0)) +
            geom_line(size=1) +
            geom_ribbon(aes(x=year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi),
                        fill=I('#00FF33'),
                        alpha=0.4) +
            geom_line(aes(year, e_inc_100k),
                      size=1,
                      colour=I('#00FF33')) +
            geom_ribbon(aes(x=year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi),
                        fill=I('#CC6666'),
                        alpha=0.4) +
            geom_line(aes(year,e_inc_tbhiv_100k),
                      size=1,
                      colour=I('#CC6666')) +

            scale_y_continuous(name = "Incidence (Rate per 100 000 population per year)") +
            xlab("Year") +

            facet_wrap( ~ country,
                        scales = "free_y",
                        ncol = 5) +

            ggtitle("Incidence graphs for the 30 high burden country profiles") +
            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the graph
ggsave(paste0(figures_folder, "/CPFigs/", "hbc_cp_inc", Sys.Date(), ".pdf"), inc_plot, width=14, height=9.5)

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Countries mortality ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

mort_data <- estimates_epi_rawvalues %>%
            filter(year >= 2000) %>%
            inner_join(profile30hbc) %>%
            select(year,
                   country,
                   iso2,
                   e_mort_exc_tbhiv_100k,
                   e_mort_exc_tbhiv_100k_lo,
                   e_mort_exc_tbhiv_100k_hi)

# Plot
mort_plot <- mort_data %>%
            ggplot(aes(x=year, y=e_mort_exc_tbhiv_100k, ymin=0)) +
            geom_line(size=1,
						colour=I('#ee82ee')) +
            geom_ribbon(aes(x=year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi),
                        fill=I('#ee82ee'),
                        alpha=0.4) +


            scale_y_continuous(name = "Mortality (excludes HIV+TB) (Rate per 100 000 population per year)") +
            xlab("Year") +

            facet_wrap( ~ country,
                        scales = "free_y",
                        ncol = 5) +

            ggtitle("Mortality graphs for the 30 high burden country profiles") +
            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the graph
ggsave(paste0(figures_folder, "/CPFigs/", "hbc_cp_mort", Sys.Date(), ".pdf"), mort_plot, width=14, height=9.5)

# Clean up (remove any objects with their name starting 'mort_')
rm(list=ls(pattern = "^mort_"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Regional and global incidence and notification ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

inc_data <- aggregated_estimates_epi_rawvalues %>%
            filter( year >= 2000 & group_type == "g_whoregion") %>%
            select(year,
                   g_whoregion = group_name,
                   e_inc_100k,
                   e_inc_100k_lo,
                   e_inc_100k_hi,
                   e_inc_tbhiv_100k,
                   e_inc_tbhiv_100k_lo,
                   e_inc_tbhiv_100k_hi,
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


# Plot
inc_plot <- inc_data %>%
            ggplot(aes(x=year, y=c_newinc_100k, ymin=0)) +
            geom_line(size=1) +
            geom_ribbon(aes(x=year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi),
                        fill=standard_palette("incidence"),
                        alpha=0.4) +
            geom_line(aes(year, e_inc_100k),
                      size=1,
                      colour=standard_palette("incidence")) +

            geom_ribbon(aes(x=year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi),
                        fill=standard_palette("tbhiv_incidence"),
                        alpha=0.4) +
            geom_line(aes(year,e_inc_tbhiv_100k),
                      size=1,
                      colour=standard_palette("tbhiv_incidence")) +

            facet_wrap( ~ entity, ncol = 4, scales="free_y") +
            scale_y_continuous(name = "Rate per 100 000 population per year") +
            xlab("Year") +

            ggtitle("Incidence graphs for the 6 regional profiles and the global profile") +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsave(inc_plot, inc_data, "aggregate_profiles_incidence")

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Regional and global mortality ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

mort_data <- aggregated_estimates_epi_rawvalues %>%
            filter( year >= 2000 & group_type == "g_whoregion") %>%
            select(year,
                   g_whoregion = group_name,
                   e_mort_exc_tbhiv_100k,
                   e_mort_exc_tbhiv_100k_lo,
                   e_mort_exc_tbhiv_100k_hi) %>%

            # merge with regional names
            inner_join(who_region_names, by = "g_whoregion") %>%
            select(-g_whoregion)

mort_global <-aggregated_estimates_epi_rawvalues %>%
              filter( year >= 2000 & group_type == "global") %>%
              select(year,
                     e_mort_exc_tbhiv_100k,
                     e_mort_exc_tbhiv_100k_lo,
                     e_mort_exc_tbhiv_100k_hi) %>%
              mutate(entity = "Global")

# Add global to the regional aggregates
mort_data <- rbind(mort_data, mort_global)

# Change the order
mort_data$entity <- factor(mort_data$entity,
                             levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))

mort_plot <- mort_data %>%
            ggplot(aes(x=year, y=e_mort_exc_tbhiv_100k, ymin=0)) +
            geom_line(size=1,
						colour=standard_palette("mortality_exc_tbhiv")) +
            geom_ribbon(aes(x=year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi),
                        fill=standard_palette("mortality_exc_tbhiv"),
                        alpha=0.4) +

            facet_wrap( ~ entity, ncol = 4, scales="free_y") +
            scale_y_continuous(name = "Mortality (excludes HIV+TB) (Rate per 100 000 population per year)") +
            xlab("Year") +

            ggtitle("Mortality graphs for the 6 regional profiles and the global profile") +
            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsave(mort_plot, mort_data, "aggregate_profiles_mortality")

# Clean up (remove any objects with their name containing 'mort_')
rm(list=ls(pattern = "mort_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Regional and global age/sex notifications  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This is similar to figure 4.2, but showing actual numbers rather than notification rates

# Get data
agesex <- filter(notification, year == (report_year - 1)) %>%
                 select(iso2,
                        g_whoregion,
                        newrel_m014, newrel_m1524, newrel_m2534, newrel_m3544, newrel_m4554, newrel_m5564, newrel_m65,
                        newrel_f014, newrel_f1524, newrel_f2534, newrel_f3544, newrel_f4554, newrel_f5564, newrel_f65)

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

agesex_filtered_agg <- rbind(agesex_filtered_regional, agesex_filtered_global)

# Switch to long ('tidy') format to plot
agesex_agg_long <- agesex_filtered_agg %>%
                      gather("group", "notified", 1:14)


# Extract sex and agegroup from the group field
agesex_agg_long$sex <- str_extract(agesex_agg_long$group, "f|m")
agesex_agg_long$sex <- factor(agesex_agg_long$sex,
                                   levels=c("f", "m"),
                                   labels=c("Female", "Male"))
agesex_agg_long$agegroup <- str_extract(agesex_agg_long$group, "014|1524|2534|3544|4554|5564|65")
agesex_agg_long$agegroup <- factor(agesex_agg_long$agegroup,
                                   levels=c("014", "1524", "2534", "3544", "4554", "5564", "65"),
                                   labels=c("0\u201314", "15\u201324", "25\u201334", "35\u201344", "45\u201354", "55\u201364", "\u226565"))

agesex_agg_long <- agesex_agg_long %>% select(-group, -tot_notified)


# Change the order
agesex_agg_long$entity <- factor(agesex_agg_long$entity,
                              levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))

# Now plot the aggregates as pyramids
# See first code example at https://github.com/JuanGaleano/Population-pyramids-ggplot2/blob/master/POPULATION%20PYRAMID%20GGPLOT2.R

# Annoyingly I cannot use free scales because I get the error "ggplot2 does not currently support free scales with a non-cartesian coord or coord_flip"

agesex_plot <- agesex_agg_long %>%
                # Multiply all the female rates by -1
                mutate(notified = ifelse(sex=="Female", notified * -1, notified )) %>%
                ggplot(aes(x=agegroup, y=notified, fill=sex)) +
                geom_bar(stat="identity",
                         size=.3,
                         colour="black",
                         position="identity") +
                scale_y_continuous(name = paste("Notified cases by age group and sex,",
                                                report_year - 1)
                                   #Set labels manually to avoid showing negative numbers!
                                   #breaks=c(-200000,-100000,0,100000,200000,300000),
                                   #labels=c("200 000", "100 000", "0", "100 000", "200 000", "300 000")
                                   ) +
                scale_x_discrete("Age group (years)",
                                 labels=levels(agesex_agg_long$agegroup)) +

                coord_flip() +
                facet_wrap( ~ entity, ncol = 4, scales="free_x") +

                ggtitle(paste0("Age/sex graphs for the 6 regional profiles and the global profile")) +
                theme_glb.rpt() +
                theme(legend.position="top",
                      legend.title=element_blank())


# Save the plot
figsave(agesex_plot, agesex_agg_long, "aggregate_profiles_agesex")

# Clean up (remove any objects with their name starting 'agesex_')
rm(list=ls(pattern = "^agesex"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Region treatment success rates  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tsr_aggs <- outcomes %>%
            filter(year >= 2000 & year <= report_year - 2) %>%
            select(year,
                   g_whoregion,
                   new_sp_coh,
                   new_sp_cur,
                   new_sp_cmplt,

                   new_snep_coh,
                   new_snep_cmplt,

                   newrel_coh,
                   newrel_succ,

                   ret_coh,
                   ret_cur,
                   ret_cmplt,

                   ret_nrel_coh,
                   ret_nrel_succ,

                   hiv_new_sp_coh,
                   hiv_new_sp_cur,
                   hiv_new_sp_cmplt,

                   hiv_new_snep_coh,
                   hiv_new_snep_cmplt,

                   hiv_ret_coh,
                   hiv_ret_cur,
                   hiv_ret_cmplt,

                   tbhiv_coh,
                   tbhiv_succ,

                   mdr_coh,
                   mdr_succ,

                   xdr_coh,
                   xdr_succ) %>%
            group_by(year, g_whoregion) %>%
            summarise_each(funs(sum(.,na.rm = TRUE)),
                           new_sp_coh:xdr_succ)  %>%


            # merge with regional names
            inner_join(who_region_names, by = "g_whoregion") %>%
            select(-g_whoregion) %>%
            ungroup()

tsr_global <- outcomes %>%
              filter(year >= 2000 & year <= report_year - 2) %>%
              select(year,
                     new_sp_coh,
                     new_sp_cur,
                     new_sp_cmplt,

                     new_snep_coh,
                     new_snep_cmplt,

                     newrel_coh,
                     newrel_succ,

                     ret_coh,
                     ret_cur,
                     ret_cmplt,

                     ret_nrel_coh,
                     ret_nrel_succ,

                     hiv_new_sp_coh,
                     hiv_new_sp_cur,
                     hiv_new_sp_cmplt,

                     hiv_new_snep_coh,
                     hiv_new_snep_cmplt,

                     hiv_ret_coh,
                     hiv_ret_cur,
                     hiv_ret_cmplt,

                     tbhiv_coh,
                     tbhiv_succ,

                     mdr_coh,
                     mdr_succ,

                     xdr_coh,
                     xdr_succ) %>%
              group_by(year) %>%
              summarise_each(funs(sum(.,na.rm = TRUE)),
                             new_sp_coh:xdr_succ)  %>%

              mutate(entity = "Global") %>%
              ungroup()

# Add global to the regional aggregates
tsr_aggs <- rbind(tsr_aggs, tsr_global)

# Change the order
tsr_aggs$entity <- factor(tsr_aggs$entity,
                             levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))


# calculate the treatment success rates
# only if the aggregated cohort exceeds 100 for most types, but > 10 for MDR and XDR so that don't get unrepresentative aggregate outcomes.

tsr_aggs$c_new_tsr <- ifelse(sum_of_row(tsr_aggs[c("new_sp_coh", "new_snep_coh", "newrel_coh")] ) > 100,
                              sum_of_row(tsr_aggs[c("new_sp_cur", "new_sp_cmplt", "new_snep_cmplt", "newrel_succ")] ) * 100 /
                              sum_of_row(tsr_aggs[c("new_sp_coh", "new_snep_coh", "newrel_coh")] ),
                              NA)

tsr_aggs$c_ret_tsr <- ifelse(sum_of_row(tsr_aggs[c("ret_coh", "ret_nrel_coh")] ) > 100,
                              sum_of_row(tsr_aggs[c("ret_cur", "ret_cmplt", "ret_nrel_succ")] ) * 100 /
                              sum_of_row(tsr_aggs[c("ret_coh", "ret_nrel_coh")] ),
                              NA)

tsr_aggs$c_tbhiv_tsr <- ifelse(sum_of_row(tsr_aggs[c("hiv_new_sp_coh", "hiv_new_snep_coh", "hiv_ret_coh", "tbhiv_coh")] ) > 100,
                                sum_of_row(tsr_aggs[c("hiv_new_sp_cur", "hiv_new_sp_cmplt", "hiv_new_snep_cmplt",
                                                      "hiv_ret_cur", "hiv_ret_cmplt",  "tbhiv_succ")] ) * 100 /
                                sum_of_row(tsr_aggs[c("hiv_new_sp_coh", "hiv_new_snep_coh", "hiv_ret_coh", "tbhiv_coh")] ),
                                NA)

tsr_aggs$c_mdr_tsr <- ifelse(tsr_aggs$mdr_coh > 10,
                                 tsr_aggs$mdr_succ * 100 / tsr_aggs$mdr_coh,
                                 NA)

tsr_aggs$c_xdr_tsr <- ifelse(tsr_aggs$xdr_coh > 10,
                                 tsr_aggs$xdr_succ * 100 / tsr_aggs$xdr_coh,
                                 NA)

# Slim the data down
tsr_aggs <- tsr_aggs %>%
                select(entity,
                       year,
                       c_new_tsr,
                       c_ret_tsr,
                       c_tbhiv_tsr,
                       c_mdr_tsr,
                       c_xdr_tsr)

# plot the aggregates as lines

tsr_plot <- tsr_aggs %>%
            ggplot(aes(x=year)) +

            geom_line(aes(y=c_new_tsr,
                          ymin=0,
                          ymax=100,
                          colour="c_new_tsr"),
                      size=1) +

            geom_line(aes(y=c_ret_tsr,
                          colour="c_ret_tsr"),
                      size=1) +

            geom_line(aes(y=c_tbhiv_tsr,
                          colour="c_tbhiv_tsr"),
                      size=1) +


            geom_line(aes(y=c_mdr_tsr,
                          colour="c_mdr_tsr"),
                      size=1) +

            geom_line(aes(y=c_xdr_tsr,
                          colour="c_xdr_tsr"),
                      size=1) +

            scale_colour_manual("",
                                breaks = c("c_new_tsr", "c_ret_tsr", "c_tbhiv_tsr", "c_mdr_tsr", "c_xdr_tsr" ),
                                labels = c("New and relapse", "Retreatment exclusing relapse", "HIV-positive", "MDR/RR-TB", "XDR-TB"),
                                values = c("c_new_tsr"="blue",
                                           "c_ret_tsr"="green",
                                           "c_tbhiv_tsr"="red",
                                           "c_mdr_tsr"="orange",
                                           "c_xdr_tsr"="black")) +

            scale_y_continuous(name = "Treatment success rate (%)") +
            xlab("Year") +

            facet_wrap( ~ entity, ncol = 4, scales="free_y") +

            ggtitle("Treatment success rate graphs for the 6 regional profiles and the global profile") +

            theme_glb.rpt() +
            theme(legend.position="bottom")

# Save the plot
figsave(tsr_plot, tsr_aggs, "aggregate_profiles_treatment_success")

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "tsr_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 7. Region finance  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(RODBC)
ch <- odbcDriverConnect(connection_string)
fin_aggs   <- sqlFetch(ch, "reports.view_TME_aggregated_finance_estimates")
close(ch)

fin_aggs <- fin_aggs %>%
                filter( year > (report_year - 5)) %>%
                mutate(domestic_funding = domestic_funding / 1e6,
                       international_funding = international_funding / 1e6,
                       unfunded_gap = unfunded_gap / 1e6) %>%

                select(g_whoregion = group_name,
                       year,
                       domestic_funding,
                       international_funding,
                       unfunded_gap) %>%

                # merge with regional names
                left_join(who_region_names, by = "g_whoregion") %>%

                # Add 'Global' entity name
                mutate(entity = ifelse(g_whoregion == "global", "Global", entity)) %>%
                select(-g_whoregion)

# Change the order
fin_aggs$entity <- factor(fin_aggs$entity,
                             levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))

# Switch to long ('tidy') format to plot
fin_aggs_long <- fin_aggs %>%
                  gather("group", "amount", 2:4)

# Make the group variable a factor to control the order of plotting
fin_aggs_long$group <- factor(fin_aggs_long$group,
                                  levels = c("unfunded_gap",
                                             "international_funding",
                                             "domestic_funding"),
                                  labels = c("Unfunded",
                                             "Funded internationally",
                                             "Funded domestically"))

# plot as stacked bars
fin_plot <- fin_aggs_long %>%
            ggplot(aes(x=year,
                       y=amount,
                       fill=group,
                       order = desc(group)))  +

            geom_bar(stat="identity",
                     size=.3,
                     colour="black") +

            scale_y_continuous(name = "Total budget (US$ millions constant 2016)") +

            facet_wrap( ~ entity, ncol = 4, scales="free_y") +

            ggtitle(paste0("Age/sex graphs for the 6 regional profiles and the global profile")) +

            theme_glb.rpt() +
            theme(legend.position="bottom",
                  legend.title=element_blank()) +

            guides(fill=guide_legend(reverse=TRUE))

# Save the plot
figsave(fin_plot, fin_aggs, "aggregate_profiles_finance")

# Clean up (remove any objects with their name containing 'fin_')
rm(list=ls(pattern = "fin_"))

