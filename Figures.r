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
# Executive summary ------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure E.1 ?  ------
# Progress towards global targets and milestones
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# UNHLM 2018-2022 targets
# Get the notification totals starting in 2018
unhlm_n <- notification %>%
           filter(year >= 2018) %>%
           select(c_newinc,
                  c_new_014,
                  unconf_rrmdr_tx,
                  conf_rrmdr_tx,
                  hiv_ipt,
                  hiv_ipt_reg_all) %>%

           # select IPT in all HIV patients in care (hiv_ipt_reg_all) if available,
           # otherwise take IPT in HIV patients newly enrolled in care (hiv_ipt)
           #
           # Also, calculate total number started on MDR treatment
           mutate(ipt = ifelse(!is.na(hiv_ipt_reg_all), hiv_ipt_reg_all, hiv_ipt),
                  rrmdr_tx = NZ(unconf_rrmdr_tx) + NZ(conf_rrmdr_tx)) %>%

           # calculate totlas
           summarise_at(vars(c_newinc,
                             c_new_014,
                             rrmdr_tx,
                             ipt),
                        sum,
                        na.rm=TRUE) %>%

           mutate(entity="Global")

# Get the preventive treatment totals starting in 2018
unhlm_s <- strategy %>%
           filter(year >= 2018) %>%
           summarise_at(vars(newinc_con_prevtx,
                             newinc_con04_prevtx),
                        sum,
                        na.rm=TRUE) %>%

           mutate(entity="Global")

# Merge the totals
unhlm_totals <- unhlm_n %>%
                inner_join(unhlm_s)


# Calculate percentage completed for each target
unhlm_pcnt <- unhlm_totals %>%
              mutate(
                     newinc = c_newinc * 100 / 40e6,   # 40 million notified
                     new_014= c_new_014 * 100 / 3.5e6, # 3.5 million children notified
                     rrmdr  = rrmdr_tx * 100 / 1.5e6,  # 1.5 million treated for drug-resistant TB
                     prevtx_tot = (ipt + newinc_con_prevtx) * 100 / 30e6, # 30 million preventive treatment
                     prevtx_04 = newinc_con04_prevtx * 100 / 4e6, # 4 million household contacts aged under 5 given preventive treatment
                     prevtx_con_5p = (newinc_con_prevtx - newinc_con04_prevtx) * 100 / 20e6, # 20 million household contacts aged 5 and above given preventive treatment
                     prevtx_hiv = ipt * 100 / 6e6  # 6 million people living with HIV given preventive treatment
                     ) %>%
              select(newinc,
                     new_014,
                     rrmdr,
                     prevtx_tot,
                     prevtx_04,
                     prevtx_con_5p,
                     prevtx_hiv
                     ) %>%

              # Switch to long format for plotting
              gather(key = "target", value = "percent")

# Plot the data

unhlm_plot <- unhlm_pcnt %>%
              ggplot(aes(x=target, y=percent)) +
              geom_bar(stat="identity", fill="darkblue",width = 0.5) +
              theme_glb.rpt() +
              expand_limits(y=c(0,100)) +
              scale_y_continuous(breaks = c(20, 40, 60 , 80, 100)) +
              ylab("Percent achieved") +
              scale_x_discrete(limits = c("prevtx_hiv",
                                          "prevtx_con_5p",
                                          "prevtx_04",
                                          "prevtx_tot",
                                          "rrmdr",
                                          "new_014",
                                          "newinc"),
                               labels = c("6 million people living with HIV\ngiven preventive treatment",
                                          "20 million household contacts\naged 5 and above\ngiven preventive treatment",
                                          "4 million household contacts\naged under 5\ngiven preventive treatment",
                                          "30 million people\ngiven preventive treatment",
                                          "1.5 million treated\nfor drug-resistant TB",
                                          "3.5 million children notified",
                                          "40 million notified")) +
              xlab("2018-2022 cumulative target") +
              coord_flip() +

              ggtitle(paste0("FIG.E.1?\nProgress towards 2018-2022 global targets and milestones, ", report_year - 1))


figsavecairo(unhlm_plot,
             unhlm_pcnt,
             "fe_1_unhlm_targets")


# Clean up (remove any objects with their name beginning with 'unhlm')
rm(list=ls(pattern = "^unhlm"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 3 ------
# TB disease burden
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


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
                             paste0("FIG.3.3\nEstimated TB incidence in ",
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
figsavecairo(topinc_map,
        select(topinc_data,
               iso3,
               country,
               bubble_size),
        "f3_3_inc_bubble_map")

# Clean up (remove any objects with their name beginning with 'topinc')
rm(list=ls(pattern = "^topinc"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 3.32  (map with bubbles) ------
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
                            paste0("FIG.3.32\n",
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
figsavecairo(rrnum_map,
        select(rrnum_data,
               iso3,
               country,
               bubble_size),
        "f3_32_rr_inc_bubble_map")

# Clean up (remove any objects with their name beginning with 'rrnum')
rm(list=ls(pattern = "^rrnum"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Diagnosis and treatment of TB, HIV-associated TB and drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1  new ------
# Notifications of TB cases (new and relapse cases, all forms) (black) compared with estimated TB incidence cases (green),
# 2000–2018, globally and for WHO regions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

inc_data <- aggregated_estimates_epi_rawvalues %>%
  filter( year >= 2000 & group_type == "g_whoregion") %>%
  select(year,
         g_whoregion = group_name,
         e_inc_num,
         e_inc_num_lo,
         e_inc_num_hi,
         c_newinc) %>%
  mutate(c_newinc_millions = c_newinc / 1e6,
         e_inc_num_millions = e_inc_num / 1e6,
         e_inc_num_lo_millions = e_inc_num_lo / 1e6,
         e_inc_num_hi_millions = e_inc_num_hi / 1e6) %>%
  
  # merge with regional names
  inner_join(who_region_names, by = "g_whoregion") %>%
  select(-g_whoregion)


inc_global <- aggregated_estimates_epi_rawvalues %>%
  filter( year >= 2000 & group_type == "global") %>%
  select(year,
         e_inc_num,
         e_inc_num_lo,
         e_inc_num_hi,
         c_newinc) %>%
  mutate(c_newinc_millions = c_newinc / 1e6,
         e_inc_num_millions = e_inc_num / 1e6,
         e_inc_num_lo_millions = e_inc_num_lo / 1e6,
         e_inc_num_hi_millions = e_inc_num_hi / 1e6,
         entity = "Global")

# Add global to the regional aggregates
inc_data <- rbind(inc_data, inc_global)

# Change the order
inc_data$entity <- factor(inc_data$entity,
                          levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))


# Plot as lines
inc_plot <- inc_data %>%
  ggplot(aes(x=year, y=c_newinc_millions, ymin=0)) +
  geom_line(size=1) +
  geom_ribbon(aes(x=year, ymin=e_inc_num_lo_millions, ymax=e_inc_num_hi_millions),
              fill=standard_palette("incidence"),
              alpha=0.4) +
  geom_line(aes(year, e_inc_num_millions),
            size=1,
            colour=standard_palette("incidence")) +
  
  facet_wrap( ~ entity, ncol = 4, scales="free_y") +
  scale_x_continuous(name="Year",
                     breaks = c(2000, 2009, report_year-1)) +
  scale_y_continuous(name = "Millions per year") +
  ggtitle(paste0("FIG.4.1\nNotifications of TB cases (new and relapse cases, all forms) (black) compared with estimated TB incident cases (green),\n2000-",
                 report_year-1,
                 ", globally and for WHO regions. Shaded areas represent uncertainty bands.")) +
  
  theme_glb.rpt() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.text.x = element_text(size=5))

# Save the plot
figsavecairo(inc_plot,
             select(inc_data,
                    year,
                    c_newinc_millions,
                    e_inc_num_millions,
                    e_inc_num_lo_millions,
                    e_inc_num_hi_millions,
                    entity),
             NA,
             "f4_1_inc_number_plot_aggregates")

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.3  ------
# New and relapse TB case notification rates and estimated TB incidence by age and sex in 2018,
# globally and for WHO regions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get data of notification
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
                                   labels=c("0\u201314", "15\u201324", "25\u201334", "35\u201344", "45\u201354", "55\u201364", "\u226565"))

agesex_agg_long <- agesex_agg_long %>% select(-group)
# Change the order
agesex_agg_long$entity <- factor(agesex_agg_long$entity,
                                 levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))



# Get data of estimated population
agesex_agg_pop    <- filter(estimates_population, year == (report_year - 1)) %>%
  select(g_whoregion,
         e_pop_m014, e_pop_m1524, e_pop_m2534, e_pop_m3544, e_pop_m4554, e_pop_m5564, e_pop_m65,
         e_pop_f014, e_pop_f1524, e_pop_f2534, e_pop_f3544, e_pop_f4554, e_pop_f5564, e_pop_f65)%>%
  group_by(g_whoregion) %>%
  summarise_at(vars(contains("e_pop_")),
               sum,
               na.rm = TRUE) %>%
  # merge with regional names and simplify to match structure of country table
  inner_join(who_region_names, by = "g_whoregion") %>%
  select(-g_whoregion)

agesex_agg_pop_global    <-agesex_agg_pop%>%
  summarise_at(vars(contains("e_pop_")),
               sum,
               na.rm = TRUE)%>%
  mutate(entity="Global")

agesex_agg_pop    <- agesex_agg_pop%>%
  rbind(agesex_agg_pop_global)

agesex_agg_pop_long <- agesex_agg_pop %>%
  gather("group", "pop", 1:14)

agesex_agg_pop_long$sex <- str_extract(agesex_agg_pop_long$group, "f|m")
agesex_agg_pop_long$sex <- factor(agesex_agg_pop_long$sex,
                                  labels=c("Female", "Male"))
agesex_agg_pop_long$agegroup <- str_extract(agesex_agg_pop_long$group, "014|1524|2534|3544|4554|5564|65")
agesex_agg_pop_long$agegroup <- factor(agesex_agg_pop_long$agegroup,
                                       labels=c("0\u201314", "15\u201324", "25\u201334", "35\u201344", "45\u201354", "55\u201364", "\u226565"))
agesex_agg_pop_long$agegroup <- gsub("–","-",agesex_agg_pop_long$agegroup)

agesex_agg_pop_long <- agesex_agg_pop_long %>% 
  select(-group)

# Get data of estimated incidence
agesex_inc    <- filter(aggregated_estimates_agesex_rawvalues, year == (report_year - 1)) %>%
  select(group_name,
         age_group,
         sex,
         best)%>%
  filter(age_group!= "all" & age_group!= "15plus" & age_group!= "0-4" & age_group!= "5-14",
         sex!= "a") %>%
  mutate(g_whoregion = group_name,
         agegroup= recode(age_group, "65plus"="\u226565"),
         incidence = best,
         sex = recode(sex, "f"="Female","m"="Male"))%>%
  # merge with regional names
  inner_join(who_region_names, by = "g_whoregion") %>%
  select(-g_whoregion,
         -group_name,
         -age_group,
         -best)

agesex_inc_global  <-agesex_inc%>%
  group_by(sex,agegroup)%>%
  summarise_at(vars(incidence),
               sum,
               na.rm = TRUE)%>%
  mutate(entity="Global")%>%
  ungroup()

agesex_inc <- agesex_inc%>%
  # aprrehend with global data 
  rbind(agesex_inc_global)%>%
  # merge with populations
  left_join(agesex_agg_pop_long,by=c("sex","entity","agegroup"))%>%
  # calculate incidence
  mutate(inc100k=incidence * 1e5 / pop)

agesex_inc$entity <- factor(agesex_inc$entity,
                            levels = c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))


# merge with notifictions
agesex_agg_long$agegroup <- gsub("–","-",agesex_agg_long$agegroup)
agesex_agg_long <- agesex_agg_long %>%
  left_join(agesex_inc,by=c("sex","entity","agegroup"))%>%
  select(-incidence,
         -pop)
agesex_agg_long$agegroup <- factor(agesex_agg_long$agegroup,
                                   levels=c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "\u226565"),
                                   labels=c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "\u226565"))

# Now plot the aggregates as pyramids
# See first code example at https://github.com/JuanGaleano/Population-pyramids-ggplot2/blob/master/POPULATION%20PYRAMID%20GGPLOT2.R

agesex_plot <- agesex_agg_long %>%
  # Multiply all the female rates by -1
  mutate(rate = ifelse(sex=="Female", rate * -1, rate ),
         inc100k = ifelse(sex=="Female", inc100k * -1, inc100k )) %>%
  ggplot() +
  geom_bar(aes(x=agegroup, y=rate, fill=sex),
           stat="identity",
           size=.3,
           position="identity") +
  scale_fill_manual(values=agesex_palette()) +
  geom_bar(aes(x=agegroup, y=inc100k,fill=NA),
           stat="identity",
           size=.3,
           colour="black",
           position="identity") +
  scale_y_continuous(name = "Rate per 100 000 population per year",
                     #Set labels manually to avoid showing negative numbers!
                     breaks=c(-400,-200,0,200,400,600),
                     labels=abs(c(-400,-200,0,200,400,600))) +
  scale_x_discrete("Age group (years)",
                   labels=levels(agesex_agg_long$agegroup)) +
  coord_flip() +
  facet_wrap( ~ entity, ncol = 4) +
  ggtitle(paste0("FIG.4.3\nEstimated TB incidence (black line) and new and relapse TB case notification rates by age and sex\u1d43 (female in red; male \nin green) in ",
                 report_year - 1,
                 ", globally and for WHO regions")) +
  theme_glb.rpt() +
  theme(legend.position="right",
        legend.title=element_blank())

# Add footnote
agesex_pcnt <- rounder(sum(agesex$tot_notified, na.rm=TRUE)  * 100
                       /
                         sum(notification[notification$year==report_year -1, "c_newinc"], na.rm=TRUE))

agesex_foot <- paste0("\u1d43  Countries not reporting cases in these categories are excluded. Cases included accounted for ",
                      agesex_pcnt,
                      "% of reported cases.")

agesex_plot <- arrangeGrob(agesex_plot, bottom = textGrob(agesex_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))

# Save the plots
figsavecairo(agesex_plot, agesex_agg_long,agesex_foot, "f4_3_agesex")

# Clean up (remove any objects with their name beginning with 'agesex')
rm(list=ls(pattern = "^agesex"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.4 (map) ------
# Percentage new and relapse TB cases that were children (aged <15), 2018
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
                     c('0-1.9', '2-4.9', '5-9.9', '\u226510'),
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
                         paste0("FIG.4.4\nPercentage of new and relapse TB cases that were children (aged <15), ",
                                report_year-1),
                         "Percentage",
                         copyright=FALSE,
                         background="White",
                         #colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                         colors=brewer.pal(4, "Blues"),
                         show=FALSE)

# Add footnote about using earlier data for some countries. Removed in 2019
# kids_foot <- paste("\u1d43 ",
                    report_year - 2,
                    "data were used for",
                    nrow(kids_prev_year_data),
                    "countries")



# kids_map <- arrangeGrob(kids_map, bottom = textGrob(kids_foot, x = 0, hjust = -0.1, vjust=-1.5, gp = gpar(fontsize = 10)))

figsavecairo(kids_map,
             select(kids_data_combined,
                    iso3,
                    country,
                    kids_pct,
                    cat),
             NA,
             "f4_4_pct_children_map")

# Clean up (remove any objects with their name beginning with 'kids')
rm(list=ls(pattern = "^kids"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.5  ------
# Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, 2000-2018, globally and for WHO regions
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
                                     breaks = c(2002, 2006, 2010, 2014, report_year-1)) +
                  scale_y_continuous(name = "Percentage bacteriologically confirmed") +
                  expand_limits(y=c(0,100)) +
                  facet_wrap( ~ entity, ncol = 4) +
                  ggtitle(paste0("FIG.4.5\nPercentage of new and relapse\u1d43 pulmonary TB cases with bacteriological confirmation, globally and for WHO regions,\n2000-",
                               report_year-1)) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
bacconf_foot <- "\u1d43 The calculation for new and relapse pulmonary cases in years prior to 2013 is based on smear results, except for the European Region\nwhere data on confirmation by culture was also available for the period 2002-2012."

bacconf_plot <- arrangeGrob(bacconf_plot, bottom = textGrob(bacconf_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsavecairo(bacconf_plot, bacconf_data,bacconf_foot, "f4_5_bacconf_plot")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))

# Figure 4.6  ------
# Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, 2000-2018, in the 30 high TB burden countries
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bacconf_30hbc <- country_group_membership %>%
  filter(group_type == "g_hb_tb" & group_name == 1) %>%
  select(iso2)

bacconf_data <- notification %>%
  filter(year >= 2000) %>%
  select(iso2,
         iso3,
         year,
         country,
         g_whoregion,
         # old variables pre-2013
         new_sp,
         new_sn,
         new_su,
         # new variables
         new_labconf, new_clindx,
         ret_rel_labconf, ret_rel_clindx) %>%
  # restrict to the 30 high burden countries
  inner_join(bacconf_30hbc)%>%
  group_by(year, g_whoregion,country) %>%
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
  
  # shorten long country names
  get_names_for_tables( col = "country")%>%
  
  # get rid of the oh-so-pesky grouping variables within the dataframe
  ungroup() %>%
  
  # get rid of extra variables
  select(country,
         year,
         bacconf_pct_numerator,
         bacconf_pct_denominator) %>%
  
  # Calculate the percentages
  mutate(bacconf_pct = bacconf_pct_numerator * 100 / bacconf_pct_denominator)

# Plot as lines
bacconf_plot <- bacconf_data %>%
  ggplot(aes(x=year, y=bacconf_pct)) +
  geom_line(size=1) +
  scale_x_continuous(name="Year",
                     breaks = c(2001, 2009, report_year-1)) +
  scale_y_continuous(name = "Percentage bacteriologically confirmed") +
  expand_limits(y=c(0,100)) +
  facet_wrap( ~ country, scales="free_y",ncol = 5) +
  ggtitle(paste0("FIG.4.6\nPercentage of new and relapse\u1d43 pulmonary TB cases with bacteriological \nconfirmation, 2000-",
                 report_year-1, ", 30 high TB burden countries")) +
  theme_glb.rpt() +
  theme(legend.position="top",
        legend.title=element_blank(),
        strip.text.x = element_text(size=6))

# Add footnote
bacconf_foot <- "\u1d43 The calculation for new and relapse pulmonary cases in years prior to 2013 is based on \nsmear results, except for the Russian Federation where data on confirmation by culture \nwas also available for the period 2002-2012."

bacconf_plot <- arrangeGrob(bacconf_plot, bottom = textGrob(bacconf_foot, x = 0, hjust = -0.1, vjust=0.3, gp = gpar(fontsize = 10)))

# Save the plot
figsavecairo(bacconf_plot, bacconf_data, bacconf_foot, "f4_6_plus_bacconf_plot",width=7, height=11)

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.12  (Map) ------
# Percentage of new and relapse TB casestested using a WHO-recommended rapid diagnostic as
# the initial diagnostic test, 2018
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


wrd_data <- notification %>%
  filter(year  >= report_year - 2) %>%
  select(iso3,
         country,
         year,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         rdxsurvey_newinc,
         rdxsurvey_newinc_rdx) %>%
  
  # calculate the percentage for each country depending on data availability
  mutate(wrd_pcnt_nu = ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                             newinc_rdx,
                             ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                    rdxsurvey_newinc_rdx,
                                    NA)),
         wrd_pcnt_de = ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                              c_newinc,
                              ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                     rdxsurvey_newinc,
                                     NA)),
         wrd_pcnt =ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                          newinc_rdx * 100 / c_newinc,
                          ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                 rdxsurvey_newinc_rdx * 100 / rdxsurvey_newinc,
                                 NA)))



wrd_data$cat <- cut(wrd_data$wrd_pcnt,
                    c(0, 25, 50, 75, 90, Inf),
                    c('0-24', '25-49', '50-75', '76-90','\u226590'),
                    right=FALSE)


# Find the countries with empty data for latest year and see if there are data for the previous year
wrd_prev_year_data <- wrd_data %>%
  filter(year == report_year - 1 & is.na(wrd_pcnt)) %>%
  select(iso3) %>%
  inner_join(filter(wrd_data, year == report_year - 2)) %>%
  filter(!is.na(wrd_pcnt))

# Now combine into one dataframe, with previous data used if latest year's data are not available
wrd_data_combined <- wrd_data %>%
  filter(year == report_year - 1) %>%
  anti_join(wrd_prev_year_data, by= "iso3") %>%
  rbind(wrd_prev_year_data)

# Caculating how many countries have reach the target of 90%
wrd_target90 <- wrd_data_combined%>%
                filter(wrd_pcnt >= 90)

# produce the map
wrd_map <- WHOmap.print(wrd_data_combined,
                        paste0("FIG.4.12\nPercentage of new and relapse TB cases initially tested using a WHO-recommended rapid diagnostic\n test, ",
                               report_year-1),
                        "Percentage",
                        copyright=FALSE,
                        background="White",
                        colors=brewer.pal(5, "YlOrRd"),
                        show=FALSE)

# Add footnote about using earlier data for some countries and for reaching target of 90%
# This footnote has been removed due to Kathrine's request.
# wrd_foot <- paste("\u1d43 ",
                  report_year - 2,
                  "data were used for",
                  nrow(wrd_prev_year_data),
                  "countries.",
                  nrow(wrd_target90),
                  "countries have met the target of 90% required for this indicator as in Table 2.2.")


# wrd_map <- arrangeGrob(wrd_map, bottom = textGrob(wrd_foot, x = 0, hjust = -0.1, vjust=-1.5, gp = gpar(fontsize = 10)))



figsavecairo(wrd_map,
             select(wrd_data_combined,
                    iso3,
                    country,
                    wrd_pcnt_nu,
                    wrd_pcnt_de,
                    wrd_pcnt,
                    cat),
             NA,
             "f4_12_pct_wrd_map")

# Clean up (remove any objects with their name beginning with 'wrd')
rm(list=ls(pattern = "^wrd"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.7  (map) ------
# Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, 2018
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
                        c('0-49', '50-64', '65-79', '\u226580'),
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
                            paste0("FIG.4.7\nPercentage of new and relapse pulmonary TB cases with bacteriological confirmation, ",
                                   report_year-1),
                            "Percentage",
                            copyright=FALSE,
                            background="White",
                            #colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                            colors=brewer.pal(4, "PuRd"),
                            show=FALSE)

# Add footnote about using earlier data for some countries. Removed in 2019
# bacconf_foot <- paste("\u1d43 ",
                      report_year - 2,
                      "data were used for",
                      nrow(bacconf_prev_year_data),
                      "countries.")


# bacconf_map <- arrangeGrob(bacconf_map, bottom = textGrob(bacconf_foot, x = 0, hjust = -0.1, vjust=-1.5, gp = gpar(fontsize = 10)))



figsavecairo(bacconf_map,
             select(bacconf_data_combined,
                    iso3,
                    country,
                    bacconf_pct,
                    cat),
             NA,
             "f4_7_pct_bacconf_map")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.8  (Map) ------
# Percentage of extrapulmonary cases among new and relapse TB cases, 2018
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
                   c('0-9.9', '10-19', '20-29', '\u226530'),
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
                       paste0("FIG.4.8\nPercentage of extrapulmonary cases among new and relapse TB cases, ",
                              report_year-1),
                       "Percentage",
                       copyright=FALSE,
                       background="White",
                       colors=brewer.pal(4, "YlOrRd"),
                       show=FALSE)

# Add footnote about using earlier data for some countries, removed in 2019
# ep_foot <- paste("\u1d43 ",
                 report_year - 2,
                 "data were used for",
                 nrow(ep_prev_year_data),
                 "countries.")


# ep_map <- arrangeGrob(ep_map, bottom = textGrob(ep_foot, x = 0, hjust = -0.1, vjust=-1.5, gp = gpar(fontsize = 10)))



figsavecairo(ep_map,
             select(ep_data_combined,
                    iso3,
                    country,
                    ep_pct,
                    cat),
             NA,
             "f4_8_pct_ep_map")

# Clean up (remove any objects with their name beginning with 'ep')
rm(list=ls(pattern = "^ep"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.9  ------
# Percentage of new and relapse TB cases with documented HIV status, 2004-2018 globally and for WHO regions
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
                  scale_x_continuous(name= "Year", breaks = c(2006, 2010, 2014, report_year-1)) +
                  scale_y_continuous(name = "Percentage with documented HIV status") +
                  expand_limits(y=c(0,100)) +
                  #scale_x_discrete(name = "Year") +
                  facet_wrap( ~ entity, ncol = 4) +
                  ggtitle(paste0("FIG.4.9\nPercentage of new and relapse\u1d43 TB cases with documented HIV status, 2004-",
                               report_year-1,
                               ", globally and for WHO regions\u1d47 ")) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
hivstatus_foot <- "\u1d43 The calculation is for all cases in years prior to 2015.\n\u1d47 Countries were excluded if the number with documented HIV status was not reported to WHO."


hivstatus_plot <- arrangeGrob(hivstatus_plot, bottom = textGrob(hivstatus_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsavecairo(hivstatus_plot, hivstatus_data, hivstatus_foot, "f4_9_hivstatus_plot")

# Clean up (remove any objects with their name beginning with 'hivstatus')
rm(list=ls(pattern = "^hivstatus"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.10  (map) ------
# Percentage of new and relapse TB cases with documented HIV status, 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


hivstatus_data <- TBHIV_for_aggregates %>%
                  filter(year >= report_year - 2) %>%
                  select(iso3,
                         country,
                         year,
                         hivtest_pct_denominator,
                         hivtest_pct_numerator) %>%

                  # Calculate % with known HIV status
                  mutate(hivstatus_pct = ifelse(is.na(hivtest_pct_numerator) | NZ(hivtest_pct_denominator) == 0, NA,
                                                hivtest_pct_numerator * 100 / hivtest_pct_denominator))


hivstatus_data$cat <- cut(hivstatus_data$hivstatus_pct,
                          c(0, 25, 50, 75, Inf),
                          c('0-24', '25-49', '50-74', "\u226575"),
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
                        paste("FIG.4.10\nPercentage of new and relapse TB cases with documented HIV status, ",
                              report_year-1),
                           "Percentage",
                           copyright=FALSE,
                           background="White",
                           colors=c("#bdc9e1","#74a9cf", "#2b8cbe", "#045a8d"),
                           show=FALSE)


# Add footnote about using earlier data for some countries. Removed in 2019 report.
# hivstatus_foot <- paste("\u1d43 ",
                        report_year - 2,
                        "data were used for",
                        nrow(hivstatus_prev_year_data),
                        "countries.")



# hivstatus_map <- arrangeGrob(hivstatus_map, bottom = textGrob(hivstatus_foot, x = 0, hjust = -0.1, vjust=-1.5, gp = gpar(fontsize = 10)))


figsavecairo(hivstatus_map,
             select(hivstatus_data_combined,
                    iso3,
                    country,
                    hivstatus_pct,
                    cat),
             NA,
             "f4_10_pct_HIV_status_map")

# Clean up (remove any objects with their name beginning with 'hivstatus')
rm(list=ls(pattern = "^hivstatus"))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.11   ------
# Global numbers of notified new and relapse cases known to be HIV-positive, number started on ART
# and estimated number of incident HIV-positive TB cases, 2004-2018
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

  ggtitle(paste0("FIG.4.11\nGlobal numbers of notified new and relapse cases\u1d43 known to be HIV-positive (black), number started on antiretroviral therapy (blue)\nand estimated number of incident HIV-positive TB cases (red), 2004-",
                 report_year-1,
                 ". Shaded areas represent uncertainty bands.")) +

  theme_glb.rpt()


# Add footnote
inctbhiv_foot <- "\u1d43 The calculation is for all cases in years prior to 2015."

inctbhiv_plot <- arrangeGrob(inctbhiv_plot, bottom = textGrob(inctbhiv_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsavecairo(inctbhiv_plot, inctbhiv_data, inctbhiv_foot, "f4_11_inctbhiv_plot_global")

# Clean up (remove any objects with their name containing 'tbhiv')
rm(list=ls(pattern = "tbhiv"))


# Figure 4.13 ------
# Percentage of bacteriologically confirmed TB cases tested for RR-TB, globally and for WHO regions, 2009-2018
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This one is a bit of a mess. For years up to 2014 (i.e. reporting year 2015),
# we took the highest value of DST coverage in the surveillance or notification pages.
# For years 2015-2016 (2016 data collection year onwards) we only use the notification value
# For years 2018 onwards (2019 data collection year onwards) we will use indicator from dr_surveillance to exclude ep cases; 
# These indicators(pulm_labconf_new, pulm_labconf_ret, r_rlt_new, r_rlt_ret) are collected starting from 2018
# but in 2018, South Africa reported an unreasonablely low number for r_rlt_new 
# which would affect the graphic for Africa, to avoid this, still use notification data for South Africa for year 2017.


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
  filter(year >= 2009) %>%
  select(year,
         iso3,
         dst_rlt_new,
         dst_rlt_ret,
         pulm_labconf_new,
         pulm_labconf_ret,
         r_rlt_new,
         r_rlt_ret) %>%
  mutate(
    # numerator
    dst_drs_num = ifelse(year < 2015,ifelse(is.na(dst_rlt_new) & is.na(dst_rlt_ret), NA,
                                            (NZ(dst_rlt_new) + NZ(dst_rlt_ret))),
                         ifelse(year >= 2017, NZ(r_rlt_new) + NZ(r_rlt_ret), NA)),
    # denominator
    dst_drs_denom = ifelse(year >= 2017, NZ(pulm_labconf_new) + NZ(pulm_labconf_ret), NA)
  )

# Link the two data sets

dst_data <- dst_notif_data %>%
  left_join(dst_drs_data) %>%
  
  # To calculate the percentage DST coverage we need to identify the greater of the two numerators
  # Set numerator to NA if the denominator is NA for a country-year
  mutate(
    dst_num = ifelse(year == 2017 & iso3 == "ZAF", dst_notif_num,
                     ifelse(year >= 2017, dst_drs_num,
                            ifelse(NZ(dst_denom) == 0, NA,
                                   ifelse((is.na(dst_notif_num) & !is.na(dst_drs_num)) |
                                            (NZ(dst_drs_num) >= NZ(dst_notif_num)),
                                          dst_drs_num,
                                          ifelse((!is.na(dst_notif_num) & is.na(dst_drs_num)) |
                                                   (NZ(dst_notif_num) >= NZ(dst_drs_num)),
                                                 dst_notif_num, NA))))),
    dst_denom = ifelse(year == 2017 & iso3 == "ZAF", dst_denom,
                       ifelse(year >= 2017, dst_drs_denom, dst_denom))
    
  ) %>%
  
  # Drop unwanted variables
  select(iso3,
         year,
         g_whoregion,
         dst_num,
         dst_denom) %>%
  
  # Drop rows with empty numerators or denominator
  filter(!is.na(dst_num) | !is.na(dst_denom))


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
  mutate( entity = ifelse(entity == "Africa", "Africa\u1d47", entity))

# Change the order
dst_agg$entity <- factor(dst_agg$entity,
                         levels = c("Africa\u1d47", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific", "Global"))


# Plot as lines
dst_plot <- dst_agg %>%
  ggplot(aes(x=year, y=dst_pcnt, ymin=0)) +
  geom_line(size=1) +
  
  facet_wrap( ~ entity, ncol = 4, scales="fixed") +
  scale_x_continuous(breaks = c(2009, 2014, report_year-1)) +
  scale_y_continuous(name = "Percentage of tested") +
  expand_limits(y=c(0,100)) +
  xlab("Year") +
  
  ggtitle(paste0("Figure 4.13\nPercentage of bacteriologically confirmed TB cases tested for RR-TB\u1d43, globally and for WHO regions, 2009-",
                 report_year-1)) +
  
  theme_glb.rpt() +
  theme(legend.position="top",
        legend.title=element_blank())

# Add explanatory footnotes
dst_footnote <- "\u1d43 Includes both new and previously treated cases; data for 2017 and 2018 are for pulmonary cases only.\n\u1d47 The increase in the African Region from 2014 to 2015 was due to a large increase in reporting of laboratory results for cases in South Africa in 2015."

dst_plot <- arrangeGrob(dst_plot, bottom = textGrob(dst_footnote, x = 0, hjust = 0, vjust=0.1, gp = gpar(fontsize = 9)))

# Save the plot
figsavecairo(dst_plot, dst_agg, dst_footnote,"f4_13_dst_aggregates")

# Clean up (remove any objects with their name containing 'dst_')
rm(list=ls(pattern = "dst_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.14  (Map) ------
# Percentage of bacteriologically confirmed TB cases tested for RR-TB, 2018
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Using indicator in dr_surveillance started collecting from 2017 to only include pulmonary cases, but per Katherine's reuqest, 
# don't add the word "pulmonary" to the title.

dst_data <- dr_surveillance %>%
  filter(year >= report_year - 2) %>%
  select(year,
         country,
         iso3,
         pulm_labconf_new,
         pulm_labconf_ret,
         r_rlt_new,
         r_rlt_ret) %>%
  
  # Calculate coverage of DST percentages
  mutate(# percent DST among lab-confirmed cases is a bit of a fudge:
    # numerator rdst_new + rdst_ret  (ignore cases with unknown treatment history)
    # denominator is a bit of a fudge: new_labconf + c_ret
    dst_pct = ifelse((NZ(pulm_labconf_new) + NZ(pulm_labconf_ret)) == 0 |
                       is.na(r_rlt_new) & is.na(r_rlt_ret), NA,
                     (NZ(r_rlt_new) + NZ(r_rlt_ret)) * 100 /
                       (NZ(pulm_labconf_new) + NZ(pulm_labconf_ret)))
  )

dst_data$cat <- cut(dst_data$dst_pct,
                    c(0, 20, 50, 80, Inf),
                    c('0-19', '20-49', '50-79', '\u226580'),
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
                        paste0("Figure 4.14\nPercentage of bacteriologically confirmed TB cases tested for RR-TB, ", report_year-1, "\u1d43"),
                        "Percentage",
                        copyright=FALSE,
                        colors=brewer.pal(4, "YlOrBr"),
                        background="White",
                        show=FALSE)


# Add footnote about testing and also about using earlier data for some countries
dst_foot <- paste("\u1d43 Includes both new and previously treated cases; data are for pulmonary cases only.",
                  report_year - 2,
                  "data were used for",
                  nrow(dst_prev_year_data),
                  "countries.")



dst_map <- arrangeGrob(dst_map, bottom = textGrob(dst_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 9)))


figsavecairo(dst_map,
             select(dst_data_combined,
                    iso3,
                    country,
                    dst_pct,
                    cat),
             dst_foot,
             "f4_14_dst_map")

# Clean up (remove any objects with their name beginning with 'dst')
rm(list=ls(pattern = "^dst"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.15 ------
# Global number of MDR/RR-TB cases detected (green) and number enrolled on MDR-TB treatment (purple) 2009–2018,
# compared with estimate for 2018 of the number of incident cases of MDR/RR-TB (uncertainty interval shown in blue)
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
                         e_inc_rr_num_hi) %>%
                  mutate(e_inc_rr_num    = e_inc_rr_num    / 1000,
                         e_inc_rr_num_lo = e_inc_rr_num_lo / 1000,
                         e_inc_rr_num_hi = e_inc_rr_num_hi / 1000)
                        

rr_global <- rr_global %>%
             left_join(rr_global_inc)


# Plot as lines
rr_plot <-  rr_global %>%
            ggplot(aes(x=year, y=c_rrmdr, ymin=0)) +
            geom_line(size=1, colour = "Green") +
            geom_line(aes(year, enrolled),
                      size=1,
                      colour="Purple") +

            # Add estimated incidence
            geom_errorbar(aes(ymin=e_inc_rr_num_lo,
                              ymax=e_inc_rr_num_hi),
                          width=0.2, size=1, color="blue") +

            geom_point(aes(x=year, y=e_inc_rr_num),
                       shape = 21, colour = "blue", fill = "white", size = 5, stroke = 2) +



            scale_x_continuous(name = "Year", breaks = c(2010, 2012, 2014, 2016, report_year-1)) +
            scale_y_continuous(name = "Number of cases (thousands)") +


            ggtitle(paste0("FIG.4.15\nGlobal number of MDR/RR-TB cases detected (green) and number enrolled on MDR-TB treatment (purple), 2009-",
                         report_year-1,
                         ",\ncompared with estimate for ",
                          report_year-1,
                         " of the number of incident cases of MDR/RR-TB (uncertainty interval shown in blue)"
                         )) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsavecairo(rr_plot, rr_global,NA, "f4_15_drtb_detect_enroll_global")

# Clean up (remove any objects with their name beginning with 'rr_')
rm(list=ls(pattern = "^rr_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.16 ------
# Number of MDR/RR-TB cases detected (green) and enrolled on MDR-TB treatment (purple) 2009–2018,
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
                    g_whoregion,
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
            geom_line(size=1, colour = "Green") +
            geom_line(aes(year, enrolled),
                      size=1,
                      colour="Purple") +

            facet_wrap( ~ country, scales="free_y", ncol = 5) +

            scale_x_continuous(name = "Year", breaks = c(2010, 2014, report_year-1)) +
            scale_y_continuous(name = "Number of cases") +


            ggtitle(paste0("FIG.4.16\nNumber of MDR/RR-TB cases detected (green) and enrolled on MDR-TB treatment\n(purple), 2009-",
                         report_year-1,
                         ", 30 high MDR-TB burden countries")) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  strip.text.x = element_text(size=7))  #reduce font size of labels above each panel


# Save the plot
figsavecairo(rr_plot, rr_data,NA,"f4_16_drtb_detect_enroll_hbc", width=7, height=11)

# Clean up (remove any objects with their name beginning with 'rr_')
rm(list=ls(pattern = "^rr_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.17  (map) ------
# Percentage of MDR/RR-TB cases tested for susceptibility to second-line drugs, 2018
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
                      c(0, 20, 50, 80, Inf),
                      c('0-19', '20-49', '50-79', '\u226580'),
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
                          paste0("FIG.4.17\nPercentage of MDR/RR-TB cases tested for susceptibility to second-line drugs, ",
                                    report_year-1),
                                 "Percentage",
                                 copyright=FALSE,
                                 background="White",
                                 colors=brewer.pal(4, "Blues"),
                                 show=FALSE)

# Add footnote about using earlier data for some countries, removed in 2019
# sldst_foot <- paste("\u1d43 ",
                    report_year - 2,
                    "data were used for",
                    nrow(sldst_prev_year_data),
                    "countries.")

# sldst_map <- arrangeGrob(sldst_map, bottom = textGrob(sldst_foot, x = 0, hjust = -0.1, vjust=-1.5, gp = gpar(fontsize = 10)))


figsavecairo(sldst_map,
             select(sldst_data_combined,
                    iso3,
                    country,
                    pcnt_sldst,
                    cat),
             NA,
             "f4_17_pct_sldst_map")

# Clean up (remove any objects with their name beginning with 'sldst')
rm(list=ls(pattern = "^sldst"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.18   ------
# Case notification rates (new and relapse cases, all forms) (black) compared with estimated TB incidence rates,
# (green), 2000-2018, 30 high TB burden countries
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
                #add markers for India and other countries with pending survey results footnote
                mutate(country = recode(country, "India"="India\u1d43 ",
                                                 "Mozambique"="Mozambique\u1d47 ",
                                                 "South Africa"="South Africa\u1d47 "))

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
            scale_x_continuous(breaks = c(2000, 2009, report_year-1)) +
            scale_y_continuous(name = "Rate per 100 000 population per year") +
            xlab("Year") +

            facet_wrap( ~ country,
                        scales = "free_y",
                        ncol = 5) +

            ggtitle(paste0("FIG.4.18\nCase notification rates (new and relapse cases, all forms) (black)\ncompared with estimated TB incidence rates (green), 2000-",
                         report_year-1,
                         ",\n30 high TB burden countries. Shaded areas represent uncertainty bands.")) +
            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  strip.text.x = element_text(size=8))  #reduce font size of labels above each panel

# Add India and other countries footnotes

pending_incidence_footnote<- "Estimates of TB incidence for Lesotho, Mozambique and South Africa will be reviewed after final results from their \nrespective national TB prevalence surveys are available in 2020."

inc_foot <- paste("\u1d43 ",
                  india_incidence_footnote,"\n\u1d47 ",pending_incidence_footnote)

inc_plot <- arrangeGrob(inc_plot,
                        bottom = textGrob(inc_foot,
                                          x = 0.02,
                                          just = "left",
                                          gp = gpar(fontsize = 6)))
# Save the plot
figsavecairo(inc_plot, inc_data, inc_foot,"f4_18_inc_plot_hbc", width=7.5, height=11)

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.19   ------
# Estimated TB treatment coverage (new and relapse patients as a percentage of estimated TB incidence)
# in 2018, 30 high TB burden countries, WHO regions and globally
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
  #add marker for India and other countries footnote
  mutate(entity = recode(entity, "India"="India\u1d47 ",
                         "Mozambique"="Mozambique\u1d43 ",
                         "South Africa"="South Africa\u1d43 ",
                         "Lesotho"="Lesotho\u1d43 "))

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
       title=paste0("FIG.4.19\nEstimated TB treatment coverage (new and relapse patients as a percentage of estimated TB incidence)\n in ",
                    report_year - 1,
                    ", 30 high TB burden countries, WHO regions and globally")) +
  geom_pointrange(aes(ymin=c_cdr_lo,
                      ymax=c_cdr_hi)) +
  theme_glb.rpt() +
  theme(plot.title = element_text(hjust = 0)) +
  expand_limits(y=0) +
  coord_flip()

# Add footnotes
coverage_pending_incidence_footnote <- "Estimates of TB incidence for Lesotho, Mozambique and South Africa will be reviewed after final \nresults from their respective national TB prevalence surveys are available in 2020."


coverage_footnote <- paste("\u1d43 ",
                           coverage_pending_incidence_footnote,"\n\u1d47 ",india_incidence_footnote)
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
figsavecairo(coverage_plot, coverage_data,coverage_footnote, "f4_19_txcoverage_tb")

# Clean up (remove any objects with their name starting with 'coverage')
rm(list=ls(pattern = "^coverage"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.20  (map with bubbles) ------
# The ten countries with the largest gaps between notifications of new and relapse
# (incident) TB cases and the best estimates of TB incidence, 2018
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
                          paste0("FIG.4.20\n",
                                 "The ten countries with the largest gaps between notifications of new and relapse\n",
                                 "(incident) TB cases and the best estimates of TB incidence, ",
                                 report_year - 1,"\u1d43 "),
                          bubble_colour = "purple",
                          background = "white",
                          scale_breaks = c(70000,500000,1000000),
                          scale_limits = c(70000,1100000),
                          scale_labels = c("70 000","500 000","1 000 000"),
                          bubble_label_show_number = 10)

# Generate names for footnote
gap_ten_countries_name_by_rank <- gap_data  %>%
  arrange(desc(bubble_size))   %>%
  select(country)

gap_India_SF_foot <- "Estimates of TB incidence for India are interim, pending results from the national TB prevalence survey planned for 2019/2020. Estimate of \nTB incidence for South Africa will be reviewed after final resultS from its national TB prevalence survey is available in 2020."

gap_foot <- paste0("\u1d43 The ten countries ranked in order of the size of the gap between notified cases and the best estimates of TB incidence in ",report_year-1," are \n",
                   sapply(gap_ten_countries_name_by_rank,paste, collapse=", "),".","\n",gap_India_SF_foot)

gap_map <- arrangeGrob(gap_map,
                       bottom = textGrob(gap_foot,
                                         x = 0,
                                         hjust = -0.1,
                                         vjust=0.4,
                                         gp = gpar(fontsize = 10)))
# Save the plot
figsavecairo(gap_map,
             select(gap_data,
                    iso3,
                    country,
                    bubble_size),
             gap_foot,
             "f4_20_top_10_gap_map")

# Clean up (remove any objects with their name beginning with 'gap')
rm(list=ls(pattern = "^gap"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.21   ------
# Number of new and relapse cases known to be HIV-positive (black)
# and number started on ART (blue) compared with estimated number of incident HIV-positive TB cases (red),
# 2004-2018, 30 high TB/HIV burden countries
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
                  get_names_for_tables()%>%
                  #add markers for India and other countries with pending survey resluts footnote
                  mutate(country = recode(country, "Eswatini"="Eswatini\u1d47 ",
                                                   "South Africa"="South Africa\u1d47 ",
                                                   "Lesotho"="Lesotho\u1d47 ",
                                                   "Mozambique"="Mozambique\u1d47 ",
                                                   "India"="India\u1d9c "))

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

                  scale_x_continuous(breaks = c(2004, 2011, report_year-1)) +

                  scale_y_continuous(name = "New and relapse cases per year (thousands)") +
                  xlab("Year") +

                  facet_wrap( ~ country,
                              scales = "free_y",
                              ncol = 5) +

                  ggtitle(paste0("FIG.4.21\nNumber of new and relapse cases\u1d43 known to be HIV-positive (black) and\nnumber started on ART (blue) compared with estimated number of \nincident HIV-positive TB cases (red), 2004-",
                               report_year-1,
                               ", 30 high TB/HIV burden countries")) +

                  theme_glb.rpt() +
                  theme(strip.text.x = element_text(size=8))  #reduce font size of labels above each panel

# Add footnote
inctbhiv_foot <- "\u1d43 The calculation is for all cases in years prior to 2015."
pending_incidence_footnote <- " Estimates of TB incidence for Eswatini, Lesotho, Mozambique and South Africa will be reviewed after final \nresults from their respective national TB prevalence surveys are available in 2020."
india_incidence_footnote <- " Estimates of TB incidence for India are interim, pending results from the national TB prevalence survey planned for 2019/2020."

inctbhiv_foot <- paste(inctbhiv_foot,
                       "\n\u1d47",
                       pending_incidence_footnote,
                       "\n\u1d9c",
                       india_incidence_footnote)
  
inctbhiv_plot <- arrangeGrob(inctbhiv_plot, bottom = textGrob(inctbhiv_foot,
                                                              x = 0,
                                                              hjust = -0.1,
                                                              vjust=0.1,
                                                              gp = gpar(fontsize = 6)))


# Save the plot
figsavecairo(inctbhiv_plot, inctbhiv_data,inctbhiv_foot, "f4_21_inctbhiv_plot_hbc",  width=7, height=11)

# Clean up (remove any objects with their name containing 'tbhiv')
rm(list=ls(pattern = "tbhiv"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.22   ------
# Estimated ART treatment coverage for HIV-positive TB cases
# (HIV-positive TB patients on ART as a percentage of the estimated incidence of HIV-positive TB)
# in 2018, 30 high TB/HIV burden countries, WHO regions and globally
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
                              entity = ifelse(is.na(newrel_art), paste0(entity, "\u1d43 "), entity )) %>%
                       select(entity,
                              c_art,
                              c_art_lo,
                              c_art_hi) %>%
                       arrange(desc(c_art))

# Calculate how many countries highlighted as having no data
coveragehiv_nodata_count <- coveragehiv_country %>%
                            filter(grepl("[\u1d43 ]$", entity)) %>%
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
                         title=paste0("FIG.4.22\nEstimated ART treatment coverage for HIV-positive TB cases\n",
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
  {coveragehiv_foot <- "\u1d43  No data"
  coveragehiv_plot <- arrangeGrob(coveragehiv_plot,
                                  bottom = textGrob(coveragehiv_foot,
                                                    x = 0,
                                                    hjust = -0.1,
                                                    vjust=0,
                                                    gp = gpar(fontsize = 10)))
}

# Save the plot
figsavecairo(coveragehiv_plot, coveragehiv_data, coveragehiv_foot, "f4_22_txcoverage_tbhiv")

# Clean up (remove any objects with their name starting with 'coveragehiv')
rm(list=ls(pattern = "^coveragehiv"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.23  ------
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
       title=paste0("Figure 4.23\nEstimated treatment coverage for MDR/RR-TB (patients started on treatment for MDR-TB as a percentage \nof the estimated incidence of MDR/RR-TB) in ",
                    report_year - 1,
                    " ,30 high MDR-TB burden countries, WHO regions and globally\u1d43")) +
  geom_pointrange(aes(ymin=c_rr_coverage_lo,
                      ymax=c_rr_coverage_hi)) +
  theme_glb.rpt() +
  theme(plot.title = element_text(hjust = 0)) +
  expand_limits(y=0) +
  coord_flip()

# If there are countries with no data then add a footnote
if (coveragerr_nodata_count > 0)
{
  coveragerr_nodata_foot <- "* No data"
}


# Add a footnote for over 100% coverage if there are such countries
if (as.numeric(any(coveragerr_data$c_rr_coverage>100)))
{
  coveragerr_over100_foot <- paste0("\u1d43 Reasons for a higher than expected coverage (even exceeding 100%) include that the numerator included empirical treatment of TB patients considered at risk of \nhaving MDR/RR-TB but for whom a laboratory-confirmed diagnosis was missing, incomplete reporting of laboratory data, duplicated case reporting, or enrolment of ‘waiting lists’ of people \nwith MDR/RR-TB who were detected before ", report_year-1, "." )
}

coveragerr_foot <- ifelse(coveragerr_nodata_count > 0 & as.numeric(any(coveragerr_data$c_rr_coverage>100)),
                          paste0(coveragerr_nodata_foot,coveragerr_over100_foot),
                          ifelse (coveragerr_nodata_count > 0 & !as.numeric(any(coveragerr_data$c_rr_coverage>100)),
                                  coveragerr_nodata_foot,
                                  ifelse (as.numeric(any(coveragerr_data$c_rr_coverage>100)) & coveragerr_nodata_count == 0,
                                          coveragerr_over100_foot, "")))

coveragerr_plot <- arrangeGrob(coveragerr_plot, bottom = textGrob(coveragerr_foot,
                                                                  x = 0,
                                                                  hjust = -0.1,
                                                                  vjust=0.3,
                                                                  gp = gpar(fontsize = 9)))

# Save the plot
figsavecairo(coveragerr_plot, coveragerr_data, coveragerr_foot, "f4_23_txcoverage_drtb")

# Clean up (remove any objects with their name starting with 'coveragerr')
rm(list=ls(pattern = "^coveragerr"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.24  (map with bubbles) ------
# The ten countries with the largest gaps between the number of patients started
# on treatment for MDR-TB and the best estimates of MDR/RR-TB incidence, 2018
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
  filter(!is.na(unconf_rrmdr_tx) | !is.na(conf_rrmdr_tx)) %>%
  
  # Calculate the gap and use that for the bubble sizes
  mutate(bubble_size = e_inc_rr_num - (NZ(unconf_rrmdr_tx) + NZ(conf_rrmdr_tx))) %>%
  
  # Modify long names of countries which will be shown as labels in map
  mutate(country = recode(country, "Democratic Republic of the Congo"="DR Congo")) %>%
  
  # limit to the top 10 by size of gap
  top_n(10, bubble_size)

# Plot the gaps as bubbles

drgap_map <- who_bubble_map(drgap_data,
                            paste0("FIG.4.24\n",
                                   "The ten countries with the largest gaps between the number of patients started\n",
                                   "on treatment for MDR-TB and the best estimates of MDR/RR-TB incidence, ",
                                   report_year - 1,"\u1d43 "),
                            bubble_colour = "green",
                            background = "white",
                            scale_breaks = c(5000,50000,100000),
                            scale_limits = c(5000,130000),
                            scale_labels = c("5 000","50 000","100 000"),
                            bubble_label_show_number = 10)

# Generate names for footnote
drgap_ten_countries_name_by_rank <- drgap_data  %>%
  arrange(desc(bubble_size))   %>%
  select(country)

drgap_foot <- paste0("\u1d43 The ten countries ranked in order of the size of the gap between the number of patients started on MDR-TB treatment and the best estimate of MDR/RR-TB \nincidence in ",report_year-1," are ", sapply(drgap_ten_countries_name_by_rank,paste, collapse=", "),".")

drgap_map <- arrangeGrob(drgap_map,
                         bottom = textGrob(drgap_foot,
                                           x = 0,
                                           hjust = -0.1,
                                           vjust=0.4,
                                           gp = gpar(fontsize = 10)))

# Save the plot
figsavecairo(drgap_map,
             select(drgap_data,
                    iso3,
                    country,
                    bubble_size),
             drgap_foot,
             "f4_24_top_10_dr_gap_map")

# Clean up (remove any objects with their name beginning with 'drgap')
rm(list=ls(pattern = "^drgap"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.25   ------
# Treatment outcomes for new and relapse TB cases in 2017,
# 30 high TB burden countries, WHO regions and globally
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
                                         paste0(entity, "\u1d43 "),
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
			  labs(x="", y="Percentage of cohort") +

			  theme(legend.position="bottom",
			        panel.grid=element_blank()) +

			  expand_limits(c(0,0)) +

			  ggtitle(paste0("FIG.4.25\nTreatment outcomes for new and relapse TB cases in ",
			                 report_year - 2,
			                 ",\n30 high TB burden countries, WHO regions and globally")) +

			  geom_text(data=subset(txout_long,variable=="Treatment success"),aes(label = round(value, digits = 0)),
			            position = position_stack(reverse = TRUE), size=3,hjust=1.5,color="white")

txout_foot <- "\u1d43 Treatment outcomes are for new cases only."

txout_plot <- arrangeGrob(txout_plot,
                          bottom = textGrob(txout_foot,
                                            x = 0,
                                            hjust = -0.1,
                                            vjust=0,
                                            gp = gpar(fontsize = 10)))

figsavecairo(txout_plot, txout,txout_foot, "f4_25_outcomes_tb", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txout')
rm(list=ls(pattern = "^txout"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.26 ------
# Treatment outcomes for new and relapse TB cases,
# new and relapse cases HIV-positive TB cases, and
# MDR/RR-TB cases, 2012-2017 globally
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
                       country,
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

			  labs(x="Year started on treatment", y="Percentage of cohort") +

        scale_x_continuous(breaks = c(2012,2013,2014,2015,2016,2017)) +

			  theme(legend.position="bottom",
			        panel.grid=element_blank()) +

			  expand_limits(c(0,0)) +

			  ggtitle(paste0("FIG.4.26\nTreatment outcomes for new and relapse TB cases,\nnew and relapse HIV-positive TB cases,\nand MDR/RR-TB cases, 2012-",
			                 report_year - 2,
			                 " globally\u1d43 ")) +

			  geom_text(data=subset(out_data_long,variable=="Treatment success"),aes(label = round(value, digits = 0)),
			            position = position_stack(reverse = TRUE), size=3,hjust=1.5,color="white")

out_foot <- "\u1d43 Outcomes for MDR/RR-TB annual treatment cohorts are reported one year later than other TB cohorts."
  
out_plot <- arrangeGrob(out_plot,
                        bottom = textGrob(out_foot,
                                          x = 0,
                                          hjust = -0.1,
                                          vjust=0,
                                          gp = gpar(fontsize = 10)))

figsavecairo(out_plot, out_data, out_foot, "f4_26_outcomes_tb_hiv_mdr", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'out_')
rm(list=ls(pattern = "^out_"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.27  ------
# Treatment outcomes for new and relapse TB cases (absolute numbers), 2000 - 2017,
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

                  scale_x_continuous(breaks = c(2000,2008,report_year-2)) +

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

                  scale_x_continuous(breaks = c(2000,2004,2008,2012,report_year-2)) +

                    theme(legend.position="none",
                        panel.grid=element_blank()) +

                  expand_limits(c(0,0))

txoutnum_foot <- "\u1d43 Cohorts before 2012 included new cases only."

txoutnum_plot <- arrangeGrob(txoutnum_plot_glob,
                             txoutnum_plot_reg,
                             nrow = 2,
                             ncol = 1,
                             top = textGrob(label = paste0("FIG.4.27\nTreatment outcomes for new and relapse TB cases\u1d43 (absolute numbers), 2000-",
                                                           report_year-2,
                                                           ", globally\nand for WHO regions"),
                                            x = 0.02,
                                            just = "left",
                                            gp = gpar(fontsize = 10)),

                             bottom = textGrob(txoutnum_foot,
                                               x = 0.02,
                                               just = "left",
                                               gp = gpar(fontsize = 10)))


# Save the plot
figsavecairo(txoutnum_plot, txoutnum_data, txoutnum_foot, "f4_27_outcomes_absolute", width=7, height=11)

# Clean up (remove any objects with their name starting 'txout')
rm(list=ls(pattern = "^txoutnum"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.28   ------
# Treatment outcomes for new and relapse HIV-positive TB cases in 2017,
# 30 high TB/HIV burden countries, WHO regions and globally
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
					  labs(x="", y="Percentage of cohort") +

					  theme(legend.position="bottom",
					        panel.grid=element_blank()) +

					  expand_limits(c(0,0)) +

					  ggtitle(paste0("FIG.4.28\nTreatment outcomes for new and relapse HIV-positive TB cases in\n",
					                 report_year - 2,
					                 ", 30 high TB/HIV burden countries, WHO regions and globally")) +

					  geom_text(data=subset(txtbhivout_long,variable=="Treatment success"),aes(label = round(value, digits = 0)),
					            position = position_stack(reverse = TRUE), size=3,hjust=1.5,color="white")


# Save the plot
figsavecairo(txtbhivout_plot, txtbhivout, NA, "f4_28_outcomes_tbhiv", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txtbhivout')
rm(list=ls(pattern = "^txtbhivout"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.29   ------
# Treatment outcomes for MDR/RR-TB cases started on treatment in 2016,
# 30 high MDR-TB burden countries, WHO regions and globally
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
  arrange(entity) %>%
  # add call out to countries whose MDR cohort were less than 500
  mutate(entity = ifelse(!is.na(mdr_coh) & NZ(mdr_coh) < 500, paste0(entity, "\u1d43 "), entity))

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
  labs(x="", y="Percentage of cohort") +

  theme(legend.position="bottom",
        panel.grid=element_blank()) +

  expand_limits(c(0,0)) +

  ggtitle(paste0("FIG.4.29\nTreatment outcomes for MDR/RR-TB cases\nstarted on treatment in ",
                 report_year - 3,
                 ",\n30 high MDR-TB burden countries, WHO regions and globally")) +

  geom_text(data=subset(txmdrout_long,variable=="Treatment success"),aes(label = round(value, digits = 0)),
            position = position_stack(reverse = TRUE), size=3,hjust=1.5,color="white")

# Add explanatory footnotes
txmdrout_foot <- paste0("\u1d43 These countries reported cohorts of less than 500 MDR/RR-TB cases in ",
                        report_year - 3, ".")

txmdrout_plot <- arrangeGrob(txmdrout_plot,
                             bottom = textGrob(txmdrout_foot,
                                               x = 0,
                                               hjust = -0.1,
                                               vjust=0.7,
                                               gp = gpar(fontsize = 8)))

figsavecairo(txmdrout_plot, txmdrout, txmdrout_foot, "f4_29_outcomes_mdr", width=7, height=11) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'txmdrout')
rm(list=ls(pattern = "^txmdrout"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.30  (map) ------
# Countries that used shorter MDR–TB treatment regimens by the end of 2018
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

short_data <- notification %>%
              filter(year == report_year - 1) %>%
              select(country,
                     iso3,
                     mdr_shortreg_used) %>%
              mutate(cat = ifelse(mdr_shortreg_used == 3, "No data",
                            ifelse(mdr_shortreg_used == 0, "Not used",
                            ifelse(mdr_shortreg_used == 1, "Used", NA))))

short_data$cat <-factor(short_data$cat)

short_data$cat <- factor(short_data$cat,levels = rev(levels(short_data$cat)))

# produce the map
short_map<- WHOmap.print(short_data,
                         paste0("FIG.4.30\nCountries that used shorter MDR–TB treatment regimens by the end of ", report_year - 1),
                         legend.title = "Country \nresponse",
                         copyright=FALSE,
                         colors=c("darkgreen", "lightgreen", "white"),
                         na.label="No data",
                         background="White",
                         show=FALSE)

figsavecairo(short_map,
        short_data,
        NA,
        "f4_30_short_regimen_map")


# Clean up (remove any objects with their name beginning with 'short')
rm(list=ls(pattern = "^short"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.31  (map) ------
# Countries that used bedaquiline for the treatment of M/XDR–TB as part of expanded access,
# compassionate use or under normal programmatic conditions by the end of 2018
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bdq_data <- notification %>%
            filter(year == report_year - 1) %>%
            select(country,
                   iso3,
                   mdrxdr_bdq_used) %>%
            mutate(cat = ifelse(mdrxdr_bdq_used == 1, "Used",
                                ifelse(mdrxdr_bdq_used == 0, "Not used",
                                       ifelse(mdrxdr_bdq_used == 3, "No data", NA)))) %>%

            # drop unnecessary variables
            select(country, iso3, cat)


bdq_data$cat <- factor(bdq_data$cat)

bdq_data$cat <- factor(bdq_data$cat,levels(bdq_data$cat)[c(3,2,1)])

# produce the map
bdq_map<- WHOmap.print(bdq_data,
                       paste0("FIG.4.31\nCountries that used bedaquiline for the treatment of MDR/XDR–TB as part of expanded access,\ncompassionate use or under normal programmatic conditions by the end of ", report_year - 1),
                       legend.title = "Country \nresponse",
                       copyright=FALSE,
                       colors=c("darkgreen", "lightgreen", "white"),
                       na.label="No data",
                       background="White",
                       show=FALSE)

figsavecairo(bdq_map,
        bdq_data,
        NA,
        "f4_31_bdq_map")


# Clean up (remove any objects with their name beginning with 'bdq')
rm(list=ls(pattern = "^bdq"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.32  (map) ------
# Countries that used delamanid for the treatment of M/XDR–TB as part of expanded access,
# compassionate use or under normal programmatic conditions by the end of 2018
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


dlm_data <- notification %>%
            filter(year == report_year - 1) %>%
            select(country,
                   iso3,
                   mdrxdr_dlm_used) %>%
            mutate(cat = ifelse(mdrxdr_dlm_used == 1, "Used",
                                ifelse(mdrxdr_dlm_used == 0, "Not used",
                                       ifelse(mdrxdr_dlm_used == 3, "No data", NA)))) %>%

            # drop unnecessary variables
            select(country, iso3, cat)

dlm_data$cat <- factor(dlm_data$cat)

dlm_data$cat <- factor(dlm_data$cat,levels(dlm_data$cat)[c(3,2,1)])

# produce the map
dlm_map<- WHOmap.print(dlm_data,
                       paste0("FIG.4.32\nCountries that used delamanid for the treatment of MDR/XDR–TB as part of expanded access,\ncompassionate use or under normal programmatic conditions by the end of ", report_year - 1),
                       legend.title = "Country \nresponse",
                       copyright=FALSE,
                       colors=c("darkgreen", "lightgreen", "white"),
                       na.label="No data",
                       background="White",
                       show=FALSE)


figsavecairo(dlm_map,
        dlm_data,
        NA,
        "f4_32_dlm_map")


# Clean up (remove any objects with their name beginning with 'dlm')
rm(list=ls(pattern = "^dlm"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 4.3.1 ------
# Contribution of public–public mix to TB case notifications in eight countries, 2012–2018
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get PPM data
ppm_data <- strategy %>%
            select(iso2, year, country, pub_new_dx) %>%
            filter(year >= 2012 & iso2 %in% c('AF', 'BD', 'CN', 'IN', 'ID', 'PH', 'TH', 'VN'))

# Merge with notifications
ppm_data <- notification %>%
            select(iso2, year, c_notified) %>%
            inner_join(ppm_data)

# Calculate percent contributions
ppm_data <- ppm_data %>%
            mutate(public_pcnt = ifelse(!is.na(pub_new_dx) & c_notified > 0,
                                        pub_new_dx * 100 / c_notified,
                                        NA))

# Plot as lines
ppm_plot <- ppm_data %>%
            ggplot(aes(x=year, y=public_pcnt, ymin=0)) +
            geom_line(size=1) +

            facet_wrap(~ country, ncol = 4, scales = "free_y") +

            scale_x_continuous(name="Year",
                               breaks = c(2012, 2015, report_year-1)) +

            scale_y_continuous(name = "Contribution of public-public mix to total notifications (%)") +

            ggtitle(paste0("FIG.B4.3.1\nContribution of public-public mix to TB case notifications in eight countries, 2012-",
                           report_year-1)) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  axis.text.x = element_text(size=5))

# Save the plot
figsavecairo(ppm_plot,
        select(ppm_data,
               country,
               year,
               public_pcnt),
        NA,
        "f4_box_4_3_1_public_public_contributions")

# Clean up (remove any objects with their name beginning 'ppm_')
rm(list=ls(pattern = "^ppm_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 4.3.2 ------
# Contribution of public–private mix to TB case notifications in eight countries, 2012–2018
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get PPM data
ppm_data <- strategy %>%
            select(iso2, year, country, priv_new_dx) %>%
            filter(year >= 2012 & iso2 %in% c('BD', 'ET', 'IN', 'ID', 'KE', 'MM', 'PK', 'PH'))

# Merge with notifications
ppm_data <- notification %>%
            select(iso2, year, c_notified) %>%
            inner_join(ppm_data)

# Calculate percent contributions
ppm_data <- ppm_data %>%
            mutate(private_pcnt = ifelse(!is.na(priv_new_dx) & c_notified > 0,
                                        priv_new_dx * 100 / c_notified,
                                        NA))

# Plot as lines
ppm_plot <- ppm_data %>%
            ggplot(aes(x=year, y=private_pcnt, ymin=0)) +
            geom_line(size=1) +

            facet_wrap(~ country, ncol = 4, scales = "free_y") +

            scale_x_continuous(name="Year",
                               breaks = c(2012, 2015,report_year-1)) +

            scale_y_continuous(name = "Contribution of public-private mix to total notifications (%)") +

            ggtitle(paste0("FIG.B4.3.2\nContribution of public-private mix to TB case notifications in eight countries, 2012-",
                           report_year-1)) +

            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  axis.text.x = element_text(size=5))

# Save the plot
figsavecairo(ppm_plot,
        select(ppm_data,
               country,
               year,
               private_pcnt),
        NA,
        "f4_box_4_3_2_public_private_contributions")

# Clean up (remove any objects with their name beginning 'ppm_')
rm(list=ls(pattern = "^ppm_"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 4.4.1  (Map) ------
# Countries with national case-based surveillance for TB
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
                      paste0("FIG.B4.4.1\nCountries with national case-based surveillance for TB, ",
                             report_year - 1),
                      legend.title = "Country\nresponse",
                      copyright=FALSE,
                      brewer.pal(3, "Blues"),
                      na.label="No response",
                      background="White",
                      show=FALSE)

# Add footnote about using earlier data for some countries, removed in 2019
# cb_foot <- paste("\u1d43 Responses from ",
                 report_year -1,
                 " were used for ",
                 nrow(cb_prev_year_data),
                 "countries.")

# cb_map <- arrangeGrob(cb_map,
                      bottom = textGrob(cb_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


figsavecairo(cb_map,
             select(cb_data_combined,
                    iso3,
                    country,
                    cat),
             NA,
             "f4_box_4_4_1_casebased_map")


# Clean up (remove any objects with their name beginning with 'cb')
rm(list=ls(pattern = "^cb"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 4.6.1  (Map) ------
# Percentage of basic management units in which there is community contribution to new case finding
# and/or to treatment adherence support, 2018
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

# Define countries which has not been requested for data as their persentage=1000, just to creat a new category, removed in 2019
#comm_data <- merge(comm_datarequest, comm_bmu, by= "country")%>%
#  mutate(comm_pct=replace(comm_pct,(dc_engage_community_display == 0),1000))


comm_data$cat <- cut(comm_data$comm_pct,
                     c(0, 25, 50, 75, Inf),
                     c('0-24', '25-49', '50-74', '\u226575'),
                     right=FALSE)

# produce the map
comm_map <- WHOmap.print(comm_data,
                         paste("Fig.B4.6.1\nPercentage of basic management units in which there was community contribution",
                               "\nto new case finding and/or to treatment adherence support,",
                               report_year-1,"\u1d43 "),
                         "Percentage",
                         copyright=FALSE,
                         background="White",
                         colors=c('#bdd7e7', '#6baed6', '#3182bd', '#08519c'),
                         show=FALSE)
comm_foot <- paste0("\u1d43 Data only requested from ",sum(comm_datarequest$dc_engage_community_display)," countries.")

comm_map <- arrangeGrob(comm_map,
                        bottom = textGrob(comm_foot,
                                          x = 0,
                                          hjust = -0.1,
                                          vjust=-1.5,
                                          gp = gpar(fontsize = 10)))

figsavecairo(comm_map,
        select(comm_data,
               country,
               iso3,
               comm_pct,
               cat),
        comm_foot,
        "f4_box_4_6_1_pct_BMU_community_map")

# Clean up (remove any objects with their name beginning with 'comm')
rm(list=ls(pattern = "^comm"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure Box 4.6.2 ------
# Number of countries reporting on WHO community engagement indicators, 2013-2018
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
         bmu_community_impl,
         community_data_available,
         notified_ref,
         notified_ref_community,
         rxsupport_community_coh,
         rxsupport_community_succ)%>%
  mutate(community_data_available2=ifelse((notified_ref>0 & notified_ref_community>=0) | (rxsupport_community_coh>0 & rxsupport_community_succ>=0),1,NA))%>%
  mutate(bmu_community_impl2=ifelse(bmu_community_impl>0,1,NA))

commureport_sumsince2013 <- commureport_from2013 %>%
  filter(year >= 2013 & year <= report_year) %>%
  select(year,
         g_whoregion,
         community_data_available2,
         bmu_community_impl2)%>%
  group_by(year) %>%
  summarise_at(vars(community_data_available2,bmu_community_impl2),
               sum,
               na.rm = TRUE) %>%
  mutate(entity = "Global") %>%
  ungroup()

# Also Lana said numbers has been collected in 2013(data of 2012) by herself through Email so not in the database,
# Thus we need to creat a new row for year 2012 = 13 countries manully.
commureport_2012 <- data.frame(year=2012, entity="Global", community_data_available2=13,bmu_community_impl2=0)

commureport_global <-rbind(commureport_2012,commureport_sumsince2013)

commureport_plot_glob <- commureport_global %>%
  ggplot() +
  geom_bar(mapping=aes(x=year+1, y=bmu_community_impl2),stat="identity", position = "dodge", colour = "black",fill="white",width = 0.5) +
  geom_bar(mapping=aes(x=year+1, y=community_data_available2),stat="identity", fill="darkgreen",width = 0.5) +
  #This breaks setting need to be changed afterwards, it is simply because only 6 years data were used
  #which makes the bar too wide, to control the width, it changed X axis breaks label.
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017,2018,report_year)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  theme_glb.rpt() +
  labs(x="", y="Number of countries") +
  theme(panel.grid=element_blank()) +
  expand_limits(c(0,0)) +
  ggtitle(paste0("FIG.B4.6.2\nNumber of countries reporting implementing any community-based \nactivity in line with WHO community indicators\u1d43 (black line) and \ncountries reporting on these indicators (green), 2013-",
                 report_year))

# Add footnote about only having data from 2015 for eligible countries
commureport_foot <- paste("\u1d43 ","Data has been collected since 2016.")

commureport_plot_glob <- arrangeGrob(commureport_plot_glob, bottom = textGrob(commureport_foot, x = 0, hjust = -0.1, vjust=-1.5, gp = gpar(fontsize = 10)))

# Save the plot
figsavecairo(commureport_plot_glob, commureport_global, commureport_foot, "f4_box_4_6_2_community_indicator_reporting", width=7, height=11)

# Clean up (remove any objects with their name starting 'commureport')
rm(list=ls(pattern = "^commureport"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# TB prevention services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.1 ---------
# Provision of TB preventive treatment to people newly enrolled in HIV care, 2005–2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ipt <- notification %>%
  filter(year >= 2005) %>%
  select(iso3,
         country,
         year,
         g_whoregion,
         hiv_ipt,
         hiv_ipt_reg_all) %>%
  mutate(hiv_ipt_pall=ifelse(!is.na(hiv_ipt_reg_all),hiv_ipt_reg_all,hiv_ipt),
         hiv_ipt_pnew=ifelse(!is.na(hiv_ipt),hiv_ipt,hiv_ipt_reg_all))

#remove data from Kenya, Zambia and Eritrea in 2016 and add data from Russia"
#   ipt[ipt$iso3 %in% c("ZMB", "KEN", "ERI")&ipt$year==2016,"hiv_ipt"]<-NA
#   ipt[ipt$iso3=="RUS"&ipt$year==2016,"hiv_ipt"]<-19611

#checked with Annabele, last year those coUntries were removed because of data discrepancies(also removed from sever database)
#might need to do the same thing this year later.
#Also, she confirmed we can keep data that way in 2016 since those countries still could not provide clean data for 2016


ipt$area <- ifelse(ipt$iso3 %in% c("ZAF"), "South Africa",
                   ifelse(ipt$g_whoregion=="AFR", "Rest of AFR",
                          "Rest of world"))
ipt_b <- aggregate(ipt[7], by=list(year=ipt$year, area=ipt$area), sum, na.rm=T)
ipt_b1 <- aggregate(ipt[7], by=list(year=ipt$year), FUN=sum, na.rm=T)
ipt_b1$area <- "Global"
ipt_b2 <- rbind(ipt_b, ipt_b1)
library(reshape2)
ipt_c <- melt(ipt_b2, id=1:2)
ipt_c$value <- ipt_c$value/1000
ipt_c$area <- factor(ipt_c$area, levels=c( "Rest of world", "Rest of AFR", "South Africa", "Global"))

ipt_d <- aggregate(ipt[5], by=list(year=ipt$year, area=ipt$area), sum, na.rm=T)
ipt_d1 <- aggregate(ipt[5], by=list(year=ipt$year), FUN=sum, na.rm=T)
ipt_d1$area <- "Global"
ipt_d2 <- rbind(ipt_d, ipt_d1)
ipt_e <- melt(ipt_d2, id=1:2)
ipt_e$area <- factor(ipt_e$area, levels=c( "Rest of world", "Rest of AFR", "South Africa", "Global"))
ipt_e <- ipt_e%>%
  mutate(iptnew=value/1000)%>%
  select(iptnew)
ipt_f <- cbind(ipt_c, ipt_e)


#New version as dashed line for 2005-2016,solid line for 2017-2018, plus dashed for global data 2017-2018
ipt_plot <- ggplot() + 
  geom_line(data=subset(ipt_f,year <= 2016),aes(year, iptnew, color=area), size=1,linetype="dashed")+
  geom_line(data=subset(ipt_f,year>2015),aes(year, value, color=area), size=1)+
  geom_line(data=subset(ipt_f,area=="Global"),aes(year, iptnew, color=area), size=1,linetype="dashed")+
  scale_y_continuous("Number of people (thousands)") +
  theme_glb.rpt() + scale_x_continuous(name="", breaks=c(min(ipt_c$year):max(ipt_c$year))) +
  scale_color_manual(values=c("#0070C0", "#77933C","orange", "red")) + guides(color = guide_legend(reverse = TRUE))+
  ggtitle(paste("FIG.5.1 \nProvision of TB preventive treatment to people enrolled in HIV care\u1d43 , 2005", report_year-1, sep="\u2013"))+
  annotate("text", x=2016, y=1200, label="Global", size=4)+
  annotate("text", x=2016, y=300, label="South Africa", size=4)+
  annotate("text", x=2016, y=600, label="Rest of Africa", size=4)+
  annotate("text", x=2014, y=150, label="Rest of World", size=4)+
  theme(legend.position="none")

# Add footnote about including countries report data for all HIV cases;
# This may not be needed every year,so I will just edit the footnote manullay;
ipt_foot <- "\u1d43 Prior to 2017, data were collected for PLHIV newly enrolled in HIV care(dotted lines). \nIn 2017 and 2018, data were also collected for PLHIV currently enrolled in HIV care(solid lines)."

ipt_plot <- arrangeGrob(ipt_plot,
                        bottom = textGrob(ipt_foot,
                                          x = 0,
                                          hjust = -0.1,
                                          vjust=0.1,
                                          gp = gpar(fontsize = 10)))

# Save the plot
figsavecairo(ipt_plot, ipt_c,ipt_foot, "f5_1_IPT_in_HIV_patients", width=7, height=5)

# Clean up (remove any objects with their name starting 'commureport')
rm(list=ls(pattern = "^ipt"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.2   ------
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
                    labels=c("Started on preventive treatment", "Detected and notified with active TB disease", "Gap in TB detection and TB prevention\u1d47 ")) +
  labs(x="", y="Percentage(%)") +

  theme(legend.position="bottom",
        #Set font for country names to avoid overlapping especialy because this year we got long names as PNG
        axis.text=element_text(size=7),
        #Set margins to give enough space for those long long footnotes.
        legend.box.margin=margin(-20,-20,-20,-20),
        plot.margin = margin(2, 10, 80, 10),
        panel.grid=element_blank()) +

  expand_limits(c(0,0)) +

  ggtitle(paste0("FIG.5.2\nGaps in TB prevention and TB detection for people who were newly enrolled in HIV care in ",
                 report_year - 1,
                 ",selected countries\u1d43 "))

# Add explanatory footnotes
ipt_gap_footnote <- "\u1d43 The selected countries are high TB or TB/HIV burden countries that reported on all three of the following: the number of people newly enrolled on HIV care; the number of \n      TB cases detected among people newly enrolled on HIV care; and the number of people newly enrolled on HIV care who were started on TB preventive treatment. In high \n      TB burden countries, testing for LTBI is not a requirement for initiation of TB preventive treatment, such that all those without active TB disease are eligible for TB preventive \n                              treatment.\n     \u1d47  The gap represents people living with HIV who should have undergone complete evaluation for TB disease or TB preventive treatment."
ipt_gap_plot <- arrangeGrob(ipt_gap_plot,
                            bottom = textGrob(ipt_gap_footnote,
                                              x = 0,
                                              hjust = -0.1,
                                              vjust=-0.7,
                                              gp = gpar(fontsize = 8)))

figsavecairo(ipt_gap_plot, ipt_gap_country, ipt_gap_footnote, "f5_2_gaps_for_ipt_in_new_hiv_patients", width=11, height=7) # Designer needs wide data; output portrait mode

# Clean up (remove any objects with their name starting with 'ipt_gap')
rm(list=ls(pattern = "^ipt_gap"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.3 (Map) -------
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
         e_prevtx_kids_pct,
         e_prevtx_eligible,
         newinc_con04_prevtx)

# Assign categories
prevtx_kids_data$cat <- cut(prevtx_kids_data$e_prevtx_kids_pct,
                            c(0, 25, 50, 75, Inf),
                            c('0-24', '25-49', '50-74', '\u226575'),
                            right = FALSE)


# produce the map
prevtx_kids_map <- WHOmap.print(prevtx_kids_data,
                                paste("FIG.5.3\nCoverage of TB preventive treatment among eligible children aged under 5 years\u1d43 , ",
                                      report_year-1),
                                legend.title = "Coverage (%)",
                                copyright=FALSE,
                                background="White",
                                colors=brewer.pal(4, "Blues"),
                                na.label="Not estimated",
                                show=FALSE)

# add footnote
prevtx_kids_foot <- "\u1d43 Children aged <5 years who were household contacts of bacteriologically confirmed pulmonary TB cases."

prevtx_kids_map <- arrangeGrob(prevtx_kids_map, bottom = textGrob(prevtx_kids_foot,
                                                                  x = 0,
                                                                  hjust = -0.1,
                                                                  vjust=-0.2,
                                                                  gp = gpar(fontsize = 10)))

figsavecairo(prevtx_kids_map,
             prevtx_kids_data,
             prevtx_kids_foot,
             "f5_3_prevtx_kids_map")

# Clean up (remove any objects with their name beginning with 'prevtx_kids')
rm(list=ls(pattern = "^prevtx_kids"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.4 (Map) ---------
# Notification rate ratio of TB among healthcare workers compared with the general population, 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

hcw_notif_hcw <-  strategy %>%
                  filter(year == report_year - 1) %>%
                  select(iso3,
                         hcw_tb_infected,
                         hcw_tot,
                         country)

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
                           nrr,
                           country)


hcw_data$cat <- cut(hcw_data$nrr,
                    c(0, 1, 2, 3, Inf),
                    c('0-0.9', '1-1.9', '2-2.9', '\u22653'),
                    right=FALSE)


# produce the map
hcw_map <- WHOmap.print(hcw_data,
                        paste("FIG.5.4\nNotification rate ratio of TB among healthcare workers compared with the general adult population,", report_year-1,"\u1d43"),
                        "Notification\nrate ratio",
                        copyright=FALSE,
                        colors=brewer.pal(4, "Reds"),
                        #colors=c('#edf8e9', '#bae4b3', '#74c476', '#238b45'),
                        background="White",
                        show=FALSE)


# Add footnote about filtering out countries
hcw_foot <- paste("\u1d43 Data from ",
                  hcw_filtered_out,
                  " countries were excluded where the number of health-care workers reported was less than 1000.")

hcw_map <- arrangeGrob(hcw_map, bottom = textGrob(hcw_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))

figsavecairo(hcw_map,
        hcw_data,
        hcw_foot,
        "f5_4_hcw_notf_rate_ratio")

# Clean up (remove any objects with their name beginning with 'hcw')
rm(list=ls(pattern = "^hcw"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 5.5 (Map) ---------
# BCG vaccination policy by country
# Get data from
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bcg_policy_data <- read.csv(paste0(rdata_folder, "/BCG_Atlas_Data _2018-07-19.csv"),
                            na.strings = "")

# filter out empty lines and restrict to the variables we need
bcg_policy_data <- bcg_policy_data %>%
                   select(iso3, Country, BCG.Policy.Type) %>%
                   filter(!is.na(iso3))

bcg_policy_data$cat <- as.factor(bcg_policy_data$BCG.Policy.Type)

bcg_policy_map <- WHOmap.print(bcg_policy_data,
                               "FIG.5.6\nBCG vaccination policy by country",
                               "Policy",
                               copyright=FALSE,
                               colors=c("darkgreen", "lightgreen", "purple"),
                               background="White",
                               show=FALSE)

# Add footnote
bcg_policy_foot <- "\nSource: The BCG World Atlas 2nd Edition, http://www.bcgatlas.org/, accessed 19 July 2018"
bcg_policy_map <- arrangeGrob(bcg_policy_map,
                             bottom = textGrob(bcg_policy_foot,
                                               x = 0,
                                               hjust = -0.1,
                                               vjust=0,
                                               gp = gpar(fontsize = 10)))

figsavecairo(bcg_policy_map,
        bcg_policy_data,
        bcg_policy_foot,
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
                         sheet = "world_coverage",
                         col_names = TRUE
                         ) %>%

                # Restrict to BCG covereage for the latest year
                filter(Vaccine=="BCG" & Year == report_year - 1)


# Merge with list of all countries and remove unwanted variables
bcg_cov_data <- report_country %>%
                select(country, iso3) %>%
                left_join(bcg_cov_data, by = c("iso3" = "ISO_code")) %>%
                select(country,
                       iso3,
                       coverage = Percent_covrage)


bcg_cov_data$cat <- cut(bcg_cov_data$coverage,
                        c(0, 50, 90, 100),
                        c('0-49', "50-89", "90-100"),
                        right=FALSE)


# produce the map
bcg_cov_map <- WHOmap.print(bcg_cov_data,
                            paste("FIG.5.6\nCoverage of BCG vaccination,", report_year-1,"\u1d43 "),
                            "Percentage",
                            copyright=FALSE,
                            colors=brewer.pal(3, "Greens"),
                            background="White",
                            show=FALSE)

# Add footnote
bcg_cov_foot <- paste0("\u1d43 The target population of BCG coverage varies depending on national policy, but is typically for the number of live births in the year of reporting.",
                       "\nSource: ",
                       bcg_cov_page,
                       ", accessed ",
                       format(Sys.Date(), "%d %B %Y"))
  
bcg_cov_map <- arrangeGrob(bcg_cov_map,
                           bottom = textGrob(bcg_cov_foot,
                                             x = 0,
                                             hjust = -0.1,
                                             vjust=0,
                                             gp = gpar(fontsize = 10)))


figsavecairo(bcg_cov_map,
        bcg_cov_data,
        bcg_cov_foot,
        "f5_6_BCG_coverage_map")

# Clean up (remove any objects with their name beginning with 'bcg_cov')
rm(list=ls(pattern = "^bcg_cov"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure box 5.1.1 ------
# Use of rifapentine in regimens for TB preventive treatment by July 2019
# Codes getting from Dennis, data from Sanofi as external csv file
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
require (ggplot2)
require (whomap)

rifapentine <- read.csv("C:/Users/yinyinx/Desktop/rifapentine.map.GTBR2019-0813.csv")

rifapentine$rfp_used_by_jun2019[is.na(rifapentine$rfp_used_by_jun2019) & rifapentine$rfp_used_trials==1]<-2

rifapentine$var <- factor(rifapentine$rfp_used_by_jun2019,
                          levels = c(2,1),
                          labels = c("used in trials only","used"))

rifapentine_data <- rifapentine%>%
  mutate(cat=var)%>%
  select(-rfp_used_by_jun2019,
         -rfp_used_trials,
         -rfp_registered_by_jul_2019,
         -var)

rifapentine_map <- WHOmap.print(rifapentine_data,
                                paste("FIG.B5.1.1\nUse of rifapentine in regimens for TB preventive treatment by July 2019\u1d43"),
                                copyright=FALSE,
                                colors=c("mistyrose","salmon"),
                                background="White",
                                show=FALSE)

# Add footnote
rifapentine_foot <- "\u1d43 Currently registered for use in China Hong Kong SAR, India, Indonesia, Mongolia, Philippines,\nThailand, South Africa and the United States of America [Source: Sanofi, July 2019]"

rifapentine_map <- arrangeGrob(rifapentine_map,
                               bottom = textGrob(rifapentine_foot,
                                                 x = 0,
                                                 hjust = -0.2,
                                                 vjust=-1,
                                                 gp = gpar(fontsize = 10)))

figsavecairo(rifapentine_map,
             rifapentine_data,
             rifapentine_foot,
             "f5_box_5_1_1_rifapentine_map",
             width=11, height=7)

rm(list=ls(pattern = "^rifapentine"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 6 ------
# Financing for TB prevention, diagnosis and treatment
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 6.2 (Map) ---------
# The 119 low- and middle-income countries included in analyses of TB financing, 2006–2019
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# get data from CSV file sent by Ines
include_data <- read.csv(paste0(rdata_folder,"/include_map_2019-07-12.csv"))

# merge the data with the world bank income group
include_data <- country_group_membership %>%
                filter(group_type=="g_income") %>%
                select(country, iso3, group_name) %>%
                right_join(include_data, by = "iso3")

# latest year's notifications
include_data <- notification %>%
                filter(year==report_year - 1) %>%
                select(iso3, c_newinc) %>%
                inner_join(include_data, by = "iso3")


# create the mapping variable
# (make excluded as NA so looks better in the map)
include_data <- include_data %>%
                mutate(cat = ifelse(var=="Included", var, NA)) %>%
                mutate(cat= as.factor(cat))


# calculate number of countries included
included_tot <- nrow(filter(include_data, var=="Included"))


# produce the map
include_map <- WHOmap.print(include_data,
                            paste0("FIG.6.2\nThe ",
                                  included_tot,
                                  " low- and middle-income countries included in analyses of TB financing, 2006–",
                                  report_year,"\u1d43"),
                            na.label = "Not included",
                            copyright=FALSE,
                            colors=c('blue'),
                            background="White",
                            show=FALSE)


# Calculate stuff for the footnote
include_summary <- include_data %>%
                   filter(var=="Included") %>%
                   group_by(group_name) %>%
                   summarise(included_notified = sum(c_newinc, na.rm=TRUE),
                             included_number = n())

include_all_counts <- include_data %>%
                      group_by(group_name) %>%
                      summarise(all_number = n())


include_summary <- include_all_counts %>%
                   left_join(include_summary)

# generate summary string for number of countries included in each category
include_summary <- include_summary %>%
                   mutate(countries = paste0(included_number, "/", all_number))


# calculate share of notifications
include_all_not <-  sum(include_data$c_newinc, na.rm = TRUE)


include_summary <- include_summary %>%
                   filter(group_name != "HIC") %>%
                   mutate(share = round(included_notified * 100 / include_all_not))


# generate list of low and middle income countries that were excluded
exclusion_list <- include_data %>%
                  filter(group_name != "HIC" & var == "Excluded") %>%
                  select(country) %>%
                  arrange(country) %>%
                  sapply(paste, collapse=", ")

exclusion_count <-include_data %>%
                  filter(group_name != "HIC" & var == "Excluded") %>%
                  nrow()

include_footnote <- paste0("\u1d43 Countries were included in trend analyses if at least three years of high-quality finance data were available in the period 2006–",
                            report_year,
                            "\nLow-income (",
                            include_summary[include_summary$group_name=="LIC", "countries"] %>% unlist(),
                            "), ",
                            "lower-middle income (",
                            include_summary[include_summary$group_name=="LMC", "countries"] %>% unlist(),
                            "), ",
                            "and upper-middle income (",
                            include_summary[include_summary$group_name=="UMC", "countries"] %>% unlist(),
                            ") countries representing ",
                            include_summary[include_summary$group_name=="LIC", "share"] %>% unlist(), "%, ",
                            include_summary[include_summary$group_name=="LMC", "share"] %>% unlist(), "% and ",
                            include_summary[include_summary$group_name=="UMC", "share"] %>% unlist(), "% of ",
                            report_year - 1,
                            " notified cases, respectively, were included.",
                            "\nThe following ",
                            exclusion_count,
                            " low- and middle-income countries were excluded:\n",
                            exclusion_list)


# Add the footnote to the map
include_map <- arrangeGrob(include_map,
                           bottom=textGrob(include_footnote,
                                           x = 0,
                                           hjust = -0.05,
                                           vjust = 0.4,
                                           gp = gpar(fontsize=6)))


# Save the plot
figsavecairo(include_map,
        select(include_data,
               iso3,
               country,
               var),
        "f6_2_included_map")

# Clean up (remove any objects with their name beginning with 'include' or 'exclusion')
rm(list=ls(pattern = "^include|^exclusion"))


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
                     c("1", "2-9", "10-29", "\u226530"),
                     right=FALSE)


ghcc_map <- WHOmap.print(ghcc_data,
                         "FIG.B6.3.1\nCost per patient treated for drug-susceptible TB or MDR-TB:\ncurrent availability of unit cost data from independent costing studies, 1990-2017",
                         "Number\navailable",
                         copyright=FALSE,
                         colors=brewer.pal(4, "Reds"),
                         background="White",
                         show=FALSE)

# Add footnote for the source of the data
ghcc_map <- arrangeGrob(ghcc_map,
                        bottom = textGrob("Source: Unit Cost Study Repository, https://ghcosting.org/pages/data/ucsr/app/index, accessed 19 July 2018",
                                          x = 0,
                                          hjust = -0.1,
                                          vjust=0,
                                          gp = gpar(fontsize = 10)))

figsavecairo(ghcc_map,
             ghcc_data,
             "f6_box_6_3_1_unit_cost_data_map")

# Clean up (remove any objects with their name beginning with 'ghcc')
rm(list=ls(pattern = "^ghcc"))

