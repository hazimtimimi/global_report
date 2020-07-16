# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create figures and extract data summaries for the 2020 UNSG report
# on progress towards targets set by the 2018 UN High Level Meeting on Tuberculosis
# Code also contains exploratory queries on the data and some is to be used
# for the report text.
#
# Really I should have done this in RMarkdown, but life got in the way ....
#
# This script used to take most recent data directly from the data collection tables rather
# than the data snapshots because it evolved before the first snapshot was taken
#
# In earlier iterations it relied heavily on carrying over data from the previous year
# for countries that had not yet reported. The final version doesn't (it uses reported data)
# but I kept the code in which is now bypassed if flg_carry_forward is set to FALSE.
#
# Hazim Timimi, May - July 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source("set_environment.r")

# Decide whether or not to carry forward data from 2018 when 2019 data are missing
flg_carry_forward <- FALSE

# load packages ----
library(RODBC)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define functions ----


carry_forward <- function(df, var_name, data_year = 2019, flg_zeros = FALSE){

  # Carry reported numbers over from a previous year to the next year if data were missing
  # in that next year
  # flg_zeros == TRUE means treat zeros as missing data

  # split dataframe into two
  df_manipulate <- df %>% select(iso2, year, !!var_name)
  df_remainder <- df %>% select(-!!var_name)

  df_updated <- df_manipulate %>%
    # find empty values
    filter(year == data_year &  ((flg_zeros == FALSE & is.na(!!sym(var_name)))
                                 |
                                 (flg_zeros == TRUE & !!sym(var_name) == 0 ))) %>%
    select(iso2) %>%
    # See if those countries had reported data in the previous year
    inner_join(filter(df_manipulate, year == data_year - 1)) %>%
    filter(!!sym(var_name) > 0) %>%
    #pretend they are data for the data (next) year
    mutate(year = data_year)

  #Show to the console the number carried forward
  carried_forward <- df_updated %>%
    summarise_at(.vars = var_name, .funs = sum )
  print(paste(var_name, "number carried forward:", carried_forward))

  # Reassemble the dataframe
  df_manipulate <- df_manipulate %>%
    anti_join(df_updated, by= c("iso2", "year")) %>%
    rbind(df_updated) %>%
    inner_join(df_remainder, by= c("iso2", "year"))

  return(df_manipulate)
}


rounder <- function(x){

  ifelse(is.na(x), NA,
       formatC(round(x,0), big.mark=" ", format="d")
       )
}


plot_theme <- function(base_size=14, base_family="") {

  gray <- "#BCBCBC"
  charcoal <- "#222222"


  theme(

  #Text format:
  plot.title = element_text(family=base_family,
                            size=28,
                            #face="bold",
                            color=charcoal),
  plot.subtitle = element_text(family=base_family,
                               size=18,
                               face = "italic" ,
                               margin=ggplot2::margin(5,0,9,0)),

  plot.margin = margin(30,5,30,5),

  #Legend format
  legend.position = "bottom",
  legend.text.align = 0,
  legend.background = element_blank(),
  legend.title = element_blank(),
  legend.key = element_blank(),
  legend.text = element_text(family=base_family,
                             size=14,
                             color=charcoal),

  #Axis format
  axis.title = element_text(family=base_family,
                            size=14,
                            color=charcoal),
  axis.text = element_text(family=base_family,
                           size=18,
                           color=charcoal),
  axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
  axis.ticks = ggplot2::element_blank(),
  axis.line =ggplot2::element_blank(),
  #axis.line.y = ggplot2::element_line(color=gray, size = 0.25),

  #Grid lines
  #This removes all minor gridlines and adds major y gridlines.
  panel.grid.minor = ggplot2::element_blank(),
  panel.grid.major.y = ggplot2::element_line(color=gray,
                                             size = 0.25),
  panel.grid.major.x = ggplot2::element_blank(),

  #Blank background
  panel.background = ggplot2::element_blank(),

  )

}

sum_of_row <- function(x) {
  tosum <- as.matrix(x)
  summed <- rowMeans((tosum), na.rm=TRUE) * rowSums(!is.na((tosum)))
  # Flush out any NaN's
  summed <- ifelse(is.nan(summed), NA, summed)
  return(summed)
}

# Get data on impact of COVID-19 on TB services -----

sql <- "WITH HBC AS (
	SELECT	iso2, group_name
	FROM	dbo.view_country_group_membership
	WHERE	group_type = 'g_hb_tb'
	)

SELECT	view_TME_master_covid_unhlm.iso2,
		ISNULL(group_name, 0) AS hbc,
		service_changes,
		opd_visits_down_ds,
		opd_visits_down_dr,
		drugs_up,
		nominate_collect,
		drugs_home_delivery,
		remote_advice_up,
		opd_places_down_ds,
		opd_places_down_dr,
		hosp_places_down_ds,
		hosp_places_down_dr,
		other_service_changes,
		tb_self_isolation,
		resource_reallocated,
		gxp_reassigned,
		ntp_central_reassigned,
		ntp_periph_reassigned,
		budget_reallocated, other_reallocation

FROM	view_TME_master_covid_unhlm
			LEFT OUTER JOIN HBC ON
				view_TME_master_covid_unhlm.iso2 = HBC.iso2"

ch <- odbcDriverConnect(connection_string)

responses <- sqlQuery(ch, sql, stringsAsFactors = FALSE)

close(ch)

# pivot long and calculate counts for each possible value

responses_long <- responses %>%
  # remove text fields
  select(-starts_with("other")) %>%
  pivot_longer(cols = service_changes:budget_reallocated,
               names_to = "question",
               values_to = "answer",
               values_drop_na = TRUE
               ) %>%
  # calculate group counts
  group_by(question, answer) %>%
  summarise(how_many = n()) %>%
  ungroup()

# and do the same just for the HBCs

responses_long_hbc <- responses %>%
  filter(hbc == 1) %>%
  # remove text fields
  select(-starts_with("other")) %>%
  pivot_longer(cols = service_changes:budget_reallocated,
               names_to = "question",
               values_to = "answer",
               values_drop_na = TRUE
               ) %>%
  # calculate group counts
  group_by(question, answer) %>%
  summarise(how_many = n())%>%
  ungroup()



# Plot as a horizontal stacked bar chart
responses_long %>%
  ggplot(aes(x = question, y = how_many, fill = answer, label = answer)) +
  geom_bar(stat = "identity") +
  coord_flip()


# Or plot only the yes answers as a horizontal bar chart
responses_long %>%
  filter(answer == 1) %>%
  ggplot(aes(x = question, y = how_many)) +
  geom_bar(stat = "identity") +
  coord_flip()




# pivot wide now to see totals for each questio
responses_wide <- responses_long %>%
  pivot_wider(id_cols = question,
              names_from = answer,
              names_prefix = "answer_",
              values_from = how_many)


responses_wide_hbc <- responses_long_hbc %>%
  pivot_wider(id_cols = question,
              names_from = answer,
              names_prefix = "answer_",
              values_from = how_many)

# Add question text and save as tables for KF


# Create lookup table for question text
question <- c("service_changes", "opd_visits_down_ds", "opd_visits_down_dr", "drugs_up", "nominate_collect",
              "drugs_home_delivery", "remote_advice_up", "opd_places_down_ds", "opd_places_down_dr",
              "hosp_places_down_ds", "hosp_places_down_dr", "tb_self_isolation", "resource_reallocated",
              "gxp_reassigned", "ntp_central_reassigned", "ntp_periph_reassigned", "budget_reallocated")

q_number <- c("COV.1", "COV.1. 1", "COV.1. 2", "COV.1. 3", "COV.1. 4", "COV.1. 5", "COV.1. 6", "COV.1. 7", "COV.1. 8",
              "COV.1. 9", "COV.1.10", "COV.2", "COV.3", "COV.3.1", "COV.3.2", "COV.3.3", "COV.3.4")

q_text <- c("Have any changes been made to how TB treatment services are delivered due to the COVID-19 pandemic?",
"Reduced frequency of outpatient visits to a health facility for treatment monitoring or collection of drugs for patients with drug-susceptible TB",
"Reduced frequency of outpatient visits to a health facility for treatment monitoring or collection of drugs for patients with multidrug or rifampicin-resistant TB",
"Amount of anti-TB drugs given to patients to take home increased to one month or more",
"TB patient can nominate another household member to collect anti-TB drugs from a health facility on their behalf",
"Home delivery of anti-TB drugs to TB patients",
"Expanded use of remote advice and support for TB patients",
"Reduced number of health facilities where outpatient TB treatment is provided for patients with drug-susceptible TB",
"Reduced number of health facilities where outpatient TB treatment is provided for patients with multidrug or rifampicin-resistant TB",
"Reduced number of hospitals where inpatient treatment is provided for patients with drug-susceptible TB",
"Reduced number of hospitals where inpatient treatment is provided for patients with multidrug or rifampicin-resistant TB",
"Have TB patients been asked to self-isolate at home?",
"Has there been any reallocation of resources from TB services to COVID-19 testing and treatment?",
"GeneXpert machines used for TB diagnosis reassigned for COVID-19 testing",
"Staff in the central unit of the NTP reassigned to other duties",
"NTP staff at subnational levels systematically reassigned to other duties",
"Budgets originally allocated for TB reallocated to the COVID-19 response")

q_details <- data.frame(question, q_number, q_text, stringsAsFactors = FALSE)
rm(question, q_number, q_text)


responses_wide <- responses_wide %>%
  inner_join(q_details) %>%
  select(q_number, q_text, no = answer_0, yes = answer_1, dont_know = answer_3) %>%
  arrange(q_number)

# Do the same, filterd for the 30 high burden countries
responses_wide_hbc <- responses_wide_hbc %>%
  inner_join(q_details) %>%
  select(q_number, q_text, no = answer_0, yes = answer_1, dont_know = answer_3) %>%
  arrange(q_number)

# Save answers on COVID-19 impact as CSV files ----

write.csv(responses_wide,
          file = paste0(unsg_report_folder, "tb_covid_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)

write.csv(responses_wide_hbc,
          file = paste0(unsg_report_folder, "tb_covid_hbc_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create notification charts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get notification data -----

sql <- "SELECT	iso2, year, c_newinc, c_new_014,
		ISNULL(unconf_mdr_tx,0) + ISNULL(conf_mdr_tx,0) +
		ISNULL(unconf_rrmdr_tx,0) + ISNULL(conf_rrmdr_tx, 0) AS dr_tx,
		rrmdr_014_tx
FROM	view_TME_master_notification
WHERE	year BETWEEN 2015 AND 2019
ORDER BY iso2, year;"

ch <- odbcDriverConnect(connection_string)

tx <- sqlQuery(ch, sql, stringsAsFactors = FALSE)

countries <- sqlQuery(ch, "SELECT country, iso2 FROM view_TME_master_report_country", stringsAsFactors = FALSE)

close(ch)


# Get list of countries whose 2019 notifications are > 10% compared to 2017
n_2017 <- tx %>%
  filter(year == 2017) %>%
  select(iso2, newinc_2017 = c_newinc, dr_tx_2017 = dr_tx)
n_2019 <- tx %>%
  filter(year == 2019) %>%
  select(iso2, newinc_2019 = c_newinc, dr_tx_2019 = dr_tx)

notifs <- inner_join(n_2017, n_2019, by = c("iso2")) %>%
  mutate(delta_n_pct = ifelse(newinc_2017 > 0, (newinc_2019 - newinc_2017)* 100/ newinc_2017, NA),
         delta_d_pct = ifelse(dr_tx_2017 > 0, (dr_tx_2019 - dr_tx_2017)* 100/ dr_tx_2017, NA))

# See whose notifications jumped by 10% or more
notifs %>%
  filter(delta_n_pct >= 10 ) %>%
  inner_join(countries, by = c("iso2")) %>%
  arrange(country) %>%
  select(country)

# See whose dr-tb enrollments jumped by 10% or more
notifs %>%
  filter(delta_d_pct >= 10 ) %>%
  inner_join(countries, by = c("iso2")) %>%
  arrange(country) %>%
  select(country)

tx_cf <- tx

if (flg_carry_forward) {

  # Find countries with empty data for latest year and see if there are data for the previous year
  # do for  c_newinc, c_new_014 and dr_tx (but not rrmdr_014_tx)

  tx_cf <- carry_forward(tx, var_name = "c_newinc")
  tx_cf <- carry_forward(tx_cf, var_name = "c_new_014")
  tx_cf <- carry_forward(tx_cf, var_name = "dr_tx")

}

# Calculate global totals
tx_cf_global <- tx_cf %>%
  group_by(year) %>%
  summarise_at(c("c_newinc", "c_new_014", "dr_tx", "rrmdr_014_tx"), sum, na.rm=TRUE) %>%
  ungroup()


# Calculate "adult" notifications
tx_cf_global <- tx_cf_global %>%
  mutate(c_new_15plus = c_newinc - c_new_014,
         rrmdr_15plus_tx = dr_tx - rrmdr_014_tx) %>%
  select(-c_newinc, -dr_tx)


# Plot notifications as stacked bar chart ----
plot_notifs <- tx_cf_global %>%
  #drop the mdr columns
  select(-starts_with("rrmdr")) %>%
  # switch to long
  pivot_longer(cols = starts_with("c_"),
               names_to = "age_group",
               values_to = "how_many") %>%
  # Plot timeseries as bar chart

  ggplot(aes(x=year, y=how_many, fill = age_group)) +

  geom_bar(stat = "identity") +

  scale_x_continuous(name="", breaks = c(2015, 2016, 2017, 2018, 2019)) +

  # display y-axis scale im millions
  scale_y_continuous(name = "Millions", labels = function(i){round(i/1e6)},
                     limits = c(0,8e6)) +

  scale_fill_manual("",
                    breaks = c("c_new_15plus", "c_new_014" ),
                    labels = c("Adults aged 15 and above", "Children aged under 15"),
                    values = c("c_new_15plus"="blue",
                               "c_new_014"="red")) +

  # Use guide_legend() to force the legends into two columns only. This prevented
  # long text from being truncated in the output
  guides(colour = guide_legend(ncol = 2, byrow = TRUE)) +

  # ggtitle(label = "TB cases notified") +

  plot_theme()

# Save to .png
ggsave(file = paste0(unsg_report_folder, "notifications_", Sys.Date(), ".png"),
       plot = plot_notifs,
       width=11, height=7)



# Plot enrollment on MDR treatment as stacked bar chart ----
plot_dr_tx <- tx_cf_global %>%
  #drop the notifications columns
  select(-starts_with("c_new")) %>%
  # switch to long
  pivot_longer(cols = starts_with("rrmdr"),
               names_to = "age_group",
               values_to = "how_many") %>%
  # Plot timeseries as bar chart

  ggplot(aes(x=year, y=how_many, fill = age_group)) +

  geom_bar(stat = "identity") +

  scale_x_continuous(name="", breaks = c(2015, 2016, 2017, 2018, 2019)) +

  # display y-axis scale im thousands
  scale_y_continuous(name = "Thousands", labels = function(i){round(i/1e3)},
                     limits = c(0,2e5)) +

  #ggtitle(label = "Enrollment on MDR treatment") +

  scale_fill_manual("",
                    breaks = c("rrmdr_15plus_tx", "rrmdr_014_tx" ),
                    labels = c("All ages until 2017; adults (15 years and above) 2018–2019", "Children aged under 15"),
                    values = c("rrmdr_15plus_tx"="blue",
                               "rrmdr_014_tx"="red")) +

  # Use guide_legend() to force the legends into two columns only. This prevented
  # long text from being truncated in the output
  guides(colour = guide_legend(ncol = 1)) +

  plot_theme()

# Save to .png
ggsave(file = paste0(unsg_report_folder, "MDR_", Sys.Date(), ".png"),
       plot = plot_dr_tx,
       width=11, height=7)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create TPT charts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# get the TPT data ----

sql <- "SELECT	view_TME_master_notification.iso2, view_TME_master_notification.year,
		COALESCE(hiv_ipt_reg_all, hiv_ipt) AS hiv_ipt,
		/* next one added 2016 dcyear */
		newinc_con04_prevtx,
		/* next one used 2018 dcyear only */
		newinc_con5plus_prevtx,
		/* next one added 2019 dcyear */
		newinc_con_prevtx
FROM	view_TME_master_notification
			INNER JOIN view_TME_master_strategy ON
				view_TME_master_notification.iso2 = view_TME_master_strategy.iso2 AND
				view_TME_master_notification.year = view_TME_master_strategy.year
WHERE	view_TME_master_notification.year BETWEEN 2015 AND 2019
ORDER BY iso2, year;"

ch <- odbcDriverConnect(connection_string)

prevtx <- sqlQuery(ch, sql, stringsAsFactors = FALSE)

close(ch)

prevtx_cf <- prevtx

if (flg_carry_forward){

  # Find countries with empty data for latest year and see if there are data for the previous year
  # do for  hiv_ipt, newinc_con04_prevtx and newinc_con_prevtx

  prevtx_cf <- carry_forward(prevtx, var_name = "hiv_ipt")
  prevtx_cf <- carry_forward(prevtx_cf, var_name = "newinc_con04_prevtx")
  prevtx_cf <- carry_forward(prevtx_cf, var_name = "newinc_con_prevtx")

}


# Calculate "adult" fraction
# Watch out because sometimes only some variables carried forward from the previous year resulting
# in strange number combinations
prevtx_cf <- prevtx_cf %>%
  mutate(prevtx_5plus = ifelse(newinc_con_prevtx > 0 & newinc_con04_prevtx > 0,
                                  newinc_con_prevtx - newinc_con04_prevtx,
                                  newinc_con_prevtx)) %>%
  # Convert negative prevtx_5plus casued by weird combination of carry overs to zero
  mutate(prevtx_5plus = ifelse(prevtx_5plus < 0 , 0, prevtx_5plus)) %>%
  # deal with 2017 variable
  mutate(prevtx_5plus = ifelse(year == 2017 ,
                                  newinc_con5plus_prevtx,
                                  prevtx_5plus)) %>%
  select(-newinc_con5plus_prevtx)


# Calculate global totals
prevtx_cf_global <- prevtx_cf %>%
  group_by(year) %>%
  summarise_at(c("hiv_ipt", "newinc_con04_prevtx", "prevtx_5plus"), sum, na.rm=TRUE) %>%
  ungroup()


# Plot TPT data as stacked bar chart ----
plot_tpt <- prevtx_cf_global %>%

  # switch to long
  pivot_longer(cols = hiv_ipt:prevtx_5plus,
               names_to = "TPT_category",
               values_to = "how_many") %>%

  # Plot timeseries as bar chart

  ggplot(aes(x=year, y=how_many, fill = TPT_category)) +

  geom_bar(stat = "identity")  +

  geom_col(position = position_stack(reverse = TRUE)) +

  scale_x_continuous(name="", breaks = c(2015, 2016, 2017, 2018, 2019)) +

  # display y-axis scale im millions
  scale_y_continuous(name = "Millions", labels = function(i){round(i/1e6)},
                     limits = c(0,3e6)) +

  #ggtitle(label = "Provision of TB Preventive Treatment") +

  scale_fill_manual("",
                    breaks = c("hiv_ipt", "newinc_con04_prevtx", "prevtx_5plus" ),
                    labels = c("People living with HIV", "Contacts aged under 5", "Contacts aged 5 and over"),
                    values = c("hiv_ipt"="blue",
                               "newinc_con04_prevtx"="red",
                               "prevtx_5plus"="gray")) +

  # Use guide_legend() to force the legends into two columns only. This prevented
  # long text from being truncated in the output
  guides(colour = guide_legend(ncol = 2, byrow = TRUE)) +

  plot_theme()

# Save to .png
ggsave(file = paste0(unsg_report_folder, "TPT_", Sys.Date(), ".png"),
       plot = plot_tpt,
       width=11, height=7)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create finance charts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# get the finance data ----
# actually just enter it here manually

year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
domestic <- c(4.296433143, 4.547460182, 4.766996349, 4.885291145, 5.049376563, 4.7, 4.5, 5.0, 5.1, 5.1, 5.5)
international <- c(0.651843925, 0.722705294, 0.800041564, 0.945082637, 0.792551052, 0.9, 1.0, 1.1, 1.0, 0.9, 1.0 )

research <- c(0.643, 0.675, 0.639, 0.686, 0.674, 0.621, 0.726, 0.772, 0.906, NA, NA)

tb_funding <- data.frame(year, domestic, international)
tb_research <- data.frame(year, research)

# plot tb_funding as stacked bar chart ----

plot_funding <- tb_funding %>%
  filter(year >= 2015) %>%
  pivot_longer(cols = c("domestic", "international"),
               names_to = "source",
               values_to = "amount") %>%
  # Plot timeseries as bar chart

  ggplot(aes(x=year, y=amount, fill = source)) +

  geom_bar(stat = "identity") +

  geom_col(position = position_stack(reverse = TRUE)) +

  scale_x_continuous(name="", breaks = seq(2015, 2020, by = 1)) +

  scale_y_continuous(name = "Billions (constant 2019 US$)",
                     labels = round,
                     limits = c(0,15)) +

  scale_fill_manual("",
                    breaks = c("domestic", "international" ),
                    labels = c("Domestic", "International donor"),
                    values = c("domestic"="blue",
                               "international"="red")) +

  # Use guide_legend() to force the legends into two columns only. This prevented
  # long text from being truncated in the output
  guides(colour = guide_legend(ncol = 2, byrow = TRUE)) +

  # ggtitle(label = "TB cases notified") +

  # add target funding of USD 13 billion as dashed horizontal line
  geom_hline(yintercept = 13,
             linetype="dashed",
             color = "#222222",
             size=1.5,
             show.legend = TRUE) +

  annotate("text",
           x = 2019, y = 12,
           label = "Target",
           size = 6,
           color = "#222222") +

  plot_theme()

# Save to .png
ggsave(file = paste0(unsg_report_folder, "funding_", Sys.Date(), ".png"),
       plot = plot_funding,
       width=11, height=7)


# Plot research funding as asimple bar chart ----

plot_research <- tb_research %>%

  # Get rid of 2019
  filter(year < 2019 & year >= 2015) %>%

  ggplot(aes(x=year, y=research)) +

  geom_bar(stat = "identity", fill = "blue") +

  scale_x_continuous(name="", breaks = c(2015, 2016, 2017, 2018)) +

  # display y-axis scale im millions
  scale_y_continuous(name = "Billions (current US$)",
                     limits = c(0,2.1)) +

  # add target funding of USD 2 billion as dashed horizontal line
  geom_hline(yintercept = 2,
             linetype="dashed",
             color = "#222222",
             size=1.5,
             show.legend = TRUE) +

  annotate("text",
           x = 2018, y = 1.9,
           label = "Target",
           size = 6,
           color = "#222222") +

  plot_theme()

# Save to .png
ggsave(file = paste0(unsg_report_folder, "research_", Sys.Date(), ".png"),
       plot = plot_research,
       width=11, height=7)


# Print summary stats for use in report and save as CSVs ----
print(tx_cf_global)
print(prevtx_cf_global)

write.csv(tx_cf_global,
          file = paste0(unsg_report_folder, "tx_cf_global_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)

write.csv(prevtx_cf_global,
          file = paste0(unsg_report_folder, "prevtx_cf_global_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)

# Get answers to question 5.2 ----
# (whether country has updated its targets in line with UNHLM global targets)

sql <- "WITH HBC AS (
	SELECT	iso2, group_name
	FROM	dbo.view_country_group_membership
	WHERE	group_type = 'g_hb_tb'
	)

SELECT	view_TME_master_covid_unhlm.iso2,
        group_name,
		    nsp_updated_unhlm

FROM	view_TME_master_covid_unhlm
			LEFT OUTER JOIN HBC ON
				view_TME_master_covid_unhlm.iso2 = HBC.iso2"

ch <- odbcDriverConnect(connection_string)

nsp_updated <- sqlQuery(ch, sql, stringsAsFactors = FALSE)

close(ch)

# calculate total globally and for the HBCs
nsp_updated %>% summarise(sum = sum(nsp_updated_unhlm, na.rm = TRUE))
nsp_updated %>% filter(group_name==1) %>% summarise(sum = sum(nsp_updated_unhlm, na.rm = TRUE))


# Calculate values for table 3 ----
# % pulmonary bacteriologically confirmed
# and CDR for the HBCs

sql <- "WITH HBC AS (
	SELECT	iso2, group_name
	FROM	dbo.view_country_group_membership
	WHERE	group_type = 'g_hb_mdr'
	),
notifs AS (
	SELECT	iso2, year, new_labconf, ret_rel_labconf, new_clindx, ret_rel_clindx, c_newinc
	FROM	view_TME_master_notification
	WHERE	year BETWEEN 2018 AND 2019
	),
inc AS (
  SELECT iso2, year, e_inc_num
  FROM  view_TME_estimates_epi_rawvalues
  WHERE year=2018
)

SELECT group_name, notifs.*, e_inc_num
FROM	notifs LEFT OUTER JOIN HBC ON
			notifs.iso2 = HBC.iso2
			LEFT OUTER JOIN inc ON
			notifs.iso2 = inc.iso2 AND
			notifs.year = inc.year
ORDER BY iso2, year;"

ch <- odbcDriverConnect(connection_string)

notifs <- sqlQuery(ch, sql, stringsAsFactors = FALSE)

close(ch)

notifs$numerator <- sum_of_row(notifs[c("new_labconf", "ret_rel_labconf")])
notifs$denominator <- sum_of_row(notifs[c("new_labconf", "new_clindx", "ret_rel_labconf", "ret_rel_clindx")])

# Calculate and display aggregates on bac-confirmation ----
pulm_labconf <- notifs %>%
  group_by(year) %>%
  summarise_at(vars(numerator:denominator), sum,na.rm = TRUE) %>%
  ungroup() %>%
  mutate(c_pulm_labconf_pct = round(numerator * 100 / denominator))

# MDR HBC, also estimate CDR
pulm_labconf_hbmdr <- notifs %>%
  filter(group_name == 1) %>%
  group_by(year) %>%
  summarise_at(vars(c_newinc:denominator), sum,na.rm = TRUE) %>%
  ungroup() %>%
  mutate(c_pulm_labconf_pct = round(numerator * 100 / denominator),
         cdr = ifelse(e_inc_num > 0, round(c_newinc * 100 / e_inc_num), NA))

print(pulm_labconf)
print(pulm_labconf_hbmdr)

write.csv(pulm_labconf,
          file = paste0(unsg_report_folder, "pulm_labconf_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)

write.csv(pulm_labconf_hbmdr,
          file = paste0(unsg_report_folder, "pulm_labconf_hbmdr_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)


# Get data on DST ----
sql <- "WITH HBC AS (
	SELECT	iso2, group_name
	FROM	dbo.view_country_group_membership
	WHERE	group_type = 'g_hb_mdr'
	),
dst AS (
	SELECT	iso2, year, r_rlt_new, r_rlt_ret, pulm_labconf_new, pulm_labconf_ret
	FROM	view_TME_master_dr_surveillance
	WHERE	year >= 2018 AND all_areas_covered = 1
	)

SELECT group_name, dst.*
FROM	dst LEFT OUTER JOIN HBC ON
			dst.iso2 = HBC.iso2
ORDER BY iso2, year;"

ch <- odbcDriverConnect(connection_string)

dst <- sqlQuery(ch, sql, stringsAsFactors = FALSE)

close(ch)


# Calculate and display aggregates on DST ----
dst_global <- dst %>%
  group_by(year) %>%
  summarise_at(vars(r_rlt_new:pulm_labconf_ret), sum,na.rm = TRUE) %>%
  ungroup() %>%
  mutate(c_rdst_new_pct = round(r_rlt_new * 100 / pulm_labconf_new),
         c_rdst_ret_pct = round(r_rlt_ret * 100 / pulm_labconf_ret),
         c_rdst_pct = round( (r_rlt_new + r_rlt_ret) * 100 / (pulm_labconf_new + pulm_labconf_ret)))

# MDR HBC
dst_hbmdr <- dst %>%
  filter(group_name == 1) %>%
  group_by(year) %>%
  summarise_at(vars(r_rlt_new:pulm_labconf_ret), sum,na.rm = TRUE) %>%
  ungroup() %>%
  mutate(c_rdst_new_pct = round(r_rlt_new * 100 / pulm_labconf_new),
         c_rdst_ret_pct = round(r_rlt_ret * 100 / pulm_labconf_ret),
         c_rdst_pct = round( (r_rlt_new + r_rlt_ret) * 100 / (pulm_labconf_new + pulm_labconf_ret)))

print(dst_global)
print(dst_hbmdr)

write.csv(dst_global,
          file = paste0(unsg_report_folder, "dst_global_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)

write.csv(dst_hbmdr,
          file = paste0(unsg_report_folder, "dst_hbmdr_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)


# Get data on treatment success ----
sql <- "WITH HBC AS (
	SELECT	iso2, group_name
	FROM	dbo.view_country_group_membership
	WHERE	group_type = 'g_hb_mdr'
	),
succ AS (
	SELECT	iso2, year, mdr_succ, mdr_coh
	FROM	view_TME_master_outcomes
	WHERE	year BETWEEN 2016 and 2017
	)

SELECT group_name, succ.*
FROM	succ LEFT OUTER JOIN HBC ON
			succ.iso2 = HBC.iso2
ORDER BY iso2, year;"

ch <- odbcDriverConnect(connection_string)

succ <- sqlQuery(ch, sql, stringsAsFactors = FALSE)

close(ch)


# Calculate and display aggregates on treatment success ----
mdr_tx_succ_global <- succ %>%
  group_by(year) %>%
  summarise_at(vars(mdr_succ:mdr_coh), sum,na.rm = TRUE) %>%
  ungroup() %>%
  mutate(c_mdr_tsr = round(mdr_succ * 100 / mdr_coh))


# MDR HBC
mdr_tx_succ_hbmdr <- succ %>%
  filter(group_name == 1) %>%
  group_by(year) %>%
  summarise_at(vars(mdr_succ:mdr_coh), sum,na.rm = TRUE) %>%
  ungroup() %>%
  mutate(c_mdr_tsr = round(mdr_succ * 100 / mdr_coh))

print(mdr_tx_succ_global)
print(mdr_tx_succ_hbmdr)

write.csv(mdr_tx_succ_global,
          file = paste0(unsg_report_folder, "mdr_tx_succ_global_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)

write.csv(mdr_tx_succ_hbmdr,
          file = paste0(unsg_report_folder, "mdr_tx_succ_hbmdr_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)



# text on increased notifications in India and Indonesia ----

sql <- "SELECT iso2, year, c_newinc
FROM view_TME_master_notification
WHERE year IN (2017, 2019) AND iso2 IN ('ID','IN')
ORDER BY iso2, year;"

ch <- odbcDriverConnect(connection_string)
notifs <- sqlQuery(ch, sql, stringsAsFactors = FALSE)
close(ch)

# IN changes
IN_changes <- notifs %>%
  filter(iso2=="IN") %>%
  mutate(prev = lag(c_newinc)) %>%
  mutate( diff = c_newinc - prev,
          diff_pct = (c_newinc - prev) * 100 / prev)

# ID changes
ID_changes <- notifs %>%
  filter(iso2=="ID") %>%
  mutate(prev = lag(c_newinc)) %>%
  mutate( diff = c_newinc - prev,
          diff_pct = (c_newinc - prev) * 100 / prev)

print(IN_changes)
print(ID_changes)

write.csv(IN_changes,
          file = paste0(unsg_report_folder, "IN_changes_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)

write.csv(ID_changes,
          file = paste0(unsg_report_folder, "ID_changes_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)



# text on absolute numbers enrolled on MDR treatment ----
sql <- "WITH tx_17 AS (
SELECT iso2, CASE WHEN COALESCE(unconf_rrmdr_tx,conf_rrmdr_tx) IS NOT NULL
				THEN ISNULL(unconf_rrmdr_tx,0) + ISNULL(conf_rrmdr_tx, 0)
			ELSE NULL
		END AS dr_tx_17
FROM view_TME_master_notification
WHERE year = 2017),

tx_19 AS (
SELECT iso2, CASE WHEN COALESCE(unconf_rrmdr_tx,conf_rrmdr_tx) IS NOT NULL
				THEN ISNULL(unconf_rrmdr_tx,0) + ISNULL(conf_rrmdr_tx, 0)
			ELSE NULL
		END AS dr_tx_19, rrmdr_014_tx
FROM view_TME_master_notification
WHERE year = 2019)

SELECT tx_17.iso2, dr_tx_17, dr_tx_19, rrmdr_014_tx

FROM tx_17 INNER JOIN tx_19 ON
  tx_17.iso2 = tx_19.iso2
ORDER BY tx_17.iso2;"

ch <- odbcDriverConnect(connection_string)
notifs <- sqlQuery(ch, sql, stringsAsFactors = FALSE)
close(ch)

# Get the top 5 with the largest change

delta_top5_drtx <- notifs %>%
  mutate(delta = dr_tx_19 - dr_tx_17) %>%
  arrange(desc(delta)) %>%
  head(5) %>%
  mutate(pct = delta * 100 / dr_tx_17)

# Look at global total in 2019
drtx_global<- notifs %>%
  summarise_at(vars(dr_tx_17:rrmdr_014_tx), sum, na.rm = TRUE)

print(delta_top5_drtx)
print(drtx_global)

write.csv(delta_top5_drtx,
          file = paste0(unsg_report_folder, "delta_top5_drtx_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)

write.csv(drtx_global,
          file = paste0(unsg_report_folder, "drtx_global_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)


# Text on number of countries in each region with TPT coverage >= 75% for children aged 0-4 ----
sql <- " SELECT g_whoregion, count(country) AS countries
  FROM view_TME_estimates_ltbi
  where year = 2019 and e_prevtx_kids_pct >= 75.00
  group by g_whoregion
  order by g_whoregion;"

ch <- odbcDriverConnect(connection_string)
tpt_regions <- sqlQuery(ch, sql, stringsAsFactors = FALSE)
close(ch)

print(tpt_regions)

# Text on SOuth Africa's share of global TPT numbers for PLHIV

prevtx_cf_ZA <- prevtx_cf %>%
  filter(iso2 == "ZA" & year %in% c(2018, 2019)) %>%
  select(iso2, year, hiv_ipt_ZA = hiv_ipt) %>%
  inner_join(prevtx_cf_global, by = "year") %>%
  mutate(hiv_ipt_share = hiv_ipt_ZA / hiv_ipt) %>%
  select(iso2, year, hiv_ipt_ZA, hiv_ipt, hiv_ipt_share)

print(prevtx_cf_ZA)

write.csv(prevtx_cf_ZA,
          file = paste0(unsg_report_folder, "prevtx_cf_ZA_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)


# Text on number of  adult houshold contacts on TPT and fraction of global total ----

sql <- "select iso2, g_whoregion FROM view_TME_master_report_country"

ch <- odbcDriverConnect(connection_string)
regions <- sqlQuery(ch, sql, stringsAsFactors = FALSE)
close(ch)

prevtx_cf_region <- prevtx_cf %>%
  filter(year == 2019) %>%
  inner_join(regions, by = "iso2") %>%
  select(g_whoregion, iso2, newinc_con04_prevtx, prevtx_5plus) %>%
  group_by(g_whoregion) %>%
  summarise_at(vars(newinc_con04_prevtx: prevtx_5plus), sum,na.rm = TRUE) %>%
  ungroup()

prevtx_cf_global_2019_tot <- prevtx_cf_global %>% filter(year == 2019) %>% select(prevtx_5plus)

prevtx_cf_region$proportion <- prevtx_cf_region$prevtx_5plus / prevtx_cf_global_2019_tot$prevtx_5plus

print(prevtx_cf_region)

write.csv(prevtx_cf_region,
          file = paste0(unsg_report_folder, "prevtx_cf_region_", Sys.Date(), ".csv"),
          na = "",
          row.names = FALSE)
