# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Tables in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Tom Hiatt
# 6 July 2012, updated July 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Diagnosis and treatment of TB, HIV-associated TB and drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table 4.1  ------
# Notifications of TB, TB/HIV and MDR/RR-TB cases, globally and for WHO regions, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


notifs_summary <- filter(notification, year == (report_year - 1)) %>%
                  select(g_whoregion,
                         c_notified,
                         c_newinc,
                         new_labconf, new_clindx, new_ep,
                         ret_rel_labconf, ret_rel_clindx, ret_rel_ep,
                         newrel_hivpos,
                         conf_rrmdr,
                         all_conf_xdr) %>%

                  # calculate regional aggregates
                  group_by(g_whoregion) %>%
                  summarise_at(vars(c_notified,
                                    c_newinc,
                                    new_labconf, new_clindx, new_ep,
                                    ret_rel_labconf, ret_rel_clindx, ret_rel_ep,
                                    newrel_hivpos,
                                    conf_rrmdr,
                                    all_conf_xdr),
                               sum,
                               na.rm = TRUE)

# merge with regional names
notifs_summary <- notifs_summary %>%
                  inner_join(who_region_names) %>%
                  arrange(g_whoregion) %>%
                  select(-g_whoregion)


# Add global summary to the regional summary
notifs_global <- notifs_summary %>%
                 summarise_at(vars(-entity),
                              sum,
                              na.rm = TRUE) %>%
                 mutate(entity="Global")


notifs_summary <- rbind(notifs_summary, notifs_global)


# Calculate total pulmonary and %ages that are bac confirmed and that are extrapulmonary
notifs_summary <- notifs_summary %>%
                  mutate( newrel_pulm = new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx,
                          newrel_pulm_conf_pct = (new_labconf + ret_rel_labconf) * 100
                                                  /
                                                  (new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx),
                          newrel_ep_pct = (new_ep + ret_rel_ep) * 100
                                          /
                                          (c_newinc)
                        ) %>%
                  # Restrict to variables needed in the final output
                  select(entity,
                         c_notified,
                         c_newinc,
                         newrel_pulm,
                         newrel_pulm_conf_pct,
                         newrel_ep_pct,
                         newrel_hivpos,
                         conf_rrmdr,
                         all_conf_xdr)


# format the data
notifs_summary <- notifs_summary %>%
                  mutate_at(vars(-entity),
                            rounder)


# Add % symbol to pct fields
notifs_summary <- notifs_summary %>%
                  mutate(newrel_pulm_conf_pct = paste0(newrel_pulm_conf_pct, "%"),
                         newrel_ep_pct = paste0(newrel_ep_pct, "%"))

# Create HTML output
notif_table_html <- xtable(notifs_summary)

digits(notif_table_html) <- 0

notif_table_filename <- paste0(figures_folder, "/Tables/t4_1_notifs_summary", Sys.Date(), ".htm")

cat(paste("<h3>Table 4.1<br />Notifications of TB, HIV-positive TB and MDR/RR-TB cases, globally and for WHO regions,",
          report_year-1,
          "</h3>"),
    file=notif_table_filename)

print(notif_table_html,
      type="html",
      file=notif_table_filename,
      include.rownames=FALSE,
      include.colnames=FALSE,
      html.table.attributes="border='0' rules='rows' width='1100' cellpadding='5'",
      append=TRUE,
      add.to.row=list(pos=list(0,
                               nrow(notif_table_html)),
                      command=c("<tr>
                                  <td rowspan='2'></td>
                                  <td rowspan='2' style='border-right: black 2px solid;'>Total notified</td>
                                  <td rowspan='2'>New and relapse<sup>a</sup></td>
                                  <td colspan='2'>Pulmonary new and relapse</td>
                                  <td rowspan='2' style='border-right: black 2px solid;'>Extrapulmonary<br />new and relapse (%)</td>
                                  <td rowspan='2' style='border-right: black 2px solid;'>HIV-positive<br /> new and relapse</td>
                                  <td rowspan='2'>MDR/RR-TB</td>
                                  <td rowspan='2'>XDR-TB</td>
                              </tr>
                              <tr>
                                  <td>Number</td>
                                  <td>Of which<br />bacteriologically<br />confirmed (%)</td>
                              </tr>",
                              "<tr><td colspan='9'><sup>a</sup> <i>New and relapse</i> includes cases for which the treatment history is unknown. It excludes cases that have been re-registered as <i>treatment after failure</i>, as <i>treatment after lost to follow up</i> or as <i>other previously treated</i> (whose outcome after the most recent course of treatment is unknown or undocumented).</td>
                              </tr>")
                      )
      )


# and now clear up the mess left behind
rm(list=c("notifs_global", "notifs_summary", "notif_table_html", "notif_table_filename"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table 4.2  ------
# Number of people newly enrolled in HIV care in 2016 who were also notified as a TB case in 2016, xx high TB/HIV burden countries that reported data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get list of high-burden TB/HIV countries

hiv_countries <-  country_group_membership %>%
                   filter(group_type == "g_hb_tbhiv" & group_name == 1) %>%
                   select(iso2)

# Get the data for the table

hiv_data <- notification %>%
            filter(year == report_year - 1 & iso2 %in% hiv_countries$iso2) %>%
            select(country,
                   hiv_reg_new2,
                   hiv_tbdetect
                   ) %>%

            # remove countries with no data
            filter(!(is.na(hiv_tbdetect) & is.na(hiv_reg_new2))) %>%

            # order by country
            arrange(country)

# Calculate the total and append to the end
hiv_total <- hiv_data %>%
             summarise_at(vars(-country),
                          sum,
                          na.rm = TRUE) %>%
                  mutate(country="Total")

hiv_data <- rbind(hiv_data, hiv_total) %>%

            # Calculate the percentages
            mutate(detected_pct = display_num(hiv_tbdetect * 100 / hiv_reg_new2))


# Create HTML output
hiv_table_html <- xtable(hiv_data)

digits(hiv_table_html) <- 0

hiv_table_filename <- paste0(figures_folder, "/Tables/t4_2_tb_in_hiv", Sys.Date(), ".htm")

cat(paste0("<h3>Table 4.2<br />Number of people newly enrolled in HIV care in ",
          report_year - 1,
          " who were also notified as a TB case in ",
          report_year - 1,
          ", ",
          nrow(hiv_data) - 1,
          " high TB/HIV burden countries that reported data.</h3>"),
    file=hiv_table_filename)


print(hiv_table_html,
      type="html",
      file=hiv_table_filename,
      include.rownames=FALSE,
      include.colnames=FALSE,
      html.table.attributes="border='0' rules='rows' width='800' cellpadding='5'",
      append=TRUE,
      add.to.row=list(pos=list(0),
                      command=c("<tr>
                                  <td style='width: 150px;'>&nbsp;</td>
                                  <td>Number of people newly enrolled in HIV care</td>
                                  <td>Number notified as a TB case</td>
                                  <td>Notified TB cases as a percentage of those newly enrolled in HIV care </td>
                              </tr>")
                      )
      )


# Clean up (remove any objects with their name beginning with 'hiv')
rm(list=ls(pattern = "^hiv"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table 4.3  ------
# National policies and their implementation to increase access to rapid TB testing and universal DST, 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get list of all high-burden countries (TB, TB/HIV, MDR-TB)
# and re-create old group structure (one variable per high burden group)

rdxpolicy_hbccodes <-  country_group_membership %>%
                       filter(  (group_type %in% c("g_hb_tb", "g_hb_tbhiv","g_hb_mdr")) &
                              group_name == 1) %>%
                       select(iso2, group_type, group_name) %>%
                       spread(key = group_type, value = group_name, fill = 0)


# Grab a few of the notification variables to calculate coverage of DST

rdxpolicy_notifs <- notification %>%
                    filter(year == report_year - 1) %>%
                    select(iso2,

                           new_labconf,
                           c_ret,
                           c_newinc,

                           rdx_data_available,
                           newinc_rdx,
                           rdxsurvey_newinc,
                           rdxsurvey_newinc_rdx,

                           rdst_new,
                           rdst_ret,

                           conf_rrmdr,
                           rr_sldst) %>%

                    # restrict to high burden countries
                    inner_join(rdxpolicy_hbccodes) %>%

                    # Calculate coverage percentages
                    mutate(pcnt_wrd = ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                                             display_num(newinc_rdx * 100 /c_newinc),
                                      ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                             display_num(rdxsurvey_newinc_rdx * 100 /rdxsurvey_newinc), "-")),

                           # percent DST among lab-confirmed cases is a bit of a fudge:
                           # numerator rdst_new + rdst_ret  (ignore cases with unknown treatment history)
                           # denominator is a bit of a fudge: new_labconf + c_ret
                           # Can sometimes get numerator > denominator so display ">100" in those cases
                           pcnt_dst = ifelse( (NZ(rdst_new) + NZ(rdst_ret)) > (NZ(new_labconf) + NZ(c_ret)),
                                              ">100",
                                      ifelse(NZ(new_labconf) + NZ(c_ret) > 0 & !
                                               (is.na(rdst_new) & is.na(rdst_ret)),
                                             display_num((NZ(rdst_new) + NZ(rdst_ret)) * 100 /
                                                           (NZ(new_labconf) + NZ(c_ret))),
                                             "-")),

                           pcnt_sldst = ifelse(NZ(conf_rrmdr)> 0 & !is.na(rr_sldst),
                                               display_num(rr_sldst * 100 / conf_rrmdr), "-")

                           )



rdxpolicy_country <- strategy %>%
                     filter(year == report_year - 1) %>%
                     select(country,
                            iso2,
                            g_whoregion,
                            wrd_initial_test,
                            universal_dst) %>%

                     # restrict to high burden countries, and add notification calculations
                     inner_join(rdxpolicy_notifs) %>%

                     # restrict to variables for the table
                     select(entity = country,
                            g_whoregion,
                            g_hb_tb,
                            g_hb_tbhiv,
                            g_hb_mdr,
                            wrd_initial_test,
                            pcnt_wrd,
                            universal_dst,
                            pcnt_dst,
                            pcnt_sldst) %>%
                      # shorten long country names
                      get_names_for_tables( col = "entity") %>%
                      # order by country name
                      arrange(entity)



# - - - - - - - - - - - - - - - - - - - - - - -
# Define function to calculate aggregates (% of non-empty responses that are == 1)
# Results in string of the form n/n=n%
rdxpolicy_pcnt  <- function(x, show_calc = FALSE){

  if (show_calc == TRUE)
    {
    paste0(sum(x, na.rm = TRUE),
           "/",
           sum(ifelse(is.na(x),0,1)),
           "=",
           round(sum(x, na.rm = TRUE) * 100
                 /
                 (sum(ifelse(is.na(x),0,1)))),
           "%")
    }
  else
    {  paste0(round(sum(x, na.rm = TRUE) * 100
               /
               (sum(ifelse(is.na(x),0,1)))),
         "%")
    }
}
# - - - - - - - - - - - - - - - - - - - - - - -


# calculate aggregates (% of non-empty responses that are 1 out of the 30 countries in each HB list)
# For use by Wayne, not to be shown in report table
rdxpolicy_g_hb_tb <- rdxpolicy_country %>%
                     filter(g_hb_tb == 1) %>%
                     summarise_at(c("wrd_initial_test", "universal_dst"),
                                  rdxpolicy_pcnt,
                                  show_calc = TRUE) %>%
                     mutate(entity="High TB burden countries")

rdxpolicy_g_hb_tbhiv <- rdxpolicy_country %>%
                        filter(g_hb_tbhiv == 1) %>%
                        summarise_at(c("wrd_initial_test", "universal_dst"),
                                     rdxpolicy_pcnt,
                                     show_calc = TRUE) %>%
                        mutate(entity="High TB/HIV burden countries")


rdxpolicy_g_hb_mdr <- rdxpolicy_country %>%
                      filter(g_hb_mdr == 1) %>%
                      summarise_at(c("wrd_initial_test", "universal_dst"),
                                   rdxpolicy_pcnt,
                                   show_calc = TRUE) %>%
                      mutate(entity="High MDR burden countries")


rdxpolicy_aggs <- rbind(rdxpolicy_g_hb_tb,
                        rdxpolicy_g_hb_tbhiv,
                        rdxpolicy_g_hb_mdr) %>%
                  # reorder columns for Wayne
                  select(entity, wrd_initial_test, universal_dst)


# Restrict country data to the high burden countries
rdxpolicy_hbcs <- rdxpolicy_country %>%
                  select(-g_whoregion) %>%
                  filter(g_hb_tb == 1 | g_hb_tbhiv == 1 | g_hb_mdr == 1)



# Create HTML output
rdxpolicy_table_html <- xtable(rdxpolicy_hbcs)

rdxpolicy_table_filename <- paste0(figures_folder, "/Tables/t4_3_lab_policy", Sys.Date(), ".htm")

cat(paste("<h3>Table 4.3<br />National policies and their implementation to increase access to rapid TB testing and universal DST <sup>a</sup>, ",
          report_year-1,
          "</h3>
          <style>
            table td {text-align:center;}
            table td:first-child {text-align: left;}
          </style>"),
    file=rdxpolicy_table_filename)

print(rdxpolicy_table_html,
      type="html",
      file=rdxpolicy_table_filename,
      include.rownames=FALSE,
      include.colnames=FALSE,
      html.table.attributes="border='0' rules='rows' width='1100' cellpadding='5'",
      append=TRUE,
      add.to.row=list(pos=list(0,
                               nrow(rdxpolicy_table_html)),
                      command=c("<tr>
                                  <td>&nbsp;</td>
                                  <td>High TB burden</td>
                                  <td>High TB/HIV burden</td>
                                  <td>High MDR-TB burden</td>                                                                    <td>National policy and algorithm indicate a WRD as the initial diagnostic test for all people presumed to have TB</td>
                                  <td>Percentage of notified new and relapse TB cases tested with a WRD as the initial diagnostic test</td>
                                  <td>National policy and algorithm indicate universal access to DST</td>
                                  <td>Percentage of notified bacteriologically confirmed TB cases with DST results for rifampicin <sup>b</sup></td>
                                  <td>Percentage of notified rifampicin-resistant TB cases with DST results for fluoroquinolones and second-line injectable agents</td>
                              </tr>",
                              "<tr><td colspan='9'><sup>a</sup>The 48 countries shown in the table are the countries that are in one or more of the three lists of high TB, TB/HIV and MDR-TB burden countries (see also Chapter 2, Figure 2.2 and Table 2.4).<br /><sup>b</sup>Testing in cases with unknown previous treatment history is not included. The percentage may exceed 100% for several reasons, e.g. samples rather than cases are counted in the numerator; laboratory specimen results are not linked to the denominator data source when enumerated; or there is incomplete reporting of bacteriologically confirmed cases in the denominator. Bacteriologically confirmed extrapulmonary cases are not included in the denominator because they cannot be differentiated from clinically diagnosed ones in the way data are reported to WHO.</td>
                              </tr>")
                      )
      )


# Export the aggregate stats for Wayne to use
write.csv(rdxpolicy_aggs,
          file=paste0(figures_folder, "/Tables/t4_3_lab_policy_agg_for_wayne", Sys.Date(), ".csv"),
          row.names=FALSE,
          na="")



# Clean up (remove any objects with their name beginning with 'rdxpolicy')
rm(list=ls(pattern = "^rdxpolicy"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# TB prevention services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table 5.1 -------
# TB preventive treatment for people living with HIV and children under
# 5 years of age who were household contacts of a bacteriologically confirmed pulmonary TB case,
# xx high TB or TB/HIV burden countries that reported data, WHO regions and globally, 2017
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


# Get list of TB and TB/HIV high-burden countries
prev_tx_hbccodes <-  country_group_membership %>%
                      filter(  (group_type %in% c("g_hb_tb", "g_hb_tbhiv")) &
                                 group_name == 1) %>%
                      select(iso2, originalname = country) %>%
                      distinct(iso2, originalname)


prev_tx_data <- notification %>%
                filter(year == report_year -1) %>%
                select(iso2,
                       country,
                       hiv_ipt,
                       hiv_reg_new,
                       hiv_ipt_reg_all,
                       hiv_reg_all)

# Add an asterisk to country names where IPT is reported for all patients under care instead of only
# the newly enrolled ones
prev_tx_data <- prev_tx_data %>%
                mutate(country = ifelse(!is.na(hiv_ipt_reg_all) & !is.na(hiv_reg_all),
                                        paste0(country, "*"),
                                        country))

# Create numerators and denominators for the IPT coverage data
prev_tx_data <- prev_tx_data %>%
                mutate(ipt_numerator = ifelse(!is.na(hiv_ipt_reg_all) & !is.na(hiv_reg_all),
                                              hiv_ipt_reg_all,
                                              hiv_ipt),
                       ipt_denominator = ifelse(!is.na(hiv_ipt_reg_all) & !is.na(hiv_reg_all),
                                                hiv_reg_all,
                                                hiv_reg_new))

# Calculate IPT coverage, but only if a country has reported for newly enrolled patients
prev_tx_data <- prev_tx_data %>%
                mutate(coverage = ifelse(!is.na(hiv_ipt) & !is.na(hiv_reg_new) & NZ(ipt_denominator) > 0,
                                         display_num(ipt_numerator * 100 / ipt_denominator),
                                         NA))

# Add a dash marker in coverage if a country reported for all enrolled in care
prev_tx_data <- prev_tx_data %>%
                mutate(coverage = ifelse(!is.na(hiv_ipt_reg_all) & !is.na(hiv_reg_all),
                                         "-",
                                         coverage))


# Merge with list of countries in HB TB or TB/HIV
prev_tx_data <- prev_tx_data %>%
                inner_join(prev_tx_hbccodes)

# Merge with the LTBI estimates
prev_tx_data <- estimates_ltbi %>%
                filter(year == report_year -1) %>%
                select(iso2,
                       e_prevtx_eligible,
                       e_prevtx_eligible_lo,
                       e_prevtx_eligible_hi,
                       newinc_con04_prevtx,
                       e_prevtx_kids_pct,
                       e_prevtx_kids_pct_lo,
                       e_prevtx_kids_pct_hi
                      ) %>%
                inner_join(prev_tx_data)


# Restrict to countries with data
prev_tx_data <- prev_tx_data %>%
                filter(!is.na(coverage) | !is.na(e_prevtx_kids_pct))


# Calculate number of reporting high burden countries and number not reported
prev_tx_reported <- nrow(prev_tx_data)
prev_tx_not_reported <- nrow(prev_tx_hbccodes) - prev_tx_reported

# Create list of country names that have not reported
prev_tx_countries_not_reported <-  prev_tx_hbccodes %>%
                                    anti_join(prev_tx_data, by = "iso2")


# %>%
#                                     select(originalname) %>%
#                                     str_flatten(collapse = ", ")


prev_tx_countries_not_reported <- str_flatten(prev_tx_countries_not_reported$originalname, collapse = ", ")

# Format for output
prev_tx_data <- prev_tx_data %>%
                mutate(e_prev_tx_eligible_lohi = display_intervals(e_prevtx_eligible,
                                                                    e_prevtx_eligible_lo,
                                                                    e_prevtx_eligible_hi),

                       e_prevtx_kids_pct_lohi = display_intervals(e_prevtx_kids_pct,
                                                                  e_prevtx_kids_pct_lo,
                                                                  e_prevtx_kids_pct_hi)) %>%

                mutate_at(c("ipt_numerator",
                            "ipt_denominator",
                            "e_prevtx_eligible",
                            "newinc_con04_prevtx",
                            "e_prevtx_kids_pct"),
                          display_num) %>%

                select(country,
                       ipt_numerator,
                       ipt_denominator,
                       coverage,
                       e_prevtx_eligible,
                       e_prev_tx_eligible_lohi,
                       newinc_con04_prevtx,
                       e_prevtx_kids_pct,
                       e_prevtx_kids_pct_lohi)




# Create HTML output
prev_tx_table_html <- xtable(prev_tx_data)

prev_tx_table_filename <- paste0(figures_folder, "/Tables/t5_1_prev_tx", Sys.Date(), ".htm")

cat(paste("<h3>Table 5.1<br />TB preventive treatment for people living with HIV and children",
          "under 5 years of age who were household contacts of a bacteriologically confirmed pulmonary TB case,",
          prev_tx_reported,
          "high TB or TB/HIV burden countries that reported data<sup>a</sup>, WHO regions and globally",
          report_year-1,
          "</h3>"),
    file=prev_tx_table_filename)

print(prev_tx_table_html,
      type="html",
      file=prev_tx_table_filename,
      include.rownames=FALSE,
      include.colnames=FALSE,
      html.table.attributes="border='0' rules='rows' width='1100' cellpadding='5'",
      append=TRUE,
      add.to.row=list(pos=list(0,
                               nrow(prev_tx_table_html)),
                      command=c("<tr>
                                <td rowspan='3'></td>
                                <td rowspan='3'>People living with HIV newly enrolled in care</td>
                                <td colspan='2'>People living with HIV newly enrolled in care started on TB preventive treatment</td>
                                <td colspan='2' style='border-left: black 2px solid;'>Estimated number of child contacts under 5 years of age eligible for TB preventive treatment</td>
                                <td colspan='3'>Childrenunder 5 years of age started on TB preventive treatment</td>
                                </tr>
                                <tr>
                                <td rowspan='2'>Number</td>
                                <td rowspan='2'>Coverage (%)</td>
                                <td rowspan='2' style='border-left: black 2px solid;'>Best estimate</td>
                                <td rowspan='2'>Uncertainty interval</td>
                                <td rowspan='2'>Number</td>
                                <td colspan='2'>Coverage (%)</td>
                                </tr>
                                <tr>
                                <td>Best estimate</td>
                                <td>Uncertainty interval</td>
                                </tr>",
                                paste("<tr><td colspan='7'>* Reported data for all enrolled in care, not just newly enrolled in care.
                                <br /><sup>a</sup> There were",
                                      prev_tx_not_reported,
                                      "other countries in the list of high TB or TB/HIV burden countries that did not report data for either risk group. These were: ",
                                      prev_tx_countries_not_reported,
                                      ".</td>
                                </tr>"))
                      )
      )

# Clean up (remove any objects with their name beginning with 'prev_tx')
rm(list=ls(pattern = "^prev_tx"))
