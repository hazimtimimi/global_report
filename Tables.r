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
                  summarise_each(funs(sum(., na.rm = TRUE)),
                                 c_notified,
                                 c_newinc,
                                 new_labconf, new_clindx, new_ep,
                                 ret_rel_labconf, ret_rel_clindx, ret_rel_ep,
                                 newrel_hivpos,
                                 conf_rrmdr,
                                 all_conf_xdr)

# merge with regional names
notifs_summary <- notifs_summary %>%
                  inner_join(who_region_names) %>%
                  arrange(g_whoregion) %>%
                  select(-g_whoregion)


# Add global summary to the regional summary
notifs_global <- notifs_summary %>%
                 summarise_each(funs(sum(., na.rm = TRUE)),
                                -entity) %>%
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
for(var in 2:ncol(notifs_summary)){
  notifs_summary[var] <- rounder(notifs_summary[[var]])
}

# Add % symbol to pct fields
notifs_summary <- notifs_summary %>%
                  mutate(newrel_pulm_conf_pct = paste0(newrel_pulm_conf_pct, "%"),
                         newrel_ep_pct = paste0(newrel_ep_pct, "%"))

# Create HTML output
notif_table_html <- xtable(notifs_summary)

digits(notif_table_html) <- 0

notif_table_filename <- paste0(figures_folder, "Tables/t4_1_notifs_summary", Sys.Date(), ".htm")

cat(paste("<h3>Table 4.1<br />Notifications of TB, TB/HIV and MDR/RR-TB cases, globally and for WHO regions,",
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

tablecopy("t4_1_notifs_summary")


# and now clear up the mess left behind
rm(list=c("notifs_global", "notifs_summary", "notif_table_html", "notif_table_filename"))


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

                           c_notified,
                           c_newinc,

                           rdx_data_available,
                           newinc_rdx,
                           rdxsurvey_newinc,
                           rdxsurvey_newinc_rdx,

                           rdst_new,
                           rdst_ret,
                           rdst_unk,

                           conf_rrmdr,
                           rr_sldst) %>%

                    # restrict to high burden countries
                    inner_join(rdxpolicy_hbccodes) %>%

                    # Calculate coverage percentages
                    mutate(pcnt_wrd = ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                                             frmt(newinc_rdx * 100 /c_newinc),
                                      ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                             frmt(rdxsurvey_newinc_rdx * 100 /rdxsurvey_newinc), "-")),


                           pcnt_dst = ifelse(NZ(c_notified) > 0 & !
                                               (is.na(rdst_new) & is.na(rdst_ret) & is.na(rdst_unk)),
                                             frmt((NZ(rdst_new) + NZ(rdst_ret) + NZ(rdst_unk)) * 100 / c_notified),
                                             "-"),

                           pcnt_sldst = ifelse(NZ(conf_rrmdr)> 0,
                                               frmt(rr_sldst * 100 / conf_rrmdr), "-")

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
rdxpolicy_g_hb_tb <- rdxpolicy_country %>%
                     filter(g_hb_tb == 1) %>%
                     summarise_each(funs(rdxpolicy_pcnt(.)),
                                  one_of(c("wrd_initial_test", "universal_dst"))) %>%
                     mutate(entity="High TB burden countries")

rdxpolicy_g_hb_tbhiv <- rdxpolicy_country %>%
                     filter(g_hb_tbhiv == 1) %>%
                     summarise_each(funs(rdxpolicy_pcnt(.)),
                                  one_of(c("wrd_initial_test", "universal_dst"))) %>%
                     mutate(entity="High TB/HIV burden countries")


rdxpolicy_g_hb_mdr <- rdxpolicy_country %>%
                     filter(g_hb_mdr == 1) %>%
                     summarise_each(funs(rdxpolicy_pcnt(.)),
                                  one_of(c("wrd_initial_test", "universal_dst"))) %>%
                     mutate(entity="High MDR burden countries")


rdxpolicy_aggs <- rbind(rdxpolicy_g_hb_tb,
                        rdxpolicy_g_hb_tbhiv,
                        rdxpolicy_g_hb_mdr) %>%
                  # Add placeholder variables to match structure of country list
                  mutate(g_hb_tb = NA,
                         g_hb_tbhiv = NA,
                         g_hb_mdr = NA,
                         pcnt_wrd = NA,
                         pcnt_dst = NA,
                         pcnt_sldst = NA)


# Restrict country data to the high burden countries and then append aggregates
rdxpolicy_hbcs <- rdxpolicy_country %>%
                  select(-g_whoregion) %>%
                  filter(g_hb_tb == 1 | g_hb_tbhiv == 1 | g_hb_mdr == 1) %>%
                  rbind(rdxpolicy_aggs)


# Create HTML output
rdxpolicy_table_html <- xtable(rdxpolicy_hbcs)

rdxpolicy_table_filename <- paste0(figures_folder, "Tables/t4_4_lab_policy", Sys.Date(), ".htm")

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
                                  <td>Percentage of notified TB cases with DST results for rifampicin</td>
                                  <td>Percentage of notified rifampicin-resistant TB cases with DST results for fluoroquinolones and second-line injectable agents</td>
                              </tr>",
                              "<tr><td colspan='9'><sup>a</sup>The 48 countries shown in the table are the countries that are in one of more of the three lists of high TB, TB/HIV and MDR-TB burden countries (see also Chapter 2, Figure 2.2 and Table 2.3).</td>
                              </tr>")
                      )
      )

tablecopy("t4_3_xpert_policy")

# Clean up (remove any objects with their name beginning with 'rdxpolicy')
rm(list=ls(pattern = "^rdxpolicy"))

stop("

     >>>>>>>>>>
     Stopping here for testing!
     <<<<<<<<<<<<")


