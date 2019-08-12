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
# Number of people newly enrolled in HIV care in 2018 who were also notified as a TB case in 2016, xx high TB/HIV burden countries that reported annual data
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
            filter(!(is.na(hiv_tbdetect) | is.na(hiv_reg_new2))) %>%

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
            mutate(detected_pct = display_num(hiv_tbdetect * 100 / hiv_reg_new2))%>%
            # format the data
            mutate_at(vars(hiv_reg_new2,
                           hiv_tbdetect),
                           rounder)

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
          " high TB/HIV burden countries that reported annual data</h3>"),
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
                            universal_dst,
                            #urine LAM added 2019 dcyear
                            lf_urine_lam) %>%

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
                            pcnt_sldst,
                            lf_urine_lam) %>%
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
                     summarise_at(c("wrd_initial_test", "universal_dst", "lf_urine_lam"),
                                  rdxpolicy_pcnt,
                                  show_calc = TRUE) %>%
                     mutate(entity="High TB burden countries")

rdxpolicy_g_hb_tbhiv <- rdxpolicy_country %>%
                        filter(g_hb_tbhiv == 1) %>%
                        summarise_at(c("wrd_initial_test", "universal_dst", "lf_urine_lam"),
                                     rdxpolicy_pcnt,
                                     show_calc = TRUE) %>%
                        mutate(entity="High TB/HIV burden countries")


rdxpolicy_g_hb_mdr <- rdxpolicy_country %>%
                      filter(g_hb_mdr == 1) %>%
                      summarise_at(c("wrd_initial_test", "universal_dst", "lf_urine_lam"),
                                   rdxpolicy_pcnt,
                                   show_calc = TRUE) %>%
                      mutate(entity="High MDR burden countries")


rdxpolicy_aggs <- rbind(rdxpolicy_g_hb_tb,
                        rdxpolicy_g_hb_tbhiv,
                        rdxpolicy_g_hb_mdr) %>%
                  # reorder columns for Wayne
                  select(entity, wrd_initial_test, universal_dst, lf_urine_lam)


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
                                  <td>High MDR-TB burden</td>
                                  <td>National policy and algorithm indicate a WRD as the initial diagnostic test for all people presumed to have TB</td>
                                  <td>Percentage of notified new and relapse TB cases tested with a WRD as the initial diagnostic test</td>
                                  <td>National policy and algorithm indicate universal access to DST</td>
                                  <td>Percentage of notified bacteriologically confirmed TB cases with DST results for rifampicin <sup>b</sup></td>
                                  <td>Percentage of notified rifampicin-resistant TB cases with DST results for fluoroquinolones and second-line injectable agents</td>
                                  <td>National policy and algorithm indicate the use of lateral flow urine lipoarabinomannan assay (LF-LAM) to assist in the detection of TB in people living with HIV</td>
                              </tr>",
                              "<tr><td colspan='10'>Blank cells indicate data not reported. - Indicates value that cannot be calculated.<br /><sup>a</sup>The 48 countries shown in the table are the countries that are in one or more of the three lists of high TB, TB/HIV and MDR-TB burden countries (see also Chapter 2, Figure 2.5 and Table 2.3).<br /><sup>b</sup>Testing in cases with unknown previous treatment history is not included. The percentage may exceed 100% for several reasons, e.g. samples rather than cases are counted in the numerator; laboratory specimen results are not linked to the denominator data source when enumerated; or there is incomplete reporting of bacteriologically confirmed cases in the denominator. Bacteriologically confirmed extrapulmonary cases are not included in the denominator because they cannot be differentiated from clinically diagnosed ones in the way data are reported to WHO.</td>
                              </tr>")
                      )
      )


# Export the aggregate stats for Wayne to use
write.csv(rdxpolicy_aggs,
          file=paste0(figures_folder, "/Tables/t4_3_lab_policy_aggregates_", Sys.Date(), ".csv"),
          row.names=FALSE,
          na="")



# Clean up (remove any objects with their name beginning with 'rdxpolicy')
rm(list=ls(pattern = "^rdxpolicy"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table 4.4 -------
# Quality of laboratory services, 2018
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get list of all high-burden countries (TB, TB/HIV, MDR-TB)
# and re-create old group structure (one variable per high burden group)

labquality_hbccodes <-  country_group_membership %>%
                        filter(  (group_type %in% c("g_hb_tb", "g_hb_tbhiv","g_hb_mdr")) &
                               group_name == 1) %>%
                        select(iso2, group_type, group_name) %>%
                        spread(key = group_type, value = group_name, fill = 0)

# Get the lab variables
labquality_data <- strategy %>%
                   filter(year == report_year - 1) %>%
                   select(country,
                          iso2,
                          nrl_iso15189,
                          smear, smear_eqa,
                          xpert, xpert_eqa,
                          dst, dst_eqa_pass,
                          sldst, sldst_eqa_pass,
                          lpa, lpa_eqa_pass,
                          sllpa, sllpa_eqa_pass) %>%

                    # restrict to high burden countries
                    inner_join(labquality_hbccodes)


# Define funtion to calculate and display EQA coverage
labquality_pcnt_eqa <- function(test, test_eqa){

  ifelse(NZ(test) > 0,
         display_num(test_eqa * 100 /test),
         "-")
  }


# Calculate the EQA indicators
labquality_data <- labquality_data %>%
                   mutate(pcnt_smear_eqa = labquality_pcnt_eqa(smear, smear_eqa),
                          pcnt_xpert_eqa = labquality_pcnt_eqa(xpert, xpert_eqa),
                          pcnt_dst_eqa_pass   = labquality_pcnt_eqa(dst, dst_eqa_pass),
                          pcnt_sldst_eqa_pass = labquality_pcnt_eqa(sldst, sldst_eqa_pass),
                          pcnt_lpa_eqa_pass   = labquality_pcnt_eqa(lpa, lpa_eqa_pass),
                          pcnt_sllpa_eqa_pass = labquality_pcnt_eqa(sllpa, sllpa_eqa_pass)) %>%

                    # restrict to variables for the table
                    select(entity = country,
                           nrl_iso15189,
                           starts_with("pcnt_")) %>%
                    # shorten long country names
                    get_names_for_tables( col = "entity") %>%
                    # order by country name
                    arrange(entity)


# Create HTML output
labquality_table_html <- xtable(labquality_data)

labquality_table_filename <- paste0(figures_folder, "/Tables/t4_4_lab_quality", Sys.Date(), ".htm")

cat(paste("<h3>Table 4.4<br />Quality of laboratory services <sup>a</sup>, ",
          report_year-1,
          "</h3>
          <style>
            table td {text-align:center;}
            table td:first-child {text-align: left;}
          </style>"),
    file=labquality_table_filename)

print(labquality_table_html,
      type="html",
      file=labquality_table_filename,
      include.rownames=FALSE,
      include.colnames=FALSE,
      html.table.attributes="border='0' rules='rows' width='1100' cellpadding='5'",
      append=TRUE,
      add.to.row=list(pos=list(0,
                               nrow(labquality_table_html)),
                      command=c("
                              <tr>
                                  <td rowspan='2'>&nbsp;</td>
                                  <td rowspan='2' style='border-right: black 2px solid;'>National Reference Laboratory accredited according to the ISO 15189 standard</td>
                                  <td colspan='2' style='border-right: black 2px solid;'>Percentage of testing sites that are covered by a comprehensive EQA system</td>
                                  <td colspan='4'>Percentage of testing sites that demonstrated proficiency by panel testing</td>
                              </tr>
                              <tr>
                                  <td>Smear microscopy</td>
                                  <td style='border-right: black 2px solid;'>Xpert MTB/Rif</td>
                                  <td>Phenotypic DST for first-line drugs only</td>
                                  <td>Phenotypic DST for first-line and second-line drugs</td>
                                  <td>LPA for rifampicin and isoniazid only</td>
                                  <td>LPA for rifampicin, isoniazid, fluroguinolones and second-line injectables</td>
                              </tr>",
                              "<tr><td colspan='8'>Blank cells indicate data not reported. - Indicates value that cannot be calculated.<br />
                              <sup>a</sup>The 48 countries shown in the table are the countries that are in one or more of the three lists of high TB, TB/HIV and MDR-TB burden countries (see also Chapter 2, Figure 2.5 and Table 2.4).</td>
                              </tr>")
                      )
      )

# Clean up (remove any objects with their name beginning with 'labquality')
rm(list=ls(pattern = "^labquality"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# TB prevention services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table 5.1 -------
# TB preventive treatment for people living with HIV and children under
# 5 years of age who were household contacts of a bacteriologically confirmed pulmonary TB case,
# high TB or TB/HIV burden countries, 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
         hiv_reg_all)%>%
  mutate(hiv_ipt_pall = ifelse(!is.na(hiv_ipt_reg_all) & !is.na(hiv_reg_all),
                               hiv_ipt_reg_all,
                               hiv_ipt))

# Create numerators and denominators for the IPT coverage data
#prev_tx_data <- prev_tx_data %>%
#  mutate(ipt_numerator = ifelse(!is.na(hiv_ipt_reg_all) & !is.na(hiv_reg_all),
#                                hiv_ipt_reg_all,
#                                hiv_ipt),
#         ipt_denominator = ifelse(!is.na(hiv_ipt_reg_all) & !is.na(hiv_reg_all),
#                                  hiv_reg_all,
#                                  hiv_reg_new))

# Calculate IPT coverage, but only if a country has reported for newly enrolled patients
prev_tx_data <- prev_tx_data %>%
  mutate(coverage = ifelse(!is.na(hiv_ipt) & NZ(hiv_reg_new) > 0,
                           display_num(hiv_ipt * 100 / hiv_reg_new),
                           NA))

# Add an asterisk marker in coverage if a country reported for all enrolled in care
prev_tx_data <- prev_tx_data %>%
  mutate(coverage = ifelse(!is.na(hiv_ipt_reg_all) & !is.na(hiv_reg_all) & is.na(hiv_reg_new)& is.na(hiv_ipt),
                           "*",
                           coverage)
  )


# Merge with the LTBI estimates
prev_tx_data <- estimates_ltbi %>%
  filter(year == report_year -1) %>%
  select(iso2,
         e_prevtx_eligible,
         e_prevtx_eligible_lo,
         e_prevtx_eligible_hi,
         newinc_con04_prevtx,
         ptsurvey_newinc_con04_prevtx,
         e_prevtx_kids_pct,
         e_prevtx_kids_pct_lo,
         e_prevtx_kids_pct_hi) %>%
  mutate(newinc_con04_tpt = ifelse(!is.na(newinc_con04_prevtx),
                                   newinc_con04_prevtx,
                                   ptsurvey_newinc_con04_prevtx)) %>%
  # Note the ltbi estimates have not been produced for all countries
  right_join(prev_tx_data)


# Now produce the high burden data for the table

# Merge with list of countries in HB TB or TB/HIV
prev_tx_hb_data <- prev_tx_data %>%
  inner_join(prev_tx_hbccodes)



# Restrict the dataframe to the variables needed for the final table, shorten country names and order by country name
prev_tx_hb_data <- prev_tx_hb_data %>%
  select(entity = country,
         hiv_ipt_pall,
         hiv_reg_new,
         hiv_ipt,
         coverage,
         e_prevtx_eligible,
         e_prevtx_eligible_lo,
         e_prevtx_eligible_hi,
         newinc_con04_tpt,
         e_prevtx_kids_pct,
         e_prevtx_kids_pct_lo,
         e_prevtx_kids_pct_hi)  %>%
  
  # shorten long country names
  get_names_for_tables( col = "entity") %>%
  
  arrange(entity)


# Format variables for output
prev_tx_table_data <- prev_tx_hb_data %>%
  
  mutate(e_prevtx_eligible_lohi = display_intervals(e_prevtx_eligible,
                                                    e_prevtx_eligible_lo,
                                                    e_prevtx_eligible_hi),
         
         e_prevtx_kids_pct_lohi = display_intervals(e_prevtx_kids_pct,
                                                    e_prevtx_kids_pct_lo,
                                                    e_prevtx_kids_pct_hi)) %>%
  
  # format and round numbers
  mutate(hiv_ipt = rounder(hiv_ipt),
         hiv_reg_new = rounder(hiv_reg_new),
         newinc_con04_tpt = rounder(newinc_con04_tpt),
         e_prevtx_eligible = display_num(e_prevtx_eligible),
         e_prevtx_kids_pct = display_num(e_prevtx_kids_pct)) %>%
  
  
  # drop the separate *_lo and *_hi variables
  select(entity,
         hiv_reg_new,
         hiv_ipt,
         coverage,
         hiv_ipt_pall,
         e_prevtx_eligible,
         e_prevtx_eligible_lohi,
         newinc_con04_tpt,
         e_prevtx_kids_pct,
         e_prevtx_kids_pct_lohi)





# Finally we have a list of exclusions based on feedback from countries compiled by Lele
prev_tx_footnote2 <- "** Estimated coverage was not calculated because the numerator also included contacts aged 5 years or older (Botswana, DPR Korea and Nigeria), those who were
\nnon-household contacts of TB cases (Indonesia and the Russian Federation), or those household contacts of clinically diagnosed TB cases (Malawi and the
\nPhilippines)."

prev_tx_footnote2_countries = c("DPR Korea", "Nigeria", "Indonesia", "Malawi", "Philippines", "Botswana", "Russian Federation")

# Remove the coverage calculations for the excluded countries
prev_tx_table_data <- prev_tx_table_data %>%
  mutate(e_prevtx_kids_pct = ifelse(entity %in% prev_tx_footnote2_countries,
                                    "**",
                                    e_prevtx_kids_pct),
         
         e_prevtx_kids_pct_lohi = ifelse(entity %in% prev_tx_footnote2_countries,
                                         NA,
                                         e_prevtx_kids_pct_lohi),
         #For contries got a coverage greater than 100%, show it as >100 and delete the interval
         e_prevtx_kids_pct_lohi=replace(e_prevtx_kids_pct_lohi,(e_prevtx_kids_pct == 100),NA),
         e_prevtx_kids_pct=replace(e_prevtx_kids_pct,(e_prevtx_kids_pct == 100),">100"))
#Zimbabwe only reported half year's data for 2017, remove the whole row of it after discussed with Annabel;should add back since 2018
#filter(entity!="Zimbabwe")

# Create HTML output
prev_tx_table_html <- xtable(prev_tx_table_data)

prev_tx_table_filename <- paste0(figures_folder, "/Tables/t5_1_prev_tx", Sys.Date(), ".htm")

cat(paste("<h3>Table 5.1<br />TB preventive treatment for people living with HIV and children",
          "under 5 years of age who were household contacts of a bacteriologically confirmed pulmonary TB case,",
          "high TB or TB/HIV burden countries,",
          report_year-1,
          "</h3>"),
    file=prev_tx_table_filename)

print(prev_tx_table_html,
      type="html",
      file=prev_tx_table_filename,
      include.rownames=FALSE,
      include.colnames=FALSE,
      html.table.attributes="border='1' rules='all' width='1100' cellpadding='5'",
      append=TRUE,
      add.to.row=list(pos=list(0,
                               nrow(prev_tx_table_html)),
                      command=c("<tr>
                                <td rowspan='3'></td>
                                <td colspan='3'>People living with HIV newly enrolled in care</td>
                                <td rowspan='1' style='border-left: black 1px solid;'>People living with HIV currently enrolled in care</td>
                                <td colspan='2' style='border-left: black 1px solid;'>Estimated number of child contacts under 5 years of age eligible for TB preventive treatment<sup>a</sup></td>
                                <td colspan='3' style='border-left: black 1px solid;'>Children under 5 years of age started on TB preventive treatment</td>
                                </tr>
                                <tr>
                                <td rowspan='2'>Number</td>
                                <td rowspan='2'>Number of people started on TB preventive treatment</td>
                                <td rowspan='2'>Coverage (%)</td>
                                <td rowspan='2'style='border-left: black 1px solid;'>Number of people started on TB preventive treatment</td>
                                <td rowspan='2' style='border-left: black 1px solid;'>Best estimate</td>
                                <td rowspan='2'>Uncertainty interval</td>
                                <td rowspan='2' style='border-left: black 1px solid;'>Number</td>
                                <td colspan='2'>Coverage<sup>b</sup> (%)</td>
                                </tr>
                                <tr>
                                <td>Best estimate</td>
                                <td>Uncertainty interval</td>
                                </tr>",
                                paste("<tr><td colspan='10'>Blank cells indicate data not reported.<br />","<sup>a</sup> Estimates are shown to three significant figures.<br />","<sup>b</sup> Reasons for higher than expected coverage might be that the numerator did not exclude non-household contacts or children of five years and older.<br />",
                                      "* Coverage was not calculated because reported data on people living with HIV is for all enrolled in care, not just those newly enrolled in care.<br />",
                                      prev_tx_footnote2,
                                      "</td>
                                      </tr>"))
                                )
      )

# Clean up (remove any objects with their name beginning with 'prev_tx')
rm(list=ls(pattern = "^prev_tx"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Annex 1 ------
# The WHO global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table A1.1  ------
# Reporting of data in the 2018 round of global TB data collection
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

reporting_countries <- data_collection %>%
                        filter(datcol_year == report_year & dc_form_description != 'Not requested') %>%
                        select(iso3, g_whostatus)

reporting_rates <- notification %>%
                    filter(year == report_year - 1) %>%


                    # define reported as having a non-empty value for notified cases of TB, RR-TB or HIV-positive TB
                    mutate(reported = ifelse(is.na(c_notified) & is.na(conf_rrmdr) & is.na(newrel_hivpos),
                                             0,
                                             1)) %>%
                    select(iso3, g_whoregion, reported) %>%

                    inner_join(reporting_countries) %>%

                    # ignore the distinction between associate members and non members of WHO
                    mutate(g_whostatus = ifelse(g_whostatus == "M",
                                                "M",
                                                NA)) %>%


                    # calculate summaries by region and membership of WHO
                    group_by(g_whoregion, g_whostatus) %>%
                    summarise(total = n(),
                              reported = sum(reported, na.rm = TRUE) )

# Create summary for all countries and territores by region
reporting_rates_tot <- reporting_rates %>%
                        group_by(g_whoregion) %>%
                        summarise_at(vars(total, reported),
                                     sum,
                                     na.rm = TRUE)

# Create summary for WHO member states by region
reporting_rates_ms <- reporting_rates %>%
                      filter(g_whostatus == "M") %>%
                      select(g_whoregion,
                             total_ms = total,
                             reported_ms = reported)


# Combine the two summaries into one summary table
reporting_rates_tab <- reporting_rates_tot %>%
                        inner_join(reporting_rates_ms)


# Calculate a global summary
reporting_rates_glob <- reporting_rates_tab %>%
                          select(-g_whoregion) %>%
                          summarise_all(sum) %>%
                          mutate(g_whoregion = "global")

# Add the global summary to the summary table
reporting_rates_tab <- rbind(reporting_rates_tab, reporting_rates_glob)

# Merge with group names
reporting_rates_tab <- country_groups %>%
                        filter(group_type %in% c("global", "g_whoregion")) %>%
                        inner_join(reporting_rates_tab,
                                   by = c("group_name" = "g_whoregion" ) ) %>%

                        # Remove extra text from group descriptions
                        mutate(group_description = str_replace(group_description, "WHO ", "")) %>%
                        mutate(group_description = str_replace(group_description, "WHO/PAHO ", "")) %>%
                        mutate(group_description = str_replace(group_description, " Aggregate", "")) %>%
                        select(-group_type, -group_name)


# Create HTML output
reporting_html <- xtable(reporting_rates_tab)

digits(reporting_html) <- 0

reporting_filename <- paste0(figures_folder, "/Tables/tA1_1_reporting_rates", Sys.Date(), ".htm")

cat(paste("<h3>Table A1.1<br />Reporting of data in the",
          report_year,
          "round of global TB data collection</h3>"),
    file=reporting_filename)

print(reporting_html,
      type="html",
      file=reporting_filename,
      include.rownames=FALSE,
      include.colnames=FALSE,
      html.table.attributes="border='0' rules='rows' width='1100' cellpadding='5'",
      append=TRUE,
      add.to.row=list(pos=list(0),
                      command=c("<tr>
                                <td rowspan='2'></td>
                                <td colspan='2' style='border-right: black 2px solid; text-align: center;'>Countries and territories</td>
                                <td colspan='2' style='text-align: center;'>WHO Member States</td>
                                </tr>
                                <tr>
                                <td style='text-align: right;'>Number</td>
                                <td style='border-right: black 2px solid;  text-align: right;'>Number that reported data</td>
                                <td style='text-align: right;'>Number</td>
                                <td style='text-align: right;'>Number that reported data</td>
                                </tr>")
                      )
      )

# Clean up (remove any objects with their name beginning with 'reporting')
rm(list=ls(pattern = "^reporting"))
