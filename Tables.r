# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Tables in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Tom Hiatt
# 6 July 2012, updated July 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 3 ------
# The burden of disease caused by TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 3_2_burden_num ----------------------------------------------

est_30hbc <- report_country %>%
              filter(g_hb_tb==1) %>%
              select(iso2)


est_cntry <- estimates_epi_rawvalues %>%
             filter(year == report_year - 1) %>%
             inner_join(est_30hbc) %>%
             select(country,
                   e_pop_num,
                   e_mort_exc_tbhiv_100k,
                   e_mort_exc_tbhiv_100k_lo,
                   e_mort_exc_tbhiv_100k_hi,
                   e_mort_tbhiv_100k,
                   e_mort_tbhiv_100k_lo,
                   e_mort_tbhiv_100k_hi,
                   e_inc_num,
                   e_inc_num_lo,
                   e_inc_num_hi,
                   e_inc_tbhiv_num,
                   e_inc_tbhiv_num_lo,
                   e_inc_tbhiv_num_hi) %>%
            mutate(e_pop_num_thou = rounder(e_pop_num / 1e3),
                   e_mort_exc_tbhiv_num = frmt(e_mort_exc_tbhiv_100k * e_pop_num / 1e8, thou=TRUE),
                   e_mort_exc_tbhiv_num_range = paste0(frmt(e_mort_exc_tbhiv_100k_lo * e_pop_num / 1e8, thou=TRUE),
                                                       "\u2013",
                                                       frmt(e_mort_exc_tbhiv_100k_hi * e_pop_num / 1e8, thou=TRUE)),

                   e_mort_tbhiv_num = frmt(e_mort_tbhiv_100k  * e_pop_num / 1e8, thou=TRUE),
                   e_mort_tbhiv_num_range = paste0(frmt(e_mort_tbhiv_100k_lo  * e_pop_num / 1e8, thou=TRUE),
                                                   "\u2013",
                                                   frmt(e_mort_tbhiv_100k_hi  * e_pop_num / 1e8, thou=TRUE)),

                   e_inc_num = frmt(e_inc_num / 1e3, rates=TRUE, thou=TRUE),
                   e_inc_num_range = paste0(frmt(e_inc_num_lo / 1e3, rates=TRUE, thou=TRUE),
                                           "\u2013",
                                           frmt(e_inc_num_hi  / 1e3, rates=TRUE, thou=TRUE)),

                   e_inc_tbhiv_num = frmt(e_inc_tbhiv_num / 1e3, rates=TRUE, thou=TRUE),
                   e_inc_tbhiv_num_range = paste0(frmt(e_inc_tbhiv_num_lo  / 1e3, rates=TRUE, thou=TRUE),
                                                 "\u2013",
                                                 frmt(e_inc_tbhiv_num_hi / 1e3, rates=TRUE, thou=TRUE))
                   ) %>%

            select(country,
                   e_pop_num = e_pop_num_thou,
                   e_mort_exc_tbhiv_num,
                   e_mort_exc_tbhiv_num_range,
                   e_mort_tbhiv_num,
                   e_mort_tbhiv_num_range,
                   e_inc_num,
                   e_inc_num_range,
                   e_inc_tbhiv_num,
                   e_inc_tbhiv_num_range) %>%

            arrange(country)

write.csv(est_cntry, file=paste(figures_folder, "/Tables/", "table_3_2_", Sys.Date(), ".csv", sep=""), row.names=FALSE, na="")


rm(list=ls(pattern = "^est_"))



est_aggs <- aggregated_estimates_epi_rawvalues %>%
            filter(year == report_year - 1) %>%
            mutate(e_pop_num_thou = rounder(e_pop_num / 1e3),
                   e_mort_exc_tbhiv_num = frmt(e_mort_exc_tbhiv_num / 1e3, thou=TRUE),
                   e_mort_exc_tbhiv_num_range = paste0(frmt(e_mort_exc_tbhiv_num_lo / 1e3, thou=TRUE),
                                                       "\u2013",
                                                       frmt(e_mort_exc_tbhiv_num_hi / 1e3, thou=TRUE)),

                   e_mort_tbhiv_num = frmt(e_mort_tbhiv_num / 1e3, thou=TRUE),
                   e_mort_tbhiv_num_range = paste0(frmt(e_mort_tbhiv_num_lo / 1e3, thou=TRUE),
                                                   "\u2013",
                                                   frmt(e_mort_tbhiv_num_hi  / 1e3, thou=TRUE)),

                   e_inc_num = frmt(e_inc_num / 1e3, rates=TRUE, thou=TRUE),
                   e_inc_num_range = paste0(frmt(e_inc_num_lo / 1e3, rates=TRUE, thou=TRUE),
                                           "\u2013",
                                           frmt(e_inc_num_hi  / 1e3, rates=TRUE, thou=TRUE)),

                   e_inc_tbhiv_num = frmt(e_inc_tbhiv_num / 1e3, rates=TRUE, thou=TRUE),
                   e_inc_tbhiv_num_range = paste0(frmt(e_inc_tbhiv_num_lo  / 1e3, rates=TRUE, thou=TRUE),
                                                 "\u2013",
                                                 frmt(e_inc_tbhiv_num_hi / 1e3, rates=TRUE, thou=TRUE))
                   )  %>%

            arrange(group_type, group_name) %>%

            select(group_description,
                   e_pop_num = e_pop_num_thou,
                   e_mort_exc_tbhiv_num,
                   e_mort_exc_tbhiv_num_range,
                   e_mort_tbhiv_num,
                   e_mort_tbhiv_num_range,
                   e_inc_num,
                   e_inc_num_range,
                   e_inc_tbhiv_num,
                   e_inc_tbhiv_num_range)


write.csv(est_aggs, file=paste(figures_folder, "/Tables/", "table_3_2_aggs_", Sys.Date(), ".csv", sep=""), row.names=FALSE, na="")


rm(list=ls(pattern = "^est_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Diagnosis and treatment of TB, HIV-associated TB and drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table 4.1  ------
# Notifications of TB, TB/HIV and MDR/RR-TB cases, globally and for WHO regions, 2015
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

notif_table_filename <- paste0("Tables/t4_1_notifs_summary", Sys.Date(), ".htm")

cat(paste("<h3>Table 4.1 Notifications of TB, TB/HIV and MDR/RR-TB cases, globally and for WHO regions,",
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
# National guidance in place on use of Xpert MTB/RIF in high burden countries, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rdxpolicy_hbccodes <- report_country %>%
                      select(iso2,
                             starts_with("g_hb_"))

rdxpolicy_country <- strategy %>%
                     filter(year == report_year - 1) %>%
                     select(country,
                            iso2,
                            g_whoregion,
                            xpert_in_guide_tb,
                            xpert_in_guide_TBHIV,
                            xpert_in_guide_drtb,
                            xpert_in_guide_children,
                            xpert_in_guide_eptb) %>%
                     inner_join(rdxpolicy_hbccodes) %>%
                     # drop iso2
                     select(entity = country,
                            g_whoregion,
                            g_hb_tb,
                            g_hb_tbhiv,
                            g_hb_mdr,
                            xpert_in_guide_tb,
                            xpert_in_guide_TBHIV,
                            xpert_in_guide_drtb,
                            xpert_in_guide_children,
                            xpert_in_guide_eptb) %>%
                      # shorten long country names
                      .shortnames( col = "entity") %>%
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
                                  starts_with("xpert_in_guide")) %>%
                     mutate(entity="High TB burden countries")

rdxpolicy_g_hb_tbhiv <- rdxpolicy_country %>%
                     filter(g_hb_tbhiv == 1) %>%
                     summarise_each(funs(rdxpolicy_pcnt(.)),
                                  starts_with("xpert_in_guide")) %>%
                     mutate(entity="High TB/HIV burden countries")


rdxpolicy_g_hb_mdr <- rdxpolicy_country %>%
                     filter(g_hb_mdr == 1) %>%
                     summarise_each(funs(rdxpolicy_pcnt(.)),
                                  starts_with("xpert_in_guide")) %>%
                     mutate(entity="High MDR burden countries")

# Aggregate by WHO region
rdxpolicy_regions <- rdxpolicy_country %>%
                     group_by(g_whoregion) %>%
                     summarise_each(funs(rdxpolicy_pcnt(.)),
                                  starts_with("xpert_in_guide")) %>%
                     # merge with region names
                     inner_join(who_region_names) %>%
                     # drop region code
                     select(-g_whoregion)

# Global aggregate
rdxpolicy_global <- rdxpolicy_country %>%
                     summarise_each(funs(rdxpolicy_pcnt(.)),
                                  starts_with("xpert_in_guide")) %>%
                     mutate(entity="Global")


# Wayne decided in the end to only show high burden country aggregates

rdxpolicy_aggs <- rbind(rdxpolicy_g_hb_tb,
                        rdxpolicy_g_hb_tbhiv,
                        rdxpolicy_g_hb_mdr) %>%
                  # Add placeholder variables to match structure of country list
                  mutate(g_hb_tb = NA,
                         g_hb_tbhiv = NA,
                         g_hb_mdr = NA)


# Restrict country data to the high burden countries and then append aggregates
rdxpolicy_hbcs <- rdxpolicy_country %>%
                  select(-g_whoregion) %>%
                  filter(g_hb_tb == 1 | g_hb_tbhiv == 1 | g_hb_mdr == 1) %>%
                  rbind(rdxpolicy_aggs)


# Create HTML output
rdxpolicy_table_html <- xtable(rdxpolicy_hbcs)

rdxpolicy_table_filename <- paste0("Tables/t4_3_xpert_policy", Sys.Date(), ".htm")

cat(paste("<h3>Table 4.3 National guidance in place on use of Xpert MTB/RIF in high burden countries<sup>a</sup>, ",
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
                                  <td colspan='4'>&nbsp;</td>
                                  <td colspan='5'>National policy stipulating Xpert MTB/RIF as the
                                                  <i>initial</i> diagnostic test for: </td>
                              <tr>
                              <tr>
                                  <td>&nbsp;</td>
                                  <td>High TB burden</td>
                                  <td>High TB/HIV burden</td>
                                  <td>High MDR-TB burden</td>                                                                             <td>All people presumed to have TB</td>
                                  <td>People at risk of HIV-associated TB</td>
                                  <td>People at risk of drug-resistant TB</td>
                                  <td>Children presumed to have TB</td>
                                  <td>Extrapulmonary TB using selected specimens </td>
                              </tr>",
                              "<tr><td colspan='9'><sup>a</sup>The 48 countries shown in the table are the countries that are in one of more of the three lists of high TB, TB/HIV and MDR-TB burden countries (see also Chapter 2, Figure 2.3 and Table 2.2).</td>
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


