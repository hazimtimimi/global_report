# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Tables in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Tom Hiatt
# 6 July 2012, updated July 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Diagnosis and treatment of TB, HIV-associated TB and drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table 4.1  ------
# Notifications of TB, TB/HIV and RR-TB cases for WHO regions and globally, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


notifs_summary <- filter(notification, year == (report_year - 1)) %>%
                  select(g_whoregion,
                         c_notified,
                         c_newinc,
                         new_labconf, new_clindx, new_ep,
                         ret_rel_labconf, ret_rel_clindx, ret_rel_ep,
                         ret_nrel,
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
                                 ret_nrel,
                                 newrel_hivpos,
                                 conf_rrmdr,
                                 all_conf_xdr)

# merge with regional names
notifs_summary <- filter(aggregated_estimates_epi, year == 2010 & group_type == "g_whoregion") %>%
                 select(group_name, group_description) %>%
                 rename(g_whoregion = group_name) %>%
                 inner_join(notifs_summary, by = "g_whoregion") %>%
                 rename(entity = group_description) %>%
                 arrange(g_whoregion) %>%
                 select(-g_whoregion) %>%
                 # Remove the unnecessary "WHO " text at the beginning of region names
                 mutate(entity = sub("^WHO |^WHO/PAHO ", "", entity))


# Add global summary to the regional summary
notifs_global <- notifs_summary %>%
                 summarise_each(funs(sum(., na.rm = TRUE)),
                                -entity) %>%
                  mutate(entity="Global")


notifs_summary <- rbind(notifs_summary, notifs_global)


# Calculate total pulmonary and %ages that are bac confirmed and that are extrapulmonary
notifs_summary <- notifs_summary %>%
                  mutate( newrel_pulm = new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx,
                          newrel_pulm_conf = new_labconf + ret_rel_labconf,
                          newrel_pulm_conf_pct = (new_labconf + ret_rel_labconf) * 100
                                                  /
                                                  (new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx),
                          newrel_ep = new_ep + ret_rel_ep,
                          newrel_ep_pct = (new_ep + ret_rel_ep) * 100
                                          /
                                          (c_newinc)
                        ) %>%
                  # Restrict to variables needed in the final output
                  select(entity,
                         c_notified,
                         c_newinc,
                         newrel_pulm,
                         newrel_pulm_conf,
                         newrel_pulm_conf_pct,
                         newrel_ep,
                         newrel_ep_pct,
                         ret_nrel,
                         newrel_hivpos,
                         conf_rrmdr,
                         all_conf_xdr)


# format the data
for(var in 2:ncol(notifs_summary)){
  notifs_summary[var] <- rounder(notifs_summary[[var]])
}

# Enclode % fields with parens and add % symbol
notifs_summary <- notifs_summary %>%
                  mutate(newrel_pulm_conf_pct = paste0("(", newrel_pulm_conf_pct, "%)"),
                         newrel_ep_pct = paste0("(", newrel_ep_pct, "%)"))

# Create HTML output
notif_table_html <- xtable(notifs_summary)

digits(notif_table_html) <- 0

notif_table_filename <- paste0("Tables/t4_1_notifs_summary", Sys.Date(), ".htm")

cat(paste("<h3>Table 4.1 Notifications of TB, TB/HIV and RR-TB cases for WHO regions and globally, ",
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
                                  <td></td>
                                  <td>Total notified</td>
                                  <td>New and relapse<sup>a</sup></td>
                                  <td>Pulmonary new and relapse</td>
                                  <td colspan='2'>Pulmonary new and relapse<br />bacteriologically confirmed (%)</td>
                                  <td colspan='2'>Extrapulmonary<br />new and relapse (%)</td>
                                  <td>Previously treated,<br />excluding relapse</td>
                                  <td>HIV-positive<br /> new and relapse</td>
                                  <td>RR-TB cases</td>
                                  <td>XDR-TB cases</td>
                              </tr>",
                              "<tr><td colspan='10'>Blank cells indicate data not reported.<br />

                                    <sup>a</sup> New and relapse includes cases for which the treatment history is unknown.</td>
                              </tr>")
                      )
      )

tablecopy("t4_1_notifs_summary")


# and now clear up the mess left behind
rm(list=c("notifs_global", "notifs_summary", "notif_table_html", "notif_table_filename"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table 4.2  ------
# National policies on the use of WHO-recommended rapid tests and DST in HBCs, 2015
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
                     # drop iso2 and re-order fields as per desired output
                     select(entity = country,
                            g_whoregion,
                            g_hb_tb,
                            g_hb_tbhiv,
                            g_hb_mdr,
                            xpert_in_guide_tb,
                            xpert_in_guide_TBHIV,
                            xpert_in_guide_drtb,
                            xpert_in_guide_children,
                            xpert_in_guide_eptb)


# - - - - - - - - - - - - - - - - - - - - - - -
# Define function to calculate aggregates (% of non-empty responses that are == 1)
# Results in string of the form n/n=n%
rdxpolicy_pcnt  <- function(x){

  paste0(sum(x, na.rm = TRUE),
         "/",
         sum(ifelse(is.na(x),0,1)),
         "=",
         round(sum(x, na.rm = TRUE) * 100
               /
               (sum(ifelse(is.na(x),0,1)))),
         "%")
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

rdxpolicy_table_filename <- paste0("Tables/t4_2_xpert_policy", Sys.Date(), ".htm")

cat(paste("<h3>Table 4.2 National guidance in place on use of Xpert MTB/RIF in high burden countries, ",
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
                              "<tr><td colspan='9'>Blank cells indicate data not reported.</td>
                              </tr>")
                      )
      )

tablecopy("t4_2_xpert_policy")

# Clean up (remove any objects with their name beginning with 'rdxpolicy')
rm(list=ls(pattern = "^rdxpolicy"))

stop("

     >>>>>>>>>>
     Stopping here for testing!
     <<<<<<<<<<<<")


