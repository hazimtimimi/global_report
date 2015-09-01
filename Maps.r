# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Maps in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Tom Hiatt
# 6 July 2012, updated July 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Links ------
#
# Link GTBR2015 map numbers to code sections
#
# 2.4  HIV prevalence in new cases: 2_4_hivprev_map
# 2.5  Incidence rates: 2_5_inc_map
# 2.12 Mortality from vital registration: 2_12_mort_src_map
# 2.13 Mortality rates: 2_13_mort_map
# 2.16 Availability of national electronic case-based databases: 2_16_err_map
#
# 4.10 Countries that had used bedaquiline: 4_10_bdq_map
#
# 6.2  Percentage of notified TB patients with known HIV status by country: 6_2_hivtest_map
# 6.5  Percentage of HIV-positive TB patients enrolled on ART: 6_5_HIVart_map
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# ******************************************************
# Old maps

# Find this code in past repository commits

# Dropped in 2014
# HIVipt_map
# Xpert_map

# ******************************************************

# This script does not use Philippe's who mapping package (see https://github.com/glaziou/whomap)
# instead it uses Tom's original version which allows us to add 'Not applicable' and 'No estimate'
# options to the legends, etc.
# Have therefore not switched to the simpler whomap package (yet)

source(file.path(scripts_folder, "WHO_map_functions.r"))

# Contrasting colors for easier re-layout
con.col <- c('red', 'blue', 'orange', 'green', 'purple', 'violet', 'sienna', 'dark orange')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 2 ------
# The burden of disease caused by TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2_1_sb_map -------------------------------------------------
# Countries which have completed the standards and benchmarks

mna <- e.t %>% filter(year==report_year-1, iso2 %in% c("BW","BI","CI","ET","GH","KE","LS","MG","MW","MZ","NG","RW","ZA","SS","SZ","UG","TZ","ZM","ZW","AF","EG","PK","SA", "AM","BY", "GE","TM","UA","UZ","BD","KP","ID","MM","NP","TH","MN","PH","VN")) %>% select(country, iso3, g_hbc22)

mcnt <- table(mna$g_hbc22)

mna$cat <- factor(mna$g_hbc22, levels=c("high", "low"), labels=c(paste0("High-burden countries (", mcnt[1], ")"), paste0("Other countries (", mcnt[2], ")")))

# map
sb_map <- WHOmap.print(mna, 
"Figure 2.1 Countries that had completed a systematic assessment
of TB surveillance using the WHO TB surveillance checklist of
standards and benchmarks by August 2015", "",
                            na.label="",
                            copyright=FALSE,
                            colors=c('red', 'green'),
                            show=FALSE)

figsave(sb_map, mna, "f2_1_sb_map")


# 2_5_hivprev_map -------------------------------------------------
# HIV prevalence in new cases

mga <- subset(e.t, year==report_year-1, select=c('country', 'iso3', 'e_tbhiv_prct'))

mga$cat <- cut(mga$e_tbhiv_prct,
               c(0,5,20,50,Inf),
               c('0-4', '5-19', '20-49', '>=50'),
               right=FALSE)

# map
hivprev_map <- WHOmap.print(mga, paste("Figure 2.5 Estimated HIV prevalence in new and relapse TB cases,", report_year-1),
                            'HIV prevalence \nin new TB cases, \nall ages (%)',
                            na.label="No estimate",
                            copyright=FALSE,
                            colors=c('red', 'blue', 'orange', 'green'),
                            show=FALSE)

figsave(hivprev_map, mga, "f2_5_hivprev_map")


# 2_6_inc_map -------------------------------------------------
# Incidence rates

mfa <- subset(e.t, year==report_year-1, select=c('country', 'iso3', 'e_inc_100k'))

mfa$cat <- cut(round(mfa$e_inc_100k),
               c(0,10,20,50,125,300,500,Inf),
               c('0–9.9', '10–19', '20–49', '50–124', '125–299', '300–499', '>=500'),
               right=FALSE)

# map
inc_map <- WHOmap.print(mfa, paste("Figure 2.6 Estimated TB incidence rates,", report_year-1),
                        "Estimated new TB \ncases (all forms) per \n100 000 population \nper year",
                        na.label="No estimate",
                        copyright=FALSE,
                        colors=c('red', 'blue', 'orange', 'green', 'purple', 'violet', 'sienna'),
                        show=FALSE)

figsave(inc_map, mfa, "f2_6_inc_map")



# 2_2_inc_src_map -------------------------------------------------
# Incidence from country consultations

mfa <- subset(e.t, year==report_year-1, select=c("g_whoregion", 'country', 'iso3', 'source_inc'))

mfb <- subset(mfa, source_inc %in% c("Capture-recapture","High income","Survey"), select=c("g_whoregion", "country", "iso3", "source_inc")) 
mfb$cat <- factor("Country consultation")

# map
inc_src_map <- WHOmap.print(mfb, paste0(
"Figure 2.2 Coverage of country consultations on estimates of 
TB disease burden, 2008–", report_year),
                             "[remove legend]",
                             low.color=inc.color,
                             copyright=FALSE,
                             show=FALSE)

figsave(inc_src_map, mfb, "f2_2_inc_src_map")


# 2_15_mort_src_map -------------------------------------------------
# Mortality from vital registration

mea <- subset(e.t, year==report_year-1, select=c("g_whoregion", 'country', 'iso3', 'source_mort'))

# Mortality with VR data
meb <- subset(mea, source_mort != "indirect", select=c("g_whoregion", "country", "iso3", "source_mort")) 
meb$cat <- factor("Estimated with \nVR data")

# map
mort_src_map <- WHOmap.print(meb,
                             paste0(
"Figure 2.15 Countries (in ", mort.color, ") for which TB 
mortality is estimated using measurements from vital registration 
systems (n=", nrow(meb)-2, ") and/or mortality surveys (n=2)"),
                             "[remove legend]",
                             low.color=mort.color,
                             copyright=FALSE,
                             show=FALSE)

figsave(mort_src_map, meb, "f2_15_mort_src_map")


# 2_17_mort_map -------------------------------------------------
# Mortality rates

mia <- subset(e.t, year==report_year-1, select=c('country', 'iso3', 'e_mort_exc_tbhiv_100k'))

mia$cat <- cut(mia$e_mort_exc_tbhiv_100k,
               c(0,1,4,10,20,40,Inf),
               c('0-0.9', '1-3.9', '4-9.9', '10-19', '20-39', '>=40'),
               right=FALSE)

# map
mort_map <- WHOmap.print(mia,
                         paste(
"Figure 2.17 Estimated TB mortality rates excluding TB deaths among 
HIV-positive \npeople,", report_year-1),
                         "Estimated TB \ndeaths per \n100 000 population",
                         na.label="No estimate",
                         copyright=FALSE,
                         colors=c('red', 'blue', 'orange', 'green', 'purple', 'brown'),
                         show=FALSE)

figsave(mort_map, mia, "f2_17_mort_map")


# 2_xx_err_map -------------------------------------------------
# Electronic recording and reporting

mha <- subset(s, year %in% (report_year-1):(report_year-2), select=c('country', 'year', 'iso3', 'caseb_err_nat'))

# Take last year's answer if unreported
for(cnty in unique(mha$country)) {
  if(is.na(mha[mha$country==cnty & mha$year==report_year-1, 'caseb_err_nat'])) mha[mha$country==cnty & mha$year==report_year-1, 'caseb_err_nat'] <- mha[mha$country==cnty & mha$year==report_year-2, 'caseb_err_nat']
}

mhb <- subset(mha, year==report_year-1)

# Western europe fixes
if(report_year==2013){
  mhb[mhb$iso3 %in% c('AUT', 'CHE', 'CYP', 'DNK', 'GRC', 'GRL', 'ITA', 'LUX', 'UZB', 'KOR'), 'caseb_err_nat'] <- 42
  mhb[mhb$iso3 %in% c('KGZ'), 'caseb_err_nat'] <- 0
  mhb[mhb$iso3 %in% c('NPL'), 'caseb_err_nat'] <- 43
}


mhb$cat <- factor(mhb$caseb_err_nat, levels=c(42, 43, 0), labels=c('All TB patients', 'MDR-TB patients only', 'None' ))

# map
mhc <- WHOmap.print(mhb,
                    paste("Availability of national electronic case-based databases of TB patients,", report_year-1),
                    '',
                    colors=c('dark green', 'light green', 'white'),
                    copyright=FALSE,
                    show=FALSE)

figsave(mhc, mhb, "f2_xx_err_map")

# 2_21_age_map -------------------------------------------------
# Countries able to report age disaggregated data

mja <- n.t %>% filter(year==report_year-1, !is.na(c_newinc)) %>% select(country, iso3, matches("newrel_[m|f|sex]")) %>% select(-newrel_mu, -newrel_fu, -newrel_sexunkageunk) 

mja$age <- sum_of_row(mja[3:ncol(mja)])

mja$cat <- factor(ifelse(is.na(mja$age), "No age disaggregation", "Age disaggregation"))

mjb <- mja %>% select(country, iso3, cat)

# map
age_map <- WHOmap.print(mjb,
                    paste("Figure 2.21 Reporting of age disaggregated data,", report_year-1),
                    '',
                    # colors=c('dark green', 'light green', 'white'),
                    copyright=FALSE,
                    show=TRUE)

figsave(age_map, mjb, "f2_21_age_map")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 3 ------
# TB case notifications and treatment outcomes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 3_7_ltbipolicy_map -------------------------------------------------
# Countries with national LTBI policy

ltbipolicy <- readWorksheetFromFile(file.path(rdata_folder, "Extra data", "YH", "LTBI_page_maps2207..xlsx"), sheet="policy") %>%  
  mutate(cat=factor(Availability.on.national.policy.on.LTBI, 
                    levels=c("National policy on LTBI available ",
                             "No national policy on LTBI",
                             "No data",
                             "Estimated TB incidence>=100 or low/lower middle income"), 
                    labels=c("National policy on LTBI \navailable",
                             "No national policy on LTBI",
                             "No data",
                             "High-burden countries"))
  )


ltbipolicy_map <- WHOmap.print(ltbipolicy,
                               paste("Reported national policies on LTBI,", report_year-1), 
                               "",
                               colors=con.col[1:4],
                               copyright=FALSE,
                               show=FALSE)

figsave(ltbipolicy_map, ltbipolicy, "f3_7_ltbipolicy_map")


# 3_6_ltbisurvey_map -------------------------------------------------
# Countries included in the LTBI survey

ltbisurvey <- readWorksheetFromFile(file.path(rdata_folder, "Extra data", "YH", "LTBI_page_maps.xlsx"), sheet=1) %>%  
  mutate(cat=factor(primary_target, 
                    levels=c("Estimated TB incidence<100 and high/upper-middle income"), 
                    labels=c("Estimated TB incidence<100 or \nhigh/upper-middle income"))
  )


ltbisurvey_map <- WHOmap.print(ltbisurvey,
                                 "The 113 upper-middle and high-income countries with \nan incidence rate of less than \n100 per 100 000 population that are the \nprimary audience for 2015 WHO guidelines on the \nmanagement of latent TB infection", 
                                 "[Remove legend]",
                                 copyright=FALSE,
                                 show=FALSE)

figsave(ltbisurvey_map, ltbisurvey, "f3_6_ltbisurvey_map")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 4 ------
# Drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 4_10_bdq_map -------------------------------------------------
# Countries using bedaquiline

mia <- subset(n, year %in% (report_year-1):(report_year-2), select=c('country', 'year', 'iso3', 'mdrxdr_bdq_used'))

# Take last year's answer if unreported
for(cnty in unique(mia$country)) {
  if(is.na(mia[mia$country==cnty & mia$year==report_year-1, 'mdrxdr_bdq_used'])) mia[mia$country==cnty & mia$year==report_year-1, 'mdrxdr_bdq_used'] <- mia[mia$country==cnty & mia$year==report_year-2, 'mdrxdr_bdq_used']
}

mib <- subset(mia, year==report_year-1)

#  fixes
if(report_year==2014){
  mib[mib$iso3 %in% c('BEL', "ITA"), 'mdrxdr_bdq_used'] <- 1
  mib[mib$iso3 %in% c('ARG'), 'mdrxdr_bdq_used'] <- 0
  warning("some hard coding going on with the BDQ map.")
}

# Leave off unknown category
mib$cat1 <- ifelse(mib$mdrxdr_bdq_used==3, NA, mib$mdrxdr_bdq_used)

mib$cat <- factor(mib$cat1, levels=c(1, 0, 3), labels=c('Yes', 'No', 'Unknown' ))

# map
bdq_map <- WHOmap.print(mib,
                        paste("Countries that had used bedaquiline for the treatment of M/XDR-TB as part of expanded access, \ncompassionate use or under normal programmatic conditions by the end of", report_year-1),
                        '',
                        colors=c('dark orange', 'green', 'blue'),
                        copyright=FALSE,
                        show=FALSE)

figsave(bdq_map, mib, "f4_10_bdq_map")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# Diagnostics and laboratory strengthening
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 5_1_dst_map -------------------------------------------------
# dstality rates

dst_dta <- readWorksheetFromFile(file.path(rdata_folder, "Extra data", "WvG", "andrewistheman.xls"), sheet=1) %>% select(iso3, value) %>% filter(value!=5) %>% mutate(cat=factor(value, labels=c("1st- and 2nd-line DST", "1st-line DST only", "Xpert MTB/RIF only", "No capacity"))) %>% inner_join(subset(n, year==report_year-1, c(iso3, country)))

# map
dst_map <- WHOmap.print(dst_dta, "Figure 5.1 Global capacity for drug-susceptibility testing (DST), 2014(a)",
                        "", na.label = "No data",
                        copyright=FALSE,
                        colors=c('red', 'blue', 'green', 'purple'),
                        show=FALSE)

figsave(dst_map, dst_dta, "f5_1_dst_map")



# 5_2_xpertcart_map -------------------------------------------------
# numbers of xpert cartridges procured

xpert_cart <- readWorksheetFromFile(file.path(rdata_folder, "Extra data", "WvG", "Figure 5.2 accompanying data.xlsx"), sheet=1) %>%  
  mutate(cart1=ifelse(is.na(cartridges), 0, cartridges), # NA same as 0
         cart2=as.numeric(cart1)/1000, 
         cat=cut(cart2, 
                   c(0, 5, 50, 100, 300, Inf), 
                   c("0-4", "5-49", "50-99", "100-299", ">=300"), 
                   right=FALSE) 
         )


xpert_cart_map <- WHOmap.print(xpert_cart,
                        paste("Xpert MTB/RIF cartridge procurements in", report_year-1, "at concessional prices"), 
                        "Xpert MTB/RIF \ncartridges \nprocured in 2014 \n(thousands)",
                        colors=con.col[1:5],
                        copyright=FALSE,
                        na.label="Not eligible for \npreferential pricing",
                        show=FALSE)

figsave(xpert_cart_map, xpert_cart, "f5_2_xpertcart_map")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 6 ------
# Addressing the co-epidemics of TB and HIV
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 6_2_hivtest_map -------------------------------------------------

mc <- subset(tbhiv, year==report_year-1, select=c(country, iso2, iso3, g_whoregion, hivtest_pct_numerator, hivtest_pct_denominator))
mc$hivtest_prct <- round(mc$hivtest_pct_numerator/mc$hivtest_pct_denominator * 100)

# Fix and footnote for Russian Federation
mc[mc$country=='Russian Federation', 'hivtest_prct'] <- round(subset(n, iso2=='RU' & year==report_year-1, hivtest_p) / subset(n, iso2=='RU' & year==report_year-1, c_newunk) * 100)

warning("Russian Federation modification for the HIV test map is still in place. Delete this message when no longer applicable.")


mc$cat <- NA
mc$cat[mc$hivtest_prct >= 75] <- ">=75"
mc$cat[mc$hivtest_prct < 75] <- '50-74'
mc$cat[mc$hivtest_prct < 50] <- '15-49'
mc$cat[mc$hivtest_prct < 15] <- '0-14'

mc$cat <- as.factor(mc$cat)
mc$cat <- factor(mc$cat, levels = c("0-14", "15-49",  "50-74", ">=75"))

# HIV testing map (ugly colors are for easier design)

hivtest_map <- WHOmap.print(mc,
                            paste("Percentage of notified TB patients with known HIV status by country,", report_year-1, "(a)"),
                            "Percentage of \nnotified TB patients",
                            copyright=FALSE,
                            colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                            show=FALSE)

# Footnote: Data for the Russian Federation are for new TB patients in the civilian sector only.

figsave(hivtest_map, mc, "f6_2_hivtest_map")


# 6_5_HIVart_map -------------------------------------------------

# Changed denominator in 2014 report from hiv_art_pct_denominator to e_inc_tbhiv_num

mca <- subset(tbhiv, year==report_year-1, select=c(country, iso2, iso3, g_whoregion, hiv_art_pct_numerator, hiv_art_pct_denominator))

mca <- merge(mca, subset(e.t, year==report_year-1, select=c('country', 'year', 'e_inc_tbhiv_num')))

mca$hivart_prct <- round(mca$hiv_art_pct_numerator/mca$e_inc_tbhiv_num * 100)

mca$cat <- cut(mca$hivart_prct,
               c(0, 25, 50, 75, Inf),
               c('0-24', '25-49', '50-74', '75-100'),
               right=FALSE)

# map

HIVart_map <- WHOmap.print(mca,
                           paste("Number of HIV-positive TB patients on ART as a percentage of estimated HIV-positive incident \nTB cases,", report_year-1, "(a)"),
                           "Percentage",
                           copyright=FALSE,
                           colors=c('yellow', 'lightgreen', 'green', 'darkgreen'),
                           show=FALSE)

# Footnote: The numerator (i.e. all notified HIV-positive TB cases on ART) includes all notified new, relapse and
# non-relapse retreatment cases. The denominator (i.e.
# estimated HIV-positive incident TB cases) includes new and relapse cases only.

figsave(HIVart_map, mca, "f6_5_HIVart_map")


# END ======================