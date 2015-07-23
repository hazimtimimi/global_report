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

source(file.path(scriptsfolder, "WHO_map_functions.r"))

setwd(outfolder)

# Contrasting colors for easier re-layout
con.col <- c('red', 'blue', 'orange', 'green', 'purple', 'violet', 'sienna', 'dark orange')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 2 ------
# The burden of disease caused by TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 2_4_hivprev_map -------------------------------------------------
# HIV prevalence in new cases

mga <- subset(e.t, year==report_year-1, select=c('country', 'iso3', 'e_tbhiv_prct'))

mga$cat <- cut(mga$e_tbhiv_prct,
               c(0,5,20,50,Inf),
               c('0-4', '5-19', '20-49', '>=50'),
               right=FALSE)

# map
hivprev_map <- WHOmap.print(mga, paste("Estimated HIV prevalence in new and relapse TB cases,", report_year-1),
                            'HIV prevalence \nin new TB cases, \nall ages (%)',
                            na.label="No estimate",
                            copyright=FALSE,
                            colors=c('red', 'blue', 'orange', 'green'),
                            show=FALSE)

figsave(hivprev_map, mga, "2_4_hivprev_map")


# 2_5_inc_map -------------------------------------------------
# Incidence rates

mfa <- subset(e.t, year==report_year-1, select=c('country', 'iso3', 'e_inc_100k'))

mfa$cat <- cut(round(mfa$e_inc_100k),
               c(0,10,20,50,125,300,500,Inf),
               c('0–9.9', '10–19', '20–49', '50–124', '125–299', '300–499', '>=500'),
               right=FALSE)

# map
inc_map <- WHOmap.print(mfa, paste("Estimated TB incidence rates,", report_year-1),
                        "Estimated new TB \ncases (all forms) per \n100 000 population per year",
                        na.label="No estimate",
                        copyright=FALSE,
                        colors=c('red', 'blue', 'orange', 'green', 'purple', 'violet', 'sienna'),
                        show=FALSE)

figsave(inc_map, mfa, "2_5_inc_map")


# 2_12_mort_src_map -------------------------------------------------
# Mortality from vital registration

mea <- subset(e.t, year==report_year-1, select=c("g_whoregion", 'country', 'iso3', 'source_mort'))

# Mortality with VR data
meb <- subset(mea, source_mort %in% c('VR', 'VR imputed'), select=c("g_whoregion", "country", "iso3", "source_mort"))
meb$cat <- factor("Estimated with \nVR data")

# map
mort_src_map <- WHOmap.print(meb,
                             paste0("Countries (in ", mort.color, ") for which TB mortality is estimated using measurements from vital registration systems (n=", nrow(meb) - 2,") \nand/or mortality surveys  (n=2, India and Viet Nam)"),
                             "[remove legend]",
                             low.color=mort.color,
                             copyright=FALSE,
                             show=FALSE)

figsave(mort_src_map, meb, "2_12_mort_src_map")


# 2_13_mort_map -------------------------------------------------
# Mortality rates

mia <- subset(e.t, year==report_year-1, select=c('country', 'iso3', 'e_mort_exc_tbhiv_100k'))

mia$cat <- cut(mia$e_mort_exc_tbhiv_100k,
               c(0,1,4,10,20,40,Inf),
               c('0-0.9', '1-3.9', '4-9.9', '10-19', '20-39', '>=40'),
               right=FALSE)

# map
mort_map <- WHOmap.print(mia,
                         paste("Estimated TB mortality rates excluding TB deaths among HIV-positive people,", report_year-1),
                         "Estimated TB \ndeaths per \n100 000 population",
                         na.label="No estimate",
                         copyright=FALSE,
                         colors=c('red', 'blue', 'orange', 'green', 'purple', 'brown'),
                         show=FALSE)

figsave(mort_map, mia, "2_13_mort_map")


# 2_16_err_map -------------------------------------------------
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

figsave(mhc, mhb, "2_16_err_map")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 3 ------
# TB case notifications and treatment outcomes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 3_x_ltbipolicy_map -------------------------------------------------
# Countries with national LTBI policy

ltbipolicy <- readWorksheetFromFile(file.path(datafolder, "Extra data", "YH", "LTBI_page_maps2207..xlsx"), sheet="policy") %>%  
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

figsave(ltbipolicy_map, ltbipolicy, "3_x_ltbipolicy_map")

# 3_x_ltbipractice_map -------------------------------------------------
# Countries with national LTBI practices

ltbipractice <- readWorksheetFromFile(file.path(datafolder, "Extra data", "YH", "LTBI_page_maps2207..xlsx"), sheet="practice") %>%  
  mutate(cat=factor(Practices.on.LTBI.screening.and.treatment, 
                    levels=c("Both HIV and contacts",
                             "Only contacts",
                             "Not practiced for HIV or contacts",
                             "No data",
                             "Estimated TB incidence>=100 or low/lower middle income"), 
                    labels=c("Both HIV and contacts",
                             "Only contacts",
                             "Not practiced for \nHIV or contacts",
                             "No data",
                             "High-burden countries"))
  )


ltbipractice_map <- WHOmap.print(ltbipractice,
                               paste("Practices on LTBI testing/treatment for people living with HIV and/or contacts,", report_year-1), 
                               "",
                               colors=con.col[1:5],
                               copyright=FALSE,
                               show=FALSE)

figsave(ltbipractice_map, ltbipractice, "3_x_ltbipractice_map")


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

figsave(bdq_map, mib, "4_10_bdq_map")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# Diagnostics and laboratory strengthening
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 5_2_xpertcart_map -------------------------------------------------
# numbers of xpert cartridges procured

xpert_cart <- readWorksheetFromFile(file.path(datafolder, "Extra data", "WvG", "Figure 5.2 accompanying data.xlsx"), sheet=1) %>%  
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

figsave(xpert_cart_map, xpert_cart, "5_2_xpertcart_map")


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

figsave(hivtest_map, mc, "6_2_hivtest_map")


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

figsave(HIVart_map, mca, "6_5_HIVart_map")


# END ======================