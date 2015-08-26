# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Tables in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Tom Hiatt
# 6 July 2012, updated July 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Links ------
#
# Link GTBR2015 table numbers to code sections
#
# 2.1 Estimated epidemiological burden of TB (numbers): 2_1_burden_num
# 2.2 Estimated epidemiological burden of TB (rates)  : 2_2_burden_rt
#
# 3.1 Case notifications: 3_1_notif
# 3.2 Notifications of new and relapse TB cases by age and sex: 3_2_agesex
# 3.5 Estimates of the case detection rate for new and relapse cases (%): 3_5_cdr
# 3.6 Treatment success for all new TB cases (%) and cohort size: 3_6_tsr
#
# 5.1 Laboratory capacity: 5_1_lab_capac (start at 5_1_2_lab_capac & lab_policy )
#
# 6.1 HIV testing for TB patients, ..etc: 6_1_tbhiv
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# ******************************************************
# Old tables

# Find this code in past repository commits

# Dropped in 2014
# 'top 10 countries' table
# tsr_sp

# ******************************************************


if(flg_show_estimates){

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Chapter 2 ------
  # The burden of disease caused by TB
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # 2_1_burden_num ----------------------------------------------

  tagvars <- c("e_pop_num", "e_mort_exc_tbhiv_num", "e_mort_exc_tbhiv_num_lo", "e_mort_exc_tbhiv_num_hi", "e_mort_tbhiv_num", "e_mort_tbhiv_num_lo", "e_mort_tbhiv_num_hi", "e_prev_num", "e_prev_num_lo", "e_prev_num_hi", "e_inc_num", "e_inc_num_lo", "e_inc_num_hi", "e_inc_tbhiv_num", "e_inc_tbhiv_num_lo", "e_inc_tbhiv_num_hi")
  tag1 <- subset(e.t, g_hbc22=='high' & year==report_year-1, c('country', tagvars))

  names(tag1)[1] <- 'group_name'

  tag <- rbind(tag1, subset(araw.t, group_type %in% c('global', 'g_whoregion', "g_hbc22") & year==report_year-1, c('group_name', tagvars)))
  
  # A fudge to make a row for HBC aggregates. I wonder if I can get Hazim to store these in the database...
    load(paste(rdata_folder, "Extra data/PG/hbc.Rdata", sep="/"))
fujj <- hbc %>% data.frame() %>% filter(year==report_year-1) %>% select(e_pop_num=e.pop.num, e_mort_exc_tbhiv_num=mort.nh.num, e_mort_exc_tbhiv_num_lo=mort.nh.lo.num, e_mort_exc_tbhiv_num_hi=mort.nh.hi.num, e_mort_tbhiv_num=mort.h.num, e_mort_tbhiv_num_lo=mort.h.lo.num, e_mort_tbhiv_num_hi=mort.h.hi.num, e_prev_num=prev.num, e_prev_num_lo=prev.lo.num, e_prev_num_hi=prev.hi.num, e_inc_num=inc.num, e_inc_num_lo=inc.lo.num, e_inc_num_hi=inc.hi.num, e_inc_tbhiv_num=inc.h.num, e_inc_tbhiv_num_lo=inc.h.lo.num, e_inc_tbhiv_num_hi=inc.h.hi.num) %>% mutate(group_name="High-burden countries")

  tag <- rbind(tag, fujj)

  names(tag)[1] <- 'rowname'

  tag[2] <- rounder(tag[[2]]/1000)

  for(col in 3:ncol(tag)){
  tag[col] <- ifelse(is.na(tag[[col]]), "\u2013", frmt(tag[[col]]/1000))
  }

  # combine range columns

  tag$e_mort_exc_tbhiv_range <- paste0("(", tag$e_mort_exc_tbhiv_num_lo, "\u2013", tag$e_mort_exc_tbhiv_num_hi, ")")
  tag$e_mort_tbhiv_range <- paste0("(", tag$e_mort_tbhiv_num_lo, "\u2013", tag$e_mort_tbhiv_num_hi, ")")
  tag$e_prev_range <- paste0("(", tag$e_prev_num_lo, "\u2013", tag$e_prev_num_hi, ")")
  tag$e_inc_range <- paste0("(", tag$e_inc_num_lo, "\u2013", tag$e_inc_num_hi, ")")
  tag$e_inc_tbhiv_range <- paste0("(", tag$e_inc_tbhiv_num_lo, "\u2013", tag$e_inc_tbhiv_num_hi, ")")

  tah <- subset(tag, select=c("rowname", "e_pop_num", "e_mort_exc_tbhiv_num", "e_mort_exc_tbhiv_range","e_mort_tbhiv_num", "e_mort_tbhiv_range", "e_prev_num", "e_prev_range", "e_inc_num", "e_inc_range", "e_inc_tbhiv_num", "e_inc_tbhiv_range"))

  # Fix names
  tak <- .shortnames(tah, col = "rowname", ord = "hbc")

  # Add footnotes for some countries 
  tak[tak$rowname=="India", "rowname"] <- "India(c)"


  cat(paste("<font size=5><b>Table 2.1 Estimated epidemiological burden of TB, ", report_year-1, ".</b> Best estimates are followed by the lower and upper bounds of the 95% uncertainty interval. Numbers in thousands.<sup>a</sup></font>", sep=""), file=paste0("Tables/2_1_burden_num", Sys.Date(), ".htm"))

  tai <- xtable(tak)
  print(tai, type="html", file=paste0("Tables/2_1_burden_num", Sys.Date(), ".htm"), include.rownames=F, include.colnames=F, append=T,
  html.table.attributes="border='0' rules='rows' width='1100'",
  add.to.row=list(pos=list(0,30),
              command=c("<tr width='600'>
                            <th></th>
                            <th>POPULATION</th>
                            <th colspan='2'>MORTALITY<sup>b</sup></th>
                            <th colspan='2'>HIV-POSITIVE TB MORTALITY</th>
                            <th colspan='2'>PREVALENCE</th>
                            <th colspan='2'>INCIDENCE</th>
                            <th colspan='2'>HIV-POSITIVE INCIDENT TB CASES</th>
                        </tr>",
                      "<tr>
                        <td colspan='12'>
                        <sup>a</sup> Numbers for mortality, prevalence and incidence shown to two significant figures. Totals (HBCs, regional and global) are computed prior to rounding.<br />

                        <sup>b</sup> Mortality excludes deaths among HIV-positive TB cases. Deaths among HIV-positive TB cases are classified as HIV deaths according to ICD-10 and are shown separately in this table.<br />

                        <sup>c</sup>Estimates for India have not yet been officially approved by the Ministry of Health and Family Welfare, Government of India, and should therefore be considered provisional.

                        </td>
                      </tr>")))

  tablecopy("2_1_burden_num")

  # 2_2_burden_rt --------------------------------------------------------

  tahvars <- c("e_pop_num", "e_mort_exc_tbhiv_100k", "e_mort_exc_tbhiv_100k_lo", "e_mort_exc_tbhiv_100k_hi", "e_mort_tbhiv_100k", "e_mort_tbhiv_100k_lo", "e_mort_tbhiv_100k_hi", "e_prev_100k", "e_prev_100k_lo", "e_prev_100k_hi", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_tbhiv_prct", "e_tbhiv_prct_lo", "e_tbhiv_prct_hi")
  tai <- subset(e.t, g_hbc22=='high' & year==report_year-1, c('country', tahvars))

  names(tai)[1] <- 'group_name'
  tah <- rbind(tai, subset(araw.t, group_type %in% c('global', 'g_whoregion', "g_hbc22") & year==report_year-1, c('group_name', tahvars)))
  
  # A fudge to make a row for HBC aggregates. I wonder if I can get Hazim to store these in the database...
  load(paste(rdata_folder, "Extra data/PG/hbc.Rdata", sep="/"))
  fuj <- hbc %>% data.frame() %>% filter(year==report_year-1) %>% transmute(e_pop_num=e.pop.num, e_mort_exc_tbhiv_100k=mort.nh, e_mort_exc_tbhiv_100k_lo=mort.nh.lo, e_mort_exc_tbhiv_100k_hi=mort.nh.hi, e_mort_tbhiv_100k=mort.h, e_mort_tbhiv_100k_lo=mort.h.lo, e_mort_tbhiv_100k_hi=mort.h.hi, e_prev_100k=prev, e_prev_100k_lo=prev.lo, e_prev_100k_hi=prev.hi, e_inc_100k=inc, e_inc_100k_lo=inc.lo, e_inc_100k_hi=inc.hi, e_tbhiv_prct=tbhiv*100, e_tbhiv_prct_lo=tbhiv.lo*100, e_tbhiv_prct_hi=tbhiv.hi*100) %>% mutate(group_name="High-burden countries")
  
  tah <- rbind(tah, fuj)
  

  names(tah)[1] <- 'rowname'

  # Fix names
  tah <- .shortnames(tah, col="rowname", ord='hbc')

  tah[2] <- rounder(tah[[2]]/1000)

  for(col in 3:14){
  tah[col] <- frmt(tah[[col]], rates=TRUE)
  }

  for(col in 15:17){
  tah[col] <- ifelse(tah[[col]] < 0.1, "<0.1", ifelse(tah[[col]] < 1, signif(tah[[col]],2), signif(tah[[col]],3) ))
  }

  # combine range columns
  tah$e_mort_exc_tbhiv_range <- paste0("(", tah$e_mort_exc_tbhiv_100k_lo, "\u2013", tah$e_mort_exc_tbhiv_100k_hi, ")")
  tah$e_mort_tbhiv_range <- paste0("(", tah$e_mort_tbhiv_100k_lo, "\u2013", tah$e_mort_tbhiv_100k_hi, ")")
  tah$e_prev_range <- paste0("(", tah$e_prev_100k_lo, "\u2013", tah$e_prev_100k_hi, ")")
  tah$e_inc_range <- paste0("(", tah$e_inc_100k_lo, "\u2013", tah$e_inc_100k_hi, ")")
  tah$e_tbhiv_range <- paste0("(", tah$e_tbhiv_prct_lo, "\u2013", tah$e_tbhiv_prct_hi, ")")

  tam <- subset(tah, select=c("rowname", "e_pop_num", "e_mort_exc_tbhiv_100k", "e_mort_exc_tbhiv_range","e_mort_tbhiv_100k", "e_mort_tbhiv_range", "e_prev_100k", "e_prev_range", "e_inc_100k", "e_inc_range", "e_tbhiv_prct", "e_tbhiv_range"))


  # Add footnotes for some countries (e.g. Bangladesh, India, etc.)
  tam[tam$rowname=="Bangladesh", "rowname"] <- "Bangladesh(b)"

  cat(paste("<font size=5><b>Table 2.2 Estimated epidemiological burden of TB, ", report_year-1, ".</b> Best estimates are followed by the lower and upper bounds of the 95% uncertainty interval. Rates per 100 000 population except where indicated.</font>", sep=""), file=paste0("Tables/2_2_burden_rt", Sys.Date(), ".htm"))

  taj <- xtable(tam)
  print(taj, type="html", file=paste0("Tables/2_2_burden_rt", Sys.Date(), ".htm"), include.rownames=F, include.colnames=F, append=T,
  html.table.attributes="border='0' rules='rows' width='1100'",
  add.to.row=list(pos=list(0,30),
                  command=c("<tr width='600'>
                            <th></th>
                            <th>POPULATION<br />(THOUSANDS)</th>
                            <th colspan='2'>MORTALITY<sup>a</sup></th>
                            <th colspan='2'>HIV-POSITIVE<br />TB MORTALITY</th>
                            <th colspan='2'>PREVALENCE</th>
                            <th colspan='2'>INCIDENCE</th>
                            <th colspan='2'>HIV PREVALENCE IN<br />INCIDENT TB CASES (%)</th>
                            </tr>",
                            paste("<tr>
                            <td colspan=12>
                            <sup>a</sup> Mortality excludes deaths among HIV-positive TB cases. Deaths among HIV-positive TB cases are classified as HIV deaths according to ICD-10 and are shown separately in this table.<br />

                            <sup>b</sup> ", bangladesh.est.foot,"
                            </td></tr>"))))

  tablecopy("2_2_burden_rt")

  # for country profiles

  taca <- subset(e.t, g_hbc22=='high' & year==report_year-1, select=c('country', 'year', "e_mort_exc_tbhiv_num", "e_mort_exc_tbhiv_num_lo", "e_mort_exc_tbhiv_num_hi", "e_mort_exc_tbhiv_100k", "e_mort_exc_tbhiv_100k_lo", "e_mort_exc_tbhiv_100k_hi", "e_prev_num", "e_prev_num_lo", "e_prev_num_hi", "e_prev_100k", "e_prev_100k_lo", "e_prev_100k_hi", "e_inc_num", "e_inc_num_lo", "e_inc_num_hi", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_num", "e_inc_tbhiv_num_lo", "e_inc_tbhiv_num_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "c_cdr", "c_cdr_lo", "c_cdr_hi"))

  nums <- names(taca)[grep("num", names(taca))]
  rates <- names(taca)[grep("100k", names(taca))]

  tada <- taca

  for(var in nums){
  tada[var] <- frmt(taca[[var]] / 1000, thou=TRUE)
  }
  for(var in rates){
  tada[var] <- frmt(tada[[var]], rates=TRUE)
  }

  write.csv(tada, file=paste0("Tables/burden_cp", Sys.Date(), ".csv"), row.names=FALSE)

  # end of estimates tables
}



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 3 ------
# TB Case notifications and treatment outcomes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 3_1_notif -------------------------------------------------------------------
notif <- subset(n,
                year==report_year-1,
                select=c("country", "year", "g_whoregion", "g_hbc22", "c_notified", "c_newinc", "new_labconf", "new_clindx", "new_ep", "ret_rel_labconf", "ret_rel_clindx", "ret_rel_ep", "ret_nrel"))

# create a combined table with HBCs and aggregates
notif_table <- hbc_and_aggs_table(df = notif, country.col = 1, year.col = 2, data.cols = 5:ncol(notif) )

# calculate % of cases that are pulmonary
notif_table$nrpulm_lab_pct <- (sum_of_row(notif_table[c("new_labconf", "ret_rel_labconf")] ) * 100 /
                               sum_of_row(notif_table[c("new_labconf", "new_clindx", "ret_rel_labconf", "ret_rel_clindx")]))

# format data
for(var in 3:ncol(notif_table)){
  notif_table[var] <- rounder(notif_table[[var]])
}
notif_table$nrpulm_lab_pct <- ifelse(is.na(notif_table$nrpulm_lab_pct), "\u2013", notif_table$nrpulm_lab_pct)


# Create HTM output
notif_table_html <- xtable(notif_table[c("area", "c_notified", "c_newinc", "ret_nrel", "new_labconf", "new_clindx", "new_ep", "ret_rel_labconf", "ret_rel_clindx", "ret_rel_ep", "nrpulm_lab_pct")])

digits(notif_table_html) <- 0

cat(paste("<h3>Table 3.1 Case notifications,", report_year-1, "</h3>"), file=paste0("Tables/3_1_notif", Sys.Date(), ".htm"))

print(notif_table_html, type="html", file=paste0("Tables/3_1_notif", Sys.Date(), ".htm"),
      include.rownames=F, include.colnames=F,
      html.table.attributes="border='0' rules='rows' width='1100' cellpadding='5'", append=T,
      add.to.row=list(pos=list(0, nrow(notif_table_html)),
            command=c("<tr>
                          <td colspan='4' style='border-right: solid 2px black;'></td>
                          <th colspan='3' style='border-right: solid 2px black;'>NEW OR PREVIOUS TREATMENT HISTORY UNKNOWN</th>
                          <th colspan='3' style='border-right: solid 2px black;'>RELAPSE</th>
                          <td></td>
                      </tr>
                      <tr>
                          <td></td>
                          <td>TOTAL NOTIFIED</td>
                          <td>NEW AND RELAPSE<sup>a</sup></td>
                          <td style='border-right: solid 2px black;'>RETREATMENT EXCLUDING RELAPSE</td>
                          <td>PULMONARY BACTERIO-<br />LOGICALLY CONFIRMED</td>
                          <td>PULMONARY CLINICALLY DIAGNOSED</td>
                          <td style='border-right: solid 2px black;'>EXTRA-<br />PULMONARY</td>
                          <td>PULMONARY BACTERIO-<br />LOGICALLY CONFIRMED</td>
                          <td>PULMONARY CLINICALLY DIAGNOSED</td>
                          <td style='border-right: solid 2px black;'>EXTRA-<br />PULMONARY</td>
                          <td>PERCENTAGE OF PULMONARY CASES BACTERIO-<br />LOGICALLY CONFIRMED</td>
                      </tr>",
                      "<tr><td colspan='11'>Blank cells indicate data not reported.<br />

                            <sup>a</sup> New and relapse includes cases for which the treatment history is unknown.</td>
                      </tr>")))

tablecopy("3_1_notif")

# and now clear up the mess left behind
rm(list=c("notif", "notif_table", "notif_table_html"))


# 3_2_agesex -------------------------------------------------------------------

# Get country data
agesex <- subset(n,
                 year == report_year-1,
                 select=c("country", "year", "g_whoregion", "g_hbc22", "newrel_m014", "newrel_mu","newrel_f014", "newrel_fu", "newrel_m15plus", "newrel_f15plus", "newrel_sexunk014", "newrel_sexunk15plus", "newrel_sexunkageunk"))


# Calculate totals <15, >= 15, male, female for each line
# (unknowns will be removed for this var later, but for now they are needed for age % and aggregates)
agesex$all.014      <- sum_of_row(agesex[c("newrel_m014", "newrel_f014", "newrel_sexunk014")])
agesex$all.totalage <- sum_of_row(agesex[c("all.014", "newrel_m15plus", "newrel_f15plus", "newrel_sexunk15plus")])
agesex$all.15plus   <- sum_of_row(agesex[c("newrel_m15plus", "newrel_f15plus", "newrel_sexunk15plus")])
agesex$ageunk       <- sum_of_row(agesex[c("newrel_mu", "newrel_fu", "newrel_sexunkageunk")])
agesex$all.male     <- sum_of_row(agesex[c("newrel_m014", "newrel_m15plus", "newrel_mu")])
agesex$all.female   <- sum_of_row(agesex[c("newrel_f014", "newrel_f15plus", "newrel_fu")])


# Create a combined table with HBCs and aggregates
agesex_table <- hbc_and_aggs_table(df = agesex, country.col = 1, year.col = 2, data.cols = 5:ncol(agesex) )

# Add an asterisk to the name if country did not include relapse cases in the age/sex table,
# but only if they reported some data!
asterisks <- .shortnames(subset(n, rel_in_agesex_flg==0 & g_hbc22=="high" & year==report_year-1, country))
agesex_table$area <- ifelse(agesex_table$area %in% asterisks$country & !is.na(agesex_table$all.014) ,
                            paste0(agesex_table$area, "*"), agesex_table$area)

# Cut number of variables
agesex_vars <- c("area", "newrel_m014", "newrel_f014", "all.014", "all.15plus", "ageunk", "all.totalage", "all.male", "all.female")

agesex_table <- agesex_table[agesex_vars]

# Calculate % child. (Removed for all new where countries didn't report)
agesex_table$all.child.pct <- ifelse(is.na(agesex_table$all.totalage) , "\u2013",
                                     frmt(agesex_table$all.014  * 100 / agesex_table$all.totalage))

# Calculate male female ratio. (Removed for all new where countries didn't report)
agesex_table$all.mf.ratio <- ifelse(is.na(agesex_table$all.male) |
                                      is.na(agesex_table$all.female) |
                                      agesex_table$all.male==0 |
                                      agesex_table$all.female==0, "\u2013",
                                    frmt(agesex_table$all.male / agesex_table$all.female))

# format data
for(var in agesex_vars[2:7]){
  agesex_table[var] <- rounder(agesex_table[[var]])
}


agesex_table_html <- xtable(subset(agesex_table, select=c("area", "all.014", "all.15plus", "ageunk", "all.child.pct", "all.mf.ratio")))

# Add to file

cat(paste0("<h2>Table 3.2 Notifications of new and relapse TB cases by age and sex, ", report_year-1, '</h2>'), file=paste0("Tables/3_2_agesex", Sys.Date(), ".htm"))

print(agesex_table_html, type="html",
      file=paste0("Tables/3_2_agesex", Sys.Date(), ".htm"),
      include.rownames=F, include.colnames=F, append=TRUE,
      add.to.row=list(pos=list(0,nrow(agesex_table_html)),
            command=c("<tr> <td></td>
                      <td>0-14 YEARS</td>
                      <td>&#8805;15 YEARS</td>
                      <td>AGE UNKNOWN</td>
                      <td>% AGED < 15 YEARS</td>
                      <td>MALE/FEMALE RATIO</td>
                      </tr>",
                      "<tr><td colspan='6'>Blank cells indicate data that could not be reported for the age categories shown.<br />
                      \u2013 indicates values that cannot be calculated.<br />
                      &ast; New cases only.
                      </td> </tr>")))

tablecopy("3_2_agesex")

# and now clear up the mess left behind
rm(list=c("agesex", "agesex_table", "agesex_table_html", "asterisks"))

# 3_5_cdr -------------------------------------------------------------------

if(!identical(e.t, e)) warning('These estimates are currently phony!')

# get country rows
tda <- subset(e.t, g_hbc22=='high' & year %in% c(1995, 2000, 2005, 2010, report_year-1), select=c('country', 'year', 'g_hbc22', 'g_whoregion', 'c_cdr_lo', 'c_cdr', 'c_cdr_hi'))
tda$group_name <- tda$country

# get HBC aggregate row (requires PG's add.rv estimates aggregator)
tda1 <- merge(n.t, e.t)
tdhb <- subset(tda1, g_hbc22=='high' & year %in% c(1995, 2000, 2005, 2010, report_year-1), select=c('country', 'year', 'g_hbc22', 'g_whoregion', 'e_inc_100k_lo', 'e_inc_100k', 'e_inc_100k_hi', 'e_pop_num', 'c_newinc'))

tdhb2 <- aggregate(tdhb['c_newinc'], by=list(year=tdhb$year), FUN=sum, na.rm=TRUE)
tdhb2[c('e_inc_num_lo', 'e_inc_num', 'e_inc_num_hi')] <- NA

for(yea in unique(tdhb$year)){
tdhb3 <- subset(tdhb, year==yea)
tdhb4 <- add.rv(tdhb3$e_inc_100k/1e5, tdhb3$e_inc_100k_lo/1e5, tdhb3$e_inc_100k_hi/1e5, weights=tdhb3$e_pop_num)
tdhb2[tdhb2$year==yea, c('e_inc_num_lo', 'e_inc_num', 'e_inc_num_hi')] <- tdhb4[c('r.lo.num', 'r.num', 'r.hi.num')]
}

tdhb2 <- within(tdhb2, {
group_name <- 'High-burden countries'
c_cdr_lo <- round(c_newinc / e_inc_num_hi *100)
c_cdr <- round(c_newinc / e_inc_num *100)
c_cdr_hi <- round(c_newinc / e_inc_num_lo *100)
})

# get aggregates for Regions and global
tdr <- subset(a.t, group_type %in% c('g_whoregion', 'global') & year %in% c(1995, 2000, 2005, 2010, report_year-1), select=c('group_name', 'year', 'c_cdr_lo', 'c_cdr', 'c_cdr_hi'))

# Merge and reshape
tdb <- merge(tda, tdhb2, all=TRUE)
tdb <- merge(tdb, tdr, all=TRUE)

# combine bounds into one column and format missings
tdb$c_cdr_bnd <- ifelse(is.na(tdb$c_cdr_lo), "\u2013", paste0("(", frmt(tdb$c_cdr_lo), "\u2013", frmt(tdb$c_cdr_hi), ")"))
tdb$c_cdr <- ifelse(is.na(tdb$c_cdr_lo), "\u2013", frmt(tdb$c_cdr))

tdc <- melt(tdb, measure.vars=c('c_cdr', 'c_cdr_bnd'), id=c('group_name', 'year'))

tdd <- cast(tdc, group_name~...)

# Order table, fix names
ordrd <- c(sort(unique(tda$group_name)), 'High-burden countries', sort.int(unique(tdr[tdr$group_name != 'global', 'group_name'])), 'global')
tdd <- tdd[match(ordrd, tdd$group_name),]

tde <- .shortnames(tdd, col='group_name', ord = "hbc")

cat(paste("<font size=5><b>Table 3.5 Estimates of the case detection rate for new and relapse cases (%), 1995\u2013", report_year-1, ".<sup>a</sup></b>  Best estimates are followed by the lower and upper bounds of the 95% uncertainty interval.</font>", sep=""), file=paste0("Tables/3_5_cdr", Sys.Date(), ".htm"))

tdf <- xtable(tde)
# digits(tdf) <- 1
print(tdf, type="html",
      file=paste0("Tables/3_5_cdr", Sys.Date(), ".htm"),
      include.rownames=F, include.colnames=F, append=T,
      html.table.attributes="border='0' rules='rows' width='900'",
      add.to.row=list(pos=list(0,30),
        command=c(paste0("<tr>
                           <th></th>
                           <th colspan='2'>1995</th>
                           <th colspan='2'>2000</th>
                           <th colspan='2'>2005</th>
                           <th colspan='2'>2010</th>
                           <th colspan='2'>", report_year-1, "</th>
                         </tr>"),
                        "<tr><td colspan='11'>

                        \u2013 indicates values that cannot be calculated.<br />

                        <sup>a</sup> Estimates for all years are recalculated as new information becomes available and techniques are refined, so they may differ from those published previously. The lower and upper bounds are defined as the 2.5th and 97.5th centiles of outcome distributions produced in simulations.

                        </td></tr>")))

tablecopy('3_5_cdr')


# 3_6_tsr -------------------------------------------------------------------
# Treatment success rates for all new cases

tsr <- subset(o,
              year>=1995 & year < report_year-1,
              select=c("country", "year", "g_hbc22", "g_whoregion", "new_sp_coh", "new_sp_cur", "new_sp_cmplt", "new_snep_coh", "new_snep_cmplt", "newrel_coh", "newrel_succ"))

# Combine old smear pos and smear neg cohorts
tsr$new_succ <- sum_of_row(tsr[c("new_sp_cur", "new_sp_cmplt", "new_snep_cmplt")])
tsr$new_coh <- sum_of_row(tsr[c("new_sp_coh", "new_snep_coh")])


# create a combined table with HBCs and aggregates
tsr_table <- hbc_and_aggs_table(df = tsr, country.col = 1, year.col = 2, data.cols = 10:ncol(tsr) )

# Calculate cohort size (coh) and treatment success rate (tsr) (note that variables changed after 2011 cohort)

tsr_table$coh <- ifelse(tsr_table$year > 2011, tsr_table$newrel_coh, tsr_table$new_coh)

tsr_table$tsr <- ifelse(tsr_table$year > 2011,
                        tsr_table$newrel_succ  * 100 / tsr_table$newrel_coh,
                        tsr_table$new_succ  * 100 / tsr_table$new_coh)


# Subset for 5 year intervals
tsr_table <- subset(tsr_table, year %in% c(seq(1995, report_year-4, 5), (report_year-4):(report_year-2)))

# format data
tsr_table$coh <- rounder(tsr_table$coh / 1000, decimals=TRUE)
tsr_table$tsr <- ifelse(is.na(tsr_table$tsr), "\u2013", frmt(tsr_table$tsr))

# Pivot round to have years on top
coh_pivoted <- cast(tsr_table, area~year, value="coh")
tsr_pivoted <- cast(tsr_table, area~year, value="tsr")

# Re-sort into the standard table order
rownames(coh_pivoted) <- as.character(coh_pivoted$area)
rownames(tsr_pivoted) <- as.character(tsr_pivoted$area)

coh_pivoted <- coh_pivoted[standard_table_order, ]
tsr_pivoted <- tsr_pivoted[standard_table_order, ]

# Add an asterisk to the name if country included relapse cases in the outcomes cohort DELETE THIS COMEMENT
# Count the number of countries without relapse included for the footnote.
include.relapse <- o %>% filter(year >= 2012, g_hbc22=="high", rel_with_new_flg==1) %>% group_by(year) %>% summarize(total=n()) 

# # coh_pivoted$area <- ifelse(coh_pivoted$area %in% asterisks$country,
#                            paste0(coh_pivoted$area, "*"), coh_pivoted$area)
# 
# tsr_pivoted$area <- ifelse(tsr_pivoted$area %in% asterisks$country,
#                             paste0(tsr_pivoted$area, "*"), tsr_pivoted$area)

# remove name from the area variable so that it won't appear in the HTML output
names(coh_pivoted)[1] <- ""
names(tsr_pivoted)[1] <- ""


# Create HTM output

coh_table_html <- xtable(coh_pivoted)
tsr_table_html <- xtable(tsr_pivoted)

digits(coh_table_html) <- 0
digits(tsr_table_html) <- 0

cat(paste("<h3>Table 3.6 Treatment success for all new and relapse<sup>a</sup> cases (%) and cohort size (thousands), 1995", "\u2013", report_year-2, "</h3>
          <p>a. Treatment success (%)</p>", sep=""), file=paste0("Tables/3_6_tsr", Sys.Date(), ".htm"))


print(tsr_table_html, type="html", file=paste0("Tables/3_6_tsr", Sys.Date(), ".htm"),include.rownames=F, include.colnames=T, append=T, html.table.attributes="border=0 rules=rows width=900")

cat("<p>b. Cohort size (thousands)</p>", file=paste0("Tables/3_6_tsr", Sys.Date(), ".htm"), append=T)

print(coh_table_html, type="html",
      file=paste0("Tables/3_6_tsr", Sys.Date(), ".htm"),
      include.rownames=F, include.colnames=T, append=T,
      html.table.attributes="border='0' rules='rows' width='900'",
      add.to.row=list(pos=list(30),
          command=c(paste0("<tr><td colspan=", ncol(coh_table_html), ">Blank cells indicate data not reported.<br />
                          \u2013 indicates values that cannot be calculated.<br />
                          <sup>a</sup> Cohorts before 2012 include new cases only. For the ", lister.text(include.relapse$year), " cohorts, ", lister.text(include.relapse$total), " high-burden countries respectively included both new and relapse cases, as recommended in the revised recording and reporting framework issued by WHO in 2013 (see Definitions and reporting framework for tuberculosis - 2013 revision.  Geneva, World Health Organization, 2013 (WHO/HTM/TB/2013.2). Available at www.who.int/tb/publications/definitions.</td></tr>", sep=""))))

tablecopy("3_6_tsr")

# and now clear up the mess left behind
rm(list=c("tsr", "tsr_table", "tsr_pivoted", "tsr_table_html", "coh_pivoted", "coh_table_html"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 5 ------
# Diagnostics and laboratory strengthening
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 5_* lab_capac & lab_policy ------------------------------------

# NOTE: Non reporters not included in aggregates

tfa <- subset(s, year==report_year-1, select=c(country, year, g_whoregion, g_hbc22, g_hbmdr27, c_lab_sm_100k, c_lab_cul_5m, c_lab_dst_5m, lab_sm_f, lab_sm_led, lab_cul_f, lab_dst_f, lab_lpa_f, lab_xpert, dst_in_guide, lc_rst_in_guide, sp_case_dfn_in_guide, lpa_in_guide, dx_alg_tbhiv_in_guide, xpert_in_guide, xpert_in_guide_TBHIV, xpert_in_guide_MDR, xpert_in_guide_children, xpert_in_guide_eptb, xpert_in_guide_tb))

tfb <- merge(tfa, p[c('country', 'year', 'e_pop_num')], all.x=T)

# get pops for data reported
# Non-reporters are left out of aggregation
tfb <- within(tfb, {
e_pop_num <- as.numeric(e_pop_num)
pop_sm <- ifelse(is.na(lab_sm_f), NA, e_pop_num)
pop_cul <- ifelse(is.na(lab_cul_f), NA, e_pop_num)
pop_dst <- ifelse(is.na(lab_dst_f), NA, e_pop_num)
pop_lpa <- ifelse(is.na(lab_lpa_f), NA, e_pop_num)
})

for(poli in c("dst_in_guide", "lc_rst_in_guide", "lpa_in_guide", "sp_case_dfn_in_guide", "dx_alg_tbhiv_in_guide", "xpert_in_guide", "xpert_in_guide_TBHIV", "xpert_in_guide_MDR", "xpert_in_guide_children", "xpert_in_guide_eptb", "xpert_in_guide_tb")){
tfb[paste0(poli, '_n')] <- ifelse(is.na(tfb[poli]), 0, 1)
}

tfhbc22 <- aggregate(tfb[9:ncol(tfb)], by=list(country=tfb$g_hbc22), FUN=sum, na.rm=T)
tfhbc22 <- tfhbc22[tfhbc22$country=='high',]
tfhbc22$country <- 'High-burden countries'

tfhbmdr27 <- aggregate(tfb[9:ncol(tfb)], by=list(country=tfb$g_hbmdr27), FUN=sum, na.rm=T)
tfhbmdr27 <- tfhbmdr27[tfhbmdr27$country=='high',]
tfhbmdr27$country <- 'High MDR-TB burden countries'

tfreg <- aggregate(tfb[9:ncol(tfb)], by=list(country=tfb$g_whoregion), FUN=sum, na.rm=T)

tfglob <- aggregate(tfb[9:ncol(tfb)], by=list(country=tfb$year), FUN=sum, na.rm=T)
tfglob$country <- 'Global'

tfc <- rbind(tfb[tfb$g_hbc22=='high' | tfb$g_hbmdr27=='high', c(1, 9:ncol(tfb))], tfhbc22, tfhbmdr27, tfreg, tfglob)
tfc$ord <- 1:nrow(tfc)
tfd <- merge(tfc, tfa[c('country', 'g_hbc22', 'g_hbmdr27')], all.x=T)
tfd <- tfd[order(tfd$ord),]
tfd <- .shortnames(tfd, ord='hbc.mdr')

for(poli in c("dst_in_guide", "lc_rst_in_guide", "lpa_in_guide", "sp_case_dfn_in_guide", "dx_alg_tbhiv_in_guide", "xpert_in_guide", "xpert_in_guide_TBHIV", "xpert_in_guide_MDR", "xpert_in_guide_children", "xpert_in_guide_eptb", "xpert_in_guide_tb")) {
tfd[is.na(tfd$g_hbc22), poli] <- paste0(round(tfd[is.na(tfd$g_hbc22), poli] / tfd[is.na(tfd$g_hbc22), paste0(poli, '_n')] * 100, 0) , "%")
tfd[!is.na(tfd$g_hbc22), poli] <- ifelse(tfd[!is.na(tfd$g_hbc22), poli]=='1', 'Y', tfd[!is.na(tfd$g_hbc22), poli])
tfd[!is.na(tfd$g_hbc22), poli] <- ifelse(tfd[!is.na(tfd$g_hbc22), poli]=='0', 'N', tfd[!is.na(tfd$g_hbc22), poli])
tfd[!is.na(tfd$g_hbc22), poli] <- ifelse(is.na(tfd[!is.na(tfd$g_hbc22), poli]), '', tfd[!is.na(tfd$g_hbc22), poli])
}

tfd <- within(tfd, {
c_sm_100k <- ifelse(is.na(lab_sm_f), "\u2013", round.conv(lab_sm_f / pop_sm * 100000))
c_cul_5m <- ifelse(is.na(lab_cul_f), "\u2013", round.conv(lab_cul_f / pop_cul * 5000000))
c_dst_5m <- ifelse(is.na(lab_dst_f), "\u2013", round.conv(lab_dst_f / pop_dst * 5000000))
c_lpa_5m <- ifelse(is.na(lab_lpa_f), "\u2013", round.conv(lab_lpa_f / pop_lpa * 5000000))

g_hbc22 <- ifelse(g_hbc22=='high', 'X', NA)
g_hbmdr27 <- ifelse(g_hbmdr27=='high', 'X', NA)

lab_sm_led_pct <- ifelse(is.na(lab_sm_led) | is.na(lab_sm_f), "\u2013", rounder(lab_sm_led / lab_sm_f * 100))
lab_sm_f <- rounder(lab_sm_f)
lab_cul_f <- rounder(lab_cul_f)
lab_dst_f <- rounder(lab_dst_f)
lab_lpa_f <- rounder(lab_lpa_f)
lab_xpert <- rounder(lab_xpert)
})

# Remove aggregates for sums
tfd[37:45, c("lab_sm_f", "lab_sm_led", "lab_cul_f", "lab_dst_f", "lab_lpa_f", "lab_xpert")] <- "\u2013"

# 5_1_lab_capac ----

tfe <- xtable(tfd[c("country", "g_hbc22", "g_hbmdr27", "lab_sm_f", "c_sm_100k", "lab_sm_led_pct", "lab_cul_f", "c_cul_5m", "lab_dst_f", "c_dst_5m", "lab_lpa_f", "c_lpa_5m", "lab_xpert")], align=c('l', 'l', rep('c',12)))

# Footnote 1
tfefoot <- ifelse(any(is.na(tfe[c(4,7,9,11,13)])), "Blank cells indicate data not reported.<br />", "")

print(tfe, type="html",
      file=paste0("Tables/5_1_lab_capac", Sys.Date(), ".htm"),
      include.rownames=F, include.colnames=F,
      #sanitize.text.function=identity, #this makes <0.1 disappear in word even if not in a browser.
      html.table.attributes="border='1'  width='1100' cellpadding='5'",
      add.to.row=list(pos=list(0, nrow(tfe)),
        command=c(paste0("<h2 align=\"left\" >Table 5.1 Laboratory capacity, ", report_year-1, "<sup>a</sup></h2>
                        <tr>
                          <th colspan='3'></th>
                          <th colspan=3>SMEAR MICROSCOPY</th>
                          <th colspan=2>CULTURE</th>
                          <th colspan=2>DRUG SUSCEPTIBILITY TESTING</th>
                          <th colspan=2>LINE PROBE ASSAY</th>
                          <th>XPERT MTB/RIF</th>
                        </tr>
                        <tr>
                          <td></td>
                          <td>HIGH TB BURDEN</td>
                          <td>HIGH MDR-TB BURDEN</td>
                          <td>NUMBER OF LABORATORIES</td>
                          <td>LABORATORIES PER 100 000 POPULATION</td>
                          <td>PERCENTAGE OF LABORATORIES USING LED MICROSCOPES</td>
                          <td>NUMBER OF LABORATORIES</td>
                          <td>LABORATORIES PER 5 MILLION POPULATION</td>
                          <td>NUMBER OF LABORATORIES</td>
                          <td>LABORATORIES PER 5 MILLION POPULATION</td>
                          <td>NUMBER OF LABORATORIES</td>
                          <td>LABORATORIES PER 5 MILLION POPULATION</td>
                          <td>NUMBER OF SITES</td>
                        </tr>"),
                  paste0("<tr>
                            <td colspan='13'>", tfefoot, "\u2013 indicates values that cannot be calculated.<br />

                            <sup>a</sup> The regional and global figures are aggregates of data reported by low- and middle-income countries and territories. Data for the variables shown in the table are not requested from high-income countries in the WHO data collection form.
                            </td> </tr>"))))

tablecopy("5_1_lab_capac")

# 5_2_lab_policy ####

tff <- xtable(tfd[c("country", "g_hbc22", "g_hbmdr27", "xpert_in_guide_TBHIV", "xpert_in_guide_MDR", "xpert_in_guide_children", "xpert_in_guide_eptb")], align=c('l', 'l', rep('c',6)))

# Footnote 1
tffoot <- ifelse(any(is.na(tff[4:ncol(tff)])), "Blank cells indicate data not reported.<br>", "")

print(tff, type="html", file=paste0("Tables/5_2_lab_policy", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, #sanitize.text.function=identity, 
      html.table.attributes="border=0 rules=rows width=1100 cellpadding=0", add.to.row=list(pos=list(0, nrow(tff)), command=c(paste0("<h2 align=\"left\">Table 5.2 Incorporation of WHO policy guidance on Xpert MTB/RIF, ", report_year-1, "<sup>a</sup></h2>

<TR> <TH colspan=3></TH> <TH colspan=4 style='border: solid 1px black;'>XPERT MTB/RIF AS THE INITIAL DIAGNOSTIC TEST</TH> </TR>
<TR> <TH></TH> <TH>HIGH TB BURDEN</TH> <TH style='border-right: solid 1px black;'>HIGH MDR-TB BURDEN</TH>
<TH>PEOPLE LIVING WITH HIV</TH>
<TH>PEOPLE AT RISK OF DRUG-RESISTANT TB</TH>
<TH>CHILDREN SUSPECTED OF HAVING TB</TH>
<TH>EXTRAPULMONARY TB USING SELECTED SPECIMENS</TH> </TR>"),
                                                                                                                              paste0("<TR> <TD colspan=8>", tffoot, "<sup>a</sup> The regional and global figures are aggregates of data reported by low- and middle-income countries and territories. Data for the variables shown in the table are not requested from high-income countries in the WHO data collection form. </TR>"))))

tablecopy("5_2_lab_policy")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 6 ------
# Addressing the co-epidemics of TB and HIV
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# 6_1_tbhiv -------------------------------------------------------------------
tea <- merge(n.t[c('country', 'year', "g_whoregion", 'g_hbhiv41', "hiv_ipt", 'hiv_tbdetect', 'hiv_reg_new2')], tbhiv[c('country', 'year', "hivtest", "hivtest_pct_denominator", "hivtest_pos_pct_denominator", "hivtest_pos_pct_numerator", "hiv_cpt_pct_numerator", "hiv_cpt_pct_denominator", "hiv_art_pct_numerator", "hiv_art_pct_denominator", 'hivtest_pct_numerator', 'hivtest_pos', 'hiv_cpt', 'hiv_art')])

tea <- merge(tea, e.t[c('country', 'year', 'e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')], all.x=T)

tea <- subset(tea, year==report_year-1)

# Get TB/HIV high burden countries only
te1 <- tea[tea$g_hbhiv41=='high', c(1, 5:ncol(tea))]
te1 <- .shortnames(te1, ord='tbhiv')
te1 <- te1[order(te1$country),]

# Get total of high tbhiv burden countries
te2 <- te1
te2['country'] <- 'High TB/HIV burden countries'
te2 <- aggregate(te2[2:ncol(te2)], by=list(country=te2$country), FUN=sum, na.rm=T)

# Get global totals
teb <- aggregate(tea[5:ncol(tea)], by=list(country=tea$g_whoregion), FUN=sum, na.rm=T)
tec <- teb
tec[1] <- 'Global'
tec <- aggregate(tec[2:ncol(tec)], by=list(country=tec$country), FUN=sum, na.rm=T)

teda <- te1
# teda$type <- 'country'

tedb <- rbind(te2, teb, tec)
# tedb$type <- 'agg'

# Combine all together
ted <- rbind(teda, tedb)

# replace aggregate estimates with properly aggregated numbers
ted1 <- e.t[e.t$year==report_year-1 & !is.na(e.t$e_inc_tbhiv_100k) & e.t$g_hbhiv41=='high' , c('iso3', 'e_inc_tbhiv_100k', 'e_inc_tbhiv_100k_lo', 'e_inc_tbhiv_100k_hi', 'e_pop_num')]
ted1[2:4] <- ted1[2:4] / 100000
ted1$e_pop_num <- as.numeric(ted1$e_pop_num)
ted2 <- add.rv(ted1$e_inc_tbhiv_100k, ted1$e_inc_tbhiv_100k_lo, ted1$e_inc_tbhiv_100k_hi, weights=ted1$e_pop_num)

ted[ted$country=="High TB/HIV burden countries", c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')] <- signif(ted2[c("r.num", "r.lo.num", "r.hi.num")], 2)

ted3 <- a.t[a.t$year==report_year-1 & a.t$group_type %in% c('g_whoregion', 'global'), c('group_name', 'group_type', 'e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')]
ted3 <- ted3[order(ted3$group_type, ted3$group_name),]

ted[43:nrow(ted), c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')] <- ted3[c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')]

tea <- merge(n.t[c('country', 'year', "g_whoregion", 'g_hbhiv41', "hiv_ipt", 'hiv_tbdetect', 'hiv_reg_new2')], tbhiv[c('country', 'year', "hivtest", "hivtest_pct_denominator", "hivtest_pos_pct_denominator", "hivtest_pos_pct_numerator", "hiv_cpt_pct_numerator", "hiv_cpt_pct_denominator", "hiv_art_pct_numerator", "hiv_art_pct_denominator", 'hivtest_pct_numerator', 'hivtest_pos', 'hiv_cpt', 'hiv_art')])

tea <- merge(tea, e.t[c('country', 'year', 'e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')], all.x=T)

tea <- subset(tea, year==report_year-1)

# Get TB/HIV high burden countries only
te1 <- tea[tea$g_hbhiv41=='high', c(1, 5:ncol(tea))]
te1 <- .shortnames(te1, ord='tbhiv')
te1 <- te1[order(te1$country),]

# Get total of high tbhiv burden countries
te2 <- te1
te2['country'] <- 'High TB/HIV burden countries'
te2 <- aggregate(te2[2:ncol(te2)], by=list(country=te2$country), FUN=sum, na.rm=T)

# Get global totals
teb <- aggregate(tea[5:ncol(tea)], by=list(country=tea$g_whoregion), FUN=sum, na.rm=T)
tec <- teb
tec[1] <- 'Global'
tec <- aggregate(tec[2:ncol(tec)], by=list(country=tec$country), FUN=sum, na.rm=T)

# create country/agg flag ("type")
# Format table columns 

ted <- within(ted, {
  hivtest1000 <- rounder(hivtest_pct_numerator / 1000, decimals=TRUE)
  hivtest_prct <- rounder(hivtest_pct_numerator / hivtest_pct_denominator * 100, decimals=TRUE)
  hivtest_pos_prct <- rounder(hivtest_pos_pct_numerator / hivtest_pos_pct_denominator * 100, decimals=TRUE)
  hiv_cpt_prct <- rounder(hiv_cpt_pct_numerator / hiv_cpt_pct_denominator * 100, decimals=TRUE)
  hiv_art_prct <- rounder(hiv_art_pct_numerator / hiv_art_pct_denominator * 100, decimals=TRUE)
  hiv_tb_prct <- rounder(hiv_tbdetect / hiv_reg_new2 * 100, decimals=TRUE) 
  
  hiv_art_est_prct <- rounder(hiv_art_pct_numerator / e_inc_tbhiv_num * 100, decimals=TRUE)
  
  hiv_ipt2 <- rounder(hiv_ipt / 1000, decimals=TRUE)
})



ted <- .shortnames(ted, ord='tbhiv')


ted$e_inc_tbhiv_num <- frmt(ted$e_inc_tbhiv_num / 1000, thou=TRUE)
ted$e_inc_tbhiv_num_lo <- frmt(ted$e_inc_tbhiv_num_lo / 1000, thou=TRUE)
ted$e_inc_tbhiv_num_hi <- frmt(ted$e_inc_tbhiv_num_hi / 1000, thou=TRUE)

# Fix and footnote for specific countries
cty.notes <- NULL

# Ethiopia
if (report_year==2015) {
  ted[ted$country=='Ethiopia', 'country'] <- "Ethiopia(c)"
  cty.notes <- paste(cty.notes, "<sup>c</sup> In 2014, ART and IPT data were missing for 3 of Ethiopia&rsquo;s  11 regions, which in previous years had accounted for about one third of the national totals. In the 8 regions that reported data, 65% of HIV-positive TB patients were on ART. <br />")
  warning("Ethiopia footnote is still in place for TB/HIV table.")
}

# Russia
ted[ted$country=='Russian Federation', 'hivtest1000'] <- paste0(ted[ted$country=='Russian Federation', 'hivtest1000'], '(d)')
ted[ted$country=='Russian Federation', c('hivtest_prct', 'hivtest_pos_prct')] <- NA
warning("Russian Federation modification for the TB/HIV table is still in place. Delete this message when no longer applicable.")
cty.notes <- paste(cty.notes, "<sup>d</sup> Data for the Russian Federation are for new TB patients in the civilian sector only. <br />")

# Uganda
if(report_year==2015 & ted[ted$country=='Uganda', 'hiv_tb_prct']==0.2){
  ted[ted$country=='Uganda', 'hiv_tb_prct'] <- NA
  warning("TB amongst HIV in Uganda was removed in Table 6.1.")
}


ted2 <- ted[c("country", 'e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi', "hivtest1000", "hivtest_prct", "hivtest_pos_prct", "hiv_cpt_prct", "hiv_art_prct", "hiv_art_est_prct", "hiv_ipt2", "hiv_tb_prct")]

# replace uncalculatables
for(var in c(6:10,12)){
ted2[var] <- ifelse(is.na(ted2[[var]]), "\u2013", ted2[[var]])
}

# combine estimates to two lines
ted2$est_rg <- paste0("(", ted2$e_inc_tbhiv_num_lo, "\u2013", ted2$e_inc_tbhiv_num_hi, ")")

ted3 <- ted2[c("country", 'e_inc_tbhiv_num', 'est_rg', "hivtest1000", "hivtest_prct", "hivtest_pos_prct", "hiv_art_prct", "hiv_art_est_prct", "hiv_ipt2", "hiv_tb_prct")]

tee <- xtable(ted3, align=c('l', 'l', 'r', 'l', rep('c',7)))

cat(paste0("<font size='3' align='left'><b>Table 6.1 HIV testing for TB patients, treatment for HIV-positive TB patients and prevention of TB among people living with HIV, 41 high TB/HIV burden countries and WHO regions, ", report_year-1, ".</b> Numbers in thousands except where indicated.</font><br /><br />"), file=paste0("Tables/6_1_tbhiv", Sys.Date(), ".htm"))

print(tee, type="html",
      file=paste0("Tables/6_1_tbhiv", Sys.Date(), ".htm"),
      include.rownames=F, include.colnames=F, append=T,
      html.table.attributes="border='0' rules='rows' width='1100'",
      add.to.row=list(pos=list(0, nrow(tee)),
        command=c("<tr>
                      <th></th>
                      <th colspan='2'>ESTIMATED HIV-POSTIVE INCIDENT TB CASES<sup>a</sup></th>
                      <th>NUMBER OF NOTIFIED TB PATIENTS WITH KNOWN HIV STATUS</th>
                      <th>% OF NOTIFIED TB PATIENTS WITH KNOWN HIV STATUS</th>
                      <th>% of TB PATIENTS WITH AN HIV TEST RESULT WHO WERE HIV-POSITIVE</th>
                      <th>% OF NOTIFIED HIV-POSITIVE TB PATIENTS STARTED ON ART</th>
                      <th>NUMBER OF HIV-POSTIVE TB PATIENTS ON ART AS % OF ESTIMATED HIV-POSITIVE INCIDENT TB CASES<sup>b</sup></th>
                      <th>NUMBER OF HIV-POSITIVE PEOPLE PROVIDED WITH IPT</th>
                      <th>% OF PEOPLE NEWLY ENROLLED IN HIV CARE WHO WERE NOTIFIED AS A TB CASE THE SAME YEAR</th>
                  </tr>",
                  paste0("<tr><td colspan='11'>Blank cells indicate data not reported.<br />

                  \u2013 indicates values that cannot be calculated.<br />

                  <sup>a</sup> Best estimates are followed by the lower and upper bounds of the 95% uncertainty interval.<br />

                  <sup>b</sup> The numerator (i.e. all notified HIV-positive TB cases on ART) includes all notified new, relapse and non-relapse retreatment cases. The denominator (i.e. estimated HIV-positive incident TB cases) includes new and relapse cases only.<br />", cty.notes))))

tablecopy("6_1_tbhiv")


# END ===================================================