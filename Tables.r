# -------------------------------------------------
# Tables in the global report 
# Tom Hiatt
# 6 July 2012, updated 25 June 2015
# -------------------------------------------------

# HT: run the setup manually... but then change this with a whoami !!!!!
# source('d:/users/hiattt/Dropbox/Code/Surveillance reports/Setup.r')


# Link -------------------------------------------------
# Link GTBR2014 table numbers to code sections
#
# 4.1 case notifications: 4_1_notif
# 4.2 case notifications by age and sex: 4_2_agesex.htm
# 
# 4.6 Treatment success for all new TB cases (%) and cohort size: 4_6_all_tsr
# 
# 6.1 Laboratory capacity: 6_1_lab_capac (start at 6_1_2_lab_capac & lab_policy )
# 6.2 Incorporation of WHO policy for diagnosis of TB: 6_2_lab_policy(start at 6_1_2_lab_capac & lab_policy )
# 
# 7.1 HIV testing, treatment for HIV-positive TB patients ..etc: 7_1_tbhiv
# 
# -------------------------------------------------

# ******************************************************
# Old tables ####

# Find this code in past repository commits

# Dropped in 2014
# 'top 10 countries' table
# tsr_sp

# ******************************************************


#flag for whether or not to produce estimates tables
flg_show_estimates <- FALSE

if(flg_show_estimates){

# 2_1_burden_num ----------------------------------------------

tagvars <- c("e_pop_num", "e_mort_exc_tbhiv_num", "e_mort_exc_tbhiv_num_lo", "e_mort_exc_tbhiv_num_hi", "e_mort_tbhiv_num", "e_mort_tbhiv_num_lo", "e_mort_tbhiv_num_hi", "e_prev_num", "e_prev_num_lo", "e_prev_num_hi", "e_inc_num", "e_inc_num_lo", "e_inc_num_hi", "e_inc_tbhiv_num", "e_inc_tbhiv_num_lo", "e_inc_tbhiv_num_hi")
tag1 <- subset(e.t, g_hbc22=='high' & year==thisyear-1, c('country', tagvars))

names(tag1)[1] <- 'group_name'

tag <- rbind(tag1, subset(araw.t, group_type %in% c('global', 'g_whoregion', "g_hbc22") & year==thisyear-1, c('group_name', tagvars)))

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

# Add in India footnote
tak[tak$rowname=="India", "rowname"] <- "India(c)"

cat(paste("<font size=5><b>Estimated epidemiological burden of TB, ", thisyear-1, ".</b> Numbers in thousands.<sup>a</sup></font>", sep=""), file=glue("Tables/2_1_burden_num", Sys.Date(), ".htm"))

tai <- xtable(tak)
print(tai, type="html", file=glue("Tables/2_1_burden_num", Sys.Date(), ".htm"), include.rownames=F, include.colnames=F, append=T,
html.table.attributes="border=0 rules=rows width=1100",
add.to.row=list(pos=list(0,30), command=c("<TR width=600> <TH></TH> <TH>POPULATION</TH> <TH colspan=2>MORTALITY<sup>b</sup></TH> <TH colspan=2>HIV-POSITIVE <br>TB MORTALITY</TH> <TH colspan=2>PREVALENCE</TH> <TH colspan=2>INCIDENCE</TH> <TH colspan=2>HIV-POSITIVE <br>INCIDENT TB CASES</TH> </TR>", 
"<TR> <TD colspan=12><sup>a</sup> Numbers for mortality, prevalence and incidence shown to two significant figures. Best estimates are followed by the lower and upper bounds of the 95% uncertainty interval. Totals (HBCs, regional and global) are computed prior to rounding. <br><sup>b</sup> Mortality excludes deaths among HIV-positive TB cases. Deaths among HIV-positive TB cases are classified as HIV deaths according to ICD-10 and are shown separately in this table. <br><sup>c</sup> Estimates for India have not yet been officially approved by the Ministry of Health & Family Welfare, Government of India, and should therefore be considered provisional.</TD></TR>")))

tablecopy("2_1_burden_num")

# 2_2_burden_rt --------------------------------------------------------

tahvars <- c("e_pop_num", "e_mort_exc_tbhiv_100k", "e_mort_exc_tbhiv_100k_lo", "e_mort_exc_tbhiv_100k_hi", "e_mort_tbhiv_100k", "e_mort_tbhiv_100k_lo", "e_mort_tbhiv_100k_hi", "e_prev_100k", "e_prev_100k_lo", "e_prev_100k_hi", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_tbhiv_prct", "e_tbhiv_prct_lo", "e_tbhiv_prct_hi")
tai <- subset(tb, g_hbc22=='high' & year==thisyear-1, c('country', tahvars))

names(tai)[1] <- 'group_name'
tah <- rbind(tai, subset(araw, group_type %in% c('global', 'g_whoregion', "g_hbc22") & year==thisyear-1, c('group_name', tahvars)))

names(tah)[1] <- 'rowname'

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

tam[tam$rowname=="India", "rowname"] <- "India(c)"

cat(paste("<font size=5><b>Estimated epidemiological burden of TB, ", thisyear-1, ".</b> Rates per 100 000 population except where indicated.<sup>a</sup></font>", sep=""), file=glue("Tables/2_2_burden_rt", Sys.Date(), ".htm"))

taj <- xtable(tam)
print(taj, type="html", file=glue("Tables/2_2_burden_rt", Sys.Date(), ".htm"), include.rownames=F, include.colnames=F, append=T,
html.table.attributes="border=0 rules=rows width=1100",
add.to.row=list(pos=list(0,30), command=c("<TR width=600> <TH></TH> <TH>POPULATION<br>(THOUSANDS)</TH> <TH colspan=2>MORTALITY<sup>b</sup></TH> <TH colspan=2>HIV-POSITIVE TB MORTALITY</TH> <TH colspan=2>PREVALENCE</TH> <TH colspan=2>INCIDENCE</TH> <TH colspan=2>HIV PREVALENCE IN <br>INCIDENT TB CASES (%)</TH> </TR>", "<TR> <TD colspan=12> <sup>a</sup> Best estimates are followed by the lower and upper bounds of the 95% uncertainty interval. <br><sup>b</sup> Mortality excludes deaths among HIV-positive TB cases. Deaths among HIV-positive TB cases are classified as HIV deaths according to ICD-10 and are shown separately in this table. <br><sup>c</sup> Estimates for India have not yet been officially approved by the Ministry of Health & Family Welfare, Government of India, and should therefore be considered provisional.</TD></TR>")))

tablecopy("2_2_burden_rt")

# for country profiles

taca <- subset(e.t, g_hbc22=='high' & year==thisyear-1, select=c('country', 'year', "e_mort_exc_tbhiv_num", "e_mort_exc_tbhiv_num_lo", "e_mort_exc_tbhiv_num_hi", "e_mort_exc_tbhiv_100k", "e_mort_exc_tbhiv_100k_lo", "e_mort_exc_tbhiv_100k_hi", "e_prev_num", "e_prev_num_lo", "e_prev_num_hi", "e_prev_100k", "e_prev_100k_lo", "e_prev_100k_hi", "e_inc_num", "e_inc_num_lo", "e_inc_num_hi", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_num", "e_inc_tbhiv_num_lo", "e_inc_tbhiv_num_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "c_cdr", "c_cdr_lo", "c_cdr_hi"))

nums <- names(taca)[grep("num", names(taca))]
rates <- names(taca)[grep("100k", names(taca))]

tada <- taca

for(var in nums){
tada[var] <- frmt(taca[[var]] / 1000, thou=TRUE)
}
for(var in rates){
tada[var] <- frmt(tada[[var]], rates=TRUE)
}

write.csv(tada, file=glue("Tables/burden_cp", Sys.Date(), ".csv"), row.names=FALSE)
}# end of estimates tables ----------------


# 4_1_notif -------------------------------------------------------------------

tbb <- subset(n, year==thisyear-1, select=c('country', 'g_whoregion', 'g_hbc22', 'c_notified', 'c_newinc', "new_labconf", "new_clindx", "new_ep", "ret_rel_labconf", "ret_rel_clindx", "ret_rel_ep", "ret_nrel"))

tbb <- tbb[order(tbb$country),]
names(tbb)[names(tbb)=='country'] <- 'area'

# make aggregate rows
tbbh <- aggregate(tbb[4:ncol(tbb)], by=list(area=tbb$g_hbc22), FUN=sum, na.rm=TRUE)
tbbh <- tbbh[tbbh$area=='high',]
tbbh$area <- 'High-burden countries'
tbbr <- aggregate(tbb[4:ncol(tbb)], by=list(area=tbb$g_whoregion), FUN=sum, na.rm=TRUE)
tbbg <- tbb; tbbg$area <- 'Global'
tbbga <- aggregate(tbbg[4:ncol(tbb)], by=list(area=tbbg$area), FUN=sum, na.rm=TRUE)

# combine together
tbc <- rbind(tbb[tbb$g_hbc22=='high', c(1, 4:ncol(tbb))], tbbh, tbbr, tbbga)

# calculate and format vars

tbc$nrpulm_lab_pct <- .rowsums(tbc[c("new_labconf", "ret_rel_labconf")] )* 100 / .rowsums(tbc[c("new_labconf", "new_clindx", "ret_rel_labconf", "ret_rel_clindx")])

for(var in 2:ncol(tbc)){
tbc[var] <- rounder(tbc[[var]])
}

tbc[is.na(tbc$nrpulm_lab_pct), 'nrpulm_lab_pct'] <- "\u2013"

# rename
tbc <- .shortnames(tbc, col='area', ord = "hbc")

# Add to file

tbm <- xtable(tbc[c("area", "c_notified", 'c_newinc', "ret_nrel", "new_labconf", "new_clindx", "new_ep", "ret_rel_labconf", "ret_rel_clindx", "ret_rel_ep", "nrpulm_lab_pct")])

digits(tbm) <- 0

cat(paste("<h3>Case notifications,", thisyear-1, "</h3>"), file=glue("Tables/4_1_notif", Sys.Date(), ".htm"))

print(tbm, type="html", file=glue("Tables/4_1_notif", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, html.table.attributes="border=0 rules=rows width=1100 cellpadding=5", append=T, add.to.row=list(pos=list(0, nrow(tbm)), command=c(
"<TR> 
<TD colspan='4' style='border-right: solid 2px black;'></TD> 
<TH colspan='3' style='border-right: solid 2px black;'>NEW OR PREVIOUS TREATMENT HISTORY UNKNOWN</TH> 
<TH colspan='3' style='border-right: solid 2px black;'>RELAPSE</TH> 
<TD></TD> 
</TR> 
<TR> 
<TD></TD> 
<TD>TOTAL NOTIFIED</TD> 
<TD>NEW AND RELAPSE(a)</TD> 
<TD style='border-right: solid 2px black;'>RETREATMENT EXCLUDING RELAPSE</TD> 
<TD>PULMONARY BACTERIOLOGICALLY CONFIRMED</TD>
<TD>PULMONARY CLINICALLY DIAGNOSED</TD> 
<TD style='border-right: solid 2px black;'>EXTRAPULMONARY</TD> 
<TD>PULMONARY BACTERIOLOGICALLY CONFIRMED</TD>
<TD>PULMONARY CLINICALLY DIAGNOSED</TD> 
<TD style='border-right: solid 2px black;'>EXTRAPULMONARY</TD> 
<TD>PERCENTAGE OF PULMONARY CASES BACTERIOLOGICALLY CONFIRMED</TD> 
</TR> 
<TR>", 
"<TR> <TD colspan=11>Blank cells indicate data not reported.<br>
(a) NEW AND RELAPSE includes cases for which the treatment history is unknown.</TD></TR>")))

tablecopy("4_1_notif")


# 4_2_agesex -------------------------------------------------------------------

# Get country data
tiasb <- subset(n.t, year == thisyear-1, select=c("iso3", "country", "year", "g_whoregion", "g_hbc22", "newrel_m014", "newrel_mu","newrel_f014", "newrel_fu", 'newrel_m15plus', 'newrel_f15plus', 'newrel_sexunk014', 'newrel_sexunk15plus', "newrel_sexunkageunk"))


# combine all vars
tiasb$all.014 <- .rowsums(tiasb[c("newrel_m014", "newrel_f014", 'newrel_sexunk014')]) # (unknowns will be removed for this var later, but for now they are needed for age % and aggregates)


tiasb$all.totalage <- .rowsums(tiasb[c('all.014', 'newrel_m15plus', 'newrel_f15plus', 'newrel_sexunk15plus')]) 

tiasb$all.15plus <- .rowsums(tiasb[c('newrel_m15plus', 'newrel_f15plus', 'newrel_sexunk15plus')])

tiasb$ageunk <- .rowsums(tiasb[c("newrel_mu", "newrel_fu", "newrel_sexunkageunk")])

tiasb$all.male <- .rowsums(tiasb[c("newrel_m014", 'newrel_m15plus', "newrel_mu")])
tiasb$all.female <- .rowsums(tiasb[c("newrel_f014", 'newrel_f15plus', "newrel_fu")])

# Assemble aggregates
tiaa <- glb.rpt.table(df = tiasb, column.nums = 6:ncol(tiasb), country.col = 2)

tiaa1 <- .shortnames(subset(tb, rel_in_agesex_flg==0 & g_hbc22=="high" & year==thisyear-1, country))

#Add an asterisk to the name if country did not include relapse cases in the age/sex table, but only if they reported some data!
tiaa$area <- ifelse(tiaa$area %in% tiaa1$country & !is.na(tiaa$all.014) , paste0(tiaa$area, "*"), tiaa$area)

tikeep <- c("area", "newrel_m014", "newrel_f014", "all.014", "all.15plus", "ageunk", "all.totalage", "all.male", "all.female") #"sn.ep.totalage",

tiaa <- tiaa[tikeep]

# Calculate % child. (Removed for all new where countries didn't report)
tiaa$all.child.pct <- ifelse(is.na(tiaa$all.totalage) , "\u2013", frmt(tiaa$all.014 / tiaa$all.totalage * 100))

# Calculate male female ratio. (Removed for all new where countries didn't report)
tiaa$all.mf.ratio <- ifelse(is.na(tiaa$all.male) | is.na(tiaa$all.female) | tiaa$all.male==0 | tiaa$all.female==0, "\u2013", frmt(tiaa$all.male / tiaa$all.female))

for(var in tikeep[2:7]){
tiaa[var] <- rounder(tiaa[[var]])
}

# tiaa <- tiaa[order(tiaa$order),]
tid <- xtable(subset(tiaa, select=c("area", "all.014", "all.15plus", "ageunk", "all.child.pct", "all.mf.ratio")))

# Add to file

cat(glue("<h2>Notifications of new and relapse TB cases by age and sex, ", thisyear-1, '</h2>'), file=glue("Tables/4_2_agesex", Sys.Date(), ".htm"))

print(tid, type="html", file=glue("Tables/4_2_agesex", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, append=TRUE, add.to.row=list(pos=list(0,nrow(tid)), command=c(
" 
<TR> <TD></TD> 
<TD>0-14 YEARS</TD>
<TD>&#8805;15 YEARS</TD>
<TD>AGE UNKNOWN</TD>
<TD>% AGED &#8804; 15 YEARS</TD>
<TD>MALE/FEMALE RATIO</TD>
</TR>", 
"<TR> <TD colspan=7>Blank cells indicate data that could not be reported for the age categories shown.<br>\u2013 indicates values that cannot be calculated.<br>* New cases only.</TD> </TR>")))

tablecopy("4_2_agesex")


# 4_5_cdr -------------------------------------------------------------------

if(!identical(e.t, e)) warning('These estimates are currently phony!')

# get country rows
tda <- subset(e.t, g_hbc22=='high' & year %in% c(1995, 2000, 2005, 2010, thisyear-1), select=c('country', 'year', 'g_hbc22', 'g_whoregion', 'c_cdr_lo', 'c_cdr', 'c_cdr_hi'))
tda$group_name <- tda$country

# get HBC aggregate row (requires PG's add.rv estimates aggregator)
tda1 <- merge(n.t, e.t)
tdhb <- subset(tda1, g_hbc22=='high' & year %in% c(1995, 2000, 2005, 2010, thisyear-1), select=c('country', 'year', 'g_hbc22', 'g_whoregion', 'e_inc_100k_lo', 'e_inc_100k', 'e_inc_100k_hi', 'e_pop_num', 'c_newinc'))

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
tdr <- subset(a.t, group_type %in% c('g_whoregion', 'global') & year %in% c(1995, 2000, 2005, 2010, thisyear-1), select=c('group_name', 'year', 'c_cdr_lo', 'c_cdr', 'c_cdr_hi'))

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

cat(paste("<font size=5><b>Estimates of the case detection rate for new and relapse cases (%), 1995\u2013", thisyear-1, "</b><sup>a</sup></font>", sep=""), file=glue("Tables/4_5_cdr", Sys.Date(), ".htm"))

tdf <- xtable(tde)
# digits(tdf) <- 1
print(tdf, type="html", file=glue("Tables/4_5_cdr", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, append=T,
html.table.attributes="border=0 rules=rows width=900", add.to.row=list(pos=list(0,30), 
 command=c(paste0("<TR> <TH></TH> <TH colspan=2>1995</TH> <TH colspan=2>2000</TH> <TH colspan=2>2005</TH> <TH colspan=2>2010</TH> <TH colspan=2>", thisyear-1, "</TH> </TR>"), "<TR> <TD colspan=11>\u2013 indicates values that cannot be calculated.<br><sup>a</sup> Estimates for all years are recalculated as new information becomes available and techniques are refined, so they may differ from those published previously. Best estimates are followed by lower and upper bounds. The lower and upper bounds are defined as the 2.5th and 97.5th centiles of outcome distributions produced in simulations.</TD></TR>")))

tablecopy('4_5_cdr')


# 4_6_all_tsr -------------------------------------------------------------------
# All treatment success (smear pos, extrapulm, ret)

tcbb <- subset(o, year>=1995 & year<thisyear-1, select=c('country', 'year', 'g_hbc22', 'g_whoregion', 'new_sp_coh', 'new_sp_cur', 'new_sp_cmplt', 'new_snep_coh', 'new_snep_cmplt', "newrel_coh", "newrel_succ"))

tcbb$new_succ <- .rowsums(tcbb[c('new_sp_cur', 'new_sp_cmplt', 'new_snep_cmplt')])
tcbb$new_coh <- .rowsums(tcbb[c('new_sp_coh', 'new_snep_coh')])

# Combine together
tcba <- glb.rpt.table(tcbb, column.nums = 10:ncol(tcbb), country.col = 1, year.col=2)

# Calculate TSR and coh, after 2011 it includes relapse as well.
tcba$tsr <- ifelse(tcba$year > 2011, tcba$newrel_succ / tcba$newrel_coh, tcba$new_succ / tcba$new_coh) * 100

tcba$coh <- ifelse(tcba$year > 2011, tcba$newrel_coh, tcba$new_coh)

# Subset for 5 year intervals
tcbb <- subset(tcba, year %in% c(seq(1995, thisyear-4, 5), (thisyear-4):(thisyear-2)))

# Pretty the formatting

tcbb$coh <- rounder(tcbb$coh / 1000, decimals=TRUE)

tcbb$tsr <- ifelse(is.na(tcbb$tsr), "\u2013", frmt(tcbb$tsr))

# Put years up top
tcbc1 <- cast(tcbb, area~year, value='tsr')
tcbd <- cast(tcbb, area~year, value='coh')

# Short names and ordering and asterisking
tc2 <- .shortnames(subset(tb, rel_with_new_flg==1 & g_hbc22=="high" & year==thisyear-2, country))

tcbc <- .shortnames(tcbc1, "area", ord="hbc")
tcbc$area <- ifelse(tcbc$area %in% tc2$country, paste0(tcbc$area, "*"), tcbc$area)

tcb_coh <- .shortnames(tcbd, "area", ord="hbc")
tcb_coh$area <- ifelse(tcb_coh$area %in% tc2$country, paste0(tcb_coh$area, "*"), tcb_coh$area)

names(tcbc)[1] <- names(tcb_coh)[1] <- ""

# Add to file

cat(paste("<h3>Treatment success for all new cases (%) and cohort size (thousands), 1995", "\u2013", thisyear-2, "</h3>a. Treatment success (%)", sep=""), file=glue("Tables/4_6_all_tsr", Sys.Date(), ".htm"))

tcb_tsra <- xtable(tcbc)
digits(tcb_tsra) <- 0
print(tcb_tsra, type="html", file=glue("Tables/4_6_all_tsr", Sys.Date(), ".htm"),include.rownames=F, include.colnames=T, append=T, html.table.attributes="border=0 rules=rows width=900")

cat("<br>b. Cohort size (thousands)", file=glue("Tables/4_6_all_tsr", Sys.Date(), ".htm"), append=T)

tcb_coha <- xtable(tcb_coh)

print(tcb_coha, type="html", file=glue("Tables/4_6_all_tsr", Sys.Date(), ".htm"),include.rownames=F, include.colnames=T, append=T, html.table.attributes="border=0 rules=rows width=900", add.to.row=list(pos=list(30), command=c(paste("<TR> <TD colspan=", thisyear-1995, ">Blank cells indicate data not reported. \u2013 indicates values that cannot be calculated.<br>* Data for 2012 include relapse cases.</TD></TR>", sep=""))))

tablecopy("4_6_all_tsr")


# 6_* lab_capac & lab_policy ------------------------------------

# NOTE: Non reporters not included in aggregates

tfa <- subset(s, year==thisyear-1, select=c(country, year, g_whoregion, g_hbc22, g_hbmdr27, c_lab_sm_100k, c_lab_cul_5m, c_lab_dst_5m, lab_sm_f, lab_sm_led, lab_cul_f, lab_dst_f, lab_lpa_f, lab_xpert, dst_in_guide, lc_rst_in_guide, sp_case_dfn_in_guide, lpa_in_guide, dx_alg_tbhiv_in_guide, xpert_in_guide, xpert_in_guide_TBHIV, xpert_in_guide_MDR))

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

for(poli in c("dst_in_guide", "lc_rst_in_guide", "lpa_in_guide", "sp_case_dfn_in_guide", "dx_alg_tbhiv_in_guide", "xpert_in_guide", "xpert_in_guide_TBHIV", "xpert_in_guide_MDR")){
tfb[glue(poli, '_n')] <- ifelse(is.na(tfb[poli]), 0, 1)
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

for(poli in c("dst_in_guide", "lc_rst_in_guide", "lpa_in_guide", "sp_case_dfn_in_guide", "dx_alg_tbhiv_in_guide", "xpert_in_guide", "xpert_in_guide_TBHIV", "xpert_in_guide_MDR")) {
tfd[is.na(tfd$g_hbc22), poli] <- glue(round(tfd[is.na(tfd$g_hbc22), poli] / tfd[is.na(tfd$g_hbc22), glue(poli, '_n')] * 100, 0) , "%")
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

# 6_1_lab_capac ####

tfe <- xtable(tfd[c("country", "g_hbc22", "g_hbmdr27", "lab_sm_f", "c_sm_100k", "lab_sm_led_pct", "lab_cul_f", "c_cul_5m", "lab_dst_f", "c_dst_5m", "lab_lpa_f", "c_lpa_5m", "lab_xpert")], align=c('l', 'l', rep('c',12)))

# Footnote 1
tfefoot <- ifelse(any(is.na(tfe[c(4,7,9,11,13)])), "Blank cells indicate data not reported.<br>", "")

print(tfe, type="html", file=glue("Tables/6_1_lab_capac", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, #sanitize.text.function=identity, #this makes <0.1 disappear in word even if not in a browser. 
html.table.attributes="border=0 rules=rows width=1100 cellpadding=5", add.to.row=list(pos=list(0, nrow(tfe)), command=c(glue("<h2 align=\"left\" >Laboratory capacity, ", thisyear-1, "<sup>a</sup></h2>
 <TR> <TH></TH> <TH></TH> <TH></TH> <TH colspan=3>SMEAR MICROSCOPY</TH> 
 <TH colspan=2>CULTURE</TH>
 <TH colspan=2>DRUG SUSCEPTIBILITY TESTING</TH>
 <TH colspan=2>LINE PROBE ASSAY</TH>
 <TH colspan=1>XPERT MTB/RIF</TH>
 </TR>
 <TR> <TD></TD> <TD>HIGH TB BURDEN</TD>
 <TD>HIGH MDR-TB BURDEN</TD>
 <TD>NUMBER OF LABORATORIES</TD>
 <TD>LABORATORIES PER 100 000 POPULATION</TD>
 <TD>PERCENTAGE OF LABORATORIES USING LED MICROSCOPES</TD>
 <TD>NUMBER OF LABORATORIES</TD>
 <TD>LABORATORIES PER 5 MILLION POPULATION</TD>
 <TD>NUMBER OF LABORATORIES</TD>
 <TD>LABORATORIES PER 5 MILLION POPULATION</TD>
 <TD>NUMBER OF LABORATORIES</TD>
 <TD>LABORATORIES PER 5 MILLION POPULATION</TD>
 <TD>NUMBER OF SITES</TD>
 </TR>"),
paste0("<TR> <TD colspan=11>", tfefoot, "\u2013 indicates values that cannot be calculated.<br><sup>a</sup> The regional and global figures are aggregates of data reported by low- and middle-income countries and territories. Data for the variables shown in the table are not requested from high-income countries in the WHO data collection form. <TD colspan=8></TD> </TR>"))))

tablecopy("6_1_lab_capac")

# B6_1_1_lab_policy ####

tff <- xtable(tfd[c("country", "g_hbc22", "g_hbmdr27", "xpert_in_guide_TBHIV", "xpert_in_guide_MDR")], align=c('l', 'l', rep('c',4)))

# Footnote 1
tffoot <- ifelse(any(is.na(tff[4:ncol(tff)])), "Blank cells indicate data not reported.<br>", "")

print(tff, type="html", file=glue("Tables/B6_2_1_lab_policy", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, #sanitize.text.function=identity, 
html.table.attributes="border=0 rules=rows width=1100 cellpadding=0", add.to.row=list(pos=list(0, nrow(tff)), command=c(glue("<h2 align=\"left\">Incorporation of WHO policy guidance on Xpert MTB/RIF, ", thisyear-1, "<sup>a</sup></h2>
<TR> <TH></TH> <TH>HIGH TB BURDEN</TH> <TH>HIGH MDR-TB BURDEN</TH>
<TH>XPERT MTB/RIF AS THE INITIAL DIAGNOSTIC TEST FOR TB IN PEOPLE LIVING WITH HIV</TH>
<TH>XPERT MTB/RIF AS THE INITIAL DIAGNOSTIC TEST IN PEOPLE AT RISK OF DRUG-RESISTANT TB</TH> </TR>"),
paste0("<TR> <TD colspan=8>", tffoot, "<sup>a</sup> The regional and global figures are aggregates of data reported by low- and middle-income countries and territories. Data for the variables shown in the table are not requested from high-income countries in the WHO data collection form. 
<TD colspan=8></TD> </TR>"))))

tablecopy("B6_1_1_lab_policy")


# 7_1_tbhiv -------------------------------------------------------------------

tea <- merge(n[c('country', 'year', "g_whoregion", 'g_hbhiv41', "hiv_ipt")], tbhiv[c('country', 'year', "hivtest", "hivtest_pct_denominator", "hivtest_pos_pct_denominator", "hivtest_pos_pct_numerator", "hiv_cpt_pct_numerator", "hiv_cpt_pct_denominator", "hiv_art_pct_numerator", "hiv_art_pct_denominator", 'hivtest_pct_numerator', 'hivtest_pos', 'hiv_cpt', 'hiv_art')])

tea <- merge(tea, e.t[c('country', 'year', 'e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')], all.x=T)

tea <- subset(tea, year==thisyear-1)

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
teda <- te1
teda$type <- 'country'

tedb <- rbind(te2, teb, tec)
tedb$type <- 'agg'

# Combine all together
ted <- rbind(teda, tedb)

# replace aggregate estimates with properly aggregated numbers
ted1 <- e.t[e.t$year==thisyear-1 & !is.na(e.t$e_inc_tbhiv_100k) & e.t$g_hbhiv41=='high' , c('iso3', 'e_inc_tbhiv_100k', 'e_inc_tbhiv_100k_lo', 'e_inc_tbhiv_100k_hi', 'e_pop_num')] 
ted1[2:4] <- ted1[2:4] / 100000
ted1$e_pop_num <- as.numeric(ted1$e_pop_num)
ted2 <- add.rv(ted1$e_inc_tbhiv_100k, ted1$e_inc_tbhiv_100k_lo, ted1$e_inc_tbhiv_100k_hi, weights=ted1$e_pop_num)

ted[ted$country=="High TB/HIV burden countries", c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')] <- signif(ted2[c("r.num", "r.lo.num", "r.hi.num")], 2)

ted3 <- a.t[a.t$year==thisyear-1 & a.t$group_type %in% c('g_whoregion', 'global'), c('group_name', 'group_type', 'e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')]
ted3 <- ted3[order(ted3$group_type, ted3$group_name),]

ted[43:nrow(ted), c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')] <- ted3[c('e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi')]

# Format table columns WHY AM I DOING IT DIFFERENT FOR COUNTRY?!! Need to come back to this and the ART map. And the HIV test map.
ted <- within(ted, {
hivtest1000 <- rounder(ifelse(type=='country', hivtest, hivtest_pct_numerator) / 1000, decimals=TRUE)
hivtest_prct <- rounder(ifelse(type=='country', hivtest, hivtest_pct_numerator) / hivtest_pct_denominator * 100, decimals=TRUE)
hivtest_pos_prct <- rounder(ifelse(type=='country', hivtest_pos / hivtest,hivtest_pos_pct_numerator / hivtest_pos_pct_denominator) * 100, decimals=TRUE)
hiv_cpt_prct <- rounder(ifelse(type=='country', hiv_cpt / hivtest_pos, hiv_cpt_pct_numerator / hiv_cpt_pct_denominator) * 100, decimals=TRUE)
hiv_art_prct <- rounder(ifelse(type=='country', hiv_art / hivtest_pos, hiv_art_pct_numerator / hiv_art_pct_denominator) * 100, decimals=TRUE)

# new in 2014, calculate ART coverage of ESTIMATED TB/HIV patients
hiv_art_est_prct <- rounder(ifelse(type=='country', hiv_art / e_inc_tbhiv_num, hiv_art_pct_numerator / e_inc_tbhiv_num) * 100, decimals=TRUE)

hiv_ipt2 <- rounder(hiv_ipt / 1000, decimals=TRUE)
})

ted <- .shortnames(ted, ord='tbhiv')


ted$e_inc_tbhiv_num <- frmt(ted$e_inc_tbhiv_num / 1000, thou=TRUE)
ted$e_inc_tbhiv_num_lo <- frmt(ted$e_inc_tbhiv_num_lo / 1000, thou=TRUE)
ted$e_inc_tbhiv_num_hi <- frmt(ted$e_inc_tbhiv_num_hi / 1000, thou=TRUE)



# Fix and footnote for Russian Federation
ted[ted$country=='Russian Federation', 'hivtest1000'] <- glue(ted[ted$country=='Russian Federation', 'hivtest1000'], '(a)')
ted[ted$country=='Russian Federation', c('hivtest_prct', 'hivtest_pos_prct')] <- NA
warning("Russian Federation modification for the TB/HIV table is still in place. Delete this message when no longer applicable.")

ted2 <- ted[c("country", 'e_inc_tbhiv_num', 'e_inc_tbhiv_num_lo', 'e_inc_tbhiv_num_hi', "hivtest1000", "hivtest_prct", "hivtest_pos_prct", "hiv_cpt_prct", "hiv_art_prct", "hiv_art_est_prct", "hiv_ipt2")]

# replace uncalculatables
for(var in 6:10){
ted2[var] <- ifelse(is.na(ted2[[var]]), "\u2013", ted2[[var]])
}

# combine estimates to two lines
ted2$est_rg <- paste0("(", ted2$e_inc_tbhiv_num_lo, "\u2013", ted2$e_inc_tbhiv_num_hi, ")")

ted3 <- ted2[c("country", 'e_inc_tbhiv_num', 'est_rg', "hivtest1000", "hivtest_prct", "hivtest_pos_prct", "hiv_cpt_prct", "hiv_art_prct", "hiv_art_est_prct", "hiv_ipt2")]

tee <- xtable(ted3, align=c('l', 'l', 'r', 'l', rep('c',7)))

cat(glue("<font size=3 align=\"left\"><b>HIV testing, treatment for HIV-positive TB patients and prevention of TB among people living with <b>HIV</b>, 41 high TB/HIV burden countries and WHO regions, ", thisyear-1, ".</b> Numbers in thousands except where indicated.</font><br><br>"), file=glue("Tables/7_1_tbhiv", Sys.Date(), ".htm"))

print(tee, type="html", file=glue("Tables/7_1_tbhiv", Sys.Date(), ".htm"),include.rownames=F, include.colnames=F, append=T,
html.table.attributes="border=0 rules=rows width=1100", add.to.row=list(pos=list(0, nrow(tee)), command=c("
<TR> <TH></TH> <TH colspan=2>ESTIMATED HIV-POSTIVE INCIDENT TB CASES</TH> 
<TH>NUMBER OF NOTIFIED TB PATIENTS WITH KNOWN HIV STATUS</TH> 
<TH>% OF NOTIFIED TB PATIENTS WITH KNOWN HIV STATUS</TH> 
<TH>% OF NOTIFIED TB PATIENTS WITH HIV-POSITIVE TEST</TH> 
<TH>% OF NOTIFIED HIV-POSITIVE TB PATIENTS STARTED ON CPT</TH> 
<TH>% OF NOTIFIED HIV-POSITIVE TB PATIENTS STARTED ON ART</TH> 
<TH>% OF ESTIMATED HIV-POSITIVE INCIDENT TB CASES STARTED ON ART<sup>b</sup></TH> 
<TH>NUMBER OF HIV-POSITIVE PEOPLE PROVIDED WITH IPT</TH></TR>",
"<TR> <TD colspan=11>Blank cells indicate data not reported. 
<br>\u2013 indicates values that cannot be calculated.<br>
<sup>a</sup> Data for the Russian Federation exclude retreatment cases and cases from prisons.<br><sup>b</sup> TB cases started on ART include non-relapse retreatment cases.<TD colspan=8></TD> </TR>")))

tablecopy("7_1_tbhiv")


# END ===================================================