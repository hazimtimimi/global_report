# -------------------------------------------------
# Figures in the global report
# Tom Hiatt
# 10 July 2012, revised 25 June 2015
# -------------------------------------------------

# Probably I need to change this to a R markdown document so the whole thing produces a PDF with all the tables and figures one after the other and individual PDFs, PNGs, etc. in a folder. UPDATE: Rmarkdown can't make a decent table in word or HTML to print. Someday I will learn LaTex or something and make it work.

# HT: run the Setup.r manually first  ...
# source('D:/TMEData/TomsCode/Global TB control Reports/Tables and figures/Setup.r')


# Link -------------------------------------------------
# Link GTBR2014 figure numbers to code sections
# 
# 4.5 Treatment outcomes for new and relapse cases: 4_5_txsucc
# (box 4.5): Outcomes of TB treatment by HIV status: B4_5_hiv_ts_d
# 
# 
# 7.1 Percentage of TB patients with known HIV status: 7_1_hivtest_graph
#
# 7.4 Number of HIV-positive TB patients enrolled on CPT and ART: 7_4_cpt_art_hiv_graph
# 7.3 % TB patients HIV+, and % HIV+ TB patients on CPT and ART: 7_3_hivprog_graph_all
#
# 7.7 IPT for PLHIV without active TB : 7_7_hiv_ipt_graph
# -------------------------------------------------

# ******************************************************
# Old figures ####

# Find this code in past repository commits

# Dropped in 2014
# prev_glo
# 7_xxxx_hivtest_num
# cpt_graph
# art_graph
# 7_xxxx_tbscr_graph
# hivdist_graph
# hiv_txout _rep and _tsr
# hiv_tsr_graph
# txout_reg
# inc_glo

# ******************************************************


#flag for whether or not to produce estimates figures
flg_show_estimates <- TRUE


if(flg_show_estimates){
  
  # Global rates of incidence, prevalence and mortality - est_global
  # 2_6_fig_global ----------------------------------------------------
  
  eza <- subset(araw.t, group_name=='global')
  eza$forecast <- ifelse(eza$year >= thisyear, "forecast", "current")
  
  p1 <- qplot(year, e_inc_100k, data=eza, geom='line', colour=I('#00FF33')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('#00FF33'), alpha=0.4) +
    #   geom_line(aes(year, newrel_100k)) + 
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I(inch.color)) +      
    geom_ribbon(aes(year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi), fill=I(inch.color), alpha=0.4) +
    #   facet_wrap(~g_whoregion, scales='free_y') +
    scale_x_continuous('', limits=c(1990, yr)) + ylab('Rate per 100 000 population per year') +
    expand_limits(y=c(0, max(pretty(c(eza$e_inc_100k_hi, max(eza$e_inc_100k_hi) * (1.20)))))) + theme_glb.rpt() + theme(legend.position='none', plot.title = element_text(hjust = 0)) + ggtitle('Incidence')
  
  p2 <- qplot(year, e_prev_100k, data=eza, geom='line', colour=I(prev.color), linetype=forecast) +
    geom_ribbon(aes(year, ymin=e_prev_100k_lo, ymax=e_prev_100k_hi), fill=I(prev.color), alpha=0.4) +
    geom_hline(aes(yintercept=eza[eza$year==1990, "e_prev_100k"] / 2), linetype=2)  + ylab('Rate per 100 000 population') + xlab('') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position='none') + ggtitle('Prevalence')  
  
  p3 <- qplot(year, e_mort_exc_tbhiv_100k, data=eza, geom='line', colour=I(mort.color), linetype=forecast) +
    geom_ribbon(aes(year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi), fill=I(mort.color), alpha=0.4) +
    geom_hline(aes(yintercept=eza[eza$year==1990, "e_mort_exc_tbhiv_100k"] / 2), linetype=2)  + ylab('Rate per 100 000 population per year') + xlab('') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position='none') + ggtitle('Mortality')
  
  pdf(width=8, height=4, file='Figs/2_6_fig_global.pdf', 
      title='Global trends in case notification rates and estimated rates of incidence, morality and prevalence.') 
  
  grid.arrange(p1, p2, p3, nrow=1)
  dev.off()
  
figsave(p1, eza, "2_6_fig_global") # This isn't perfect since it only saves the first plot. The "2_6_fig_global" file without the date is complete.
  
  # Global rates of incidence, and notifications
  # 4_2_inc_notif_glo ----------------------------------------------------
  
  eha <- subset(araw.t, group_type=="global")
  
  ehb <- merge(subset(eha, select=c("group_name", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), aggregate(n.t['c_newinc'], by=list(year=n.t$year), FUN=sum, na.rm=TRUE))
  
  ehb$newrel_100k <- ehb$c_newinc / ehb$e_pop_num * 100000
  
  inc_notif_glo <- qplot(year, e_inc_100k, data=ehb, geom='line', colour=I('#00FF33')) + geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('#00FF33'), alpha=0.4) + geom_line(aes(year, newrel_100k)) + scale_x_continuous('') + ylab('Rate per 100 000 population per year')  + theme_glb.rpt() + ggtitle(paste0('Global trends in case notification (black) and estimated TB \nincidence (green) rates, 1990-', thisyear-1, ". \nCase notifications include new and relapse cases (all forms).")) 
  
  figsave(inc_notif_glo, ehb, "4_2_inc_notif_glo", width=6, height=6)
  
  # Regional rates of incidence, and notifications
  # 4_3_inc_notif_reg ----------------------------------------------------
  
  efa1 <- subset(araw.t, group_type=="g_whoregion" & year < thisyear)
  
  # names(regional) <- gsub ('_', '\\.', names (regional))
  efa1$g_whoregion <- factor(efa1$group_name, labels=c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific"))
  
  efa <- merge(subset(efa1, select=c("group_name", "g_whoregion", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), aggregate(n.t['c_newinc'], by=list(group_name=n.t$g_whoregion, year=n.t$year), FUN=sum, na.rm=TRUE))
  
  efa$newrel_100k <- efa$c_newinc / efa$e_pop_num * 100000
  
  # a fudging to get values on top [drop this in the future I think.]
  topper <- function(dat){
    dat$top <- max(pretty(c(dat$e_inc_100k_hi, max(dat$e_inc_100k_hi) * (1.1))))
    return(dat)
  }
  efc <- ddply(efa, "g_whoregion", topper)
  
inc_notif_reg <- qplot(year, e_inc_100k, data=efa, geom='line', colour=I('#00FF33')) + geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('#00FF33'), alpha=0.4) + geom_line(aes(year, newrel_100k)) + facet_wrap(~g_whoregion, scales='free_y') + scale_x_continuous('') + ylab('Rate per 100 000 population per year') +expand_limits(y=0) + theme_glb.rpt() +  ggtitle(paste0('Figure 4.3 Case notification and estimated TB incidence rates by WHO region, 1990-', thisyear-1, '. \nRegional trends in case notification rates (new and relapse cases, all forms) (black) and estimated TB incidence rates (green). \nShaded areas represent uncertainty bands.')) 
  
  figsave(inc_notif_reg, efa, "4_3_inc_notif_reg")
  
  # Incidence only
  # 2_7_inc_reg ------------------------------------------------------
  
inc_reg <- qplot(year, e_inc_100k, data=efc, geom='line', colour=I(inc.color)) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I(inc.color), alpha=0.4) +
    #   geom_line(aes(year, newrel_100k)) + 
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I(inch.color)) +
    geom_ribbon(aes(year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi), 
                fill=I(inch.color), alpha=0.4) +
    facet_wrap(~g_whoregion, scales='free_y') +
    scale_x_continuous('') + ylab('Rate per 100 000 population per year') +
    expand_limits(y=0) + geom_point(aes(year, top), alpha=0) +
    theme_glb.rpt() + 
    ggtitle(paste('Estimated TB incidence rates by WHO region, 1990-', thisyear-1, '.', sep="")) 
  
  figsave(inc_reg, efa, "2_7_inc_reg")
  
  # HBC rates of incidence, and notifications
  # 4_4_inc_notif_hbc ----------------------------------------------------
  
  ega <- subset(merge(eraw.t, e.t[e.t$year==thisyear-1,c("country", "g_hbc22")]), g_hbc22=="high" & year < thisyear) # This hack is until we add g_hbc22 to eraw.
  
  egb <- .shortnames(merge(subset(ega, select=c("country", "g_whoregion", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), subset(n.t, select=c('country', 'year', 'c_newinc'))), ord='multiyear')

  egb$newrel_100k <- egb$c_newinc / egb$e_pop_num * 100000
  
inc_notif_hbc <- qplot(year, e_inc_100k, data=egb, geom='line', colour=I(inc.color)) + geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I(inc.color), alpha=0.4) + geom_line(aes(year, newrel_100k)) + facet_wrap(~country, scales='free_y') + scale_x_continuous('') + ylab('Rate per 100 000 population per year') + expand_limits(y=0) + theme_glb.rpt() + ggtitle(paste0('Case notification and estimated TB incidence rates, 22 high-burden countries, 1990-', thisyear-1, '. \nTrends in case notification rates (new and relapse cases, all forms) (black) and estimated TB incidence rates (green). \nShaded areas represent uncertainty bands.')) 
  
  figsave(inc_notif_hbc, egb, "4_4_inc_notif_hbc")
  
  # Incidence only
  # 2_8_inc_hbc -----------------------------------------------------
  
inc_hbc <- qplot(year, e_inc_100k, data=egb, geom='line', colour=I(inc.color)) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I(inc.color), alpha=0.4) +
    #   geom_line(aes(year, newrel_100k)) + 
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I(inch.color)) +
    geom_ribbon(aes(year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi), 
                fill=I(inch.color), alpha=0.4) +
    facet_wrap(~country, scales='free_y') +
    scale_x_continuous('') + ylab('Rate per 100 000 population per year') +
    expand_limits(y=0) + # geom_point(aes(year, top), alpha=0) +
    theme_glb.rpt() + 
    ggtitle(paste('Estimated TB incidence rates, 22 high-burden countries, 1990-', thisyear-1, '.', sep="")) 

inc_hbc <- facetAdjust(inc_hbc)
  
  figsave(inc_hbc, egb, "2_8_inc_hbc")
  
  
  # Regional rates of TB prevalence and mortality
  # 2_10_prev_reg ----------------------------------------------------
  
  regional <- subset(araw.t, group_type=="g_whoregion")
  regional$forecast <- ifelse(regional$year >= thisyear, "forecast", "current")
  
  regional$g_whoregion <- factor(regional$group_name, labels=c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific"))

regional2 <- ddply(regional, .(g_whoregion), transform, target.prev=e_prev_100k[1]/2, target.mort=e_mort_exc_tbhiv_100k[1]/2)


prev_reg <- ggplot(regional2, aes(year, e_prev_100k, linetype=forecast)) + geom_line(colour=I(prev.color)) +
    geom_ribbon(aes(year, ymin=e_prev_100k_lo, ymax=e_prev_100k_hi), fill=I(prev.color), alpha=0.4) +
    facet_wrap(~g_whoregion, scales='free_y') + 
      geom_hline(aes(yintercept=target.prev), linetype=2)  +
    scale_x_continuous('') + ylab('Rate per 100 000 population') +
    expand_limits(y=0) + 
    theme_glb.rpt() + theme(legend.position="none") +
    ggtitle(paste('Estimated prevalence rates by WHO region, 1990-', thisyear-1, '.', sep="")) 

  
  figsave(prev_reg, regional, "2_10_prev_reg")
  
  # 2_13_mort_reg ----------------------------------------------------
  
mort_reg <- ggplot(regional2, aes(year, e_mort_exc_tbhiv_100k, linetype=forecast)) + geom_line(colour=I(mort.color)) +
    geom_ribbon(aes(year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi), fill=I(mort.color), alpha=0.4) + 
      geom_hline(aes(yintercept=target.mort), linetype=2) +
    facet_wrap(~g_whoregion, scales='free_y') +
    scale_x_continuous('') + ylab('Rate per 100 000 population') +
    expand_limits(y=0) + 
    theme_glb.rpt() + theme(legend.position="none") +
    ggtitle(paste('Estimated mortality rates by WHO region, 1990-', thisyear-1, '.', sep="")) 
  
  figsave(mort_reg, regional, "2_13_mort_reg")

# 2_14_mort_hbc ---------------------------------------------------------
# From Philippe Glaziou 

moa <- subset(eraw, iso3 %in% e[e$g_hbc22=="high", "iso3"], c(country, iso3, year, e_prev_100k, e_prev_100k_lo, e_prev_100k_hi, e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi, vr.tbrate.raw))

moa$forecast <- ifelse(moa$year >= thisyear, "forecast", "current")

hbc.ff2 <- ddply(as.data.frame(moa), .(iso3), transform, target.prev=e_prev_100k[1]/2, target.mort=e_mort_exc_tbhiv_100k[1]/2)

hbc.ff3 <- .shortnames(hbc.ff2, col="country")

Bangladesh.note <- ""
if(thisyear==2014){
  Bangladesh.note <- "Estimates of TB disease burden have not been approved by the national TB programme in Bangladesh and a joint reassessment 
  will be undertaken following the completion of the prevalence survey planned for 2015."
  hbc.ff3[hbc.ff3$iso3=="BGD", "country"] <- "Bangladesh(b)"
  warning("Bangladesh footnote applied.")
}

mort_hbc1 <- qplot(year, e_mort_exc_tbhiv_100k, data=hbc.ff3, geom='line', colour=I(mort.color), linetype=forecast) +
  geom_ribbon(aes(year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi), fill=I(mort.color), alpha=0.4) +
  geom_hline(aes(yintercept=target.mort), linetype=2) +
  facet_wrap(~country, scales='free_y') +
  geom_point(aes(year, vr.tbrate.raw), shape=I(4)) +
  xlab('') + ylab('Rate per 100 000 population per year') +
  expand_limits(y=0) +
  theme_glb.rpt() +
  theme(legend.position='none') +
  ggtitle(paste0("Figure 2.14 Trends in estimated TB mortality rates 1990–2013 and forecast TB mortality rates 2014–2015, 22 high-
burden countries. Estimated TB mortality excludes TB deaths among HIV-positive people. The horizontal dashed lines 
represent the Stop TB Partnership target of a 50% reduction in the mortality rate by 2015 compared with 1990. The 
other dashed lines show projections up to 2015. (a)  Uncertainty is due to adjustments made to the mortality data from vital 
registration systems that were reported by countries (mortality data from vital registration systems are represented by the “x” 
symbol). Further explanation of methods is provided in Annex 1."))

mort_hbc <- arrangeGrob(mort_hbc1, sub = textGrob(paste("(a) The width of an uncertainty band narrows as the quality and completeness of vital registration data improves.", "\n(b) ", Bangladesh.note), x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))

# mort_hbc <- facetAdjust(mort_hbc1)

# It looks like you can have one tweak or another, but not both. (X-axis on orphaned panels or footnote.)

figsave(mort_hbc, hbc.ff3, "2_14_mort_hbc")

 
# *********************************************************************
  
  # country and reg profiles -------------------------------------------------
  # Country and regional Profiles - Incidence, prevalence and mortality in 22 HBCs

  cpfd1 <- merge(n, eraw,)
  rpfd <- aggregate(cpfd1['c_newinc'], by=list(group_name=cpfd1$g_whoregion, year=cpfd1$year), FUN=sum, na.rm=TRUE)
  rpfd <- merge(rpfd, araw)
  cpfd1$group_name <- cpfd1$country
  cpfd <- merge(cpfd1, rpfd, all=T)
  
  # Incidence 
  
  hest <- subset(cpfd, g_hbc22=='high' | group_name %in% rpfd$group_name)
  levels(hest$group_name)[match('Democratic Republic of the Congo', levels(hest$group_name))] <- 'DR Congo'
  levels(hest$group_name)[match('United Republic of Tanzania', levels(hest$group_name))] <- 'UR Tanzania'
  hest$c_newinc_100k <- hest$c_newinc / hest$e_pop_num * 1e5
  
  pdf(width=14, height=9.5, file='CPFigs/hbc_cp_inc.pdf')
  # windows(14,9.5) windows(3, 0.87)
  qplot(year, e_inc_100k, data=hest, geom='line', colour=I(inc.color)) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I(inc.color), alpha=0.4) +
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I(inch.color)) +
    geom_line(aes(year, c_newinc_100k)) + 
    geom_ribbon(aes(year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi), 
                fill=I(inch.color), alpha=0.4) +
    facet_wrap(~group_name, scales='free', ncol=4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(name="", expand = c(0, 0)) + 
    expand_limits(y=0) +
    theme_glb.rpt(base_size=6) + theme(legend.position='none', panel.grid.minor = element_blank()) +  ggtitle('Incidence')
  dev.off()
  
  # Prevalence
  
  pdf(width=14, height=9.5, file='CPFigs/hbc_cp_prev.pdf')
  p1 <- qplot(year, e_prev_100k, data=hest, geom='line', colour=I(prev.color)) +
    geom_ribbon(aes(year, ymin=e_prev_100k_lo, ymax=e_prev_100k_hi), fill=I(prev.color), alpha=0.4) +
    # geom_hline(aes(yintercept=target.prev), linetype=2) +
    facet_wrap(~group_name, scales='free', ncol=4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(name="", expand = c(0, 0)) + 
    expand_limits(y=0) +
    theme_glb.rpt(base_size=6) +
    theme(legend.position='none', panel.grid.minor = element_blank()) + ggtitle('Prevalence')
  print(p1) 
  dev.off()
  
  # Mortality
  
  pdf(width=14, height=9.5, file='CPFigs/hbc_cp_mort.pdf')
  p2 <- qplot(year, e_mort_exc_tbhiv_100k, data=hest, geom='line', colour=I(mort.color)) +
    geom_ribbon(aes(year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi), fill=I(mort.color), alpha=0.4) +
    # geom_hline(aes(yintercept=target.mort), linetype=2) +
    facet_wrap(~group_name, scales='free', ncol=4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(name="", expand = c(0, 0)) + 
    expand_limits(y=0) +
    theme_glb.rpt(base_size=6) +
    theme(legend.position='none', panel.grid.minor = element_blank()) + ggtitle('Mortality')
  print(p2)
  dev.off()
} # End of flg_show_estimates --------------------------------------


# 2_3_topten ---------------------------------------------------------
# The 'top 10 countries' figure (previously a table), in 2014 only 1 and 2 are used. Only countries with over a thousand total cases are considered.

tma <- subset(e.t,e_inc_num > 1e3 & year==thisyear-1)

tma$e_mort_num <- tma$e_mort_100k / 1e5 * tma$e_pop_num
tma$e_mort_num_lo <- tma$e_mort_100k_lo / 1e5 * tma$e_pop_num
tma$e_mort_num_hi <- tma$e_mort_100k_hi / 1e5 * tma$e_pop_num

.topten <- function(df, vect, num.rows){
  df1 <- df[order(df[vect], decreasing=TRUE),]
  df1 <- .shortnames(df1[1:num.rows, c('country', vect, glue(vect, '_lo'), glue(vect, '_hi'))])
  df1$var <- vect
  names(df1) <- c("country", 'best', 'lo', 'hi', 'var')
  (df1)
}

tm1 <- .topten(tma, 'e_inc_num', 10)
tm2 <- .topten(tma, 'e_inc_100k', 10)
tm3 <- .topten(tma, 'e_inc_tbhiv_num', 10)
tm4 <- .topten(tma, 'e_mort_exc_tbhiv_num', 10)
tm5 <- .topten(tma, 'e_mort_exc_tbhiv_100k', 10)
tm6 <- .topten(tma, 'e_mort_num', 10)
tm7 <- .topten(tma, 'e_mort_100k', 10)

tm.nhiv <- rbind(tm1, tm2, tm3, tm4, tm5, tm6, tm7)
tm.nhiv$var <- factor(tm.nhiv$var, levels=c("e_inc_num", "e_inc_100k",  "e_inc_tbhiv_num", "e_mort_exc_tbhiv_num", "e_mort_exc_tbhiv_100k", 'e_mort_num', 'e_mort_100k'), labels=c("Incidence: absolute numbers", "Incidence: rate per 100 000 population",  "TB/HIV incidence", "Mortality (excluding TB/HIV)", "Mortality per 100 000 (excluding TB/HIV)", "Mortality (including TB/HIV)", "Mortality per 100 000 (including TB/HIV)"))

for(pn in c(1,4,6)){
  vr <- levels(tm.nhiv$var)[pn]
  tn <- subset(tm.nhiv, var==vr)
  tn$country <- factor(tn$country, levels=rev(tn$country))
  tn1 <- ggplot(tn, aes(best/1e6, country, xmin=lo/1e6, xmax=hi/1e6)) + geom_point()  + geom_errorbarh(height=.25) +  theme_glb.rpt() + labs(y="", x='Millions', title=vr) + theme(plot.title = element_text(hjust = 0)) 
  
  figsave(tn1, tn, glue('2_3_topten_', pn, '_'), width=5, height=4) 
}

for(pn in c(2,5,7)){
  vr <- levels(tm.nhiv$var)[pn]
  tn <- subset(tm.nhiv, var==vr)
  tn$country <- factor(tn$country, levels=rev(tn$country))
  tn1 <- ggplot(tn, aes(best, country, xmin=lo, xmax=hi)) + geom_point()  + geom_errorbarh(height=.25) +  theme_glb.rpt() + labs(y="", x='Rate per 100 000 population per year', title=vr) + theme(plot.title = element_text(hjust = 0)) 
  
  figsave(tn1, tn, glue('2_3_topten_', pn, '_'), width=5, height=4) 
}


# 4_1_agesex_reg -------------------------------------------------------------

fa <- subset(tb, year==thisyear-1, c("country", "g_whoregion", "newrel_m014", "newrel_m1524", "newrel_m2534", "newrel_m3544", "newrel_m4554", "newrel_m5564", "newrel_m65", "newrel_f014", "newrel_f1524", "newrel_f2534", "newrel_f3544", "newrel_f4554", "newrel_f5564", "newrel_f65"))

fb <- subset(p, year==thisyear-1, c("country", "e_pop_m014", "e_pop_m1524", "e_pop_m2534", "e_pop_m3544", "e_pop_m4554", "e_pop_m5564", "e_pop_m65", "e_pop_f014", "e_pop_f1524", "e_pop_f2534", "e_pop_f3544", "e_pop_f4554", "e_pop_f5564", "e_pop_f65"))

fc <- merge(fa, fb)

# Drop pop numbers from non reporters
fc$check <- .rowsums(fc[c("newrel_m1524", "newrel_m2534", "newrel_m3544", "newrel_m4554", "newrel_m5564", "newrel_m65", "newrel_f1524", "newrel_f2534", "newrel_f3544", "newrel_f4554", "newrel_f5564", "newrel_f65")] )

fc1 <- fc[!is.na(fc$check), -ncol(fc)]

fd <- aggregate(fc1[3:ncol(fc1)], by=list(Region=fc1$g_whoregion), FUN=sum, na.rm=TRUE)

fe <- melt(fd, id=1)

fe$type <- str_extract(fe$variable, "newrel|e_pop")
fe$sex <- str_extract(fe$variable, "m|f")
fe$age <- str_extract(fe$variable, "014|1524|2534|3544|4554|5564|65")

fg <- cast(fe, Region+age~type, fun.aggregate = sum) # This aggregates male and female

fg$newrel_100k <- fg$newrel / fg$e_pop * 1e5

fg <- .shortnames(fg, col="Region")
fg$age <- factor(fg$age, levels=c("014", "1524", "2534", "3544", "4554", "5564", "65"), labels=c("0\u201314", "15\u201324", "25\u201334", "35\u201344", "45\u201354", "55\u201364", "\u226565"))

agesex_reg1 <- ggplot(fg, aes(age, newrel_100k, colour=Region, group=Region)) + geom_line(size=1) + scale_y_continuous(name = "Rate per 100 000 population per year") + scale_x_discrete("", labels=levels(fg$age)) + scale_color_brewer(name="", palette="Dark2") + ggtitle(paste0("Regional TB notification rates by age, ", thisyear-1, "(a)")) + theme_glb.rpt() 

# Add footnote
fgf1 <- rounder(sum(fg$newrel) / sum(tb[tb$year==yr, "c_newinc"], na.rm=TRUE) * 100)
fgf2 <- tb[tb$year==yr & tb$g_hbc22=="high", "country"]
fgf3 <- fgf2[!fgf2 %in% fc1$country]
fgf4 <- .shortnames(data.frame(country=fgf3), col="country")

fgfoot <- paste0("(a) Countries not reporting cases in these categories are excluded. \nCases included make up ", fgf1, "% of reported cases and exclude the following high-burden countries: \n", "Afghanistan, Ethiopia, India, Mozambique, Myanmar, Pakistan, Thailand", " and ", fgf4$country[nrow(fgf4)], ".")

agesex_reg <- arrangeGrob(agesex_reg1, sub = textGrob(fgfoot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))

# windows(11, 7); gad; dev.off()
figsave(agesex_reg, fg, "4_1_agesex_reg")


# 4_5_txsucc -------------------------------------------------------------------

ha1 <- subset(tb, year==thisyear-2, select=c('country', 'g_whoregion', 'g_hbc22', "rel_with_new_flg", "newrel_coh", "newrel_succ", "newrel_fail", "newrel_died", "newrel_lost", "c_newrel_neval"))

# Aggregate and reassemple

haa <- glb.rpt.table(ha1, 5:ncol(ha1))

ha2 <- .shortnames(subset(ha1, rel_with_new_flg==0 & g_hbc22=="high", country))
haa$area <- ifelse(haa$area %in% ha2$country, paste0(haa$area, "*"), haa$area)

haa$area <- factor(haa$area, levels=rev(haa$area))

haa$`Treatment success` <- haa$newrel_succ / haa$newrel_coh * 100
haa$Failure <- haa$newrel_fail / haa$newrel_coh * 100
haa$Died <- haa$newrel_died / haa$newrel_coh * 100
haa$`Lost to follow-up` <- haa$newrel_lost / haa$newrel_coh * 100
haa$`Not evaluated` <- haa$c_newrel_neval / haa$newrel_coh * 100

# Plot

hab <- melt(haa[c(1, 8:12)], id=1)

txsucc <- ggplot(hab, aes(area, value, fill=variable)) + geom_bar(stat="identity", position="stack") + geom_hline(yintercept=85, color="grey70") + geom_text(data=subset(hab, variable=="Treatment success"), aes(label=round(value,0)), hjust=1.25, vjust=0.3, size=4, color="white") + theme_glb.rpt() + coord_flip() +   scale_fill_brewer("", type = "qual", palette = 8) + labs(x="", y="Percentage of cohort") + theme(legend.position="bottom", panel.grid=element_blank()) + expand_limits(c(0,0)) + ggtitle(paste0("Treatment outcomes for new and relapse cases, ", thisyear-2, ", globally, \nfor the six WHO regions and 22 high-burden countries")) 

figsave(txsucc, hab, "4_5_txsucc")


# B4_6_hiv_ts_d ---------------------------------------------------

# Remove non-HIV outcomes reporters (because otherwise we can't minus out the HIV)
hma2 <- subset(tb, year==thisyear-2 & !is.na(tbhiv_succ) & !is.na(newrel_succ), c(country, year, newrel_coh, newrel_succ, newrel_fail, newrel_died, newrel_lost, c_newrel_neval, ret_nrel_coh, ret_nrel_succ, ret_nrel_fail, ret_nrel_died, ret_nrel_lost, c_ret_nrel_neval, tbhiv_coh, tbhiv_succ, tbhiv_fail, tbhiv_died, tbhiv_lost, c_tbhiv_neval)) 

if(thisyear==2014){
  hma2 <- subset(hma2, country %nin% c('COD', "MOZ"))
  warning("DRC and Mozambique numbers removed!!!")
}

hma <- hma2 

hma[1] <- "global"

hmb <- aggregate(hma[3:ncol(hma)], by=list(area=hma$country, year=hma$year), FUN=sum, na.rm=TRUE)

hmc <- melt(hmb, id=1:2)
hmc$type <- str_extract(hmc$variable, "tbhiv|newrel|ret_nrel")
hmc$out <- str_extract(hmc$variable, "coh|succ|fail|died|lost|neval")

# Combine newrel and ret_nrel

hmc$type <- ifelse(hmc$type=="tbhiv", "tbhiv", "all")
hmc1 <- aggregate(hmc[4], by=list(year=hmc$year, type=hmc$type, out=hmc$out), FUN=sum, na.rm=TRUE)

hmd <- cast(hmc1, year+out~type)

hmd$`HIV-` <- hmd$all - hmd$tbhiv
hmd <- rename(hmd, c(tbhiv="HIV+"))

hme <- melt(as.data.frame(hmd[-3]), id=1:2, variable_name = "type")

hmf <- cast(hme, ...~out)

hmf$`Treatment success` <- hmf$succ / hmf$coh * 100
hmf$Failed <- hmf$fail / hmf$coh * 100
hmf$Died <- hmf$died / hmf$coh * 100
hmf$`Lost to follow-up` <- hmf$lost / hmf$coh * 100
hmf$`Not evaluated` <- hmf$neval / hmf$coh * 100

hmg <- melt(as.data.frame(hmf[c(1:2,9:ncol(hmf))]), id=1:2) # It's a melt and cast fiesta!

hiv_ts_d <- ggplot(hmg, aes(variable, value, fill=type)) + geom_bar(stat="identity", position="dodge", width=0.5) + theme_glb.rpt() + labs(x="") + scale_y_continuous("Percentage of cohort", limits=c(0,100)) + scale_fill_brewer("", type = "qual", palette=6) + ggtitle(paste("Outcomes of TB treatment by HIV status, ", thisyear-2, sep=""))

figsave(hiv_ts_d, hmg, "B4_6_hiv_ts_d")


# 5_9_txout_mdr --------------------------------------------------------

tma <- subset(tb, year %in% 2007:(yr-2), c(g_whoregion, year, mdr_coh, mdr_cur, mdr_cmplt, mdr_succ, mdr_fail, mdr_died, mdr_lost, c_mdr_neval))


# Fill in 0s for NAs
tma[is.na(tma)] <- 0

trbr <- aggregate(tma[3:ncol(tma)], by=list(g_whoregion=tma$g_whoregion, year=tma$year), FUN=sum, na.rm=TRUE)
trbg <- aggregate(tma[3:ncol(tma)], by=list(year=tma$year), FUN=sum, na.rm=TRUE)

trbg$g_whoregion <- "Global"

#combine
trb <- rbind(trbr, trbg)
trb <- .shortnames(trb, col="g_whoregion")
trb$g_whoregion <- factor(trb$g_whoregion, levels=trb[trb$year==2007, "g_whoregion"])

trb$mdr_succ <- ifelse(trb$year>=2011, trb$mdr_succ, trb$mdr_cur + trb$mdr_cmplt)
trb$Success <- trb$mdr_succ / trb$mdr_coh * 100
trb$Died <- trb$mdr_died / trb$mdr_coh * 100
trb$Failed <- trb$mdr_fail / trb$mdr_coh * 100
trb$`Lost to follow-up` <- trb$mdr_lost / trb$mdr_coh * 100
trb$`Not evaluated` <- (trb$mdr_coh - (trb$mdr_succ + trb$mdr_died + trb$mdr_fail + trb$mdr_lost)) / trb$mdr_coh * 100

trb$cohort.size <- rounder(trb$mdr_coh)

trc <- melt(trb[c("g_whoregion", "year", "Success", "Died", "Failed", "Lost to follow-up", "Not evaluated")], id=1:2)

txout_mdr <- ggplot(trc, aes(year, value, fill=variable)) + geom_bar(stat="identity", position="stack") + facet_wrap(~g_whoregion, ncol=2) + geom_text(data=trb, aes(x=year, y=0,label=cohort.size, fill=NA), hjust=-.1, size=2.5, color="white") + theme_glb.rpt() + coord_flip() + scale_fill_brewer("", type = "qual", palette = 8) + labs(x="", y="Percentage of cohort") + theme(legend.position="bottom", panel.grid=element_blank()) + expand_limits(c(0,0)) + ggtitle(paste0("Treatment outcomes for patients diagnosed with MDR-TB by WHO Region, 2007\u2013", thisyear-3, " cohorts. \nTotal cases with outcome data is shown beside each bar.")) + scale_x_reverse() 

figsave(txout_mdr, trb, "5_9_txout_mdr")


# 7_1_hivtest_graph -----------------------------------------------------------

gaa <- subset(tbhiv, year>=2004, select=c('g_whoregion', 'year', 'hivtest_pct_denominator', 'hivtest_pct_numerator'))

gaa$regional <- ifelse(gaa$g_whoregion=='AFR', 'African \nregion', 'Regions \noutside \nAfrica')

gabr <- aggregate(gaa[c('hivtest_pct_denominator', 'hivtest_pct_numerator')], by=list(area=gaa$regional, year=gaa$year), FUN=sum, na.rm=TRUE)
gabg <- aggregate(gaa[c('hivtest_pct_denominator', 'hivtest_pct_numerator')], by=list(year=gaa$year), FUN=sum, na.rm=TRUE)
gabg$area <- 'Global'

gac <- rbind(gabr, gabg)

gac$hivtest_pct <- gac$hivtest_pct_numerator / gac$hivtest_pct_denominator * 100

gadstart <- 2004

hivtest_graph <- ggplot(subset(gac, year >= gadstart), aes(year, hivtest_pct, colour=area)) +   geom_line(size=1.5) + geom_text(data=subset(gac, year==max(gac$year)), aes(label = area), hjust=-.1, vjust=0, size=5) + scale_y_continuous(name = "Percentage of notified TB patients", limits=c(0,100), expand=c(0,0)) + scale_x_continuous("", labels=gadstart:(thisyear-1), breaks=gadstart:(thisyear-1)) + scale_color_brewer(name="WHO region", palette="Dark2") + expand_limits(x=c(gadstart, thisyear+0.5)) + ggtitle(glue('Percentage of notified TB patients with known HIV status, ', gadstart, "\u2013", thisyear-1)) + theme_glb.rpt() + theme(legend.position="none")

# windows(11, 7); gad; dev.off()
figsave(hivtest_graph, gac, "7_1_hivtest_graph")


# 7_3_hivprog_graph_all ------------------------------------------------------
warning("I should come back to 7_3_hivprog_graph_all next year and make sure it still works.")
gg <- subset(tbhiv, year>=2003, select=c('iso3', 'year', 'g_hbhiv63', 'hivtest', 'hivtest_pos', 'hiv_cpt', 'hiv_art', 'hiv_cpt_pct_numerator', 'hiv_cpt_pct_denominator', 'hiv_art_pct_numerator', 'hiv_art_pct_denominator', 'c_notified', 'hivtest_pos_pct_denominator', 'hivtest_pos_pct_numerator'))

# replace denominators with interpolated rates across years

gga <- merge(e.t[c('iso3', 'year', 'e_pop_num')], gg, all.y=T)

ggb <- melt(gga, id=1:4)

ggb$rate <- ggb$value / ggb$e_pop_num

ghe <- gga

for(var in c('c_notified', 'hivtest', 'hivtest_pos')) {
  
  gha <- cast(ggb[ggb$variable==var & !is.na(ggb$rate),c('iso3', 'year', 'rate')], year~iso3, value='rate')
  ghb <- timeSeries(as.matrix(as.matrix.cast_df(gha)))
  ghc <- na.omit(ghb, method="ie")
  ghc$year <- 2003:(thisyear-1)
  ghd <- melt(as.data.frame(ghc), id='year', variable='iso3')
  names(ghd)[3] <- paste(var, "ir", sep="_")
  ghe <- merge(ghe, ghd, all.x=T)
}

ghe$c_notified_m <- ghe$c_notified_ir * ghe$e_pop_num
ghe$hivtest_m <- ghe$hivtest_ir * ghe$e_pop_num
ghe$hivtest_pos_m <- ghe$hivtest_pos_ir * ghe$e_pop_num

ggf <- ghe

gl <- within(ggf, {
  c_notified_hnm <- ifelse(!is.na(c_notified) & !is.na(hivtest) & 
                             c_notified >= hivtest, c_notified, NA)
  hivtest_hnm <- ifelse(!is.na(c_notified) & !is.na(hivtest) & 
                          c_notified >= hivtest, hivtest, NA)
  
  hivtest_lo <- ifelse(!is.na(c_notified_m) & is.na(hivtest), 0, hivtest)
  hivtest_hi <- ifelse(!is.na(c_notified_m) & is.na(hivtest), c_notified_m, hivtest)
  
  hivtest_pos_lo <- ifelse(!is.na(hivtest_m) & is.na(hivtest_pos), 0, hivtest_pos)
  hivtest_pos_hi <- ifelse(!is.na(hivtest_m) & is.na(hivtest_pos), hivtest_m, hivtest_pos)
  
  hiv_cpt_lo <- ifelse(!is.na(hivtest_pos_m) & is.na(hiv_cpt), 0, hiv_cpt)
  hiv_cpt_hi <- ifelse(!is.na(hivtest_pos_m) & is.na(hiv_cpt), hivtest_pos_m, hiv_cpt)
  
  hiv_art_lo <- ifelse(!is.na(hivtest_pos_m) & is.na(hiv_art), 0, hiv_art)
  hiv_art_hi <- ifelse(!is.na(hivtest_pos_m) & is.na(hiv_art), hivtest_pos_m, hiv_art)
  
})

table(gl[!is.na(gl$c_notified_m),'year']) # 214 / 216 countries have reported in at least 1 year
table(gl[!is.na(gl$hivtest_m),'year'])     # 203 / 216 countries
table(gl[!is.na(gl$hivtest_pos_m),'year'])	# 202 / 216 countries (somehow I lost a country)

unique(e.t[!e.t$iso3  %in% unique(gl[!is.na(gl$c_notified_m),'iso3']),'country'])
unique(e.t[!e.t$iso3  %in% unique(gl[!is.na(gl$hivtest_m),'iso3']),'country'])
unique(e.t[!e.t$iso3  %in% unique(gl[!is.na(gl$hivtest_pos_m),'iso3']),'country'])
# table(tbhiv[!is.na(tbhiv$hivtest),'year'])

gk <- aggregate(gl[6:ncol(gl)], by=list(year=gl$year), FUN=sum, na.rm=T)

gah <- within(gk, {
  pht_best <- hivtest_hnm / c_notified_hnm
  pht_lo <- hivtest_lo / c_notified_m
  pht_hi <- hivtest_hi / c_notified_m
  
  phtp_best <- hivtest_pos_pct_numerator / hivtest_pos_pct_denominator
  phtp_lo <- hivtest_pos_lo / hivtest_m
  phtp_hi <- hivtest_pos_hi / hivtest_m
  
  pcpt_best <- hiv_cpt_pct_numerator / hiv_cpt_pct_denominator
  pcpt_lo <- hiv_cpt_lo / hivtest_pos_m
  pcpt_hi <- hiv_cpt_hi / hivtest_pos_m
  
  part_best <- hiv_art_pct_numerator / hiv_art_pct_denominator
  part_lo <- hiv_art_lo / hivtest_pos_m
  part_hi <- hiv_art_hi / hivtest_pos_m
})

gai <- melt(gah[c('year', "part_hi", "part_lo", "part_best", "pcpt_hi", "pcpt_lo", "pcpt_best", "phtp_hi", "phtp_lo", "phtp_best", "pht_hi", "pht_lo", "pht_best")], id=1)

for(ro in 1:nrow(gai)){
  
  both <- as.data.frame(str_split(gai[ro,'variable'], "_"))
  gai[ro, 'var'] <- both[1,]
  gai[ro, 'hilo'] <- both[2,]
  
}

gaj <- cast(gai, year+var~hilo)

gaj['Percentages'] <- factor(gaj$var, levels=c("pht", "phtp", "pcpt", "part"), labels=c('% of notified TB patients with known HIV status', '% of notified TB patients with known HIV status \nwho were HIV-positive', '% of notified HIV-positive TB patients\n started on CPT', '% of notified HIV-positive TB patients\nstarted on ART'))

gak <- gaj[gaj$year>=2007 & gaj$Percentages!='% of notified TB patients with known HIV status',] 

gak[c("best", "hi", "lo")] <- gak[c("best", "hi", "lo")] * 100

hivprog_graph_all <- ggplot(gak, aes(year, best)) + geom_line(size=1, alpha=.5) + geom_ribbon(aes (year, best, ymin=lo, ymax=hi), alpha=0.2) + facet_wrap(~Percentages, ncol=3) + scale_y_continuous(limits = c(0, 100), name = "Percentage of notified TB patients", breaks=c(0, 20, 40, 60, 80, 100)) + scale_x_continuous("") + theme_glb.rpt()  + ggtitle(paste0('Percentage of notified TB patients with known HIV status who were HIV positive, and percentage of notified \nHIV-positive TB patients enrolled on co-trimoxazole preventive therapy (CPT) and antiretroviral therapy (ART),\n ', min(gak$year), '\u2013', max(gak$year), '(a)'))

# windows(13,6); gal
figsave(hivprog_graph_all, gak, "7_3_hivprog_graph_all")

# Title: Proportion of TB patients tested for HIV, tested patients found positive and 
# HIV-positive TB patients on CPT and ART, 2003-2010.
# Footnote: Lines indicate percentage from countries who have reported both the 
# numerator and denominator for the percentage. Ranges indicate 0% and 100% for 
# countries reporting the denominator only. Countries reporting neither numerator
# nor denominator for the time series are excluded and account for <1% of the estimated
# Global TB/HIV burden. Missing denominator data were estimated using interpolation and
# continuation of rates outside of the range of reported data.

# Intervals indicate scenarios where non-reporting countries report 0% or 100%.


# 7_4_hiv_art_etbhiv ----------------------------------------------------------

gca <- subset(tbhiv, year>=2004, select=c('iso3', 'year', 'hivtest_pos', 'hiv_art'))

gcb <- aggregate(gca[3:ncol(gca)], by=list(year=gca$year), FUN=sum, na.rm=T)

gcb1 <- merge(subset(araw, group_name=="global", c(year, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi, e_inc_tbhiv_num)), gcb)

gcc <- melt(gcb1, id=1:3)

gcc$value <- gcc$value/1000

# gcc1 <- cast()

gcc$variable <- factor(gcc$variable, levels = c("e_inc_tbhiv_num", "hivtest_pos", "hiv_art"), labels = c("Estimated HIV-positive incident TB cases", 'Notified HIV-positive TB patients', 'HIV-positive TB patients on ART'))

hiv_art_etbhiv <- ggplot(gcc, aes(year, value, color=variable)) + geom_ribbon(aes(year, ymin=e_inc_tbhiv_num_lo/1000, ymax=e_inc_tbhiv_num_hi/1000), alpha=0.4, fill=inch.color, color=NA) + geom_line(size=1) + scale_y_continuous("Numbers in thousands", limits=c(0,1500)) + theme_glb.rpt() + scale_x_continuous(name="") +  scale_colour_manual(name="", values=c(inch.color, "firebrick", "dark blue")) + geom_text(data=gcc[gcc$year==thisyear-2, ], aes(label=variable), vjust=3, hjust=1)  + ggtitle(paste("ART enrolment among HIV-positive TB patients compared with the reported number of HIV-positive \nTB patients and the estimated number of HIV-positive people who developed TB, 2004", thisyear-1, sep="\u2013")) + theme(legend.position="none")

# windows (10,7); hiv_art_etbhiv; dev.off()
figsave(hiv_art_etbhiv, gcc, "7_4_hiv_art_etbhiv")


# 7_6_hiv_ipt_graph -----------------------------------------------------

gfa <- subset(n, year>=2005, select=c('iso3', "g_whoregion", 'year', 'hiv_ipt'))

gfa$area <- ifelse(gfa$iso3 %in% c("ZAF"), "South Africa", ifelse(gfa$g_whoregion=="AFR", "Rest of AFR", "Rest of world"))


gfb <- aggregate(gfa[4], by=list(year=gfa$year, area=gfa$area), FUN=sum, na.rm=T)

gfb1 <- aggregate(gfa[4], by=list(year=gfa$year), FUN=sum, na.rm=T)
gfb1$area <- "Global"

gfb2 <- rbind(gfb, gfb1)

gfc <- melt(gfb2, id=1:2)

gfc$value <- gfc$value/1000

gfc$area <- factor(gfc$area, levels=c( "Rest of world", "Rest of AFR", "South Africa", "Global"))

# line option
hiv_ipt_graph <- ggplot(gfc, aes(year, value, color=area)) + geom_line(size=1) + scale_y_continuous("Number of people living with HIV (thousands)") + theme_glb.rpt() + scale_x_continuous(name="", breaks=c(min(gfc$year):max(gfc$year))) +  scale_color_brewer(name="", palette="Dark2") + guides(color = guide_legend(reverse = TRUE)) + ggtitle(paste("Provision of isoniazid preventive therapy (IPT) to people living with HIV, 2005", thisyear-1, sep="\u2013")) 

# windows (10,7); gfd; dev.off()
figsave(hiv_ipt_graph, gfc, "7_6_hiv_ipt_graph")

# Highly experimental figure on where countries lie in terms of burden.

# test <- tb %>% filter(year==thisyear-1) %>% select(year, g_whoregion, c_newinc, c_newinc_100k, iso2) %>% merge(p[c("year", "iso2", "e_pop_num")]) %>% mutate(c_newinc_100k=c_newinc / e_pop_num * 1e5) 
#   ggplot(test, aes(c_newinc_100k, c_newinc, color=g_whoregion)) +  geom_text(aes(label=iso2), size=4) + scale_y_log10() + scale_x_log10()


## END ---------------------------------------


