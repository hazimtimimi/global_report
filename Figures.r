# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figures in the global report
# Called from create_tables_figures.r which sets up the necessary dependencies
# Tom Hiatt
# 10 July 2012, updated July 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Probably I need to change this to a R markdown document so the whole thing produces a PDF with all the tables and figures one after the other and individual PDFs, PNGs, etc. in a folder. UPDATE: Rmarkdown can't make a decent table in word or HTML to print. Someday I will learn LaTex or something and make it work.


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Links ------
#
# Link GTBR2015 figure numbers to code sections
#
# 2.3 Estimated TB incidence: top-ten countries: 2_3_topten
# 2.7 Estimated TB incidence rates by WHO region: 2_7_inc_reg
# 2.8 Estimated TB incidence rates, 22 high-burden countries: 2_8_inc_hbc
# 2.11 Trends in estimated TB prevalence rates by WHO region: 2_11_prev_reg
# 2.14 Trends in estimated TB mortality rates by WHO region: 2_14_mort_reg
# 2.15 Trends in estimated TB mortality rates, 22 HBCs: 2_15_mort_hbc
#
# 3.1 Regional TB notification rates by age: 3_1_agesex_reg
# 3.2 Global trends in case notification and incidence rates:3_2_inc_notif_glo
# 3.3 Case notification and incidence rates by WHO region: 3_3_inc_notif_reg
# 3.4 Case notification and incidence rates, 22 HBCs: 3_4_inc_notif_hbc
# 3.5 Treatment outcomes for new and relapse cases: 3_5_txsucc
#
# (box 3.5): Outcomes of TB treatment by HIV status: B3_5_hiv_ts_d
#
#
# 6.1 Percentage of TB patients with known HIV status: 6_1_hivtest_graph
# 6.3 % TB patients HIV+, and % HIV+ TB patients on CPT and ART: 6_3_hivprog_graph_all
# 6.4 ART enrolment among HIV-positive TB patients: 6_4_hiv_art_etbhiv
# 6.6 Provision of IPT for PLHIV: 6_6_hiv_ipt
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# ******************************************************
# Old figures

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

# Dropped in 2015 (done by others):
# 2_6_fig_global
# 5_9_txout_mdr

# ******************************************************


if(flg_show_estimates){
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Chapter 2 ------
  # The burden of disease caused by TB
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  
  # 2_3_topten ---------------------------------------------------------
  # The 'top 10 countries' by absolute number and by rate, in 2015 only (incidence) are used.
  # Only countries with over a thousand total cases are considered.
  
  latest_estimates <- eraw.t  %>%
    filter(e_inc_num > 1e3 & year==report_year-1)  %>%
    .shortnames()
  
  hplot_estimates_with_ui <- function(df, plot_title, var_name, scale_label){
    
    # Horizontal plot of estimates with uncertainty intervals
    # Assumes df has 4 variables called country, var, var_lo, var_hi
    
    p <-  ggplot(df, aes(x=reorder(country,var), y=var)) +
      geom_point() +
      labs(x="", y=scale_label, title=plot_title) +
      geom_errorbar(aes(ymin=var_lo, ymax=var_hi), height=.25) +
      theme_glb.rpt() +
      theme(plot.title = element_text(hjust = 0)) +
      expand_limits(y=0) +
      coord_flip()
    
    figsave(p, df, paste0("2_3_topten_", var_name), width=5, height=4)
  }
  
  # Top 10 countries by incidence rates
  latest_estimates %>%
    select(country,
           var    = e_inc_100k,
           var_lo = e_inc_100k_lo,
           var_hi = e_inc_100k_hi) %>%
    arrange(desc(var)) %>%
    slice(1:10) %>%
    hplot_estimates_with_ui("Incidence: rate per 100 000 population per year", "e_inc_100k", "Rate")
  
  # Top 10 countries by incidence absolute number
  latest_estimates %>%
    select(country,
           var    = e_inc_num,
           var_lo = e_inc_num_lo,
           var_hi = e_inc_num_hi) %>%
    arrange(desc(var)) %>%
    slice(1:10) %>%
    mutate(var    = var / 1e6,
           var_lo = var_lo / 1e6,
           var_hi = var_hi / 1e6) %>%
    hplot_estimates_with_ui("Incidence: absolute numbers", "e_inc_num", "Millions")
  
  # and now clear up the mess left behind
  rm(latest_estimates)
  
  # 2_6_incprevmort_glo ----------------------------------
  
  
  eget <- c("year", "e_pop_num", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_prev_100k", "e_prev_100k_lo", "e_prev_100k_hi", "e_mort_exc_tbhiv_100k", "e_mort_exc_tbhiv_100k_lo", "e_mort_exc_tbhiv_100k_hi")
  
  esta <- subset(araw.t, year >=1990 & group_name=="global", select=eget)
  
  estb <- n.t %>% filter(year>=1990) %>% select(year, c_newinc) %>%
    group_by(year) %>% summarize(c_newinc=sum0(c_newinc)) 
  #   estc <- aggregate(estb["c_newinc"], by=list(year=estb$year), FUN=sum, na.rm=TRUE)
  
  estd <- merge(esta, estb, all.x=TRUE)
  
  estd$c_newinc_100k <- estd$c_newinc / estd$e_pop_num * 1e5
  
  estf <- NULL
  for(var in c("inc", "prev", "mort_exc_tbhiv")){
    df <- subset(estd, select=c("year", paste0("e_", var, "_100k"), paste0("e_", var, "_100k_lo"), paste0("e_", var, "_100k_hi")))
    names(df) <- c("year", "best", "lo", "hi")
    df$var <- var
    estf <- rbind(estf, df)
  }
  
  estg <- estd[c("year", "c_newinc_100k")]
  estg$var <- "inc"
  
  esth <- merge(estf, estg, all.x=TRUE)
  esth$var <- factor(esth$var, levels=c("inc", "prev", "mort_exc_tbhiv"), labels=c("Incidence and notification", "Prevalence", "Mortality (excluding HIV)"))
  
  esti <- esth %>% group_by(var) %>% arrange(var, year) %>% mutate(target=ifelse(var=="Incidence and notification", as.numeric(NA), best[1]/2)) 
  
  incprevmort_glo <- ggplot(esti, aes(year, best, fill=var, ymin=0)) + geom_line(size=1, aes(color=var)) + geom_ribbon(aes (year, best, ymin=lo, ymax=hi), alpha=0.2) + geom_hline(aes(yintercept=target), linetype=2) + geom_line(aes(year, c_newinc_100k), color="black", size=1) + theme_glb.rpt() + facet_wrap(~var, scales="free_y") + theme(legend.position="none") + scale_x_continuous("", minor_breaks=seq(1990, 2015, 1)) + scale_y_continuous(breaks=pretty_breaks()) + ylab("Rate per 100 000 population")
  
  figsave(incprevmort_glo, esti, "2_6_incprevmort_glo")
  
  # 2_x_incdist ----------------------------------
  
  
  inca <- eraw.t %>% filter(year==report_year-1, !is.na(e_inc_num)) %>% mutate(name=ifelse(e_inc_num > 1e5 | e_inc_100k > 450, iso3, NA)) %>% inner_join(subset(n.t, year==report_year-1, c(iso3, g_hbc22))) %>% mutate(hbc=ifelse(g_hbc22=="high", "High-burden \ncountries", NA)) 
    
  incb <- araw.t %>% filter(year==report_year-1, group_type=="g_whoregion") %>% .shortnames(col = "group_name") %>% mutate(name=group_name)   
    
    incdist <- ggplot(inca, aes(e_inc_100k, e_inc_num/1e3, color=hbc, size=e_pop_num/1e6)) + geom_point() + theme_glb.rpt() + geom_text(aes(label=name), size=2.5, vjust=-1.2) + labs(x="Rate per 100 000 population per year", y="Cases per year (thousands)", color="22 High-burden \ncountries", size="Population\n(millions)", title=paste("Global distribution of estimated TB incidence by rate and absolute number,", max(inca$year))) 
  
  incdistb <- ggplot(incb, aes(e_inc_100k, e_inc_num/1e3, size=e_pop_num/1e6, ymin=0, xmin=0)) + geom_point() + theme_glb.rpt() + geom_text(aes(label=name), size=2.5, vjust=2.3) + labs(x="Rate per 100 000 population per year", y="Cases per year (thousands)", color="22 High-burden \ncountries", size="Population\n(millions)", title="WHO region") 
  

  figsave(incdist, inca, "2_x_incdist")
  figsave(incdistb, incb, "2_x_incdistb", width = 5, height=4)
  
  # 2_7_inc_reg ------------------------------------------------------
  
  reg_aggs <- araw.t %>%
    filter(group_type=="g_whoregion" & year < report_year)
  
  reg_aggs$g_whoregion <- factor(reg_aggs$group_name,
                                 labels=c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific"))
  
  reg_aggs <- select(reg_aggs,
                     group_name, g_whoregion, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
                     e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi  )
  
  inc_reg <- qplot(year, e_inc_100k, data=reg_aggs, geom='line', colour=I(inc.color)) +
    geom_ribbon(aes(year,
                    ymin=e_inc_100k_lo,
                    ymax=e_inc_100k_hi), fill=I(inc.color), alpha=0.4) +
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I(inch.color)) +
    geom_ribbon(aes(year,
                    ymin=e_inc_tbhiv_100k_lo,
                    ymax=e_inc_tbhiv_100k_hi), fill=I(inch.color), alpha=0.4) +
    facet_wrap(~g_whoregion, scales='free_y') +
    scale_x_continuous("") +
    ylab("Rate per 100 000 population per year") +
    expand_limits(y=0) +
    theme_glb.rpt() +
    ggtitle(paste0("Estimated TB incidence rates by WHO region, 1990-", report_year-1, ". \nRegional trends in estimated TB incidence rates (green) and estimated incidence rates of HIV-positive TB (red). \nShaded areas represent uncertainty bands."))
  
  figsave(inc_reg, reg_aggs, "2_7_inc_reg")
  
  # and now clear up the mess left behind
  rm(list=c("inc_reg", "reg_aggs"))
  
  
  # 2_8_inc_hbc -----------------------------------------------------
  
  # Fudge because eraw.t doesn't contain g_hbc22
  hbc_iso2 <- e.t %>%
    filter(g_hbc22=="high" & year==report_year-1) %>%
    select(iso2)
  
  # Get incidence estimates for the HBCs
  inc_hbc_data <- eraw.t %>%
    filter(iso2 %in% hbc_iso2$iso2 & year < report_year) %>%
    select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
           e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi)  %>%
    .shortnames()
  
  
  inc_hbc <- qplot(year, e_inc_100k, data=inc_hbc_data, geom='line', colour=I(inc.color)) +
    geom_ribbon(aes(year,
                    ymin=e_inc_100k_lo,
                    ymax=e_inc_100k_hi), fill=I(inc.color), alpha=0.4) +
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I(inch.color)) +
    geom_ribbon(aes(year,
                    ymin=e_inc_tbhiv_100k_lo,
                    ymax=e_inc_tbhiv_100k_hi), fill=I(inch.color), alpha=0.4) +
    facet_wrap(~country, scales='free_y') +
    scale_x_continuous("") +
    ylab("Rate per 100 000 population per year") +
    expand_limits(y=0) +
    theme_glb.rpt() +
    ggtitle(paste0("Estimated TB incidence rates, 22 high-burden countries, 1990-", report_year-1, ". \nTrends in estimated TB incidence rates (green) and estimated incidence rates of HIV-positive TB (red). \nShaded areas represent uncertainty bands."))
  
  # Put back x-axis (weird ggplot problem because facets don;t fit page exactly...)
  inc_hbc <- facetAdjust(inc_hbc)
  
  figsave(inc_hbc, inc_hbc_data, "2_8_inc_hbc")
  
  # and now clear up the mess left behind
  rm(list=c("inc_hbc_data", "inc_hbc"))
  
  
  
  # 2_11_prev_reg ----------------------------------------------------
  
  prev_mort_reg_aggs <- araw.t %>%
    filter(group_type=="g_whoregion")
  
  prev_mort_reg_aggs$forecast <- ifelse(prev_mort_reg_aggs$year >= report_year, "forecast", "current")
  
  prev_mort_reg_aggs$g_whoregion <- factor(prev_mort_reg_aggs$group_name,
                                           labels=c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific"))
  
  prev_mort_reg_aggs <- select(prev_mort_reg_aggs,                              group_name, g_whoregion, year, forecast,                                e_prev_100k, e_prev_100k_lo, e_prev_100k_hi,                                e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi) %>% group_by(g_whoregion) %>% arrange(g_whoregion, year) %>% mutate(target.prev=e_prev_100k[1]/2, target.mort=e_mort_exc_tbhiv_100k[1]/2) # Add two variables for STP target prevalence and mortality rates (half of 1990 values) to be displayed as dashed lines
  
  
  prev_reg <- ggplot(prev_mort_reg_aggs,
                     aes(year, e_prev_100k)) +
    geom_line(colour=I(prev.color)) +
    geom_ribbon(aes(year,
                    ymin=e_prev_100k_lo,
                    ymax=e_prev_100k_hi), fill=I(prev.color), alpha=0.4) +
    facet_wrap(~g_whoregion, scales='free_y') +
    geom_hline(aes(yintercept=target.prev), linetype=2)  +
    scale_x_continuous('') +
    ylab('Rates per 100 000 population') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position="none") +
    ggtitle(paste0("Trends in estimated TB prevalence rates 1990-", report_year-1, " and forecast TB prevalence rates 2015, by WHO region. \nShaded areas represent uncertainty bands. The horizontal dashed lines represent the Stop TB Partnership \ntarget of a 50% reduction in the prevalence rate by 2015 compared with 1990. The last point shows a projection for 2015."))
  
  figsave(prev_reg, prev_mort_reg_aggs, "2_11_prev_reg")
  
  # 2_14_mort_reg ----------------------------------------------------
  
  mort_reg <- ggplot(prev_mort_reg_aggs,
                     aes(year, e_mort_exc_tbhiv_100k)) +
    geom_line(colour=I(mort.color)) +
    geom_ribbon(aes(year,
                    ymin=e_mort_exc_tbhiv_100k_lo,
                    ymax=e_mort_exc_tbhiv_100k_hi), fill=I(mort.color), alpha=0.4) +
    facet_wrap(~g_whoregion, scales='free_y') +
    geom_hline(aes(yintercept=target.mort), linetype=2) +
    scale_x_continuous('') +
    ylab('Rates per 100 000 population') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position="none") +
    ggtitle(paste0("Trends in estimated TB mortality rates 1990-", report_year-1, " and forecast TB mortality rates 2015, by WHO region. \nEstimated TB mortality excludes TB deaths among HIV-positive people. Shaded areas represent uncertainty bands.a \nThe horizontal dashed lines represent the Stop TB Partnership target of a 50% reduction in the mortality rate by 2015 compared with 1990. \nThe last point shows a projection for 2015."))
  
  figsave(mort_reg, prev_mort_reg_aggs, "2_14_mort_reg")
  
  # and now clear up the mess left behind
  rm(list=c("prev_mort_reg_aggs", "prev_reg", "mort_reg"))
  
  
  
  
  # 2_15_mort_hbc ---------------------------------------------------------
  
  # Fudge because eraw doesn't contain g_hbc22
  hbc_iso2 <- e.t %>%
    filter(g_hbc22=="high" & year==report_year-1) %>%
    select(iso2)
  
  # Get mortality estimates for the HBCs
  mort_hbc_data <- eraw.t %>%
    filter(iso2 %in% hbc_iso2$iso2 & year < report_year) %>%
    select(country, iso2, year, e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi) %>% mutate(target.mort=e_mort_exc_tbhiv_100k[1]/2)  %>% # Add variable for STP target prevalence rates (half of 1990 values) to be displayed as dashed lines
    .shortnames() 
  
  mort_hbc_data$forecast <- ifelse(mort_hbc_data$year >= report_year, "forecast", "current")
  
  
  mort_hbc <- qplot(year, e_mort_exc_tbhiv_100k, data=mort_hbc_data, geom='line', colour=I(mort.color)) +
    geom_ribbon(aes(year,
                    ymin=e_mort_exc_tbhiv_100k_lo,
                    ymax=e_mort_exc_tbhiv_100k_hi), fill=I(mort.color), alpha=0.4) +
    geom_hline(aes(yintercept=target.mort), linetype=2) +
    facet_wrap(~country, scales='free_y') +
    #geom_point(aes(year, vr.tbrate.raw), shape=I(4)) +
    xlab("") + ylab('Rates per 100 000 population per year') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position='none') +
    ggtitle(paste0("Trends in estimated TB mortality rates 1990-", report_year-1, " and forecast TB mortality rates 2015, 22 high-burden countries. \nEstimated TB mortality excludes TB deaths among HIV-positive people. The horizontal dashed lines represent the Stop TB Partnership \ntarget of a 50% reduction in the mortality rate by 2015 compared with 1990. The last point shows a projection for 2015. (a) \nUncertainty is due to adjustments made to the mortality data from vital registration systems that were reported by countries \n(mortality data from vital registration systems are represented by the 'x' symbol)."))
  
  # Add footnote
  mort_hbc <- arrangeGrob(mort_hbc, sub = textGrob("(a) The width of an uncertainty band narrows as the quality and completeness of vital registration data improves.", x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))
  
  # mort_hbc <- facetAdjust(mort_hbc)
  # It looks like you can have one tweak or another, but not both. (X-axis on orphaned panels or footnote.)
  
  figsave(mort_hbc, mort_hbc_data, "2_15_mort_hbc")
  
  # and now clear up the mess left behind
  rm(list=c("mort_hbc_data", "mort_hbc"))
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 22 HBCs and regional Profiles --------
  # Incidence (and notifications), prevalence and mortality graphs
  # For graphic designer to use in the printed report
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  
  # Get HBCs case notification and merge with their estimates
  profile_est <-  filter(n, g_hbc22=="high") %>%
    select(iso2, year, c_newinc) %>%
    merge(eraw)
  
  profile_est$group_name <- profile_est$country
  
  # Combine with the regional aggregated estimates
  profile_est <- merge(profile_est, araw[araw$group_type=="g_whoregion",], all=TRUE)
  
  #Calculate the case notification rate
  profile_est$c_newinc_100k <- profile_est$c_newinc  * 1e5 / profile_est$e_pop_num
  
  # Incidence and notifications
  
  pdf(width=14, height=9.5, file="CPFigs/hbc_cp_inc.pdf")
  qplot(year, e_inc_100k, data=profile_est, geom="line", colour=I(inc.color)) +
    geom_ribbon(aes(year,
                    ymin=e_inc_100k_lo,
                    ymax=e_inc_100k_hi), fill=I(inc.color), alpha=0.4) +
    geom_line(aes(year, e_inc_tbhiv_100k), colour=I(inch.color)) +
    geom_line(aes(year, c_newinc_100k)) +
    geom_ribbon(aes(year,
                    ymin=e_inc_tbhiv_100k_lo,
                    ymax=e_inc_tbhiv_100k_hi), fill=I(inch.color), alpha=0.4) +
    facet_wrap(~group_name, scales="free", ncol=4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(name="", expand = c(0, 0)) +
    expand_limits(y=0) +
    theme_glb.rpt(base_size=6) +
    theme(legend.position="none", panel.grid.minor = element_blank()) +
    ggtitle("Incidence")
  dev.off()
  
  # Prevalence
  
  pdf(width=14, height=9.5, file="CPFigs/hbc_cp_prev.pdf")
  qplot(year, e_prev_100k, data=profile_est, geom="line", colour=I(prev.color)) +
    geom_ribbon(aes(year,
                    ymin=e_prev_100k_lo,
                    ymax=e_prev_100k_hi), fill=I(prev.color), alpha=0.4) +
    facet_wrap(~group_name, scales="free", ncol=4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(name="", expand = c(0, 0)) +
    expand_limits(y=0) +
    theme_glb.rpt(base_size=6) +
    theme(legend.position="none", panel.grid.minor = element_blank()) +
    ggtitle("Prevalence")
  dev.off()
  
  # Mortality
  
  pdf(width=14, height=9.5, file="CPFigs/hbc_cp_mort.pdf")
  qplot(year, e_mort_exc_tbhiv_100k, data=profile_est, geom="line", colour=I(mort.color)) +
    geom_ribbon(aes(year,
                    ymin=e_mort_exc_tbhiv_100k_lo,
                    ymax=e_mort_exc_tbhiv_100k_hi), fill=I(mort.color), alpha=0.4) +
    facet_wrap(~group_name, scales="free", ncol=4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(name="", expand = c(0, 0)) +
    expand_limits(y=0) +
    theme_glb.rpt(base_size=6) +
    theme(legend.position="none", panel.grid.minor = element_blank()) +
    ggtitle("Mortality")
  dev.off()
  
  # End of figures including estimates
}



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 3 ------
# TB Case notifications and treatment outcomes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 3_1_agesex_reg -------------------------------------------------------------

fa <- subset(n, year==report_year-1, c("country", "g_whoregion", "newrel_m014", "newrel_m1524", "newrel_m2534", "newrel_m3544", "newrel_m4554", "newrel_m5564", "newrel_m65", "newrel_f014", "newrel_f1524", "newrel_f2534", "newrel_f3544", "newrel_f4554", "newrel_f5564", "newrel_f65"))

fb <- subset(p, year==report_year-1, c("country", "e_pop_m014", "e_pop_m1524", "e_pop_m2534", "e_pop_m3544", "e_pop_m4554", "e_pop_m5564", "e_pop_m65", "e_pop_f014", "e_pop_f1524", "e_pop_f2534", "e_pop_f3544", "e_pop_f4554", "e_pop_f5564", "e_pop_f65"))

fc <- merge(fa, fb)

# Drop pop numbers from non reporters
fc$check <- sum_of_row(fc[c("newrel_m1524", "newrel_m2534", "newrel_m3544", "newrel_m4554", "newrel_m5564", "newrel_m65", "newrel_f1524", "newrel_f2534", "newrel_f3544", "newrel_f4554", "newrel_f5564", "newrel_f65")] )

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

agesex_reg1 <- ggplot(fg, aes(age, newrel_100k, colour=Region, group=Region)) + geom_line(size=1) + scale_y_continuous(name = "Rate per 100 000 population per year") + scale_x_discrete("", labels=levels(fg$age)) + scale_color_brewer(name="", palette="Dark2") + ggtitle(paste0("Regional TB notification rates by age, ", report_year-1, "(a)")) + theme_glb.rpt()

# Add footnote
fgf1 <- rounder(sum(fg$newrel) / sum(n[n$year==report_year -1, "c_newinc"], na.rm=TRUE) * 100)
fgf2 <- n[n$year==report_year -1 & n$g_hbc22=="high", "country"]
fgf3 <- fgf2[!fgf2 %in% fc1$country]
fgf4 <- .shortnames(data.frame(country=fgf3), col="country")

fgfoot <- paste0("(a) Countries not reporting cases in these categories are excluded. \nCases included make up ", fgf1, "% of reported cases and exclude the following high-burden countries: \n", "Afghanistan, Ethiopia, Mozambique and Thailand.")

agesex_reg <- arrangeGrob(agesex_reg1, sub = textGrob(fgfoot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))

# windows(11, 7); gad; dev.off()
figsave(agesex_reg, fg, "3_1_agesex_reg")


if(flg_show_estimates){
  
  # Global rates of incidence, and notifications
  # 3_2_inc_notif_glo ----------------------------------------------------
  
  eha <- subset(araw.t, group_type=="global")
  
  ehb <- merge(subset(eha, select=c("group_name", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), aggregate(n.t['c_newinc'], by=list(year=n.t$year), FUN=sum, na.rm=TRUE))
  
  ehb$newrel_100k <- ehb$c_newinc / ehb$e_pop_num * 100000
  
  inc_notif_glo <- qplot(year, e_inc_100k, data=ehb, geom='line', colour=I('#00FF33')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('#00FF33'), alpha=0.4) +
    geom_line(aes(year, newrel_100k)) + scale_x_continuous('') +
    ylab('Rate per 100 000 population per year')  + theme_glb.rpt() +
    ggtitle(paste0('Global trends in case notification (black) and estimated TB \nincidence (green) rates, 1990-', report_year-1, ". \nCase notifications include new and relapse cases (all forms)."))
  
  figsave(inc_notif_glo, ehb, "3_2_inc_notif_glo", width=6, height=6)
  
  # Regional rates of incidence, and notifications
  # 3_3_inc_notif_reg ----------------------------------------------------
  
  efa1 <- subset(araw.t, group_type=="g_whoregion" & year < report_year)
  
  # names(regional) <- gsub ('_', '\\.', names (regional))
  efa1$g_whoregion <- factor(efa1$group_name, labels=c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific"))
  
  efa <- merge(subset(efa1, select=c("group_name", "g_whoregion", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), aggregate(n.t['c_newinc'], by=list(group_name=n.t$g_whoregion, year=n.t$year), FUN=sum, na.rm=TRUE))
  
  efa$newrel_100k <- efa$c_newinc / efa$e_pop_num * 100000
  
  inc_notif_reg <- qplot(year, e_inc_100k, data=efa, geom='line', colour=I('#00FF33')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('#00FF33'), alpha=0.4) +
    geom_line(aes(year, newrel_100k)) +
    facet_wrap(~g_whoregion, scales='free_y') + scale_x_continuous('') +
    ylab('Rate per 100 000 population per year') +expand_limits(y=0) + theme_glb.rpt() +
    ggtitle(paste0('Case notification and estimated TB incidence rates by WHO region, 1990-', report_year-1, '. \nRegional trends in case notification rates (new and relapse cases, all forms) (black) and estimated TB incidence rates (green). \nShaded areas represent uncertainty bands.'))
  
  figsave(inc_notif_reg, efa, "3_3_inc_notif_reg")
  
  
  # HBC rates of incidence, and notifications
  # 3_4_inc_notif_hbc ----------------------------------------------------
  
  ega <- subset(merge(eraw.t, e.t[e.t$year==report_year-1,c("country", "g_hbc22")]), g_hbc22=="high" & year < report_year) # This hack is until we add g_hbc22 to eraw.
  
  egb <- .shortnames(merge(subset(ega, select=c("country", "g_whoregion", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), subset(n.t, select=c('country', 'year', 'c_newinc'))), ord='multiyear')
  
  egb$newrel_100k <- egb$c_newinc / egb$e_pop_num * 100000
  
  inc_notif_hbc <- qplot(year, e_inc_100k, data=egb, geom='line', colour=I(inc.color)) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I(inc.color), alpha=0.4) +
    geom_line(aes(year, newrel_100k)) +
    facet_wrap(~country, scales='free_y') + scale_x_continuous('') +
    ylab('Rate per 100 000 population per year') + expand_limits(y=0) + theme_glb.rpt() +
    ggtitle(paste0('Case notification and estimated TB incidence rates, 22 high-burden countries, 1990-', report_year-1, '. \nTrends in case notification rates (new and relapse cases, all forms) (black) and estimated TB incidence rates (green). \nShaded areas represent uncertainty bands.'))
  
  figsave(inc_notif_hbc, egb, "3_4_inc_notif_hbc")
  
  
  # End of estimates figures
}



# 3_5_txsucc -------------------------------------------------------------------

tsr <- subset(o,
              year==report_year-2,
              select=c("country", "year","g_whoregion", "g_hbc22", "newrel_coh", "newrel_succ", "newrel_fail", "newrel_died", "newrel_lost", "c_newrel_neval"))

# create a combined table with HBCs and aggregates
tsr_table <- hbc_and_aggs_table(df = tsr, country.col = 1, year.col = 2, data.cols = 5:ncol(tsr) )

# Add an asterisk to the name if country did not include relapse cases in the outcomes cohort
asterisks <- .shortnames(subset(o, rel_with_new_flg==0 & g_hbc22=="high" & year==report_year-2, country))

tsr_table$area <- ifelse(tsr_table$area %in% asterisks$country, paste0(tsr_table$area, "*"), tsr_table$area)

tsr_table$area <- factor(tsr_table$area, levels=rev(tsr_table$area))

tsr_table$`Treatment success` <- tsr_table$newrel_succ     * 100 / tsr_table$newrel_coh
tsr_table$Failure             <- tsr_table$newrel_fail     * 100 / tsr_table$newrel_coh
tsr_table$Died                <- tsr_table$newrel_died     * 100 / tsr_table$newrel_coh
tsr_table$`Lost to follow-up` <- tsr_table$newrel_lost     * 100 / tsr_table$newrel_coh
tsr_table$`Not evaluated`     <- tsr_table$c_newrel_neval  * 100 / tsr_table$newrel_coh

# Plot
tsr_table_melted <- melt(tsr_table[c(1, 9:13)], id=1)

txsucc <- ggplot(tsr_table_melted, aes(area, value, fill=variable)) +
  geom_bar(stat="identity", position="stack") +
  geom_hline(yintercept=85, color="grey70") +
  geom_text(data=subset(tsr_table_melted, variable=="Treatment success"), aes(label=round(value,0)), hjust=1.25, vjust=0.3, size=4, color="white") +
  theme_glb.rpt() + coord_flip() +
  scale_fill_brewer("", type = "qual", palette = 8) +
  labs(x="", y="Percentage of cohort") +
  theme(legend.position="bottom", panel.grid=element_blank()) + expand_limits(c(0,0)) +
  ggtitle(paste0("Treatment outcomes for new and relapse cases, ", report_year-2, ", globally, \nfor the six WHO regions and 22 high-burden countries"))

figsave(txsucc, tsr_table_melted, "3_5_txsucc")

# and now clear up the mess left behind
rm(list=c("tsr", "tsr_table", "tsr_table_melted", "txsucc"))


# B3_5_hiv_ts_d ---------------------------------------------------

# Remove non-HIV outcomes reporters (because otherwise we can't minus out the HIV)
# This should exclude those where ALL tbhiv outcomes have been assigned to 'not evaluated'
# (therefore make sure if tbhiv_coh > 0 then the not evaluated column is less than the total cohort )
hma2 <- subset(o, year==report_year-2 &
                 !is.na(tbhiv_succ) &
                 !is.na(newrel_succ) &
                 ( tbhiv_coh == 0 |
                     (tbhiv_coh > 0 & c_tbhiv_neval < tbhiv_coh)
                 ),
               c(country, year,
                 newrel_coh, newrel_succ, newrel_fail, newrel_died, newrel_lost, c_newrel_neval,
                 ret_nrel_coh, ret_nrel_succ, ret_nrel_fail, ret_nrel_died, ret_nrel_lost, c_ret_nrel_neval,
                 tbhiv_coh, tbhiv_succ, tbhiv_fail, tbhiv_died, tbhiv_lost, c_tbhiv_neval))

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

hmd <- hmd %>% rename("HIV+"=tbhiv)


hme <- melt(as.data.frame(hmd[-3]), id=1:2, variable_name = "type")

hmf <- cast(hme, ...~out)

hmf$`Treatment success` <- hmf$succ / hmf$coh * 100
hmf$Failed <- hmf$fail / hmf$coh * 100
hmf$Died <- hmf$died / hmf$coh * 100
hmf$`Lost to follow-up` <- hmf$lost / hmf$coh * 100
hmf$`Not evaluated` <- hmf$neval / hmf$coh * 100

hmg <- melt(as.data.frame(hmf[c(1:2,9:ncol(hmf))]), id=1:2) # It's a melt and cast fiesta!

hiv_ts_d <- ggplot(hmg, aes(variable, value, fill=type)) +
  geom_bar(stat="identity", position="dodge", width=0.5) + theme_glb.rpt() + labs(x="") +
  scale_y_continuous("Percentage of cohort", limits=c(0,100)) + scale_fill_brewer("", type = "qual", palette=6) +
  ggtitle(paste("Outcomes of TB treatment by HIV status, ", report_year-2, sep=""))

figsave(hiv_ts_d, hmg, "B3_5_hiv_ts_d")


# B3_2_notif_ind ----------------------------------------------------

inda <- n.t %>% filter(year>=1990, iso3=="IND") %>% select(iso3, year, c_newinc) %>% inner_join(subset(p, select=c(iso3, year, e_pop_num))) %>% mutate(c_newinc_100k=c_newinc/e_pop_num * 1e5)
  
B3_2_notif_ind <- ggplot(inda, aes(year, c_newinc/1e3, ymin=0)) + geom_line() + scale_x_continuous('') + ylab('Cases per year (thousands)')  + theme_glb.rpt() + ggtitle(paste0('Trends in case notification in India, 1990-', report_year-1))

figsave(B3_2_notif_ind, inda, "B3_2_notif_ind", width=6, height=6)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Chapter 6 ------
# Addressing the co-epidemics of TB and HIV
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 6_1_hivtest_graph -----------------------------------------------------------

gaa <- subset(tbhiv, year>=2004, select=c('g_whoregion', 'year', 'hivtest_pct_denominator', 'hivtest_pct_numerator'))

gaa$regional <- ifelse(gaa$g_whoregion=='AFR', 'African \nregion', 'Regions \noutside \nAfrica')

gabr <- aggregate(gaa[c('hivtest_pct_denominator', 'hivtest_pct_numerator')], by=list(area=gaa$regional, year=gaa$year), FUN=sum, na.rm=TRUE)
gabg <- aggregate(gaa[c('hivtest_pct_denominator', 'hivtest_pct_numerator')], by=list(year=gaa$year), FUN=sum, na.rm=TRUE)
gabg$area <- 'Global'

gac <- rbind(gabr, gabg)

gac$hivtest_pct <- gac$hivtest_pct_numerator / gac$hivtest_pct_denominator * 100

gadstart <- 2004

hivtest_graph <- ggplot(subset(gac, year >= gadstart), aes(year, hivtest_pct, colour=area)) +
  geom_line(size=1.5) +
  geom_text(data=subset(gac, year==max(gac$year)), aes(label = area), hjust=-.1, vjust=0, size=5) +
  scale_y_continuous(name = "Percentage of notified TB patients", limits=c(0,100), expand=c(0,0)) +
  scale_x_continuous("", labels=gadstart:(report_year-1), breaks=gadstart:(report_year-1)) +
  scale_color_brewer(name="WHO region", palette="Dark2") + expand_limits(x=c(gadstart, report_year+0.5)) +
  ggtitle(paste0('Percentage of notified TB patients with known HIV status, ', gadstart, "\u2013", report_year-1)) + theme_glb.rpt() + theme(legend.position="none")

# windows(11, 7); gad; dev.off()
figsave(hivtest_graph, gac, "6_1_hivtest_graph")


# 6_3_hivprog_graph_all ------------------------------------------------------
warning("I should come back to 6_3_hivprog_graph_all next year and make sure it still works.")
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
  ghc$year <- 2003:(report_year-1)
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

table(gl[!is.na(gl$c_notified_m),'year'])
table(gl[!is.na(gl$hivtest_m),'year'])
table(gl[!is.na(gl$hivtest_pos_m),'year'])

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

gaj['Percentages'] <- factor(gaj$var,
                             levels=c("pht", "phtp", "pcpt", "part"),
                             labels=c('% of notified TB patients with known HIV status',
                                      'Notified TB patients with known HIV status who were HIV-positive',
                                      'Notified HIV-positive TB patients \nstarted on CPT',
                                      'Notified HIV-positive TB patients \nstarted on ART'))

gak <- gaj[gaj$year>=2007 & gaj$Percentages!='% of notified TB patients with known HIV status',]

gak[c("best", "hi", "lo")] <- gak[c("best", "hi", "lo")] * 100

hivprog_graph_all <- ggplot(gak, aes(year, best)) +
  geom_line(size=1, alpha=.5) +
  geom_ribbon(aes (year, best, ymin=lo, ymax=hi), alpha=0.2) +
  facet_wrap(~Percentages, ncol=3) +
  scale_y_continuous(limits = c(0, 100), name = "Percentage of notified TB patients", breaks=c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous("") + theme_glb.rpt()  +
  ggtitle(paste0('Percentage of notified TB patients with known HIV status who were HIV positive, and percentage of notified \nHIV-positive TB patients enrolled on co-trimoxazole preventive therapy (CPT) and antiretroviral therapy (ART),\n ', min(gak$year), '\u2013', max(gak$year), '(a)'))

# Footnote: The solid lines show values for countries that reported data.
# The shaded areas show upper and lower limits when countries that did not report data are considered..

figsave(hivprog_graph_all, gak, "6_3_hivprog_graph_all")



# 6_4_hiv_art_etbhiv ----------------------------------------------------------

gca <- subset(tbhiv, year>=2004, select=c('iso3', 'year', 'hivtest_pos', 'hiv_art'))

gcb <- aggregate(gca[3:ncol(gca)], by=list(year=gca$year), FUN=sum, na.rm=T)

gcb1 <- merge(subset(araw, group_name=="global", c(year, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi, e_inc_tbhiv_num)), gcb)

gcc <- melt(gcb1, id=1:3)

gcc$value <- gcc$value/1000

# gcc1 <- cast()

gcc$variable <- factor(gcc$variable,
                       levels = c("e_inc_tbhiv_num", "hivtest_pos", "hiv_art"),
                       labels = c("Estimated HIV-positive incident TB cases",
                                  "Notified HIV-positive TB patients",
                                  "HIV-positive TB patients on ART"))

hiv_art_etbhiv <- ggplot(gcc, aes(year, value, color=variable)) +
  geom_ribbon(aes(year, ymin=e_inc_tbhiv_num_lo/1000, ymax=e_inc_tbhiv_num_hi/1000), alpha=0.4, fill=inch.color, color=NA) +
  geom_line(size=1) +
  scale_y_continuous("Number of TB patients (thousands)", limits=c(0,1500)) + theme_glb.rpt() +
  scale_x_continuous(name="") +  scale_colour_manual(name="", values=c(inch.color, "firebrick", "dark blue")) +
  geom_text(data=gcc[gcc$year==report_year-2, ], aes(label=variable), vjust=3, hjust=1)  +
  ggtitle(paste("ART enrolment among HIV-positive TB patients compared with the reported number of HIV-positive \nTB patients and the estimated number of HIV-positive people who developed TB(a), 2004", report_year-1, sep="\u2013")) + theme(legend.position="none")

# Footnote:
# Notified HIV-positive TB patients on ART includes new and relapse TB
# cases plus prevalent TB cases re-registered for treatment change
# (e.g. after treatment failure). Estimated HIV-positive incident TB cases
# includes only new and relapse TB cases.


figsave(hiv_art_etbhiv, gcc, "6_4_hiv_art_etbhiv")


# 6_6_hiv_ipt -----------------------------------------------------

gfa <- subset(n.t, year>=2005, select=c('iso3', "g_whoregion", 'year', 'hiv_ipt'))

# 2015 fix for Ethiopia
if(gfa[gfa$iso3=="ETH" & gfa$year==2014,"hiv_ipt"]==738) {
  gfa[gfa$iso3=="ETH" & gfa$year==2014,"hiv_ipt"] <- 10385 
  warning("Ethiopia IPT for 2014 has been modified.")
}

gfa$area <- ifelse(gfa$iso3 %in% c("ZAF"), "South Africa", ifelse(gfa$g_whoregion=="AFR", "Rest of AFR", "Rest of world"))


gfb <- aggregate(gfa[4], by=list(year=gfa$year, area=gfa$area), FUN=sum, na.rm=T)

gfb1 <- aggregate(gfa[4], by=list(year=gfa$year), FUN=sum, na.rm=T)
gfb1$area <- "Global"

gfb2 <- rbind(gfb, gfb1)

gfc <- melt(gfb2, id=1:2)

gfc$value <- gfc$value/1000

gfc$area <- factor(gfc$area, levels=c( "Rest of world", "Rest of AFR", "South Africa", "Global"))

# line option
hiv_ipt_graph <- ggplot(gfc, aes(year, value, color=area)) +
  geom_line(size=1) +
  scale_y_continuous("Number of people living with HIV (thousands)") + theme_glb.rpt() +
  scale_x_continuous(name="", breaks=c(min(gfc$year):max(gfc$year))) +
  scale_color_brewer(name="", palette="Dark2") + guides(color = guide_legend(reverse = TRUE)) +
  ggtitle(paste("Provision of isoniazid preventive therapy (IPT) to people living with HIV, 2005", report_year-1, sep="\u2013"))

figsave(hiv_ipt_graph, gfc, "6_6_hiv_ipt_graph")


## END ---------------------------------------
