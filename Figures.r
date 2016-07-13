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


if(flg_show_estimates){

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Chapter 2 ------
  # The burden of disease caused by TB
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # 2_3_incmort_num_glo ------------------------------------------------

  ima <- araw.t %>% filter(group_name=="global") %>% select(year, matches(".*inc.*num.*"), matches(".*mort.*num.*"))

  # Reshape
  imaf <- NULL
  for(var in c("inc", "inc_tbhiv", "mort_tbhiv", "mort_exc_tbhiv")){
    df <- subset(ima, select=c("year", paste0("e_", var, "_num"), paste0("e_", var, "_num_lo"), paste0("e_", var, "_num_hi")))
    names(df) <- c("year", "best", "lo", "hi")
    df[2:4] <- df[2:4] / 1e6
    df$var <- var
    imaf <- rbind(imaf, df)
  }

  incmort_num_glo1 <- imaf %>% filter(var %in% c("inc", "inc_tbhiv")) %>% ggplot(aes(year, best, fill=var, ymin=0)) + geom_line(size=1, aes(color=var)) + geom_ribbon(aes (year, best, ymin=lo, ymax=hi), alpha=0.2) + theme_glb.rpt() + theme(legend.position="none") + scale_x_continuous("", minor_breaks=seq(1990, 2015, 1)) + scale_y_continuous(breaks=pretty_breaks()) + ylab("Millions") + scale_color_manual(values = c(inc.color, inch.color)) + scale_fill_manual(values = c(inc.color, inch.color)) + geom_text(aes(2004,8.3, label="All TB cases")) + geom_text(aes(2004,2.3, label="HIV-positive TB cases")) + ggtitle("TB incidence")

  figsave(incmort_num_glo1, imaf, "f2_3_incmort_num_glo1", width=6, height=6)

  incmort_num_glo2 <- imaf %>% filter(var %in% c("mort_tbhiv", "mort_exc_tbhiv")) %>% ggplot(aes(year, best, fill=var, ymin=0)) + geom_line(size=1, aes(color=var)) + geom_ribbon(aes (year, best, ymin=lo, ymax=hi), alpha=0.2) + theme_glb.rpt() + theme(legend.position="none") + scale_x_continuous("", minor_breaks=seq(1990, 2015, 1)) + scale_y_continuous(breaks=pretty_breaks()) + ylab("Millions") + scale_color_manual(values = c(mort.color, inch.color)) + scale_fill_manual(values = c(mort.color, inch.color)) + geom_text(aes(1998,1.2, label="TB deaths among \nHIV-negative people")) + geom_text(aes(2004,0.7, label="TB deaths among HIV-positive people(a)")) + ggtitle("TB deaths")

  # Footnote:  "HIV-associated deaths are classified as HIV deaths according to ICD-10"

  figsave(incmort_num_glo2, imaf, "f2_3_incmort_num_glo2", width=6, height=6)

  # incmort_num <- arrangeGrob(incmort_num_glo1, incmort_num_glo2)

  # 2_4_topten ---------------------------------------------------------
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
      geom_pointrange(aes(ymin=var_lo, ymax=var_hi)) +
      theme_glb.rpt() +
      theme(plot.title = element_text(hjust = 0)) +
      expand_limits(y=0) +
      coord_flip()

    figsave(p, df, paste0("f2_4_topten_", var_name), width=5, height=4)
    return(p)
  }

  # Top 10 countries by incidence rates
  topten_e_inc_100k <- latest_estimates %>%
    select(country,
           var    = e_inc_100k,
           var_lo = e_inc_100k_lo,
           var_hi = e_inc_100k_hi) %>%
    arrange(desc(var)) %>%
    slice(1:10) %>%
    hplot_estimates_with_ui("Incidence: rates", "e_inc_100k", "Rate per 100 000 population per year")

  # Top 10 countries by incidence absolute number
  topten_e_inc_num <- latest_estimates %>%
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

  # 2_8_incprevmort_glo ----------------------------------


  eget <- c("year", "e_pop_num", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_prev_100k", "e_prev_100k_lo", "e_prev_100k_hi", "e_mort_exc_tbhiv_100k", "e_mort_exc_tbhiv_100k_lo", "e_mort_exc_tbhiv_100k_hi")

  esta <- subset(araw.t, year >=1990 & group_name=="global", select=eget)

  # estb <- n.t %>% filter(year>=1990) %>% select(year, c_newinc) %>%
    # group_by(year) %>% summarize(c_newinc=sum0(c_newinc))
  #   estc <- aggregate(estb["c_newinc"], by=list(year=estb$year), FUN=sum, na.rm=TRUE)

  # estd <- merge(esta, estb, all.x=TRUE)

  # estd$c_newinc_100k <- estd$c_newinc / estd$e_pop_num * 1e5

  # Reshape
  estf <- NULL
  for(var in c("inc", "inc_tbhiv", "prev", "mort_exc_tbhiv")){
    df <- subset(esta, select=c("year", paste0("e_", var, "_100k"), paste0("e_", var, "_100k_lo"), paste0("e_", var, "_100k_hi")))
    names(df) <- c("year", "best", "lo", "hi")
    df$var <- var
    estf <- rbind(estf, df)
  }

  # estg <- estd[c("year", "c_newinc_100k")]
  # estg$var <- "inc"

  # create panel grouping
  estf$panel <- ifelse(estf$var %in% c("inc", "inc_tbhiv"), "inc", estf$var)
  estf$panel <- factor(estf$panel, levels=c("inc", "prev", "mort_exc_tbhiv"), labels=c("Incidence", "Prevalence", "Mortality"))

  esti <- estf %>% group_by(var) %>% arrange(var, year) %>% mutate(target=ifelse(panel=="Incidence", as.numeric(NA), best[1]/2))

  incprevmort_glo <- ggplot(esti, aes(year, best, fill=var, ymin=0)) + geom_line(size=1, aes(color=var)) + geom_ribbon(aes (year, best, ymin=lo, ymax=hi), alpha=0.2) + geom_hline(aes(yintercept=target), linetype=2) + theme_glb.rpt() + facet_wrap(~panel, scales="free_y") + theme(legend.position="none") + scale_x_continuous("", minor_breaks=seq(1990, 2015, 1)) + scale_y_continuous(breaks=pretty_breaks()) + ylab("Rate per 100 000 population per year") +
    scale_color_manual(values = c(inc.color, inch.color, mort.color, prev.color)) + scale_fill_manual(values = c(inc.color, inch.color, mort.color, prev.color)) +
    ggtitle(paste0("
Figure 2.8 Global trends in estimated rates of TB incidence (1990–", report_year-1, ",
and prevalence and mortality rates (1990–", report_year, "). Left: Estimated incidence rate including
HIV-positive TB (", inc.color, ") and estimated incidence rate of
HIV-positive TB (", inch.color,"). Centre and right: The
horizontal dashed lines represent the Stop TB Partnership targets
of a 50% reduction in prevalence and mortality rates by 2015
compared with 1990. Shaded areas represent uncertainty bands.
Mortality excludes TB deaths among HIV-positive people."))

  figsave(incprevmort_glo, esti, "f2_8_incprevmort_glo")

  # 2_7_incdist ----------------------------------


  inca <- eraw.t %>% filter(year==report_year-1, !is.na(e_inc_num)) %>% select(year, g_hbc22, iso3, country, e_inc_num, e_inc_100k, e_pop_num) %>% mutate(name=ifelse(e_inc_num > 1e5 | e_inc_100k > 450 | g_hbc22=="high", iso3, NA)) %>% mutate(hbc=ifelse(g_hbc22=="high", "High-burden \ncountries", NA))

  incb <- araw.t %>% filter(year==report_year-1, group_type=="g_whoregion") %>% select(year, group_name, e_inc_100k, e_inc_num, e_pop_num) %>% .shortnames(col = "group_name") %>% mutate(name=group_name)

    incdist <- ggplot(inca, aes(e_inc_100k, e_inc_num/1e3, color=hbc, size=e_pop_num/1e6)) + geom_point() + theme_glb.rpt() + geom_text(aes(label=name), size=2.5, vjust=-1.2) + labs(x="Rate per 100 000 population per year", y="Cases per year (thousands)", color="22 High-burden \ncountries", size="Population\n(millions)", title=paste0("Figure 2.7 Global distribution of estimated TB incidence by rate and absolute number, ", max(inca$year), ". The size of each \nbubble is proportional to the size of the country's population. High-burden countries are shown in red.")) + scale_x_continuous(breaks=seq(0,900,100))

  incdistb <- ggplot(incb, aes(e_inc_100k, e_inc_num/1e3, size=e_pop_num/1e6, ymin=0, xmin=0)) + geom_point() + theme_glb.rpt() + geom_text(aes(label=name), size=2.5, vjust=2.3) + labs(x="Rate per 100 000 population per year", y="Cases per year (thousands)", color="22 High-burden \ncountries", size="Population\n(millions)", title="WHO region")


  figsave(incdist, inca, "f2_7_incdist")
  figsave(incdistb, incb, "f2_7_incdistb", width = 5, height=4)

  # 2_9_inc_reg ------------------------------------------------------

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
    ggtitle(paste0("Figure 2.9 Estimated TB incidence rates by WHO region, 1990-", report_year-1, ". \nEstimated TB incidence rates (green) and estimated incidence rates of HIV-positive TB (red). \nShaded areas represent uncertainty bands."))

  figsave(inc_reg, reg_aggs, "f2_9_inc_reg")

  # and now clear up the mess left behind
#   rm(list=c("inc_reg", "reg_aggs"))


  # 2_10_inc_hbc -----------------------------------------------------

  # Get incidence estimates for the HBCs
  inc_hbc_data <- eraw.t %>%
    filter(g_hbc22=="high" & year < report_year) %>%
    select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
           e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi)  %>%
    .shortnames() %>% mutate(country=ifelse(country=="Bangladesh", "Bangladesh(a)", country))


  inc_hbc1 <- qplot(year, e_inc_100k, data=inc_hbc_data, geom='line', colour=I(inc.color)) +
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
    ggtitle(paste0("Figure 2.10 Estimated TB incidence rates, 22 high-burden countries, 1990-", report_year-1, ". \nEstimated TB incidence rates (green) and estimated incidence rates of HIV-positive TB (red). \nShaded areas represent uncertainty bands."))

  # Put back x-axis (weird ggplot problem because facets don;t fit page exactly...)
  inc_hbc <- facetAdjust(inc_hbc1)

  figsave(inc_hbc, inc_hbc_data, "f2_10_inc_hbc")

  # and now clear up the mess left behind
#   rm(list=c("inc_hbc_data", "inc_hbc"))



  # 2_13_prev_reg ----------------------------------------------------

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
    ylab('Rate per 100 000 population') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position="none") +
    ggtitle(paste0("Figure 2.13 Estimated TB prevalence rates 1990-", report_year, ", by WHO region. \nShaded areas represent uncertainty bands. The horizontal dashed lines represent the Stop TB Partnership \ntarget of a 50% reduction in the prevalence rate by 2015 compared with 1990."))

  figsave(prev_reg, prev_mort_reg_aggs, "f2_13_prev_reg")

  # 2_14_prev_hbc ----------------------------------------------------

  # Get prevalence estimates for the HBCs
  prev_hbc_data <- eraw.t %>%
    filter(g_hbc22=="high") %>%
    select(country, iso2, year, e_prev_100k, e_prev_100k_lo, e_prev_100k_hi) %>% arrange(iso2, year) %>% group_by(iso2) %>% mutate(target.prev=e_prev_100k[1]/2)  %>% # Add variable for STP target prevalence rates (half of 1990 values) to be displayed as dashed lines
    .shortnames() %>% mutate(country=ifelse(country=="Bangladesh", "Bangladesh(a)", country))

  prev_hbc_data$forecast <- ifelse(prev_hbc_data$year >= report_year, "forecast", "current")


  prev_hbc <- qplot(year, e_prev_100k, data=prev_hbc_data, geom='line', colour=I(prev.color)) +
    geom_ribbon(aes(year,
                    ymin=e_prev_100k_lo,
                    ymax=e_prev_100k_hi), fill=I(prev.color), alpha=0.4) +
    geom_hline(aes(yintercept=target.prev), linetype=2) +
    facet_wrap(~country, scales='free_y') +
    xlab("") + ylab('Rate per 100 000 population') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position='none') +
    ggtitle(paste0("Figure 2.14 Estimated TB prevalence rates 1990-", report_year, ", 22 high-burden countries. \nShaded areas represent uncertainty bands. The horizontal dashed lines represent the Stop TB Partnership \ntarget of a 50% reduction in the prevalence rate by 2015 compared with 1990."))


  figsave(prev_hbc, prev_hbc_data, "f2_14_prev_hbc")


  # 2_18_mort_reg ----------------------------------------------------

  mort_reg <- ggplot(prev_mort_reg_aggs,
                     aes(year, e_mort_exc_tbhiv_100k)) +
    geom_line(colour=I(mort.color)) +
    geom_ribbon(aes(year,
                    ymin=e_mort_exc_tbhiv_100k_lo,
                    ymax=e_mort_exc_tbhiv_100k_hi), fill=I(mort.color), alpha=0.4) +
    facet_wrap(~g_whoregion, scales='free_y') +
    geom_hline(aes(yintercept=target.mort), linetype=2) +
    scale_x_continuous('') +
    ylab('Rate per 100 000 population per year') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position="none") +
    ggtitle(paste0(
"Figure 2.18 Estimated TB mortality rates 1990-", report_year, ",
by WHO region. Estimated TB mortality excludes TB deaths among
HIV-positive people. Shaded areas represent uncertainty bands.(a)
The horizontal dashed lines represent the Stop TB Partnership
target of a 50% reduction in the mortality rate by 2015 compared
with 1990."))

  figsave(mort_reg, prev_mort_reg_aggs, "f2_18_mort_reg")

  # and now clear up the mess left behind
#   rm(list=c("prev_mort_reg_aggs", "prev_reg", "mort_reg"))




  # 2_19_mort_hbc ---------------------------------------------------------


# Get mortality estimates for the HBCs
  mort_hbc_data <- eraw.t %>%
    filter(g_hbc22=="high") %>%
    select(country, iso2, year, e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi, vr.raw) %>% arrange(iso2, year) %>% group_by(iso2) %>% mutate(target.mort=e_mort_exc_tbhiv_100k[1]/2)  %>% # Add variable for STP target prevalence rates (half of 1990 values) to be displayed as dashed lines
    .shortnames() %>% mutate(country=ifelse(country=="Bangladesh", "Bangladesh(b)", country))

  mort_hbc_data$forecast <- ifelse(mort_hbc_data$year >= report_year, "forecast", "current")


  mort_hbc <- qplot(year, e_mort_exc_tbhiv_100k, data=mort_hbc_data, geom='line', colour=I(mort.color)) +
    geom_ribbon(aes(year,
                    ymin=e_mort_exc_tbhiv_100k_lo,
                    ymax=e_mort_exc_tbhiv_100k_hi), fill=I(mort.color), alpha=0.4) +
    geom_hline(aes(yintercept=target.mort), linetype=2) +
    facet_wrap(~country, scales='free_y') +
    geom_point(aes(year, vr.raw), shape=I(4)) +
    xlab("") + ylab('Rate per 100 000 population per year') +
    expand_limits(y=0) +
    theme_glb.rpt() +
    theme(legend.position='none') +
    ggtitle(paste0(
"Figure 2.19 Estimated TB mortality rates 1990-", report_year, ",
22 high-burden countries. Estimated TB mortality excludes TB deaths
among HIV-positive people. The horizontal dashed lines represent
the Stop TB Partnership target of a 50% reduction in the mortality
rate by 2015 compared with 1990. Uncertainty is due to adjustments
made to the mortality data from vital registration systems that
were reported by countries(a) (mortality data from vital registration
systems are represented by the 'x' symbol)."))

  # Add footnote
  # This currently breaks ggsave, but in a near release it should be fixed.
  # mort_hbc <- arrangeGrob(mort_hbc, sub = textGrob("(a) The width of an uncertainty band narrows as the quality and completeness of vital registration data improves.", x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))

  # mort_hbc <- facetAdjust(mort_hbc)
  # It looks like you can have one tweak or another, but not both. (X-axis on orphaned panels or footnote.)

  figsave(mort_hbc, mort_hbc_data, "f2_19_mort_hbc")

  # and now clear up the mess left behind
#   rm(list=c("mort_hbc_data", "mort_hbc"))


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
# Chapter 4 ------
# Diagnosis and treatment of TB, HIV-associated TB and drug-resistant TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.1 New and relapse TB case notification rates by age group and sex,  all WHO regions and global, 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Get data ----
agesex_notifs <- filter(notification, year == (report_year - 1)) %>%
                 select(iso2,
                        g_whoregion,
                        newrel_m014, newrel_m1524, newrel_m2534, newrel_m3544, newrel_m4554, newrel_m5564, newrel_m65,
                        newrel_f014, newrel_f1524, newrel_f2534, newrel_f3544, newrel_f4554, newrel_f5564, newrel_f65)


agesex_pop    <- filter(estimates_population, year == (report_year - 1)) %>%
                 select(iso2,
                        e_pop_m014, e_pop_m1524, e_pop_m2534, e_pop_m3544, e_pop_m4554, e_pop_m5564, e_pop_m65,
                        e_pop_f014, e_pop_f1524, e_pop_f2534, e_pop_f3544, e_pop_f4554, e_pop_f5564, e_pop_f65)

agesex        <- merge(agesex_notifs, agesex_pop)

rm(list=c("agesex_notifs","agesex_pop"))


# Drop numbers from countries who didn't report cases in the 10-year age intervals for ages 15 and above

agesex$tot_notified <- sum_of_row(agesex[c("newrel_m1524", "newrel_m2534", "newrel_m3544", "newrel_m4554", "newrel_m5564", "newrel_m65",
                                           "newrel_f1524", "newrel_f2534", "newrel_f3544", "newrel_f4554", "newrel_f5564", "newrel_f65")] )


agesex_filtered_agg <- agesex %>%
                    filter(!is.na(tot_notified)) %>%
                    group_by(g_whoregion) %>%
                    summarise_each(funs(sum(.,na.rm = TRUE)),
                                   newrel_m014:tot_notified) %>%

                    # Calculate rate per 100 000 population for each age/sex group
                    mutate(m014 = newrel_m014 * 1e5 / e_pop_m014,
                           m1524 = newrel_m1524 * 1e5 / e_pop_m1524,
                           m2534 = newrel_m2534 * 1e5 / e_pop_m2534,
                           m3544 = newrel_m3544 * 1e5 / e_pop_m3544,
                           m4554 = newrel_m4554 * 1e5 / e_pop_m4554,
                           m5564 = newrel_m5564 * 1e5 / e_pop_m5564,
                           m65 = newrel_m65 * 1e5 / e_pop_m65,

                           f014 = newrel_f014 * 1e5 / e_pop_f014,
                           f1524 = newrel_f1524 * 1e5 / e_pop_f1524,
                           f2534 = newrel_f2534 * 1e5 / e_pop_f2534,
                           f3544 = newrel_f3544 * 1e5 / e_pop_f3544,
                           f4554 = newrel_f4554 * 1e5 / e_pop_f4554,
                           f5564 = newrel_f5564 * 1e5 / e_pop_f5564,
                           f65 = newrel_f65 * 1e5 / e_pop_f65
                           ) %>%

                    # select only the rates
                    select(g_whoregion,
                           m014, m1524, m2534, m3544, m4554, m5564, m65,
                           f014, f1524, f2534, f3544, f4554, f5564, f65) %>%

                    # merge with regional names
                    inner_join(who_region_names, by = "g_whoregion") %>%
                    select(-g_whoregion)

# Switch to long ('tidy') format to plot
agesex_agg_long <- agesex_filtered_agg %>%
                      gather("group", "rate", 1:14)


# Extract sex and agegroup from the group field
agesex_agg_long$sex <- str_extract(agesex_agg_long$group, "f|m")
agesex_agg_long$sex <- factor(agesex_agg_long$sex,
                                   levels=c("f", "m"),
                                   labels=c("Female", "Male"))
agesex_agg_long$agegroup <- str_extract(agesex_agg_long$group, "014|1524|2534|3544|4554|5564|65")
agesex_agg_long$agegroup <- factor(agesex_agg_long$agegroup,
                                   levels=c("014", "1524", "2534", "3544", "4554", "5564", "65"),
                                   labels=c("0\u201314", "15\u201324", "25\u201334", "35\u201344", "45\u201354", "55\u201364", "\u226565"))

agesex_agg_long <- agesex_agg_long %>% select(-group)


# Now plot the aggregates

# A. Plot as lines (similar to 2015 global report)
agesex_plotA <- agesex_agg_long %>%
                ggplot(aes(x=agegroup, y=rate, colour=sex, group=sex)) +
                  geom_line(size=1) +
                  scale_y_continuous(name = "TB case notification rate per 100 000 population per year") +
                  scale_x_discrete("Age group (years)",
                                   labels=levels(agesex_agg_long$agegroup)) +
                  facet_wrap( ~ entity) +
                  ggtitle(paste0("Figure 4.1 New and relapse TB case notification rates by age group and sex, all WHO regions, ",
                               report_year-1,
                               "(a)")) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())


# B. Plot as pyramids
# See first code example at https://github.com/JuanGaleano/Population-pyramids-ggplot2/blob/master/POPULATION%20PYRAMID%20GGPLOT2.R

agesex_plotB <- agesex_agg_long %>%
                # Multiply all the female rates by -1
                mutate(rate = ifelse(sex=="Female", rate * -1, rate )) %>%
                ggplot(aes(x=agegroup, y=rate, fill=sex)) +
                geom_bar(stat="identity",
                         size=.3,
                         colour="black",
                         position="identity") +
                scale_y_continuous(name = "TB case notification rate per 100 000 population per year") +
                scale_x_discrete("Age group (years)",
                                 labels=levels(agesex_agg_long$agegroup)) +
                coord_flip() +
                facet_wrap( ~ entity) +
                ggtitle(paste0("Figure 4.1 New and relapse TB case notification rates by age group and sex, all WHO regions, ",
                               report_year-1,
                               "(a)")) +
                theme_glb.rpt() +
                theme(legend.position="top",
                      legend.title=element_blank())


# Add footnote
agesex_pcnt <- rounder(sum(agesex$tot_notified, na.rm=TRUE)  * 100
                       /
                       sum(notification[notification$year==report_year -1, "c_newinc"], na.rm=TRUE))

agesex_foot <- paste0("(a) Countries not reporting cases in these categories are excluded. Cases included make up ",
                      agesex_pcnt,
                      "% of reported cases.")




agesex_plotA <- arrangeGrob(agesex_plotA, bottom = textGrob(agesex_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))
agesex_plotB <- arrangeGrob(agesex_plotB, bottom = textGrob(agesex_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plots
figsave(agesex_plotA, agesex_agg_long, "f4_1_agesex_plotA")
figsave(agesex_plotB, agesex_agg_long, "f4_1_agesex_plotB")

# Clean up (remove any objects with their name beginning with 'agesex')
rm(list=ls(pattern = "^agesex"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.4 Percentage of new and relapse pulmonary TB cases with bacteriological confirmation,  all WHO regions and global, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bacconf_data <- notification %>%
                filter(year >= 2009) %>%
                select(iso3,
                       year,
                       g_whoregion,
                       # old variables pre-2013
                       new_sp,
                       new_sn,
                       new_su,
                       # new variables
                       new_labconf, new_clindx,
                       ret_rel_labconf, ret_rel_clindx) %>%
                group_by(year, g_whoregion) %>%
                summarise_each(funs(sum(.,na.rm = TRUE)),
                               new_sp:ret_rel_clindx) %>%

                #calculate % of pulmonary cases with bac confirmation
                # a bit tricky for years before 2013, so do for new only by smear only
                mutate(bacconf_pct = ifelse(year < 2013 & g_whoregion != 'EUR',
                                            # old variables, do for new only outside EUR
                                            new_sp * 100 / (new_sp + new_sn + new_su),
                                            # new variables
                                           (new_labconf + ret_rel_labconf) * 100
                                            /
                                            (new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx))) %>%

                # Adjust calculation for EUR pre-2013
                 mutate(bacconf_pct = ifelse(year < 2013 & g_whoregion == 'EUR',
                                            # old variables, but using new_labconf
                                            new_labconf * 100 / (new_sp + new_sn + new_su),
                                            # otherwise keep calculation from previous step
                                            bacconf_pct)) %>%


                # get rid of raw totals
                select(g_whoregion,
                       year,
                       bacconf_pct) %>%

                # merge with regional names
                inner_join(who_region_names, by = "g_whoregion") %>%
                select(-g_whoregion)


# Plot as lines
bacconf_plot <- bacconf_data %>%
                ggplot(aes(x=year, y=bacconf_pct)) +
                  geom_line(size=1) +
                  scale_y_continuous(name = "% bacteriologically confirmed") +
                  expand_limits(y=c(0,100)) +
                  xlab("Year") +
                  #scale_x_discrete(name = "Year") +
                  facet_wrap( ~ entity) +
                  ggtitle(paste0("Figure 4.4 Percentage of new and relapse pulmonary TB cases with bacteriological confirmation, all WHO regions(a), 2009 - ",
                               report_year-1)) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
bacconf_foot <- "(a) The calculation is for new pulmonary cases only in years prior to 2013 based on smear results only, except for the European Region\nwhere data on confirmation by culture was also available."

bacconf_plot <- arrangeGrob(bacconf_plot, bottom = textGrob(bacconf_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(bacconf_plot, bacconf_data, "f4_4_bacconf_plot")

# Clean up (remove any objects with their name beginning with 'bacconf')
rm(list=ls(pattern = "^bacconf"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.6 Percentage of new and relapse TB cases with documented HIV status,  all WHO regions and global, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


hivstatus_data <- TBHIV_for_aggregates %>%
                  filter(year >= 2009) %>%
                  select(g_whoregion,
                         year,
                         hivtest_pct_numerator,
                         hivtest_pct_denominator) %>%
                  group_by(year, g_whoregion) %>%
                  summarise_each(funs(sum(.,na.rm = TRUE)),
                                 hivtest_pct_numerator:hivtest_pct_denominator) %>%

                  # Calculate % with known HIV status
                  mutate(hivstatus_pct = hivtest_pct_numerator * 100
                                        / hivtest_pct_denominator) %>%

                  # get rid of raw totals
                  select(g_whoregion,
                         year,
                         hivstatus_pct) %>%

                  # merge with regional names
                  inner_join(who_region_names, by = "g_whoregion") %>%
                  select(-g_whoregion)

# Plot as lines
hivstatus_plot <- hivstatus_data %>%
                  ggplot(aes(x=year, y=hivstatus_pct)) +
                  geom_line(size=1) +
                  scale_y_continuous(name = "% with documented status") +
                  expand_limits(y=c(0,100)) +
                  xlab("Year") +
                  #scale_x_discrete(name = "Year") +
                  facet_wrap( ~ entity) +
                  ggtitle(paste0("Figure 4.6 Percentage of new and relapse TB cases with documented HIV status, all WHO regions (a), 2009 - ",
                               report_year-1)) +
                  theme_glb.rpt() +
                  theme(legend.position="top",
                        legend.title=element_blank())

# Add footnote
hivstatus_foot <- "(a) The calculation is for all cases in years prior to 2015."

hivstatus_plot <- arrangeGrob(hivstatus_plot, bottom = textGrob(hivstatus_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(hivstatus_plot, bacconf_data, "f4_6_hivstatus_plot")

# Clean up (remove any objects with their name beginning with 'hivstatus')
rm(list=ls(pattern = "^hivstatus"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.10i Number of new and relapse cases notified and estimated number of TB incidence cases, global, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

newinc_data <- notification %>%
                filter(year >= 2009) %>%
                select(year, c_newinc) %>%
                group_by(year) %>%
                summarise_each(funs(sum(.,na.rm = TRUE)),
                               c_newinc) %>%
                mutate(c_newinc = c_newinc / 1e6)

inc_data <- aggregated_estimates_epi_rawvalues %>%
            filter( year >= 2009 & group_name == 'global') %>%
            select(year,
                   e_inc_num,
                   e_inc_num_lo,
                   e_inc_num_hi) %>%
            mutate(e_inc_num = e_inc_num / 1e6,
                   e_inc_num_lo = e_inc_num_lo / 1e6,
                   e_inc_num_hi = e_inc_num_hi / 1e6) %>%
            # Use a right-join so can see the data for the final year even in the absence of estimates
            right_join(newinc_data)

# Plot as lines
inc_plot <- inc_data %>%
            ggplot(aes(x=year, y=c_newinc, ymin=0)) +
            geom_line(size=1) +
            geom_ribbon(aes(x=year, ymin=e_inc_num_lo, ymax=e_inc_num_hi),
                        fill=I('#00FF33'),
                        alpha=0.4) +
            geom_line(aes(year, e_inc_num),
                      size=1,
                      colour=I('#00FF33')) +

            scale_y_continuous(name = "New and relapse cases per year (millions)") +
            xlab("Year") +

            ggtitle(paste0("Figure 4.10i Number of new and relapse cases notified and estimated number of TB incidence cases, global, 2009 - ",
                         report_year-1)) +
            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsave(inc_plot, inc_data, "f4_10i_inc_plot")

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.10ii Number of new and relapse cases  known to be HIV-positive,
# number started on ART and estimated number of incident HIV-positive TB cases, global, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tbhiv_data <- TBHIV_for_aggregates %>%
              filter(year >= 2009) %>%
              select(year,
                     hivtest_pos,
                     hiv_art,
                     # new variables from 2015 onwards
                     newrel_hivpos,
                     newrel_art) %>%
              group_by(year) %>%
              summarise_each(funs(sum(.,na.rm = TRUE)),
                             hivtest_pos:newrel_art) %>%

              # Convert to millions and merge pre/post 2014 variables
              mutate(hivtest_pos = ifelse(year < 2015,
                                          hivtest_pos / 1e6,
                                          newrel_hivpos / 1e6),
                     hiv_art = ifelse(year < 2015,
                                      hiv_art / 1e6,
                                      newrel_art / 1e6)) %>%
              select(year,
                     hivtest_pos,
                     hiv_art)


inctbhiv_data <- aggregated_estimates_epi_rawvalues %>%
                  filter( year >= 2009 & group_name == 'global') %>%
                  select(year,
                         e_inc_tbhiv_num,
                         e_inc_tbhiv_num_lo,
                         e_inc_tbhiv_num_hi) %>%
                  mutate(e_inc_tbhiv_num = e_inc_tbhiv_num / 1e6,
                         e_inc_tbhiv_num_lo = e_inc_tbhiv_num_lo / 1e6,
                         e_inc_tbhiv_num_hi = e_inc_tbhiv_num_hi / 1e6) %>%

                  # Use a right-join so can see the data for the final year even in the absence of estimates
                  right_join(tbhiv_data)


# Plot as lines
inctbhiv_plot <- inctbhiv_data %>%
                  ggplot(aes(x=year, y=hivtest_pos, ymin=0)) +
                  geom_line(size=1) +
                  geom_ribbon(aes(x=year, ymin=e_inc_tbhiv_num_lo, ymax=e_inc_tbhiv_num_hi),
                              fill="red",
                              alpha=0.4) +
                  geom_line(aes(year, e_inc_tbhiv_num),
                            size=1,
                            colour="red") +

                  geom_line(aes(year, hiv_art),
                            size=1,
                            colour="blue") +

                  scale_y_continuous(name = "New and relapse cases per year (millions)") +
                  xlab("Year") +

                  ggtitle(paste0("Figure 4.10ii Number of new and relapse cases(a) known to be HIV-positive,\nnumber started on ART and estimated number of incident HIV-positive TB cases, global, 2009 - ",
                               report_year-1)) +
                  theme_glb.rpt()


# Add footnote
inctbhiv_foot <- "(a) The calculation is for all cases in years prior to 2015."

inctbhiv_plot <- arrangeGrob(inctbhiv_plot, bottom = textGrob(inctbhiv_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(inctbhiv_plot, inctbhiv_data, "f4_10ii_inctbhiv_plot")

# Clean up (remove any objects with their name containing 'tbhiv')
rm(list=ls(pattern = "tbhiv"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.11 Number of new and relapse cases notified and estimated number of TB incidence cases,
# 30 high TB burden countries, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

newinc_30hbc <- report_country %>%
                filter(g_hb_tb==1) %>%
                select(iso2)

newinc_data <- notification %>%
                filter(year >= 2009) %>%
                inner_join(newinc_30hbc) %>%
                select(year,
                       iso2,
                       country,
                       c_newinc) %>%
                mutate(c_newinc = c_newinc / 1e3)

inc_data <- estimates_epi_rawvalues %>%
            filter(year >= 2009) %>%
            inner_join(newinc_30hbc) %>%
            select(year,
                   iso2,
                   e_inc_num,
                   e_inc_num_lo,
                   e_inc_num_hi) %>%
            mutate(e_inc_num = e_inc_num / 1e3,
                   e_inc_num_lo = e_inc_num_lo / 1e3,
                   e_inc_num_hi = e_inc_num_hi / 1e3) %>%
            # Use a right-join so can see the data for the final year even in the absence of estimates
            right_join(newinc_data)

# Plot as lines
inc_plot <- inc_data %>%
            ggplot(aes(x=year, y=c_newinc, ymin=0)) +
            geom_line(size=1) +
            geom_ribbon(aes(x=year, ymin=e_inc_num_lo, ymax=e_inc_num_hi),
                        fill=I('#00FF33'),
                        alpha=0.4) +
            geom_line(aes(year, e_inc_num),
                      size=1,
                      colour=I('#00FF33')) +

            scale_y_continuous(name = "New and relapse cases per year (thousands)") +
            xlab("Year") +

            facet_wrap( ~ country,
                        scales = "free_y") +

            ggtitle(paste0("Figure 4.11 Number of new and relapse cases notified and estimated number of TB incidence cases,\n30 high TB burden countries, 2009 - ",
                         report_year-1)) +
            theme_glb.rpt() +
            theme(legend.position="top",
                  legend.title=element_blank())

# Save the plot
figsave(inc_plot, inc_data, "f4_11_inc_plot_hbc")

# Clean up (remove any objects with their name containing 'inc_')
rm(list=ls(pattern = "inc_"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Figure 4.12 Number of new and relapse cases known to be HIV-positive,
# number started on ART and estimated number of incident HIV-positive TB cases,
# 30 high TB/HIV burden countries, 2009-2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tbhiv_30hbc <- report_country %>%
                filter(g_hb_tbhiv==1) %>%
                select(iso2)

tbhiv_data <- TBHIV_for_aggregates %>%
              filter(year >= 2009) %>%
              inner_join(tbhiv_30hbc) %>%
              select(year,
                     iso2,
                     country,
                     hivtest_pos,
                     hiv_art,
                     # new variables from 2015 onwards
                     newrel_hivpos,
                     newrel_art) %>%

              # Convert to thousands and merge pre/post 2014 variables
              mutate(hivtest_pos = ifelse(year < 2015,
                                          hivtest_pos / 1e3,
                                          newrel_hivpos / 1e3),
                     hiv_art = ifelse(year < 2015,
                                      hiv_art / 1e3,
                                      newrel_art / 1e3)) %>%
              select(year,
                     iso2,
                     country,
                     hivtest_pos,
                     hiv_art)

inctbhiv_data <- estimates_epi_rawvalues %>%
                  filter(year >= 2009) %>%
                  inner_join(tbhiv_30hbc) %>%
                  select(year,
                         iso2,
                         e_inc_tbhiv_num,
                         e_inc_tbhiv_num_lo,
                         e_inc_tbhiv_num_hi) %>%
                  mutate(e_inc_tbhiv_num = e_inc_tbhiv_num / 1e3,
                         e_inc_tbhiv_num_lo = e_inc_tbhiv_num_lo / 1e3,
                         e_inc_tbhiv_num_hi = e_inc_tbhiv_num_hi / 1e3) %>%

                  # Use a right-join so can see the data for the final year even in the absence of estimates
                  right_join(tbhiv_data)


# Plot as lines
inctbhiv_plot <- inctbhiv_data %>%
                  ggplot(aes(x=year, y=hivtest_pos, ymin=0)) +
                  geom_line(size=1) +
                  geom_ribbon(aes(x=year, ymin=e_inc_tbhiv_num_lo, ymax=e_inc_tbhiv_num_hi),
                              fill="red",
                              alpha=0.4) +
                  geom_line(aes(year, e_inc_tbhiv_num),
                            size=1,
                            colour="red") +

                  geom_line(aes(year, hiv_art),
                            size=1,
                            colour="blue") +

                  scale_y_continuous(name = "New and relapse cases per year (thousands)") +
                  xlab("Year") +

                  facet_wrap( ~ country,
                              scales = "free_y") +

                  ggtitle(paste0("Figure 4.12 Number of new and relapse cases(a) known to be HIV-positive,\nnumber started on ART and estimated number of incident HIV-positive TB cases,\n30 high TB/HIV burden countries, 2009 - ",
                               report_year-1)) +
                  theme_glb.rpt()


# Add footnote
inctbhiv_foot <- "(a) The calculation is for all cases in years prior to 2015."

inctbhiv_plot <- arrangeGrob(inctbhiv_plot, bottom = textGrob(inctbhiv_foot, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


# Save the plot
figsave(inctbhiv_plot, inctbhiv_data, "f4_12_inctbhiv_plot_hbc")

# Clean up (remove any objects with their name containing 'tbhiv')
rm(list=ls(pattern = "tbhiv"))




stop("

     >>>>>>>>>>
     Stopping here so can do the rest manually!
     <<<<<<<<<<<<")


if(flg_show_estimates){

  # Global rates of incidence, and notifications
  # 3_1_inc_notif_glo ----------------------------------------------------

  eha <- subset(araw.t, group_type=="global")

  # Absolute number

  ehba <- merge(subset(eha, select=c("group_name", "year", "e_inc_num", "e_inc_num_lo", "e_inc_num_hi", "e_inc_tbhiv_num", "e_inc_tbhiv_num_lo", "e_inc_tbhiv_num_hi", "e_pop_num")), aggregate(n.t['c_newinc'], by=list(year=n.t$year), FUN=sum, na.rm=TRUE))

#   ehba$c_newinc_mil <- ehba$c_newinc / 1e6

  inc_notif_gloa <- ggplot(ehba, aes(year, e_inc_num/1e6, ymin=0)) + geom_line(colour=I('#00FF33')) +
    geom_ribbon(aes(year, ymin=e_inc_num_lo/1e6, ymax=e_inc_num_hi/1e6), fill=I('#00FF33'), alpha=0.4) +
    geom_line(aes(year, c_newinc/1e6)) + scale_x_continuous('') +
    ylab('Cases per year (millions)')  + theme_glb.rpt() +
    ggtitle(paste0('Global trends in absolute number of notified TB cases (black) and \nestimated TB incidence (green), 1990-', report_year-1, ". \nCase notifications include new and relapse cases (all forms)."))

  figsave(inc_notif_gloa, ehba, "f3_1a_inc_notif_glo", width=6, height=6)

  # Rates

  ehb <- merge(subset(eha, select=c("group_name", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), aggregate(n.t['c_newinc'], by=list(year=n.t$year), FUN=sum, na.rm=TRUE))

  ehb$newrel_100k <- ehb$c_newinc / ehb$e_pop_num * 100000

  inc_notif_glob <- ggplot(ehb, aes(year, e_inc_100k, ymin=0)) + geom_line(colour=I('#00FF33')) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I('#00FF33'), alpha=0.4) +
    geom_line(aes(year, newrel_100k)) + scale_x_continuous('') +
    ylab('Rate per 100 000 population per year')  + theme_glb.rpt() +
    ggtitle(paste0('Global trends in case notification (black) and estimated TB \nincidence (green) rates, 1990-', report_year-1, ". \nCase notifications include new and relapse cases (all forms)."))

  figsave(inc_notif_glob, ehb, "f3_1b_inc_notif_glo", width=6, height=6)


  # Regional rates of incidence, and notifications
  # 3_2_inc_notif_reg ----------------------------------------------------

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
    ggtitle(paste0('Case notification and estimated TB incidence rates by WHO region, 1990-', report_year-1, '. \nRegional trends in case notification rates (new and relapse cases, all forms) (black) \nand estimated TB incidence rates (green). \nShaded areas represent uncertainty bands.'))

  figsave(inc_notif_reg, efa, "f3_2_inc_notif_reg")


  # HBC rates of incidence, and notifications
  # 3_3_inc_notif_hbc ----------------------------------------------------

  ega <- subset(merge(eraw.t, e.t[e.t$year==report_year-1,c("country", "g_hbc22")]), g_hbc22=="high" & year < report_year) # This hack is until we add g_hbc22 to eraw.

  egb <- .shortnames(merge(subset(ega, select=c("country", "g_whoregion", "year", "e_inc_100k", "e_inc_100k_lo", "e_inc_100k_hi", "e_inc_tbhiv_100k", "e_inc_tbhiv_100k_lo", "e_inc_tbhiv_100k_hi", "e_pop_num")), subset(n.t, select=c('country', 'year', 'c_newinc'))), ord='multiyear')

  egb$newrel_100k <- egb$c_newinc / egb$e_pop_num * 100000

  inc_notif_hbc <- qplot(year, e_inc_100k, data=egb, geom='line', colour=I(inc.color)) +
    geom_ribbon(aes(year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi), fill=I(inc.color), alpha=0.4) +
    geom_line(aes(year, newrel_100k)) +
    facet_wrap(~country, scales='free_y') + scale_x_continuous('') +
    ylab('Rate per 100 000 population per year') + expand_limits(y=0) + theme_glb.rpt() +
    ggtitle(paste0('Case notification and estimated TB incidence rates, 22 high-burden countries, 1990-', report_year-1, '. \nTrends in case notification rates (new and relapse cases, all forms) (black) and \nestimated TB incidence rates (green). \nShaded areas represent uncertainty bands.'))

  figsave(inc_notif_hbc, egb, "f3_3_inc_notif_hbc")


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

txsucc1 <- ggplot(tsr_table_melted, aes(area, value, fill=variable)) +
  geom_bar(stat="identity", position="stack") +
  geom_hline(yintercept=85, color="grey70") +
  geom_text(data=subset(tsr_table_melted, variable=="Treatment success"), aes(label=round(value,0)), hjust=1.25, vjust=0.3, size=4, color="white") +
  theme_glb.rpt() + coord_flip() +
  scale_fill_brewer("", type = "qual", palette = 8) +
  labs(x="", y="Percentage of cohort (%)") +
  theme(legend.position="bottom", panel.grid=element_blank()) + expand_limits(c(0,0)) +
  ggtitle(paste0("Treatment outcomes for new and relapse cases, ", report_year-2, ", globally, \nfor the six WHO regions and 22 high-burden countries"))

txsucc <- arrangeGrob(txsucc1, sub = textGrob("* Treatment outcomes are for new cases only.", x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))


figsave(txsucc, tsr_table, "f3_5_txsucc") # Designer needs wide data

# and now clear up the mess left behind
rm(list=c("tsr", "tsr_table", "tsr_table_melted"))


# B3_6_hiv_ts_d ---------------------------------------------------

# Remove countries that did not report or reported 0
# - total outcomes (newrel_coh) (because otherwise we can't minus out the HIV)
# - tbhiv outcomes (tbhiv_coh) in order to compare within reporting countries.

# Subset the data
hma <- o %>% filter(year==report_year-2, !is.na(newrel_coh), newrel_coh!=0, !is.na(tbhiv_coh), tbhiv_coh != 0) %>% select(country, year, newrel_coh, newrel_succ, newrel_fail, newrel_died, newrel_lost, c_newrel_neval, ret_nrel_coh, ret_nrel_succ, ret_nrel_fail, ret_nrel_died, ret_nrel_lost, c_ret_nrel_neval, tbhiv_coh, tbhiv_succ, tbhiv_fail, tbhiv_died, tbhiv_lost, c_tbhiv_neval)

# Country adjustments
# Remove a few who did not put any outcomes, but did list the cohort
if(report_year==2015){
  hma <- hma %>% filter(!country %in% c("Togo", "Angola"))
    warning("Togo and Angola are removed from TB/HIV outcomes graph.")
}

# Aggregate for global figures
hma[1] <- "global"

hmb <- aggregate(hma[3:ncol(hma)], by=list(area=hma$country, year=hma$year), FUN=sum, na.rm=TRUE)

# Extract variable name components for later combining
hmc <- melt(hmb, id=1:2)
hmc$type <- str_extract(hmc$variable, "tbhiv|newrel|ret_nrel")
hmc$out <- str_extract(hmc$variable, "coh|succ|fail|died|lost|neval")

# Combine newrel and ret_nrel

hmc$type <- ifelse(hmc$type=="tbhiv", "tbhiv", "all")
hmc1 <- aggregate(hmc[4], by=list(year=hmc$year, type=hmc$type, out=hmc$out), FUN=sum, na.rm=TRUE)

# Reshape with HIV+ and HIV- across the top
hmd <- cast(hmc1, year+out~type)

hmd$`HIV-` <- hmd$all - hmd$tbhiv

hmd <- hmd %>% rename("HIV+"=tbhiv)

# Reshape for calculating percentages
hme <- melt(as.data.frame(hmd[-3]), id=1:2, variable_name = "type")

hmf <- cast(hme, ...~out)

hmf$`Treatment success` <- hmf$succ / hmf$coh * 100
hmf$Failed <- hmf$fail / hmf$coh * 100
hmf$Died <- hmf$died / hmf$coh * 100
hmf$`Lost to follow-up` <- hmf$lost / hmf$coh * 100
hmf$`Not evaluated` <- hmf$neval / hmf$coh * 100

# Reshape for plotting
hmg <- melt(as.data.frame(hmf[c(1:2,9:ncol(hmf))]), id=1:2) # It's a melt and cast fiesta!

hiv_ts_d <- ggplot(hmg, aes(variable, value, fill=type)) +
  geom_bar(stat="identity", position="dodge", width=0.5) + theme_glb.rpt() + labs(x="") +
  scale_y_continuous("Percentage of cohort", limits=c(0,100)) + scale_fill_brewer("", type = "qual", palette=6) +
  ggtitle(paste("Outcomes of TB treatment by HIV status, ", report_year-2, sep=""))

figsave(hiv_ts_d, hmg, "B3_6_hiv_ts_d")


# B3_2_notif_ind ----------------------------------------------------

india <- n.t %>%
  filter(year>=2000, iso3=="IND") %>%
  select(iso3, year, c_newinc) %>%
  mutate(c_newinc_mill=c_newinc /1000000 )  %>%
  select(iso3, year, c_newinc_mill)

B3_2_notif_ind <- ggplot(india, aes(year, c_newinc_mill, ymin=0)) +
  geom_line() +
  scale_x_continuous("") +
  ylab("Cases per year (millions)")  +
  theme_glb.rpt() +
  ggtitle(paste0("Case notifications in India, 2000-", report_year-1))

figsave(B3_2_notif_ind, india, "B3_2_notif_ind", width=6, height=6)


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

# wide data for graphic designer
gaca <- gac %>% select(area, year, hivtest_pct) %>% spread(area, hivtest_pct)

# windows(11, 7); gad; dev.off()
figsave(hivtest_graph, gaca, "f6_1_hivtest_graph")


# 6_3_hivprog_graph_all ------------------------------------------------------

gg.yrs <- 2007:(report_year-1)

gg <- tbhiv %>% filter(year %in% gg.yrs) %>% select(iso3, year, g_hbhiv63, hiv_cpt_pct_numerator, hiv_cpt_pct_denominator, hiv_art_pct_numerator, hiv_art_pct_denominator, hivtest_pos_pct_denominator, hivtest_pos_pct_numerator)

gk <- aggregate(gg[4:ncol(gg)], by=list(year=gg$year), FUN=sum, na.rm=T)

gah <- within(gk, {
  hivtest_pos <- hivtest_pos_pct_numerator / hivtest_pos_pct_denominator * 100
  hiv_cpt <- hiv_cpt_pct_numerator / hiv_cpt_pct_denominator * 100
  hiv_art <- hiv_art_pct_numerator / hiv_art_pct_denominator * 100
})

gai <- melt(gah[c('year', "hivtest_pos", "hiv_cpt", "hiv_art")], id=1)

gai['Percentages'] <- factor(gai$variable,
                             levels=c("hivtest_pos", "hiv_cpt", "hiv_art"),
                             labels=c('Notified TB patients with known HIV status who were HIV-positive',
                                      'Notified HIV-positive TB patients \nstarted on CPT',
                                      'Notified HIV-positive TB patients \nstarted on ART'))

hivprog_graph_all <- ggplot(gai, aes(year, value)) +
  geom_line(size=1, alpha=.5) +
  # geom_ribbon(aes (year, best, ymin=lo, ymax=hi), alpha=0.2) +
  facet_wrap(~Percentages, ncol=3) +
  scale_y_continuous(limits = c(0, 100), name = "Percentage of notified TB patients", breaks=seq(0, 100, 20)) +
  scale_x_continuous("") + theme_glb.rpt()  +
  ggtitle(paste0('Percentage of notified TB patients with known HIV status who were HIV-positive, and percentage of notified \nHIV-positive TB patients enrolled on co-trimoxazole preventive therapy (CPT) and antiretroviral therapy (ART),\n ', min(gai$year), '\u2013', max(gai$year)))

figsave(hivprog_graph_all, gai, "f6_3_hivprog_graph_all")



# 6_4_hiv_art_etbhiv ----------------------------------------------------------

gca <- subset(tbhiv, year>=2004, select=c('iso3', 'year', 'hivtest_pos', 'hiv_art'))

gcb <- aggregate(gca[3:ncol(gca)], by=list(year=gca$year), FUN=sum, na.rm=T)

gcb1 <- merge(subset(araw.t, group_name=="global", c(year, e_inc_tbhiv_num_lo, e_inc_tbhiv_num_hi, e_inc_tbhiv_num)), gcb)

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
  scale_y_continuous("Number of TB patients (thousands)") + theme_glb.rpt() +
  scale_x_continuous(name="") +  scale_colour_manual(name="", values=c(inch.color, "firebrick", "dark blue")) +
  geom_text(data=gcc[gcc$year==report_year-2, ], aes(label=variable), vjust=3, hjust=1)  +
  ggtitle(paste("ART enrolment among HIV-positive TB patients compared with the reported number of HIV-positive \nTB patients and the estimated number of HIV-positive people who developed TB(a), 2004", report_year-1, sep="\u2013")) + theme(legend.position="none")

# Footnote:
# Notified HIV-positive TB patients on ART includes new and relapse TB
# cases plus prevalent TB cases re-registered for treatment change
# (e.g. after treatment failure). Estimated HIV-positive incident TB cases
# includes only new and relapse TB cases.


figsave(hiv_art_etbhiv, gcc, "f6_4_hiv_art_etbhiv")


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

gfd <- spread(gfc, area, value)

figsave(hiv_ipt_graph, gfd, "f6_6_hiv_ipt_graph")


## END ---------------------------------------
