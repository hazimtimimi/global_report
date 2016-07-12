# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to produce tables and figures (including maps)
# for the global TB report.
#
# Source this to set up the working environment and to call Figures.r, Tables.r and Maps.r
#
# A few details on my way of working. For the tables, figures, maps repeated from year to year,
# all the code is here and the data source is nearly 100% from the global database.
# Some people have to send me excel files which I save in the 'External data' folder. For other one-off
# tables I just save the final files in my folders and iterate with the creator to get them in a ready format.
#
# Tom Hiatt
# 6 July 2012, revised July 2016
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Clear the decks ----
rm(list=ls())

starting_time <- Sys.time()


# Establish the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2016

# Include estimates tables and figures? ----
# Decide whether or not to produce the tables and figures relying upon burden estimates
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
flg_show_estimates <- FALSE
flg_forecast_estimates_ready <- FALSE

# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)


# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# scripts_folder: Folder containing these scripts
# figures_folder: Folder containing output subfolders for tables,figures and maps
# rdata_folder:   Folder in which to find a .RData file (if available)
# rdata_name:     Name of a .RData file containing copy of database views
# use_live_db:    Flag -- if TRUE then data loaded durectly from the global TB database
#                 if FALSE then data loaded from the .RData file
#
# and, from the global TB database, each data frame is named according to the underlying
# SQL-Server view, but without the leading "view_TME_" or "view_TME_master_" bits, so
#
#
# notification:       dataframe copy of view_TME_master_notification
# estimates_epi:      dataframe copy of view_TME_estimates_epi
# ... etc.
#
# data.date: When the source datasets were created
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

scripts_folder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616 # If not running by source, go to Session > Set Working Directory > To Source File Location. Then run scripts_folder <- getwd()


setwd(scripts_folder)

source("set_environment.r")  # particular to each person so this file is in the ignore list

if (use_live_db==TRUE){

  # load the data directly from the global TB database
  source("load_data_from_database.r")

} else {

  # load up an .RData file containing a copy of the database views
  load(paste(rdata_folder, rdata_name, sep="/"))
}

# Generate lookup table for regional names to be used for publication ----
g_whoregion <- c("AFR", "AMR", "EMR", "EUR", "SEA", "WPR")
entity <- c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific")

who_region_names <- data.frame(g_whoregion, entity)

rm(g_whoregion)
rm(entity)



# Repeated footnotes -----
bangladesh.est.foot <- "For Bangladesh, a joint reassessment of estimates of TB disease burden will be undertaken following completion of the national TB prevalence survey."

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library("reshape")
library("ggplot2")
library("grid")
library("scales")
library("xtable")
library("stringr")
library("timeSeries")
library("ggthemes")
library("dplyr")
library("tidyr")
library("gridExtra")
library("XLConnect")

# Create a folder structure for output files ----
# (only if they don't exist yet)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

dir.create(figures_folder, showWarnings = FALSE)
if(!(file.path(figures_folder, "FigData") %in% list.dirs(figures_folder, recursive=FALSE))){
  dir.create(file.path(figures_folder, "Review"))
  dir.create(file.path(figures_folder, "FigData"))
  dir.create(file.path(figures_folder, "Figs"))
  dir.create(file.path(figures_folder, "CPFigs"))
  dir.create(file.path(figures_folder, "Tables"))
}

setwd(figures_folder)




# Graph theme components ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
theme_glb.rpt <- function(base_size=10, base_family="") {
  colors <- ggthemes_data$few
  gray <- colors$medium['gray']
  black <- colors$dark['black'] # I don't know why these last 3 parts are needed, but they are. List is here: http://docs.ggplot2.org/0.9.2.1/theme.html
  theme_bw(base_size=base_size, base_family=base_family) +
    theme(
      line = element_line(colour = gray),
      rect = element_rect(fill = "white", colour = NA),
      text = element_text(colour = black),
      axis.ticks.x = element_line(colour = gray),
      axis.ticks.y = element_blank(),
      legend.key = element_rect(colour = NA),
      ## Examples do not use grid lines
      panel.border = element_rect(colour = gray),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill="white", colour=NA),
      strip.text = element_text(hjust=0),
      plot.title = element_text(hjust=0)
      #       plot.margin = unit(c(0,0))
    )
}

# Bits I'm going to implement:
#   - Always show 0 (and not 0.0)
#   - Always expand for a comfy fit
#   - Labels be in the color of the lines where applicable [done]
#   - Titles left justified [done]
#   - label range always includes all data [giving up. maybe not a big deal.]
#   - All use same color palette [done]


# Burden colors ----
inc.color <- "green"
inch.color <- "red"
prev.color <- "blue"
mort.color <- "violet"

# # Dummy data ----
# # Create dummy data for latest year until data are available
# # new dataframes have names with _dummy appended ...
#
# for(df in c("estimates_epi",
#             "estimates_epi_rawvalues",
#             "aggregate_estimates_epi",
#             "aggregate_estimates_epi_rawvalues",
#             "notification",
#             "dr_surveillance")){
#   obj <- get(df)
#   if(max(obj["year"]) < report_year-1) {
#     warning(paste("Still need to get updated dataset for", df, "!!!"))
#     copy <- obj[obj["year"]==report_year-2,]
#     copy$year <- report_year-1
#     comb <- rbind(obj, copy)
#   }
#   else(comb <- obj)
#   assign(paste(df, "dummy", sep="_"), comb)
# }

# # Real forecast data (Download from PG's GitHub and save to dropbox)
# if (max(araw.t$year) < report_year & flg_forecast_estimates_ready) {
#   # Load from dropbox
#   load(paste(rdata_folder, "Extra data/PG/global_ff.Rdata", sep="/"))
#   load(paste(rdata_folder, "Extra data/PG/regional_ff.Rdata", sep="/"))
#
#   # Keep only forecast years and prev and mort; combine global and regional
#   glbl <- global.ff %>% data.frame() %>% filter(year==report_year) %>% select(year, e_pop_num=e.pop.num, e_prev_100k=prev, e_prev_100k_lo=prev.lo, e_prev_100k_hi=prev.hi, e_mort_exc_tbhiv_100k=mort.nh, e_mort_exc_tbhiv_100k_lo=mort.nh.lo, e_mort_exc_tbhiv_100k_hi=mort.nh.hi, forecast) %>% mutate(group_name="global", group_type="global")
#   reglo <- regional.ff %>% data.frame() %>% filter(year==report_year) %>% select(group_name=g.whoregion, year, e_pop_num=e.pop.num, e_prev_100k=prev, e_prev_100k_lo=prev.lo, e_prev_100k_hi=prev.hi, e_mort_exc_tbhiv_100k=mort.nh, e_mort_exc_tbhiv_100k_lo=mort.nh.lo, e_mort_exc_tbhiv_100k_hi=mort.nh.hi, forecast)%>% mutate(group_type="g_whoregion") %>% rbind(glbl)
#
#     araw.t <- merge(araw.t, reglo, all=TRUE) %>% mutate(forecast=ifelse(is.na(forecast), FALSE, forecast))
# }
#
# if (max(eraw.t$year) < report_year & flg_forecast_estimates_ready) {
#   # Load from dropbox
#   load(paste(rdata_folder, "Extra data/PG/hbc_ff.Rdata", sep="/"))
#   # Keep only forecast years and prev and mort
#   hbcff <- hbc.ff %>% data.frame() %>% filter(year==report_year) %>% select(iso3, year, e_pop_num=e.pop.num, e_prev_100k=prev, e_prev_100k_lo=prev.lo, e_prev_100k_hi=prev.hi, e_mort_exc_tbhiv_100k=mort.nh, e_mort_exc_tbhiv_100k_lo=mort.nh.lo, e_mort_exc_tbhiv_100k_hi=mort.nh.hi, forecast)
#
#   # Get est Rdata as well for vr.raw (necessary for HBC mortality plot)
#   # Load from dropbox
#   load(paste(rdata_folder, "Extra data/PG/est.Rdata", sep="/"))
#   # Keep only forecast years and prev and mort
#   hbcvr <- est %>% data.frame() %>% select(iso3, year, vr.raw)
#   eraw.t <- merge(eraw.t, hbcff, all=TRUE) %>% mutate(forecast=ifelse(is.na(forecast), FALSE, forecast)) %>% merge(hbcvr, all=TRUE)
# }
#
#
# while(max(araw.t$year) < 2015) {
#   warning(paste('Still need to get updated forecast for araw!'))
#   copy <- araw.t[araw.t$year==max(araw.t$year),]
#   copy$year <- max(araw.t$year) + 1
#   araw.t <- rbind(araw.t, copy)
# }
#
# while(max(eraw.t$year) < 2015) {
#   warning(paste('Still need to get updated forecast for eraw!'))
#   copy <- eraw.t[eraw.t$year==max(eraw.t$year),]
#   copy$year <- max(eraw.t$year) + 1
#   eraw.t <- rbind(eraw.t, copy)
# }


# Report rounding convention ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Report rounding convention
# - 0 is written as "0"
# - values under 0.1 are written "<0.1"
# - from 0.1 to under 10 are written rounding 1 decimal place
# - 10 to under 100 are written rounded to the whole number.
# - 100 and above are written as 3 significant figures for rates and 2 significant figures for absolute numbers.
# - data that are not reported, but could be are represented as empty cells and should be accompanied by a footnote.
# - data that cannot be calculated, either because of missing data, data was not requested, or any other reason are represented with an en-dash (ctrl - on your keyboard).


# rounding function for absolute numbers
round.conv <- function(x) {
  ifelse(x==0, 0,
  ifelse(x < 0.1, "<0.1",
  ifelse(signif(x, 2) < 1, formatC(round(x,1), format='f', digits=1),
  ifelse(signif(x, 2) < 10, sapply(signif(x,2), sprintf, fmt="%#.2g"), signif(x, 2)))))
}

# rounding convention for rates
round.conv.rates <- function(x) {
  ifelse(x==0, 0,
  ifelse(x < 0.1, "<0.1",
  ifelse(signif(x, 2) < 10, formatC(round(x,1), format='f', digits=1),
  ifelse(signif(x, 3) < 100, signif(x, 2), signif(x, 3)))))
}

# Depends on whether dealing with thousands or rates. In general, 0 is 0, under .1 to "<0.1", then appropriate sig figs.
# Numbers < 1 are only rounded to 1 sig fig
frmt <- function(x, rates=FALSE, thou=FALSE) {
  ifelse(x==0, "0",
  ifelse(x < 0.01 & thou==TRUE, "<0.01",
  ifelse(x < 0.1 & thou==FALSE, "<0.1",
  ifelse(signif(x, 2) < 1 & thou==TRUE, formatC(signif(x,2), format='f', digits=2),
  ifelse(signif(x, 2) < 1, formatC(signif(x,1), format='f', digits=1),
  ifelse(signif(x, 2) < 10, formatC(signif(x,2), format='f', digits=1),
  ifelse(x > 1 & rates==FALSE, formatC(signif(x, 2), big.mark=" ", format='d'),
  ifelse(signif(x, 3) < 100, formatC(signif(x, 2), big.mark=" ", format='d'), formatC(signif(x, 3), big.mark=" ", format='d')))))))))
}

# Simple rounder that just adds in the thousands separator
rounder <- function(x, decimals=FALSE) {

  if (decimals==TRUE) {
    ifelse(is.na(x), NA,
    ifelse(x==0, 0,
    ifelse(x < 0.01, "<0.01",
    ifelse(round(x,2) < 0.1, formatC(round(x,2), format='f', digits=2),
    ifelse(round(x,1) < 10, formatC(round(x,1), format='f', digits=1), formatC(round(x,0), big.mark=" ", format='d') )))))

  } else {

    ifelse(is.na(x), NA,
    ifelse(x==0, 0,
    ifelse(x < 1, "< 1", formatC(round(x,0), big.mark=" ", format='d'))))
  }
}



# Shorten and correct names (and order them properly!) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

standard_table_order <- c("Afghanistan", "Bangladesh", "Brazil", "Cambodia", "China",
                          "DR Congo", "Ethiopia", "India", "Indonesia", "Kenya", "Mozambique",
                          "Myanmar", "Nigeria", "Pakistan", "Philippines", "Russian Federation",
                          "South Africa", "Thailand", "Uganda", "UR Tanzania", "Viet Nam",
                          "Zimbabwe",
                          "High-burden countries",
                          "AFR", "AMR", "EMR", "EUR", "SEAR", "WPR", "Global")


.shortnames <- function(d, col='country', ord='somethingelse'){
  d[col] <- as.character(d[[col]])
  d[col] <- ifelse(d[[col]]=='Democratic Republic of the Congo', 'DR Congo',
            ifelse(d[[col]]=='Democratic People\'s Republic of Korea', 'DPR Korea',
            ifelse(d[[col]]=='United Republic of Tanzania', 'UR Tanzania',
            ifelse(d[[col]]=='hbc22', 'High-burden countries',
            ifelse(d[[col]]=='global', 'Global',
            ifelse(d[[col]]=='SEA', 'SEAR', d[[col]]))))))


  if (ord=='hbc') {
    d <- d[match(standard_table_order, d[[col]]),]
  }
  return(d)
}



# Philippe's aggregation functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

add.rv <- function (r, r.lo, r.hi, r.sd, weights = 1, method = "beta")
{
  if (is.null(r) || length(r) == 0)
    stop("Error: r must contain at least one value")
  if (sum(r < 0 & !is.na(r) & method == "beta"))
    stop("Error: r must be positive with method 'beta'")
  if (sum(r > 1 & !is.na(r) & method == "beta"))
    stop("Error: r must be between 0 and 1 with method 'beta'")
  if (missing(r.sd))
    r.sd <- (r.hi - r.lo)/4
  if (missing(r.lo) & !missing(r.sd))
    r.lo <- numeric()
  if (missing(r.hi) & !missing(r.sd))
    r.hi <- numeric()
  if (sum(r.lo < 0 & !is.na(r.lo) & method == "beta"))
    stop("Error: r.lo must be positive with method 'beta'")
  if (sum(r.lo > 1 & !is.na(r.lo) & method == "beta"))
    stop("Error: r.lo must be between 0 and 1 with method 'beta'")
  if (sum(r.hi < 0 & !is.na(r.hi) & method == "beta"))
    stop("Error: r.hi must be positive with method 'beta'")
  if (sum(r.hi > 1 & !is.na(r.hi) & method == "beta"))
    stop("Error: r.hi must be between 0 and 1 with method 'beta'")
  if (sum(r.sd > 1 & !is.na(r.sd) & method == "beta"))
    stop("Error: sd must be between 0 and 1 with method 'beta'")
  if (sum(r[!is.na(r) & is.na(r.sd)]))
    stop("Error: some values for r are supplied without uncertainty")
  if (sum(r.sd < 0 & !is.null(r.sd) & !is.na(r.sd)))
    stop("Error: sd must be positive")
  if (!is.null(r.sd))
    v <- r.sd^2
  else v <- ((r.hi - r.lo)/4)^2
  sw <- ifelse(length(weights) > 1, sum(weights[!is.na(r)],
                                        na.rm = TRUE), 1)
  out.m <- sum(r * weights, na.rm = TRUE)/sw
  out.v <- ifelse(length(weights) > 1, sum(v[!is.na(r)] * weights[!is.na(r)]^2,
                                           na.rm = TRUE)/sw^2, sum(v))
  if (method == "beta") {
    S <- (out.m * (1 - out.m)/out.v) - 1
    a <- S * out.m
    b <- S * (1 - out.m)
    lo <- qbeta(0.025, a, b)
    hi <- qbeta(0.975, a, b)
  }
  else {
    lo <- qnorm(0.025, out.m, sqrt(out.v))
    hi <- qnorm(0.975, out.m, sqrt(out.v))
  }
  if (all(weights == 1))
    return(data.frame(r = out.m, r.lo = lo, r.hi = hi, r.sd = sqrt(out.v)))
  else return(data.frame(r = out.m, r.lo = lo, r.hi = hi, r.sd = sqrt(out.v),
                         r.num = out.m * sw, r.lo.num = lo * sw, r.hi.num = hi *
                           sw, e.pop.num = sw))
}


# product of two random variables X and Y using Taylor expansion approximation
prodXY <- function(X, Y, varX, varY, covXY=0){
  eXY <- X * Y + covXY
  varXY <- X^2*varY + Y^2*varX + 2*X*Y*covXY + varX*varY + covXY^2
  return(list("E(XY)"=eXY, "Var(XY)"=varXY))
}


# ratio of two random variables X and Y using Taylor expansion
divXY <- function(X, Y, varX, varY, covXY=0){
  eXY <- X/Y - covXY/Y^2 + X*varY/Y^3
  varXY <- varX/Y^2 - 2*X*covXY/Y^3 + X^2*varY/Y^4
  return(list("E(X/Y)"=eXY, "Var(X/Y)"=varXY))
}


# For saving figures ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

figsave <- function(obj, data, name, width=11, height=7){
  #   save PDF for designer
  ggsave(paste0(figures_folder, "/Figs/", name, Sys.Date(), ".pdf"), obj, width=width, height=height)
  #   save PNG for reviewer
  ggsave(paste0(figures_folder, "/Review/", name, ".png"), obj, width=width, height=height)
  #   save data for designer
  write.csv(data, file=paste(figures_folder, "/FigData/", name, Sys.Date(), ".csv", sep=""), row.names=FALSE, na="")
  #   save data for reviewer
  out <- xtable(data)
  print(out, file=paste(figures_folder, "/Review/", name, ".htm", sep=""), type="html")
}

# For saving tables ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tablecopy <- function(table){
  file.copy(paste0("Tables/", table, Sys.Date(), ".htm"), paste0("Review/", table, ".htm"), overwrite=TRUE)
}

# To make typical report table ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

hbc_and_aggs_table <- function(df, country.col=1, year.col=2, data.cols ){
  # Produce a standard table with the 22 HBC countries followed by regional and global aggregates
  #
  # country.col: which column number has country names
  # year.col:    Which column number has the year
  # data.cols:   Which column numbers have the data to be included and aggregated

  # Select and order high burden countries
  hbcs <- df[df$g_hbc22=="high", c(country.col, year.col, data.cols)]
  hbcs <- hbcs[order(hbcs$country, hbcs$year),]
  names(hbcs)[1] <- "area"

  # Make aggregate rows
  agg_hbc <- aggregate(df[data.cols], by=list(area=df$g_hbc22, year=df$year), FUN=sum, na.rm=TRUE)
  agg_hbc <- agg_hbc[agg_hbc$area=="high",]
  agg_hbc$area <- "High-burden countries"

  agg_region <- aggregate(df[data.cols], by=list(area=df$g_whoregion, year=df$year), FUN=sum, na.rm=TRUE)

  agg_global <- df
  agg_global[country.col] <- "Global"
  agg_global <- aggregate(agg_global[data.cols], by=list(area=agg_global[[country.col]], year=df$year), FUN=sum, na.rm=TRUE)

  # Combine HBCs and aggregates into one data frame
  combined_df <- rbind(hbcs, agg_hbc, agg_region, agg_global)

  # Use short country names
  # (Don't use .shortnames() because lose records if have mutliple years)
  combined_df$area <- ifelse(combined_df$area=='Democratic Republic of the Congo', 'DR Congo',
                      ifelse(combined_df$area=='Democratic People\'s Republic of Korea', 'DPR Korea',
                      ifelse(combined_df$area=='United Republic of Tanzania', 'UR Tanzania',
                      ifelse(combined_df$area=='SEA', 'SEAR', combined_df$area))))

  return(combined_df)
}



# For adding an x-axis to orphaned plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

facetAdjust <- function(x, pos = c("up", "down"))
{
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p); dev.off()
  dims <- apply(p$panel$layout[2:3], 2, max)
  nrow <- dims[1]
  ncol <- dims[2]
  panels <- sum(grepl("panel", names(gtable$grobs)))
  space <- ncol * nrow
  n <- space - panels
  if(panels != space){
    idx <- (space - ncol - n + 1):(space - ncol)
    gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
    if(pos == "down"){
      rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"),
                   gtable$layout$name)
      lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
      gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
    }
  }
  class(gtable) <- c("facetAdjust", "gtable", "ggplot"); gtable
}

# The function for printing which differs only by few lines from ggplot2:::print.ggplot:
print.facetAdjust <- function(x, newpage = is.null(vp), vp = NULL) {
  if(newpage)
    grid.newpage()
  if(is.null(vp)){
    grid.draw(x)
  } else {
    if (is.character(vp))
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(x)
    upViewport()
  }
  invisible(x)
}


# Better row sum ----
# This function sums rows ignoring NAs unless all are NA
# [rowSums() returns 0 instead of NA if all are NA and you use na.rm=TRUE]
# use it like this
# df$snu <- sum_of_row(df[c('new_sn', 'new_su')])
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sum_of_row <- function(x) {
  tosum <- as.matrix(x)
  summed <- rowMeans((tosum), na.rm=TRUE) * rowSums(!is.na((tosum)))
  return(summed)
}

# Column sum that removes NAs unless all are NA
sum0 <- function(x) sum(x, na.rm = !all(is.na(x)))

# Text helps --------

# This function takes a vector and makes it into a comma separated list for text.
lister.text <- function(x){
  if(length(x)==1) return(x)
  if(length(x)>1) paste(paste(x[1:(length(x)-1)], collapse=", "), x[length(x)], sep=" and ")
}


# Convert a null (NA) to zero
NZ <- function(x){
  x <- ifelse(is.na(x),0,x)
  return(x)
}


stop("

     >>>>>>>>>>
     Stopping here so can do the rest manually!
     <<<<<<<<<<<<")



# Run everything -----
source(file.path(scripts_folder, "Figures.r"))
source(file.path(scripts_folder, "Tables.r"))
source(file.path(scripts_folder, "Maps.r"))

cat("\nThat took", signif(Sys.time() - starting_time, 3), units(Sys.time() - starting_time), "to run.\n")
