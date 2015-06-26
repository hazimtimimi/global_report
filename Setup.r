# -------------------------------------------------
# Initial set up script for tables and figures
# Tom Hiatt
# 6 July 2012, revised 25 June 2015
# -------------------------------------------------

# bits to change if another dude is running this.
whoami <- "Tom"   # I hope you know the answer to this.

if(whoami=="Tom"){
  Rprofile <- "d:/users/hiattt/Dropbox/Code/R/.Rprofile"
  basefolder <- "d:/users/hiattt/Google Drive/Work files/Global TB report/Tables and Figures"
  scriptsfolder <- "D:/Users/hiattt/Dropbox/Code/Surveillance reports"
}

if(whoami=="Hazim"){
  Rprofile <- "d:/TMEData/TomsCode/MyEnvironment/.Rprofile" # Note for Hazim: I've added a part into my .Rprofile that adds in the HBC groupings and forecast estimates. You may want to similarly include in your .Rprofile
  basefolder <- "D:/Extracted Data/Extracted Data2014/GTBR2014_internal/tables_figures"
  scriptsfolder <- "D:/TMEData/TomsCode/Global TB control Reports/Tables and figures"
}


# A few details on my way of working. For the tables, figures, maps repeated from year to year, all the code is here and the data source is nearly 100% from the global database. Some people have to send me excel files which I save in the 'External data' folder. For other one-off tables I just save the final files in my folders and iterate with the creator to get them in a ready format.

# -------------------------------------------------

start <- Sys.time()
source(Rprofile)
runprofile()
library(plyr) # The next function uses dplyr and plyr must be loaded first.
getforecastestimates()

# -------------------------------------------------

# Find the report year
thisyear <- as.numeric(format(Sys.time(),"%Y")) - ifelse(as.numeric(format(Sys.time(),"%m")) < 6, 1, 0) # This refers to the report year

# Set-up

libraries(c('reshape', 'ggplot2', 'grid', 'scales', 'xtable', 'stringr', 'timeSeries', 'ggthemes', 'plyr', "gridExtra"))

# Create a folder structure for saving files if doesn't exist yet 
if(file.path(basefolder, 'FigData') %nin% list.dirs(basefolder, recursive=FALSE)){
  dir.create(file.path(basefolder, "Review"))
  dir.create(file.path(basefolder, "FigData"))
  dir.create(file.path(basefolder, "Figs"))
  dir.create(file.path(basefolder, "CPFigs"))
  dir.create(file.path(basefolder, "Slides"))
  dir.create(file.path(basefolder, "Tables"))
}

outfolder <- basefolder
setwd(basefolder)

# Graph theme components
# ---------------------------
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


# Burden colors

inc.color <- "green"
inch.color <- "red"
prev.color <- "blue"
mort.color <- "violet"

# Dummy data ---------------------------------------------------
# Create dummy data for latest year until data are available
# new dataframes have names with .t appended ...

for(df in c('e', 'eraw', 'a', 'araw', 'n', 'd')){
  obj <- get(df)
  if(max(obj["year"]) < thisyear-1) {
    warning(paste('Still need to get updated dataset for', df, '!!!'))
    copy <- obj[obj['year']==thisyear-1-1,]
    copy$year <- thisyear-1
    comb <- rbind(obj, copy)
    if(df=="n") comb[comb$iso3=="DZA" & comb$year==thisyear-1, 'tot_newrel'] <- NA
  }
  else(comb <- obj)
  assign(paste(df, "t", sep="."), comb)
}

while(max(araw.t$year) < 2015) { 
  warning(paste('Still need to get updated forecast for araw!'))
  copy <- araw.t[araw.t$year==max(araw.t$year),]
  copy$year <- max(araw.t$year) + 1
  araw.t <- rbind(araw.t, copy)
}

while(max(eraw.t$year) < 2015) { 
  warning(paste('Still need to get updated forecast for eraw!'))
  copy <- eraw.t[eraw.t$year==max(eraw.t$year),]
  copy$year <- max(eraw.t$year) + 1
  eraw.t <- rbind(eraw.t, copy)
}


# functions

# Report rounding convention
# - 0 is written as "0"
# - values under 0.1 are written "<0.1"
# - from 0.1 to under 10 are written rounding 1 decimal place
# - 10 and above are written as 3 significant figures for rates and 2 significant figures for absolute numbers.
# - data that are not reported, but could be are represented as empty cells and should be accompanied by a footnote.
# - data that cannot be calculated, either because of missing data, data was not requested, or any other reason are represented with an en-dash (ctrl - on your keyboard).

# 0 is 0, under .1 to "<0.1", under 1 to 1 sig fig, otherwise 2 sig fig
round.conv <- function(x) {
  ifelse(x==0, 0, ifelse(x < 0.1, "<0.1", ifelse(signif(x, 2) < 1, formatC(round(x,1), format='f', digits=1),
                                                 ifelse(signif(x, 2) < 10, sapply(signif(x,2), sprintf, fmt="%#.2g"), signif(x, 2)))))
}  # Note: The second method for trailing 0s does not work with 9.99

# rounding convention for rates
# 0 is 0, under .1 to "<0.1", under 1 to 1 sig fig, under 100 to 2 sig figs,
# otherwise 3 sig figs
round.conv.rates <- function(x) {
  ifelse(x==0, 0, ifelse(x < 0.1, "<0.1", ifelse(signif(x, 2) < 10, formatC(round(x,1), format='f', digits=1),
                                                 # ifelse(signif(x, 2) < 10, formatC(round(x,1), format='f', digits=1),
                                                 ifelse(signif(x, 3) < 100, signif(x, 2), signif(x, 3)))))
}	

# Depends on whether dealing with thousands or rates. In general, 0 is 0, under .1 to "<0.1", then appropriate sig figs.
# Amended by Hazim 2012-08-31 to fix double-rounding error, plus also
# changed so that numbers < 1 are only rounded to 1 sig fig
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
  if(decimals==TRUE){
    ifelse(is.na(x), NA, ifelse(x==0, 0, ifelse(x < 0.01, "<0.01", ifelse(round(x,2) < 0.1, formatC(round(x,2), format='f', digits=2), ifelse(round(x,1) < 10, formatC(round(x,1), format='f', digits=1), formatC(round(x,0), big.mark=" ", format='d') )))))
  }
  else ifelse(is.na(x), NA, ifelse(x==0, 0, ifelse(x < 1, "< 1", formatC(round(x,0), big.mark=" ", format='d'))))
}

# Shorten and correct names (and order them properly!)
.shortnames <- function(d, col='country', ord='somethingelse'){
  d[col] <- as.character(d[[col]])
  d[col] <- ifelse(d[[col]]=='Democratic Republic of the Congo', 'DR Congo',
                   ifelse(d[[col]]=='Democratic People\'s Republic of Korea', 'DPR Korea',
                   ifelse(d[[col]]=='United Republic of Tanzania', 'UR Tanzania', 
                          ifelse(d[[col]]=='hbc22', 'High-burden countries', 
                                 ifelse(d[[col]]=='global', 'Global', 
                                        ifelse(d[[col]]=='SEA', 'SEAR', 
                                               d[[col]]))))))
#   if(ord %nin% c('hbc')) warning('Not ordering.')
  if(ord=='hbc')  d <- d[match(c("Afghanistan", "Bangladesh", "Brazil", "Cambodia", "China", "DR Congo", "Ethiopia", "India", "Indonesia", "Kenya", "Mozambique",  "Myanmar", "Nigeria", "Pakistan", "Philippines", "Russian Federation", "South Africa", "Thailand", "Uganda", "UR Tanzania", "Viet Nam", "Zimbabwe", "High-burden countries", "AFR", "AMR", "EMR", "EUR", "SEAR", "WPR", "Global"), d[[col]]),]
  
  
  return(d)
}

# Nab Philippe's functions (for aggregating)
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

# For saving figures

figsave <- function(obj, data, name, width=11, height=7){
  #   save PDF for designer
  ggsave(glue(outfolder, "/Figs/", name, Sys.Date(), ".pdf"), obj, width=width, height=height)
  #   save PNG for reviewer
  ggsave(glue(outfolder, "/Review/", name, ".png"), obj, width=width, height=height)
  #   save data for designer
  write.csv(data, file=paste(outfolder, "/FigData/", name, Sys.Date(), ".csv", sep=""), row.names=FALSE, na="")
  #   save data for reviewer
  out <- xtable(data)
  print(out, file=paste(outfolder, "/Review/", name, ".htm", sep=""), type="html")
}

# For saving tables

tablecopy <- function(table){
  file.copy(glue("Tables/", table, Sys.Date(), ".htm"), glue("Review/", table, ".htm"), overwrite=TRUE)
}

# To make typical report table
glb.rpt.table <- function(df, column.nums, country.col=1, year.col=NA){
  
  if(is.na(year.col)){
    hbcs <- df[df$g_hbc22=='high', c(country.col, column.nums)]
    names(hbcs)[1] <- "area"
    
    # make aggregate rows
    agg1 <- aggregate(df[column.nums], by=list(area=df$g_hbc22), FUN=sum, na.rm=TRUE)
    agg1 <- agg1[agg1$area=='high',]
    agg1$area <- 'High-burden countries'
    
    agg2 <- aggregate(df[column.nums], by=list(area=df$g_whoregion), FUN=sum, na.rm=TRUE)
    
    agg3 <- df; agg3[country.col] <- 'Global'
    agg3b <- aggregate(agg3[column.nums], by=list(area=agg3[[country.col]]), FUN=sum, na.rm=TRUE) 
    
    # combine together
    com1 <- rbind(hbcs, agg1, agg2, agg3b)
    com2 <- .shortnames(com1, col = "area", ord = "hbc")
  }
  
  if(!is.na(year.col)){
    hbcs <- df[df$g_hbc22=='high', c(country.col, year.col, column.nums)]
    names(hbcs)[1] <- "area"
    
    # make aggregate rows
    agg1 <- aggregate(df[column.nums], by=list(area=df$g_hbc22, year=df$year), FUN=sum, na.rm=TRUE)
    agg1 <- agg1[agg1$area=='high',]
    agg1$area <- 'High-burden countries'
    
    agg2 <- aggregate(df[column.nums], by=list(area=df$g_whoregion, year=df$year), FUN=sum, na.rm=TRUE)
    
    agg3 <- df; agg3[country.col] <- 'Global'
    agg3b <- aggregate(agg3[column.nums], by=list(area=agg3[[country.col]], year=df$year), FUN=sum, na.rm=TRUE) 
    
    # combine together
    com2 <- rbind(hbcs, agg1, agg2, agg3b)
    
  }
  return(com2)
}

# For adding an x-axis to orphaned plots -----------------------------
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


########################################################
# Functions to assist with tables and figure in markdown document (poached from here: http://rmflight.github.io/posts/2012/10/papersinRmd.html)
########################################################

yr <- as.numeric(format(Sys.time(),"%Y")) - ifelse(as.numeric(format(Sys.time(),"%m")) < 4, 2, 1)

incCount <- function(inObj, useName) {
  nObj <- length(inObj)
  useNum <- max(inObj) + 1
  inObj <- c(inObj, useNum)
  names(inObj)[nObj + 1] <- useName
  inObj
}
figCount <- c(`_` = 0)
tableCount <- c(`_` = 0)

# tableCount <- incCount(tableCount, "t.blogPostDocs")
# tableCount

pasteLabel <- function(preText, inObj, objName, insLink = TRUE) {
  objNum <- inObj[objName]
  
  useText <- paste(preText, objNum, sep = " ")
  if (insLink) {
    useText <- paste("[", useText, "](#", objName, ")", sep = "")
  }
  useText
}

tableCat <- function(inFrame) {
  outText <- paste(names(inFrame), collapse = " | ")
  outText <- c(outText, paste(rep("---", ncol(inFrame)), collapse = " | "))
  invisible(apply(inFrame, 1, function(inRow) {
    outText <<- c(outText, paste(inRow, collapse = " | "))
  }))
  return(outText)
}

# Run everything ------------------------------------------------

source(file.path(scriptsfolder, "Figures.r"))
source(file.path(scriptsfolder, "Tables.r")) 
source(file.path(scriptsfolder, "Maps.r"))

cat("\nThat took", signif(Sys.time() - start, 3), units(Sys.time() - start), "to run.\n")
