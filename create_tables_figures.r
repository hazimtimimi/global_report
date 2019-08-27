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
# 6 July 2012, revised June/July 2017
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Clear the decks ----
rm(list=ls())


# Establish the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2019

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


source("set_environment.r")  # particular to each person so this file is in the ignore list


# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library("reshape")
library("ggplot2")
library("grid")
library("scales")
library("xtable")
library("stringr")
library("dplyr")
library("tidyr")
library("gridExtra")
library("RColorBrewer")
library(readxl)
library(openxlsx)
library(Cairo)


# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source("functions/combine_distributions.r")
source("functions/fix_entity_names.r")
source("functions/handle_NAs.r")
source("functions/make_who_maps.r")
source("functions/round_numbers.r")
source("functions/save_outputs.r")
source("functions/set_plot_themes.r")
source("functions/calculate_outcomes_pct.r")


# Load data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if (use_live_db==TRUE){

  # load the data directly from the global TB database
  source("load_data_from_database.r")

} else {

  # load up an .RData file containing a copy of the database views
  load(paste(rdata_folder, rdata_name, sep="/"))
}

# Load lookup table for regional names to be used in the report ----
who_region_names <- get_region_names()



# Repeated footnotes -----
india_incidence_footnote <- "Estimates of TB incidence for India are interim, pending results from the national TB prevalence survey planned for 2019/2020."
india_mortality_footnote <- "Estimates of TB mortality for India are interim, pending results from the national TB prevalence survey planned for 2019/2020."
pending_incidence_footnote <- "Estimates of TB incidence for Eswatini, Lesotho, Mozambique, Nepal, and South Africa will be reviewed after final \nresults from their respective national TB prevalence surveys are available in 2020."
pending_mortality_footnote <- "Estimates of TB mortality for Eswatini, Lesotho, Mozambique, Nepal, and South Africa will be reviewed after final \nresults from their respective national TB prevalence surveys are available in 2020."

# Create a folder structure for output files ----
# (only if they don't exist yet)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

dir.create(figures_folder, showWarnings = FALSE)
if(!(file.path(figures_folder, "FigData") %in% list.dirs(figures_folder, recursive=FALSE))){
  dir.create(file.path(figures_folder, "FigData"))
  dir.create(file.path(figures_folder, "Figs"))
  dir.create(file.path(figures_folder, "CPFigs"))
  dir.create(file.path(figures_folder, "Tables"))
  dir.create(file.path(figures_folder, "Footnotes"))
}


stop("

     >>>>>>>>>>
     Stopping here so can do the rest manually!
     <<<<<<<<<<<<")



# Run everything -----
source(file.path(scripts_folder, "Tables.r"))

source(file.path(scripts_folder, "Figures.r"))

source(file.path(scripts_folder, "profile_figures.r"))

# Create the HTML file with headline statistics
rmarkdown::render(input = "create_headline_stats.Rmd", output_dir = figures_folder, output_file = "headline_statistics.htm")

# Merge every footnote file in the footnote folder into one list file

setwd(footnotes_folder)
footfiles <- list.files(footnotes_folder, pattern="*.xlsx")

require(purrr)
Footlist <- footfiles %>% 
            map_dfr(read.xlsx)  

write.xlsx(as.data.frame(Footlist),
           file=paste(figures_folder, "/Footnotes/", "Footnote List", Sys.Date(), ".xlsx", sep=""),
           row.names=FALSE,
           showNA=FALSE,
           colWidths="auto",
           firstActiveRow=2)
rm(footfiles,Footlist)

# Reset the working directory
source("set_environment.r")
