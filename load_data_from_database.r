# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Connect to the global TB database and copy views to a standard set
# of data frames:
#
# n:     dataframe copy of view_TME_master_notification
# nk:    dataframe copy of view_TME_master_notification_exceptions
# tbhiv: dataframe copy of view_TME_master_TBHIV_for_aggregates
# e:     dataframe copy of view_TME_estimates_epi
# eraw:  dataframe copy of view_TME_estimates_epi_rawvalues
# f:     dataframe copy of view_TME_master_finance
# be:    dataframe copy of view_TME_master_budget_expenditure
# p:     dataframe copy of view_TME_estimates_population
# o:     dataframe copy of view_TME_master_outcomes
# s:     dataframe copy of view_TME_master_strategy
# i:     dataframe copy of view_TME_master_data_collection
# a:     dataframe copy of view_TME_aggregated_estimates_epi
# araw:  dataframe copy of view_TME_aggregated_estimates_epi_rawvalues
# d:     dataframe copy of view_TME_master_dr_surveillance
# dsvy:  dataframe copy of view_TME_master_drs
# dictionary:  dataframe copy of view_TME_data_dictionary
# datacodes:   dataframe copy of view_TME_data_codes
# emdr:  dataframe copy of view_TME_estimates_mdr
# emdrn: dataframe copy of view_TME_estimates_mdr_in_notified
# emdra: dataframe copy of view_TME_aggregated_estimates_mdr_in_notified
#
# data.date: When the source datasets were created
#
# Hazim Timimi July 2015, based on a original from Tom
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)

.fixnamibia <- function(df){
  # make sure Namibia's iso2 code is not interpreted as R's NA (null)
  df$iso2 <- ifelse(df$country=="Namibia", "NA", as.character(df$iso2))
  return(df)
}


# Get ODBC connection string from the environment file (if not already loaded)
if (exists("connection_string")==FALSE) {
  source("set_environment.r")
}

# connect to the database
require(RODBC)
ch <- odbcDriverConnect(connection_string)

# load views into dataframes
n <- .fixnamibia(sqlFetch(ch, "view_TME_master_notification"))
nk <- sqlFetch(ch, "view_TME_master_notification_exceptions")

if (exists("notification_maxyear")) {
  # get the TB/HIV figures appropriate for the data collection year
  # (This is used by create_tables_annex_for_web because the tables could be updated at the same time as the following
  # year's report is being compiled so it is important to keep unpublished 'final' TB/HIV data out)
  tbhiv	<- .fixnamibia(sqlQuery(ch, paste0("SELECT * FROM TBHIV_for_aggregates_for_dcyear(", notification_maxyear + 1 , ")")))
} else {
  tbhiv <- .fixnamibia(sqlFetch(ch, "view_TME_master_TBHIV_for_aggregates"))
}

e <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_epi"))
eraw <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_epi_rawvalues"))
f <- .fixnamibia(sqlFetch(ch, "view_TME_master_finance"))
be <- .fixnamibia(sqlFetch(ch, "view_TME_master_budget_expenditure"))
p <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_population"))
o <- .fixnamibia(sqlFetch(ch, "view_TME_master_outcomes"))
s <- .fixnamibia(sqlFetch(ch, "view_TME_master_strategy"))
i <- .fixnamibia(sqlFetch(ch, "view_TME_master_data_collection"))
a <- sqlFetch(ch, "view_TME_aggregated_estimates_epi")
araw <- sqlFetch(ch, "view_TME_aggregated_estimates_epi_rawvalues")
d <- .fixnamibia(sqlFetch(ch, "view_TME_master_dr_surveillance"))
dsvy <- .fixnamibia(sqlFetch(ch, "view_TME_master_drs"))
dictionary <- sqlFetch(ch, "view_TME_data_dictionary")
datacodes <- sqlFetch(ch, "view_TME_data_codes")
emdr <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_mdr"))
emdrn <- .fixnamibia(sqlFetch(ch, "view_TME_estimates_mdr_in_notified"))
emdra <- sqlFetch(ch, "view_TME_aggregated_estimates_mdr_in_notified")
close(ch)

# format d to play nicer with the other data.frames
# - Removing records that aren't explicitly covering a whole country
# - Adding in past years so the number of records is identical with view_TME_master_notification (with NAs filled in for missing data.)
# - Adding in grouping variables and other meta (1:19 in n)

d <- merge(n[n$year>=min(d$year), 1:19], d[d$all_areas_covered==1 |
  is.na(d$all_areas_covered),], all.x=T)

# Add an internal date'n'time stamp
data.date <- Sys.time()


# set e_pop_num to numeric to avoid integer overflow error
e$e_pop_num <- as.numeric(e$e_pop_num)


# That's it folks ----

