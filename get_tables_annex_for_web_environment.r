# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Set up the running environment for creating the CSV files for the
# annex tables to be published on the web
#
# (called by create_tables_annex_for_web.r)
#
# Hazim's version for 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


outfolder    <- "D:/Extracted Data/Extracted Data2015/GTBR15_internal/tables_annex_for_web/linked CSVs/"
datafolder   <- "D:/Dropbox/Tom_global_report/"
# datafolder <- "D:/scratch/"
rdata_name   <- "data_2015-07-15.Rdata"

# Decide whether to take data directly from the database instead of from a .RData file
use_live_db  <- TRUE
