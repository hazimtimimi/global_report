# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load views from the global TB database and save them all in a
# single dated .Rdata file.
# Run by Hazim in Geneva who saves the .Rdata in a shared dropbox folder
# so that Tom can get access to the data from Manilla.
#
# Hazim Timimi, based on a original from Tom
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Clear the decks ----
rm(list=ls())

scripts_folder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616
setwd(scripts_folder)


# Get DB connection string and path where to save the data (variable rdata_folder in set_environment.r)
source("set_environment.r")

# Load the views directly from the global TB database
source("load_data_from_database.r")


setwd(rdata_folder)

# Save the views
save (n,
      nk,
      tbhiv,
      e,
      eraw,
      f,
      be,
      p,
      o,
      s,
      i,
      a,
      araw,
      d,
      dsvy,
      dictionary,
      datacodes,
      emdr,
      emdrn,
      emdra,
      data.date,
      file=paste("data_",Sys.Date(),".Rdata",sep=""))

# Clear the decks ----
rm(list=ls())
