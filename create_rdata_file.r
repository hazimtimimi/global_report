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

scriptsfolder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616
setwd(scriptsfolder)

# Load the views directly from the global TB database
source("load_data_from_database.r")

# Find where to save the data (variable datafolder in get_tables_figures_environment.r)
source("get_tables_figures_environment.r")

setwd(datafolder)

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
