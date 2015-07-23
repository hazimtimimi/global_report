# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This script takes the PDFs produced from Excel by table and splits them up and reassembles them by region.
# It also adds a two-page introductory PDF for each region at the beginning, originally generated from Word.
# The script calls PDFsam (PDF split and merge -- http://www.pdfsam.org/) to do the heavy lifting.
#
# Tom Hiatt, 2011
# Hazim Timimi, 2014 - 2015, to use XML files to define the list of files to be split up and merged.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Instructions ----
#
#   1. Make sure all the source PDFs created from Excel and Word are ready in the 'PDF_by_table' subfolder.
#   2. Make sure the get_tables_annex_for_web_environment.r file has defined all the files, paths and PDFsam commands
#   3. Run this script
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Clear the decks ----
rm(list=ls())


# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# scriptsfolder:    Folder containing these scripts.
# pdforigin_folder: Folder containing the starting PDFs and XML configuration files.
# pdfoutput_folder: Folder where the final PDFs will be saved.
# pdfsamfolder:     Folder containing the PDFsam executable file.
# pdfsamcmd:        The DOS command to launch PDFSam, with reference to the local installation of Java.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

scriptsfolder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616

setwd(scriptsfolder)

source("get_tables_annex_for_web_environment.r")  # particular to each person so this file is in the ignore list

# Text to be used to create the XML input files telling PDFsam which files to read
# I could hard code this, but more fun to do using code ...

# Get a DOS version of the outfolder -- backslashes need escaping with ... backslash!
outfolder_DOS <- gsub("/", "\\" ,outfolder, fixed=TRUE )

# aggregates
xml_for_agg <- c('<?xml version="1.0" encoding="UTF-8"?>',
                 '<filelist>',
                 paste0('<file value="', outfolder_DOS, 'section_notes\\section_notes.pdf"/>'),
                 paste0('<file value="', outfolder_DOS, 'PDF_by_table\\mort_prev_agg.pdf"/>'),
                 paste0('<file value="', outfolder_DOS, 'PDF_by_table\\inc_cdr_agg.pdf"/>'),
                 paste0('<file value="', outfolder_DOS, 'PDF_by_table\\notif_agg.pdf"/>'),
                 paste0('<file value="', outfolder_DOS, 'PDF_by_table\\agesex_agg.pdf"/>'),
                 paste0('<file value="', outfolder_DOS, 'PDF_by_table\\rrmdr_agg.pdf"/>'),
                 paste0('<file value="', outfolder_DOS, 'PDF_by_table\\hiv_test_agg.pdf"/>'),
                 paste0('<file value="', outfolder_DOS, 'PDF_by_table\\outcome_agg.pdf"/>'),
                 '</filelist>'
)

# countries
xml_for_c <- c('<?xml version="1.0" encoding="UTF-8"?>',
               '<filelist>',
               paste0('<file value="', outfolder_DOS, 'section_notes\\section_notes.pdf"/>'),
               paste0('<file value="', outfolder_DOS, 'PDF_by_table\\mort_prev_c.pdf"/>'),
               paste0('<file value="', outfolder_DOS, 'PDF_by_table\\inc_cdr_c.pdf"/>'),
               paste0('<file value="', outfolder_DOS, 'PDF_by_table\\notif_c.pdf"/>'),
               paste0('<file value="', outfolder_DOS, 'PDF_by_table\\agesex_c.pdf"/>'),
               paste0('<file value="', outfolder_DOS, 'PDF_by_table\\rrmdr_c.pdf"/>'),
               paste0('<file value="', outfolder_DOS, 'PDF_by_table\\hiv_test_c.pdf"/>'),
               paste0('<file value="', outfolder_DOS, 'PDF_by_table\\outcome_c.pdf"/>'),
               paste0('<file value="', outfolder_DOS, 'PDF_by_table\\labs_c.pdf"/>'),
               paste0('<file value="', outfolder_DOS, 'PDF_by_table\\mdr_est_c.pdf"/>'),
               '</filelist>'
)

# Use PDFSAM's ability to concatenate subsections of PDFs using the -u switch.
# The string definitions below define the page ranges to extract from each of the files in the lists
# above (xml_for_agg and xml_for_c), with : as the list separator.
# So "1-2:all" means take pages 1 - 2 from the first document and all pages from the second document.
# Need to define manually by inspecting each of the PDFs to see where data for each region starts and ends.
# This may change from year to year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


pages_agg <- "1-2:all:all:all:all:all:all:all:"
pages_AFR <- "3-4:1-16:1-16:1-12:1-16:1-5:1-10:1-16:1:1:"
pages_AMR <- "5-6:17-31:17-31:13-24:17-31:6-10:11-19:17-31:2:2:"
pages_EMR <- "7-8:32-39:32-39:25-30:32-39:11-13:20-24:32-39:3:3:"
pages_EUR <- "9-10:40-57:40-57:31-44:40-57:14-19:25-35:40-57:4:4:"
pages_SEAR <- "11-12:58-61:58-61:45-47:58-61:20-21:36-38:58-61:5:5:"
pages_WPR <- "13-14:62-73:62-73:48-56:62-73:22-25:39-46:62-73:6:6:"



# Create output folder (only if it doesn't yet exist)
dir.create(pdfoutput_folder, showWarnings = FALSE)

# Create the XML files ----
# These are used by PDFSam to find the PDF files to process

setwd(pdforigin_folder)

xml_agg_file <- file("aggregate_files.xml",  encoding = "UTF-8")
writeLines(xml_for_agg, xml_agg_file)
close(xml_agg_file)


xml_c_file <- file("country_files.xml",  encoding = "UTF-8")
writeLines(xml_for_c, xml_c_file)
close(xml_c_file)

# Change to the folder containing the PDFSam executable.
setwd(pdfsamfolder)


# Use PDFSAM's ability to concatenate subsections of PDFs.

# The regional and global summaries are easy because just combine all the *_AGG PDFs into one, preceeded by the first two pages of the notes file. The list of files is held in file aggregate_files.xml in same folder as the PDFs; use the -l switch to refer to it.
# Use the -u switch to specify the page range for each PDF  (cannot do this from within the XML file).
# The -u switch uses : as the separator between page ranges for each PDF in the file list.


shell(paste0(pdfsamcmd, ' -l "', file.path(pdforigin_folder, 'aggregate_files.xml'), '" -o "', file.path(pdfoutput_folder, 'Indicators_global_and_regional_summaries.pdf'), '" -u ', pages_agg, '  -overwrite concat'))


# For each region, the list of source PDFs is in country_files.xml. Use the -u switch to specify the page range for each PDF.

# AFR
shell(paste(pdfsamcmd, ' -l "', file.path(pdforigin_folder, 'country_files.xml'), '" -o "', file.path(pdfoutput_folder, 'Indicators_African_Region.pdf'), '" -u ', pages_AFR, '  -overwrite concat', sep=""))

# AMR
shell(paste(pdfsamcmd, ' -l "', file.path(pdforigin_folder, 'country_files.xml'), '" -o "', file.path(pdfoutput_folder, 'Indicators_Region_of_the Americas.pdf'), '" -u ', pages_AMR, '  -overwrite concat', sep=""))

# EMR
shell(paste(pdfsamcmd, ' -l "', file.path(pdforigin_folder, 'country_files.xml'), '" -o "', file.path(pdfoutput_folder, 'Indicators_Eastern_Mediterranean_Region.pdf'), '" -u ', pages_EMR, '  -overwrite concat', sep=""))

# EUR
shell(paste(pdfsamcmd, ' -l "', file.path(pdforigin_folder, 'country_files.xml'), '" -o "', file.path(pdfoutput_folder, 'Indicators_European_Region.pdf'), '" -u ', pages_EUR, '  -overwrite concat', sep=""))

# SEAR
shell(paste(pdfsamcmd, ' -l "', file.path(pdforigin_folder, 'country_files.xml'), '" -o "', file.path(pdfoutput_folder, 'Indicators_South_East_Asia_Region.pdf'), '" -u ', pages_SEAR, '  -overwrite concat', sep=""))

# WPR
shell(paste(pdfsamcmd, ' -l "', file.path(pdforigin_folder, 'country_files.xml'), '" -o "', file.path(pdfoutput_folder, 'Indicators_Western_Pacific_Region.pdf'), '" -u ', pages_WPR, '  -overwrite concat', sep=""))

