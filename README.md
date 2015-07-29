# Scripts to generate tables, figures, maps and annexes for the global TB report
*(Shared by Tom and Hazim, so either can run regardless of which office they are sitting in.)*

## Use

Make sure you have your version of `set_environment.r` set up (see notes further below).

* To create an .RData file from the database, do in R:
```
> source(create_rdata_file.r)
```

* To create tables, figures and maps for the global report, do in R:
```
> source(create_tables_figures.r)
```
This will create a series of folders (or add to them if they are already created) with the following uses:
    + _CPFigs_ These are formatted PDF figures for the country profiles for the graphic designer.
    + _FigData_ These are CSV files of the data behind each figure in case specific numbers are needed.
    + _Figs_ These are dated PDF files for sending to the graphic designer.
    + _Review_ These are the latest tables and figures (along with data) that can be reviewed in one place.
    + _Tables_ These are dated HTM files for sending to the graphic designer.


* To create CSV files for the web annex tables, do in R:
```
> source(create_tables_annex_for_web.r)
```

* To re-assemble PDF files for the web annex tables, do in R:
```
> source(split_rename_PDFs_annex_for_web.r)
```


## The set_environment.r file

You need to create this to match your local computing environment. It sets up the paths to input and output folders and the connection string to the database (if available).

Here is a template to use:

```
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Set up the running environment for creating the tables and figures
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Decide whether to take data directly from the database instead of from a .RData file

use_live_db  <- FALSE

# Location and name of .RData file (if used)

rdata_folder <- "D:/Example/rdata/"
rdata_name   <- "data_2015-07-29.Rdata"


# ODBC connection string for the global TB database (if used), where:
# sssss = SQL-Server name on local network
# ddddd = SQL-Server database name
# uuuuu = SQL-Server username to log into the database
# ppppp = SQL-Server password to log into the database

connection_string <- "driver={SQL Server};server=sssssss;database=ddddddd;uid=uuuuuuu;pwd=pppppppp"


# Folder containing output subfolders for tables,figures and maps:

figures_folder  <- "D:/Example/tables_figures/"


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Paths, etc for creating the CSV files for the annex tables to be
# published on the web, and also for splitting/merging PDFs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Parent folder for CSV and PDF files:

annexweb_folder    <- "D:/Example/annex_for_web/"

# Define input/output PDF subfolder locations:

pdforigin_folder <- file.path(annexweb_folder, "PDF_by_table")
pdfoutput_folder <- file.path(annexweb_folder, "PDF_by_region")


# PDFsam folder and DOS launch command:

pdfsamfolder <- "C:/Program Files/PDF Split And Merge Basic/bin/"
pdfsamcmd    <- 'C:\\Progra~2\\Java\\jre7\\bin\\java.exe -Xmx256m -Dlog4j.configuration=console-log4j.xml -classpath  "..\\lib\\pdfsam-console-2.4.3e.jar" org.pdfsam.console.ConsoleClient'


```


