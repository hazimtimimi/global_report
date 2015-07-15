# Scripts to generate tables, figures and maps for the global TB report
*(Shared by Tom and Hazim, so either can run regardless of which office they are sitting in.)*

## Use

1. Make sure you have your version of `get_tables_figures_environment.r` set up to load TB data and to establish where outpute files will be saved.

2. Then, in R, do:

```
> source(create_tables_figures.r)
```

## Output
The script will create a series of folders (or add to them if they are already created) with the following uses.

* *CPFigs* These are formatted PDF figures for the country profiles for the graphic designer.
* *FigData* These are CSV files of the data behind each figure in case specific numbers are needed.
* *Figs* These are dated PDF files for sending to the graphic designer.
* *Review* These are the latest tables and figures (along with data) that can be reviewed in one place.
* *Tables* These are dated HTM files for sending to the graphic designer.
