# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Script to produce files to send to the UNSDG database and
# to the UN Regional Economic Commissions using Philippe Glaziou's addXY() function
#
# This replaces the old method of generating these estimates using
# T-SQL and the normal approximation which did all the work in the
# SQL-Server database and where the estimated standard deviation had
# not been imported. Therefore use the est.rda file of estimates
#
# Hazim Timimi, December 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Set environment variables ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Establish the report year
report_year <- 2024

# Descriptions UN Statistics Division asks to be included in the file sent to them

unsd_Indicator <- "3.3.2"
unsd_SeriesID <- 2875
unsd_SeriesDescription <- "Tuberculosis incidence (per 100,000 population)"
unsd_Source <- paste("WHO Global Tuberculosis Report",
                     report_year,
                     "(https://www.who.int/teams/global-tuberculosis-programme/data)")
unsd_FootNote <- paste("Data extracted as of",
                       format(Sys.Date(), "%Y-%m-%d"),
                       "based on data originally compiled for the",
                       report_year,
                       "WHO Global TB Report")

unsd_Nature_region <- "N"
unsd_Nature_country <- "G"

unsd_Units <- "PER_100000_POP"
unsd_Reporting_Type <- "G"
unsd_SeriesCode <- "SH_TBS_INCD"

unsd_Type_region <- "Region"
unsd_Type_country <- "Country"


# PG's enigmatic 100k constant
m <- 1e5

# Get paths for folders. These will be particular to each person so this file is in the Git ignore list.
# The file defines locations on the local machine for:
# - report_git_repo
# - unsd_folder
# - unsd_rc_folder


source("set_environment_UNSDG.r")

# Define path to estimates and country name files in the latest report git repo
git_repo_est_path <- paste0(report_git_repo, "inc_mort/analysis/est.rda")
git_repo_cty_path <- paste0(report_git_repo, "data/gtb/other/cty.rda")

# Define filenames for group configurations sent by UNSD
# 1. SDG groups
unsd_groups_file <- "2. Regional Groupings and Compositions-2025Reporting_20241219.xlsx"
unsd_groups_file_sheet <- "Group Composition (List View)"

# 2. Regional Economic Commissions groups
unsd_rc_groups_file <- "9. CompositionOfRegions_RCs_20241219.xlsx"
unsd_rc_groups_file_sheet = "Ref_Area_Long"

# Define base name to use for output files
output_file_base_name <- paste0("138-3.3.2-2875-SH_TBS_INCD-5400_",
                                format(Sys.Date(), "%Y-%m-%d"),
                                ".xlsx")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(data.table)
library(readxl)
library(dplyr)
library(gtbreport)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Define functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Calculate aggregated rates
# Uses weighted means and uncertainty intervals using beta distribution
# (from Philippe Glaziou)
addXY <-
  function (r,
            r.lo,
            r.hi,
            r.sd,
            weights = 1,
            method = "beta")  {
    #' @param r rates
    #' @param r.lo low bounds
    #' @param r.hi high bounds
    #' @param r.sd SDs
    #' @param weights weights
    #' @param method distribution of rates, defaults to beta else normal
    #' @export
    if (is.null(r) || length(r) == 0)
      stop("Error: r must contain at least one value")
    if (sum(r < 0 & !is.na(r) & method == "beta"))
      stop("Error: r must be positive with method 'beta'")
    if (sum(r > 1 & !is.na(r) & method == "beta"))
      stop("Error: r must be between 0 and 1 with method 'beta'")
    if (missing(r.sd))
      r.sd <- (r.hi - r.lo) / 4
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
      v <- r.sd ^ 2
    else
      v <- ((r.hi - r.lo) / 4) ^ 2
    sw <- ifelse(length(weights) > 1, sum(weights[!is.na(r)],
                                          na.rm = TRUE), 1)
    out.m <- sum(r * weights, na.rm = TRUE) / sw
    out.v <-
      ifelse(length(weights) > 1,
             sum(v[!is.na(r)] * weights[!is.na(r)] ^ 2,
                 na.rm = TRUE) / sw ^ 2,
             sum(v))
    if (method == "beta") {
      S <- (out.m * (1 - out.m) / out.v) - 1
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
      return(data.frame(
        r = out.m,
        r.lo = lo,
        r.hi = hi,
        r.sd = sqrt(out.v)
      ))
    else
      return(
        data.frame(
          r = out.m,
          r.lo = lo,
          r.hi = hi,
          r.sd = sqrt(out.v),
          r.num = out.m * sw,
          r.lo.num = lo * sw,
          r.hi.num = hi *
            sw,
          pop = as.double(sw)
        )
      )
  }


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load Estimates, country names and SDG groups ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# load estimates and country names from the report git repo
load(git_repo_est_path)
load(paste0(report_git_repo, "data/gtb/other/cty.rda"))


# Load SDG groupings from file sent by the UN Statistics Division
sdg_groups_detailed <- read_xlsx(path = paste0(unsd_folder, unsd_groups_file),
                        sheet = unsd_groups_file_sheet)

sdg_groups <- sdg_groups_detailed |>
  dplyr::select(iso3 = `ISO Code`,
                group_code = `M49 Code(region)`)

sdg_group_names <- sdg_groups_detailed |>
  dplyr::select(group_code = `M49 Code(region)`,
                group_name = `Region Name`) |>
  dplyr::distinct()

rm(sdg_groups_detailed)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Produce incidence aggregates by SDG groups ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Need a many to mny join between country estimates and the SDG groups
# because of the mish-mash of overlapping groups used by the UNSD
# Keep working in data.table because PG's functions assume this
est_sdg <- merge(est, sdg_groups,
                 by = 'iso3',
                 allow.cartesian = TRUE)

# Calculate aggregates by UNSD group codes
inc_sdg <-
  est_sdg[, addXY(inc / m, r.sd = inc.sd / m, weights = pop), by = c("group_code", "year")]

setnames(
  inc_sdg,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc',
    'inc.lo',
    'inc.hi',
    'inc.sd',
    'inc.num',
    'inc.lo.num',
    'inc.hi.num',
    'pop'
  )
)


# Now safe to switch to dplyr commands to produce the output for UNSD
inc_sdg <- inc_sdg |>
  mutate(Value = gtbreport::ftb(inc * m),
         UpperBound = gtbreport::ftb(inc.hi * m),
         LowerBound = gtbreport::ftb(inc.lo * m),
         Indicator = unsd_Indicator,
         SeriesID = unsd_SeriesID,
         SeriesDescription = unsd_SeriesDescription,
         Source = unsd_Source,
         FootNote = unsd_FootNote,
         Nature = unsd_Nature_region,
         Units = unsd_Units,
         `Reporting Type` = unsd_Reporting_Type,
         SeriesCode = unsd_SeriesCode,
         ISOalpha3 = "",
         Type = unsd_Type_region
         ) |>
  inner_join(sdg_group_names, by = "group_code") |>
  select(Indicator,
         SeriesID,
         SeriesDescription,
         GeoAreaCode = group_code,
         GeoAreaName = group_name,
         TimePeriod = year,
         Value,
         Time_Detail = year,
         UpperBound,
         LowerBound,
         Source,
         FootNote,
         Nature,
         Units,
         `Reporting Type`,
         ISOalpha3,
         Type,
         SeriesCode) |>
  arrange(GeoAreaCode, TimePeriod)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Produce formatted incidence by country ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


inc_cty <- est |>
  mutate(Value = gtbreport::ftb(inc),
         UpperBound = gtbreport::ftb(inc.hi),
         LowerBound = gtbreport::ftb(inc.lo),
         Indicator = unsd_Indicator,
         SeriesID = unsd_SeriesID,
         SeriesDescription = unsd_SeriesDescription,
         Source = unsd_Source,
         FootNote = unsd_FootNote,
         Nature = unsd_Nature_country,
         Units = unsd_Units,
         `Reporting Type` = unsd_Reporting_Type,
         SeriesCode = unsd_SeriesCode,
         Type = unsd_Type_country
  ) |>
  inner_join(cty, by = "iso3") |>
  select(Indicator,
         SeriesID,
         SeriesDescription,
         GeoAreaCode = iso.numeric,
         GeoAreaName = country,
         TimePeriod = year,
         Value,
         Time_Detail = year,
         UpperBound,
         LowerBound,
         Source,
         FootNote,
         Nature,
         Units,
         `Reporting Type`,
         ISOalpha3 = iso3,
         Type,
         SeriesCode) |>
  arrange(GeoAreaCode, TimePeriod)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Combine and save the SDG group and country incidence estimates ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Combine the two sets of estimates
inc <- rbind(inc_sdg, inc_cty)

# Write the result to Excel in a sheet called "Data"
writexl::write_xlsx(list(Data = inc),
                    path = paste0(unsd_folder, output_file_base_name),
                    format_headers = FALSE)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load Regional Economic Commissions groups ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Following a similar process for the Regional Econmic Commissions as for SDG groups

# Load RC groupings from file sent by the UN Statistics Division
rc_groups_detailed <- read_xlsx(path = paste0(unsd_rc_folder, unsd_rc_groups_file),
                                 sheet = unsd_rc_groups_file_sheet)


rc_groups <- rc_groups_detailed |>
  dplyr::select(iso.numeric = M49,
                group_code = RC_UNSDCode) |>
  dplyr::inner_join(cty, by = "iso.numeric") |>
  dplyr::select(iso3,
                group_code)

rc_group_names <- rc_groups_detailed |>
  dplyr::select(group_code = RC_UNSDCode,
                group_name = RC_RegionName,
                RC_Name) |>
  dplyr::distinct()

rm(rc_groups_detailed)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Produce incidence aggregates by RC groups ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Need a many to mny join between country estimates and the SDG groups
# because of the mish-mash of overlapping groups used by the UNSD
# Keep working in data.table because PG's functions assume this
est_rc <- merge(est, rc_groups,
                by = 'iso3',
                allow.cartesian = TRUE)

# Calculate aggregates by UNSD group codes
inc_rc <-
  est_rc[, addXY(inc / m, r.sd = inc.sd / m, weights = pop), by = c("group_code", "year")]

setnames(
  inc_rc,
  c(
    "r",
    "r.lo",
    "r.hi",
    "r.sd",
    "r.num",
    "r.lo.num",
    "r.hi.num",
    "pop"
  ),
  c(
    'inc',
    'inc.lo',
    'inc.hi',
    'inc.sd',
    'inc.num',
    'inc.lo.num',
    'inc.hi.num',
    'pop'
  )
)


# Now safe to switch to dplyr commands to produce the output for UNSD
inc_rc <- inc_rc |>
  mutate(Value = gtbreport::ftb(inc * m),
         UpperBound = gtbreport::ftb(inc.hi * m),
         LowerBound = gtbreport::ftb(inc.lo * m),
         Indicator = unsd_Indicator,
         SeriesID = unsd_SeriesID,
         SeriesDescription = unsd_SeriesDescription,
         Source = unsd_Source,
         FootNote = unsd_FootNote,
         Nature = unsd_Nature_region,
         Units = unsd_Units,
         `Reporting Type` = unsd_Reporting_Type,
         SeriesCode = unsd_SeriesCode,
         ISOalpha3 = "",
         Type = unsd_Type_region
  ) |>
  inner_join(rc_group_names, by = "group_code") |>
  select(Indicator,
         SeriesID,
         SeriesDescription,
         GeoAreaCode = group_code,
         GeoAreaName = group_name,
         TimePeriod = year,
         Value,
         Time_Detail = year,
         UpperBound,
         LowerBound,
         Source,
         FootNote,
         Nature,
         Units,
         `Reporting Type`,
         Type,
         SeriesCode,
         RC_Name) |>
  arrange(GeoAreaCode, TimePeriod)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Save the RC group incidence estimates, one per regional commission ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Define a quick function to avoid repeating code
# Because we need to filter on regional commission name to create each file
save_rc <- function(x, rc_name){

  x <- x |>
    filter(RC_Name == rc_name) |>
    select(-RC_Name)

  # Write the result to Excel in a sheet called "Data"
  writexl::write_xlsx(list(Data = x),
                      path = paste0(unsd_rc_folder,
                                    rc_name,
                                    "_",
                                    output_file_base_name),
                      format_headers = FALSE)
}

# Write the results for each regional commission
save_rc(inc_rc, "ECA")
save_rc(inc_rc, "ECE")
save_rc(inc_rc, "ECLAC")
save_rc(inc_rc, "ESCAP")
save_rc(inc_rc, "ESCWA")



