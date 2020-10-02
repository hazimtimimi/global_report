# Functions to get data in long format, eg for sending to adappt or the GHO ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


get_estimates <- function(df, var_name, starting_year = 2000, output_var_name = NA) {

  # Pass a string to identify the variable name and use !!sym() to identify and
  # rename the variables to best, lo, hi
  # (see https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter)

  # Determine if the dataframe is for aggregates or countries
  location_var <- ifelse("iso3" %in% names(df),
                         "iso3",
                         "group_name")

  output <-
    df %>%
      filter(year >= starting_year) %>%
      mutate( # rename variables if requested
             indicator_code = ifelse(is.na(output_var_name),
                                     var_name,
                                     output_var_name)) %>%
      select(indicator_code,
             location_code = !!sym(location_var),
             year,
             value = !!sym(var_name),
             lo = !!sym(paste0(var_name, "_lo")),
             hi = !!sym(paste0(var_name, "_hi")) )

  return(output)

}


get_estimates_agesex <- function(df,
                                 starting_year = notification_maxyear,
                                 measure_filter = "inc",
                                 unit_filter = "num",
                                 age_group_filter = "all",
                                 sex_filter = "a",
                                 risk_factor_filter = "all",
                                 output_var_name) {

  # Records from the age/sex/risk factor tables which are already in long format and need to be
  # queried in a slightly different way
  # I set defaults for all the possible dimensions so when quering only have to specify the
  # particular dimension of interest in the function call

  # Determine if the dataframe is for aggregates or countries
  location_var <- ifelse("iso3" %in% names(df),
                         "iso3",
                         "group_name")

  output <-
    df %>%
      filter(year == starting_year &
               measure == measure_filter &
               unit == unit_filter &
               age_group == age_group_filter &
               sex == sex_filter &
               risk_factor == risk_factor_filter) %>%
      mutate(indicator_code = output_var_name) %>%
      select(indicator_code,
             location_code = !!sym(location_var),
             year,
             value = best,
             lo,
             hi)

  return(output)

}


get_catatrophic_costs <- function(df,
                                  patient_group_filter = "all",
                                  area_type_filter = "a",
                                  output_var_name) {
# Get latest catastrophic costs
# treat differently again.
# First add a new variable which shows the latest year for each country, just in case in future we start to
# get repeat surveys

  output <-
    df %>%
    group_by(iso3) %>%
    mutate(ref_year = last(year),
           indicator_code = output_var_name) %>%
    ungroup() %>%

    # select the latest year for all patient groups and all area types
    filter(year == ref_year &
             patient_group == patient_group_filter &
             area_type == area_type_filter) %>%
    select(indicator_code,
           location_code = iso3,
           year,
           value = catast_pct,
           lo = catast_pct_lo,
           hi = catast_pct_hi)

  return(output)

}

get_external_indicators <- function(df,
                                    indicator_filter = "all",
                                    sex_filter = "a",
                                    flg_latest_year = TRUE,
                                    output_var_name = NA,
                                    round_sig_fig = NA,
                                    round_whole_digit = FALSE) {

# Get external (SDG) data, by default only for the latest year available

  # Adandon function if the dataframe doesn't have the indicator_id field
  if (!("indicator_id" %in% names(df) )) return(NULL)

  output <-
    df %>%
    filter(indicator_id == indicator_filter & sex == sex_filter) %>%
    mutate(# rename variables if requested
           indicator_code = ifelse(is.na(output_var_name),
                                   indicator_filter,
                                   output_var_name)) %>%
    select(indicator_code,
           location_code = iso3,
           year,
           value)

  # Filter to most recent year available if requested
  if (flg_latest_year == TRUE){

    output <-
      output %>%
      group_by(location_code) %>%
      mutate(ref_year = last(year)) %>%
      ungroup() %>%

      # select the latest year for all patient groups and all area types
      filter(year == ref_year) %>%
      # remove the ref_year variable from the output
      select(-ref_year)
  }

  # Round to specified number of significant figures
  if (!is.na(round_sig_fig)){

    output <-
      output %>%
      mutate(value = signif(value, round_sig_fig))
  }

  # Round to nearest whole number if requested
  if (round_whole_digit == TRUE){

    output <-
      output %>%
      mutate(value = round(value, digits=0))
  }


  return(output)

}






get_vars_and_aggregates <- function(df, vars, starting_year = 2000, ending_year = NA, flg_long = TRUE) {

# Get the variables listed in vars and calculate simple sum by WHO region and globally
# Will only work if df contains iso3 and g_whoregion variables

  # Abandon function if the dataframe doesn't have  iso3 and g_whoregion fields
  if (!("iso3" %in% names(df) & "g_whoregion" %in% names(df))) return(NULL)

  output_country <-
    df %>%
    select(location_code = iso3,
           g_whoregion,
           year,
           one_of(vars)) %>%
    filter(year >= starting_year)

if (!is.na(ending_year)) {
  # add an end year filter
  output_country <-
    output_country %>%
    filter(year <= ending_year)
}

  # Calculate aggregates
  # 1. WHO regions
  output_agg_r <-
    output_country %>%
    group_by(g_whoregion, year) %>%
    summarise_at(vars(one_of(vars)),
                 sum,
                 na.rm = TRUE) %>%
    ungroup() %>%
    rename(location_code = g_whoregion )

  # 2. Global aggregates
  output_agg_g <-
    output_country %>%
    group_by(year) %>%
    summarise_at(vars(one_of(vars)),
                 sum,
                 na.rm = TRUE) %>%
    ungroup() %>%
    mutate(location_code = "global" )


  # Combine aggregates
  output_agg <- rbind(output_agg_r, output_agg_g)

  # get rid of g_whoregion from output_country
  output_country <-
    output_country %>%
        select(-g_whoregion)

  # combine country and aggregates
  output <- rbind(output_country, output_agg)

  if (flg_long==TRUE) {

    # melt into long format
    output <-
      output %>%
      gather(key="indicator_code",
             value="value",
             -location_code,
             -year)
  }

  return(output)

}


get_pct <- function(df,
                    numerator_vars,
                    denominator_vars,
                    starting_year = 2000,
                    output_var_name = NA) {

# calculate numerators (as sum of numerator_vars) and denominators (as sum of denominator_vars)
# and then calculate percentage. Do for countries and aggregates. Don't bother about excluding countries
# from the aggregates if they are missing numerators or denominators

  # Adandon function if the dataframe doesn't have  iso3 and g_whoregion fields
  if (!("iso3" %in% names(df) & "g_whoregion" %in% names(df))) return(NULL)

  # Adandon function if the output variable name isn't given
  if (is.na(output_var_name)) return(NULL)

  output_country <-
    df %>%
    select(location_code = iso3,
           g_whoregion,
           year,
           one_of(numerator_vars, denominator_vars)) %>%
    filter(year >= starting_year)

  # calculate numerators and denominators

  output_country$numerator <- sum_of_row(output_country[numerator_vars])
  output_country$denominator <- sum_of_row(output_country[denominator_vars])

  output_country <-
    output_country %>%
    select(location_code,
           g_whoregion,
           year,
           numerator,
           denominator)

  # Calculate aggregates
  # 1. WHO regions
  output_agg_r <-
    output_country %>%
    group_by(g_whoregion, year) %>%
    summarise_at(vars(numerator:denominator),
                 sum,
                 na.rm = TRUE) %>%
    ungroup() %>%
    rename(location_code = g_whoregion )

  # 2. Global aggregates
  output_agg_g <-
    output_country %>%
    group_by(year) %>%
    summarise_at(vars(numerator:denominator),
                 sum,
                 na.rm = TRUE) %>%
    ungroup() %>%
    mutate(location_code = "global" )


  # Combine aggregates
  output_agg <- rbind(output_agg_r, output_agg_g)

  # get rid of g_whoregion from output_country
  output_country <-
    output_country %>%
        select(-g_whoregion)

  # combine country and aggregates
  output <- rbind(output_country, output_agg)


  # Calculate the percentage

  output <-
    output %>%
    mutate(indicator_code = output_var_name) %>%
    mutate(value = display_cap_pct(numerator, denominator)) %>%
    select(indicator_code,
           location_code,
           year,
           value)


  return(output)

}

get_rate <- function(df,
                    numerator_var,
                    population_var,
                    output_var_name = NA) {

# calculate numerator as rate per 100k of population Do for countries and aggregates. Don't bother about excluding countries
# from the aggregates if they are missing numerators or denominators

  # Adandon function if the dataframe doesn't have  iso3 and g_whoregion fields
  if (!("iso3" %in% names(df) & "g_whoregion" %in% names(df))) return(NULL)

  # Adandon function if the output variable name isn't given
  if (is.na(output_var_name)) return(NULL)

  output_country <-
    df %>%
    select(location_code = iso3,
           g_whoregion,
           year,
           one_of(numerator_var, population_var))

  # Calculate aggregates
  # 1. WHO regions
  output_agg_r <-
    output_country %>%
    group_by(g_whoregion, year) %>%
    summarise_at(vars(numerator_var:population_var),
                 sum,
                 na.rm = TRUE) %>%
    ungroup() %>%
    rename(location_code = g_whoregion )

  # 2. Global aggregates
  output_agg_g <-
    output_country %>%
    group_by(year) %>%
    summarise_at(vars(numerator_var:population_var),
                 sum,
                 na.rm = TRUE) %>%
    ungroup() %>%
    mutate(location_code = "global" )


  # Combine aggregates
  output_agg <- rbind(output_agg_r, output_agg_g)

  # get rid of g_whoregion from output_country
  output_country <-
    output_country %>%
        select(-g_whoregion)

  # combine country and aggregates
  output <- rbind(output_country, output_agg)


  # Calculate the rate
  # use !!sym() to refer to the variables in the table
  # (see https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter)

  output <-
    output %>%
    mutate(indicator_code = output_var_name) %>%
    mutate(value = display_num(!!sym(numerator_var) * 1e5 / !!sym(population_var))) %>%
    select(indicator_code,
           location_code,
           year,
           value)


  return(output)

}

