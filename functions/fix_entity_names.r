# Shorten and correct entity names to show in tables ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_names_for_tables <- function(df, col="country"){

  df[col] <- as.character(df[[col]])
  df[col] <- ifelse(df[[col]]=='Central African Republic', 'Central African Rep',
             ifelse(df[[col]]=='Democratic Republic of the Congo', 'DR Congo',
             ifelse(df[[col]]=='Democratic People\'s Republic of Korea', 'DPR Korea',
             ifelse(df[[col]]=='United Republic of Tanzania', 'UR Tanzania',
             ifelse(df[[col]]=='global', 'Global',
             ifelse(df[[col]]=='SEA', 'SEAR', df[[col]]))))))

  return(df)
}


# Generate a lookup table for regional names to be used for publication ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_region_names <- function(){

  g_whoregion <- c("AFR", "AMR", "EMR", "EUR", "SEA", "WPR")
  entity <- c("Africa", "The Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific")

  return(data.frame(g_whoregion, entity))

}




