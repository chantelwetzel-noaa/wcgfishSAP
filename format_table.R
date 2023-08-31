#' Function to join the given table with the table that has the species management
#' groups. Replaces underscores in column names with spaces.
#'
#' @param df1 A factor dataframe
#' @param df2 A dataframe containing the species management groups
#' @returns Joined dataframe with edited column names

format_table <- function(df1, df2) {
  # join table + species mgmt. groups
  joined_df <- left_join(df1, df2, by = c("Species" = "speciesName"))
  
  # rename columns + arrange in ascending rank
  joined_df <- joined_df %>%
    rename_with(~gsub("_", " ", colnames(joined_df)))
  
  return(joined_df)
}