#' Split a DF into two based on a specified variable
#'
#' Used in calculations for [maketable1] and [makedifftables]. This step is not necessary when replicating the tables, but it is used for the extension to compare two different groups.
#'
#' @param df Data frame to split
#' @param split_var String. Name of dummy variable that df should be split on
#' @return Returns a list of two data frames + third item with the name of the split variable
#' @examples
#' # Split DatDLc on gender:
#' test2 <- split_dataframe(datDLc, "gender")
#'
#' @export
split_dataframe <- function(df, split_var) {
  # Remove NAs:
  df_no_na <- df[!is.na(df[[split_var]]), ]
  # Check if the var is binary:
  unique_vals <- unique(df_no_na[[split_var]])
  if (length(unique_vals) != 2) {
    stop("The variable must have exactly two unique values.")
  }
  # Split the dataframe into two:
  df1 <- as.data.frame(df_no_na[df_no_na[[split_var]] == unique_vals[1], ])
  df2 <- as.data.frame(df_no_na[df_no_na[[split_var]] == unique_vals[2], ])
  # Create the list of dataframes with appropriate names:
  df_list <- list()
  df_list[[as.character(unique_vals[1])]] <- df1
  df_list[[as.character(unique_vals[2])]] <- df2
  df_list[["split_var"]] <- split_var
  # Return the list of the new dataframes
  return(df_list)
}
