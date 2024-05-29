#' Averages and SEs summary of fata frames
#'
#' Used in the replication and extension of Table 1 in Orr, Fowler and Huber (2023). Outputs summary data frames with averages and SEs grouped in the same way as in the paper.
#'
#' @param df_list Data Frame or list of data frames. If data frame, it will be placed into a list.
#' @return Returns a list of data frames, one DF for each of the ones in the input list. Made to work with output from [split_dataframe].
#' @import dplyr
#' @importFrom stats sd lm
#' @importFrom rlang .data
#' @examples NULL
#'
#' @export
df_summary <- function(df_list) {
  summarised_df_list <- list()
  # Makes it work even with DFs by turning them into lists
  input_list <- list()
  if (class(df_list) == "data.frame") {
    inputname <- deparse(substitute(df_list))
    input_list[[inputname]] <- df_list
  } else if (class(df_list) == "list") {
    input_list <- df_list
  }
  else {
    stop("Input must be a data frame or a list of data frames.")
  }
  # Loop to do it for each DF in the list:
  for (df_name in names(input_list)[names(input_list) != "split.var"]) {
    # Get the DF:
    df <- input_list[[df_name]]
    # Get the averages and SEs. These calculations are directly from the authors of the article:
    summarised_df <- df %>%
      group_by(.data$match_party, .data$policy_type, .data$match_policy) %>%
      summarize(
        # Change from original: Renamed 'ft' to 'therm' for parity with the dataset
        therm_avg = round(mean(.data$therm, na.rm = TRUE), 2),
        therm_se = round(sd(.data$therm, na.rm = TRUE) / sqrt(n()), 2),
        child_marry_avg = round(mean(as.numeric(.data$child_marry), na.rm = TRUE), 2),
        child_marry_se = round(sd(as.numeric(.data$child_marry), na.rm = TRUE) / sqrt(n()), 2),
        be_neighbor_avg = round(mean(as.numeric(.data$be_neighbor), na.rm = TRUE), 2),
        be_neighbor_se = round(sd(as.numeric(.data$be_neighbor), na.rm = TRUE) / sqrt(n()), 2),
        n = n()
      ) %>%
      as.data.frame()
    # Add the summarised DF to the list with the same name:
    summarised_df_list[[df_name]] <- summarised_df
  }
  # Return the list of summarised DFs:
  return(summarised_df_list)
}
