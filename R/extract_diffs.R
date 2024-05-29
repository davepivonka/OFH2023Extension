#' Statistical significance of differences
#'
#' Takes a data frame or a list of data frames (created by [split_dataframe]), and returns a list of coefficients (difference between the averages), the SEs and p-values, used to assess statistical significance of the differences between groups.
#'
#' @param df_list Data frame or list of data frames. If data frame, it will be placed into a list and the output will be identical to that in the article by Orr, Fowler and Huber (2023).
#' @return Returns a list of coefficients, SEs and p-values for each of the variables in the data frames. These are grouped based on whether the respondent is evaluating a copartisan or an out-partisan.
#' @import dplyr
#' @examples NULL
#'
#' @export
extract_diffs <- function(df_list) {
  results <- list()
  input_list <- list()
  model.vars = c("therm", "child_marry", "be_neighbor")
  match_values <- c("Yes", "No")
  # Makes it work even with DFs by turning them into lists
  if (class(df_list) == "data.frame") {
    inputname <- deparse(substitute(df_list))
    input_list[[inputname]] <- df_list
  } else if (class(df_list) == "list") {
    input_list <- df_list
  }
  else {
    stop("Input must be a data frame or a list of data frames.")
  }
  # Loop for each DF name:
  for (df_name in names(input_list)[names(input_list) != "split.var"]) {
    df <- input_list[[df_name]]
    df_results <- list()
    # Loop for each model and match value:
    for (mod in model.vars) {
      mod_results <- list()
      for (match_value in match_values) {
        lm_result <- summary(lm(as.numeric(df[mod][df$match_party == match_value, ]) ~ df$policy_type[df$match_party == match_value]))
        coef <- round(lm_result$coefficients[2, 1], 2)
        if (match_value == "Yes") {
          coef <- coef * (-1)
        }
        se <- round(lm_result$coefficients[2, 2], 2)
        p_value <- lm_result$coefficients[2, 4]
        mod_results[[match_value]] <- list(coef = coef, se = se, p_value = p_value)
      }
      df_results[[mod]] <- mod_results
    }
    results[[df_name]] <- df_results
  }
  return(results)
}
