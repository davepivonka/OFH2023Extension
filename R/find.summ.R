#' Search table with SEs and averages
#'
#' Searches the output of [df_summary] for a specific value. Used in [maketable1]. This function was only made to save space in the main script.
#'
#' @param summ Summary table to search
#' @param var Variable of affect to search for
#' @param match_party Value of 'match_party' to search for.
#' @param policy_type Value of 'policy_type' to search for.
#' @param match_policy Value of 'match_policy' to search for.
#' @return Returns the value in the table
#'
#' @import dplyr
#'
#' @examples NULL
#'
#' @export
find.summ <- function(summ, var, match_party, policy_type, match_policy) {
  value <- summ[summ$`match_party` == match_party & summ$`match_policy` == match_policy & summ$`policy_type` == policy_type, var]
  # When 'var' ends ends with '_se'
  if (grepl("_se$", var)) { value <- paste0("(", value, ")") }
  return(value)
}
