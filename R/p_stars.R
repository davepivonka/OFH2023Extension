#' Get number of stars OR thresholds for stars
#'
#' Used in [maketable1] and [makedifftables] to construct the legends.
#'
#' @param p_val String or numeric. If numeric, the function outputs the number of stars. If "legend", the output is a full legend for tables. If '***", "**", or "*", the output is the threshold.
#' , the function outputs the threshold for the number of stars.
#' @param stars Vector. Sets the thresholds for the stars. Default is 0.001 for 3 stars, 0.01 for 2, 0.05 for 1.
#' @return Returns either full legend for tables, number of stars, or a specific threshold for a number of stars.
#' @examples
#' p_stars(0.001)
#' p_stars("**")
#' p_stars("legend")
#'
#' @export
p_stars <- function(p_val,stars = c(0.001, 0.01, 0.05)) {
  # For the table:
  if (p_val == "legend") {
    return(paste0("* $p < ",stars[3],"$; ** $p < ",stars[2],"$; *** $p < ",stars[1],"$"))
  }
  # Get values:
  else if (p_val == "***") {
    return(stars[1])
  } else if (p_val == "**") {
    return(stars[2])
  } else if (p_val == "*") {
    return(stars[3])
  } # Get stars:
  else if (p_val < stars[1]) {
    return("***")
  } else if (p_val < stars[2]) {
    return("**")
  } else if (p_val < stars[3]) {
    return("*")
  } else {
    return("")
  }
}
