#' Combined tables of differences
#'
#' Combines multiple tables from [maketable1] into one - but it only contains the differences and their statistical significance, which are the most important part of Table 1 in Orr, Fowler and Huber (2023). This function uses [extract_diffs] and [split_dataframe] to generate the differences between two groups of respondents in the data set.
#'
#' @param df Data frame
#' @param tables.list List containing information about the tables. The structure is: list( list( split.var = "", table.name = "", values = list("Value in "DF = "Display Value")),list(...))). So it is a list of list with information about what we want to do and what the final table should look like. None of the values are required, but allow significantly better output.
#' @param caption Character. Caption for the table
#' @param label Character. Label for the table
#' @param pos Character. Position of the table
#' @param skip.table.names Logical. Skip the table names
#' @param note Character. Note to add to the table
#' @return Returns a LaTeX table.
#' @examples NULL
#'
#' @import dplyr
#'
#' @export
makedifftables <- function(df, tables.list = NULL, caption = "Policy Agreement vs Party Loyalty by Variable", label = "tab:allTables", pos = "!htbp", skip.table.names = FALSE, note = NULL) {

  # Table rows, used in a loop:
  mod_list <- list(
    therm = "Thermometer",
    child_marry = "Marriage",
    be_neighbor = "Neighbour"
  )

  match_values = c("Yes","No")

  # Set note if null:
  if (is.null(note)) note <- "Differences of averages are reported with standard errors in parentheses. The feeling thermometer ranges from 0--100, while the comfort with someone marrying a child or living nearby range from 1--4."

  # TABLE SETUP:
  diffstable <- ""
  diffstable <- paste0(
    "\\begin{table}[", pos,"]\n",
    "\\centering\n",
    "\\caption{", caption,"}\n",
    "\\label{", label,"}\n",
    "\\begin{tabular}{lcccccc}\n",
    "\\toprule\n",
    "& \\multicolumn{2}{c}{\\textbf{",mod_list[[1]],"}} & \\multicolumn{2}{c}{\\textbf{",mod_list[[2]],"}} & \\multicolumn{2}{c}{\\textbf{",mod_list[[3]],"}}\\\\\n",
    "\\cmidrule(r){2-3} \\cmidrule(r){4-5} \\cmidrule(r){6-7}\n",
    "& Co- & Out- & Co- & Out- & Co- & Out- \\\\\n"
  )

  # Loop for each item in the list:
  for (item in tables.list) {
    if (is.null(item$split.var)) {
      splitdf <- df
    } else {
      splitdf <- split_dataframe(df, item$split.var)
    }
    diffs <- extract_diffs(splitdf)

    # Check skip.table.names:
    if (!skip.table.names) {
      # Check if there is split.var
      if (is.null(item$split.var) & is.null(item$table.name)) {
      } else {
        if (is.null(item$table.name)) {
          item$table.name <- item$split.var
        }
        # Add the table name:
        diffstable <- paste0(
          diffstable,
          "\\addlinespace[-0.1pt]\\textbf{",item$table.name,"} \\\\\n",
          "\\addlinespace[-0.3pt]\\midrule\n"
        )
      }
    }

    # ADD ALL VALUES IN TABLE:
    for (var in names(diffs)) {

      # ADD VALUE NAMES:
      # - first checks for custom ones
      if (is.null(item$split.var) & !is.null(item$values[[1]])) {
        diffstable <- paste0(
          diffstable,
          item$values[[1]]
        )
      } else if (!is.null(item$values[[var]])) {
        diffstable <- paste0(
          diffstable,
          item$values[[var]]
        )
      } else {
        diffstable <- paste0(
          diffstable,
          var
        )
      }

      # ADD COEFFICIENTS
      for (mod in names(mod_list)) {
        for (val in match_values) {
          diffstable <- paste0(
            diffstable,
            " & ", diffs[[var]][[mod]][[val]]$coef,p_stars(diffs[[var]][[mod]][[val]]$p_value)
          )
        }
      }
      # End the row:
      diffstable <- paste0(diffstable, " \\\\\n")

      # ADD SEs:
      for (mod in names(mod_list)) {
        for (val in match_values) {
          diffstable <- paste0(
            diffstable,
            " & (",diffs[[var]][[mod]][[val]]$se,")"
          )
        }
      }
      # End the row:
      diffstable <- paste0(diffstable, " \\\\\n")
    }
  }

  # END THE TABLE:
  diffstable <- paste0(
    diffstable,
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\caption*{\\footnotesize\\textit{Notes:} ", note ," \\\\\n",
    "\\textsuperscript{\\textdagger} ",p_stars("legend"),".}\n",
    "\\end{table}\n"
  )
  cat(diffstable)
}
