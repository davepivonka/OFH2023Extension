#' Generate table 1 or extension
#'
#' Generates a LaTeX table based on Table 1 in Orr, Fowler and Huber (2023) or an extension of it. The table is based on the output from [df_summary] and [extract_diffs]. This function is a part of the [maketable1] function.
#'
#' @param summaries List. Summary table/s from [df_summary]
#' @param diffs List. Differences in means, SEs and p-values from [extract_diffs]
#' @param caption Character. Caption for the table
#' @param label Character. Label for the table
#' @param pos Character. Position of the table
#' @param split.var Character. Variable to split the table by
#' @param custom.table.names Vector. Custom names for the tables
#' @param skip.table.names Logical. Skip the table names
#' @param note Character. Note to add to the table
#' @return Returns a LaTeX table
#'
#' @import dplyr
#'
#' @examples NULL
#'
#' @export
generate_table1 <- function(summaries, diffs, caption = NULL, label = NULL, pos = "tbp", split.var = NULL, custom.table.names = NULL, skip.table.names = FALSE, note = NULL) {

  # Check for the split variable input and set caption + label if not defined:
  if (is.null(split.var)) {
    if (is.null(caption)) caption <- "Policy Agreement vs Party Loyalty"
    if (is.null(label)) label <- "tab:table1"
  } else {
    if (is.null(caption)) caption <- paste0("Policy Agreement vs Party Loyalty by \\texttt{", split.var,"}")
    if (is.null(label)) label <- paste0("tab:table1-", split.var)
  }
  #if (is.null(custom.table.names) & !skip.table.names & is.null(split.var)) custom.table.names <- names(summaries)

  if (is.null(note)) note <- "Averages are reported with standard errors in parentheses. The feeling thermometer ranges from 0--100, while the comfort with someone marrying a child or living nearby range from 1--4."

  # Model lookup - for translation in part 2
  mod_lookup <- list(
    therm = "Feeling thermometer",
    child_marry = "Marriage",
    be_neighbor = "Neighbor"
  )

  # PART 1: Table setup
  table1 <- ""
  table1 <- paste0(
    "\\begin{table}[", pos,"]\n",
    "\\centering\n",
    "\\caption{", caption,"}\n",
    "\\label{", label,"}\n",
    "\\resizebox{\\textwidth}{!}{%\n",
    "\\begin{tabular}{lccc|ccc}\n",
    "\\toprule\n",
    "& \\multicolumn{3}{c|}{\\textbf{Evaluating Copartisans}} & \\multicolumn{3}{c}{\\textbf{Evaluating Out-Partisans}} \\\\",
    "\\cmidrule(r){2-4} \\cmidrule(r){5-7}\n",
    "& Agree Disloyal & Disagree Loyal & Difference & Agree Loyal & Disagree Disloyal & Difference \\\\\n"
  )

  dfname <- 0
  # PART 2: Loop for each split DF + custom table names
  for (df_name in names(summaries)) {
    dfname <- dfname + 1
    if (skip.table.names == TRUE) {
      table1 <- paste0(
        table1,
        "\\midrule\n"
      )
    } else if (!is.null(custom.table.names)) {
      if (length(custom.table.names) != length(names(summaries))) {
        stop("Length of 'custom.table.names' must match the number of values in the variable.")
      } else {
        table1 <- paste0(
          table1,
          "\\midrule\n",
          "\\textbf{", custom.table.names[[dfname]], "} \\\\\n",
          "\\midrule\n"
        )
      }
    } else {
      table1 <- paste0(
        table1,
        "\\midrule\n",
        "\\textbf{", df_name, "} \\\\\n",
        "\\midrule\n"
      )
    }

    # PART 3: Main contents of the table
    for (mod in names(diffs[[df_name]])) {
      table1 <- paste0(
        table1,
        mod_text <- ifelse(mod %in% names(mod_lookup), mod_lookup[[mod]], mod)
      )

      # 3.1 Loop to print both coeffs and SEs
      for (m in c(paste0(mod,"_avg"), paste0(mod, "_se"))) {
        diffsm <- diffs[[df_name]][[mod]]
        table1 <- paste0(
          table1,
          # Using my function 'find.summ' to search the summary tables:
          ## - order: DF, variable, match_party, policy_type, match_policy
          " & ",find.summ(summaries[[df_name]], m, "Yes", "Disloyal", "Yes"), " & ",
          find.summ(summaries[[df_name]], m, "Yes", "Loyal", "No"), " & ",
          ifelse(grepl("_avg", m), paste0(diffsm$Yes$coef,p_stars(diffsm$Yes$p_value)), paste0("(",diffsm$No$se,")")), " & ",

          find.summ(summaries[[df_name]], m, "No", "Loyal", "Yes"), " & ",
          find.summ(summaries[[df_name]], m, "No", "Disloyal", "No"), " & ",

          ifelse(grepl("_avg", m), paste0(diffsm$No$coef,p_stars(diffsm$No$p_value)), paste0("(",diffsm$No$se,")")), " \\\\\n"
        )
      }
    }
    table1 <- paste0(
      table1,
      "Sample Size & ", find.summ(summaries[[df_name]], "n", "Yes", "Disloyal", "Yes"), " & ",
      find.summ(summaries[[df_name]], "n", "Yes", "Loyal", "No"), " & ",
      " & ",
      find.summ(summaries[[df_name]], "n", "No", "Loyal", "Yes"), " & ",
      find.summ(summaries[[df_name]], "n", "No", "Disloyal", "No"), " & \\\\\n"
    )
  }
  table1 <- paste0(
    table1,
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "}\n",
    "\\caption*{\\footnotesize\\textit{Notes:} ", note ,"\\\\\n",
    "\\textsuperscript{\\textdagger} ",p_stars("legend"),".}\n",
    "\\end{table}\n")
  return(table1)
}
