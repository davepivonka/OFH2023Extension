#' Replicate Table 1 from the paper
#'
#' Requires the booktabs LaTeX package. Allows both for identical replication, and extension of Table 1 from Orr, Fowler and Huber (2023). The function can be used to generate the table for the entire dataset, or for a split dataset based on a specified variable. This function combines [df_summary], [extract_diffs], and [generate_table1] and automatically ties them together.
#'
#' @param df Data frame.
#' @param split.var Variable that the dataframe should be split on before generating the table. If NULL, the table will be generated for the entire dataset.
#' @param caption Character. Caption for the table.
#' @param label Character. Label for the table.
#' @param pos Character. Position of the table.
#' @param custom.table.names Vector. Custom names for the tables.
#' @param skip.table.names Logical. Skip the table names.
#' @param note Character. Note to add to the table.
#' @return Returns a LaTeX table.
#' @examples
#' example_code()
#'
#' @export
maketable1 <- function(df, caption = NULL, label = NULL, pos = "tbp", split.var = NULL, custom.table.names = NULL, skip.table.names = FALSE, note = NULL) {
  if (is.null(custom.table.names) & !skip.table.names & is.null(split.var)) custom.table.names <- deparse(substitute(df))
  if (!is.null(split.var)) {
    # 1. Split the DF:
    df.split <- split_dataframe(df, split.var)
    # 2.
    df.summaries <- df_summary(df.split)
    # 3.
    df.diffs <- extract_diffs(df.split)
  }  else {
    # 2.
    df.summaries <- df_summary(df)
    # 3.
    df.diffs <- extract_diffs(df)
  }
  # 4.
  table1 <- generate_table1(summaries = df.summaries, diffs = df.diffs, split.var = split.var, caption = caption, label = label, pos = pos,skip.table.names = skip.table.names, custom.table.names = custom.table.names)
  # 5.
  return(table1)
}
