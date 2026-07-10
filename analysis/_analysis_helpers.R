analysis_csv <- function(...) file.path("outputs", ...)

read_analysis_csv <- function(...) {
  path <- analysis_csv(...)
  if (!file.exists(path)) {
    return(data.frame(note = paste("Missing analysis output:", path), stringsAsFactors = FALSE))
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

analysis_table <- function(df, caption = NULL, digits = 3) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(df)) df <- data.frame(note = "No rows in this diagnostic output.", stringsAsFactors = FALSE)
  tab <- knitr::kable(
    df,
    caption = caption,
    digits = digits,
    booktabs = knitr::is_latex_output(),
    longtable = knitr::is_latex_output(),
    row.names = FALSE,
    linesep = ""
  )
  if (knitr::is_latex_output() && requireNamespace("kableExtra", quietly = TRUE)) {
    tab <- kableExtra::kable_styling(tab, latex_options = c("striped", "repeat_header"), full_width = FALSE, font_size = 9)
  }
  tab
}
