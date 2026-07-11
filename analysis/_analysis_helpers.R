analysis_project_root <- function(start = getwd()) {
  here <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(here, "_targets.R")) && dir.exists(file.path(here, "analysis"))) {
      return(here)
    }
    parent <- dirname(here)
    if (identical(parent, here)) {
      stop("Could not locate project root from ", start, call. = FALSE)
    }
    here <- parent
  }
}

analysis_path <- function(...) file.path(analysis_project_root(), ...)
analysis_csv <- function(...) analysis_path("outputs", ...)

read_analysis_csv <- function(...) {
  path <- analysis_csv(...)
  if (!file.exists(path)) {
    return(data.frame(note = paste("Missing analysis output:", path), stringsAsFactors = FALSE))
  }

  if (file.info(path)$size <= 3L) {
    return(data.frame(note = paste("No rows in analysis output:", path), stringsAsFactors = FALSE))
  }

  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      data.frame(
        note = paste("Could not read analysis output:", path),
        reason = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    }
  )
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
