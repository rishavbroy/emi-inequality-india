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


analysis_legacy_chunk_path <- function(filename) {
  analysis_path("archive", "legacy-rmd-chunks", filename)
}

analysis_legacy_comment_lines <- function(filename, from = NULL, to = NULL) {
  path <- analysis_legacy_chunk_path(filename)
  if (!file.exists(path)) {
    return(paste("Missing legacy chunk:", path))
  }
  lines <- readLines(path, warn = FALSE)
  idx <- seq_along(lines)
  if (!is.null(from)) idx <- idx[idx >= from]
  if (!is.null(to)) idx <- idx[idx <= to]
  lines <- lines[idx]
  keep <- grepl("^\\s*#", lines) | !nzchar(trimws(lines))
  lines <- lines[keep]
  lines <- sub("^\\s*# ?", "", lines)
  rle_blank <- rle(!nzchar(trimws(lines)))
  out <- character()
  pos <- 1L
  for (i in seq_along(rle_blank$lengths)) {
    run <- lines[pos:(pos + rle_blank$lengths[[i]] - 1L)]
    if (rle_blank$values[[i]]) {
      out <- c(out, "")
    } else {
      out <- c(out, run)
    }
    pos <- pos + rle_blank$lengths[[i]]
  }
  while (length(out) && !nzchar(trimws(out[[1]]))) out <- out[-1]
  while (length(out) && !nzchar(trimws(out[[length(out)]]))) out <- out[-length(out)]
  out
}

analysis_render_legacy_comments <- function(filename, from = NULL, to = NULL, caption = NULL) {
  if (!is.null(caption)) cat("\n### ", caption, "\n\n", sep = "")
  lines <- analysis_legacy_comment_lines(filename, from = from, to = to)
  cat(paste(lines, collapse = "\n"), "\n\n", sep = "")
}

analysis_deviation_note <- function(text) {
  cat("\n**Deviation note.** ", text, "\n\n", sep = "")
}
