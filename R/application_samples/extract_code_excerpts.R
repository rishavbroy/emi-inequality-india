# Utilities for extracting coding-sample excerpts from R source files.
# Code excerpts are marked with comments like:
# # sample-start: code-census-geospatial-import
# ...
# # sample-end: code-census-geospatial-import

#' Extract multiple code excerpts from a YAML spec
#'
#' @param spec Parsed YAML list with an `excerpts` element.
#' @return Character vector of Quarto markdown lines containing code excerpts.
extract_code_excerpts <- function(spec) {
  pieces <- lapply(spec$excerpts, function(x) {
    file <- x$file
    id <- x$id
    title <- x$title %||% id
    code <- extract_between_sample_markers(file, id)
    c(
      "",
      paste0("## ", title),
      "",
      paste0("Source: `", file, "`"),
      "",
      "```{r}",
      "#| eval: false",
      "#| echo: true",
      code,
      "```",
      ""
    )
  })
  unlist(pieces, use.names = FALSE)
}

#' Extract one code excerpt by marker ID
#'
#' @param file Path to R source file.
#' @param id Marker ID.
#' @return Character vector of source lines between start/end markers.
extract_between_sample_markers <- function(file, id) {
  if (!file.exists(file)) stop("Code excerpt file does not exist: ", file, call. = FALSE)
  text <- readLines(file, warn = FALSE)
  start_pat <- paste0("^\\s*#\\s*sample-start:\\s*", gsub("([\\W])", "\\\\\\1", id), "\\s*$")
  end_pat <- paste0("^\\s*#\\s*sample-end:\\s*", gsub("([\\W])", "\\\\\\1", id), "\\s*$")
  start <- grep(start_pat, text)
  end <- grep(end_pat, text)
  if (length(start) != 1L || length(end) != 1L || end <= start) {
    stop("Could not find a unique valid code excerpt marker pair for ID: ", id, call. = FALSE)
  }
  text[(start + 1L):(end - 1L)]
}

#' Assemble a temporary coding-sample QMD
#'
#' @param cover_note Path to cover-note QMD.
#' @param body Character vector of code excerpt lines.
#' @param output_qmd Path to write assembled QMD.
#' @return `output_qmd` invisibly.
assemble_coding_sample_qmd <- function(cover_note, body, output_qmd) {
  cover <- if (!is.null(cover_note) && file.exists(cover_note)) readLines(cover_note, warn = FALSE) else character()
  cover <- normalize_coding_sample_yaml(cover)
  lines <- c(cover, "", "\\newpage", "", body)
  dir.create(dirname(output_qmd), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, output_qmd)
  invisible(output_qmd)
}

normalize_coding_sample_yaml <- function(lines) {
  if (!length(lines) || !identical(lines[[1]], "---")) return(lines)
  close <- which(lines[-1L] == "---")
  if (!length(close)) return(lines)
  end <- close[[1]] + 1L
  yaml <- lines[seq_len(end)]
  rest <- lines[-seq_len(end)]

  if (!any(grepl("^format:", yaml))) {
    yaml <- append(yaml, c("format:", "  pdf:", "    pdf-engine: xelatex"), after = end - 1L)
    end <- end + 3L
  }
  if (!any(grepl("^    pdf-engine:", yaml))) {
    pdf_idx <- grep("^  pdf:\\s*$", yaml)
    if (length(pdf_idx)) yaml <- append(yaml, "    pdf-engine: xelatex", after = pdf_idx[[1]])
  }
  if (!any(grepl("^include-in-header:", yaml))) {
    yaml <- append(yaml, c(
      "include-in-header:",
      "  text: |",
      "    \\usepackage{fvextra}",
      "    \\DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines,breakanywhere,commandchars=\\\\\\{\\}}"
    ), after = length(yaml) - 1L)
  }
  if (!any(grepl("^highlight-style:", yaml))) {
    yaml <- append(yaml, "highlight-style: default", after = length(yaml) - 1L)
  }
  c(yaml, rest)
}

#' Validate code excerpt markers listed in a spec
#'
#' @param spec Parsed YAML list.
#' @return TRUE invisibly.
validate_code_excerpt_markers <- function(spec) {
  invisible(lapply(spec$excerpts, function(x) extract_between_sample_markers(x$file, x$id)))
  invisible(TRUE)
}
