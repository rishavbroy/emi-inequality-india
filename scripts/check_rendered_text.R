# Check rendered/public files for placeholder value text and visible crossref
# failures. PDF text is checked when an extractor is available; machines without
# pdftotext/pdftools still run the HTML/TeX/Markdown/source checks.

if (file.exists("R/diagnostics/rendered_text_checks.R")) {
  source("R/diagnostics/rendered_text_checks.R")
}

args <- commandArgs(trailingOnly = TRUE)
is_final_check <- "--final" %in% args ||
  identical(normalizePath(Sys.getenv("EMI_CONFIG"), mustWork = FALSE), normalizePath("config/final.yml", mustWork = FALSE)) ||
  identical(basename(Sys.getenv("EMI_CONFIG")), "final.yml")

text_paths <- c(
  list.files("paper", pattern = "\\.(html|md|tex)$", full.names = TRUE),
  list.files("docs", pattern = "\\.(html|md|tex)$", full.names = TRUE),
  list.files("application-samples/.work", pattern = "\\.(html|md|qmd|tex)$", full.names = TRUE, recursive = TRUE)
)
text_paths <- text_paths[file.exists(text_paths)]

source_paths <- c(
  list.files("paper", pattern = "\\.(qmd|md|tex)$", full.names = TRUE),
  list.files("docs", pattern = "\\.(qmd|md|tex)$", full.names = TRUE),
  list.files("application-samples/.work", pattern = "\\.(qmd|md|tex)$", full.names = TRUE, recursive = TRUE)
)
source_paths <- source_paths[file.exists(source_paths)]

pdf_paths <- c(
  "paper/report.pdf",
  "paper/appendix.pdf",
  "docs/district-matching.pdf",
  "docs/long-paths-and-8-3-filenames.pdf",
  list.files("application-samples/output", pattern = "\\.pdf$", full.names = TRUE)
)
pdf_paths <- pdf_paths[file.exists(pdf_paths)]

extract_pdf_text <- function(path) {
  if (nzchar(Sys.which("pdftotext"))) {
    out <- tempfile(fileext = ".txt")
    status <- system2("pdftotext", c(path, out), stdout = FALSE, stderr = FALSE)
    if (identical(status, 0L) && file.exists(out)) return(paste(readLines(out, warn = FALSE), collapse = "\n"))
  }
  if (requireNamespace("pdftools", quietly = TRUE)) return(paste(pdftools::pdf_text(path), collapse = "\n"))
  NA_character_
}

if (!exists("pdf_text_extractor_available", mode = "function")) {
  pdf_text_extractor_available <- function() {
    nzchar(Sys.which("pdftotext")) || requireNamespace("pdftools", quietly = TRUE)
  }
}
if (!exists("pdf_text_skip_message", mode = "function")) {
  pdf_text_skip_message <- function(pdf_paths) {
    paste0(
      "PDF text extractor unavailable; skipped PDF text checks for: ",
      paste(pdf_paths, collapse = ", "),
      ". Install poppler/pdftotext or the R package pdftools to enable PDF text checks. ",
      "HTML, TeX, Markdown, and source text checks still ran."
    )
  }
}
if (!exists("pdf_text_failure_message", mode = "function")) {
  pdf_text_failure_message <- function(pdf_paths) {
    paste0(
      "PDF text extraction failed for: ",
      paste(pdf_paths, collapse = ", "),
      ". Because a PDF text extractor is available, this may indicate a corrupt or unreadable PDF."
    )
  }
}
if (!exists("should_fail_pdf_text_skip", mode = "function")) {
  should_fail_pdf_text_skip <- function(pdf_paths, extractor_available) {
    length(pdf_paths) > 0L && isTRUE(extractor_available)
  }
}

fixed_patterns <- c(
  "not yet available",
  "not run in current draft pipeline",
  "?@",
  "Sec. Section",
  "Section Section",
  "Table Table",
  "Figure Figure",
  "Table ?@",
  "Figure ?@",
  "active figures below use district-level empirical distributions",
  "Draft diagnostic for unavailable"
)

regex_patterns <- c(
  "AME[^\n.;]*=\\s*—",
  "coefficient[^\n.;]*—",
  "p\\s*=\\s*—"
)

source_regex_patterns <- c(
  "\\bFigures?\\s+@fig-",
  "\\bTables?\\s+@tbl-",
  "\\bSec\\.\\s+@sec-",
  "\\bSections?\\s+@sec-"
)

hits <- character()
scan_text <- function(path, text, include_source_patterns = FALSE) {
  text <- gsub("\\s+", " ", text)
  local_hits <- character()
  if (include_source_patterns) {
    for (pattern in source_regex_patterns) {
      if (grepl(pattern, text, perl = TRUE)) local_hits <- c(local_hits, paste0(path, " matches /", pattern, "/"))
    }
  }
  for (pattern in fixed_patterns) {
    if (grepl(pattern, text, fixed = TRUE)) local_hits <- c(local_hits, paste0(path, " contains '", pattern, "'"))
  }
  for (pattern in regex_patterns) {
    if (grepl(pattern, text, perl = TRUE)) local_hits <- c(local_hits, paste0(path, " matches /", pattern, "/"))
  }
  local_hits
}

for (path in source_paths) hits <- c(hits, scan_text(path, paste(readLines(path, warn = FALSE), collapse = "\n"), TRUE))
for (path in text_paths) hits <- c(hits, scan_text(path, paste(readLines(path, warn = FALSE), collapse = "\n"), FALSE))

pdf_skipped <- character()
for (path in pdf_paths) {
  text <- extract_pdf_text(path)
  if (length(text) != 1L || is.na(text)) {
    pdf_skipped <- c(pdf_skipped, path)
    next
  }
  hits <- c(hits, scan_text(path, text, FALSE))
}

if (length(hits)) {
  cat(paste0("- ", hits, collapse = "\n"), "\n")
  stop("Rendered public text still contains placeholder values or visible cross-reference failures.", call. = FALSE)
}

if (length(pdf_skipped)) {
  extractor_available <- pdf_text_extractor_available()
  if (should_fail_pdf_text_skip(pdf_skipped, extractor_available)) {
    stop(pdf_text_failure_message(pdf_skipped), call. = FALSE)
  }
  warning(pdf_text_skip_message(pdf_skipped), call. = FALSE)
}

message("No rendered placeholder value text or visible cross-reference failures detected in checked public files.")
