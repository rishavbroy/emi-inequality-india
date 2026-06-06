# Check rendered/public files for placeholder value text and visible crossref
# failures. Final checks require PDF text extraction so broken PDFs cannot pass
# because the local machine lacks an extractor.

args <- commandArgs(trailingOnly = TRUE)
is_final_check <- "--final" %in% args ||
  identical(normalizePath(Sys.getenv("EMI_CONFIG"), mustWork = FALSE), normalizePath("config/final.yml", mustWork = FALSE)) ||
  identical(basename(Sys.getenv("EMI_CONFIG")), "final.yml")

paths <- c(
  list.files("paper", pattern = "\\.(html|md|tex)$", full.names = TRUE),
  list.files("docs", pattern = "\\.(html|md|tex)$", full.names = TRUE),
  list.files("application-samples/.work", pattern = "\\.(html|md)$", full.names = TRUE, recursive = TRUE)
)
paths <- paths[file.exists(paths)]

is_ignored_by_git <- function(path) {
  if (!nzchar(Sys.which("git"))) return(FALSE)
  status <- system2("git", c("check-ignore", "--no-index", "--quiet", path), stdout = FALSE, stderr = FALSE)
  identical(status, 0L)
}

paths <- paths[!vapply(paths, is_ignored_by_git, logical(1))]

source_paths <- c(
  list.files("paper", pattern = "\\.(qmd|md|tex)$", full.names = TRUE),
  list.files("docs", pattern = "\\.(qmd|md|tex)$", full.names = TRUE),
  list.files("application-samples/.work", pattern = "\\.(qmd|md|tex)$", full.names = TRUE, recursive = TRUE)
)
source_paths <- source_paths[file.exists(source_paths)]
source_paths <- source_paths[!vapply(source_paths, is_ignored_by_git, logical(1))]

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
    if (identical(status, 0L) && file.exists(out)) {
      return(paste(readLines(out, warn = FALSE), collapse = "\n"))
    }
  }
  if (requireNamespace("pdftools", quietly = TRUE)) {
    return(paste(pdftools::pdf_text(path), collapse = "\n"))
  }
  NA_character_
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
  "active figures below use district-level empirical distributions"
)

regex_patterns <- c(
  "AME[^\\n\\.;]*=\\s*—",
  "coefficient[^\\n\\.;]*—",
  "p\\s*=\\s*—"
)

source_regex_patterns <- c(
  "\\bFigures?\\s+@fig-",
  "\\bTables?\\s+@tbl-",
  "\\bSec\\.\\s+@sec-",
  "\\bSections?\\s+@sec-"
)

hits <- character()
for (path in source_paths) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  text <- gsub("\\s+", " ", text)
  for (pattern in source_regex_patterns) {
    if (grepl(pattern, text, perl = TRUE)) {
      hits <- c(hits, paste0(path, " matches /", pattern, "/"))
    }
  }
}

for (path in paths) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  text <- gsub("\\s+", " ", text)
  for (pattern in fixed_patterns) {
    if (grepl(pattern, text, fixed = TRUE)) {
      hits <- c(hits, paste0(path, " contains '", pattern, "'"))
    }
  }
  for (pattern in regex_patterns) {
    if (grepl(pattern, text, perl = TRUE)) {
      hits <- c(hits, paste0(path, " matches /", pattern, "/"))
    }
  }
}

pdf_checked <- character()
pdf_skipped <- character()
for (path in pdf_paths) {
  text <- extract_pdf_text(path)
  if (length(text) != 1L || is.na(text)) {
    pdf_skipped <- c(pdf_skipped, path)
    next
  }
  pdf_checked <- c(pdf_checked, path)
  text <- gsub("\\s+", " ", text)
  for (pattern in fixed_patterns) {
    if (grepl(pattern, text, fixed = TRUE)) {
      hits <- c(hits, paste0(path, " contains '", pattern, "'"))
    }
  }
  for (pattern in regex_patterns) {
    if (grepl(pattern, text, perl = TRUE)) {
      hits <- c(hits, paste0(path, " matches /", pattern, "/"))
    }
  }
}

if (length(hits)) {
  cat(paste0("- ", hits, collapse = "\n"), "\n")
  stop("Rendered public text still contains placeholder values or visible cross-reference failures.", call. = FALSE)
}

if (length(pdf_skipped)) {
  msg <- paste0(
    "PDF text extraction unavailable; skipped PDF text checks for: ",
    paste(pdf_skipped, collapse = ", "),
    ". Install poppler/pdftotext or the R package pdftools before running final public checks."
  )
  if (is_final_check) stop(msg, call. = FALSE)
  warning(msg, call. = FALSE)
}

message("No rendered placeholder value text or visible cross-reference failures detected in checked public files.")
