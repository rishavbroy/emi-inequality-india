# Check rendered/public files for placeholder value text and visible crossref
# failures. PDF text is checked when pdftotext is available; other text checks
# still run when the system extractor is absent.

source("R/diagnostics/rendered_text_checks.R")

args <- commandArgs(trailingOnly = TRUE)
is_final_check <- "--final" %in% args ||
  identical(normalizePath(Sys.getenv("EMI_CONFIG"), mustWork = FALSE), normalizePath("config/final.yml", mustWork = FALSE)) ||
  identical(basename(Sys.getenv("EMI_CONFIG")), "final.yml")

is_false_env <- function(name, default = "true") {
  tolower(trimws(Sys.getenv(name, default))) %in% c("0", "false", "no", "off")
}
check_application_samples <- !is_false_env("EMI_REQUIRE_APPLICATION_SAMPLES", Sys.getenv("EMI_RENDER_APPLICATION_SAMPLES", "true"))

text_paths <- c(
  list.files("paper", pattern = "\\.(html|md|tex)$", full.names = TRUE),
  list.files("docs", pattern = "\\.(html|md|tex)$", full.names = TRUE)
)
if (check_application_samples) {
  text_paths <- c(
    text_paths,
    list.files("application-samples/.work", pattern = "\\.(html|md|qmd|tex)$", full.names = TRUE, recursive = TRUE)
  )
}
text_paths <- text_paths[file.exists(text_paths)]

source_paths <- c(
  list.files("paper", pattern = "\\.(qmd|md|tex)$", full.names = TRUE),
  list.files("docs", pattern = "\\.(qmd|md|tex)$", full.names = TRUE)
)
if (check_application_samples) {
  source_paths <- c(
    source_paths,
    list.files("application-samples/.work", pattern = "\\.(qmd|md|tex)$", full.names = TRUE, recursive = TRUE)
  )
}
source_paths <- source_paths[file.exists(source_paths)]

pdf_paths <- c(
  "paper/report.pdf",
  "paper/appendix.pdf",
  "docs/district-matching.pdf",
  "docs/long-paths-and-8-3-filenames.pdf"
)
if (check_application_samples) {
  pdf_paths <- c(
    pdf_paths,
    list.files("application-samples/output", pattern = "\\.pdf$", full.names = TRUE)
  )
}
pdf_paths <- pdf_paths[file.exists(pdf_paths)]

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

if (!check_application_samples) {
  message("Rendered text checks skipped application-sample files because EMI_REQUIRE_APPLICATION_SAMPLES=false.")
}
message("No rendered placeholder value text or visible cross-reference failures detected in checked public files.")
