# Helpers for checking rendered public text artifacts.

pdf_text_extractor_available <- function() {
  nzchar(Sys.which("pdftotext")) || requireNamespace("pdftools", quietly = TRUE)
}

pdf_text_skip_message <- function(pdf_paths) {
  paste0(
    "PDF text extractor unavailable; skipped PDF text checks for: ",
    paste(pdf_paths, collapse = ", "),
    ". Install poppler/pdftotext or the R package pdftools to enable PDF text checks. ",
    "HTML, TeX, Markdown, and source text checks still ran."
  )
}

pdf_text_failure_message <- function(pdf_paths) {
  paste0(
    "PDF text extraction failed for: ",
    paste(pdf_paths, collapse = ", "),
    ". Because a PDF text extractor is available, this may indicate a corrupt or unreadable PDF."
  )
}

should_fail_pdf_text_skip <- function(pdf_paths, extractor_available) {
  length(pdf_paths) > 0L && isTRUE(extractor_available)
}
