# Helpers for checking rendered public text artifacts.

pdf_text_command <- function() {
  unname(Sys.which("pdftotext"))
}

pdf_text_extractor_available <- function(command = pdf_text_command()) {
  length(command) == 1L && !is.na(command) && nzchar(command)
}

extract_pdf_text <- function(path, command = pdf_text_command()) {
  if (!pdf_text_extractor_available(command)) return(NA_character_)

  output <- tempfile(fileext = ".txt")
  on.exit(unlink(output), add = TRUE)
  status <- suppressWarnings(system2(command, c(path, output), stdout = FALSE, stderr = FALSE))
  if (!identical(status, 0L) || !file.exists(output)) return(NA_character_)

  paste(readLines(output, warn = FALSE), collapse = "\n")
}

pdf_text_skip_message <- function(pdf_paths) {
  paste0(
    "PDF text extractor unavailable; skipped PDF text checks for: ",
    paste(pdf_paths, collapse = ", "),
    ". Install Poppler/pdftotext to enable PDF text checks. ",
    "HTML, TeX, Markdown, and source text checks still ran."
  )
}

pdf_text_failure_message <- function(pdf_paths) {
  paste0(
    "PDF text extraction failed for: ",
    paste(pdf_paths, collapse = ", "),
    ". Because pdftotext is available, this may indicate a corrupt or unreadable PDF."
  )
}

should_fail_pdf_text_skip <- function(pdf_paths, extractor_available) {
  length(pdf_paths) > 0L && isTRUE(extractor_available)
}
