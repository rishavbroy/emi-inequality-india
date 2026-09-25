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

pdf_info_command <- function() {
  unname(Sys.which("pdfinfo"))
}

pdf_info_available <- function(command = pdf_info_command()) {
  length(command) == 1L && !is.na(command) && nzchar(command)
}

parse_pdf_page_layout <- function(info) {
  info <- as.character(info)
  size_lines <- grep("^Page\\s+[0-9]+\\s+size:", info, value = TRUE)
  if (!length(size_lines)) {
    return(data.frame(page = integer(), width = numeric(), height = numeric(), rotation = integer()))
  }

  sizes <- do.call(rbind, lapply(size_lines, function(line) {
    match <- regexec(
      "^Page\\s+([0-9]+)\\s+size:\\s*([0-9.]+)\\s+x\\s+([0-9.]+)\\s+pts",
      line,
      perl = TRUE
    )
    parts <- regmatches(line, match)[[1]]
    if (length(parts) != 4L) return(NULL)
    c(page = as.integer(parts[[2]]), width = as.numeric(parts[[3]]), height = as.numeric(parts[[4]]))
  }))
  if (is.null(sizes) || !nrow(sizes)) {
    return(data.frame(page = integer(), width = numeric(), height = numeric(), rotation = integer()))
  }

  out <- data.frame(
    page = as.integer(sizes[, "page"]),
    width = as.numeric(sizes[, "width"]),
    height = as.numeric(sizes[, "height"]),
    rotation = 0L
  )
  rot_lines <- grep("^Page\\s+[0-9]+\\s+rot:", info, value = TRUE)
  for (line in rot_lines) {
    match <- regexec("^Page\\s+([0-9]+)\\s+rot:\\s*(-?[0-9]+)", line, perl = TRUE)
    parts <- regmatches(line, match)[[1]]
    if (length(parts) != 3L) next
    row <- match(as.integer(parts[[2]]), out$page)
    if (!is.na(row)) out$rotation[[row]] <- as.integer(parts[[3]])
  }
  out
}

extract_pdf_page_layout <- function(path, command = pdf_info_command()) {
  if (!pdf_info_available(command)) return(NULL)
  info <- suppressWarnings(system2(
    command,
    c("-f", "1", "-l", "999999", path),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(info, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) return(NULL)
  parse_pdf_page_layout(info)
}

pdf_has_landscape_page <- function(layout) {
  if (is.null(layout) || !nrow(layout)) return(FALSE)
  rotated <- abs(layout$rotation) %% 180L == 90L
  any(layout$width > layout$height | rotated, na.rm = TRUE)
}

source_requests_landscape <- function(path) {
  if (!file.exists(path)) return(FALSE)
  lines <- readLines(path, warn = FALSE)
  any(grepl("^\\s*:::\\s*\\{[^}]*\\.landscape(?:\\s|\\}|$)", lines, perl = TRUE)) ||
    any(grepl("\\\\begin\\{landscape\\}", lines, perl = TRUE))
}

rendered_layout_request_source <- function(source_path) {
  if (!grepl("\\.qmd$", source_path, ignore.case = TRUE)) return(source_path)
  rendered_tex <- sub("\\.qmd$", ".tex", source_path, ignore.case = TRUE)
  if (file.exists(rendered_tex)) rendered_tex else source_path
}
