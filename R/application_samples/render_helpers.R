# Shared rendering and validation helpers for application-sample PDFs.

# Serialize metadata for Quarto, which follows YAML 1.2 boolean syntax.
quarto_yaml_lines <- function(x, indent.mapping.sequence = FALSE) {
  text <- yaml::as.yaml(
    x,
    indent.mapping.sequence = indent.mapping.sequence,
    handlers = list(logical = yaml::verbatim_logical)
  )
  strsplit(text, "\n", fixed = TRUE)[[1]]
}

render_qmd_to_pdf <- function(input_qmd, output_file) {
  input_qmd <- normalizePath(input_qmd, mustWork = TRUE)
  output_dir <- dirname(output_file)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  output_file <- file.path(output_dir, basename(output_file))
  rendered_name <- basename(output_file)

  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) stop("Quarto CLI was not found on PATH; cannot render ", input_qmd, call. = FALSE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dirname(input_qmd))

  status <- system2(quarto, c("render", basename(input_qmd), "--to", "pdf", "--output", rendered_name))
  if (!identical(status, 0L)) stop("quarto render failed for ", input_qmd, call. = FALSE)

  rendered_path <- file.path(dirname(input_qmd), rendered_name)
  if (!file.exists(rendered_path)) stop("Expected rendered PDF was not created: ", rendered_path, call. = FALSE)
  if (!identical(normalizePath(rendered_path, mustWork = TRUE), output_file)) {
    ok <- file.copy(rendered_path, output_file, overwrite = TRUE)
    if (!isTRUE(ok)) stop("Could not copy rendered PDF to ", output_file, call. = FALSE)
  }
  if (!file.exists(output_file)) stop("Rendered PDF was not available at expected output path: ", output_file, call. = FALSE)
  invisible(output_file)
}

pdf_page_count <- function(path) {
  pdfinfo <- Sys.which("pdfinfo")
  if (!nzchar(pdfinfo)) stop("pdfinfo is required to validate application-sample page counts.", call. = FALSE)
  info <- system2(pdfinfo, shQuote(path), stdout = TRUE, stderr = TRUE)
  status <- attr(info, "status") %||% 0L
  if (!identical(status, 0L)) stop("pdfinfo failed for ", path, call. = FALSE)
  page_line <- grep("^Pages:[[:space:]]+[0-9]+", info, value = TRUE)
  if (length(page_line) != 1L) stop("Could not read PDF page count for ", path, call. = FALSE)
  as.integer(sub("^Pages:[[:space:]]+", "", page_line))
}

validate_writing_sample_page_count <- function(path, expected_pages = NULL) {
  if (is.null(expected_pages) || !length(expected_pages)) return(invisible(TRUE))
  expected_pages <- as.integer(expected_pages)
  actual <- pdf_page_count(path)
  if (!identical(actual, expected_pages)) {
    stop(
      basename(path), " rendered to ", actual, " pages; expected ", expected_pages,
      ". Adjust the section selection in application-samples/samples.yml.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

validate_anonymous_sample <- function(path, forbidden_strings) {
  forbidden_strings <- unique(as.character(unlist(forbidden_strings, use.names = FALSE)))
  forbidden_strings <- forbidden_strings[nzchar(forbidden_strings)]
  if (!length(forbidden_strings)) return(invisible(TRUE))
  pdftotext <- Sys.which("pdftotext")
  if (!nzchar(pdftotext)) stop("pdftotext is required to validate anonymous application samples.", call. = FALSE)
  text <- system2(pdftotext, c(shQuote(path), "-"), stdout = TRUE, stderr = TRUE)
  status <- attr(text, "status") %||% 0L
  if (!identical(status, 0L)) stop("pdftotext failed for ", path, call. = FALSE)
  collapsed <- paste(text, collapse = "\n")
  hits <- forbidden_strings[vapply(forbidden_strings, grepl, logical(1), x = collapsed, fixed = TRUE)]
  if (length(hits)) {
    stop("Anonymous sample contains identifying text: ", paste(hits, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}
