# Render writing samples from excerpt markers in paper/report.qmd.

#' Render all writing samples described by YAML specs
#'
#' @param spec_dir Directory containing `writing-*.yml` specs.
#' @return Character vector of output PDF paths.
render_writing_samples <- function(spec_dir = "application-samples/specs", output_files = NULL) {
  force(output_files)
  specs <- list.files(spec_dir, pattern = "^writing-.*\\.yml$", full.names = TRUE)
  vapply(specs, render_one_writing_sample, character(1))
}

#' Render one writing sample
#'
#' @param spec_path YAML spec path.
#' @return Output PDF path.
render_one_writing_sample <- function(spec_path) {
  spec <- yaml::read_yaml(spec_path)
  output <- spec$output
  work_dir <- file.path("application-samples", ".work")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  output_qmd <- file.path(work_dir, paste0(tools::file_path_sans_ext(basename(output)), ".qmd"))

  raw_source_lines <- readLines(spec$source, warn = FALSE)
  if (identical(spec$mode, "full")) {
    source_lines <- c(report_abstract_block(raw_source_lines), strip_qmd_yaml(raw_source_lines))
    assemble_writing_sample_qmd(spec$cover_note, source_lines, output_qmd)
  } else {
    excerpts <- c(
      report_setup_chunks(raw_source_lines),
      extract_qmd_excerpts(spec$source, unlist(spec$excerpts, use.names = FALSE))
    )
    assemble_writing_sample_qmd(spec$cover_note, excerpts, output_qmd)
  }

  clean_writing_sample_qmd(output_qmd, replace_external_refs = !identical(spec$mode, "full"))
  render_qmd_to_pdf(output_qmd, output)
  output
}

read_sample_report_values <- function() {
  if (!requireNamespace("targets", quietly = TRUE)) return(list())
  tryCatch(targets::tar_read(report_values), error = function(e) list())
}

sample_value <- function(values, name, digits = NULL) {
  x <- values[[name]]
  if (is.null(x)) return(NA_character_)
  if (is.list(x) && !is.null(x$value)) x <- x$value
  if (is.list(x) && !is.null(x$display)) x <- x$display
  if (length(x) == 0L || all(is.na(x))) return(NA_character_)
  x <- x[[1]]
  if (is.numeric(x) && is.finite(x) && !is.null(digits)) x <- round(x, digits)
  as.character(x)
}

inject_cover_note_values <- function(lines) {
  values <- read_sample_report_values()
  if (!length(values)) return(lines)
  f <- sample_value(values, "partial_f", 1)
  beta <- sample_value(values, "iv_emie_estimate", 1)
  p <- sample_value(values, "iv_emie_p", 2)
  if (any(is.na(c(f, beta, p)))) return(lines)
  new_line <- paste0(
    "4. **Main result**: Current generated first-stage ($F=", f,
    "$) and second-stage estimates ($", beta, "$ pp, $p=", p,
    "$) are reported in the included tables. Estimates are provisional pending a validated district-geometry join, repaired district matching, and state-FE/FD-2SLS redesign."
  )
  idx <- grep("^4[.] [*][*]Main result[*][*]:", lines)
  if (length(idx)) lines[idx] <- new_line
  lines
}

clean_writing_sample_qmd <- function(path, replace_external_refs = TRUE) {
  lines <- readLines(path, warn = FALSE)
  lines <- inject_cover_note_values(lines)

  # Quarto resolves @fig-*/@tbl-*/@sec-* with their own prefixes; legacy prose
  # already wrote Figure/Table/Sec. before bookdown \@ref(). Remove those
  # prefixes, then convert any excerpt-external references to clear prose so
  # sample PDFs never show ?@ markers or "Figure Figure"/"Sec. Section".
  lines <- gsub("Figure @fig-", "@fig-", lines, fixed = TRUE)
  lines <- gsub("Figures @fig-", "@fig-", lines, fixed = TRUE)
  lines <- gsub("Table @tbl-", "@tbl-", lines, fixed = TRUE)
  lines <- gsub("Tables @tbl-", "@tbl-", lines, fixed = TRUE)
  lines <- gsub("Sec. @sec-", "@sec-", lines, fixed = TRUE)
  lines <- gsub("Section @sec-", "@sec-", lines, fixed = TRUE)
  if (isTRUE(replace_external_refs)) {
    lines <- gsub("@fig-[A-Za-z0-9_-]+", "the corresponding figure in the full report", lines, perl = TRUE)
    lines <- gsub("@tbl-[A-Za-z0-9_-]+", "the corresponding table in the full report", lines, perl = TRUE)
    lines <- gsub("@sec-[A-Za-z0-9_-]+", "the corresponding section of the full report", lines, perl = TRUE)
    lines <- gsub("@eq-[A-Za-z0-9_-]+", "the corresponding equation in the full report", lines, perl = TRUE)
  }
  writeLines(lines, path)
  invisible(path)
}

#' Render a QMD file to a PDF path
#'
#' @param input_qmd Assembled temporary QMD.
#' @param output_file Desired PDF output path.
#' @return Output PDF path invisibly.
render_qmd_to_pdf <- function(input_qmd, output_file) {
  old_wd <- getwd()

  input_qmd <- normalizePath(input_qmd, mustWork = TRUE)
  if (!grepl("^/", output_file)) output_file <- file.path(old_wd, output_file)
  output_file <- normalizePath(output_file, mustWork = FALSE)
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  rendered_name <- basename(output_file)

  if (!nzchar(Sys.which("quarto"))) stop("Quarto CLI was not found on PATH; cannot render ", input_qmd, call. = FALSE)

  on.exit(setwd(old_wd), add = TRUE)
  setwd(dirname(input_qmd))

  status <- system2("quarto", c("render", basename(input_qmd), "--to", "pdf", "--output", rendered_name))
  if (!identical(status, 0L)) stop("quarto render failed for ", input_qmd, call. = FALSE)

  rendered_path <- normalizePath(file.path(dirname(input_qmd), rendered_name), mustWork = FALSE)
  if (!file.exists(rendered_path)) stop("Expected rendered PDF was not created: ", rendered_path, call. = FALSE)
  if (!identical(rendered_path, output_file)) {
    ok <- file.copy(rendered_path, output_file, overwrite = TRUE)
    if (!isTRUE(ok)) stop("Could not copy rendered PDF to ", output_file, call. = FALSE)
  }
  if (!file.exists(output_file)) stop("Rendered PDF was not copied to expected output path: ", output_file, call. = FALSE)

  invisible(output_file)
}
