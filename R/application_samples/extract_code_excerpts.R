# Utilities for marker-delimited coding-sample excerpts from R files.

extract_code_excerpts <- function(spec, variant, manifest) {
  pieces <- lapply(spec$excerpts, function(x) {
    file <- x$file
    id <- x$id
    title <- x$title %||% id
    code <- extract_between_sample_markers(file, id)
    c(
      "",
      paste0("## ", title),
      "",
      paste0("File: ", application_sample_file_reference(file, variant, manifest)),
      "",
      "```r",
      code,
      "```",
      ""
    )
  })
  unlist(pieces, use.names = FALSE)
}

extract_between_sample_markers <- function(file, id) {
  if (!file.exists(file)) stop("Code excerpt file does not exist: ", file, call. = FALSE)
  text <- readLines(file, warn = FALSE)
  escaped <- gsub("([\\W])", "\\\\\\1", id)
  start <- grep(paste0("^\\s*#\\s*sample-start:\\s*", escaped, "\\s*$"), text)
  end <- grep(paste0("^\\s*#\\s*sample-end:\\s*", escaped, "\\s*$"), text)
  if (length(start) != 1L || length(end) != 1L || end <= start) {
    stop("Could not find a unique valid code excerpt marker pair for ID: ", id, call. = FALSE)
  }
  text[(start + 1L):(end - 1L)]
}

coding_sample_notice <- function(spec, variant, manifest, source_metadata) {
  paper_title <- quoted_paper_title(source_metadata)
  description <- paste0(
    "This document contains excerpts from the replication code of ",
    if (identical(variant, "anonymous")) "a paper titled " else "the paper ",
    paper_title,
    ". Selected outputs are rendered at the end."
  )

  c(
    paste(
      c(
        description,
        application_sample_availability_sentence(variant, manifest),
        application_sample_build_sentence(variant, manifest)
      ),
      collapse = " "
    ),
    ""
  )
}

coding_sample_metadata <- function(spec, variant, manifest, source_metadata) {
  meta <- paper_sample_metadata(source_metadata, variant, manifest)
  meta$title <- "Code Sample"
  meta$subtitle <- if (identical(spec$id, "short")) "Short Version" else "Long Version"
  meta$abstract <- NULL
  meta$bibliography <- NULL
  meta$execute <- NULL
  meta$`link-citations` <- NULL
  meta$`cite-method` <- NULL
  meta$`number-sections` <- FALSE
  meta$format$pdf <- utils::modifyList(
    meta$format$pdf %||% list(),
    list(
      `syntax-highlighting` = "tango",
      `code-block-bg` = "#f7f7f7",
      `keep-tex` = TRUE
    )
  )
  meta$`header-includes` <- c(
    meta$`header-includes` %||% list(),
    list(
      "\\usepackage{fvextra}",
      paste0(
        "\\RecustomVerbatimEnvironment{Highlighting}{Verbatim}",
        "{commandchars=\\\\\\{\\},breaklines=true,breaknonspaceingroup,",
        "breakanywhere=true,fontsize=\\footnotesize}"
      ),
      "\\providecommand{\\citeproc}[2]{#2}"
    )
  )
  meta
}

assemble_coding_sample_qmd <- function(spec, variant, manifest, body, output_qmd) {
  source_metadata <- read_qmd_metadata(readLines(manifest$paper$source, warn = FALSE))
  meta <- coding_sample_metadata(spec, variant, manifest, source_metadata)
  yaml_lines <- quarto_yaml_lines(meta, indent.mapping.sequence = TRUE)
  lines <- c("---", yaml_lines, "---", "", coding_sample_notice(spec, variant, manifest, source_metadata), body)
  dir.create(dirname(output_qmd), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, output_qmd)
  invisible(output_qmd)
}


validate_code_excerpt_markers <- function(spec) {
  invisible(lapply(spec$excerpts, function(x) extract_between_sample_markers(x$file, x$id)))
  invisible(TRUE)
}
