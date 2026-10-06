# Utilities for marker-delimited coding-sample excerpts from research code.

code_excerpt_language <- function(file, language = NULL) {
  if (!is.null(language) && nzchar(language)) return(language)
  switch(
    tolower(tools::file_ext(file)),
    r = "r",
    py = "python",
    sh = "bash",
    "text"
  )
}

extract_code_excerpt <- function(excerpt, variant, manifest) {
  file <- excerpt$file
  id <- excerpt$id
  title <- excerpt$title %||% id
  section <- trimws(excerpt$section %||% "")
  description <- trimws(excerpt$description %||% "")
  code <- extract_between_sample_markers(file, id)
  c(
    if (nzchar(section)) c("", paste0("# ", section)) else character(),
    "",
    paste0("## ", title),
    if (nzchar(description)) c("", description) else character(),
    "",
    paste0("File: ", application_sample_file_reference(file, variant, manifest)),
    "",
    paste0("```", code_excerpt_language(file, excerpt$language %||% NULL)),
    code,
    "```",
    ""
  )
}

extract_code_excerpts <- function(spec, variant, manifest) {
  unlist(lapply(spec$excerpts, extract_code_excerpt, variant = variant, manifest = manifest), use.names = FALSE)
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
  paper_title <- quoted_paper_title(source_metadata, terminal_period = TRUE)
  description <- paste0(
    "This document contains excerpts from the replication code of ",
    if (identical(variant, "anonymous")) "a paper titled " else "my paper ",
    paper_title
  )
  description <- paste0(
    application_sample_size_description(description, spec),
    " Excerpts are followed by their corresponding outputs when appropriate."
  )

  c(
    paste(
      c(
        description,
        application_sample_availability_sentence("coding", spec$id, variant, manifest),
        application_sample_build_sentence(variant, manifest)
      ),
      collapse = " "
    ),
    ""
  )
}


coding_sample_citeproc_fallback <- function(variant, manifest) {
  url <- manifest$paper$full_paper_url %||% ""
  if (identical(variant, "named") && nzchar(url)) {
    return(paste0("\\providecommand{\\citeproc}[2]{\\href{", url, "}{#2}}"))
  }
  "\\providecommand{\\citeproc}[2]{#2}"
}

coding_sample_metadata <- function(spec, variant, manifest, source_metadata) {
  meta <- paper_sample_metadata(source_metadata, variant, manifest)
  meta$title <- "Code Sample"
  label <- as.character(spec$label %||% "")
  meta$subtitle <- if (nzchar(label)) label else NULL
  meta$abstract <- NULL
  meta$bibliography <- "../../paper/references.bib"
  meta$`suppress-bibliography` <- TRUE
  meta$execute <- NULL
  meta$`link-citations` <- FALSE
  meta$`cite-method` <- "citeproc"
  meta$`number-sections` <- FALSE
  meta$format$pdf <- utils::modifyList(
    meta$format$pdf %||% list(),
    list(
      `syntax-highlighting` = "tango",
      `code-block-bg` = "#f7f7f7",
      `keep-tex` = TRUE
    )
  )
  if (identical(variant, "named") && nzchar(manifest$paper$full_paper_url %||% "")) {
    meta$`sample-full-paper-url` <- manifest$paper$full_paper_url
    meta$filters <- c(
      meta$filters %||% list(),
      list(list(at = "post-quarto", path = "../filters/link-citations-to-paper.lua"))
    )
  }
  meta$`header-includes` <- c(
    meta$`header-includes` %||% list(),
    list(
      "\\usepackage{fvextra}",
      "\\usepackage{placeins}",
      paste0(
        "\\RecustomVerbatimEnvironment{Highlighting}{Verbatim}",
        "{commandchars=\\\\\\{\\},breaklines=true,breaknonspaceingroup,",
        "breakanywhere=true,fontsize=\\footnotesize}"
      ),
      coding_sample_citeproc_fallback(variant, manifest)
    )
  )
  meta
}

assemble_coding_sample_qmd <- function(spec, variant, manifest, body, output_qmd) {
  source_metadata <- read_qmd_metadata(readLines(manifest$paper$source, warn = FALSE))
  meta <- coding_sample_metadata(spec, variant, manifest, source_metadata)
  yaml_lines <- quarto_yaml_lines(meta, indent.mapping.sequence = TRUE)
  paper_helpers <- c(
    "```{r coding-sample-paper-helpers}",
    "#| include: false",
    "source(\"../../R/output/public_qmd_helpers.R\", local = knitr::knit_global())",
    "```",
    ""
  )
  lines <- c(
    "---", yaml_lines, "---", "",
    paper_helpers,
    coding_sample_notice(spec, variant, manifest, source_metadata),
    body
  )
  dir.create(dirname(output_qmd), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, output_qmd)
  invisible(output_qmd)
}

validate_code_excerpt_markers <- function(spec) {
  invisible(lapply(spec$excerpts, function(x) extract_between_sample_markers(x$file, x$id)))
  invisible(TRUE)
}
