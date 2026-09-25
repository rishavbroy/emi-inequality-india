# Utilities for marker-delimited coding-sample excerpts from R files.

extract_code_excerpts <- function(spec) {
  pieces <- lapply(spec$excerpts, function(x) {
    file <- x$file
    id <- x$id
    title <- x$title %||% id
    code <- extract_between_sample_markers(file, id)
    c(
      "",
      paste0("## ", title),
      "",
      paste0("File: `", file, "`"),
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

coding_sample_notice <- function(spec, variant, manifest) {
  heading <- paste0("**CODING SAMPLE: ", toupper(spec$id), " COPY**")
  description <- "These excerpts are selected from the replication code for the paper. Selected outputs appear at the end of the document."
  if (identical(variant, "named")) {
    description <- paste0(
      description,
      " The [full paper](", manifest$paper$full_paper_url, ") and [repository](",
      manifest$paper$repository_url,
      ") are available online. The complete paper-facing table and figure assembly code is in ",
      "[`R/output/make_tables.R`](", manifest$paper$repository_url, "/blob/main/R/output/make_tables.R) and ",
      "[`R/output/make_figures.R`](", manifest$paper$repository_url, "/blob/main/R/output/make_figures.R). ",
      "This PDF can be generated using `make samples` or `bash scripts/run_full_build.sh`."
    )
  }
  c(heading, "", description, "")
}

assemble_coding_sample_qmd <- function(spec, variant, manifest, body, output_qmd) {
  meta <- list(
    title = "Code Sample",
    subtitle = "Selected Replication Code",
    author = manifest$identity[[variant]]$author,
    format = list(pdf = list(`pdf-engine` = "xelatex")),
    geometry = "left=0.75in, right=0.75in, top=0.8in, bottom=0.8in",
    `highlight-style` = "default"
  )
  yaml_lines <- quarto_yaml_lines(meta)
  lines <- c("---", yaml_lines, "---", "", coding_sample_notice(spec, variant, manifest), body)
  dir.create(dirname(output_qmd), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, output_qmd)
  invisible(output_qmd)
}

validate_code_excerpt_markers <- function(spec) {
  invisible(lapply(spec$excerpts, function(x) extract_between_sample_markers(x$file, x$id)))
  invisible(TRUE)
}
