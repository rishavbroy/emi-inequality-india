# Build writing-sample QMDs from the current paper's ordinary Quarto section IDs.

split_qmd_front_matter <- function(lines) {
  if (length(lines) < 3L || !identical(lines[[1]], "---")) {
    stop("Expected Quarto YAML front matter.", call. = FALSE)
  }
  close <- which(lines[-1L] == "---")
  if (!length(close)) stop("Unclosed Quarto YAML front matter.", call. = FALSE)
  end <- close[[1]] + 1L
  list(yaml = lines[2L:(end - 1L)], body = lines[-seq_len(end)])
}

read_qmd_metadata <- function(lines) {
  parts <- split_qmd_front_matter(lines)
  yaml::yaml.load(paste(parts$yaml, collapse = "\n"))
}

qmd_section_index <- function(lines) {
  body <- split_qmd_front_matter(lines)$body
  pattern <- "^(#{1,6})\\s+(.+?)\\s+\\{#([A-Za-z0-9_-]+)(?:\\s+[^}]*)?\\}\\s*$"
  hits <- grep(pattern, body, perl = TRUE)
  if (!length(hits)) {
    return(data.frame(id = character(), title = character(), level = integer(), stringsAsFactors = FALSE))
  }
  data.frame(
    id = sub(pattern, "\\3", body[hits], perl = TRUE),
    title = sub(pattern, "\\2", body[hits], perl = TRUE),
    level = nchar(sub(pattern, "\\1", body[hits], perl = TRUE)),
    stringsAsFactors = FALSE
  )
}

validate_writing_section_ids <- function(source, section_ids) {
  index <- qmd_section_index(readLines(source, warn = FALSE))
  missing <- setdiff(section_ids, index$id)
  if (length(missing)) {
    stop("Writing-sample section IDs are missing from the current paper: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

writing_section_titles <- function(source_lines, section_ids) {
  index <- qmd_section_index(source_lines)
  setNames(index$title[match(section_ids, index$id)], section_ids)
}

format_sample_title_list <- function(titles) {
  titles <- as.character(titles)
  if (length(titles) == 1L) return(titles)
  if (length(titles) == 2L) return(paste(titles, collapse = " and "))
  paste0(paste(titles[-length(titles)], collapse = ", "), ", and ", titles[[length(titles)]])
}

writing_sample_notice <- function(spec, variant, manifest, source_lines) {
  is_full <- identical(spec$mode %||% "excerpt", "full")
  label <- if (is_full) "FULL PAPER" else paste0(spec$target_pages, "-PAGE COPY")
  heading <- paste0("**WRITING SAMPLE: ", label, "**")

  if (is_full) {
    description <- "This is the full paper prepared as a writing sample."
  } else {
    titles <- unname(writing_section_titles(source_lines, unlist(spec$sections, use.names = FALSE)))
    description <- paste0(
      "This excerpt contains ",
      format_sample_title_list(titles),
      " from the full paper."
    )
  }

  availability_note <- if (identical(variant, "named")) {
    paste0(
      "The [full paper](", manifest$paper$full_paper_url, ") and [repository](",
      manifest$paper$repository_url, ") are available online."
    )
  } else {
    # Keep anonymous first-page layout comparable to the named copy without
    # exposing identity-bearing URLs.  The parallel notice prevents anonymity
    # itself from changing a fixed-length sample's pagination.
    "Links to the full paper and repository are omitted here."
  }
  description <- paste(
    description,
    availability_note,
    "This writing sample can be generated using `make samples` or `bash scripts/run_full_build.sh`."
  )
  c(heading, "", description, "")
}

sample_metadata <- function(source_metadata, spec, variant, manifest) {
  meta <- source_metadata
  abstract <- meta$abstract %||% ""
  meta$abstract <- NULL
  meta$thanks <- NULL
  meta$author <- manifest$identity[[variant]]$author
  meta$bibliography <- "../../paper/references.bib"
  meta$`number-sections` <- TRUE
  if (!identical(spec$mode %||% "excerpt", "full")) {
    # The linked full paper carries the complete bibliography. Suppressing the
    # repeated reference list keeps fixed-length excerpts focused on the writing
    # itself while citeproc still renders the in-text citations.
    meta$`suppress-bibliography` <- TRUE
    meta$`link-citations` <- FALSE
    meta$crossref <- utils::modifyList(meta$crossref %||% list(), list(`ref-hyperlink` = FALSE))
    meta$format <- meta$format %||% list()
    meta$format$pdf <- utils::modifyList(meta$format$pdf %||% list(), list(`keep-tex` = TRUE))
    # Pandoc normally defines \citeproc only when it emits a bibliography.
    # Excerpts suppress that bibliography, while existing LaTeX table notes use
    # \citeproc{ref-key}{visible label} for source labels.  A fallback keeps
    # those labels readable without overriding Pandoc when it defines the macro.
    #
    # The LaTeX xr package is the standard mechanism for cross-document labels.
    # The paper target creates paper-reference-labels.aux from the rendered
    # paper.tex; excerpt references use a local label when the referenced item
    # is retained and otherwise fall back to that full-paper label.
    external_reference_header <- paste(
      "\\usepackage{xr}",
      "\\externaldocument[full-][nocite]{../../paper/paper-reference-labels}",
      "\\makeatletter",
      "\\let\\sample@localref\\ref",
      paste0(
        "\\renewcommand{\\ref}[1]{\\@ifundefined{r@#1}",
        "{\\sample@localref{full-#1}\\textnormal{ (full paper)}}",
        "{\\sample@localref{#1}}}"
      ),
      "\\makeatother",
      sep = "\n"
    )
    meta$`header-includes` <- c(
      meta$`header-includes` %||% list(),
      list("\\providecommand{\\citeproc}[2]{#2}", external_reference_header)
    )
    meta$filters <- c(
      meta$filters %||% list(),
      list(list(at = "post-quarto", path = "../filters/select-sections.lua"))
    )
    meta$`sample-sections` <- unname(unlist(spec$sections, use.names = FALSE))
  }
  list(metadata = meta, abstract = abstract)
}

normalize_sample_resource_paths <- function(lines) {
  gsub("../outputs/", "../../outputs/", lines, fixed = TRUE)
}


assemble_writing_sample_qmd <- function(source, spec, variant, manifest, output_qmd) {
  source_lines <- readLines(source, warn = FALSE)
  parts <- split_qmd_front_matter(source_lines)
  prepared <- sample_metadata(read_qmd_metadata(source_lines), spec, variant, manifest)
  yaml_lines <- quarto_yaml_lines(prepared$metadata, indent.mapping.sequence = TRUE)
  body <- normalize_sample_resource_paths(parts$body)

  preamble <- c(
    writing_sample_notice(spec, variant, manifest, source_lines),
    "## Abstract {-}",
    "",
    prepared$abstract,
    ""
  )
  lines <- c("---", yaml_lines, "---", "", preamble, body)
  dir.create(dirname(output_qmd), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, output_qmd)
  invisible(output_qmd)
}
