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

qmd_heading_index <- function(lines) {
  body <- split_qmd_front_matter(lines)$body
  pattern <- "^(#{1,6})\\s+(.+?)\\s+\\{#([A-Za-z0-9_-]+)(?:\\s+[^}]*)?\\}\\s*$"
  hits <- grep(pattern, body, perl = TRUE)
  if (!length(hits)) {
    return(data.frame(
      id = character(), title = character(), level = integer(), line = integer(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    id = sub(pattern, "\\3", body[hits], perl = TRUE),
    title = sub(pattern, "\\2", body[hits], perl = TRUE),
    level = nchar(sub(pattern, "\\1", body[hits], perl = TRUE)),
    line = hits,
    stringsAsFactors = FALSE
  )
}

qmd_section_index <- function(lines) {
  index <- qmd_heading_index(lines)
  index[c("id", "title", "level")]
}

writing_sample_retained_body_lines <- function(source_lines, section_ids) {
  body <- split_qmd_front_matter(source_lines)$body
  headings <- qmd_heading_index(source_lines)
  if (!length(section_ids)) return(seq_along(body))

  selected_rows <- match(section_ids, headings$id)
  if (anyNA(selected_rows)) {
    missing <- section_ids[is.na(selected_rows)]
    stop("Writing-sample section IDs are missing from the current paper: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  keep <- integer()
  if (nrow(headings) && headings$line[[1L]] > 1L) {
    keep <- seq_len(headings$line[[1L]] - 1L)
  }

  # Keep each selected section through its descendants.
  for (row in selected_rows) {
    start_line <- headings$line[[row]]
    following <- which(seq_len(nrow(headings)) > row & headings$level <= headings$level[[row]])
    end_line <- if (length(following)) headings$line[[following[[1L]]]] - 1L else length(body)
    keep <- c(keep, seq.int(start_line, end_line))
  }

  # The Pandoc selector retains ancestor headings but omits their unselected body.
  stack <- rep(NA_integer_, 6L)
  ancestor_rows <- integer()
  for (row in seq_len(nrow(headings))) {
    level <- headings$level[[row]]
    stack[level:6L] <- NA_integer_
    if (headings$id[[row]] %in% section_ids) {
      if (level > 1L) ancestor_rows <- c(ancestor_rows, stack[seq_len(level - 1L)])
    }
    stack[[level]] <- row
  }
  ancestor_rows <- unique(ancestor_rows[!is.na(ancestor_rows)])
  if (length(ancestor_rows)) keep <- c(keep, headings$line[ancestor_rows])

  sort(unique(keep))
}

qmd_label_ids <- function(lines) {
  text <- paste(lines, collapse = "\n")
  brace_hits <- regmatches(
    text, gregexpr("\\{#[A-Za-z0-9_-]+", text, perl = TRUE)
  )[[1L]]
  ids <- sub("^\\{#", "", brace_hits)

  chunk_pattern <- "^\\s*#\\|\\s*label:\\s*([A-Za-z0-9_-]+)\\s*$"
  chunk_hits <- grep(chunk_pattern, lines, perl = TRUE, value = TRUE)
  if (length(chunk_hits)) {
    ids <- c(ids, sub(chunk_pattern, "\\1", chunk_hits, perl = TRUE))
  }
  unique(ids[nzchar(ids)])
}


tex_crossref_ids <- function(path) {
  if (!file.exists(path)) return(character())
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  hits <- regmatches(
    text,
    gregexpr("\\\\label\\{(?:tbl|fig|eq)-[A-Za-z0-9_-]+\\}", text, perl = TRUE)
  )[[1L]]
  if (!length(hits) || identical(hits, "")) return(character())
  sub("\\}$", "", sub("^\\\\label\\{", "", hits, perl = TRUE), perl = TRUE)
}

qmd_rendered_tex_ids <- function(lines, source_dir) {
  # Public paper tables are inserted by literal render_public_table() calls.
  # Read those paths directly from retained source lines, then inspect the TeX
  # that owns the cross-reference label.  Do not normalize first: file.exists()
  # in tex_crossref_ids() is the appropriate check and preserves ordinary paths
  # relative to the paper containing the call.
  pattern <- "render_public_table\\(\\s*['\"]([^'\"]+[.]tex)['\"]"
  matches <- regexec(pattern, lines, perl = TRUE)
  parts <- regmatches(lines, matches)
  paths <- vapply(parts, function(x) {
    if (length(x) < 2L) return(NA_character_)
    x[[2L]]
  }, character(1))
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  if (!length(paths)) return(character())

  unique(unlist(lapply(
    file.path(source_dir, paths),
    tex_crossref_ids
  ), use.names = FALSE))
}

writing_sample_retained_label_ids <- function(source_lines, section_ids, source_dir = ".") {
  body <- split_qmd_front_matter(source_lines)$body
  keep <- writing_sample_retained_body_lines(source_lines, section_ids)
  kept <- body[keep]
  unique(c(qmd_label_ids(kept), qmd_rendered_tex_ids(kept, source_dir)))
}

validate_writing_reference_labels <- function(source, section_ids, reference_labels) {
  source_lines <- readLines(source, warn = FALSE)
  retained <- writing_sample_retained_label_ids(source_lines, section_ids, dirname(source))
  retained <- retained[grepl("^(?:sec|tbl|fig|eq)-", retained, perl = TRUE)]
  missing <- setdiff(retained, names(reference_labels %||% character()))
  if (length(missing)) {
    stop(
      "Full-paper reference index has no label for retained writing-sample content: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

paper_reference_aux_path <- function(source) {
  file.path(
    dirname(source),
    paste0(tools::file_path_sans_ext(basename(source)), "-reference-labels.aux")
  )
}

read_latex_reference_labels <- function(path) {
  if (!file.exists(path)) {
    stop("Full-paper reference index is missing: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  pattern <- "^\\\\newlabel\\{([^}]+)\\}\\{\\{([^}]*)\\}"
  hits <- regexec(pattern, lines, perl = TRUE)
  parts <- regmatches(lines, hits)
  parts <- parts[lengths(parts) == 3L]
  if (!length(parts)) {
    stop("Full-paper reference index contains no LaTeX labels: ", path, call. = FALSE)
  }

  ids <- vapply(parts, `[[`, character(1), 2L)
  numbers <- vapply(parts, `[[`, character(1), 3L)
  keep <- nzchar(ids) & nzchar(numbers)
  ids <- ids[keep]
  numbers <- numbers[keep]

  grouped <- split(numbers, ids)
  conflicts <- names(grouped)[vapply(grouped, function(x) length(unique(x)) > 1L, logical(1))]
  if (length(conflicts)) {
    stop(
      "Full-paper reference index contains conflicting numbers for: ",
      paste(conflicts, collapse = ", "),
      call. = FALSE
    )
  }
  vapply(grouped, function(x) unique(x)[[1L]], character(1))
}

crossref_display_name <- function(id) {
  prefix <- sub("-.*$", "", id)
  switch(prefix,
    sec = "Section",
    tbl = "Table",
    fig = "Figure",
    eq = "Equation",
    stop("Unsupported Quarto cross-reference ID: ", id, call. = FALSE)
  )
}

externalize_crossrefs_in_line <- function(line, retained_ids, reference_labels, variant, full_paper_url) {
  pattern <- "@((?:sec|tbl|fig|eq)-[A-Za-z0-9_-]+)"
  match <- gregexpr(pattern, line, perl = TRUE)[[1L]]
  if (length(match) == 1L && identical(match[[1L]], -1L)) return(line)

  tokens <- regmatches(line, list(match))[[1L]]
  ids <- sub("^@", "", tokens)
  replacements <- tokens
  omitted <- !ids %in% retained_ids

  for (i in which(omitted)) {
    id <- ids[[i]]
    number <- unname(reference_labels[id])
    if (length(number) != 1L || is.na(number) || !nzchar(number)) {
      stop("Full-paper reference index has no label for ", id, call. = FALSE)
    }
    label <- paste(crossref_display_name(id), number)
    replacements[[i]] <- if (identical(variant, "named") && nzchar(full_paper_url %||% "")) {
      paste0("[", label, "](", full_paper_url, ")")
    } else {
      label
    }
  }

  regmatches(line, list(match)) <- list(replacements)
  line
}

# Quarto assigns the authoritative numbers in the full-paper render. Resolve
# references to content that the excerpt will omit before Quarto processes the
# excerpt; references to retained targets stay in Quarto's ordinary @id form.
externalize_omitted_writing_crossrefs <- function(
    body, source_lines, section_ids, reference_labels, variant, full_paper_url = "", source_dir = "."
) {
  keep <- writing_sample_retained_body_lines(source_lines, section_ids)
  retained_ids <- writing_sample_retained_label_ids(source_lines, section_ids, source_dir)
  body[keep] <- vapply(
    body[keep],
    externalize_crossrefs_in_line,
    character(1),
    retained_ids = retained_ids,
    reference_labels = reference_labels,
    variant = variant,
    full_paper_url = full_paper_url,
    USE.NAMES = FALSE
  )
  body
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

format_english_list <- function(items) {
  items <- as.character(items)
  if (!length(items)) return("")
  if (length(items) == 1L) return(items[[1L]])
  if (length(items) == 2L) return(paste(items, collapse = " and "))
  paste0(paste(items[-length(items)], collapse = ", "), ", and ", items[[length(items)]])
}

writing_sample_contents <- function(spec, source_lines, reference_labels) {
  if (identical(spec$mode %||% "excerpt", "full")) return("the full paper")

  section_ids <- unname(unlist(spec$sections, use.names = FALSE))
  titles <- unname(writing_section_titles(source_lines, section_ids))
  special <- titles %in% c("Introduction", "Conclusion")

  named_sections <- titles[special]
  named_sections[named_sections == "Introduction"] <- "the Introduction"

  numbered_ids <- section_ids[!special]
  numbered <- character()
  if (length(numbered_ids)) {
    missing <- setdiff(numbered_ids, names(reference_labels %||% character()))
    if (length(missing)) {
      stop(
        "Full-paper reference index has no section number for writing-sample section(s): ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    numbers <- unname(reference_labels[numbered_ids])
    prefix <- if (length(numbers) == 1L) "Section " else "Sections "
    numbered <- paste0(prefix, format_english_list(numbers))
  }

  format_english_list(c(named_sections, numbered))
}

writing_sample_notice <- function(spec, variant, manifest, source_lines, reference_labels = NULL) {
  is_full <- identical(spec$mode %||% "excerpt", "full")
  label <- if (is_full) "FULL PAPER" else paste0(spec$target_pages, "-PAGE COPY")
  description <- if (is_full) character() else {
    paste0(
      "This document contains ",
      writing_sample_contents(spec, source_lines, reference_labels),
      " of the titular paper."
    )
  }
  availability <- if (identical(variant, "named")) {
    application_sample_availability_sentence(variant, manifest, also = is_full)
  } else if (is_full) {
    "The full paper and repository are also available online, and I would be happy to provide links for them if desired."
  } else {
    "The full paper and repository are available online, and I would be happy to provide links for them if desired."
  }
  text <- paste(
    c(description, availability, application_sample_build_sentence(variant, manifest)),
    collapse = " "
  )

  c(
    "```{=latex}",
    "\\begingroup",
    paste0("\\renewcommand{\\abstractname}{WRITING SAMPLE: ", label, "}"),
    "\\begin{abstract}",
    "```",
    text,
    "```{=latex}",
    "\\end{abstract}",
    "\\endgroup",
    "```",
    ""
  )
}


sample_metadata <- function(source_metadata, spec, variant, manifest) {
  meta <- paper_sample_metadata(source_metadata, variant, manifest)
  abstract <- meta$abstract %||% ""
  meta$abstract <- NULL
  meta$bibliography <- "../../paper/references.bib"
  meta$`number-sections` <- TRUE
  if (!identical(spec$mode %||% "excerpt", "full")) {
    # The linked full paper carries the complete bibliography. Suppressing the
    # repeated reference list keeps fixed-length excerpts focused on the writing
    # itself while citeproc still renders the in-text citations.
    meta$`suppress-bibliography` <- TRUE
    meta$`link-citations` <- FALSE
    meta$format <- meta$format %||% list()
    meta$format$pdf <- utils::modifyList(meta$format$pdf %||% list(), list(`keep-tex` = TRUE))
    # Pandoc normally defines \citeproc only when it emits a bibliography.
    # Excerpts suppress that bibliography, while existing LaTeX table notes use
    # \citeproc{ref-key}{visible label} for source labels. A fallback keeps
    # those labels readable without overriding Pandoc when it defines the macro.
    meta$`header-includes` <- c(
      meta$`header-includes` %||% list(),
      list("\\providecommand{\\citeproc}[2]{#2}")
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


assemble_writing_sample_qmd <- function(source, spec, variant, manifest, output_qmd, reference_labels = NULL) {
  source_lines <- readLines(source, warn = FALSE)
  parts <- split_qmd_front_matter(source_lines)
  source_metadata <- read_qmd_metadata(source_lines)
  prepared <- sample_metadata(source_metadata, spec, variant, manifest)
  if (!identical(spec$mode %||% "excerpt", "full")) {
    prepared$metadata$`sample-reference-labels` <- as.list(reference_labels)
  }
  yaml_lines <- quarto_yaml_lines(prepared$metadata, indent.mapping.sequence = TRUE)
  body <- normalize_sample_resource_paths(parts$body)
  if (!identical(spec$mode %||% "excerpt", "full")) {
    if (is.null(reference_labels)) {
      stop("Writing excerpts require the current full-paper reference index.", call. = FALSE)
    }
    body <- externalize_omitted_writing_crossrefs(
      body, source_lines, unlist(spec$sections, use.names = FALSE), reference_labels,
      variant, manifest$paper$full_paper_url %||% "", dirname(source)
    )
  }

  preamble <- c(
    writing_sample_notice(spec, variant, manifest, source_lines, reference_labels),
    "```{=latex}",
    "\\begin{abstract}",
    "```",
    prepared$abstract,
    "```{=latex}",
    "\\end{abstract}",
    "```",
    ""
  )
  lines <- c("---", yaml_lines, "---", "", preamble, body)
  dir.create(dirname(output_qmd), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, output_qmd)
  invisible(output_qmd)
}
