# Render paper-formatted empirical outputs next to the code that produces them.

coding_output_registry <- function(manifest) {
  entries <- manifest$coding_outputs %||% list()
  ids <- names(entries)
  if (length(entries) && (is.null(ids) || any(!nzchar(ids)) || anyDuplicated(ids))) {
    stop("coding_outputs must be a named mapping with unique IDs.", call. = FALSE)
  }
  entries
}

resolve_coding_outputs <- function(ids, manifest) {
  ids <- unlist(ids %||% character(), use.names = FALSE)
  registry <- coding_output_registry(manifest)
  missing <- setdiff(ids, names(registry))
  if (length(missing)) {
    stop("Coding sample references unknown selected outputs: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  unname(registry[ids])
}

sample_output_path <- function(path) {
  paste0("../../", path)
}

selected_latex_lines <- function(item, reference_labels) {
  path <- item$file %||% ""
  if (!file.exists(path)) stop("Selected coding-sample table does not exist: ", path, call. = FALSE)

  paper_label <- item$paper_label %||% ""
  if (!grepl("^tbl-[A-Za-z0-9_-]+$", paper_label)) {
    stop("Selected coding-sample table must name its paper_label: ", path, call. = FALSE)
  }
  number <- unname(reference_labels[paper_label])
  if (length(number) != 1L || is.na(number) || !grepl("^[0-9]+$", number)) {
    stop("Full-paper reference index has no integer table number for ", paper_label, call. = FALSE)
  }

  c(
    "```{=latex}",
    "\\FloatBarrier",
    sprintf("\\setcounter{table}{%d}", as.integer(number) - 1L),
    paste0("\\input{", sample_output_path(path), "}"),
    "\\FloatBarrier",
    "```"
  )
}

paper_figure_block <- function(source, paper_label) {
  if (!file.exists(source)) stop("Current paper source does not exist: ", source, call. = FALSE)
  if (!grepl("^fig-[A-Za-z0-9_-]+$", paper_label)) {
    stop("Selected coding-sample figure must name its fig- paper_label.", call. = FALSE)
  }

  lines <- readLines(source, warn = FALSE)
  label_pattern <- paste0("\\{#", paper_label, "(?:\\s+[^}]*)?\\}")
  hits <- grep(label_pattern, lines, perl = TRUE)
  if (length(hits) != 1L) {
    stop("Expected exactly one paper figure block for ", paper_label, call. = FALSE)
  }

  start <- hits[[1L]]
  if (!grepl("^\\s*:{3,}\\s*\\{#", lines[[start]], perl = TRUE)) {
    return(lines[[start]])
  }

  depth <- 0L
  end <- NA_integer_
  for (i in seq.int(start, length(lines))) {
    line <- lines[[i]]
    if (grepl("^\\s*:{3,}\\s*\\{", line, perl = TRUE)) depth <- depth + 1L
    if (grepl("^\\s*:{3,}\\s*$", line, perl = TRUE)) {
      depth <- depth - 1L
      if (depth == 0L) {
        end <- i
        break
      }
    }
  }
  if (is.na(end)) stop("Unclosed paper figure block for ", paper_label, call. = FALSE)
  lines[start:end]
}

normalize_coding_sample_figure_paths <- function(lines) {
  gsub("../outputs/", "../../outputs/", lines, fixed = TRUE)
}

selected_figure_lines <- function(item, manifest, reference_labels) {
  paper_label <- item$paper_label %||% ""
  if (!grepl("^fig-[A-Za-z0-9_-]+$", paper_label)) {
    stop("Selected coding-sample figure must name its fig- paper_label.", call. = FALSE)
  }
  number <- unname(reference_labels[paper_label])
  if (length(number) != 1L || is.na(number) || !grepl("^[0-9]+$", number)) {
    stop("Full-paper reference index has no integer figure number for ", paper_label, call. = FALSE)
  }

  figure <- paper_figure_block(manifest$paper$source, paper_label)
  figure <- normalize_coding_sample_figure_paths(figure)
  c(
    "```{=latex}",
    "\\FloatBarrier",
    sprintf("\\setcounter{figure}{%d}", as.integer(number) - 1L),
    "```",
    figure,
    "```{=latex}",
    "\\FloatBarrier",
    "```"
  )
}

coding_output_code_file_lines <- function(item, variant, manifest) {
  files <- unlist(item$code_files %||% character(), use.names = FALSE)
  if (!length(files)) return(character())
  refs <- vapply(
    files,
    application_sample_file_reference,
    character(1),
    variant = variant,
    manifest = manifest
  )
  output_name <- if (identical(item$type %||% "", "latex")) "table" else "figure"
  c("", paste0("Additional code used for this ", output_name, ": ", paste(refs, collapse = ", "), "."), "")
}

coding_sample_output_lines <- function(output_ids, variant, manifest, reference_labels) {
  items <- resolve_coding_outputs(output_ids, manifest)
  if (!length(items)) return(character())
  unlist(lapply(items, function(item) {
    type <- item$type %||% ""
    content <- switch(
      type,
      latex = selected_latex_lines(item, reference_labels),
      figure = selected_figure_lines(item, manifest, reference_labels),
      stop("Unsupported coding-sample output type: ", type, call. = FALSE)
    )
    c(
      coding_output_code_file_lines(item, variant, manifest),
      "",
      content,
      ""
    )
  }), use.names = FALSE)
}
