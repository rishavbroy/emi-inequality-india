# Append selected paper-formatted empirical outputs to coding samples.

coding_output_registry <- function(manifest) {
  entries <- manifest$coding_outputs %||% list()
  ids <- names(entries)
  if (length(entries) && (is.null(ids) || any(!nzchar(ids)) || anyDuplicated(ids))) {
    stop("coding_outputs must be a named mapping with unique IDs.", call. = FALSE)
  }
  entries
}

resolve_coding_outputs <- function(spec, manifest) {
  ids <- unlist(spec$outputs %||% character(), use.names = FALSE)
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

selected_figure_lines <- function(item) {
  path <- item$file %||% ""
  if (!file.exists(path)) stop("Selected coding-sample figure does not exist: ", path, call. = FALSE)
  c(
    "```{=latex}",
    "\\clearpage",
    "```",
    "",
    paste0("## ", item$title %||% basename(path)),
    "",
    item$description %||% "",
    "",
    paste0("![](", sample_output_path(path), "){width=95%}")
  )
}

coding_sample_output_lines <- function(spec, manifest, variant, reference_labels) {
  items <- resolve_coding_outputs(spec, manifest)
  if (!length(items)) return(character())
  pieces <- lapply(items, function(item) {
    content <- switch(
      item$type %||% "",
      latex = selected_latex_lines(item, reference_labels),
      figure = selected_figure_lines(item),
      stop("Unsupported coding-sample output type: ", item$type %||% "", call. = FALSE)
    )
    c("", content, "")
  })
  table_code <- application_sample_file_reference("R/output/make_tables.R", variant, manifest)
  figure_code <- application_sample_file_reference("R/output/make_figures.R", variant, manifest)
  c(
    "", "# Selected Paper Outputs", "",
    paste0(
      "These outputs are the same formatted table and figure files used by the paper; their presentation is assembled in ",
      table_code, " and ", figure_code, "."
    ),
    unlist(pieces, use.names = FALSE)
  )
}
