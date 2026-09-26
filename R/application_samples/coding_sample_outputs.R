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

selected_latex_lines <- function(item) {
  path <- item$file %||% ""
  if (!file.exists(path)) stop("Selected coding-sample table does not exist: ", path, call. = FALSE)
  c("```{=latex}", paste0("\\input{", sample_output_path(path), "}"), "```")
}

selected_figure_lines <- function(item) {
  path <- item$file %||% ""
  if (!file.exists(path)) stop("Selected coding-sample figure does not exist: ", path, call. = FALSE)
  c(
    paste0("## ", item$title %||% basename(path)),
    "",
    item$description %||% "",
    "",
    paste0("![](", sample_output_path(path), "){width=95%}")
  )
}

coding_sample_output_lines <- function(spec, manifest, variant) {
  items <- resolve_coding_outputs(spec, manifest)
  if (!length(items)) return(character())
  pieces <- lapply(items, function(item) {
    content <- switch(
      item$type %||% "",
      latex = selected_latex_lines(item),
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
