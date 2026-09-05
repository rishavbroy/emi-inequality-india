# Semantic inventory of rendered and diagnostic artifacts. This runs outside
# the {targets} graph: estimation metadata stays authoritative in its registries,
# while target/file provenance comes from targets::tar_meta().

output_artifact_roots <- function(include_samples = FALSE, include_analysis = FALSE) {
  roots <- c("paper", "outputs", "docs", "posters")
  if (isTRUE(include_samples)) roots <- c(roots, "application-samples/output")
  if (isTRUE(include_analysis)) roots <- c(roots, "analysis")
  roots
}

output_artifact_scope <- function(path) {
  rules <- c(
    "^outputs/diagnostics/build/" = "build_diagnostic",
    "^outputs/diagnostics/public/" = "public_diagnostic",
    "^outputs/diagnostics/extended/" = "extended_diagnostic",
    "^outputs/benchmarking/" = "benchmark",
    "^outputs/tables/" = "public_table",
    "^outputs/figures/" = "public_figure",
    "^paper/" = "paper",
    "^posters/" = "poster",
    "^docs/" = "documentation",
    "^analysis/" = "analysis_note",
    "^application-samples/output/" = "application_sample"
  )
  vapply(as.character(path), function(x) {
    hit <- which(vapply(names(rules), grepl, logical(1), x = x))
    if (length(hit)) unname(rules[[hit[[1L]]]]) else "other"
  }, character(1))
}

relative_output_path <- function(path, root) {
  root <- paste0(normalizePath(root, winslash = "/", mustWork = TRUE), "/")
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  ifelse(startsWith(path, root), substring(path, nchar(root) + 1L), path)
}

build_output_artifact_manifest <- function(target_meta, design_registry, roots, root = getwd()) {
  extensions <- c("pdf", "html", "md", "csv", "tex", "png", "json")
  paths <- unlist(lapply(unique(roots), function(dir) {
    full <- file.path(root, dir)
    if (dir.exists(full)) list.files(full, recursive = TRUE, full.names = TRUE) else character()
  }), use.names = FALSE)
  paths <- paths[file.exists(paths) & !dir.exists(paths)]
  paths <- paths[tolower(tools::file_ext(paths)) %in% extensions]
  paths <- sort(unique(relative_output_path(paths, root)))
  paths <- setdiff(paths, "outputs/diagnostics/build/output_manifest.csv")

  meta <- as.data.frame(target_meta, stringsAsFactors = FALSE)
  if (!all(c("name", "format", "path") %in% names(meta))) {
    stop("Target metadata must contain name, format, and path.", call. = FALSE)
  }
  meta <- meta[meta$format == "file", , drop = FALSE]
  target_paths <- do.call(rbind, lapply(seq_len(nrow(meta)), function(i) {
    value <- unlist(meta$path[[i]], use.names = FALSE)
    if (!length(value)) return(NULL)
    data.frame(
      target_name = rep(as.character(meta$name[[i]]), length(value)),
      path = relative_output_path(value, root),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(target_paths)) target_paths <- data.frame(target_name = character(), path = character())
  if (anyDuplicated(target_paths$path)) stop("Multiple file targets claim one output path.", call. = FALSE)

  idx <- match(paths, target_paths$path)
  target_name <- ifelse(is.na(idx), "", target_paths$target_name[idx])
  registry <- as.data.frame(design_registry, stringsAsFactors = FALSE)
  if (!all(c("analysis_id", "family") %in% names(registry))) {
    stop("Design registry must contain analysis_id and family.", call. = FALSE)
  }
  link <- lapply(paths, function(path) {
    full <- file.path(root, path)
    if (tolower(tools::file_ext(path)) != "csv") return(c(0L, 0L, ""))
    header <- tryCatch(names(utils::read.csv(full, nrows = 0L, check.names = FALSE)), error = function(e) character())
    if (!"analysis_id" %in% header) return(c(0L, 0L, ""))
    ids <- unique(trimws(as.character(utils::read.csv(full, stringsAsFactors = FALSE, check.names = FALSE)$analysis_id)))
    ids <- ids[nzchar(ids) & !is.na(ids)]
    matched <- match(ids, registry$analysis_id)
    families <- sort(unique(registry$family[matched[!is.na(matched)]]))
    c(length(ids), sum(!is.na(matched)), paste(families, collapse = ";"))
  })

  out <- data.frame(
    artifact_id = ifelse(nzchar(target_name), paste(target_name, basename(paths), sep = "::"), paste("filesystem", paths, sep = "::")),
    path = paths,
    target_name = target_name,
    output_scope = output_artifact_scope(paths),
    artifact_type = tolower(tools::file_ext(paths)),
    bytes = as.numeric(file.info(file.path(root, paths))$size),
    analysis_id_count = as.integer(vapply(link, `[[`, character(1), 1L)),
    linked_analysis_id_count = as.integer(vapply(link, `[[`, character(1), 2L)),
    analysis_families = vapply(link, `[[`, character(1), 3L),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(out$artifact_id) || anyDuplicated(out$path)) stop("Output manifest identifiers must be unique.", call. = FALSE)
  out
}
