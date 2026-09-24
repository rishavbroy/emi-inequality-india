# Conservative static source-health audit.
#
# This audit reports top-level production functions that have no symbolic
# reference in active R/QMD code and no exact metadata reference. It is
# deliberately advisory: R permits dynamic dispatch, so absence of a static
# reference is evidence for review, not proof that a function is unreachable.

source_health_r_files <- function() {
  sort(unique(c(
    "_targets.R",
    if (file.exists("_targets_processed.R")) "_targets_processed.R" else character(),
    list.files("R", "\\.[Rr]$", recursive = TRUE, full.names = TRUE),
    list.files("scripts", "\\.[Rr]$", recursive = TRUE, full.names = TRUE),
    list.files("tests", "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
  )))
}

source_health_qmd_files <- function() {
  roots <- c("paper", "docs", "application-samples", "posters")
  sort(unique(unlist(lapply(roots[dir.exists(roots)], function(root) {
    list.files(root, "\\.qmd$", recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE)))
}

source_health_function_definitions <- function(paths = list.files(
    "R", "\\.[Rr]$", recursive = TRUE, full.names = TRUE)) {
  rows <- list()
  for (path in sort(paths[file.exists(paths)])) {
    code <- parse(path, keep.source = TRUE)
    refs <- attr(code, "srcref")
    for (i in seq_along(code)) {
      expr <- code[[i]]
      if (!is.call(expr) || length(expr) < 3L ||
          !as.character(expr[[1L]]) %in% c("<-", "=") ||
          !is.symbol(expr[[2L]]) || !is.call(expr[[3L]]) ||
          !identical(expr[[3L]][[1L]], as.name("function"))) next
      line <- NA_integer_
      if (length(refs) >= i && !is.null(refs[[i]])) {
        line <- as.integer(refs[[i]][[1L]])
      }
      rows[[length(rows) + 1L]] <- data.frame(
        function_name = as.character(expr[[2L]]),
        file = gsub("\\\\", "/", path),
        line = line,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(
      function_name = character(), file = character(), line = integer(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

source_health_expression_names <- function(expr) {
  if (!is.call(expr)) return(all.names(expr, functions = TRUE))
  if (identical(expr[[1L]], as.name("function"))) {
    defaults <- as.list(expr[[2L]])
    defaults <- defaults[vapply(defaults, length, integer(1)) > 0L]
    return(c(
      unlist(lapply(defaults, all.names, functions = TRUE), use.names = FALSE),
      all.names(expr[[3L]], functions = TRUE)
    ))
  }
  all.names(expr, functions = TRUE)
}

source_health_code_names <- function(code) {
  unlist(lapply(code, function(expr) {
    if (is.call(expr) && length(expr) >= 3L &&
        as.character(expr[[1L]]) %in% c("<-", "=") &&
        is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) {
      source_health_expression_names(expr[[3L]])
    } else {
      all.names(expr, functions = TRUE)
    }
  }), use.names = FALSE)
}

source_health_symbol_counts <- function(
    r_paths = source_health_r_files(), qmd_paths = source_health_qmd_files()) {
  names_seen <- character()
  for (path in r_paths[file.exists(r_paths)]) {
    names_seen <- c(names_seen, source_health_code_names(parse(path)))
  }
  for (path in qmd_paths[file.exists(qmd_paths)]) {
    output <- tempfile(fileext = ".R")
    old_options <- options(knitr.purl.inline = TRUE)
    tryCatch({
      knitr::purl(path, output = output, quiet = TRUE)
      names_seen <- c(names_seen, source_health_code_names(parse(output)))
    }, finally = {
      options(old_options)
      unlink(output)
    })
  }
  table(names_seen)
}

source_health_metadata_references <- function(
    function_names, path = "data/metadata/file_manifest.csv") {
  if (!length(function_names) || !file.exists(path)) return(character())
  manifest <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character"
  )
  values <- unique(unname(unlist(manifest, use.names = FALSE)))
  intersect(function_names, values[nzchar(values)])
}

source_health_report <- function(
    definition_paths = list.files("R", "\\.[Rr]$", recursive = TRUE, full.names = TRUE),
    reference_paths = source_health_r_files(),
    qmd_paths = source_health_qmd_files(),
    metadata_path = "data/metadata/file_manifest.csv") {
  definitions <- source_health_function_definitions(definition_paths)
  if (!nrow(definitions)) {
    definitions$symbol_occurrences <- integer()
    definitions$metadata_reference <- logical()
    definitions$status <- character()
    return(definitions)
  }
  counts <- source_health_symbol_counts(reference_paths, qmd_paths)
  definition_counts <- table(definitions$function_name)
  dynamic <- source_health_metadata_references(
    unique(definitions$function_name), metadata_path
  )
  definitions$symbol_occurrences <- unname(
    as.integer(counts[definitions$function_name])
  )
  definitions$symbol_occurrences[is.na(definitions$symbol_occurrences)] <- 0L
  definitions$metadata_reference <- definitions$function_name %in% dynamic
  duplicate_definition <- definition_counts[definitions$function_name] > 1L
  symbolic_reference <- definitions$symbol_occurrences > 0L
  definitions$status <- ifelse(
    duplicate_definition, "multiple_definitions",
    ifelse(
      symbolic_reference, "referenced",
      ifelse(definitions$metadata_reference, "metadata_reference", "possible_orphan")
    )
  )
  definitions[order(definitions$status, definitions$file, definitions$line), , drop = FALSE]
}

write_source_health_report <- function(
    report = source_health_report(),
    path = "outputs/build/source_health.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(report, path, row.names = FALSE)
  path
}
