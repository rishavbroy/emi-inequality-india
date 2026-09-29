# Shared application-sample configuration and validation.

application_sample_manifest_path <- function() {
  "application-samples/samples.yml"
}

read_application_sample_manifest <- function(path = application_sample_manifest_path()) {
  if (!file.exists(path)) stop("Application-sample manifest does not exist: ", path, call. = FALSE)
  manifest <- yaml::read_yaml(path)
  validate_application_sample_manifest(manifest)
  manifest
}

validate_application_sample_manifest <- function(manifest) {
  required <- c("schema_version", "paper", "identity", "writing", "coding_outputs", "coding")
  missing <- setdiff(required, names(manifest))
  if (length(missing)) {
    stop("Application-sample manifest is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(as.integer(manifest$schema_version), 1L)) {
    stop("Unsupported application-sample manifest schema version.", call. = FALSE)
  }
  if (is.null(manifest$paper$source) || !nzchar(manifest$paper$source)) {
    stop("Application-sample manifest must name the current paper source.", call. = FALSE)
  }
  if (!all(c("named", "anonymous") %in% names(manifest$identity))) {
    stop("Application-sample manifest must define named and anonymous identities.", call. = FALSE)
  }
  anonymous_identity <- manifest$identity$anonymous %||% list()
  acknowledgment_names <- as.character(unlist(
    anonymous_identity$acknowledgment_names %||% character(), use.names = FALSE
  ))
  acknowledgment_replacement <- as.character(anonymous_identity$acknowledgment_replacement %||% "")
  if (length(acknowledgment_names) && !nzchar(acknowledgment_replacement)) {
    stop("Anonymous acknowledgment names require a replacement label.", call. = FALSE)
  }
  writing_ids <- vapply(manifest$writing, function(x) x$id %||% "", character(1))
  coding_ids <- vapply(manifest$coding, function(x) x$id %||% "", character(1))
  if (any(!nzchar(writing_ids)) || anyDuplicated(writing_ids)) {
    stop("Writing-sample IDs must be nonempty and unique.", call. = FALSE)
  }
  if (any(!nzchar(coding_ids)) || anyDuplicated(coding_ids)) {
    stop("Coding-sample IDs must be nonempty and unique.", call. = FALSE)
  }
  output_ids <- names(manifest$coding_outputs)
  if (is.null(output_ids) || any(!nzchar(output_ids)) || anyDuplicated(output_ids)) {
    stop("coding_outputs must be a named mapping with unique IDs.", call. = FALSE)
  }
  referenced_outputs <- unique(unlist(lapply(manifest$coding, function(spec) {
    c(
      spec$outputs %||% character(),
      unlist(lapply(spec$excerpts, function(x) x$outputs %||% character()), use.names = FALSE)
    )
  }), use.names = FALSE))
  missing_outputs <- setdiff(referenced_outputs, output_ids)
  if (length(missing_outputs)) {
    stop("Coding samples reference unknown selected outputs: ", paste(missing_outputs, collapse = ", "), call. = FALSE)
  }
  output_code_files <- unique(unlist(lapply(manifest$coding_outputs, function(x) {
    unlist(x$code_files %||% character(), use.names = FALSE)
  }), use.names = FALSE))
  project_root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  code_file_paths <- file.path(project_root, output_code_files)
  missing_code_files <- output_code_files[!file.exists(code_file_paths)]
  if (length(missing_code_files)) {
    stop(
      "Coding-sample output code files do not exist: ",
      paste(missing_code_files, collapse = ", "),
      call. = FALSE
    )
  }
  latex_outputs <- manifest$coding_outputs[vapply(
    manifest$coding_outputs, function(x) identical(x$type %||% "", "latex"), logical(1)
  )]
  bad_labels <- names(latex_outputs)[!vapply(
    latex_outputs,
    function(x) grepl("^tbl-[A-Za-z0-9_-]+$", x$paper_label %||% ""),
    logical(1)
  )]
  if (length(bad_labels)) {
    stop(
      "LaTeX coding outputs must declare a tbl- paper_label: ",
      paste(bad_labels, collapse = ", "),
      call. = FALSE
    )
  }
  figure_outputs <- manifest$coding_outputs[vapply(
    manifest$coding_outputs, function(x) identical(x$type %||% "", "figure"), logical(1)
  )]
  bad_figure_labels <- names(figure_outputs)[!vapply(
    figure_outputs,
    function(x) grepl("^fig-[A-Za-z0-9_-]+$", x$paper_label %||% ""),
    logical(1)
  )]
  if (length(bad_figure_labels)) {
    stop(
      "Figure coding outputs must declare a fig- paper_label: ",
      paste(bad_figure_labels, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

application_sample_variants <- function(manifest = read_application_sample_manifest()) {
  names(manifest$identity)
}

anonymous_sample_forbidden_strings <- function(manifest) {
  identity <- manifest$identity$anonymous %||% list()
  unique(c(
    as.character(unlist(identity$forbidden_strings %||% character(), use.names = FALSE)),
    as.character(unlist(identity$acknowledgment_names %||% character(), use.names = FALSE))
  ))
}

application_sample_output_path <- function(kind, sample_id, variant, manifest = read_application_sample_manifest()) {
  identity <- manifest$identity[[variant]]
  if (is.null(identity)) stop("Unknown application-sample identity: ", variant, call. = FALSE)
  prefix <- identity$prefix %||% variant
  kind_label <- if (identical(kind, "writing")) "WritingSample" else "CodeSample"
  suffix <- if (identical(kind, "writing")) {
    if (identical(sample_id, "full")) "Full" else sample_id
  } else {
    if (identical(sample_id, "short")) "Short" else if (identical(sample_id, "long")) "Long" else sample_id
  }
  filename <- paste0(prefix, "_", kind_label, "_", suffix, ".pdf")
  file.path("application-samples", "output", filename)
}

application_sample_expected_outputs <- function(manifest = read_application_sample_manifest()) {
  variants <- application_sample_variants(manifest)
  paths <- character()
  for (variant in variants) {
    for (spec in manifest$writing) {
      paths <- c(paths, application_sample_output_path("writing", spec$id, variant, manifest))
    }
    for (spec in manifest$coding) {
      paths <- c(paths, application_sample_output_path("coding", spec$id, variant, manifest))
    }
  }
  paths
}

application_sample_input_files <- function(manifest_path = application_sample_manifest_path()) {
  manifest <- read_application_sample_manifest(manifest_path)
  coding_files <- unlist(lapply(manifest$coding, function(spec) {
    vapply(spec$excerpts, function(x) x$file %||% NA_character_, character(1))
  }), use.names = FALSE)
  files <- unique(c(
    manifest_path,
    manifest$paper$source,
    "application-samples/filters/select-sections.lua",
    "application-samples/filters/link-citations-to-paper.lua",
    coding_files[!is.na(coding_files)]
  ))
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    stop("Application-sample inputs are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  normalizePath(files, mustWork = TRUE)
}

prune_application_sample_kind <- function(kind) {
  token <- if (identical(kind, "writing")) "WritingSample" else "(Code|Coding)Sample"
  output_files <- list.files(
    "application-samples/output",
    pattern = paste0(token, ".*[.]pdf$"),
    full.names = TRUE
  )
  work_files <- list.files(
    "application-samples/.work",
    pattern = token,
    full.names = TRUE
  )
  unlink(c(output_files, work_files), recursive = TRUE, force = TRUE)
  invisible(TRUE)
}
