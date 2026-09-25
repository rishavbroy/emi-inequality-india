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
  referenced_outputs <- unique(unlist(lapply(manifest$coding, function(x) x$outputs %||% character()), use.names = FALSE))
  missing_outputs <- setdiff(referenced_outputs, output_ids)
  if (length(missing_outputs)) {
    stop("Coding samples reference unknown selected outputs: ", paste(missing_outputs, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

application_sample_variants <- function(manifest = read_application_sample_manifest()) {
  names(manifest$identity)
}

application_sample_output_path <- function(kind, sample_id, variant, manifest = read_application_sample_manifest()) {
  identity <- manifest$identity[[variant]]
  if (is.null(identity)) stop("Unknown application-sample identity: ", variant, call. = FALSE)
  prefix <- identity$prefix %||% variant
  kind_label <- if (identical(kind, "writing")) "WritingSample" else "CodingSample"
  if (identical(kind, "coding") && identical(sample_id, "full")) {
    filename <- paste0(prefix, "_", kind_label, ".pdf")
  } else {
    suffix <- if (identical(sample_id, "full")) "Full" else sample_id
    if (identical(kind, "coding") && identical(sample_id, "short")) suffix <- "Short"
    filename <- paste0(prefix, "_", kind_label, "_", suffix, ".pdf")
  }
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
    coding_files[!is.na(coding_files)]
  ))
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    stop("Application-sample inputs are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  normalizePath(files, mustWork = TRUE)
}

prune_application_sample_kind <- function(kind) {
  token <- if (identical(kind, "writing")) "WritingSample" else "CodingSample"
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
