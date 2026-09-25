# Render writing samples from the tracked application-sample manifest.

render_writing_samples <- function(manifest_path = application_sample_manifest_path(), output_files = NULL) {
  force(output_files)
  prune_application_sample_kind("writing")
  manifest <- read_application_sample_manifest(manifest_path)
  excerpt_specs <- vapply(manifest$writing, function(x) !identical(x$mode %||% "excerpt", "full"), logical(1))
  reference_labels <- NULL
  if (any(excerpt_specs)) {
    reference_labels <- read_latex_reference_labels(paper_reference_aux_path(manifest$paper$source))
  }

  outputs <- character()
  expected_pages <- integer()
  for (variant in application_sample_variants(manifest)) {
    for (spec in manifest$writing) {
      outputs <- c(outputs, render_one_writing_sample(spec, variant, manifest, reference_labels))
      expected_pages <- c(expected_pages, as.integer(spec$target_pages %||% NA_integer_))
    }
  }
  validate_writing_sample_page_counts(outputs, expected_pages)
  unname(outputs)
}

render_one_writing_sample <- function(spec, variant, manifest, reference_labels = NULL) {
  source <- manifest$paper$source
  if (!identical(spec$mode %||% "excerpt", "full")) {
    validate_writing_section_ids(source, unlist(spec$sections, use.names = FALSE))
  }

  output <- application_sample_output_path("writing", spec$id, variant, manifest)
  work_dir <- file.path("application-samples", ".work")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  output_qmd <- file.path(work_dir, paste0(tools::file_path_sans_ext(basename(output)), ".qmd"))

  assemble_writing_sample_qmd(source, spec, variant, manifest, output_qmd, reference_labels)
  render_qmd_to_pdf(output_qmd, output)
  if (identical(variant, "anonymous")) {
    validate_anonymous_sample(output, manifest$identity$anonymous$forbidden_strings %||% character())
  }
  output
}
