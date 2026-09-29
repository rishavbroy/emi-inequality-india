# Render coding samples from marker-delimited research-code excerpts in the shared manifest.

render_coding_samples <- function(manifest_path = application_sample_manifest_path()) {
  manifest <- read_application_sample_manifest(manifest_path)
  reference_labels <- read_latex_reference_labels(paper_reference_aux_path(manifest$paper$source))
  for (spec in manifest$coding) validate_code_excerpt_markers(spec)
  prune_application_sample_kind("coding")
  outputs <- character()
  for (variant in application_sample_variants(manifest)) {
    for (spec in manifest$coding) {
      outputs <- c(outputs, render_one_coding_sample(spec, variant, manifest, reference_labels))
    }
  }
  unname(outputs)
}


coding_sample_body <- function(spec, variant, manifest, reference_labels) {
  unlist(lapply(spec$excerpts, function(excerpt) {
    c(
      extract_code_excerpt(excerpt, variant, manifest),
      coding_sample_output_lines(excerpt$outputs %||% character(), variant, manifest, reference_labels)
    )
  }), use.names = FALSE)
}

render_one_coding_sample <- function(spec, variant, manifest, reference_labels) {
  output <- application_sample_output_path("coding", spec$id, variant, manifest)
  work_dir <- file.path("application-samples", ".work")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  output_qmd <- file.path(work_dir, paste0(tools::file_path_sans_ext(basename(output)), ".qmd"))
  body <- coding_sample_body(spec, variant, manifest, reference_labels)
  assemble_coding_sample_qmd(spec, variant, manifest, body, output_qmd)
  render_qmd_to_pdf(output_qmd, output)
  if (identical(variant, "anonymous")) {
    validate_anonymous_sample(output, anonymous_sample_forbidden_strings(manifest))
  }
  output
}
