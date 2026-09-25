# Render coding samples from marker-delimited R excerpts in the shared manifest.

render_coding_samples <- function(manifest_path = application_sample_manifest_path(), output_files = NULL) {
  force(output_files)
  prune_application_sample_kind("coding")
  manifest <- read_application_sample_manifest(manifest_path)
  outputs <- character()
  for (variant in application_sample_variants(manifest)) {
    for (spec in manifest$coding) {
      outputs <- c(outputs, render_one_coding_sample(spec, variant, manifest))
    }
  }
  unname(outputs)
}

render_one_coding_sample <- function(spec, variant, manifest) {
  output <- application_sample_output_path("coding", spec$id, variant, manifest)
  work_dir <- file.path("application-samples", ".work")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  output_qmd <- file.path(work_dir, paste0(tools::file_path_sans_ext(basename(output)), ".qmd"))
  body <- extract_code_excerpts(spec)
  assemble_coding_sample_qmd(spec, variant, manifest, body, output_qmd)
  render_qmd_to_pdf(output_qmd, output)
  if (identical(variant, "anonymous")) {
    validate_anonymous_sample(output, manifest$identity$anonymous$forbidden_strings %||% character())
  }
  output
}
