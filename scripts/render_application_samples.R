# Render application samples through the {targets} graph.
#
# This wrapper exists for compatibility with older workflows that called this
# script directly. Public render caching and invalidation are owned by targets.

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run `make init-renv`.", call. = FALSE)
}

if (!nzchar(Sys.getenv("EMI_CONFIG"))) {
  Sys.setenv(EMI_CONFIG = "config/final.yml")
}
Sys.setenv(EMI_RENDER_APPLICATION_SAMPLES = "true")

targets::tar_make(names = tidyselect::all_of(c("writing_sample_pdfs", "coding_sample_pdfs")))
writing <- targets::tar_read(writing_sample_pdfs)
coding <- targets::tar_read(coding_sample_pdfs)

message("Writing samples: ", paste(writing, collapse = ", "))
message("Coding samples: ", paste(coding, collapse = ", "))
