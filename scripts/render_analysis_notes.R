# Compatibility wrapper: render analysis notes through {targets} so up-to-date
# Markdown outputs are skipped instead of regenerated unconditionally.
Sys.setenv(
  EMI_CONFIG = Sys.getenv("EMI_CONFIG", "config/final.yml"),
  EMI_RUN_EXTENDED_DIAGNOSTICS = "true",
  EMI_RUN_BENCHMARKS = "true",
  EMI_RENDER_ANALYSIS_NOTES = "true",
  EMI_RENDER_APPLICATION_SAMPLES = Sys.getenv("EMI_RENDER_APPLICATION_SAMPLES", "false")
)

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run `make init-renv`.", call. = FALSE)
}

targets::tar_make(names = tidyselect::all_of("analysis_markdown_files"))
