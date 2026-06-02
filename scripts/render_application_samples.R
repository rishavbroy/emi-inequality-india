source("R/packages.R")
source("R/samples/extract_qmd_excerpts.R")
source("R/samples/extract_code_excerpts.R")
source("R/samples/render_writing_sample.R")
source("R/samples/render_coding_sample.R")

# Source the legacy-backed implementation layer last so this script uses the
# same smoke-test/fallback implementations that _targets.R uses.
source("R/samples/zzz_legacy_pipeline_impl.R")

# Load only the packages actually needed here. Loading every project package made
# `make samples` noisy and brittle while the full empirical pipeline is still in
# transition.
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required for application-sample specs. Run `make init-renv`.", call. = FALSE)
}

writing <- render_writing_samples()
coding <- render_coding_samples()

message("Writing samples: ", paste(writing, collapse = ", "))
message("Coding samples: ", paste(coding, collapse = ", "))
