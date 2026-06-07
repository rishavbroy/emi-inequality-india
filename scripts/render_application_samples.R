source("R/packages.R")
source("R/application_samples/extract_qmd_excerpts.R")
source("R/application_samples/extract_code_excerpts.R")
source("R/application_samples/render_writing_sample.R")
source("R/application_samples/render_coding_sample.R")

dir.create("application-samples/output", recursive = TRUE, showWarnings = FALSE)
dir.create("application-samples/.work", recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required for application-sample specs. Run `make init-renv`.", call. = FALSE)
}

writing <- render_writing_samples()
coding <- render_coding_samples()

message("Writing samples: ", paste(writing, collapse = ", "))
message("Coding samples: ", paste(coding, collapse = ", "))
