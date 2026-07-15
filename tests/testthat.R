library(testthat)

project_root <- normalizePath(getwd(), mustWork = TRUE)
Sys.setenv(EMI_PROJECT_ROOT = project_root)

source("R/packages.R")
source("R/config.R")
source("R/paths.R")

# Source all project functions in the same broad order as _targets.R while
# avoiding a hard dependency on targets itself.
source_dirs <- c(
  "R/io", "R/clean", "R/districts", "R/measures", "R/selection",
  "R/iv", "R/diagnostics", "R/benchmarking", "R/output", "R/application_samples"
)
for (dir in source_dirs) {
  files <- sort(list.files(dir, pattern = "\\.R$", full.names = TRUE))
  for (file in files) source(file)
}

test_dir("tests/testthat")
