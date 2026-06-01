# Initialize/update renv for this research repo.
# Run locally after the scaffold is overlaid and core dependencies are installed.

required <- c(
  "targets", "tarchetypes", "yaml", "tidyverse", "readxl", "haven", "sf", "spdep",
  "survey", "marginaleffects", "ivreg", "sandwich", "lmtest", "modelsummary",
  "kableExtra", "stringdist", "fuzzyjoin", "magick", "broom", "testthat", "quarto",
  "readODS", "car"
)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
}

missing <- setdiff(required, rownames(installed.packages()))
if (length(missing) > 0L) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

renv::snapshot(prompt = FALSE)
message("renv.lock updated.")
