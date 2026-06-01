# Initialize/update renv for this research repo.
# Design goals:
# - Non-interactive: works from Terminal, CI, or a fresh clone.
# - Explicit: no hidden dependence on RStudio's CRAN mirror settings.
# - Reproducible: records package versions in renv.lock.
# - Diagnostic: reports likely system-dependency issues clearly.

message("== EMI repo: initializing renv ==")

# CRAN / repository configuration

default_cran <- "https://cloud.r-project.org"

repos <- getOption("repos")
repos_missing <- is.null(repos) ||
  length(repos) == 0L ||
  identical(unname(repos["CRAN"]), "@CRAN@") ||
  is.na(repos["CRAN"]) ||
  repos["CRAN"] == ""

if (repos_missing) {
  options(repos = c(CRAN = default_cran))
  message("Set CRAN mirror to: ", default_cran)
} else {
  message("Using existing CRAN mirror: ", paste(repos, collapse = ", "))
}

# Prefer multiple cores for source installs when available.
options(Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 1L))

# Package list

required <- c(
  "renv",
  "targets",
  "tarchetypes",
  "yaml",
  "tidyverse",
  "readxl",
  "haven",
  "sf",
  "spdep",
  "survey",
  "marginaleffects",
  "ivreg",
  "sandwich",
  "lmtest",
  "modelsummary",
  "kableExtra",
  "stringdist",
  "fuzzyjoin",
  "magick",
  "broom",
  "testthat",
  "quarto",
  "readODS",
  "car"
)

# System diagnostics

check_cmd <- function(cmd) {
  nzchar(Sys.which(cmd))
}

message("Checking common system tools...")

system_checks <- c(
  R = TRUE,
  quarto = check_cmd("quarto"),
  gdal_config = check_cmd("gdal-config"),
  geos_config = check_cmd("geos-config"),
  pkg_config = check_cmd("pkg-config")
)

print(system_checks)

if (!system_checks[["quarto"]]) {
  message(
    "Note: Quarto CLI was not found on PATH. The R package 'quarto' can still ",
    "install, but rendering .qmd files requires the Quarto CLI."
  )
}

if (!system_checks[["gdal_config"]] || !system_checks[["geos_config"]]) {
  message(
    "Note: sf/spatial packages may require GDAL/GEOS/PROJ system libraries. ",
    "On macOS, these are commonly installed with Homebrew, e.g. ",
    "`brew install gdal geos proj pkg-config`."
  )
}

# Install renv first

if (!requireNamespace("renv", quietly = TRUE)) {
  message("Installing renv...")
  install.packages("renv", repos = getOption("repos"))
}

# Initialize renv

if (!file.exists("renv.lock")) {
  message("Creating bare renv project...")
  renv::init(bare = TRUE)
} else {
  message("renv.lock already exists; using existing renv project.")
  renv::activate()
}

# Install required packages

installed <- rownames(installed.packages())
missing <- setdiff(required, installed)

if (length(missing) > 0L) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))

  # renv::install uses renv's installation machinery/cache and can install from
  # CRAN, GitHub, Bioconductor, etc. For plain package names, it installs from CRAN.
  renv::install(missing, prompt = FALSE)
} else {
  message("All required packages are already installed.")
}

# Snapshot lockfile

message("Writing renv.lock...")
renv::snapshot(
  packages = required,
  prompt = FALSE
)

message("renv.lock updated.")
message("== Done ==")