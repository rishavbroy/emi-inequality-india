# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

read_description_packages <- function(path = "DESCRIPTION") {
  if (!file.exists(path)) {
    stop("Missing DESCRIPTION file; package dependencies should be declared there.", call. = FALSE)
  }
  desc <- read.dcf(path)
  fields <- intersect(c("Imports", "Suggests"), colnames(desc))
  pkgs <- unlist(strsplit(paste(desc[1, fields], collapse = ","), ","), use.names = FALSE)
  pkgs <- trimws(gsub("\\s*\\(.*\\)", "", pkgs))
  pkgs <- pkgs[nzchar(pkgs)]
  unique(pkgs)
}

#' project packages
#'
#' @return Character vector of packages declared in DESCRIPTION Imports/Suggests.
project_packages <- function() {
  read_description_packages()
}

#' load project packages
#'
#' @return Invisibly returns TRUE after loading declared project packages.
load_project_packages <- function() {
  invisible(lapply(project_packages(), require, character.only = TRUE))
}

#' check project packages
#'
#' @return Invisibly returns missing packages.
check_project_packages <- function() {
  missing <- project_packages()[!vapply(project_packages(), requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) warning("Missing packages: ", paste(missing, collapse = ", "))
  invisible(missing)
}
