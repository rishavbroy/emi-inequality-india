# Verify that public render dependencies exist before Quarto starts.
# This catches incomplete pipelines with one concise error.

args <- commandArgs(trailingOnly = TRUE)
require_stamp <- "--require-final-stamp" %in% args

failures <- character()
add_failure <- function(...) failures <<- c(failures, paste0(...))

if (require_stamp && !file.exists(".pipeline-final-ok")) {
  add_failure("Missing .pipeline-final-ok; run `make pipeline-final` successfully before rendering public outputs.")
}

required_files <- c(
  "paper/references.bib",
  "outputs/tables/main/sum_tbl_probit_quant.csv",
  "outputs/tables/main/sum_tbl_probit_cat.csv",
  "outputs/tables/main/probit_mfx.csv",
  "outputs/tables/main/sum_tbl_iv.csv",
  "outputs/tables/main/fs_cons.csv",
  "outputs/tables/main/cons_iv.csv",
  "outputs/figures/main/fig_ilo_trends.png",
  "outputs/figures/main/district_carveouts_shifts.png"
)
missing <- required_files[!file.exists(required_files) | file.info(required_files)$size <= 0]
if (length(missing)) add_failure("Missing required public file(s): ", paste(missing, collapse = ", "))

check_bibliography_paths <- function(path) {
  if (!file.exists(path)) return()
  lines <- readLines(path, warn = FALSE)
  idx <- grep("^bibliography:\\s*", lines)
  for (i in idx) {
    val <- trimws(sub("^bibliography:\\s*", "", lines[[i]]))
    if (!nzchar(val)) next
    val <- gsub("^['\"]|['\"]$", "", val)
    full <- file.path(dirname(path), val)
    if (!file.exists(full)) add_failure(path, " points to missing bibliography: ", val)
  }
}

for (qmd in c("paper/report.qmd", "paper/appendix.qmd", "docs/district-matching.qmd", "docs/long-paths-and-8-3-filenames.qmd")) {
  check_bibliography_paths(qmd)
}

if (file.exists("paper/report.qmd")) {
  report <- paste(readLines("paper/report.qmd", warn = FALSE), collapse = "\n")
  if (grepl("read_public_table\\(", report) && !grepl("public-output-table-helper", report, fixed = TRUE)) {
    add_failure("paper/report.qmd calls read_public_table() but lacks public-output-table-helper.")
  }
}

if (length(failures)) {
  cat(paste0("- ", failures, collapse = "\n"), "\n")
  stop("Required public render dependencies are missing.", call. = FALSE)
}

message("Required public render dependencies exist.")
