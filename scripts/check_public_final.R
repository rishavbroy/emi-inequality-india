# Strict public-output checks for the final replication mode.

required_files <- c(
  "paper/report.pdf",
  "docs/district-matching.html",
  "docs/long-paths-and-8-3-filenames.html",
  "application-samples/output/RishavRoy_WritingSample.pdf",
  "application-samples/output/RishavRoy_WritingSample10pg.pdf",
  "application-samples/output/RishavRoy_WritingSample5pg.pdf",
  "application-samples/output/RishavRoy_CodingSample.pdf",
  "application-samples/output/RishavRoy_CodingSample47pg.pdf",
  "application-samples/output/RishavRoy_CodingSample25pg.pdf"
)

missing_or_empty <- required_files[!file.exists(required_files) | file.info(required_files)$size <= 0]

report_lines <- readLines("paper/report.qmd", warn = FALSE)
report_text <- paste(report_lines, collapse = "\n")

refs <- unique(regmatches(report_text, gregexpr("@(fig|tbl|sec|eq)-[A-Za-z0-9_-]+", report_text, perl = TRUE))[[1]])
refs <- refs[!is.na(refs)]
labels <- unique(regmatches(report_text, gregexpr("\\{#(fig|tbl|sec|eq)-[A-Za-z0-9_-]+\\}", report_text, perl = TRUE))[[1]])
labels <- sub("^\\{#", "@", sub("\\}$", "", labels))
missing_refs <- setdiff(refs, labels)

failures <- character()
if (length(missing_or_empty)) {
  failures <- c(failures, paste0("Missing or empty final output: ", missing_or_empty))
}
if (length(missing_refs)) {
  failures <- c(failures, paste0("Unresolved report cross-reference: ", missing_refs))
}

if (length(failures)) {
  cat(paste0("- ", failures, collapse = "\n"), "\n")
  stop("Final public checks failed.", call. = FALSE)
}

message("Final public checks passed.")
