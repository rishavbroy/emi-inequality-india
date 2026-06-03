# Check rendered/public files for placeholder value text.
#
# This check intentionally normalizes whitespace so it catches phrases that are
# split across lines in HTML or Markdown output.

paths <- c(
  list.files("paper", pattern = "\\.(html|md)$", full.names = TRUE),
  list.files("docs", pattern = "\\.(html|md)$", full.names = TRUE),
  list.files("application-samples/.work", pattern = "\\.(html|md)$", full.names = TRUE, recursive = TRUE)
)
paths <- paths[file.exists(paths)]

patterns <- c(
  "not yet available",
  "not run in current draft pipeline"
)

hits <- character()
for (path in paths) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  text <- gsub("\\s+", " ", text)
  for (pattern in patterns) {
    if (grepl(pattern, text, fixed = TRUE)) {
      hits <- c(hits, paste0(path, " contains '", pattern, "'"))
    }
  }
}

if (length(hits)) {
  cat(paste0("- ", hits, collapse = "\n"), "\n")
  stop("Rendered public text still contains placeholder value text.", call. = FALSE)
}

message("No rendered placeholder value text detected in public HTML/Markdown files.")
