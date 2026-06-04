# Audit Quarto cross-references in public report and note sources.

args <- commandArgs(trailingOnly = TRUE)
strict_report <- "--strict-report" %in% args

qmd_files <- c(
  "paper/report.qmd",
  "paper/appendix.qmd",
  "docs/district-matching.qmd",
  "docs/long-paths-and-8-3-filenames.qmd"
)

work_files <- character()
if (dir.exists("application-samples/.work")) {
  work_files <- list.files(
    "application-samples/.work",
    pattern = "\\.qmd$",
    full.names = TRUE,
    recursive = TRUE
  )
}

qmd_files <- unique(c(qmd_files[file.exists(qmd_files)], work_files))

extract_matches <- function(text, pattern) {
  hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  hits[!is.na(hits)]
}

scan_crossrefs <- function(path) {
  lines <- readLines(path, warn = FALSE)
  text <- paste(lines, collapse = "\n")

  refs <- unique(extract_matches(text, "@(fig|tbl|sec|eq)-[A-Za-z0-9_-]+"))
  brace_labels <- extract_matches(text, "\\{#(fig|tbl|sec|eq)-[A-Za-z0-9_-]+\\}")
  brace_labels <- sub("^\\{#", "", sub("\\}$", "", brace_labels))

  chunk_label_lines <- grep("^\\s*#\\|\\s*label:\\s*(fig|tbl|sec|eq)-[A-Za-z0-9_-]+\\s*$", lines, value = TRUE, perl = TRUE)
  chunk_labels <- sub("^\\s*#\\|\\s*label:\\s*", "", chunk_label_lines)
  chunk_labels <- trimws(chunk_labels)

  labels <- unique(c(brace_labels, chunk_labels))
  refs_no_at <- sub("^@", "", refs)
  unresolved <- refs_no_at[!refs_no_at %in% labels]

  data.frame(
    file = path,
    refs = length(refs_no_at),
    labels = length(labels),
    unresolved = length(unresolved),
    unresolved_refs = paste(sort(unique(unresolved)), collapse = "; "),
    stringsAsFactors = FALSE
  )
}

if (!length(qmd_files)) {
  stop("No QMD files found for cross-reference audit.", call. = FALSE)
}

results <- do.call(rbind, lapply(qmd_files, scan_crossrefs))

cat("Cross-reference audit\n")
cat("=====================\n")
for (i in seq_len(nrow(results))) {
  row <- results[i, ]
  cat(sprintf(
    "- %s: %s refs, %s labels, %s unresolved\n",
    row$file,
    row$refs,
    row$labels,
    row$unresolved
  ))
  if (nzchar(row$unresolved_refs)) {
    cat(sprintf("  unresolved: %s\n", row$unresolved_refs))
  }
}

report_row <- results[results$file == "paper/report.qmd", , drop = FALSE]
if (strict_report && nrow(report_row) && report_row$unresolved > 0L) {
  stop(
    sprintf(
      "Strict report cross-reference audit failed: paper/report.qmd has unresolved references: %s",
      report_row$unresolved_refs
    ),
    call. = FALSE
  )
}

message(if (strict_report) "Strict report cross-reference audit completed." else "Draft cross-reference audit completed.")
