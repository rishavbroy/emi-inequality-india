# Audit Quarto cross-references in public report and note sources.

args <- commandArgs(trailingOnly = TRUE)
strict_report <- "--strict-report" %in% args
explicit_qmd_files <- args[!startsWith(args, "--")]

if (length(explicit_qmd_files)) {
  qmd_files <- explicit_qmd_files[file.exists(explicit_qmd_files)]
} else {
  source("scripts/public_output_contract.R", local = TRUE)

  qmd_files <- public_qmd_sources()
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
}

extract_matches <- function(text, pattern) {
  hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  hits[!is.na(hits)]
}

scan_crossrefs <- function(path, include_generated_tex_labels = TRUE) {
  lines <- readLines(path, warn = FALSE)
  text <- paste(lines, collapse = "\n")
  refs <- unique(extract_matches(text, "@(fig|tbl|sec|eq)-[A-Za-z0-9_-]+"))
  latex_refs <- unique(extract_matches(text, "\\\\ref\\{(fig|tbl|sec|eq)-[A-Za-z0-9_-]+\\}"))
  latex_refs <- sub("^\\\\ref\\{", "", latex_refs)
  latex_refs <- sub("\\}$", "", latex_refs)
  # Quarto/Pandoc labels can appear either as a bare attribute,
  # `{#fig-example}`, or alongside other attributes,
  # `{#fig-example fig-pos="H" width="100%"}`. The strict public
  # cross-reference audit should count both as labels.
  brace_labels <- extract_matches(text, "\\{#(fig|tbl|sec|eq)-[A-Za-z0-9_-]+(?:[^}]*)\\}")
  brace_labels <- sub("^\\{#", "", brace_labels)
  brace_labels <- sub("[[:space:]}].*$", "", brace_labels)

  chunk_label_lines <- grep("^\\s*#\\|\\s*label:\\s*(fig|tbl|sec|eq)-[A-Za-z0-9_-]+\\s*$", lines, value = TRUE, perl = TRUE)
  chunk_labels <- sub("^\\s*#\\|\\s*label:\\s*", "", chunk_label_lines)
  chunk_labels <- trimws(chunk_labels)

  table_tex_files <- if (include_generated_tex_labels && dir.exists("outputs/tables")) {
    list.files("outputs/tables", pattern = "\\.tex$", full.names = TRUE, recursive = TRUE)
  } else {
    character()
  }
  tex_labels <- unique(unlist(lapply(table_tex_files, function(tex_path) {
    tex <- paste(readLines(tex_path, warn = FALSE), collapse = "\n")
    hits <- extract_matches(tex, "\\\\label\\{(fig|tbl|sec|eq)-[A-Za-z0-9_-]+\\}")
    hits <- sub("^\\\\label\\{", "", hits)
    sub("\\}$", "", hits)
  }), use.names = FALSE))

  labels <- unique(c(brace_labels, chunk_labels, tex_labels))
  refs_no_at <- unique(c(sub("^@", "", refs), latex_refs))
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

results <- do.call(
  rbind,
  lapply(
    qmd_files,
    scan_crossrefs,
    include_generated_tex_labels = !length(explicit_qmd_files)
  )
)

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

if (strict_report) {
  bad <- results[results$unresolved > 0L, , drop = FALSE]
  if (nrow(bad)) {
    details <- vapply(seq_len(nrow(bad)), function(i) {
      row <- bad[i, ]
      paste0(row$file, ": unresolved=", row$unresolved_refs)
    }, character(1))
    stop(
      "Strict public cross-reference audit failed:\n",
      paste(details, collapse = "\n"),
      call. = FALSE
    )
  }
}

message(if (strict_report) "Strict public cross-reference audit completed." else "Draft cross-reference audit completed.")
