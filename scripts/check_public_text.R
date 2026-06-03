# Check public-facing files for placeholder or fallback text.
# This is intentionally conservative: archived legacy files are excluded because
# they preserve historical drafts and sample sources.

roots <- c("paper", "docs", "application-samples", "R")
files <- unlist(lapply(roots, function(root) {
  if (!dir.exists(root)) return(character())
  list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE)
}), use.names = FALSE)

files <- files[file.exists(files)]
files <- files[!dir.exists(files)]
files <- files[!grepl("(^|/)archive(/|$)", files)]
files <- files[!grepl("(^|/)application-samples/output(/|$)", files)]
files <- files[!grepl("(^|/)application-samples/\\.work(/|$)", files)]
files <- files[grepl("\\.(qmd|md|R|yml|yaml|tex)$", files, ignore.case = TRUE)]

patterns <- c(
  "Insert the current",
  "Generated fallback",
  "This file is part of the EMI inequality research pipeline",
  "Functions are intentionally small enough"
)

hits <- list()
for (file in files) {
  txt <- readLines(file, warn = FALSE)
  for (pattern in patterns) {
    idx <- grep(pattern, txt, fixed = TRUE)
    if (length(idx)) {
      hits[[length(hits) + 1L]] <- data.frame(
        file = file,
        line = idx,
        pattern = pattern,
        text = txt[idx],
        stringsAsFactors = FALSE
      )
    }
  }
}

if (length(hits)) {
  out <- do.call(rbind, hits)
  print(out, row.names = FALSE)
  stop("Public-facing placeholder/scaffold text remains.", call. = FALSE)
}

message("No public-facing placeholder/scaffold text detected.")
