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
  "active figures below use district-level empirical distributions"
)

legacy_figure_captions <- c(
  "paper/report.qmd" = "Trends in earnings, labor‐force participation, and unemployment (ILO, 2024).",
  "paper/report.qmd" = "Number of 2001 districts which absorbed a percentage of a 1991 district's population via name change, clean merger, carve-out, or border shift. Data from Kumar \\& Somanathan (2016).",
  "docs/district-matching.qmd" = "Number of 2001 districts which absorbed a percentage of a 1991 district's population via name change, clean merger, carve-out, or border shift. Data from Kumar \\& Somanathan (2016)."
)

required_map_paths <- c(
  "../outputs/figures/main/collage_main_maps.pdf",
  "../outputs/figures/main/collage_iv_region_maps.pdf"
)

required_map_refs <- c("@fig-map1-fig", "@fig-map2-fig")

blocked_map_text <- c(
  "Final district map figures are withheld",
  "Final map figures are withheld",
  "the withheld final map figures"
)

legacy_table_captions <- c(
  "Summary Statistics for Enrollment Participation Model (Numeric Variables)",
  "Summary Statistics for Enrollment Participation Model (Categorical Variables)",
  "Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit",
  "Summary Statistics for 2SLS Model",
  "First-Stage Regression: EMI Exposure on Linguistic Distance",
  "Second-Stage Regression: Consumption Growth on EMIE (Fitted)"
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

caption_hits <- character()
for (file in unique(names(legacy_figure_captions))) {
  if (!file.exists(file)) {
    caption_hits <- c(caption_hits, paste0(file, " is missing"))
    next
  }
  text <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expected <- unname(legacy_figure_captions[names(legacy_figure_captions) == file])
  missing <- expected[!vapply(expected, grepl, logical(1), x = text, fixed = TRUE)]
  if (length(missing)) {
    caption_hits <- c(caption_hits, paste0(file, " is missing legacy figure caption: ", missing))
  }
}

if (file.exists("paper/report.qmd")) {
  report_text <- paste(readLines("paper/report.qmd", warn = FALSE), collapse = "\n")
  missing <- legacy_table_captions[!vapply(legacy_table_captions, grepl, logical(1), x = report_text, fixed = TRUE)]
  if (length(missing)) {
    caption_hits <- c(caption_hits, paste0("paper/report.qmd is missing legacy table caption: ", missing))
  }
}

for (file in c("paper/report.qmd", "docs/district-matching.qmd")) {
  if (!file.exists(file)) next
  text <- paste(readLines(file, warn = FALSE), collapse = "\n")
  if (!grepl(geometry_note, text, fixed = TRUE)) {
    caption_hits <- c(caption_hits, paste0(file, " is missing the final map geometry-validation note"))
  }
  stale <- blocked_map_paths[vapply(blocked_map_paths, grepl, logical(1), x = text, fixed = TRUE)]
  if (length(stale)) {
    caption_hits <- c(caption_hits, paste0(file, " still references blocked final map path: ", paste(stale, collapse = ", ")))
  }
  dangling <- blocked_map_refs[vapply(blocked_map_refs, grepl, logical(1), x = text, fixed = TRUE)]
  if (length(dangling)) {
    caption_hits <- c(caption_hits, paste0(file, " still references blocked final map figure id: ", paste(dangling, collapse = ", ")))
  }
}

if (length(hits)) {
  out <- do.call(rbind, hits)
  print(out, row.names = FALSE)
  stop("Public-facing placeholder/fallback text remains.", call. = FALSE)
}

if (length(caption_hits)) {
  cat(paste0("- ", caption_hits, collapse = "\n"), "\n")
  stop("Public-facing legacy captions are missing.", call. = FALSE)
}

message("No public-facing placeholder/fallback text detected.")
