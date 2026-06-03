# Rebuild public-facing QMDs from the legacy Rmd.
#
# This script is deliberately mechanical. Its job is to copy Rishav's original
# prose from archive/legacy-paper-drafts/580-Draft-ECON-580.Rmd into static QMD
# source files, while making only the syntax changes needed for Quarto to render.

legacy_path <- "archive/legacy-paper-drafts/580-Draft-ECON-580.Rmd"
legacy_lines <- readLines(legacy_path, warn = FALSE)

strip_yaml <- function(lines) {
  if (length(lines) >= 2L && identical(lines[[1]], "---")) {
    close <- which(lines[-1L] == "---")
    if (length(close)) return(lines[-seq_len(close[[1]] + 1L)])
  }
  lines
}

strip_code_chunks <- function(lines) {
  out <- character()
  in_chunk <- FALSE
  for (line in lines) {
    if (startsWith(line, "```")) {
      in_chunk <- !in_chunk
      next
    }
    if (!in_chunk) out <- c(out, line)
  }
  out
}

find_line <- function(lines, pattern, start = 1L) {
  hit <- grep(pattern, lines, fixed = TRUE)
  hit <- hit[hit >= start]
  if (!length(hit)) stop("Could not find: ", pattern, call. = FALSE)
  hit[[1]]
}

extract_lines <- function(lines, start, end = NULL) {
  i <- find_line(lines, start)
  j <- if (is.null(end)) length(lines) + 1L else find_line(lines, end, i + 1L)
  lines[i:(j - 1L)]
}

insert_sample_markers <- function(lines, markers) {
  locations <- lapply(markers, function(marker) {
    start <- find_line(lines, marker$start)
    end <- if (is.null(marker$end)) length(lines) + 1L else find_line(lines, marker$end, start + 1L)
    list(start = start, end = end, attrs = marker$attrs, id = marker$id)
  })

  starts <- vapply(locations, `[[`, integer(1), "start")
  locations <- locations[order(starts)]

  for (i in seq_len(length(locations) - 1L)) {
    if (locations[[i]]$end > locations[[i + 1L]]$start) {
      stop("Overlapping sample excerpts: ", locations[[i]]$id, " and ", locations[[i + 1L]]$id, call. = FALSE)
    }
  }

  out <- character()
  cursor <- 1L
  for (location in locations) {
    if (location$start > cursor) out <- c(out, lines[cursor:(location$start - 1L)])
    out <- c(out, paste0("::: {", location$attrs, "}"), lines[location$start:(location$end - 1L)], ":::", "")
    cursor <- location$end
  }
  if (cursor <= length(lines)) out <- c(out, lines[cursor:length(lines)])
  out
}

convert_legacy_math <- function(lines) {
  # Only standalone display-math delimiters should be converted. Replacing every
  # occurrence of \[ or \] corrupts LaTeX line-spacing commands such as \\[-0.0em].
  lines[trimws(lines) == "\\["] <- "$$"
  lines[trimws(lines) == "\\]"] <- "$$"
  lines <- gsub("\\\\\\(", "$", lines)
  lines <- gsub("\\\\\\)", "$", lines)
  lines
}

inline_expression_keys <- new.env(parent = emptyenv())
inline_expression_order <- character()

replace_inline_r_with_target_values <- function(lines) {
  replace_one <- function(line) {
    matches <- gregexpr("`r ([^`]+)`", line, perl = TRUE)[[1]]
    if (matches[[1]] < 0L) return(line)
    pieces <- regmatches(line, list(matches))[[1]]
    for (piece in pieces) {
      expr <- sub("^`r ", "", sub("`$", "", piece))
      key <- paste0("inline_", substr(digest::digest(expr, algo = "xxhash32"), 1L, 8L))
      if (!exists(key, inline_expression_keys, inherits = FALSE)) {
        assign(key, expr, envir = inline_expression_keys)
        inline_expression_order <<- c(inline_expression_order, key)
      }
      line <- sub(piece, paste0("`r report_value(\"", key, "\")`"), line, fixed = TRUE)
    }
    line
  }
  vapply(lines, replace_one, character(1))
}

quote_string <- function(x) paste(deparse(x, control = "all"), collapse = "")

report_setup_chunk <- function() {
  expr_lines <- if (length(inline_expression_order)) {
    values <- vapply(inline_expression_order, function(key) get(key, inline_expression_keys), character(1))
    c(
      "legacy_inline_expressions <- list(",
      paste0(
        "  ", inline_expression_order, " = ",
        vapply(values, quote_string, character(1)),
        collapse = ",\n"
      ),
      ")"
    )
  } else {
    "legacy_inline_expressions <- list()"
  }

  c(
    "```{r report-target-values, include=FALSE}",
    "safe_tar_read <- function(name) {",
    "  if (!requireNamespace(\"targets\", quietly = TRUE)) return(NULL)",
    "  tryCatch(targets::tar_read_raw(name), error = function(e) NULL)",
    "}",
    "report_values <- safe_tar_read(\"report_values\")",
    "if (is.null(report_values) || !is.list(report_values)) report_values <- list()",
    expr_lines,
    "report_value <- function(key) {",
    "  value <- report_values[[key]]",
    "  if (is.null(value)) value <- report_values[[legacy_inline_expressions[[key]]]]",
    "  if (is.null(value)) value <- NA",
    "  if (length(value) == 0L || all(is.na(value))) return(\"not yet available\")",
    "  paste(value, collapse = \", \")",
    "}",
    "```"
  )
}

write_qmd <- function(path, yaml, lines) {
  writeLines(c(yaml, "", lines), path)
  message("Wrote ", path)
}

validate_yaml_indentation <- function(paths) {
  bad <- unlist(lapply(paths, function(path) {
    lines <- readLines(path, warn = FALSE)
    hit <- grep("^ warning:", lines)
    if (length(hit)) paste0(path, ":", hit, ":", lines[hit]) else character()
  }), use.names = FALSE)
  if (length(bad)) stop("Malformed YAML indentation detected:\n", paste(bad, collapse = "\n"), call. = FALSE)
}

body <- strip_code_chunks(strip_yaml(legacy_lines))
main <- extract_lines(body, "# Introduction and Literature Review {#intro}", "# (APPENDIX) Appendix {-}")
appendix <- extract_lines(body, "# (APPENDIX) Appendix {-}", "# Technical Note for Replication")
technical <- extract_lines(body, "# Technical Note for Replication", "# References")

report <- c(main, "", appendix, "", technical)
report <- convert_legacy_math(report)
report <- replace_inline_r_with_target_values(report)
markers <- list(
  list(id = "ws-intro-question-contribution", start = "# Introduction and Literature Review {#intro}", end = "# Data Sources {#data}", attrs = '.sample-excerpt #ws-intro-question-contribution sets="writing-5pg writing-10pg" order="1"'),
  list(id = "ws-selection-model-missingness", start = "# The Composition of Education Participation {#heckman}", end = "# The Effect of EMIE: 2SLS Estimation and Main Results {#iv}", attrs = '.sample-excerpt #ws-selection-model-missingness sets="writing-10pg" order="2"'),
  list(id = "ws-2sls-limits-remedies", start = "# The Effect of EMIE: 2SLS Estimation and Main Results {#iv}", end = "## Instrumental Variable {#iv-iv}", attrs = '.sample-excerpt #ws-2sls-limits-remedies sets="writing-10pg" order="4"'),
  list(id = "ws-iv-relevance-exclusion-problem", start = "## Instrumental Variable {#iv-iv}", end = "## Results {#iv-results}", attrs = '.sample-excerpt #ws-iv-relevance-exclusion-problem sets="writing-5pg writing-10pg" order="3"'),
  list(id = "ws-first-stage-results", start = "## Results {#iv-results}", end = "## Discussion {#disc}", attrs = '.sample-excerpt #ws-first-stage-results sets="writing-10pg" order="5"'),
  list(id = "ws-second-stage-results", start = "## Discussion {#disc}", end = "This result may be driven by our large urbanization estimate", attrs = '.sample-excerpt #ws-second-stage-results sets="writing-5pg writing-10pg" order="6"'),
  list(id = "ws-2sls-interpretation-spillovers-bad-controls", start = "This result may be driven by our large urbanization estimate", end = "# (APPENDIX) Appendix {-}", attrs = '.sample-excerpt #ws-2sls-interpretation-spillovers-bad-controls sets="writing-5pg writing-10pg" order="7"'),
  list(id = "ws-district-harmonization-method", start = "### District Matching Method {#distma}", end = "### Spatial Autocorrelation, Spatial Spillovers, and Migration {#spa}", attrs = '.sample-excerpt #ws-district-harmonization-method sets="writing-5pg writing-10pg" order="8"')
)
report <- insert_sample_markers(report, markers)

report_yaml <- c(
  "---",
  'title: "Escaping Inequality in India: Role of English-Medium Instruction"',
  'author: "Rishav Roy"',
  "format:",
  "  pdf:",
  "    pdf-engine: xelatex",
  "bibliography: references.bib",
  "execute:",
  "  echo: false",
  "  warning: false",
  "  message: false",
  "---"
)
write_qmd("paper/report.qmd", report_yaml, c(report_setup_chunk(), "", report))

appendix_yaml <- c(
  "---",
  'title: "Appendix"',
  'author: "Rishav Roy"',
  "format:",
  "  pdf:",
  "    pdf-engine: xelatex",
  "bibliography: references.bib",
  "execute:",
  "  echo: false",
  "  warning: false",
  "  message: false",
  "---"
)
write_qmd("paper/appendix.qmd", appendix_yaml, convert_legacy_math(c(appendix, "", technical)))

district_yaml <- c(
  "---",
  'title: "District Matching and Spatial Autocorrelation"',
  'author: "Rishav Roy"',
  "format:",
  "  html: default",
  "  pdf:",
  "    pdf-engine: xelatex",
  "bibliography: ../paper/references.bib",
  "execute:",
  "  echo: false",
  "  warning: false",
  "  message: false",
  "---"
)
district <- extract_lines(body, "## District Matching and Spatial Autocorrelation {#distma-spa}", "# Technical Note for Replication")
write_qmd("docs/district-matching.qmd", district_yaml, convert_legacy_math(district))

long_yaml <- c(
  "---",
  'title: "Technical Note for Replication: 8.3 Filenames"',
  'author: "Rishav Roy"',
  "format:",
  "  html: default",
  "  pdf:",
  "    pdf-engine: xelatex",
  "execute:",
  "  echo: false",
  "  warning: false",
  "  message: false",
  "---"
)
tech <- convert_legacy_math(technical)
comment_start <- find_line(legacy_lines, "# ---TROUBLESHOOTING---")
comment_end <- find_line(legacy_lines, "# Goal: Make an alternative read_sav() which accepts 8.3 filenames")
comment_block <- c("```{r}", "#| echo: true", "#| eval: false", legacy_lines[comment_start:(comment_end - 1L)], "```")
write_qmd("docs/long-paths-and-8-3-filenames.qmd", long_yaml, c(tech, "", comment_block))

validate_yaml_indentation(c(
  "paper/report.qmd",
  "paper/appendix.qmd",
  "docs/district-matching.qmd",
  "docs/long-paths-and-8-3-filenames.qmd"
))

message("Static QMD rebuild complete.")
