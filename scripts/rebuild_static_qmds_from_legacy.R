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

find_line <- function(lines, pattern) {
  hit <- grep(pattern, lines, fixed = TRUE)
  if (!length(hit)) stop("Could not find: ", pattern, call. = FALSE)
  hit[[1]]
}

extract_lines <- function(lines, start, end = NULL) {
  i <- find_line(lines, start)
  j <- if (is.null(end)) length(lines) + 1L else find_line(lines, end)
  lines[i:(j - 1L)]
}

wrap_section_lines <- function(lines, start, end, attrs) {
  i <- find_line(lines, start)
  j <- if (is.null(end)) length(lines) + 1L else find_line(lines, end)
  before <- if (i > 1L) lines[seq_len(i - 1L)] else character()
  after <- if (j <= length(lines)) lines[j:length(lines)] else character()
  c(before, paste0("::: {", attrs, "}"), lines[i:(j - 1L)], ":::", "", after)
}

convert_legacy_math <- function(lines) {
  lines <- gsub("\\\\\\[", "$$", lines)
  lines <- gsub("\\\\\\]", "$$", lines)
  lines <- gsub("\\\\\\(", "$", lines)
  lines <- gsub("\\\\\\)", "$", lines)
  lines <- gsub("`r ", "` r ", lines, fixed = TRUE)
  lines
}

write_qmd <- function(path, yaml, lines) {
  writeLines(c(yaml, "", lines), path)
  message("Wrote ", path)
}

body <- strip_code_chunks(strip_yaml(legacy_lines))
main <- extract_lines(body, "# Introduction and Literature Review {#intro}", "# (APPENDIX) Appendix {-}")
appendix <- extract_lines(body, "# (APPENDIX) Appendix {-}", "# Technical Note for Replication")
technical <- extract_lines(body, "# Technical Note for Replication", "# References")

report <- c(main, "", appendix, "", technical)
markers <- list(
  list("### District Matching Method {#distma}", "### Spatial Autocorrelation, Spatial Spillovers, and Migration {#spa}", '.sample-excerpt #ws-district-harmonization-method sets="writing-5pg writing-10pg" order="8"'),
  list("## Discussion {#disc}", "# (APPENDIX) Appendix {-}", '.sample-excerpt #ws-second-stage-results sets="writing-5pg writing-10pg" order="6"'),
  list("## Results {#iv-results}", "## Discussion {#disc}", '.sample-excerpt #ws-first-stage-results sets="writing-10pg" order="5"'),
  list("## Instrumental Variable {#iv-iv}", "## Results {#iv-results}", '.sample-excerpt #ws-iv-relevance-exclusion-problem sets="writing-5pg writing-10pg" order="3"'),
  list("# The Effect of EMIE: 2SLS Estimation and Main Results {#iv}", "## Instrumental Variable {#iv-iv}", '.sample-excerpt #ws-2sls-limits-remedies sets="writing-10pg" order="4"'),
  list("# The Composition of Education Participation {#heckman}", "# The Effect of EMIE: 2SLS Estimation and Main Results {#iv}", '.sample-excerpt #ws-selection-model-missingness sets="writing-10pg" order="2"'),
  list("# Introduction and Literature Review {#intro}", "# Data Sources {#data}", '.sample-excerpt #ws-intro-question-contribution sets="writing-5pg writing-10pg" order="1"')
)
for (marker in markers) report <- wrap_section_lines(report, marker[[1]], marker[[2]], marker[[3]])
report <- convert_legacy_math(report)

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
write_qmd("paper/report.qmd", report_yaml, report)

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

message("Static QMD rebuild complete.")
