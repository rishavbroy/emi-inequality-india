repo_file <- function(...) {
  candidates <- c(
    file.path(getwd(), ...),
    file.path(getwd(), "..", ...),
    file.path(getwd(), "..", "..", ...)
  )
  hits <- candidates[file.exists(candidates)]
  if (!length(hits)) {
    stop("Could not locate repository file: ", file.path(...), call. = FALSE)
  }
  normalizePath(hits[[1]], mustWork = TRUE)
}

test_that("public build helper scripts parse", {
  expect_silent(parse(repo_file("_targets.R")))
  expect_silent(parse(repo_file("scripts", "postprocess_public_qmds.R")))
  expect_silent(parse(repo_file("scripts", "check_required_outputs.R")))
  expect_silent(parse(repo_file("scripts", "check_rendered_text.R")))
  expect_silent(parse(repo_file("R", "application_samples", "extract_qmd_excerpts.R")))
})

test_that("report target renders the PDF artifact explicitly", {
  src <- paste(readLines(repo_file("_targets.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "tar_target\\(report, render_report_pdf\\(report_values, figure_files, table_files\\), format = \"file\"\\)")
  expect_match(src, "quarto\", c\\(\"render\", \"paper/report\\.qmd\", \"--to\", \"pdf\"\\)")
  expect_match(src, "paper/report\\.pdf")
  expect_false(grepl("tar_render\\(report|tar_quarto\\(report", src, perl = TRUE))
})

test_that("postprocessor records legacy map placement and references-heading helpers", {
  src <- paste(readLines(repo_file("scripts", "postprocess_public_qmds.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "Summary statistics for all of the variables in this model")
  expect_match(src, "collage_main_maps")
  expect_match(src, "We are currently unable to replicate her justification of the exclusion restriction")
  expect_match(src, "collage_iv_region_maps")
  expect_match(src, "ensure_references_heading")
})

test_that("writing sample YAML includes LaTeX table packages for raw table excerpts", {
  source(repo_file("R", "application_samples", "extract_qmd_excerpts.R"), local = TRUE)
  lines <- c(
    "---",
    "title: Test",
    "format:",
    "  pdf:",
    "    pdf-engine: xelatex",
    "---"
  )

  out <- normalize_sample_yaml(lines)

  expect_true(any(out == "  - \\usepackage{threeparttable}"))
  expect_true(any(out == "  - \\usepackage{booktabs}"))
  expect_true(any(out == "  - \\usepackage{xcolor}"))
})


test_that("report raw TeX table chunks rely on TeX captions and load their dependencies", {
  src <- paste(readLines(repo_file("scripts", "postprocess_public_qmds.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "\\usepackage{xcolor}", fixed = TRUE)
  expect_match(src, "\\usepackage{colortbl}", fixed = TRUE)
  expect_match(src, "\\usepackage{pdflscape}", fixed = TRUE)
  expect_match(src, "is_raw_tex", fixed = TRUE)
  expect_match(src, "knitr::asis_output(tex)", fixed = TRUE)
})

test_that("postprocessor places probit AME table after the explanatory paragraph", {
  src <- paste(readLines(repo_file("scripts", "postprocess_public_qmds.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "@tbl-probit-mfx has been calculated over all observations", fixed = TRUE)
  expect_false(grepl(
    '"Average marginal effects for numeric variables and counterfactual comparisons",\\n    output_table_chunk\\("tbl-probit-mfx"',
    src,
    perl = TRUE
  ))
})


test_that("raw TeX table inclusion is emitted as an as-is block with block boundaries", {
  path <- file.path("scripts", "postprocess_public_qmds.R")
  if (!file.exists(path)) path <- file.path("..", "..", "scripts", "postprocess_public_qmds.R")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(src, "knitr::asis_output", fixed = TRUE)
  expect_match(src, "paste0(\"\\\\n\\\\n\", tex, \"\\\\n\\\\n\")", fixed = TRUE)
})
