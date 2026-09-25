sample_test_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("R", "io", "utils_data_frame.R"), envir = env)
  sys.source(repo_file("R", "application_samples", "sample_manifest.R"), envir = env)
  sys.source(repo_file("R", "application_samples", "writing_sample_sections.R"), envir = env)
  sys.source(repo_file("R", "application_samples", "extract_code_excerpts.R"), envir = env)
  sys.source(repo_file("R", "application_samples", "render_helpers.R"), envir = env)
  env
}

test_that("application-sample manifest references current paper sections and code markers", {
  env <- sample_test_env()
  manifest <- env$read_application_sample_manifest(repo_file("application-samples", "samples.yml"))
  paper <- repo_file(manifest$paper$source)
  index <- env$qmd_section_index(readLines(paper, warn = FALSE))

  for (spec in manifest$writing) {
    if (identical(spec$mode %||% "excerpt", "full")) next
    expect_true(all(unlist(spec$sections, use.names = FALSE) %in% index$id), info = spec$id)
  }
  for (spec in manifest$coding) {
    for (excerpt in spec$excerpts) {
      lines <- env$extract_between_sample_markers(repo_file(excerpt$file), excerpt$id)
      expect_true(sum(nzchar(trimws(lines))) > 1L, info = excerpt$id)
    }
  }
})


test_that("application-sample directory ignores only transient work files", {
  ignore <- readLines(repo_file(".gitignore"), warn = FALSE)
  application_rules <- trimws(ignore[grepl("^application-samples/", trimws(ignore))])

  expect_identical(application_rules, "application-samples/.work/")
})

test_that("application-sample outputs are unique across content and identity variants", {
  env <- sample_test_env()
  manifest <- env$read_application_sample_manifest(repo_file("application-samples", "samples.yml"))
  outputs <- env$application_sample_expected_outputs(manifest)

  expect_identical(anyDuplicated(outputs), 0L)
  expect_length(outputs, length(manifest$identity) * (length(manifest$writing) + length(manifest$coding)))
  expect_true(any(grepl("Anonymous_WritingSample_5pg[.]pdf$", outputs)))
  expect_true(any(grepl("RishavRoy_CodingSample[.]pdf$", outputs)))
})

test_that("Quarto metadata serialization uses YAML 1.2 booleans", {
  env <- sample_test_env()
  lines <- env$quarto_yaml_lines(list(
    execute = list(echo = FALSE, warning = FALSE),
    `number-sections` = TRUE,
    `link-citations` = TRUE
  ))
  text <- paste(lines, collapse = "\n")

  expect_match(text, "echo: false", fixed = TRUE)
  expect_match(text, "warning: false", fixed = TRUE)
  expect_match(text, "number-sections: true", fixed = TRUE)
  expect_match(text, "link-citations: true", fixed = TRUE)
  expect_false(grepl("\\b(?:yes|no)\\b", text, perl = TRUE))
})

test_that("writing sample assembly derives identity and section selection from one manifest", {
  env <- sample_test_env()
  manifest <- env$read_application_sample_manifest(repo_file("application-samples", "samples.yml"))
  spec <- manifest$writing[[1]]
  source <- repo_file(manifest$paper$source)
  out_named <- tempfile(fileext = ".qmd")
  out_anonymous <- tempfile(fileext = ".qmd")

  env$assemble_writing_sample_qmd(source, spec, "named", manifest, out_named)
  env$assemble_writing_sample_qmd(source, spec, "anonymous", manifest, out_anonymous)
  named <- paste(readLines(out_named, warn = FALSE), collapse = "\n")
  anonymous <- paste(readLines(out_anonymous, warn = FALSE), collapse = "\n")

  selected <- unlist(spec$sections, use.names = FALSE)
  expect_match(named, "sample-sections", fixed = TRUE)
  expect_match(named, selected[[1L]], fixed = TRUE)
  expect_match(named, "suppress-bibliography: true", fixed = TRUE)
  expect_match(named, "Rishav Roy", fixed = TRUE)
  expect_match(named, manifest$paper$repository_url, fixed = TRUE)
  expect_match(anonymous, "author: Anonymous", fixed = TRUE)
  expect_false(grepl(manifest$paper$repository_url, anonymous, fixed = TRUE))
})

test_that("coding sample assembly uses static code blocks", {
  env <- sample_test_env()
  code_file <- tempfile(fileext = ".R")
  writeLines(c(
    "# sample-start: fixture",
    "answer <- 42",
    "# sample-end: fixture"
  ), code_file)
  spec <- list(
    id = "short",
    excerpts = list(list(file = code_file, id = "fixture", title = "Fixture"))
  )
  manifest <- list(
    identity = list(named = list(author = "Example Author")),
    paper = list(
      full_paper_url = "https://example.com/paper.pdf",
      repository_url = "https://example.com/repository"
    )
  )
  body <- env$extract_code_excerpts(spec)
  out <- tempfile(fileext = ".qmd")

  env$assemble_coding_sample_qmd(spec, "named", manifest, body, out)
  rendered <- paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_match(rendered, "```r", fixed = TRUE)
  expect_match(rendered, "answer <- 42", fixed = TRUE)
  expect_false(grepl("```{r}", rendered, fixed = TRUE))
  expect_false(grepl("DefineVerbatimEnvironment", rendered, fixed = TRUE))
})

test_that("section-selection Lua filter retains preamble and requested sections", {
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc is unavailable")
  input <- tempfile(fileext = ".md")
  output <- tempfile(fileext = ".md")
  writeLines(c(
    "---",
    "sample-sections:",
    "  - sec-b",
    "---",
    "Preamble.",
    "",
    "## Abstract {-}",
    "",
    "Abstract text.",
    "",
    "# A {#sec-a}",
    "",
    "Alpha.",
    "",
    "# B {#sec-b}",
    "",
    "Bravo.",
    "",
    "## B child {#sec-b-child}",
    "",
    "Charlie.",
    "",
    "# C {#sec-c}",
    "",
    "Delta."
  ), input)
  status <- system2(
    Sys.which("pandoc"),
    c(
      shQuote(input),
      "--lua-filter", shQuote(repo_file("application-samples", "filters", "select-sections.lua")),
      "-t", "markdown",
      "-o", shQuote(output)
    )
  )
  expect_identical(status, 0L)
  rendered <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(rendered, "Preamble.", fixed = TRUE)
  expect_match(rendered, "Abstract text.", fixed = TRUE)
  expect_match(rendered, "Bravo.", fixed = TRUE)
  expect_match(rendered, "Charlie.", fixed = TRUE)
  expect_false(grepl("Alpha.", rendered, fixed = TRUE))
  expect_false(grepl("Delta.", rendered, fixed = TRUE))
})

test_that("writing page-count validation enforces declared deliverable length", {
  skip_if(!nzchar(Sys.which("pdfinfo")), "pdfinfo is unavailable")
  pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf, width = 4, height = 4)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "sample")
  grDevices::dev.off()
  env <- sample_test_env()

  expect_silent(env$validate_writing_sample_page_count(pdf, 1L))
  expect_error(env$validate_writing_sample_page_count(pdf, 2L), "rendered to 1 pages")
})

test_that("final-output requirements are derived from the sample manifest", {
  env <- sample_test_env()
  manifest <- env$read_application_sample_manifest(repo_file("application-samples", "samples.yml"))
  expected <- env$application_sample_expected_outputs(manifest)
  contract_env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "public_output_contract.R"), envir = contract_env)
  old_wd <- setwd(dirname(repo_file("_targets.R")))
  on.exit(setwd(old_wd), add = TRUE)

  expect_setequal(contract_env$application_sample_outputs(), expected)
})
