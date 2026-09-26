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


test_that("long coding sample contains every short-sample excerpt plus additional material", {
  env <- sample_test_env()
  manifest <- env$read_application_sample_manifest(repo_file("application-samples", "samples.yml"))
  specs <- setNames(manifest$coding, vapply(manifest$coding, `[[`, character(1), "id"))
  short_ids <- vapply(specs$short$excerpts, `[[`, character(1), "id")
  long_ids <- vapply(specs$long$excerpts, `[[`, character(1), "id")

  expect_true(all(short_ids %in% long_ids))
  expect_gt(length(long_ids), length(short_ids))
})


test_that("paper cross-references use Quarto identifiers", {
  paper <- readLines(repo_file("paper", "paper.qmd"), warn = FALSE)
  raw_refs <- grep("(?:Table|Figure|Section|Equation) \\\\ref\\{(?:tbl|fig|sec|eq)-", paper, perl = TRUE, value = TRUE)

  expect_length(raw_refs, 0L)
})


test_that("render cells do not redefine labels already owned by included table TeX", {
  paper <- readLines(repo_file("paper", "paper.qmd"), warn = FALSE)
  cell_lines <- grep(
    "^\\s*#\\|\\s*label:\\s*tbl-[A-Za-z0-9_-]+\\s*$",
    paper,
    value = TRUE,
    perl = TRUE
  )
  cell_labels <- sub("^\\s*#\\|\\s*label:\\s*", "", cell_lines, perl = TRUE)
  cell_labels <- trimws(cell_labels)

  tex_files <- list.files(
    repo_file("outputs", "tables"),
    pattern = "[.]tex$",
    recursive = TRUE,
    full.names = TRUE
  )
  env <- sample_test_env()
  tex_labels <- unique(unlist(lapply(tex_files, env$tex_crossref_ids), use.names = FALSE))

  expect_length(intersect(cell_labels, tex_labels), 0L)
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
  expect_true(any(grepl("Anon_WritingSample_5pg[.]pdf$", outputs)))
  expect_true(any(grepl("RishavRoy_CodeSample_Long[.]pdf$", outputs)))
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

  paper_lines <- readLines(source, warn = FALSE)
  ids <- env$qmd_label_ids(paper_lines)
  ids <- unique(c(ids, "tbl-paper-core-summary", "tbl-paper-language-behavior", "tbl-paper-schooling-market-geography", "tbl-paper-economic-conversion", "tbl-paper-local-development"))
  labels <- setNames(as.character(seq_along(ids)), ids)

  env$assemble_writing_sample_qmd(source, spec, "named", manifest, out_named, labels)
  env$assemble_writing_sample_qmd(source, spec, "anonymous", manifest, out_anonymous, labels)
  named <- paste(readLines(out_named, warn = FALSE), collapse = "\n")
  anonymous <- paste(readLines(out_anonymous, warn = FALSE), collapse = "\n")

  selected <- unlist(spec$sections, use.names = FALSE)
  expect_match(named, "sample-sections", fixed = TRUE)
  expect_match(named, selected[[1L]], fixed = TRUE)
  expect_match(named, "suppress-bibliography: true", fixed = TRUE)
  named_meta <- env$read_qmd_metadata(readLines(out_named, warn = FALSE))
  anonymous_meta <- env$read_qmd_metadata(readLines(out_anonymous, warn = FALSE))
  header_includes <- unlist(named_meta$`header-includes`, use.names = FALSE)
  expect_true(any(grepl("\\providecommand{\\citeproc}[2]{#2}", header_includes, fixed = TRUE)))
  expect_false(any(grepl("externaldocument", header_includes, fixed = TRUE)))
  expect_null(named_meta$`sample-reference-aux`)
  expect_null(named_meta$`sample-full-paper-url`)
  expect_null(anonymous_meta$`sample-full-paper-url`)
  expect_true(length(named_meta$`sample-reference-labels`) > 0L)
  expect_true(isTRUE(named_meta$format$pdf$`keep-tex`))
  expect_identical(named_meta$format$pdf$documentclass, "article")
  expect_match(named, "\\begin{abstract}", fixed = TRUE)
  expect_match(named, "\\end{abstract}", fixed = TRUE)
  expect_false(grepl("## Abstract", named, fixed = TRUE))
  selector <- named_meta$filters[[length(named_meta$filters)]]
  expect_identical(selector$at, "post-quarto")
  expect_identical(selector$path, "../filters/select-sections.lua")
  expect_match(named, "Rishav Roy", fixed = TRUE)
  expect_match(named, manifest$paper$repository_url, fixed = TRUE)
  expect_match(named, env$application_sample_makefile_url(manifest), fixed = TRUE)
  expect_match(named, env$application_sample_build_script_url(manifest), fixed = TRUE)
  expect_false(grepl(env$quoted_paper_title(env$read_qmd_metadata(readLines(source, warn = FALSE))), named, fixed = TRUE))
  expect_false(grepl("bash scripts/run_full_build.sh", named, fixed = TRUE))
  expect_match(anonymous, "author: Anonymous", fixed = TRUE)
  expect_false(grepl(manifest$paper$repository_url, anonymous, fixed = TRUE))
  expect_false(grepl(manifest$paper$full_paper_url, anonymous, fixed = TRUE))
  expect_false(grepl("http://", anonymous, fixed = TRUE))
  expect_false(grepl("https://", anonymous, fixed = TRUE))
  expect_match(anonymous, "make samples", fixed = TRUE)
  expect_match(anonymous, "`scripts/run_full_build.sh`", fixed = TRUE)
  expect_false(grepl("bash scripts/run_full_build.sh", anonymous, fixed = TRUE))
})

test_that("writing sample notices use current paper numbers and abstract styling", {
  env <- sample_test_env()
  source <- c(
    "---",
    "title: Fixture Paper",
    "subtitle: Fixture Subtitle",
    "---",
    "# Introduction {#sec-intro}",
    "",
    "# Parent {#sec-parent}",
    "",
    "## First selected section {#sec-first}",
    "",
    "## Second selected section {#sec-second}",
    "",
    "# Conclusion {#sec-discussion}"
  )
  spec <- list(id = "fixture", target_pages = 5L, sections = c("sec-intro", "sec-first", "sec-second", "sec-discussion"))
  manifest <- list(
    paper = list(
      full_paper_url = "https://example.com/paper.pdf",
      repository_url = "https://github.com/example/repository"
    )
  )
  labels <- c(`sec-first` = "3.1", `sec-second` = "3.2")

  named <- paste(env$writing_sample_notice(spec, "named", manifest, source, labels), collapse = "\n")
  anonymous <- paste(env$writing_sample_notice(spec, "anonymous", manifest, source, labels), collapse = "\n")

  expect_match(named, "This document contains the Introduction, Conclusion, and Sections 3.1 and 3.2 of the titular paper.", fixed = TRUE)
  expect_false(grepl("Fixture Paper", named, fixed = TRUE))
  expect_match(named, "\\renewcommand{\\abstractname}{WRITING SAMPLE: 5-PAGE COPY}", fixed = TRUE)
  expect_match(named, "\\begin{abstract}", fixed = TRUE)
  expect_match(named, env$application_sample_build_script_url(manifest), fixed = TRUE)
  expect_false(grepl("bash scripts/run_full_build.sh", named, fixed = TRUE))
  expect_false(grepl("Fixture Paper", anonymous, fixed = TRUE))
  expect_false(grepl("https://", anonymous, fixed = TRUE))
  expect_match(anonymous, "The full paper and repository are available online, and I would be happy to provide links for them if desired.", fixed = TRUE)
  expect_match(anonymous, "To generate this PDF, run `make samples` or `scripts/run_full_build.sh` from the repo root.", fixed = TRUE)

  full <- paste(
    env$writing_sample_notice(list(id = "full", mode = "full"), "named", manifest, source),
    collapse = "\n"
  )
  expect_false(grepl("This document contains", full, fixed = TRUE))
  expect_match(full, "The full [paper](https://example.com/paper.pdf) and [repository](https://github.com/example/repository) are also available online.", fixed = TRUE)
  expect_match(full, "WRITING SAMPLE: FULL PAPER", fixed = TRUE)

  full_anonymous <- paste(
    env$writing_sample_notice(list(id = "full", mode = "full"), "anonymous", manifest, source),
    collapse = "\n"
  )
  expect_match(full_anonymous, "The full paper and repository are also available online, and I would be happy to provide links for them if desired.", fixed = TRUE)
  expect_false(grepl("https://", full_anonymous, fixed = TRUE))
})


test_that("coding sample assembly uses paper typography and wrapped highlighted code", {
  env <- sample_test_env()
  code_file <- tempfile(fileext = ".R")
  paper_file <- tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: Fixture Paper",
    "subtitle: Fixture Subtitle",
    "format:",
    "  pdf:",
    "    pdf-engine: xelatex",
    "geometry: left=0.85in, right=0.85in, top=0.9in, bottom=0.9in",
    "header-includes:",
    "  - \\usepackage{setspace}\\doublespacing",
    "---",
    "# Intro {#sec-intro}"
  ), paper_file)
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
      source = paper_file,
      full_paper_url = "https://example.com/paper.pdf",
      repository_url = "https://example.com/repository"
    )
  )
  body <- env$extract_code_excerpts(spec, "named", manifest)
  out <- tempfile(fileext = ".qmd")

  env$assemble_coding_sample_qmd(spec, "named", manifest, body, out)
  rendered <- paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_match(rendered, "```r", fixed = TRUE)
  expect_match(rendered, "answer <- 42", fixed = TRUE)
  expect_match(rendered, env$application_sample_file_url(code_file, manifest), fixed = TRUE)
  expect_false(grepl("```{r}", rendered, fixed = TRUE))
  expect_false(grepl("CODING SAMPLE:", rendered, fixed = TRUE))
  expect_match(rendered, "the paper “Fixture Paper: Fixture Subtitle”", fixed = TRUE)
  expect_match(rendered, env$application_sample_makefile_url(manifest), fixed = TRUE)
  expect_match(rendered, env$application_sample_build_script_url(manifest), fixed = TRUE)
  expect_false(grepl("bash scripts/run_full_build.sh", rendered, fixed = TRUE))
  meta <- env$read_qmd_metadata(readLines(out, warn = FALSE))
  expect_identical(meta$subtitle, "Short Version")
  expect_identical(meta$format$pdf$documentclass, "article")
  expect_identical(meta$format$pdf$`syntax-highlighting`, "tango")
  expect_identical(meta$format$pdf$`code-block-bg`, "#f7f7f7")
  headers <- unlist(meta$`header-includes`, use.names = FALSE)
  expect_true(any(grepl("usepackage{fvextra}", headers, fixed = TRUE)))
  expect_true(any(grepl("RecustomVerbatimEnvironment{Highlighting}", headers, fixed = TRUE)))

  anon_out <- tempfile(fileext = ".qmd")
  manifest$identity$anonymous <- list(author = "Anonymous")
  anonymous_body <- env$extract_code_excerpts(spec, "anonymous", manifest)
  env$assemble_coding_sample_qmd(spec, "anonymous", manifest, anonymous_body, anon_out)
  anonymous <- paste(readLines(anon_out, warn = FALSE), collapse = "\n")
  expect_match(anonymous, "a paper titled “Fixture Paper: Fixture Subtitle”", fixed = TRUE)
  expect_false(grepl("https://", anonymous, fixed = TRUE))
  expect_match(anonymous, "`scripts/run_full_build.sh`", fixed = TRUE)
})


test_that("coding samples reuse paper-formatted output files", {
  env <- sample_test_env()
  sys.source(repo_file("R", "application_samples", "coding_sample_outputs.R"), envir = env)
  table_file <- tempfile(fileext = ".tex")
  figure_file <- tempfile(fileext = ".pdf")
  writeLines("\\begin{table}\\caption{Fixture}\\label{tbl-fixture}paper table\\end{table}", table_file)
  writeBin(charToRaw("pdf fixture"), figure_file)
  manifest <- list(coding_outputs = list(
    table = list(type = "latex", file = table_file),
    figure = list(type = "figure", file = figure_file, title = "Paper figure")
  ))
  spec <- list(outputs = c("table", "figure"))

  manifest$paper <- list(repository_url = "https://github.com/example/repository")
  text <- paste(
    env$coding_sample_output_lines(spec, manifest, "named", c(`tbl-fixture` = "7")),
    collapse = "\n"
  )

  expect_match(text, "# Selected Paper Outputs", fixed = TRUE)
  expect_match(text, "\\setcounter{table}{6}", fixed = TRUE)
  expect_equal(length(gregexpr("\\\\FloatBarrier", text, perl = TRUE)[[1L]]), 2L)
  expect_match(text, paste0("\\input{../../", table_file, "}"), fixed = TRUE)
  expect_match(text, paste0("![](../../", figure_file, "){width=95%}"), fixed = TRUE)
  expect_match(text, "## Paper figure", fixed = TRUE)
  expect_match(text, "https://github.com/example/repository/blob/main/R/output/make_tables.R", fixed = TRUE)
  expect_match(text, "https://github.com/example/repository/blob/main/R/output/make_figures.R", fixed = TRUE)
})


test_that("coding-sample tables require an unambiguous full-paper number", {
  env <- sample_test_env()
  sys.source(repo_file("R", "application_samples", "coding_sample_outputs.R"), envir = env)
  table_file <- tempfile(fileext = ".tex")
  writeLines("\\begin{table}\\caption{Fixture}\\label{tbl-fixture}x\\end{table}", table_file)
  item <- list(type = "latex", file = table_file)

  expect_error(env$selected_latex_lines(item, character()), "no integer table number for tbl-fixture")
  expect_error(
    env$selected_latex_lines(item, c(`tbl-fixture` = "7a")),
    "no integer table number for tbl-fixture"
  )
})


test_that("section-selection Lua filter retains selected subsections and ancestor headings", {
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc is unavailable")
  input <- tempfile(fileext = ".md")
  output <- tempfile(fileext = ".md")
  writeLines(c(
    "---",
    "sample-sections:",
    "  - sec-b-one",
    "  - sec-b-three",
    "---",
    "Preamble.",
    "",
    "# A {#sec-a}",
    "",
    "Alpha.",
    "",
    "# B {#sec-b}",
    "",
    "Parent body should be omitted.",
    "",
    "## B one {#sec-b-one}",
    "",
    "Bravo one.",
    "",
    "## B two {#sec-b-two}",
    "",
    "Bravo two should be omitted.",
    "",
    "## B three {#sec-b-three}",
    "",
    "Bravo three.",
    "",
    "# C {#sec-c}",
    "",
    "Charlie."
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
  expect_match(rendered, "# B", fixed = TRUE)
  expect_match(rendered, "Bravo one.", fixed = TRUE)
  expect_match(rendered, "Bravo three.", fixed = TRUE)
  expect_false(grepl("Parent body should be omitted.", rendered, fixed = TRUE))
  expect_false(grepl("Bravo two should be omitted.", rendered, fixed = TRUE))
  expect_false(grepl("Alpha.", rendered, fixed = TRUE))
  expect_false(grepl("Charlie.", rendered, fixed = TRUE))
})

test_that("writing excerpts recognize labels owned by included TeX tables", {
  env <- sample_test_env()
  root <- tempfile("sample-paper-")
  dir.create(root)
  tex <- file.path(root, "table.tex")
  writeLines("\\begin{table}\\caption{Fixture}\\label{tbl-fixture}\\end{table}", tex)
  expect_identical(env$tex_crossref_ids(tex), "tbl-fixture")
  source <- c(
    "---", "title: Fixture", "---",
    "# Keep {#sec-keep}", "",
    "```{r}", "#| results: asis",
    'render_public_table("table.tex", "fixture")',
    "```"
  )
  ids <- env$writing_sample_retained_label_ids(source, "sec-keep", root)
  expect_true("tbl-fixture" %in% ids)

  source <- sub('"table[.]tex"', "'table.tex'", source)
  ids <- env$writing_sample_retained_label_ids(source, "sec-keep", root)
  expect_true("tbl-fixture" %in% ids)
})

test_that("writing numbering preflight requires labels for retained cross-reference content", {
  env <- sample_test_env()
  root <- tempfile("sample-paper-")
  dir.create(root)
  source <- file.path(root, "paper.qmd")
  writeLines(c(
    "---", "title: Fixture", "---",
    "# Keep {#sec-keep}", "",
    "![Figure](figure.pdf){#fig-keep}"
  ), source)

  expect_error(
    env$validate_writing_reference_labels(source, "sec-keep", c(`sec-keep` = "3")),
    "no label for retained writing-sample content: fig-keep"
  )
  expect_no_error(env$validate_writing_reference_labels(
    source,
    "sec-keep",
    c(`sec-keep` = "3", `fig-keep` = "4")
  ))
})

test_that("section-selection filter preserves full-paper numbering for retained content", {
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc is unavailable")
  input <- tempfile(fileext = ".md")
  output <- tempfile(fileext = ".tex")
  writeLines(c(
    "---",
    "sample-sections:",
    "  - sec-language-choice",
    "  - sec-social",
    "sample-reference-labels:",
    "  sec-language: '3'",
    "  sec-language-choice: '3.2'",
    "  sec-access: '4'",
    "  sec-social: '4.3'",
    "  fig-social: '4'",
    "  tbl-language: '2'",
    "---",
    "# Language {#sec-language}",
    "",
    "## Choice {#sec-language-choice}",
    "",
    "```{=latex}",
    "\\begin{table}\\caption{Language}\\label{tbl-language}x\\end{table}",
    "```",
    "",
    "# Access {#sec-access}",
    "",
    "## Social {#sec-social}",
    "",
    "![Social](figure.pdf){#fig-social}"
  ), input)
  status <- system2(
    Sys.which("pandoc"),
    c(
      shQuote(input),
      "--lua-filter", shQuote(repo_file("application-samples", "filters", "select-sections.lua")),
      "-t", "latex",
      "-o", shQuote(output)
    )
  )
  expect_identical(status, 0L)
  rendered <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(rendered, "\\setcounter{section}{2}", fixed = TRUE)
  expect_match(rendered, "\\setcounter{subsection}{1}", fixed = TRUE)
  expect_match(rendered, "\\setcounter{section}{3}", fixed = TRUE)
  expect_match(rendered, "\\setcounter{subsection}{2}", fixed = TRUE)
  expect_match(rendered, "\\setcounter{table}{1}", fixed = TRUE)
  expect_match(rendered, "\\setcounter{figure}{3}", fixed = TRUE)
})

test_that("omitted writing-sample cross-references use the full-paper label index", {
  env <- sample_test_env()
  source <- c(
    "---",
    "title: Fixture",
    "---",
    "Preamble.",
    "",
    "# Keep {#sec-keep}",
    "",
    "See @sec-drop and @fig-keep.",
    "",
    "![Kept figure](figure.pdf){#fig-keep}",
    "",
    "# Drop {#sec-drop}",
    "",
    "Omitted."
  )
  body <- env$split_qmd_front_matter(source)$body
  labels <- c(`sec-drop` = "2", `fig-keep` = "1")

  named <- env$externalize_omitted_writing_crossrefs(
    body, source, "sec-keep", labels, "named", "https://example.com/paper.pdf"
  )
  anonymous <- env$externalize_omitted_writing_crossrefs(
    body, source, "sec-keep", labels, "anonymous", "https://example.com/paper.pdf"
  )

  named_text <- paste(named, collapse = "\n")
  anonymous_text <- paste(anonymous, collapse = "\n")
  expect_match(named_text, "[Section 2](https://example.com/paper.pdf)", fixed = TRUE)
  expect_match(named_text, "@fig-keep", fixed = TRUE)
  expect_false(grepl("@sec-drop", named_text, fixed = TRUE))
  expect_match(anonymous_text, "Section 2", fixed = TRUE)
  expect_false(grepl("example.com", anonymous_text, fixed = TRUE))
  expect_match(anonymous_text, "@fig-keep", fixed = TRUE)
  expect_error(
    env$externalize_omitted_writing_crossrefs(
      body, source, "sec-keep", c(`fig-keep` = "1"), "named", "https://example.com/paper.pdf"
    ),
    "no label for sec-drop"
  )
})


test_that("LaTeX reference labels reject conflicting full-paper numbers", {
  env <- sample_test_env()
  aux <- tempfile(fileext = ".aux")
  writeLines(c(
    "\\newlabel{sec-one}{{1}{2}{One}{section.1}{}}",
    "\\newlabel{sec-one}{{1}{2}{One}{section.1}{}}",
    "\\newlabel{tbl-one}{{3}{4}{Table}{table.3}{}}"
  ), aux)
  labels <- env$read_latex_reference_labels(aux)
  expect_identical(unname(labels[c("sec-one", "tbl-one")]), c("1", "3"))

  writeLines(c(
    "\\newlabel{sec-one}{{1}{2}{One}{section.1}{}}",
    "\\newlabel{sec-one}{{2}{3}{One}{section.2}{}}"
  ), aux)
  expect_error(env$read_latex_reference_labels(aux), "conflicting numbers for: sec-one")
})


test_that("writing excerpts fail closed without the full-paper reference index", {
  env <- sample_test_env()
  source <- tempfile(fileext = ".qmd")
  output <- tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: Fixture",
    "abstract: Abstract.",
    "---",
    "# Keep {#sec-keep}",
    "",
    "See @sec-drop.",
    "",
    "# Drop {#sec-drop}"
  ), source)
  manifest <- list(
    paper = list(full_paper_url = "https://example.com/paper.pdf", repository_url = "https://example.com/repo"),
    identity = list(named = list(author = "Example Author"))
  )
  spec <- list(id = "fixture", target_pages = 1L, sections = "sec-keep")

  expect_error(
    env$assemble_writing_sample_qmd(source, spec, "named", manifest, output),
    "require the current full-paper reference index"
  )
})


test_that("writing page-count checks report all mismatched deliverables without failing", {
  skip_if(!nzchar(Sys.which("pdfinfo")), "pdfinfo is unavailable")
  pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf, width = 4, height = 4)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "sample")
  grDevices::dev.off()
  env <- sample_test_env()

  expect_silent(env$check_writing_sample_page_counts(c(pdf, pdf), c(1L, NA_integer_)))
  expect_message(
    env$check_writing_sample_page_counts(c(pdf, pdf), c(2L, 3L)),
    "(?s)WARNING: Writing-sample page counts differ from their current targets:.+1 pages \\(target 2\\).+1 pages \\(target 3\\)",
    perl = TRUE
  )
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
