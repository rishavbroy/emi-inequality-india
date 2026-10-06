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

  paper_labels <- env$qmd_label_ids(readLines(paper, warn = FALSE))
  figure_labels <- vapply(
    manifest$coding_outputs[vapply(
      manifest$coding_outputs, function(x) identical(x$type %||% "", "figure"), logical(1)
    )],
    function(x) x$paper_label %||% "",
    character(1)
  )
  expect_true(all(figure_labels %in% paper_labels))
})



test_that("LaTeX coding outputs declare the paper cross-reference they preserve", {
  env <- sample_test_env()
  manifest <- yaml::read_yaml(repo_file("application-samples", "samples.yml"))
  latex_ids <- names(manifest$coding_outputs)[vapply(
    manifest$coding_outputs, function(x) identical(x$type %||% "", "latex"), logical(1)
  )]
  expect_true(length(latex_ids) > 0L)

  broken <- manifest
  broken$coding_outputs[[latex_ids[[1L]]]]$paper_label <- NULL
  expect_error(env$validate_application_sample_manifest(broken))
})

test_that("figure coding outputs declare the paper cross-reference they preserve", {
  env <- sample_test_env()
  manifest <- yaml::read_yaml(repo_file("application-samples", "samples.yml"))
  figure_ids <- names(manifest$coding_outputs)[vapply(
    manifest$coding_outputs, function(x) identical(x$type %||% "", "figure"), logical(1)
  )]
  expect_true(length(figure_ids) > 0L)

  broken <- manifest
  broken$coding_outputs[[figure_ids[[1L]]]]$paper_label <- NULL
  expect_error(env$validate_application_sample_manifest(broken))
})




test_that("writing manifest supports file-size-limited excerpts", {
  env <- sample_test_env()
  manifest <- yaml::read_yaml(repo_file("application-samples", "samples.yml"))
  fixture <- manifest
  fixture$writing <- list(list(
    id = "size-limit",
    label = "SIZE-LIMIT COPY",
    max_bytes = 1000,
    selective_rasterization = TRUE,
    sections = "sec-intro"
  ))

  expect_no_error(env$validate_application_sample_manifest(fixture))

  full_fixture <- manifest
  full_fixture$writing <- list(list(
    id = "full-size-limit",
    mode = "full",
    label = "SIZE-LIMIT COPY",
    max_bytes = 1000,
    selective_rasterization = TRUE
  ))
  expect_no_error(env$validate_application_sample_manifest(full_fixture))

  broken <- fixture
  broken$writing[[1L]]$label <- NULL
  expect_error(env$validate_application_sample_manifest(broken))

  broken <- fixture
  broken$writing[[1L]]$max_bytes <- 0
  expect_error(env$validate_application_sample_manifest(broken))
})

test_that("coding-output file validation uses the repository root", {
  env <- sample_test_env()
  manifest <- yaml::read_yaml(repo_file("application-samples", "samples.yml"))
  root <- normalizePath(file.path(dirname(repo_file("application-samples", "samples.yml")), ".."), mustWork = TRUE)
  old_root <- Sys.getenv("EMI_PROJECT_ROOT", unset = NA_character_)
  old_wd <- setwd(tempdir())
  on.exit({
    setwd(old_wd)
    if (is.na(old_root)) Sys.unsetenv("EMI_PROJECT_ROOT") else Sys.setenv(EMI_PROJECT_ROOT = old_root)
  }, add = TRUE)
  Sys.setenv(EMI_PROJECT_ROOT = root)

  expect_silent(env$validate_application_sample_manifest(manifest))
})

test_that("coding-output references are validated at the excerpt that displays them", {
  env <- sample_test_env()
  manifest <- yaml::read_yaml(repo_file("application-samples", "samples.yml"))
  broken <- manifest
  broken$coding[[1L]]$excerpts[[1L]]$outputs <- "missing-output"

  expect_error(env$validate_application_sample_manifest(broken))
})


test_that("coding-output rendering files must exist", {
  env <- sample_test_env()
  manifest <- yaml::read_yaml(repo_file("application-samples", "samples.yml"))
  broken <- manifest
  broken$coding_outputs[[1L]]$rendering_files <- "R/does-not-exist.R"

  expect_error(env$validate_application_sample_manifest(broken))
})

test_that("coding outputs declare generated files", {
  env <- sample_test_env()
  manifest <- yaml::read_yaml(repo_file("application-samples", "samples.yml"))

  broken <- manifest
  broken$coding_outputs[[1L]]$files <- NULL
  expect_error(env$validate_application_sample_manifest(broken))

  latex_id <- names(manifest$coding_outputs)[vapply(
    manifest$coding_outputs, function(x) identical(x$type %||% "", "latex"), logical(1)
  )][[1L]]
  broken <- manifest
  broken$coding_outputs[[latex_id]]$files <- c("first.tex", "second.tex")
  expect_error(env$validate_application_sample_manifest(broken))

  broken <- manifest
  broken$coding_outputs[[1L]]$type <- "unknown"
  expect_error(env$validate_application_sample_manifest(broken))
})


test_that("size-limited code samples support the same byte and figure options", {
  env <- sample_test_env()
  manifest <- yaml::read_yaml(repo_file("application-samples", "samples.yml"))
  fixture <- manifest
  fixture$coding <- list(list(
    id = "limited",
    label = "LIMITED COPY",
    max_bytes = 1000,
    selective_rasterization = TRUE,
    excerpts = list()
  ))

  expect_no_error(env$validate_application_sample_manifest(fixture))

  broken <- fixture
  broken$coding[[1L]]$max_bytes <- 0
  expect_error(env$validate_application_sample_manifest(broken))

  broken <- fixture
  broken$coding[[1L]]$selective_rasterization <- "yes"
  expect_error(env$validate_application_sample_manifest(broken))
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
  expect_true(any(grepl("RishavRoy_CodeSample[.]pdf$", outputs)))
  expect_true(any(grepl("RishavRoy_CodeSample_Under10MB[.]pdf$", outputs)))
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


test_that("writing sample labels may be configured independently of page targets", {
  env <- sample_test_env()

  expect_identical(env$writing_sample_label(list(label = "SIZE-LIMIT COPY")), "SIZE-LIMIT COPY")
  expect_identical(env$writing_sample_label(list(target_pages = 7L)), "7-PAGE COPY")
  expect_identical(env$writing_sample_label(list(id = "full", mode = "full")), "FULL PAPER")
  expect_identical(
    env$writing_sample_label(list(id = "limited", mode = "full", label = "LIMITED COPY")),
    "LIMITED COPY"
  )
})

test_that("size-limited samples use smaller raster figure assets", {
  env <- sample_test_env()
  root <- tempfile("writing-raster-")
  paper_dir <- file.path(root, "paper")
  figure_dir <- file.path(root, "outputs", "figures", "main")
  dir.create(paper_dir, recursive = TRUE)
  dir.create(figure_dir, recursive = TRUE)
  writeBin(as.raw(rep(1L, 20L)), file.path(figure_dir, "map.pdf"))
  writeBin(as.raw(rep(1L, 5L)), file.path(figure_dir, "map.png"))
  writeBin(as.raw(rep(1L, 5L)), file.path(figure_dir, "chart.pdf"))
  writeBin(as.raw(rep(1L, 20L)), file.path(figure_dir, "chart.png"))

  map <- "![Map](../outputs/figures/main/map.pdf)"
  chart <- "![Chart](../outputs/figures/main/chart.pdf)"
  expect_identical(
    env$use_smaller_figure_assets(map, paper_dir, TRUE),
    "![Map](../outputs/figures/main/map.png)"
  )
  expect_identical(env$use_smaller_figure_assets(map, paper_dir, FALSE), map)
  expect_identical(env$use_smaller_figure_assets(chart, paper_dir, TRUE), chart)
  expect_identical(
    env$use_smaller_figure_assets(
      "![Other](../outputs/figures/main/other.pdf)", paper_dir, TRUE
    ),
    "![Other](../outputs/figures/main/other.pdf)"
  )
})

test_that("application-sample byte limits are enforced after rendering", {
  env <- sample_test_env()
  path <- tempfile(fileext = ".pdf")
  writeBin(as.raw(1:4), path)

  expect_no_error(env$validate_application_sample_max_bytes(path, 4L))
  expect_error(env$validate_application_sample_max_bytes(path, 3L))
  expect_no_error(env$validate_application_sample_max_bytes(path, NULL))
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
  named_lines <- readLines(out_named, warn = FALSE)
  anonymous_lines <- readLines(out_anonymous, warn = FALSE)
  named_meta <- env$read_qmd_metadata(named_lines)
  anonymous_meta <- env$read_qmd_metadata(anonymous_lines)

  expect_identical(named_meta$author, manifest$identity$named$author)
  expect_identical(anonymous_meta$author, manifest$identity$anonymous$author)
  expect_identical(named_meta$`sample-sections`, unname(unlist(spec$sections, use.names = FALSE)))
  expect_identical(anonymous_meta$`sample-sections`, unname(unlist(spec$sections, use.names = FALSE)))
  expect_identical(named_meta$`sample-figures`, unname(unlist(spec$figures, use.names = FALSE)))
  expect_identical(anonymous_meta$`sample-figures`, unname(unlist(spec$figures, use.names = FALSE)))
  expect_true(isTRUE(named_meta$`suppress-bibliography`))
  expect_true(isTRUE(anonymous_meta$`suppress-bibliography`))
  expect_true(nzchar(named_meta$thanks %||% ""))
  expect_true(nzchar(anonymous_meta$thanks %||% ""))
  expect_false(grepl(manifest$paper$repository_url, named_meta$thanks, fixed = TRUE))
  acknowledgment_names <- manifest$identity$anonymous$acknowledgment_names %||% character()
  expect_false(any(vapply(
    acknowledgment_names, grepl, logical(1), x = anonymous_meta$thanks, fixed = TRUE
  )))
  expect_true(grepl(
    manifest$identity$anonymous$acknowledgment_replacement,
    anonymous_meta$thanks,
    fixed = TRUE
  ))

  header_includes <- unlist(named_meta$`header-includes`, use.names = FALSE)
  expect_true(any(grepl("\\providecommand{\\citeproc}[2]{#2}", header_includes, fixed = TRUE)))
  expect_false(any(grepl("externaldocument", header_includes, fixed = TRUE)))
  expect_null(named_meta$`sample-reference-aux`)
  expect_null(named_meta$`sample-full-paper-url`)
  expect_null(anonymous_meta$`sample-full-paper-url`)
  expect_true(length(named_meta$`sample-reference-labels`) > 0L)
  expect_true(any(grepl(
    "emi.application_sample_reference_labels", named_lines, fixed = TRUE
  )))
  expect_true(isTRUE(named_meta$format$pdf$`keep-tex`))
  expect_identical(named_meta$format$pdf$documentclass, "article")

  selector <- named_meta$filters[[length(named_meta$filters)]]
  expect_identical(selector$at, "pre-ast")
  expect_identical(selector$path, "../filters/select-sections.lua")

  named <- paste(named_lines, collapse = "\n")
  anonymous <- paste(anonymous_lines, collapse = "\n")
  expect_match(named, env$application_sample_document_url("writing", spec$id, "named", manifest), fixed = TRUE)
  expect_match(named, env$application_sample_makefile_url(manifest), fixed = TRUE)
  expect_match(named, env$application_sample_build_script_url(manifest), fixed = TRUE)
  forbidden <- env$anonymous_sample_forbidden_strings(manifest)
  expect_false(any(vapply(forbidden, grepl, logical(1), x = anonymous, fixed = TRUE)))
})


test_that("writing sample notices use current section numbers and identity-specific links", {
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
    identity = list(
      named = list(prefix = "Example"),
      anonymous = list(prefix = "Anon")
    ),
    paper = list(
      full_paper_url = "https://example.com/paper.pdf",
      repository_url = "https://github.com/example/repository"
    )
  )
  labels <- c(`sec-first` = "3.1", `sec-second` = "3.2")
  contents <- env$writing_sample_contents(spec, source, labels)
  expect_lt(regexpr("Introduction", contents, fixed = TRUE), regexpr("3.1", contents, fixed = TRUE))
  expect_lt(regexpr("3.2", contents, fixed = TRUE), regexpr("Conclusion", contents, fixed = TRUE))

  named <- paste(env$writing_sample_notice(spec, "named", manifest, source, labels), collapse = "\n")
  anonymous <- paste(env$writing_sample_notice(spec, "anonymous", manifest, source, labels), collapse = "\n")

  expect_match(named, "3.1", fixed = TRUE)
  expect_match(named, "3.2", fixed = TRUE)
  expect_match(named, env$application_sample_document_url("writing", "fixture", "named", manifest), fixed = TRUE)
  expect_match(named, env$application_sample_build_script_url(manifest), fixed = TRUE)
  expect_match(anonymous, "3.1", fixed = TRUE)
  expect_match(anonymous, "3.2", fixed = TRUE)
  expect_false(grepl(env$application_sample_document_url("writing", "fixture", "named", manifest), anonymous, fixed = TRUE))

  full_named <- paste(
    env$writing_sample_notice(list(id = "full", mode = "full"), "named", manifest, source),
    collapse = "\n"
  )
  full_anonymous <- paste(
    env$writing_sample_notice(list(id = "full", mode = "full"), "anonymous", manifest, source),
    collapse = "\n"
  )
  expect_match(full_named, env$application_sample_document_url("writing", "full", "named", manifest), fixed = TRUE)
  expect_false(grepl(env$application_sample_document_url("writing", "full", "named", manifest), full_anonymous, fixed = TRUE))
})


test_that("writing-sample acknowledgments apply identity redactions", {
  env <- sample_test_env()
  repository_url <- "https://example.com/repository"
  identity <- list(
    acknowledgment_names = c("Professor One", "Professor Two", "Professor Three"),
    acknowledgment_replacement = "[my professors]"
  )
  thanks <- paste(
    "Acknowledgments: Thanks to Professor One, Professor Two, and Professor Three.",
    paste0("Replication materials are [available here](", repository_url, ").")
  )

  anonymous <- env$writing_sample_acknowledgments(thanks, repository_url, identity)
  expect_true(grepl(identity$acknowledgment_replacement, anonymous, fixed = TRUE))
  expect_false(any(vapply(
    identity$acknowledgment_names, grepl, logical(1), x = anonymous, fixed = TRUE
  )))
  expect_false(grepl(repository_url, anonymous, fixed = TRUE))
})

test_that("anonymous sample validation includes configured acknowledgment names", {
  env <- sample_test_env()
  manifest <- list(identity = list(anonymous = list(
    forbidden_strings = c("Applicant Name", "applicant-handle"),
    acknowledgment_names = c("Professor One", "Professor Two")
  )))

  expect_setequal(
    env$anonymous_sample_forbidden_strings(manifest),
    c("Applicant Name", "applicant-handle", "Professor One", "Professor Two")
  )
})

test_that("sample availability links target the published named PDFs", {
  env <- sample_test_env()
  manifest <- list(
    identity = list(named = list(prefix = "Example")),
    paper = list(
      full_paper_url = "https://example.com/paper.pdf",
      repository_url = "https://github.com/example/repository"
    )
  )

  expect_identical(
    env$application_sample_document_url("coding", "main", "named", manifest),
    "https://example.com/application-samples/Example_CodeSample.pdf"
  )
  expect_identical(
    env$application_sample_document_url("writing", "full", "named", manifest),
    "https://example.com/application-samples/Example_WritingSample_Full.pdf"
  )
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
  python_file <- tempfile(fileext = ".py")
  writeLines(c(
    "# sample-start: python-fixture",
    "answer = 43",
    "# sample-end: python-fixture"
  ), python_file)
  spec <- list(
    id = "main",
    excerpts = list(
      list(file = code_file, id = "fixture", title = "Fixture", description = "Fixture context."),
      list(file = python_file, id = "python-fixture", title = "Python fixture")
    )
  )
  manifest <- list(
    identity = list(named = list(author = "Example Author", prefix = "Example")),
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
  expect_match(rendered, "Fixture context.", fixed = TRUE)
  expect_match(rendered, "answer <- 42", fixed = TRUE)
  expect_match(rendered, "```python", fixed = TRUE)
  expect_match(rendered, "answer = 43", fixed = TRUE)
  expect_match(rendered, env$application_sample_file_url(code_file, manifest), fixed = TRUE)
  expect_false(grepl("```{r}", rendered, fixed = TRUE))
  expect_false(grepl("CODING SAMPLE:", rendered, fixed = TRUE))
  expect_match(rendered, env$application_sample_document_url("coding", "main", "named", manifest), fixed = TRUE)
  expect_match(rendered, env$application_sample_makefile_url(manifest), fixed = TRUE)
  expect_match(rendered, env$application_sample_build_script_url(manifest), fixed = TRUE)
  meta <- env$read_qmd_metadata(readLines(out, warn = FALSE))
  expect_null(meta$subtitle)
  expect_identical(meta$format$pdf$documentclass, "article")
  expect_identical(meta$format$pdf$`syntax-highlighting`, "tango")
  expect_identical(meta$format$pdf$`code-block-bg`, "#f7f7f7")
  expect_identical(meta$bibliography, "../../paper/references.bib")
  expect_true(isTRUE(meta$`suppress-bibliography`))
  expect_false(isTRUE(meta$`link-citations`))
  expect_identical(meta$`sample-full-paper-url`, manifest$paper$full_paper_url)
  citation_filter <- meta$filters[[length(meta$filters)]]
  expect_identical(citation_filter$at, "post-quarto")
  expect_identical(citation_filter$path, "../filters/link-citations-to-paper.lua")
  headers <- unlist(meta$`header-includes`, use.names = FALSE)
  expect_true(any(grepl("usepackage{fvextra}", headers, fixed = TRUE)))
  expect_true(any(grepl("RecustomVerbatimEnvironment{Highlighting}", headers, fixed = TRUE)))
  expect_true(any(grepl("\\href{https://example.com/paper.pdf}{#2}", headers, fixed = TRUE)))

  anon_out <- tempfile(fileext = ".qmd")
  manifest$identity$anonymous <- list(author = "Anonymous", prefix = "Anon")
  anonymous_body <- env$extract_code_excerpts(spec, "anonymous", manifest)
  env$assemble_coding_sample_qmd(spec, "anonymous", manifest, anonymous_body, anon_out)
  anonymous <- paste(readLines(anon_out, warn = FALSE), collapse = "\n")
  anonymous_meta <- env$read_qmd_metadata(readLines(anon_out, warn = FALSE))
  expect_null(anonymous_meta$`sample-full-paper-url`)
  anonymous_headers <- unlist(anonymous_meta$`header-includes`, use.names = FALSE)
  expect_true(any(grepl("\\providecommand{\\citeproc}[2]{#2}", anonymous_headers, fixed = TRUE)))
  expect_true(is.null(anonymous_meta$filters) || !any(vapply(
    anonymous_meta$filters,
    function(x) identical(x$path %||% "", "../filters/link-citations-to-paper.lua"),
    logical(1)
  )))
})


test_that("code samples reuse paper-formatted tables and exact paper figures", {
  env <- sample_test_env()
  sys.source(repo_file("R", "application_samples", "coding_sample_outputs.R"), envir = env)
  table_file <- tempfile(fileext = ".tex")
  figure_file <- tempfile(fileext = ".pdf")
  paper_file <- tempfile(fileext = ".qmd")
  writeLines("\\begin{table}\\caption{Fixture}\\label{tbl-file-fixture}paper table\\end{table}", table_file)
  writeBin(charToRaw("figure fixture"), figure_file)
  writeLines(c(
    "---", "title: Fixture", "---", "",
    "![Paper figure caption with source [@shastry2012a].](../outputs/figures/fixture.pdf){#fig-paper-fixture width=90%}"
  ), paper_file)
  manifest <- list(
    paper = list(source = paper_file, repository_url = "https://example.com/repository"),
    coding_outputs = list(
      table = list(type = "latex", files = table_file, paper_label = "tbl-paper-fixture"),
      figure = list(
        type = "figure", files = figure_file, paper_label = "fig-paper-fixture"
      )
    )
  )
  text <- paste(
    env$coding_sample_output_lines(
      c("table", "figure"), "named", manifest,
      c(`tbl-paper-fixture` = "7", `tbl-file-fixture` = "7a", `fig-paper-fixture` = "4")
    ),
    collapse = "\n"
  )

  expect_match(text, "\\setcounter{table}{6}", fixed = TRUE)
  expect_match(text, paste0("\\input{../../", table_file, "}"), fixed = TRUE)
  expect_match(text, "\\setcounter{figure}{3}", fixed = TRUE)
  expect_match(text, "Paper figure caption with source [@shastry2012a].", fixed = TRUE)
  expect_match(text, "{#fig-paper-fixture width=90%}", fixed = TRUE)
  expect_match(text, "../../outputs/figures/fixture.pdf", fixed = TRUE)
  expect_match(text, env$application_sample_file_url(table_file, manifest), fixed = TRUE)
  expect_match(text, env$application_sample_file_url(figure_file, manifest), fixed = TRUE)
  expect_false(grepl("\\clearpage", text, fixed = TRUE))
})

test_that("size-limited code samples rasterize only smaller figure counterparts", {
  env <- sample_test_env()
  sys.source(repo_file("R", "application_samples", "coding_sample_outputs.R"), envir = env)
  root <- tempfile("coding-raster-")
  paper_dir <- file.path(root, "paper")
  figure_dir <- file.path(root, "outputs", "figures", "main")
  dir.create(paper_dir, recursive = TRUE)
  dir.create(figure_dir, recursive = TRUE)
  writeBin(as.raw(rep(1L, 20L)), file.path(figure_dir, "map.pdf"))
  writeBin(as.raw(rep(1L, 5L)), file.path(figure_dir, "map.png"))
  paper_file <- file.path(paper_dir, "paper.qmd")
  writeLines(c(
    "---", "title: Fixture", "---", "",
    "![Fixture](../outputs/figures/main/map.pdf){#fig-fixture}"
  ), paper_file)
  manifest <- list(
    paper = list(source = paper_file, repository_url = "https://example.com/repository"),
    coding_outputs = list(figure = list(
      type = "figure", files = file.path(figure_dir, "map.pdf"), paper_label = "fig-fixture"
    ))
  )

  vector <- paste(env$coding_sample_output_lines(
    "figure", "named", manifest, c(`fig-fixture` = "2"), FALSE
  ), collapse = "\n")
  compact <- paste(env$coding_sample_output_lines(
    "figure", "named", manifest, c(`fig-fixture` = "2"), TRUE
  ), collapse = "\n")

  expect_match(vector, "../../outputs/figures/main/map.pdf", fixed = TRUE)
  expect_match(compact, "../../outputs/figures/main/map.png", fixed = TRUE)
  expect_match(vector, paste0("`", file.path(figure_dir, "map.pdf"), "`"), fixed = TRUE)
  expect_match(compact, paste0("`", file.path(figure_dir, "map.png"), "`"), fixed = TRUE)
})


test_that("code-sample result annotations link outputs and rendering code", {
  env <- sample_test_env()
  sys.source(repo_file("R", "application_samples", "coding_sample_outputs.R"), envir = env)
  output_files <- c("outputs/first.pdf", "outputs/second.pdf", "outputs/third.pdf")
  rendering_files <- c("R/output/first.R", "R/output/second.R")
  manifest <- list(paper = list(repository_url = "https://example.com/repository"))
  item <- list(type = "figure", files = output_files, rendering_files = rendering_files)

  named_lines <- env$coding_output_annotation_lines(item, "named", manifest)
  anonymous_lines <- env$coding_output_annotation_lines(item, "anonymous", manifest)
  named <- paste(named_lines, collapse = "\n")
  anonymous <- paste(anonymous_lines, collapse = "\n")

  output_line <- which(startsWith(named_lines, "Outputs: "))[[1L]]
  rendering_line <- which(startsWith(named_lines, "Rendering code: "))[[1L]]
  expect_identical(rendering_line, output_line + 2L)
  expect_identical(named_lines[[output_line + 1L]], "")
  expect_match(named, "Outputs: ", fixed = TRUE)
  expect_match(named, "Rendering code: ", fixed = TRUE)
  expect_true(all(vapply(
    c(output_files, rendering_files),
    function(path) grepl(env$application_sample_file_url(path, manifest), named, fixed = TRUE),
    logical(1)
  )))
  expect_match(anonymous, "`outputs/first.pdf`, `outputs/second.pdf`, and `outputs/third.pdf`", fixed = TRUE)
  expect_match(anonymous, "`R/output/first.R` and `R/output/second.R`", fixed = TRUE)
})


test_that("coding-sample results follow the excerpt that produces them", {
  env <- sample_test_env()
  sys.source(repo_file("R", "application_samples", "coding_sample_outputs.R"), envir = env)
  sys.source(repo_file("R", "application_samples", "render_coding_sample.R"), envir = env)
  code_file <- tempfile(fileext = ".R")
  table_file <- tempfile(fileext = ".tex")
  paper_file <- tempfile(fileext = ".qmd")
  writeLines(c(
    "# sample-start: fixture",
    "answer <- 42",
    "# sample-end: fixture"
  ), code_file)
  writeLines("\\begin{table}fixture\\end{table}", table_file)
  writeLines(c("---", "title: Fixture", "---", "# Intro"), paper_file)
  manifest <- list(
    paper = list(source = paper_file, repository_url = "https://example.com/repository"),
    coding_outputs = list(
      table = list(type = "latex", files = table_file, paper_label = "tbl-fixture")
    )
  )
  spec <- list(excerpts = list(list(
    id = "fixture", file = code_file, title = "Fixture code", outputs = "table"
  )))

  body <- env$coding_sample_body(spec, "named", manifest, c(`tbl-fixture` = "3"))
  code_heading <- match("## Fixture code", body)
  annotation_line <- which(grepl("Output: ", body, fixed = TRUE))[[1L]]
  output_line <- which(grepl("\\input{../../", body, fixed = TRUE))[[1L]]

  expect_true(is.finite(code_heading))
  expect_true(is.finite(annotation_line))
  expect_true(is.finite(output_line))
  expect_lt(code_heading, annotation_line)
  expect_lt(annotation_line, output_line)
  expect_true(any(grepl("answer <- 42", body, fixed = TRUE)))
})


test_that("coding-sample tables require an unambiguous full-paper number", {
  env <- sample_test_env()
  sys.source(repo_file("R", "application_samples", "coding_sample_outputs.R"), envir = env)
  table_file <- tempfile(fileext = ".tex")
  writeLines("\\begin{table}\\caption{Fixture}\\label{tbl-file-fixture}x\\end{table}", table_file)
  item <- list(type = "latex", files = table_file, paper_label = "tbl-paper-fixture")

  expect_error(env$selected_latex_lines(item, character()))
  expect_error(env$selected_latex_lines(item, c(`tbl-paper-fixture` = "7a")))
  expect_error(env$selected_latex_lines(
    list(type = "latex", files = table_file),
    c(`tbl-paper-fixture` = "7")
  ))
})


test_that("coding-sample figures require an unambiguous full-paper number", {
  env <- sample_test_env()
  sys.source(repo_file("R", "application_samples", "coding_sample_outputs.R"), envir = env)
  paper_file <- tempfile(fileext = ".qmd")
  writeLines(c(
    "---", "title: Fixture", "---", "",
    "![Fixture](figure.pdf){#fig-fixture}"
  ), paper_file)
  manifest <- list(paper = list(source = paper_file))
  item <- list(type = "figure", paper_label = "fig-fixture")

  expect_error(env$selected_figure_lines(item, manifest, character()))
  expect_error(env$selected_figure_lines(item, manifest, c(`fig-fixture` = "4a")))
  expect_error(env$selected_figure_lines(
    list(type = "figure"), manifest, c(`fig-fixture` = "4")
  ))
})


test_that("citation-link filter sends named-sample citations to the full paper", {
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc is unavailable")
  input <- tempfile(fileext = ".md")
  output <- tempfile(fileext = ".tex")
  filter <- repo_file("application-samples", "filters", "link-citations-to-paper.lua")
  bibliography <- repo_file("paper", "references.bib")
  writeLines(c(
    "---",
    paste0("bibliography: ", bibliography),
    "suppress-bibliography: true",
    "sample-full-paper-url: https://example.com/paper.pdf",
    "---",
    "Source [@shastry2012a]."
  ), input)

  status <- system2(
    Sys.which("pandoc"),
    c(
      shQuote(input),
      paste0("--lua-filter=", shQuote(filter)),
      "--citeproc", "-t", "latex", "-o", shQuote(output)
    )
  )
  expect_identical(status, 0L)
  rendered <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(rendered, "\\href{https://example.com/paper.pdf}", fixed = TRUE)
  expect_match(rendered, "Shastry", fixed = TRUE)
})



test_that("public TeX tables honor application-sample reference numbers", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("R", "output", "public_qmd_helpers.R"), envir = env)
  tex <- tempfile(fileext = ".tex")
  writeLines(c(
    "\\begin{longtable}{l}",
    "\\caption{\\label{tbl-fixture}Fixture}\\\\",
    "x\\\\",
    "\\end{longtable}"
  ), tex)
  old <- getOption("emi.application_sample_reference_labels")
  on.exit(options(emi.application_sample_reference_labels = old), add = TRUE)
  options(emi.application_sample_reference_labels = c(`tbl-fixture` = "7"))

  rendered <- as.character(env$render_public_tex(tex))
  expect_match(rendered, "\\setcounter{table}{6}", fixed = TRUE)
  expect_lt(
    regexpr("\\setcounter{table}{6}", rendered, fixed = TRUE),
    regexpr("\\caption", rendered, fixed = TRUE)
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

  expect_error(env$validate_writing_reference_labels(source, "sec-keep", c(`sec-keep` = "3")))
  expect_no_error(env$validate_writing_reference_labels(
    source,
    "sec-keep",
    c(`sec-keep` = "3", `fig-keep` = "4")
  ))
})

test_that("section-selection filter can retain a numbered figure outside selected sections", {
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc is unavailable")
  input <- tempfile(fileext = ".md")
  output <- tempfile(fileext = ".tex")
  writeLines(c(
    "---",
    "sample-sections:",
    "  - sec-keep",
    "sample-figures:",
    "  - fig-extra",
    "sample-reference-labels:",
    "  sec-keep: '1'",
    "  fig-extra: '7'",
    "---",
    "# Keep {#sec-keep}",
    "",
    "Kept text.",
    "",
    "# Omit {#sec-omit}",
    "",
    "Omitted text.",
    "",
    "::: {#fig-extra layout-ncol=2}",
    "![Panel A](figure-a.pdf)",
    "",
    "![Panel B](figure-b.pdf)",
    "",
    "Selected figure",
    ":::"
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
  expect_match(rendered, "Kept text.", fixed = TRUE)
  expect_match(rendered, "Selected figure", fixed = TRUE)
  expect_match(rendered, "\\setcounter{figure}{6}", fixed = TRUE)
  expect_false(grepl("Omitted text.", rendered, fixed = TRUE))
})


test_that("schooling geography figure is anchored before the following subsection", {
  source <- readLines(repo_file("paper", "paper.qmd"), warn = FALSE)
  figure <- grep("::: {#fig-schooling-geography", source, fixed = TRUE)
  next_section <- grep("{#sec-schooling-market}", source, fixed = TRUE)

  expect_length(figure, 1L)
  expect_length(next_section, 1L)
  expect_lt(figure, next_section)
  expect_match(source[[figure]], 'fig-pos="H"', fixed = TRUE)
})


test_that("writing figure selection validates paper membership and reference numbering", {
  env <- sample_test_env()
  source <- tempfile(fileext = ".qmd")
  writeLines(c(
    "---", "title: Fixture", "---",
    "# Keep {#sec-keep}", "",
    "![Figure](figure.pdf){#fig-extra}"
  ), source)

  expect_no_error(env$validate_writing_figure_ids(source, "fig-extra"))
  expect_error(env$validate_writing_figure_ids(source, "fig-missing"))
  expect_error(env$validate_writing_figure_ids(source, "table-extra"))
  expect_no_error(env$validate_writing_reference_labels(
    source, "sec-keep", c(`sec-keep` = "1", `fig-extra` = "7"), "fig-extra"
  ))
  expect_error(env$validate_writing_reference_labels(
    source, "sec-keep", c(`sec-keep` = "1"), "fig-extra"
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
    "See @sec-drop and @fig-extra.",
    "",
    "# Drop {#sec-drop}",
    "",
    "Omitted.",
    "",
    "![Selected figure](figure.pdf){#fig-extra}"
  )
  body <- env$split_qmd_front_matter(source)$body
  labels <- c(`sec-drop` = "2", `fig-extra` = "7")

  named <- env$externalize_omitted_writing_crossrefs(
    body, source, "sec-keep", labels, "named", "https://example.com/paper.pdf",
    figure_ids = "fig-extra"
  )
  anonymous <- env$externalize_omitted_writing_crossrefs(
    body, source, "sec-keep", labels, "anonymous", "https://example.com/paper.pdf",
    figure_ids = "fig-extra"
  )

  named_text <- paste(named, collapse = "\n")
  anonymous_text <- paste(anonymous, collapse = "\n")
  expect_match(named_text, "[Section 2](https://example.com/paper.pdf)", fixed = TRUE)
  expect_match(named_text, "@fig-extra", fixed = TRUE)
  expect_false(grepl("@sec-drop", named_text, fixed = TRUE))
  expect_match(anonymous_text, "Section 2", fixed = TRUE)
  expect_false(grepl("example.com", anonymous_text, fixed = TRUE))
  expect_match(anonymous_text, "@fig-extra", fixed = TRUE)
  expect_error(env$externalize_omitted_writing_crossrefs(
    body, source, "sec-keep", c(`fig-keep` = "1"), "named", "https://example.com/paper.pdf"
  ))
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
  expect_error(env$read_latex_reference_labels(aux))
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

  expect_error(env$assemble_writing_sample_qmd(source, spec, "named", manifest, output))
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
  expect_message(env$check_writing_sample_page_counts(c(pdf, pdf), c(2L, 3L)))
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
