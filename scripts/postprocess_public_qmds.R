# Postprocess generated public QMDs after copying prose from the legacy Rmd.
# This keeps the copied prose intact while applying Quarto-specific syntax changes:
# - section labels become #sec-* labels;
# - legacy \@ref(...) references become Quarto @sec-*, @fig-*, @tbl-*, or @eq-* references;
# - citation links are enabled in YAML;
# - bibliography paths remain valid from each QMD's directory;
# - the legacy abstract is restored to the report YAML.

qmd_paths <- c(
  "paper/report.qmd",
  "paper/appendix.qmd",
  "docs/district-matching.qmd",
  "docs/long-paths-and-8-3-filenames.qmd"
)

legacy_abstract <- paste0(
  "English-medium instruction (EMI), or the teaching of school subjects in English, ",
  "is often viewed as a potential tool for economic mobility in India, where English ",
  "skills command substantial wage premia. Yet it remains unclear whether greater ",
  "exposure to EMI generates broader local development gains. We study whether ",
  "district-level EMI exposure in 2007, measured as the share of school-going children ",
  "enrolled in EMI, affected growth in average household consumption between 2007 ",
  "and 2018. To address endogeneity, we instrument EMI exposure using a proxy for ",
  "the opporunity cost of acquiring EMI over Hindi-based schooling: the ",
  "population-weighted average linguistic distance of districts’ mother tongues from ",
  "Hindi in 2001. District-level 2SLS estimates with state-clustered standard errors ",
  "are positive but insignificant, providing limited evidence that EMI exposure ",
  "increased local consumption growth over the medium run. This district-level ",
  "equilibrium analysis is supplemented with an individual-level probit model of ",
  "selection into education. We conclude by discussing threats to identification and ",
  "interpretation (namely spatial autocorrelation and migration) and their ",
  "implications for future work."
)

legacy_figure_captions <- c(
  "fig-ILO-fig" = "Trends in earnings, labor‐force participation, and unemployment (ILO, 2024).",
  "fig-map1-fig" = '(Clockwise from top left) EMI exposure, consumption growth, pucca (permanent) housing, and household heads with secondary education or more. Data from the 64th round of the NSS 2007-08, "Participation and Expenditure in Education" and "Household Consumer Expenditure."',
  "fig-map2-fig" = "From left to right: regions of India and linguistic distance from Hindi. District-level data, from the 2001 Census of India.",
  "fig-districtcarveoutsshifts-fig" = "Number of 2001 districts which absorbed a percentage of a 1991 district's population via name change, clean merger, carve-out, or border shift. Data from Kumar \\& Somanathan (2016)."
)


legacy_table_captions <- c(
  "tbl-sum-tbl-probit-quant" = "Summary Statistics for Enrollment Participation Model (Numeric Variables)",
  "tbl-sum-tbl-probit-cat" = "Summary Statistics for Enrollment Participation Model (Categorical Variables)",
  "tbl-probit-mfx" = "Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit",
  "tbl-sum-tbl-iv" = "Summary Statistics for 2SLS Model",
  "tbl-fs-cons" = "First-Stage Regression: EMI Exposure on Linguistic Distance",
  "tbl-cons-iv" = "Second-Stage Regression: Consumption Growth on EMIE (Fitted)"
)

figure_markdown <- function(label, path) {
  paste0("![", legacy_figure_captions[[label]], "](", path, "){#", label, " fig-pos=\"H\" width=\"100%\"}")
}


ensure_yaml_field <- function(lines, field, value) {
  if (!length(lines) || !identical(lines[[1]], "---")) return(lines)
  close <- which(lines[-1L] == "---")
  if (!length(close)) return(lines)
  end <- close[[1]] + 1L
  field_re <- paste0("^", field, ":")
  if (any(grepl(field_re, lines[seq_len(end)], perl = TRUE))) return(lines)
  append(lines, paste0(field, ": ", value), after = end - 1L)
}

rewrite_yaml_field <- function(lines, field, value) {
  if (!length(lines) || !identical(lines[[1]], "---")) return(lines)
  close <- which(lines[-1L] == "---")
  if (!length(close)) return(lines)
  end <- close[[1]] + 1L
  field_re <- paste0("^", field, ":")
  idx <- grep(field_re, lines[seq_len(end)], perl = TRUE)
  if (length(idx)) {
    lines[idx[[1]]] <- paste0(field, ": ", value)
  } else {
    lines <- append(lines, paste0(field, ": ", value), after = end - 1L)
  }
  lines
}

remove_yaml_field <- function(lines, field) {
  if (!length(lines) || !identical(lines[[1]], "---")) return(lines)
  close <- which(lines[-1L] == "---")
  if (!length(close)) return(lines)
  end <- close[[1]] + 1L
  field_re <- paste0("^", field, ":")
  idx <- grep(field_re, lines[seq_len(end)], perl = TRUE)
  if (!length(idx)) return(lines)
  lines[-idx[[1]]]
}

ensure_yaml_block <- function(lines, field, values) {
  if (!length(lines) || !identical(lines[[1]], "---")) return(lines)
  close <- which(lines[-1L] == "---")
  if (!length(close)) return(lines)
  end <- close[[1]] + 1L
  field_re <- paste0("^", field, ":")
  if (any(grepl(field_re, lines[seq_len(end)], perl = TRUE))) return(lines)
  append(lines, c(paste0(field, ":"), values), after = end - 1L)
}

ensure_yaml_list_item <- function(lines, field, item) {
  if (!length(lines) || !identical(lines[[1]], "---")) return(lines)
  close <- which(lines[-1L] == "---")
  if (!length(close)) return(lines)
  end <- close[[1]] + 1L
  yaml_idx <- seq_len(end)
  if (any(grepl(item, lines[yaml_idx], fixed = TRUE))) return(lines)
  field_line <- grep(paste0("^", field, ":"), lines[yaml_idx], perl = TRUE)
  if (!length(field_line)) return(ensure_yaml_block(lines, field, paste0("  - ", item)))
  insert_after <- field_line[[1]]
  next_field <- which(seq_along(lines) > field_line[[1]] & seq_along(lines) <= end & grepl("^[A-Za-z0-9_-]+:", lines, perl = TRUE))
  if (length(next_field)) insert_after <- next_field[[1]] - 1L else insert_after <- end - 1L
  append(lines, paste0("  - ", item), after = insert_after)
}


ensure_pdf_geometry <- function(lines, geometry) {
  if (!length(lines) || !identical(lines[[1]], "---")) return(lines)
  close <- which(lines[-1L] == "---")
  if (!length(close)) return(lines)
  end <- close[[1]] + 1L
  yaml <- lines[seq_len(end)]
  if (any(grepl(paste0("- ", geometry), yaml, fixed = TRUE))) return(lines)
  pdf_engine <- grep("pdf-engine:", yaml, fixed = TRUE)
  if (length(pdf_engine)) {
    insert <- pdf_engine[[length(pdf_engine)]]
    return(append(lines, c("    geometry:", paste0("      - ", geometry)), after = insert))
  }
  format_line <- grep("format:", yaml, fixed = TRUE)
  if (length(format_line)) {
    return(append(lines, c("  pdf:", "    pdf-engine: xelatex", "    geometry:", paste0("      - ", geometry)), after = format_line[[1]]))
  }
  append(lines, c("format:", "  pdf:", "    pdf-engine: xelatex", "    geometry:", paste0("      - ", geometry)), after = end - 1L)
}

normalize_yaml <- function(lines, path) {
  if (grepl("^paper/", path)) {
    lines <- rewrite_yaml_field(lines, "bibliography", "references.bib")
  }
  if (identical(path, "paper/report.qmd")) {
    lines <- rewrite_yaml_field(lines, "abstract", paste0('"', legacy_abstract, '"'))
    lines <- rewrite_yaml_field(lines, "author", '"Rishav Roy"')
    lines <- rewrite_yaml_field(lines, "geometry", '"left=0.85in, right=0.85in, top=0.9in, bottom=0.9in"')
    lines <- ensure_yaml_block(lines, "header-includes", c(
      "  - \\usepackage{setspace}\\doublespacing",
      "  - \\usepackage{mathtools}",
      "  - \\usepackage{longtable}",
      "  - \\usepackage{booktabs}",
      "  - \\usepackage{array}",
      "  - \\usepackage{xcolor}",
      "  - \\definecolor{gray35}{gray}{0.35}",
      "  - \\usepackage{colortbl}",
      "  - \\usepackage{pdflscape}",
      "  - \\usepackage{threeparttablex}",
      "  - \\usepackage{makecell}",
      "  - \\usepackage{tabularray}",
      "  - \\usepackage{float}",
      "  - \\usepackage{threeparttable}",
      "  - \\usepackage{etoolbox}",
      "  - \\AtBeginEnvironment{CSLReferences}{\\setstretch{1}\\setlength{\\parskip}{0pt}\\setlength{\\itemsep}{0pt}}",
      "  - \\UseTblrLibrary{booktabs}",
      "  - \\UseTblrLibrary{siunitx}"
    ))
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{booktabs}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{array}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{xcolor}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\definecolor{gray35}{gray}{0.35}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{colortbl}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{pdflscape}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{threeparttablex}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{makecell}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{caption}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{subcaption}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\captionsetup{width=.95\\linewidth,justification=centering}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\captionsetup[subtable]{labelformat=empty}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{float}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{threeparttable}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\usepackage{etoolbox}")
    lines <- ensure_yaml_list_item(lines, "header-includes", "\\AtBeginEnvironment{CSLReferences}{\\setstretch{1}\\setlength{\\parskip}{0pt}\\setlength{\\itemsep}{0pt}}")
  }
  if (identical(path, "docs/district-matching.qmd")) {
    lines <- rewrite_yaml_field(lines, "bibliography", "../paper/references.bib")
  }
  if (!identical(path, "docs/long-paths-and-8-3-filenames.qmd")) {
    lines <- ensure_yaml_field(lines, "link-citations", "true")
    lines <- ensure_yaml_field(lines, "cite-method", "citeproc")
  }
  lines
}

normalize_heading_labels <- function(lines) {
  is_heading <- grepl("^#{1,6}\\s", lines)
  lines[is_heading] <- gsub("\\{#(?!sec-|fig-|tbl-|eq-)([A-Za-z0-9_-]+)\\}", "{#sec-\\1}", lines[is_heading], perl = TRUE)
  lines
}

convert_legacy_crossrefs <- function(lines) {
  lines <- gsub("\\\\@ref\\(fig:([A-Za-z0-9_-]+)\\)", "@fig-\\1", lines, perl = TRUE)
  lines <- gsub("\\\\@ref\\(tab:([A-Za-z0-9_-]+)\\)", "@tbl-\\1", lines, perl = TRUE)
  lines <- gsub("\\\\@ref\\(eq:([A-Za-z0-9_-]+)\\)", "@eq-\\1", lines, perl = TRUE)
  lines <- gsub("\\\\@ref\\(([A-Za-z0-9_-]+)\\)", "@sec-\\1", lines, perl = TRUE)
  lines <- gsub("@ref\\(fig:([A-Za-z0-9_-]+)\\)", "@fig-\\1", lines, perl = TRUE)
  lines <- gsub("@ref\\(tab:([A-Za-z0-9_-]+)\\)", "@tbl-\\1", lines, perl = TRUE)
  lines <- gsub("@ref\\(eq:([A-Za-z0-9_-]+)\\)", "@eq-\\1", lines, perl = TRUE)
  lines <- gsub("@ref\\(([A-Za-z0-9_-]+)\\)", "@sec-\\1", lines, perl = TRUE)
  lines <- gsub("\\(\\\\#eq:([A-Za-z0-9_-]+)\\)", "{#eq-\\1}", lines, perl = TRUE)
  lines
}

remove_quarto_crossref_prefixes <- function(lines) {
  lines <- gsub("\\bFigures?\\s+(@fig-[A-Za-z0-9_-]+)", "\\1", lines, perl = TRUE)
  lines <- gsub("\\bTables?\\s+(@tbl-[A-Za-z0-9_-]+)", "\\1", lines, perl = TRUE)
  lines <- gsub("\\b(?:Sec\\.|Sections?)\\s+(@sec-[A-Za-z0-9_-]+)", "\\1", lines, perl = TRUE)
  lines
}

fix_equation_labels <- function(lines) {
  label_idx <- grep("^\\{#eq-[A-Za-z0-9_-]+\\}\\s*$", lines)
  if (!length(label_idx)) return(lines)

  for (idx in rev(label_idx)) {
    label <- trimws(lines[[idx]])
    starts <- grep("^\\\\begin\\{align\\}", lines[seq_len(idx)], perl = TRUE)
    ends_rel <- grep("^\\\\end\\{align\\}", lines[idx:length(lines)], perl = TRUE)
    if (!length(starts) || !length(ends_rel)) next

    start <- max(starts)
    end <- idx + min(ends_rel) - 1L
    if (start >= idx || end <= idx) next

    block <- lines[start:end]
    block <- block[trimws(block) != label]
    block[grepl("^\\\\begin\\{align\\}\\s*$", block, perl = TRUE)] <- "$$"
    block[grepl("^\\\\begin\\{split\\}\\s*$", block, perl = TRUE)] <- "\\begin{aligned}"
    block[grepl("^\\\\end\\{split\\}\\s*$", block, perl = TRUE)] <- "\\end{aligned}"
    block[grepl("^\\\\end\\{align\\}\\s*$", block, perl = TRUE)] <- paste0("$$ ", label)

    lines <- c(
      if (start > 1L) lines[seq_len(start - 1L)] else character(),
      block,
      if (end < length(lines)) lines[(end + 1L):length(lines)] else character()
    )
  }
  lines
}

cleanup_public_placeholders <- function(lines) {
  lines <- gsub("not yet available", "—", lines, fixed = TRUE)
  lines <- gsub("not run in current draft pipeline", "—", lines, fixed = TRUE)
  lines
}


defer_unavailable_morans_i_values <- function(lines) {
  lines[startsWith(lines, "We can control for this spatial autocorrelation by incorporating spatial lags into our model. To test for spatial autocorrelation")] <-
    "We can control for this spatial autocorrelation by incorporating spatial lags into our model. To test for spatial autocorrelation we would follow p. 323 of @anselin2001 and construct a Moran's I statistic; but proper estimation of it would depend on us having proper shapefiles for the year of interest with which to build contiguity neighbor lists. If our unit of analysis ends up being 2001 districts, then this would require 2001 shapefiles. The current 2019-20 shapefile is available in the repository, but the active district panel is not yet joined to geometry with a validated 2001/2007-to-2020 crosswalk, so we do not report Moran's I $p$-values in this draft. Future work will combine shapefiles from the 2011 and 2001 censuses with the data harmonization changes described in @sec-distma to ensure these reflect genuine spatial autocorrelation as opposed to the flaws of our current district tracking methodology."

  lines
}


fix_district_note_crossrefs <- function(lines) {
  lines <- gsub("These district changes are plotted in @fig-districtcarveoutsshifts-fig.", "These district changes are summarized in the district carve-outs diagnostic figure generated with the main public artifacts.", lines, fixed = TRUE)
  lines <- gsub("These district changes are plotted in Figure @fig-districtcarveoutsshifts-fig.", "These district changes are summarized in the district carve-outs diagnostic figure generated with the main public artifacts.", lines, fixed = TRUE)
  lines <- gsub("These district changes are plotted in the district carve-outs figure below.", "These district changes are summarized in the district carve-outs diagnostic figure generated with the main public artifacts.", lines, fixed = TRUE)
  lines <- gsub("see @sec-iv-iv", "see the IV section of the main report", lines, fixed = TRUE)
  lines <- gsub("in @sec-iv-iv", "in the IV section of the main report", lines, fixed = TRUE)
  lines <- gsub("@sec-iv-iv", "the IV section of the main report", lines, fixed = TRUE)
  lines <- gsub("@sec-intro", "the introduction of the main report", lines, fixed = TRUE)
  lines <- gsub("@sec-distma", "this district-matching note", lines, fixed = TRUE)
  lines <- gsub("@fig-districtcarveoutsshifts-fig", "the district carve-outs figure below", lines, fixed = TRUE)
  lines
}

fix_appendix_crossrefs <- function(lines) {
  lines <- gsub("Equation @eq-iv-eq", "the main report's district-level 2SLS equation", lines, fixed = TRUE)
  lines <- gsub("in @tbl-cons-iv", "in the main report's second-stage table", lines, fixed = TRUE)
  lines <- gsub("see @sec-iv-iv", "see the IV section of the main report", lines, fixed = TRUE)
  lines <- gsub("in @sec-iv-iv", "in the IV section of the main report", lines, fixed = TRUE)
  lines <- gsub("@sec-intro", "the introduction of the main report", lines, fixed = TRUE)
  lines
}

neutralize_standalone_map_crossrefs <- function(lines) {
  # Legacy prose in the appendix and district-matching note mentions the main
  # report's map figures. Those standalone documents do not define the map
  # figure labels, so keep the prose but remove Quarto cross-reference tokens.
  lines <- gsub("the maps of Figures @fig-map1-fig and @fig-map2-fig", "the main report's map figures", lines, fixed = TRUE)
  lines <- gsub("the maps of @fig-map1-fig and @fig-map2-fig", "the main report's map figures", lines, fixed = TRUE)
  lines <- gsub("Figures @fig-map1-fig and @fig-map2-fig", "the main report's map figures", lines, fixed = TRUE)
  lines <- gsub("@fig-map1-fig and @fig-map2-fig", "the main report's map figures", lines, fixed = TRUE)
  lines <- gsub("@fig-map1-fig", "the main report's first map figure", lines, fixed = TRUE)
  lines <- gsub("@fig-map2-fig", "the main report's second map figure", lines, fixed = TRUE)
  lines
}

fix_appendix_headings <- function(lines) {
  idx <- grep("^# \\(APPENDIX\\) Appendix \\{-\\}$", lines, perl = TRUE)
  if (!length(idx)) return(lines)
  first <- idx[[1]]
  lines[first] <- "\\appendix"
  dup <- which(seq_along(lines) > first & lines == "# Appendix {#sec-appendix}")
  if (!length(dup)) {
    lines <- append(lines, c("", "# Appendix {#sec-appendix}"), after = first)
  }
  lines
}



insert_after_first <- function(lines, pattern, block) {
  hit <- grep(pattern, lines, fixed = TRUE)
  if (!length(hit)) return(lines)
  append(lines, block, after = hit[[1]])
}

output_table_helper_chunk <- function() {
  c(
    "```{r public-output-table-helper}",
    "#| include: false",
    "find_targets_store <- function(start = getwd()) {",
    "  here <- normalizePath(start, mustWork = TRUE)",
    "  repeat {",
    "    candidate <- file.path(here, \"_targets\")",
    "    if (dir.exists(candidate)) return(candidate)",
    "    parent <- dirname(here)",
    "    if (identical(parent, here)) return(\"_targets\")",
    "    here <- parent",
    "  }",
    "}",
    "report_values <- tryCatch(targets::tar_read(report_values, store = find_targets_store()), error = function(e) list())",
    "report_value <- function(key) {",
    "  value <- report_values[[key]]",
    "  if (is.null(value)) value <- NA",
    "  if (is.list(value) && !is.null(value$value)) value <- value$value",
    "  if (length(value) == 0L || all(is.na(value))) return(\"—\")",
    "  paste(value, collapse = \", \")",
    "}",
    "regression_star_note <- function() \"* p < 0.05, ** p < 0.01, *** p < 0.001\"",
    "legacy_table_caption_text <- function(name) {",
    "  captions <- c(",
    "    sum_tbl_probit_quant = \"Summary Statistics for Enrollment Participation Model (Numeric Variables)\",",
    "    sum_tbl_probit_cat = \"Summary Statistics for Enrollment Participation Model (Categorical Variables)\",",
    "    probit_mfx = \"Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit\",",
    "    sum_tbl_iv = \"Summary Statistics for 2SLS Model\",",
    "    fs_cons = \"First-Stage Regression: EMI Exposure on Linguistic Distance\",",
    "    cons_iv = \"Second-Stage Regression: Consumption Growth on EMIE (Fitted)\")",
    "  captions[[name]] %||% name",
    "}",
    "regression_caption <- function(cap) cap",
    "table_caption <- function(name) {",
    "  cap <- legacy_table_caption_text(name)",
    "  cap",
    "}",
    "table_note <- function(name) {",
    "  switch(name,",
    "    sum_tbl_probit_quant = \"Min. = minimum; 1Q = first quartile; Med. = median; 3Q = third quartile; Max. = maximum; Mean = arithmetic mean; SD = standard deviation; N = number of observations.\",",
    "    sum_tbl_iv = \"Min. = minimum; 1Q = first quartile; Med. = median; 3Q = third quartile; Max. = maximum; Mean = arithmetic mean; SD = standard deviation; N = number of observations.\",",
    "    sum_tbl_probit_cat = \"Values = all possible values; Mode = most frequent value; Pct. Mode = percent of observations taking the modal value; Least Freq. = least frequent value; Pct. Least Freq. = percent of observations taking the least frequent value; N = number of observations.\",",
    "    probit_mfx = \"Data from the 64th round of the NSS, \\\"Participation and Expenditure in Education\\\" in 2007-08. All standard errors are design-based (clustered and nested within strata).\",",
    "    fs_cons = \"Standard errors clustered by state in parentheses.\",",
    "    cons_iv = \"Standard errors clustered by state in parentheses.\",",
    "    NULL)",
    "}",
    "resolve_public_output_path <- function(path) {",
    "  candidates <- unique(c(path, file.path(getwd(), path), file.path(\"paper\", path), file.path(dirname(knitr::current_input()), path), sub(\"^\\\\.\\\\/\", \"\", path)))",
    "  hit <- candidates[file.exists(candidates) & file.info(candidates)$size > 0]",
    "  if (length(hit)) return(hit[[1]])",
    "  stop(\"Missing table output: \", path, call. = FALSE)",
    "}",
    "read_public_table <- function(path) {",
    "  df <- utils::read.csv(resolve_public_output_path(path), check.names = FALSE, na.strings = character())",
    "  for (nm in names(df)) if (is.character(df[[nm]])) df[[nm]][is.na(df[[nm]])] <- \"\"",
    "  df",
    "}",
    "render_public_tex <- function(path) {",
    "  tex <- paste(readLines(resolve_public_output_path(path), warn = FALSE), collapse = \"\\n\")",
    "  knitr::asis_output(paste0(\"\\n\\n\", tex, \"\\n\\n\"))",
    "}",
    "cell_string <- function(x) {",
    "  if (length(x) == 0L) return(\"\")",
    "  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)",
    "  if (length(x) == 0L || all(is.na(x))) return(\"\")",
    "  paste(as.character(x), collapse = \"; \")",
    "}",
    "column_strings <- function(x) {",
    "  if (is.factor(x)) x <- as.character(x)",
    "  if (is.list(x)) out <- vapply(x, cell_string, character(1)) else out <- as.character(x)",
    "  out[is.na(out)] <- \"\"",
    "  out",
    "}",
    "summary_table_groups <- function(df) {",
    "  if (!nrow(df) || !length(names(df))) return(list(data = df, groups = data.frame()))",
    "  empty_rest <- if (ncol(df) > 1L) apply(df[-1], 1, function(x) all(!nzchar(column_strings(x)))) else rep(TRUE, nrow(df))",
    "  first_col <- column_strings(df[[1]])",
    "  group_row <- grepl(\":$\", first_col) & empty_rest",
    "  group_idx <- which(group_row)",
    "  if (!length(group_idx)) return(list(data = df, groups = data.frame()))",
    "  groups <- lapply(seq_along(group_idx), function(i) {",
    "    start_orig <- group_idx[[i]] + 1L",
    "    end_orig <- if (i < length(group_idx)) group_idx[[i + 1L]] - 1L else nrow(df)",
    "    start <- start_orig - sum(group_idx < start_orig)",
    "    end <- end_orig - sum(group_idx <= end_orig)",
    "    if (start > end) return(NULL)",
    "    data.frame(label = first_col[[group_idx[[i]]]], start = start, end = end, stringsAsFactors = FALSE)",
    "  })",
    "  groups <- do.call(rbind, Filter(Negate(is.null), groups))",
    "  if (is.null(groups)) groups <- data.frame()",
    "  list(data = df[!group_row, , drop = FALSE], groups = groups)",
    "}",
    "wrap_table_text <- function(df) as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)",
    "table_header_labels <- function(df, name) {",
    "  labels <- names(df)",
    "  wrap <- c(\"Pct. Mode\" = \"Pct.\\nMode\", \"Least Freq.\" = \"Least\\nFreq.\", \"Pct. Least Freq.\" = \"Pct. Least\\nFreq.\", \"Adjusted R-squared\" = \"Adjusted\\nR-squared\")",
    "  labels <- ifelse(labels %in% names(wrap), unname(wrap[labels]), labels)",
    "  vapply(labels, function(x) if (grepl(\"\\n\", x, fixed = TRUE)) kableExtra::linebreak(x, align = \"c\") else x, character(1))",
    "}",
    "caption_for_latex <- function(name) {",
    "  # Keep captions as plain text. Caption wrapping is handled by the LaTeX caption package;",
    "  # kableExtra::linebreak() is for cells/headers and corrupts full kable captions.",
    "  table_caption(name)",
    "}",
    "latex_escape_text <- function(x) {",
    "  column_strings(x)",
    "}",
    "render_regression_table <- function(df, name) {",
    "  if (!requireNamespace(\"modelsummary\", quietly = TRUE)) stop(\"modelsummary is required for regression table rendering.\", call. = FALSE)",
    "  if (ncol(df) < 2L) return(knitr::kable(df, row.names = FALSE))",
    "  model_col <- switch(name, probit_mfx = \"Enrolled (1 = yes)\", fs_cons = \"EMI Exposure\", cons_iv = \"Consumption Growth\", names(df)[[2]])",
    "  out <- data.frame(Term = latex_escape_text(df[[1]]), stringsAsFactors = FALSE, check.names = FALSE)",
    "  out[[model_col]] <- latex_escape_text(df[[2]])",
    "  out$Term[!nzchar(out$Term)] <- \"~\"",
    "  # Use modelsummary for layout, but emit Markdown into Quarto rather than raw LaTeX.",
    "  # Raw modelsummary LaTeX tabular output is fragile inside extracted writing-sample",
    "  # chunks with Quarto table captions; Markdown lets Pandoc own the final LaTeX table.",
    "  note <- table_note(name)",
    "  tab <- suppressWarnings(modelsummary::datasummary_df(out, output = \"markdown\", fmt = identity, align = \"lc\"))",
    "  md <- as.character(tab)",
    "  if (!is.null(note) && !grepl(note, md, fixed = TRUE)) md <- paste0(md, \"\\n\\n_\", note, \"_\")",
    "  md",
    "}",
    "render_public_table <- function(path, name) {",
    "  if (tolower(tools::file_ext(path)) == \"tex\") return(render_public_tex(path))",
    "  df <- read_public_table(path)",
    "  grouped <- summary_table_groups(df)",
    "  df_render <- wrap_table_text(grouped$data)",
    "  wide <- name %in% c(\"sum_tbl_iv\", \"sum_tbl_probit_quant\", \"sum_tbl_probit_cat\")",
    "  regression <- name %in% c(\"probit_mfx\", \"fs_cons\", \"cons_iv\")",
    "  if (regression) return(knitr::asis_output(render_regression_table(df_render, name)))",
    "  names(df_render) <- table_header_labels(df_render, name)",
    "  tab <- knitr::kable(df_render, digits = 3, booktabs = knitr::is_latex_output(), longtable = knitr::is_latex_output() && !wide, escape = FALSE, row.names = FALSE, caption = caption_for_latex(name), linesep = \"\")",
    "  if (knitr::is_latex_output() && requireNamespace(\"kableExtra\", quietly = TRUE)) {",
    "    opts <- c(\"striped\")",
    "    if (!wide) opts <- c(opts, \"repeat_header\")",
    "    tab <- kableExtra::kable_styling(tab, latex_options = opts, position = \"center\", full_width = FALSE, font_size = 10)",
    "    if (nrow(grouped$groups)) for (i in rev(seq_len(nrow(grouped$groups)))) tab <- kableExtra::pack_rows(tab, grouped$groups$label[[i]], grouped$groups$start[[i]], grouped$groups$end[[i]], bold = TRUE, italic = FALSE, background = \"white\", escape = FALSE)",
    "    if (name == \"sum_tbl_probit_cat\") tab <- tab |> kableExtra::column_spec(1, width = \"3.0cm\") |> kableExtra::column_spec(2, width = \"5.0cm\") |> kableExtra::column_spec(3, width = \"2.4cm\") |> kableExtra::column_spec(4, width = \"1.35cm\") |> kableExtra::column_spec(5, width = \"2.7cm\") |> kableExtra::column_spec(6, width = \"1.45cm\") |> kableExtra::column_spec(7, width = \"1.25cm\")",
    "    if (name == \"sum_tbl_iv\") tab <- tab |> kableExtra::column_spec(1, width = \"3.0cm\") |> kableExtra::column_spec(2, width = \"4.6cm\") |> kableExtra::column_spec(3:ncol(df_render), width = \"1.45cm\")",
    "    if (name == \"sum_tbl_probit_quant\") tab <- tab |> kableExtra::column_spec(1, width = \"4.0cm\") |> kableExtra::column_spec(2:ncol(df_render), width = \"1.55cm\")",
    "    note <- table_note(name)",
    "    if (!is.null(note)) tab <- kableExtra::footnote(tab, general = note, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE)",
    "    if (wide) tab <- kableExtra::landscape(tab)",
    "  }",
    "  tab",
    "}",
    "```"
  )
}

ensure_output_table_helper <- function(lines) {
  if (any(grepl("public-output-table-helper", lines, fixed = TRUE))) return(lines)
  insert_at <- grep("^```\\{r", lines, perl = TRUE)
  if (length(insert_at)) return(append(lines, c("", output_table_helper_chunk(), ""), after = insert_at[[1]] - 1L))
  append(lines, c("", output_table_helper_chunk(), ""), after = length(lines))
}

public_table_name_for_label <- function(label) {
  switch(label,
    "tbl-sum-tbl-probit-quant" = "sum_tbl_probit_quant",
    "tbl-sum-tbl-probit-cat" = "sum_tbl_probit_cat",
    "tbl-probit-mfx" = "probit_mfx",
    "tbl-sum-tbl-iv" = "sum_tbl_iv",
    "tbl-fs-cons" = "fs_cons",
    "tbl-cons-iv" = "cons_iv",
    gsub("^tbl-", "", gsub("-", "_", label))
  )
}

output_table_chunk <- function(label, caption, path) {
  caption <- gsub("\\\\", "\\\\\\\\", caption)
  caption <- gsub('"', '\\"', caption, fixed = TRUE)
  is_raw_tex <- identical(tolower(tools::file_ext(path)), "tex")
  chunk <- c(
    "```{r}",
    paste0("#| label: ", label),
    "#| echo: false",
    "#| results: asis"
  )
  if (!is_raw_tex) chunk <- append(chunk, paste0("#| tbl-cap: \"", caption, "\""), after = 2L)

  c(
    chunk,
    "",
    paste0("render_public_table(\"", path, "\", \"", public_table_name_for_label(label), "\")"),
    "```"
  )
}
normalize_inserted_output_captions <- function(lines) {
  lines <- gsub("^Table: +", "", lines)
  lines <- gsub("^Figure: +", "", lines)
  lines
}

normalize_legacy_quotes <- function(lines) {
  lines <- gsub("``([^`]*)''", '"\\1"', lines, perl = TRUE)
  lines
}

remove_report_output_objects <- function(lines) {
  labels <- c(
    "tbl-sum-tbl-probit-quant", "tbl-sum-tbl-probit-cat", "tbl-probit-mfx",
    "tbl-sum-tbl-iv", "tbl-fs-cons", "tbl-cons-iv"
  )
  out <- character()
  i <- 1L
  while (i <= length(lines)) {
    if (grepl("^```\\{r\\}", lines[[i]], perl = TRUE)) {
      end <- which(seq_along(lines) > i & grepl("^```\\s*$", lines, perl = TRUE))
      end <- if (length(end)) end[[1]] else i
      block <- lines[i:end]
      if (any(grepl(paste0("#\\|\\s*label:\\s*(", paste(labels, collapse = "|"), ")\\s*$"), block, perl = TRUE))) {
        i <- end + 1L
        next
      }
    }
    if (grepl("\\{#fig-(map1|map2|ILO|districtcarveoutsshifts)-fig(\\s|\\})", lines[[i]], perl = TRUE)) {
      i <- i + 1L
      next
    }
    out <- c(out, lines[[i]])
    i <- i + 1L
  }
  out
}

insert_after_line <- function(lines, pattern, block, fixed = TRUE) {
  hit <- grep(pattern, lines, fixed = fixed)
  if (!length(hit)) return(lines)
  append(lines, c("", block, ""), after = hit[[1]])
}

insert_report_output_objects_explicit <- function(lines) {
  lines <- remove_report_output_objects(lines)
  lines <- insert_after_line(
    lines,
    "Summary statistics for numeric variables are given in @tbl-sum-tbl-probit-quant, and for categorical variables in @tbl-sum-tbl-probit-cat.",
    c(
      output_table_chunk("tbl-sum-tbl-probit-quant", legacy_table_captions[["tbl-sum-tbl-probit-quant"]], "../outputs/tables/main/sum_tbl_probit_quant.tex"),
      "",
      output_table_chunk("tbl-sum-tbl-probit-cat", legacy_table_captions[["tbl-sum-tbl-probit-cat"]], "../outputs/tables/main/sum_tbl_probit_cat.tex")
    )
  )
  lines <- insert_after_line(
    lines,
    "@tbl-probit-mfx has been calculated over all observations",
    output_table_chunk("tbl-probit-mfx", legacy_table_captions[["tbl-probit-mfx"]], "../outputs/tables/main/probit_mfx.tex")
  )
  lines <- insert_after_line(
    lines,
    "Summary statistics for all of the variables in this model, including the controls",
    c(
      figure_markdown("fig-map1-fig", "../outputs/figures/main/collage_main_maps.pdf"),
      "",
      output_table_chunk("tbl-sum-tbl-iv", legacy_table_captions[["tbl-sum-tbl-iv"]], "../outputs/tables/main/sum_tbl_iv.tex")
    )
  )
  lines <- insert_after_line(
    lines,
    "We are currently unable to replicate her justification of the exclusion restriction",
    figure_markdown("fig-map2-fig", "../outputs/figures/main/collage_iv_region_maps.pdf")
  )
  lines <- insert_after_line(
    lines,
    "The results of our first-stage regression, of EMI exposure on linguistic distance, are provided in @tbl-fs-cons.",
    c(
      output_table_chunk("tbl-fs-cons", legacy_table_captions[["tbl-fs-cons"]], "../outputs/tables/main/fs_cons.tex"),
      "",
      output_table_chunk("tbl-cons-iv", legacy_table_captions[["tbl-cons-iv"]], "../outputs/tables/main/cons_iv.tex")
    )
  )
  lines <- ensure_report_object(lines, "fig-ILO-fig", figure_markdown("fig-ILO-fig", "../outputs/figures/main/fig_ilo_trends.png"))
  lines <- ensure_report_object(lines, "fig-districtcarveoutsshifts-fig", figure_markdown("fig-districtcarveoutsshifts-fig", "../outputs/figures/main/district_carveouts_shifts.pdf"))
  lines
}

normalize_output_table_chunks <- function(lines) {
  lines
}

insert_report_output_objects <- function(lines) {
  ensure_report_output_objects(lines)
}

ensure_report_object <- function(lines, label, block) {
  if (any(grepl(paste0("#", label, "(\\s|\\})"), lines, perl = TRUE)) ||
      any(grepl(paste0("#\\|\\s*label:\\s*", label, "\\s*$"), lines, perl = TRUE))) {
    return(lines)
  }
  ref <- grep(paste0("@", label), lines, fixed = TRUE)
  if (length(ref)) return(append(lines, c("", block, ""), after = ref[[1]]))
  append(lines, c("", block, ""), after = length(lines))
}

ensure_report_output_objects <- function(lines) {
  lines <- ensure_report_object(lines, "fig-ILO-fig", figure_markdown("fig-ILO-fig", "../outputs/figures/main/fig_ilo_trends.png"))
  lines <- ensure_report_object(lines, "fig-map1-fig", figure_markdown("fig-map1-fig", "../outputs/figures/main/collage_main_maps.pdf"))
  lines <- ensure_report_object(lines, "fig-map2-fig", figure_markdown("fig-map2-fig", "../outputs/figures/main/collage_iv_region_maps.pdf"))
  lines <- ensure_report_object(lines, "fig-districtcarveoutsshifts-fig", figure_markdown("fig-districtcarveoutsshifts-fig", "../outputs/figures/main/district_carveouts_shifts.pdf"))
  for (label in names(legacy_table_captions)) {
    path <- paste0("../outputs/tables/main/", gsub("-", "_", sub("^tbl-", "", label)), ".csv")
    # Legacy labels do not always map mechanically to file names.
    path <- switch(label,
      "tbl-sum-tbl-probit-quant" = "../outputs/tables/main/sum_tbl_probit_quant.tex",
      "tbl-sum-tbl-probit-cat" = "../outputs/tables/main/sum_tbl_probit_cat.tex",
      "tbl-probit-mfx" = "../outputs/tables/main/probit_mfx.tex",
      "tbl-sum-tbl-iv" = "../outputs/tables/main/sum_tbl_iv.tex",
      "tbl-fs-cons" = "../outputs/tables/main/fs_cons.tex",
      "tbl-cons-iv" = "../outputs/tables/main/cons_iv.tex",
      path
    )
    lines <- ensure_report_object(lines, label, output_table_chunk(label, legacy_table_captions[[label]], path))
  }
  lines
}

ensure_references_heading <- function(lines) {
  if (any(grepl("^# References", lines))) return(lines)
  appendix <- grep("^# (\\(APPENDIX\\) )?Appendix|^# A Appendix|^# B Technical Note", lines, perl = TRUE)
  insert_at <- if (length(appendix)) appendix[[1]] - 1L else length(lines)
  append(lines, c("", "# References {-}"), after = max(0L, insert_at - 1L))
}

postprocess_one <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- normalize_yaml(lines, path)
  lines <- normalize_heading_labels(lines)
  lines <- convert_legacy_crossrefs(lines)
  lines <- remove_quarto_crossref_prefixes(lines)
  lines <- fix_equation_labels(lines)
  lines <- cleanup_public_placeholders(lines)
  if (identical(path, "paper/report.qmd")) lines <- insert_report_output_objects_explicit(lines)
  if (identical(path, "paper/appendix.qmd")) {
    lines <- fix_district_note_crossrefs(lines)
    lines <- fix_appendix_crossrefs(lines)
  }
  if (identical(path, "docs/district-matching.qmd")) {
    lines <- fix_district_note_crossrefs(lines)
  }
  lines <- defer_unavailable_morans_i_values(lines)
  lines <- remove_quarto_crossref_prefixes(lines)
  if (identical(path, "paper/appendix.qmd") || identical(path, "docs/district-matching.qmd")) {
    lines <- neutralize_standalone_map_crossrefs(lines)
  }
  if (identical(path, "paper/report.qmd") || identical(path, "paper/appendix.qmd")) lines <- fix_appendix_headings(lines)
  if (identical(path, "paper/report.qmd")) lines <- ensure_references_heading(lines)

  lines <- normalize_inserted_output_captions(lines)
  if (identical(path, "paper/report.qmd")) {
    lines <- normalize_output_table_chunks(lines)
    lines <- insert_report_output_objects_explicit(lines)
  }
  if (identical(path, "paper/report.qmd")) {
    lines <- ensure_output_table_helper(lines)
  }
  lines <- normalize_legacy_quotes(lines)
  lines <- sub("[ \t]+$", "", lines, perl = TRUE)
  writeLines(lines, path)
  message("Postprocessed ", path)
}

for (path in qmd_paths[file.exists(qmd_paths)]) postprocess_one(path)
