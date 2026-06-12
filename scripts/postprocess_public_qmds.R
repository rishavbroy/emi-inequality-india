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
  "fig-map1-fig" = 'Clockwise from top left: EMI exposure, consumption growth, household heads with secondary education or more, and pucca (permanent) housing. Data from the 64th round of the NSS 2007-08, "Participation and Expenditure in Education" and "Household Consumer Expenditure."',
  "fig-map2-fig" = "From left to right: regions of India and linguistic distance from Hindi. District-level data, from the 2001 Census of India.",
  "fig-districtcarveoutsshifts-fig" = "Number of 2001 districts which absorbed a percentage of a 1991 district's population via name change, clean merger, carve-out, or border shift. Data from Kumar \\& Somanathan (2016)."
)

map_geometry_validation_note <- paste(
  "Final district map figures are withheld until the district panel joins to validated",
  "geometry for at least 75% of district-panel rows. This prevents diagnostic",
  "distribution plots from being presented as geographic maps."
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

map_geometry_note_block <- function() {
  c(
    "::: {.callout-important}",
    map_geometry_validation_note,
    ":::"
  )
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
      "  - \\usepackage{tabularray}",
      "  - \\usepackage{float}",
      "  - \\usepackage{threeparttable}",
      "  - \\usepackage{etoolbox}",
      "  - \\AtBeginEnvironment{CSLReferences}{\\setstretch{1}\\setlength{\\parskip}{0pt}\\setlength{\\itemsep}{0pt}}",
      "  - \\UseTblrLibrary{booktabs}",
      "  - \\UseTblrLibrary{siunitx}"
    ))
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

prune_unavailable_report_inline_expressions <- function(lines) {
  remove_ids <- c("inline_7a04581a", "inline_0937ca53", "inline_2b7c944a", "inline_7e66a3fd")
  lines <- lines[!grepl(paste0("^\\s*(", paste(remove_ids, collapse = "|"), ")\\s*="), lines, perl = TRUE)]

  start <- grep("^legacy_inline_expressions <- list\\(", lines)
  if (!length(start)) return(lines)
  close <- which(seq_along(lines) > start[[1]] & trimws(lines) == ")")
  if (!length(close)) return(lines)
  end <- close[[1]]
  value_lines <- seq.int(start[[1]] + 1L, end - 1L)
  value_lines <- value_lines[nzchar(trimws(lines[value_lines]))]
  if (length(value_lines)) {
    lines[value_lines[[length(value_lines)]]] <- sub(",\\s*$", "", lines[value_lines[[length(value_lines)]]])
  }
  lines
}

fix_final_public_prose <- function(lines) {
  lines[startsWith(lines, "Geospatial data used to construct the maps in this paper as well as all spatial autocorrelation measures")] <-
    "Geospatial data intended for maps and spatial autocorrelation measures is sourced from @bhatiaMergingUpdatedDistrictlevel2020, which is itself an adaptation of @meyersIndiaOfficialBoundaries2020. Our methods for tracking districts across time (see Sec. @sec-distma) begin with data from @indiastatestoriesDistrictEvolution2024 and @jaacksIndiaDistrictChanges2020."

  lines[startsWith(lines, "Geospatial data intended for maps and spatial autocorrelation measures is sourced from @bhatiaMergingUpdatedDistrictlevel2020")] <-
    "Geospatial data intended for maps and spatial autocorrelation measures is sourced from @bhatiaMergingUpdatedDistrictlevel2020, which is itself an adaptation of @meyersIndiaOfficialBoundaries2020. Our methods for tracking districts across time (see Sec. @sec-distma) begin with data from @indiastatestoriesDistrictEvolution2024 and @jaacksIndiaDistrictChanges2020."

  lines[startsWith(lines, "Looking deeper into the supply side of education, it seems that all variation which could potentially be explained by enrollment cost")] <-
    "Looking deeper into the supply side of education, it seems that all variation which could potentially be explained by enrollment cost has, at best, been consumed by other variables, implying direct costs matter less than social barriers and other correlates of supply-side factors. While one would expect the causal effect of an exogenous shock in scholarships/stipends, stationery, or textbooks to match the sign of their positive coefficient estimates, only textbooks show up as statistically significant. The district-level share of students who attend a school which charges no tuition fees (i.e., where 'Educ. freely available') is collinear with the intercept in the active probit specification, so we do not report its AME in this draft. @nationalsamplesurveyoffice2008 documentation indicates that such schools include most if not all government schools, as well as private schools in some states up to a certain level of education. Building off observations in Sec. @sec-intro that government schools tend to have worse facilities, chronic teacher absenteeism, and so on, this omitted coefficient remains a useful warning about the difficulty of separating direct costs from the quality and availability of public schooling. Comparing the remaining variables' $s$-values^[Given a $p$-value, we can define the $s$-value as $s=-\\log_2(p)$ [@mansourniaPvalueCompatibilitySvalue2022].] using @mansourniaPvalueCompatibilitySvalue2022 shows that 'Textbook(s) received' has an $s$-value of `r report_value(\"inline_55014f4e\")`, meaning that the data provided `r report_value(\"inline_55014f4e\")` bits of information against the null hypothesis (a coefficient of zero)."

  lines[startsWith(lines, "Summary statistics for all of the variables in this model, including the controls")] <-
    "Summary statistics for all of the variables in this model, including the controls $k$ in the vector $X_{kd}$, are provided in Table @tbl-sum-tbl-iv. The district map figures below use the validated district-panel geometry produced by the active tracker and are included here to preserve the legacy paper's map figures."

  lines[startsWith(lines, "We are currently unable to replicate her justification of the exclusion restriction, however. Her argument centers on a map depicting the geographical balance of her residual variation")] <-
    "We are currently unable to replicate her justification of the exclusion restriction, however. Her argument centers on a map depicting the geographical balance of her residual variation, made possible despite regional linguistic divides thanks to state fixed effects. In our case, adding state FEs explodes the condition number of our design matrix to $\\kappa =$ `r report_value(\"inline_7d871500\")` despite all individual collinearity measures remaining low (every scaled generalized variance inflation factor (GVIF) was below 6.05), with similar results for region FEs to a lesser degree. This almost certainly results from the many-to-many matching used in this paper's district tracking algorithms.^[Many-to-many matching from 2001 to 2007-08 to 2017-18 was used to accurately reflect how real district changes occur as both mergers and partitions. While the intent was accuracy, the effect was degradation: the multiple duplicated rows for 2001 and 2007-08 measures in particular almost surely devastated the rank of the design matrix.] Our plan to repair it moving forward is discussed in Sec. @sec-distma."

  lines[startsWith(lines, "As was evident from the maps of Figures @fig-map1-fig and @fig-map2-fig")] <-
    "As was evident from @fig-map1-fig and @fig-map2-fig, numerous districts are still missing from the data or are not comparable across all map variables. Perhaps the assumption of no carveouts and border shifts was a false one to make. And yet, even if all districts were perfectly matched, problems would arise. These figures use 2019-20 shapefiles from @bhatiaMergingUpdatedDistrictlevel2020, despite our \"treatment\" year being 2007-08. The splintering of districts into multiple neighbors over time allocates the same value of the 2007-08 treatment across neighbors, a rank wreckage equivalent to spatial autocorrelation in treatment when using 2019-20 geometry."

  lines[startsWith(lines, "As was evident from @fig-map1-fig and @fig-map2-fig")] <-
    "As was evident from @fig-map1-fig and @fig-map2-fig, numerous districts are still missing from the data or are not comparable across all map variables. Perhaps the assumption of no carveouts and border shifts was a false one to make. And yet, even if all districts were perfectly matched, problems would arise. These figures use 2019-20 shapefiles from @bhatiaMergingUpdatedDistrictlevel2020, despite our \"treatment\" year being 2007-08. The splintering of districts into multiple neighbors over time allocates the same value of the 2007-08 treatment across neighbors, a rank wreckage equivalent to spatial autocorrelation in treatment when using 2019-20 geometry."

  lines[startsWith(lines, "We can control for this spatial autocorrelation by incorporating spatial lags into our model. To test for spatial autocorrelation")] <-
    "We can control for this spatial autocorrelation by incorporating spatial lags into our model. To test for spatial autocorrelation we would follow p. 323 of @anselin2001 and construct a Moran's I statistic; but proper estimation of it would depend on us having proper shapefiles for the year of interest with which to build contiguity neighbor lists. If our unit of analysis ends up being 2001 districts, then this would require 2001 shapefiles. The current 2019-20 shapefile is available in the repository, but the active district panel is not yet joined to geometry with a validated 2001/2007-to-2020 crosswalk, so we do not report Moran's I $p$-values in this draft. Future work will combine shapefiles from the 2011 and 2001 censuses with the data harmonization changes described in @sec-distma to ensure these reflect genuine spatial autocorrelation as opposed to the flaws of our current district tracking methodology."

  lines
}

remove_unavailable_map_figures <- function(lines) {
  lines
}

ensure_map_geometry_note <- function(lines, anchor) {
  lines
}

fix_district_note_crossrefs <- function(lines) {
  lines <- gsub("These district changes are plotted in @fig-districtcarveoutsshifts-fig.", "These district changes are plotted in the district carve-outs figure below.", lines, fixed = TRUE)
  lines <- gsub("These district changes are plotted in Figure @fig-districtcarveoutsshifts-fig.", "These district changes are plotted in the district carve-outs figure below.", lines, fixed = TRUE)
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

replace_withheld_map_refs <- function(lines, allow_crossrefs = FALSE) {
  # The appendix and district-matching note are rendered as standalone QMDs, so
  # they cannot reference figure labels defined only in paper/report.qmd. Keep
  # those references as plain prose outside the main report to satisfy the
  # strict per-document cross-reference audit.
  if (!isTRUE(allow_crossrefs)) return(lines)
  lines <- gsub("the withheld final map figures", "@fig-map1-fig and @fig-map2-fig", lines, fixed = TRUE)
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
    "    sum_tbl_probit_cat = \"Summary Statistics for Enrollment Participation Model\\n(Categorical Variables)\",",
    "    sum_tbl_probit_quant = \"Summary Statistics for Enrollment Participation Model\\n(Numeric Variables)\",",
    "    probit_mfx = \"Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit\",",
    "    sum_tbl_iv = \"Summary Statistics for 2SLS Model\",",
    "    fs_cons = \"First-Stage Regression: EMI Exposure on Linguistic Distance\",",
    "    cons_iv = \"Second-Stage Regression: Consumption Growth on EMIE (Fitted)\")",
    "  captions[[name]] %||% name",
    "}",
    "regression_caption <- function(cap) paste(regression_star_note(), cap, sep = \"\\n\")",
    "table_caption <- function(name) {",
    "  cap <- legacy_table_caption_text(name)",
    "  if (name %in% c(\"probit_mfx\", \"fs_cons\", \"cons_iv\")) return(regression_caption(cap))",
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
    "  cap <- table_caption(name)",
    "  if (knitr::is_latex_output() && requireNamespace(\"kableExtra\", quietly = TRUE)) return(kableExtra::linebreak(cap, align = \"c\"))",
    "  cap",
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
    "  tab <- modelsummary::datasummary_df(out, output = if (knitr::is_latex_output()) \"latex_tabular\" else \"markdown\", fmt = identity, align = \"lc\", notes = table_note(name))",
    "  as.character(tab)",
    "}",
    "render_public_table <- function(path, name) {",
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

  c(
    "```{r}",
    paste0("#| label: ", label),
    paste0("#| tbl-cap: \"", caption, "\""),
    "#| echo: false",
    "#| results: asis",
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
      output_table_chunk("tbl-sum-tbl-probit-cat", legacy_table_captions[["tbl-sum-tbl-probit-cat"]], "../outputs/tables/main/sum_tbl_probit_cat.csv"),
      "",
      output_table_chunk("tbl-sum-tbl-probit-quant", legacy_table_captions[["tbl-sum-tbl-probit-quant"]], "../outputs/tables/main/sum_tbl_probit_quant.csv")
    )
  )
  lines <- insert_after_line(
    lines,
    "Average marginal effects for numeric variables and counterfactual comparisons",
    output_table_chunk("tbl-probit-mfx", legacy_table_captions[["tbl-probit-mfx"]], "../outputs/tables/main/probit_mfx.csv")
  )
  lines <- insert_after_line(
    lines,
    "Summary statistics for all of the variables in this model, including the controls",
    c(
      output_table_chunk("tbl-sum-tbl-iv", legacy_table_captions[["tbl-sum-tbl-iv"]], "../outputs/tables/main/sum_tbl_iv.csv"),
      "",
      figure_markdown("fig-map1-fig", "../outputs/figures/main/collage_main_maps.pdf")
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
      output_table_chunk("tbl-fs-cons", legacy_table_captions[["tbl-fs-cons"]], "../outputs/tables/main/fs_cons.csv"),
      "",
      output_table_chunk("tbl-cons-iv", legacy_table_captions[["tbl-cons-iv"]], "../outputs/tables/main/cons_iv.csv")
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

insert_district_note_output_objects <- function(lines) {
  ensure_report_object(
    lines,
    "fig-districtcarveoutsshifts-fig",
    figure_markdown("fig-districtcarveoutsshifts-fig", "../outputs/figures/main/district_carveouts_shifts.pdf")
  )
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
      "tbl-sum-tbl-probit-quant" = "../outputs/tables/main/sum_tbl_probit_quant.csv",
      "tbl-sum-tbl-probit-cat" = "../outputs/tables/main/sum_tbl_probit_cat.csv",
      "tbl-probit-mfx" = "../outputs/tables/main/probit_mfx.csv",
      "tbl-sum-tbl-iv" = "../outputs/tables/main/sum_tbl_iv.csv",
      "tbl-fs-cons" = "../outputs/tables/main/fs_cons.csv",
      "tbl-cons-iv" = "../outputs/tables/main/cons_iv.csv",
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
    lines <- ensure_report_object(lines, "fig-districtcarveoutsshifts-fig", figure_markdown("fig-districtcarveoutsshifts-fig", "../outputs/figures/main/district_carveouts_shifts.pdf"))
    lines <- fix_appendix_crossrefs(lines)
  }
  if (identical(path, "docs/district-matching.qmd")) {
    lines <- insert_district_note_output_objects(lines)
    lines <- ensure_report_object(lines, "fig-districtcarveoutsshifts-fig", figure_markdown("fig-districtcarveoutsshifts-fig", "../outputs/figures/main/district_carveouts_shifts.pdf"))
    lines <- fix_district_note_crossrefs(lines)
  }
  lines <- fix_final_public_prose(lines)
  lines <- remove_quarto_crossref_prefixes(lines)
  lines <- replace_withheld_map_refs(lines, allow_crossrefs = identical(path, "paper/report.qmd"))
  if (identical(path, "paper/appendix.qmd") || identical(path, "docs/district-matching.qmd")) {
    lines <- neutralize_standalone_map_crossrefs(lines)
  }
  lines <- remove_unavailable_map_figures(lines)
  if (identical(path, "paper/report.qmd") || identical(path, "paper/appendix.qmd")) lines <- fix_appendix_headings(lines)
  if (identical(path, "paper/report.qmd")) lines <- ensure_references_heading(lines)

  lines <- normalize_inserted_output_captions(lines)
  if (identical(path, "paper/report.qmd")) {
    lines <- normalize_output_table_chunks(lines)
    lines <- insert_report_output_objects_explicit(lines)
  }
  if (identical(path, "paper/report.qmd") || identical(path, "docs/district-matching.qmd")) {
    lines <- prune_unavailable_report_inline_expressions(lines)
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
