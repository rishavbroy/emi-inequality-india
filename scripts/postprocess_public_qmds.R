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
  "fig-map1-fig" = "(Clockwise from top left) EMI exposure, consumption growth, pucca (permanent) housing, and household heads with secondary education or more. Data from the 64th round of the NSS 2007-08, ``Participation and Expenditure in Education'' and ``Household Consumer Expenditure.''",
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
  paste0("![", legacy_figure_captions[[label]], "](", path, "){#", label, "}")
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

normalize_yaml <- function(lines, path) {
  if (grepl("^paper/", path)) {
    lines <- rewrite_yaml_field(lines, "bibliography", "references.bib")
  }
  if (identical(path, "paper/report.qmd")) {
    lines <- rewrite_yaml_field(lines, "abstract", paste0('"', legacy_abstract, '"'))
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
    "Geospatial data intended for maps and spatial autocorrelation measures is sourced from @bhatiaMergingUpdatedDistrictlevel2020, which is itself an adaptation of @meyersIndiaOfficialBoundaries2020. The active figures below use district-level empirical distributions while the geometry join remains under validation. Our methods for tracking districts across time (see Sec. @sec-distma) begin with data from @indiastatestoriesDistrictEvolution2024 and @jaacksIndiaDistrictChanges2020."

  lines[startsWith(lines, "Looking deeper into the supply side of education, it seems that all variation which could potentially be explained by enrollment cost")] <-
    "Looking deeper into the supply side of education, it seems that all variation which could potentially be explained by enrollment cost has, at best, been consumed by other variables, implying direct costs matter less than social barriers and other correlates of supply-side factors. While one would expect the causal effect of an exogenous shock in scholarships/stipends, stationery, or textbooks to match the sign of their positive coefficient estimates, only textbooks show up as statistically significant. The district-level share of students who attend a school which charges no tuition fees (i.e., where 'Educ. freely available') is collinear with the intercept in the active probit specification, so we do not report its AME in this draft. @nationalsamplesurveyoffice2008 documentation indicates that such schools include most if not all government schools, as well as private schools in some states up to a certain level of education. Building off observations in Sec. @sec-intro that government schools tend to have worse facilities, chronic teacher absenteeism, and so on, this omitted coefficient remains a useful warning about the difficulty of separating direct costs from the quality and availability of public schooling. Comparing the remaining variables' $s$-values^[Given a $p$-value, we can define the $s$-value as $s=-\\log_2(p)$ [@mansourniaPvalueCompatibilitySvalue2022].] using @mansourniaPvalueCompatibilitySvalue2022 shows that 'Textbook(s) received' has an $s$-value of `r report_value(\"inline_55014f4e\")`, meaning that the data provided `r report_value(\"inline_55014f4e\")` bits of information against the null hypothesis (a coefficient of zero)."

  lines[startsWith(lines, "Summary statistics for all of the variables in this model, including the controls")] <-
    "Summary statistics for all of the variables in this model, including the controls $k$ in the vector $X_{kd}$, are provided in Table @tbl-sum-tbl-iv. Distribution figures for these variables are presented in Figures @fig-map1-fig and @fig-map2-fig; the missing geometry join discussed in Sec. @sec-distma-spa is the product of a data harmonization method which performed many-to-many matching from 2001 to 2007-08 to 2017-18 to 2019-20, the years our shapefiles data was collected [@bhatiaMergingUpdatedDistrictlevel2020]. Issues of and improvements to this method are also discussed in Sec. @sec-distma-spa."

  lines[startsWith(lines, "As was evident from the maps of Figures @fig-map1-fig and @fig-map2-fig")] <-
    "As was evident from Figures @fig-map1-fig and @fig-map2-fig, numerous districts are still missing from the data. Perhaps the assumption of no carveouts and border shifts was a false one to make. And yet, even if all districts were perfectly matched, problems would arise. The geographic version of these figures would use 2019-20 shapefiles from @bhatiaMergingUpdatedDistrictlevel2020, despite our \"treatment\" year being 2007-08. The splintering of districts into multiple neighbors over time allocates the same value of the 2007-08 treatment across neighbors, a rank wreckage equivalent to spatial autocorrelation in treatment when using 2019-20 geometry."

  lines[startsWith(lines, "We can control for this spatial autocorrelation by incorporating spatial lags into our model. To test for spatial autocorrelation")] <-
    "We can control for this spatial autocorrelation by incorporating spatial lags into our model. To test for spatial autocorrelation we would follow p. 323 of @anselin2001 and construct a Moran's I statistic; but proper estimation of it would depend on us having proper shapefiles for the year of interest with which to build contiguity neighbor lists. If our unit of analysis ends up being 2001 districts, then this would require 2001 shapefiles. The current 2019-20 shapefile is available in the repository, but the active district panel is not yet joined to geometry with a validated 2001/2007-to-2020 crosswalk, so we do not report Moran's I $p$-values in this draft. Future work will combine shapefiles from the 2011 and 2001 censuses with the data harmonization changes described in @sec-distma to ensure these reflect genuine spatial autocorrelation as opposed to the flaws of our current district tracking methodology."

  lines
}

output_table_chunk <- function(label, caption, path) {
  c(
    "```{r}",
    paste0("#| label: ", label),
    paste0("#| tbl-cap: \"", caption, "\""),
    "#| echo: false",
    "output_table <- function(path) {",
    "  if (file.exists(path)) return(utils::read.csv(path, check.names = FALSE))",
    "  data.frame(status = \"missing generated output\", path = path)",
    "}",
    paste0("knitr::kable(output_table(\"", path, "\"), digits = 3)"),
    "```"
  )
}

normalize_inserted_output_captions <- function(lines) {
  for (label in names(legacy_figure_captions)) {
    idx <- grep(paste0("\\{#", label, "\\}"), lines, fixed = FALSE)
    if (!length(idx)) next
    for (i in idx) {
      path <- sub("^!\\[[^]]*\\]\\(([^)]*)\\)\\{#[^}]+\\}.*$", "\\1", lines[[i]], perl = TRUE)
      if (!identical(path, lines[[i]])) {
        lines[[i]] <- figure_markdown(label, path)
      }
    }
  }

  for (label in names(legacy_table_captions)) {
    idx <- grep(paste0("^#\\|\\s*label:\\s*", label, "\\s*$"), lines, perl = TRUE)
    if (!length(idx)) next
    for (i in idx) {
      cap_idx <- which(seq_along(lines) > i & grepl("^#\\|\\s*tbl-cap:", lines, perl = TRUE))
      if (!length(cap_idx)) next
      lines[[cap_idx[[1]]]] <- paste0("#| tbl-cap: \"", legacy_table_captions[[label]], "\"")
    }
  }
  lines
}

insert_after_first <- function(lines, pattern, block) {
  hit <- grep(pattern, lines, fixed = TRUE)
  if (!length(hit)) return(lines)
  append(lines, block, after = hit[[1]])
}

insert_report_output_objects <- function(lines) {
  if (any(grepl("#\\|\\s*label:\\s*tbl-cons-iv", lines))) return(lines)

  lines <- insert_after_first(lines, "all while higher education has continued to develop an extremely strong, positive correlation with higher youth *unemployment*.", c(
    "",
    figure_markdown("fig-ILO-fig", "../outputs/figures/main/fig_ilo_trends.png"),
    ""
  ))

  lines <- insert_after_first(lines, "the average population of a district in either sample period (2007-08 and 2017-18) is 2 million.", c(
    "",
    output_table_chunk("tbl-sum-tbl-iv", legacy_table_captions[["tbl-sum-tbl-iv"]], "../outputs/tables/main/sum_tbl_iv.csv"),
    ""
  ))

  lines <- insert_after_first(lines, "Summary statistics for numeric variables are given in Table @tbl-sum-tbl-probit-quant, and for categorical variables in Table @tbl-sum-tbl-probit-cat.", c(
    "",
    output_table_chunk("tbl-sum-tbl-probit-quant", legacy_table_captions[["tbl-sum-tbl-probit-quant"]], "../outputs/tables/main/sum_tbl_probit_quant.csv"),
    "",
    output_table_chunk("tbl-sum-tbl-probit-cat", legacy_table_captions[["tbl-sum-tbl-probit-cat"]], "../outputs/tables/main/sum_tbl_probit_cat.csv"),
    ""
  ))

  lines <- insert_after_first(lines, "Average marginal effects for numeric variables and counterfactual comparisons", c(
    "",
    output_table_chunk("tbl-probit-mfx", legacy_table_captions[["tbl-probit-mfx"]], "../outputs/tables/main/probit_mfx.csv"),
    ""
  ))

  lines <- insert_after_first(lines, "Maps of these variables are presented in Figures @fig-map1-fig and @fig-map2-fig;", c(
    "",
    figure_markdown("fig-map1-fig", "../outputs/figures/main/collage_main_maps.png"),
    "",
    figure_markdown("fig-map2-fig", "../outputs/figures/main/collage_iv_region_maps.png"),
    ""
  ))

  lines <- insert_after_first(lines, "The results of our first-stage regression, of EMI exposure on linguistic distance, are provided in Table @tbl-fs-cons.", c(
    "",
    output_table_chunk("tbl-fs-cons", legacy_table_captions[["tbl-fs-cons"]], "../outputs/tables/main/fs_cons.csv"),
    "",
    output_table_chunk("tbl-cons-iv", legacy_table_captions[["tbl-cons-iv"]], "../outputs/tables/main/cons_iv.csv"),
    ""
  ))

  lines <- insert_after_first(lines, "These district changes are plotted in Figure @fig-districtcarveoutsshifts-fig.", c(
    "",
    figure_markdown("fig-districtcarveoutsshifts-fig", "../outputs/figures/main/district_carveouts_shifts.png"),
    ""
  ))

  lines
}

insert_district_note_output_objects <- function(lines) {
  if (any(grepl("#fig-map1-fig", lines, fixed = TRUE))) return(lines)

  lines <- insert_after_first(lines, "These district changes are plotted in Figure @fig-districtcarveoutsshifts-fig.", c(
    "",
    figure_markdown("fig-districtcarveoutsshifts-fig", "../outputs/figures/main/district_carveouts_shifts.png"),
    ""
  ))

  lines <- insert_after_first(lines, "As was evident from the maps of Figures @fig-map1-fig and @fig-map2-fig,", c(
    "",
    figure_markdown("fig-map1-fig", "../outputs/figures/main/collage_main_maps.png"),
    "",
    figure_markdown("fig-map2-fig", "../outputs/figures/main/collage_iv_region_maps.png"),
    ""
  ))

  lines
}

fix_district_note_crossrefs <- function(lines) {
  lines <- gsub("Sec\\. @sec-iv-iv", "the IV section of the main report", lines, perl = TRUE)
  lines <- gsub("Sec\\. @sec-intro", "the introduction of the main report", lines, perl = TRUE)
  lines
}

postprocess_one <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- normalize_yaml(lines, path)
  lines <- normalize_heading_labels(lines)
  lines <- convert_legacy_crossrefs(lines)
  lines <- remove_quarto_crossref_prefixes(lines)
  lines <- fix_equation_labels(lines)
  lines <- cleanup_public_placeholders(lines)
  if (identical(path, "paper/report.qmd")) lines <- insert_report_output_objects(lines)
  if (identical(path, "docs/district-matching.qmd")) {
    lines <- insert_district_note_output_objects(lines)
    lines <- fix_district_note_crossrefs(lines)
  }
  lines <- fix_final_public_prose(lines)
  lines <- remove_quarto_crossref_prefixes(lines)
  lines <- normalize_inserted_output_captions(lines)
  if (identical(path, "paper/report.qmd") || identical(path, "docs/district-matching.qmd")) {
    lines <- prune_unavailable_report_inline_expressions(lines)
  }
  writeLines(lines, path)
  message("Postprocessed ", path)
}

for (path in qmd_paths[file.exists(qmd_paths)]) postprocess_one(path)
