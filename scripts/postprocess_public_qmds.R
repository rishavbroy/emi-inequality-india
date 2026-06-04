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

insert_after_first <- function(lines, pattern, block) {
  hit <- grep(pattern, lines, fixed = TRUE)
  if (!length(hit)) return(lines)
  append(lines, block, after = hit[[1]])
}

insert_report_output_objects <- function(lines) {
  if (any(grepl("#\\|\\s*label:\\s*tbl-cons-iv", lines))) return(lines)

  lines <- insert_after_first(lines, "all while higher education has continued to develop an extremely strong, positive correlation with higher youth *unemployment*.", c(
    "",
    "![ILO labor market indicators composed from archived ILO figure assets.](../outputs/figures/main/fig_ilo_trends.png){#fig-ILO-fig}",
    ""
  ))

  lines <- insert_after_first(lines, "the average population of a district in either sample period (2007-08 and 2017-18) is 2 million.", c(
    "",
    output_table_chunk("tbl-sum-tbl-iv", "Summary statistics for 2SLS model variables", "../outputs/tables/main/sum_tbl_iv.csv"),
    ""
  ))

  lines <- insert_after_first(lines, "Summary statistics for numeric variables are given in Table @tbl-sum-tbl-probit-quant, and for categorical variables in Table @tbl-sum-tbl-probit-cat.", c(
    "",
    output_table_chunk("tbl-sum-tbl-probit-quant", "Summary statistics for enrollment participation model numeric variables", "../outputs/tables/main/sum_tbl_probit_quant.csv"),
    "",
    output_table_chunk("tbl-sum-tbl-probit-cat", "Summary statistics for enrollment participation model categorical variables", "../outputs/tables/main/sum_tbl_probit_cat.csv"),
    ""
  ))

  lines <- insert_after_first(lines, "Average marginal effects for numeric variables and counterfactual comparisons", c(
    "",
    output_table_chunk("tbl-probit-mfx", "Average marginal effects from the education participation probit", "../outputs/tables/main/probit_mfx.csv"),
    ""
  ))

  lines <- insert_after_first(lines, "Maps of these variables are presented in Figures @fig-map1-fig and @fig-map2-fig;", c(
    "",
    "![Main district-level map inputs.](../outputs/figures/main/collage_main_maps.png){#fig-map1-fig}",
    "",
    "![Instrument and region map inputs.](../outputs/figures/main/collage_iv_region_maps.png){#fig-map2-fig}",
    ""
  ))

  lines <- insert_after_first(lines, "The results of our first-stage regression, of EMI exposure on linguistic distance, are provided in Table @tbl-fs-cons.", c(
    "",
    output_table_chunk("tbl-fs-cons", "First-stage regression results", "../outputs/tables/main/fs_cons.csv"),
    "",
    output_table_chunk("tbl-cons-iv", "Second-stage 2SLS consumption regression results", "../outputs/tables/main/cons_iv.csv"),
    ""
  ))

  lines <- insert_after_first(lines, "These district changes are plotted in Figure @fig-districtcarveoutsshifts-fig.", c(
    "",
    "![District carve-outs, shifts, and non-partitions.](../outputs/figures/main/district_carveouts_shifts.png){#fig-districtcarveoutsshifts-fig}",
    ""
  ))

  lines
}

insert_district_note_output_objects <- function(lines) {
  if (any(grepl("#fig-map1-fig", lines, fixed = TRUE))) return(lines)

  lines <- insert_after_first(lines, "These district changes are plotted in Figure @fig-districtcarveoutsshifts-fig.", c(
    "",
    "![District carve-outs, shifts, and non-partitions.](../outputs/figures/main/district_carveouts_shifts.png){#fig-districtcarveoutsshifts-fig}",
    ""
  ))

  lines <- insert_after_first(lines, "As was evident from the maps of Figures @fig-map1-fig and @fig-map2-fig,", c(
    "",
    "![Main district-level map inputs.](../outputs/figures/main/collage_main_maps.png){#fig-map1-fig}",
    "",
    "![Instrument and region map inputs.](../outputs/figures/main/collage_iv_region_maps.png){#fig-map2-fig}",
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
  lines <- fix_equation_labels(lines)
  lines <- cleanup_public_placeholders(lines)
  if (identical(path, "paper/report.qmd")) lines <- insert_report_output_objects(lines)
  if (identical(path, "docs/district-matching.qmd")) {
    lines <- insert_district_note_output_objects(lines)
    lines <- fix_district_note_crossrefs(lines)
  }
  writeLines(lines, path)
  message("Postprocessed ", path)
}

for (path in qmd_paths[file.exists(qmd_paths)]) postprocess_one(path)
