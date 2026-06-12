# Utilities for extracting writing-sample excerpts from paper/report.qmd.
# Excerpts are marked in the report with fenced Divs like:
# ::: {.sample-excerpt #ws-intro-question-contribution sets="writing-5pg writing-10pg" order="1"}
# ...
# :::

#' Extract marked Quarto excerpts by ID
#'
#' @param source Path to a .qmd source file.
#' @param excerpt_ids Character vector of excerpt IDs to extract, in desired order.
#' @return Character vector of excerpt lines, with section breaks inserted between excerpts.
extract_qmd_excerpts <- function(source, excerpt_ids) {
  text <- readLines(source, warn = FALSE)
  blocks <- extract_marked_divs(text)
  validate_excerpt_ids(blocks, excerpt_ids)
  pieces <- lapply(excerpt_ids, function(id) {
    c("", paste0("<!-- excerpt: ", id, " -->"), blocks[[id]], "")
  })
  unlist(pieces, use.names = FALSE)
}

#' Parse sample-excerpt fenced Divs from Quarto lines
#'
#' @param text Character vector of lines.
#' @return Named list mapping excerpt IDs to excerpt lines.
extract_marked_divs <- function(text) {
  blocks <- list()
  i <- 1L
  n <- length(text)
  while (i <= n) {
    line <- text[[i]]
    if (grepl('^:::\\s*\\{', line) && grepl('\\.sample-excerpt', line)) {
      id <- sub('.*#([A-Za-z0-9_-]+).*', '\\1', line)
      if (identical(id, line) || is.na(id) || !nzchar(id)) {
        stop("sample-excerpt marker lacks an ID at line ", i, call. = FALSE)
      }
      start <- i + 1L
      j <- start
      depth <- 1L
      while (j <= n) {
        if (grepl('^:::\\s*$', text[[j]])) {
          depth <- depth - 1L
          if (depth == 0L) break
        } else if (grepl('^:::\\s*\\{', text[[j]])) {
          depth <- depth + 1L
        }
        j <- j + 1L
      }
      if (j > n) stop("Unclosed sample-excerpt marker: ", id, call. = FALSE)
      blocks[[id]] <- text[start:(j - 1L)]
      i <- j + 1L
    } else {
      i <- i + 1L
    }
  }
  blocks
}

strip_qmd_yaml <- function(lines) {
  if (length(lines) >= 2L && identical(lines[[1]], "---")) {
    close <- which(lines[-1L] == "---")
    if (length(close)) return(lines[-seq_len(close[[1]] + 1L)])
  }
  lines
}

extract_yaml_field <- function(lines, field) {
  if (!length(lines) || !identical(lines[[1]], "---")) return(NULL)
  close <- which(lines[-1L] == "---")
  if (!length(close)) return(NULL)
  end <- close[[1]] + 1L
  field_re <- paste0("^", field, ":\\s*")
  idx <- grep(field_re, lines[seq_len(end)], perl = TRUE)
  if (!length(idx)) return(NULL)
  value <- sub(field_re, "", lines[idx[[1]]], perl = TRUE)
  value <- sub('^"', "", sub('"$', "", value))
  value
}

report_abstract_block <- function(source_lines) {
  abstract <- extract_yaml_field(source_lines, "abstract")
  if (is.null(abstract) || !nzchar(abstract)) return(character())
  c("", "## Abstract", "", abstract, "")
}

extract_named_r_chunk <- function(source_lines, label) {
  start <- grep(paste0("^```\\{r\\s+", label, "(,|\\s|\\})"), source_lines, perl = TRUE)
  if (!length(start)) return(character())
  end <- grep("^```\\s*$", source_lines)
  end <- end[end > start[[1]]]
  if (!length(end)) stop("Unclosed setup chunk in report: ", label, call. = FALSE)
  source_lines[start[[1]]:end[[1]]]
}

report_setup_chunks <- function(source_lines) {
  labels <- c("report-target-values", "public-output-table-helper")
  chunks <- lapply(labels, extract_named_r_chunk, source_lines = source_lines)
  chunks <- chunks[lengths(chunks) > 0L]
  if (!length(chunks)) return(character())
  c(unlist(lapply(chunks, function(x) c(x, "")), use.names = FALSE), "")
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

ensure_yaml_list_item <- function(lines, field, value) {
  if (!length(lines) || !identical(lines[[1]], "---")) return(lines)
  close <- which(lines[-1L] == "---")
  if (!length(close)) return(lines)
  end <- close[[1]] + 1L
  field_re <- paste0("^", field, ":\\s*$")
  field_idx <- grep(field_re, lines[seq_len(end)], perl = TRUE)
  item <- paste0("  - ", value)
  if (any(lines[seq_len(end)] == item)) return(lines)
  if (!length(field_idx)) {
    lines <- append(lines, c(paste0(field, ":"), item), after = end - 1L)
    return(lines)
  }
  insert_after <- field_idx[[1]]
  while (insert_after + 1L <= end && grepl("^\\s+-\\s+", lines[[insert_after + 1L]], perl = TRUE)) {
    insert_after <- insert_after + 1L
  }
  append(lines, item, after = insert_after)
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

normalize_sample_yaml <- function(lines, bibliography = "../../paper/references.bib") {
  if (!any(grepl("^format:", lines))) {
    close <- which(lines[-1L] == "---")
    if (length(close)) {
      end <- close[[1]] + 1L
      lines <- append(lines, c("format:", "  pdf:", "    pdf-engine: xelatex"), after = end - 1L)
    }
  }
  if (!any(grepl("^    pdf-engine:", lines))) {
    pdf_idx <- grep("^  pdf:\\s*$", lines)
    if (length(pdf_idx)) lines <- append(lines, "    pdf-engine: xelatex", after = pdf_idx[[1]])
  }
  lines <- rewrite_yaml_field(lines, "bibliography", bibliography)
  lines <- ensure_yaml_field(lines, "link-citations", "true")
  for (pkg in c(
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{array}",
    "\\usepackage{multirow}",
    "\\usepackage{wrapfig}",
    "\\usepackage{float}",
    "\\usepackage{colortbl}",
    "\\usepackage{pdflscape}",
    "\\usepackage{tabu}",
    "\\usepackage{threeparttable}",
    "\\usepackage{threeparttablex}",
    "\\usepackage[normalem]{ulem}",
    "\\usepackage{makecell}",
    "\\usepackage{xcolor}"
  )) {
    lines <- ensure_yaml_list_item(lines, "header-includes", pkg)
  }
  lines
}

normalize_sample_resource_paths <- function(lines) {
  gsub("../outputs/", "../../outputs/", lines, fixed = TRUE)
}

#' Assemble a temporary writing-sample QMD
#'
#' @param cover_note Path to cover-note QMD.
#' @param excerpts Character vector of excerpt lines.
#' @param output_qmd Path to write assembled QMD.
#' @return `output_qmd` invisibly.
assemble_writing_sample_qmd <- function(cover_note, excerpts, output_qmd) {
  cover <- if (!is.null(cover_note) && file.exists(cover_note)) readLines(cover_note, warn = FALSE) else character()
  cover <- normalize_sample_yaml(cover)
  excerpts <- normalize_sample_resource_paths(excerpts)
  body <- c(cover, "", "\\newpage", "", excerpts)
  dir.create(dirname(output_qmd), recursive = TRUE, showWarnings = FALSE)
  writeLines(body, output_qmd)
  invisible(output_qmd)
}

#' Validate requested excerpt IDs
#'
#' @param blocks Named list returned by extract_marked_divs().
#' @param excerpt_ids Character vector of requested IDs.
#' @return TRUE invisibly.
validate_excerpt_ids <- function(blocks, excerpt_ids) {
  missing <- setdiff(excerpt_ids, names(blocks))
  if (length(missing) > 0L) {
    stop("Missing writing-sample excerpt IDs: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}
