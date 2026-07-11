analysis_project_root <- function(start = getwd()) {
  here <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(here, "_targets.R")) && dir.exists(file.path(here, "analysis"))) {
      return(here)
    }
    parent <- dirname(here)
    if (identical(parent, here)) {
      stop("Could not locate project root from ", start, call. = FALSE)
    }
    here <- parent
  }
}

analysis_path <- function(...) file.path(analysis_project_root(), ...)
analysis_csv <- function(...) analysis_path("outputs", ...)
analysis_rel_path <- function(path) {
  root <- normalizePath(analysis_project_root(), winslash = "/", mustWork = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}


read_analysis_csv <- function(...) {
  path <- analysis_csv(...)
  rel <- analysis_rel_path(path)
  if (!file.exists(path)) {
    return(data.frame(note = paste("Missing analysis output:", rel), stringsAsFactors = FALSE))
  }

  if (file.info(path)$size <= 3L) {
    return(data.frame(note = paste("No rows in analysis output:", rel), stringsAsFactors = FALSE))
  }

  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      data.frame(
        note = paste("Could not read analysis output:", rel),
        reason = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    }
  )
}

analysis_table <- function(df, caption = NULL, digits = 3, max_rows = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(df)) df <- data.frame(note = "No rows in this diagnostic output.", stringsAsFactors = FALSE)
  if (!is.null(max_rows) && nrow(df) > max_rows) {
    note_row <- as.data.frame(as.list(rep("", ncol(df))), stringsAsFactors = FALSE)
    names(note_row) <- names(df)
    note_row[[1]] <- paste("Table truncated in rendered note; full CSV has", nrow(df), "rows.")
    df <- rbind(head(df, max_rows), note_row)
  }
  tab <- knitr::kable(
    df,
    caption = caption,
    digits = digits,
    booktabs = knitr::is_latex_output(),
    longtable = knitr::is_latex_output(),
    row.names = FALSE,
    linesep = ""
  )
  if (knitr::is_latex_output() && requireNamespace("kableExtra", quietly = TRUE)) {
    tab <- kableExtra::kable_styling(tab, latex_options = c("striped", "repeat_header"), full_width = FALSE, font_size = 9)
  }
  tab
}

analysis_legacy_chunk_path <- function(filename) {
  analysis_path("archive", "legacy-rmd-chunks", filename)
}

analysis_legacy_comment_lines <- function(filename, from = NULL, to = NULL) {
  path <- analysis_legacy_chunk_path(filename)
  if (!file.exists(path)) {
    return(paste("Missing legacy chunk:", analysis_rel_path(path)))
  }
  lines <- readLines(path, warn = FALSE)
  idx <- seq_along(lines)
  if (!is.null(from)) idx <- idx[idx >= from]
  if (!is.null(to)) idx <- idx[idx <= to]
  lines <- lines[idx]
  keep <- grepl("^\\s*#", lines) | !nzchar(trimws(lines))
  lines <- lines[keep]
  lines <- sub("^\\s*# ?", "", lines)
  while (length(lines) && !nzchar(trimws(lines[[1]]))) lines <- lines[-1]
  while (length(lines) && !nzchar(trimws(lines[[length(lines)]]))) lines <- lines[-length(lines)]
  lines
}

analysis_is_code_like <- function(x) {
  z <- trimws(x)
  if (!nzchar(z)) return(FALSE)
  grepl("(<-|%>%|\\|>|::|:::)" , z) ||
    grepl("^(library|require|source|set\\.|[A-Za-z0-9_.]+\\s*<-|[A-Za-z0-9_.]+\\s*\\(|[A-Za-z0-9_.]+\\s*\\+|\\+|\\)|\\}|\\]|if\\s*\\(|else\\b|for\\s*\\(|function\\s*\\(|ggplot\\(|aes\\(|geom_|labs\\(|theme_|scale_|mutate\\(|filter\\(|select\\(|summari[sz]e\\(|group_by\\(|arrange\\(|View\\b|chisq\\.test\\(|t\\.test\\(|map_df\\(|bind_rows\\(|full_join\\(|left_join\\(|anti_join\\(|inner_join\\(|coalesce\\(|case_when\\(|tribble\\(|data\\.frame\\(|read_|write_)" , z)
}

analysis_flush_legacy_block <- function(lines, code) {
  if (!length(lines)) return(invisible(NULL))
  if (code) {
    cat("```r\n", paste(lines, collapse = "\n"), "\n```\n\n", sep = "")
  } else {
    # Avoid accidental fenced-div parsing in GitHub/Pandoc when legacy prose uses :::.
    lines <- gsub(":::", "\\\\:\\\\:\\\\:", lines, fixed = TRUE)
    cat(paste(lines, collapse = "\n"), "\n\n", sep = "")
  }
}

analysis_render_legacy_comments <- function(filename, from = NULL, to = NULL, caption = NULL) {
  if (!is.null(caption)) cat("\n### ", caption, "\n\n", sep = "")
  lines <- analysis_legacy_comment_lines(filename, from = from, to = to)
  if (!length(lines)) return(invisible(NULL))
  block <- character()
  block_code <- NA
  for (line in lines) {
    blank <- !nzchar(trimws(line))
    code <- analysis_is_code_like(line)
    if (blank) {
      analysis_flush_legacy_block(block, isTRUE(block_code))
      block <- character()
      block_code <- NA
      next
    }
    if (is.na(block_code)) block_code <- code
    if (!identical(code, block_code)) {
      analysis_flush_legacy_block(block, isTRUE(block_code))
      block <- character()
      block_code <- code
    }
    block <- c(block, line)
  }
  analysis_flush_legacy_block(block, isTRUE(block_code))
  invisible(NULL)
}

analysis_deviation_note <- function(text) {
  cat("\n**Deviation note.** ", text, "\n\n", sep = "")
}

analysis_render_source_file <- function(path, language = "r") {
  full <- if (file.exists(path)) path else analysis_path(path)
  cat("```", language, "\n", sep = "")
  cat(paste(readLines(full, warn = FALSE), collapse = "\n"), "\n", sep = "")
  cat("```\n\n")
}
