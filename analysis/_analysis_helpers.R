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

analysis_deviation_note <- function(text) {
  cat("\n**Deviation note.** ", text, "\n\n", sep = "")
}

analysis_store_path <- function() analysis_path("_targets")

analysis_read_target <- function(name) {
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop("Package targets is required to read analysis target output: ", name, call. = FALSE)
  }
  root <- analysis_project_root()
  store <- file.path(root, "_targets")
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(root)
  if ("tar_read_raw" %in% getNamespaceExports("targets")) {
    return(targets::tar_read_raw(name, store = store))
  }
  eval(substitute(targets::tar_read(TARGET, store = STORE), list(TARGET = as.name(name), STORE = store)))
}

analysis_target_manifest <- function(name) {
  out <- tryCatch(analysis_read_target(name), error = function(e) {
    data.frame(path = character(), description = character(), reason = conditionMessage(e), stringsAsFactors = FALSE)
  })
  if (is.character(out)) {
    return(data.frame(path = out, description = basename(out), stringsAsFactors = FALSE))
  }
  if (is.data.frame(out) && "path" %in% names(out)) return(out)
  data.frame(path = character(), description = character(), stringsAsFactors = FALSE)
}

analysis_target_path <- function(target, filename = NULL) {
  manifest <- analysis_target_manifest(target)
  if (!nrow(manifest) || !"path" %in% names(manifest)) return(NA_character_)
  paths <- as.character(manifest$path)
  if (!is.null(filename)) {
    normalized_paths <- gsub("\\\\", "/", paths)
    hit <- basename(normalized_paths) == filename | endsWith(normalized_paths, paste0("/", filename))
    paths <- paths[hit]
  }
  if (!length(paths)) return(NA_character_)
  paths[[1]]
}

analysis_target_csv <- function(target, filename) {
  path <- analysis_target_path(target, filename)
  if (is.na(path)) {
    return(data.frame(note = paste("Target output not found:", target, filename), stringsAsFactors = FALSE))
  }
  full <- if (file.exists(path)) path else analysis_path(path)
  read_analysis_csv(sub("^outputs/", "", analysis_rel_path(full)))
}

analysis_value <- function(df, row = 1L, column, default = NA) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df) || !column %in% names(df) || row > nrow(df)) return(default)
  value <- df[[column]][[row]]
  if (is.na(value)) default else value
}

analysis_metric <- function(df, key_column, key, value_column, default = NA) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df) || !all(c(key_column, value_column) %in% names(df))) return(default)
  hit <- which(as.character(df[[key_column]]) == key)
  if (!length(hit)) return(default)
  value <- df[[value_column]][[hit[[1]]]]
  if (is.na(value)) default else value
}

analysis_format_number <- function(x, digits = 3) {
  if (length(x) != 1L || is.na(x)) return("NA")
  if (is.numeric(x)) return(format(round(x, digits), big.mark = ",", trim = TRUE, scientific = FALSE))
  as.character(x)
}

analysis_rel_to_current <- function(path) {
  full <- if (file.exists(path)) normalizePath(path, winslash = "/", mustWork = FALSE) else normalizePath(analysis_path(path), winslash = "/", mustWork = FALSE)
  current <- tryCatch(knitr::current_input(), error = function(e) NULL)
  if (is.null(current) || !nzchar(current)) return(analysis_rel_path(full))
  base <- normalizePath(dirname(current), winslash = "/", mustWork = FALSE)
  from <- strsplit(base, "/", fixed = TRUE)[[1]]
  to <- strsplit(full, "/", fixed = TRUE)[[1]]
  while (length(from) && length(to) && identical(from[[1]], to[[1]])) {
    from <- from[-1]
    to <- to[-1]
  }
  parts <- c(rep("..", length(from)), to)
  if (!length(parts)) return(".")
  rel <- do.call(file.path, as.list(parts))
  if (!length(rel) || !nzchar(rel[[1]])) "." else rel[[1]]
}

analysis_image <- function(target, filename, alt = filename) {
  path <- analysis_target_path(target, filename)
  if (is.na(path)) {
    cat("Missing image target output: `", target, "` / `", filename, "`.\n\n", sep = "")
    return(invisible(NULL))
  }
  cat("![", alt, "](", analysis_rel_to_current(path), ")\n\n", sep = "")
}

analysis_top_rows <- function(df, n = 10L) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df)) return(df)
  utils::head(df, n)
}
