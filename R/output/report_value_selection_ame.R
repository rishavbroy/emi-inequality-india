# Selection-summary and AME helpers for public report values.

lookup_ame <- function(ame_results, term_pattern, value_col = "estimate", multiply = 1, digits = NULL, contrast_pattern = NULL) {
  x <- as_plain_data_frame(ame_results)
  if (!nrow(x) || !"term" %in% names(x)) return(NA_real_)

  term_text <- as.character(x$term)
  contrast_text <- if ("contrast" %in% names(x)) as.character(x$contrast) else rep("", nrow(x))
  label_text <- if ("Term" %in% names(x)) as.character(x$Term) else rep("", nrow(x))

  keep <- grepl(term_pattern, term_text)
  if (!is.null(contrast_pattern)) {
    keep <- keep & (
      grepl(contrast_pattern, contrast_text, ignore.case = TRUE) |
        grepl(contrast_pattern, term_text, ignore.case = TRUE) |
        grepl(contrast_pattern, label_text, ignore.case = TRUE)
    )
  }

  x <- x[keep, , drop = FALSE]
  if (!nrow(x) || !value_col %in% names(x)) return(NA_real_)
  out <- suppressWarnings(as.numeric(x[[value_col]][[1]])) * multiply
  if (!is.null(digits) && is.finite(out)) out <- round(out, digits)
  out
}

lookup_ame_s_value <- function(ame_results, term_pattern, contrast_pattern = NULL, digits = NULL) {
  out <- lookup_ame(ame_results, term_pattern, value_col = "s.value", contrast_pattern = contrast_pattern)
  if (is.finite(out)) {
    if (!is.null(digits)) out <- signif(out, digits)
    return(out)
  }
  p <- lookup_ame(ame_results, term_pattern, value_col = "p.value", contrast_pattern = contrast_pattern)
  if (!is.finite(p) || p <= 0) return(NA_real_)
  out <- -log2(p)
  if (!is.null(digits)) out <- signif(out, digits)
  out
}

selection_summary_value <- function(selection_data, relation_to_head, sex, summary = c("mean_age", "share_children")) {
  summary <- match.arg(summary)
  x <- as_plain_data_frame(selection_data)
  relation_col <- first_col(x, c("RELATION_TO_HEAD", "relation_to_head"))
  sex_col <- first_col(x, c("SEX", "sex"))
  age_col <- first_col(x, c("AGE", "age"))
  if (is.null(relation_col) || is.null(sex_col) || is.null(age_col)) return(NA_real_)

  sex_raw <- as.character(x[[sex_col]])
  sex_num <- suppressWarnings(as.numeric(sex_raw))
  sex_keep <- sex_num == sex | (sex == 1 & tolower(sex_raw) == "male") | (sex == 2 & tolower(sex_raw) == "female")
  keep <- suppressWarnings(as.numeric(as.character(x[[relation_col]])) == relation_to_head) &
    sex_keep
  ages <- suppressWarnings(as.numeric(as.character(x[[age_col]][keep])))
  ages <- ages[is.finite(ages)]
  if (!length(ages)) return(NA_real_)
  if (identical(summary, "mean_age")) return(round(mean(ages), 1))
  round(mean(ages <= 17), 3)
}
