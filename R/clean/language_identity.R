# Shared language-label normalization for Census and linguistic-distance inputs.

normalize_language_label <- function(x) {
  tools::toTitleCase(tolower(trimws(plain_chr(x))))
}

normalize_census_language_label <- function(x) {
  label <- gsub("^\\s*[0-9]{1,3}\\s+", "", plain_chr(x))
  normalize_language_label(label)
}
