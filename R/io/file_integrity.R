# File-integrity helpers shared by source preflight and source-specific QA.

sha256_file <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) return(NA_character_)
  if (!file.exists(path) || dir.exists(path)) return(NA_character_)
  need_pkg("digest", "SHA-256 source verification")
  tolower(digest::digest(file = path, algo = "sha256", serialize = FALSE))
}

sha256_files <- function(paths) {
  vapply(paths, sha256_file, character(1), USE.NAMES = FALSE)
}

normalize_sha256 <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

is_sha256 <- function(x) {
  x <- normalize_sha256(x)
  !is.na(x) & grepl("^[0-9a-f]{64}$", x)
}
