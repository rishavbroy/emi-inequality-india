# This file is part of the EMI inequality research pipeline.

#' Read and validate the versioned Glottolog 5.3 source bundle
#'
#' The direct languoid CSV is the canonical genealogy source used by the
#' project. The CLDF archive is retained for aliases and future reviewed Census
#' crosswalks; the Newick file is retained as an independent representation.
read_glottolog_5_3 <- function(paths = build_paths()) {
  rows <- rbind(
    require_manifest_files(paths, "glottolog_5_3"),
    require_manifest_files(paths, "glottolog_cldf_5_3")
  )
  path_for <- function(file_id) {
    hit <- rows$absolute_path[rows$file_id == file_id]
    if (length(hit) != 1L) {
      stop("Glottolog manifest must contain exactly one row for ", file_id, ".", call. = FALSE)
    }
    hit[[1]]
  }

  languoids <- read_glottolog_languoids(path_for("glottolog53_languoids"))
  cldf_metadata <- read_glottolog_cldf_metadata(path_for("glottolog53_cldf"))
  validate_glottolog_cldf_release(cldf_metadata)

  validate_glottolog_genealogy(languoids)
  hindi <- languoids[languoids$id == "hind1269", , drop = FALSE]
  if (nrow(hindi) != 1L || hindi$level[[1]] != "language" || hindi$iso639P3code[[1]] != "hin") {
    stop("Glottolog 5.3 Hindi anchor hind1269 is missing or malformed.", call. = FALSE)
  }

  list(
    version = "5.3",
    languoids = languoids,
    cldf_zip = path_for("glottolog53_cldf"),
    geo_path = path_for("glottolog53_geo"),
    tree_path = path_for("glottolog53_newick"),
    license = "CC-BY-4.0"
  )
}

read_glottolog_languoids <- function(path) {
  members <- utils::unzip(path, list = TRUE)$Name
  member <- members[basename(members) == "languoid.csv"]
  if (length(member) != 1L) {
    stop("Glottolog languoid archive must contain exactly one languoid.csv.", call. = FALSE)
  }
  out <- utils::read.csv(
    unz(path, member), stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA")
  )
  required <- c("id", "family_id", "parent_id", "name", "bookkeeping", "level", "iso639P3code")
  if (!all(required %in% names(out))) {
    stop("Glottolog languoid.csv has an invalid schema.", call. = FALSE)
  }

  clean_character <- function(x) {
    value <- trimws(plain_chr(x))
    value[is.na(value)] <- ""
    value
  }
  out$id <- clean_character(out$id)
  out$family_id <- clean_character(out$family_id)
  out$parent_id <- clean_character(out$parent_id)
  out$name <- clean_character(out$name)
  out$level <- tolower(clean_character(out$level))
  out$iso639P3code <- tolower(clean_character(out$iso639P3code))
  out
}

read_glottolog_cldf_metadata <- function(path) {
  members <- utils::unzip(path, list = TRUE)$Name
  member <- members[basename(members) == "metadata.json" & !grepl("/cldf/", members, fixed = TRUE)]
  if (length(member) != 1L) {
    stop("Glottolog CLDF archive must contain exactly one top-level metadata.json.", call. = FALSE)
  }
  need_pkg("jsonlite", "Glottolog CLDF metadata")
  jsonlite::fromJSON(
    paste(readLines(unz(path, member), warn = FALSE), collapse = "\n"),
    simplifyVector = TRUE
  )
}


validate_glottolog_cldf_release <- function(metadata) {
  if (!identical(as.character(metadata$title), "glottolog/glottolog: Glottolog database 5.3 as CLDF")) {
    stop("Glottolog CLDF archive does not identify itself as release 5.3.", call. = FALSE)
  }
  if (!identical(as.character(metadata$license), "CC-BY-4.0")) {
    stop("Glottolog CLDF archive does not declare the expected CC-BY-4.0 license.", call. = FALSE)
  }
  invisible(TRUE)
}

read_glottolog_cldf_table <- function(path, filename) {
  members <- utils::unzip(path, list = TRUE)$Name
  member <- members[basename(members) == filename & grepl("/cldf/", members, fixed = TRUE)]
  if (length(member) != 1L) {
    stop("Glottolog CLDF archive must contain exactly one ", filename, ".", call. = FALSE)
  }
  utils::read.csv(
    unz(path, member),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

validate_glottolog_cldf_languages <- function(x) {
  required <- c("ID", "Name", "Glottocode", "ISO639P3code", "Level", "Countries", "Family_ID", "Language_ID")
  if (!all(required %in% names(x))) {
    stop("Glottolog CLDF languages.csv has an invalid schema.", call. = FALSE)
  }
  character_columns <- c("ID", "Name", "Glottocode", "ISO639P3code", "Level", "Countries", "Family_ID", "Language_ID")
  for (nm in character_columns) {
    x[[nm]] <- trimws(plain_chr(x[[nm]]))
    x[[nm]][is.na(x[[nm]])] <- ""
  }
  x$ISO639P3code <- tolower(x$ISO639P3code)
  x$Level <- tolower(x$Level)
  if (anyDuplicated(x$ID) || any(!x$Level %in% c("family", "language", "dialect"))) {
    stop("Glottolog CLDF languages.csv contains invalid languoid identities.", call. = FALSE)
  }
  x
}

validate_glottolog_cldf_names <- function(x) {
  required <- c("ID", "Language_ID", "Name", "Provider")
  if (!all(required %in% names(x))) {
    stop("Glottolog CLDF names.csv has an invalid schema.", call. = FALSE)
  }
  for (nm in c("Language_ID", "Name", "Provider")) {
    x[[nm]] <- trimws(plain_chr(x[[nm]]))
    x[[nm]][is.na(x[[nm]])] <- ""
  }
  x
}

#' Read the CLDF lookup tables only when reviewed language matching is requested
read_glottolog_cldf_5_3 <- function(path) {
  metadata <- read_glottolog_cldf_metadata(path)
  validate_glottolog_cldf_release(metadata)
  list(
    languages = validate_glottolog_cldf_languages(read_glottolog_cldf_table(path, "languages.csv")),
    names = validate_glottolog_cldf_names(read_glottolog_cldf_table(path, "names.csv"))
  )
}
