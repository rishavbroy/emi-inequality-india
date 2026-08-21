# This file is part of the EMI inequality research pipeline.

validate_glottolog_genealogy <- function(languoids) {
  if (!nrow(languoids) || anyDuplicated(languoids$id)) {
    stop("Glottolog genealogy must contain unique languoid IDs.", call. = FALSE)
  }
  parent <- languoids$parent_id[nzchar(languoids$parent_id)]
  missing_parent <- setdiff(unique(parent), languoids$id)
  if (length(missing_parent)) {
    stop("Glottolog genealogy contains unresolved parent IDs.", call. = FALSE)
  }
  if (any(!languoids$level %in% c("family", "language", "dialect"))) {
    stop("Glottolog genealogy contains an unsupported languoid level.", call. = FALSE)
  }

  parent_by_id <- stats::setNames(languoids$parent_id, languoids$id)
  for (id in languoids$id) {
    seen <- character()
    node <- id
    while (nzchar(node)) {
      if (node %in% seen) {
        stop("Glottolog genealogy contains a parent cycle at ", id, ".", call. = FALSE)
      }
      seen <- c(seen, node)
      node <- parent_by_id[[node]] %||% ""
    }
  }
  invisible(TRUE)
}

# Return the endpoint-to-root lineage only when it is genealogically usable.
# Bookkeeping branches are administrative placeholders, not linguistic ancestry.
glottolog_lineage <- function(glottocode, languoids) {
  id <- trimws(plain_chr(glottocode))
  if (length(id) != 1L || is.na(id) || !nzchar(id) || !id %in% languoids$id) {
    return(character())
  }

  parent_by_id <- stats::setNames(languoids$parent_id, languoids$id)
  path <- character()
  node <- id
  while (nzchar(node)) {
    row <- languoids[match(node, languoids$id), , drop = FALSE]
    if ("bookkeeping" %in% names(row) && isTRUE(row$bookkeeping[[1]])) return(character())
    path <- c(path, node)
    node <- parent_by_id[[node]] %||% ""
  }
  path
}

#' Resolve a Glottolog dialect or language to its language-level node
#'
#' Family nodes and any descendants of bookkeeping branches are not valid
#' Census-language endpoints.
glottolog_language_node <- function(glottocode, languoids) {
  path <- glottolog_lineage(glottocode, languoids)
  if (!length(path)) return(NA_character_)
  levels <- languoids$level[match(path, languoids$id)]
  language <- which(levels == "language")
  if (!length(language)) return(NA_character_)
  path[[language[[1]]]]
}

glottolog_ancestor_path <- function(glottocode, languoids) {
  path <- glottolog_lineage(glottocode, languoids)
  if (!length(path)) return(character())
  levels <- languoids$level[match(path, languoids$id)]
  language <- which(levels == "language")
  if (!length(language)) return(character())
  path[language[[1]]:length(path)]
}

#' Unweighted Glottolog tree distance between two language-level nodes
#'
#' Languages in separate top-level families are connected through one synthetic
#' super-root, matching the cross-family convention used for the project's
#' Glottolog robustness measure.
glottolog_edge_distance <- function(a, b, languoids) {
  path_a <- glottolog_ancestor_path(a, languoids)
  path_b <- glottolog_ancestor_path(b, languoids)
  if (!length(path_a) || !length(path_b)) return(NA_real_)
  if (identical(path_a[[1]], path_b[[1]])) return(0)

  common <- intersect(path_a, path_b)
  if (length(common)) {
    lca <- common[[1]]
    return(match(lca, path_a) - 1L + match(lca, path_b) - 1L)
  }

  # Each top-level family root is one edge from the synthetic super-root.
  length(path_a) + length(path_b)
}


read_census_language_glottolog_crosswalk <- function(path = NULL) {
  if (is.null(path)) {
    root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
    path <- file.path(root, "data", "metadata", "census_language_glottolog_crosswalk.csv")
  }
  if (!file.exists(path)) stop("Missing Census-Glottolog crosswalk: ", path, call. = FALSE)
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "mother_tongue_code", "mother_tongue", "canonical_language",
    "language_glottocode", "family_id", "match_basis", "review_status"
  )
  if (!all(required %in% names(out))) stop("Census-Glottolog crosswalk has an invalid schema.", call. = FALSE)
  out$mother_tongue_code <- sprintf("%06d", suppressWarnings(as.integer(out$mother_tongue_code)))
  if (anyDuplicated(out$mother_tongue_code)) stop("Census-Glottolog crosswalk has duplicate mother-tongue codes.", call. = FALSE)
  accepted <- grepl("^accepted_", plain_chr(out$review_status))
  if (any(accepted & !nzchar(trimws(plain_chr(out$language_glottocode))))) {
    stop("Accepted Census-Glottolog rows must contain a Glottocode.", call. = FALSE)
  }
  out
}

validate_census_glottolog_crosswalk <- function(crosswalk, languoids) {
  accepted <- grepl("^accepted_", plain_chr(crosswalk$review_status))
  codes <- plain_chr(crosswalk$language_glottocode[accepted])
  missing <- setdiff(unique(codes), languoids$id)
  if (length(missing)) stop("Accepted Census-Glottolog rows contain unknown Glottocodes.", call. = FALSE)
  invalid <- codes[!vapply(codes, function(code) length(glottolog_ancestor_path(code, languoids)) > 0L, logical(1))]
  if (length(invalid)) stop("Accepted Census-Glottolog rows contain invalid genealogy endpoints.", call. = FALSE)
  invisible(TRUE)
}

glottolog_distance_from_hindi <- function(glottocode, languoids, hindi = "hind1269") {
  vapply(
    plain_chr(glottocode),
    function(code) {
      if (is.na(code) || !nzchar(code)) return(NA_real_)
      glottolog_edge_distance(code, hindi, languoids)
    },
    numeric(1)
  )
}

attach_glottolog_language_distance <- function(census_2001_languages, glottolog, crosswalk) {
  rows <- safe_df(census_2001_languages)
  if (!"mother_tongue_code" %in% names(rows)) {
    stop("Glottolog distance attachment requires Census mother_tongue_code.", call. = FALSE)
  }
  validate_census_glottolog_crosswalk(crosswalk, glottolog$languoids)
  code <- sprintf("%06d", suppressWarnings(as.integer(rows$mother_tongue_code)))
  index <- match(code, crosswalk$mother_tongue_code)
  accepted <- !is.na(index) & grepl("^accepted_", plain_chr(crosswalk$review_status[index]))
  rows$glottolog_language_glottocode <- NA_character_
  rows$glottolog_family_id <- NA_character_
  rows$glottolog_language_glottocode[accepted] <- plain_chr(crosswalk$language_glottocode[index[accepted]])
  rows$glottolog_family_id[accepted] <- plain_chr(crosswalk$family_id[index[accepted]])
  rows$glottolog_edge_distance <- glottolog_distance_from_hindi(
    rows$glottolog_language_glottocode,
    glottolog$languoids
  )
  rows
}

glottolog_language_distance_table <- function(crosswalk, glottolog) {
  validate_census_glottolog_crosswalk(crosswalk, glottolog$languoids)
  out <- crosswalk
  accepted <- grepl("^accepted_", plain_chr(out$review_status))
  out$glottolog_edge_distance_from_hindi <- NA_real_
  out$glottolog_edge_distance_from_hindi[accepted] <- glottolog_distance_from_hindi(
    out$language_glottocode[accepted],
    glottolog$languoids
  )
  out
}

save_glottolog_language_distance_table <- function(
  x,
  path = "outputs/diagnostics/extended/instrument_relevance/glottolog_linguistic_distance.csv"
) {
  write_diagnostic_csv(x, path)
}
