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

#' Resolve a Glottolog dialect or language to its language-level node
#'
#' Family nodes are not valid Census-language endpoints.
glottolog_language_node <- function(glottocode, languoids) {
  id <- trimws(plain_chr(glottocode))
  if (length(id) != 1L || is.na(id) || !nzchar(id) || !id %in% languoids$id) {
    return(NA_character_)
  }
  node <- id
  while (nzchar(node)) {
    row <- languoids[match(node, languoids$id), , drop = FALSE]
    if ("bookkeeping" %in% names(row) && isTRUE(row$bookkeeping[[1]])) return(NA_character_)
    if (identical(row$level[[1]], "language")) return(node)
    if (identical(row$level[[1]], "family")) return(NA_character_)
    node <- row$parent_id[[1]] %||% ""
  }
  NA_character_
}

glottolog_ancestor_path <- function(glottocode, languoids) {
  node <- glottolog_language_node(glottocode, languoids)
  if (is.na(node)) return(character())
  parent_by_id <- stats::setNames(languoids$parent_id, languoids$id)
  path <- character()
  while (nzchar(node)) {
    row <- languoids[match(node, languoids$id), , drop = FALSE]
    if ("bookkeeping" %in% names(row) && isTRUE(row$bookkeeping[[1]])) return(character())
    path <- c(path, node)
    node <- parent_by_id[[node]] %||% ""
  }
  path
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
