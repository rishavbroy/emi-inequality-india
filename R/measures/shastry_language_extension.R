# This file is part of the EMI inequality research pipeline.

crosswalk_direct_shastry_degree <- function(crosswalk, concordance = read_shastry_language_distance()) {
  linguistic_distance_degrees(
    crosswalk$mother_tongue,
    crosswalk$canonical_language,
    concordance
  )
}

shastry_anchor_table <- function(crosswalk, glottolog, concordance = read_shastry_language_distance()) {
  accepted <- grepl("^accepted_", plain_chr(crosswalk$review_status))
  degree <- crosswalk_direct_shastry_degree(crosswalk, concordance)
  out <- crosswalk[accepted & is.finite(degree), , drop = FALSE]
  out$shastry_degree <- degree[accepted & is.finite(degree)]
  out <- out[nzchar(plain_chr(out$language_glottocode)), , drop = FALSE]
  if (!nrow(out)) return(out)
  out <- out[!duplicated(out[c("language_glottocode", "shastry_degree")]), , drop = FALSE]
  out
}

nearest_shastry_anchors <- function(glottocode, anchors, languoids) {
  if (!nrow(anchors)) return(data.frame())
  distance <- vapply(
    anchors$language_glottocode,
    function(anchor) glottolog_edge_distance(glottocode, anchor, languoids),
    numeric(1)
  )
  finite <- is.finite(distance)
  if (!any(finite)) return(data.frame())
  minimum <- min(distance[finite])
  out <- anchors[finite & distance == minimum, , drop = FALSE]
  out$glottolog_edge_distance <- minimum
  out
}

build_shastry_extension_candidates <- function(
  crosswalk,
  glottolog,
  concordance = read_shastry_language_distance()
) {
  validate_census_glottolog_crosswalk(crosswalk, glottolog$languoids)
  accepted <- grepl("^accepted_", plain_chr(crosswalk$review_status))
  direct <- crosswalk_direct_shastry_degree(crosswalk, concordance)
  targets <- crosswalk[accepted & !is.finite(direct), , drop = FALSE]
  if (!nrow(targets)) return(data.frame())
  anchors <- shastry_anchor_table(crosswalk, glottolog, concordance)

  safe_bind_rows(lapply(seq_len(nrow(targets)), function(i) {
    row <- targets[i, , drop = FALSE]
    if (!identical(row$family_id[[1]], "indo1319")) {
      return(data.frame(
        mother_tongue_code = row$mother_tongue_code,
        mother_tongue = row$mother_tongue,
        canonical_language = row$canonical_language,
        language_glottocode = row$language_glottocode,
        candidate_degree = 5,
        candidate_basis = "shastry_non_indo_european_rule",
        nearest_anchor = NA_character_,
        nearest_anchor_degrees = NA_character_,
        glottolog_edge_distance = NA_real_,
        review_status = "rule_supported",
        stringsAsFactors = FALSE
      ))
    }

    nearest <- nearest_shastry_anchors(
      row$language_glottocode[[1]], anchors, glottolog$languoids
    )
    if (!nrow(nearest)) {
      return(data.frame(
        mother_tongue_code = row$mother_tongue_code,
        mother_tongue = row$mother_tongue,
        canonical_language = row$canonical_language,
        language_glottocode = row$language_glottocode,
        candidate_degree = NA_real_,
        candidate_basis = "glottolog_nearest_shastry_anchor",
        nearest_anchor = NA_character_,
        nearest_anchor_degrees = NA_character_,
        glottolog_edge_distance = NA_real_,
        review_status = "no_anchor",
        stringsAsFactors = FALSE
      ))
    }
    degrees <- sort(unique(num(nearest$shastry_degree)))
    data.frame(
      mother_tongue_code = row$mother_tongue_code,
      mother_tongue = row$mother_tongue,
      canonical_language = row$canonical_language,
      language_glottocode = row$language_glottocode,
      candidate_degree = if (length(degrees) == 1L) degrees[[1]] else NA_real_,
      candidate_basis = "glottolog_nearest_shastry_anchor",
      nearest_anchor = paste(sort(unique(plain_chr(nearest$mother_tongue))), collapse = ";"),
      nearest_anchor_degrees = paste(degrees, collapse = ";"),
      glottolog_edge_distance = nearest$glottolog_edge_distance[[1]],
      review_status = if (length(degrees) == 1L) "review_required" else "conflicting_nearest_degrees",
      stringsAsFactors = FALSE
    )
  }))
}

save_shastry_extension_candidates <- function(
  x,
  path = "outputs/diagnostics/extended/instrument_relevance/shastry_extension_candidates.csv"
) {
  write_diagnostic_csv(x, path)
}
