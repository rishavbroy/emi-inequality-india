# This file is part of the EMI inequality research pipeline.

crosswalk_direct_shastry_degree <- function(
  crosswalk,
  concordance = read_shastry_language_distance(),
  adjudications = read_shastry_language_adjudications()
) {
  resolve_shastry_language_degrees(crosswalk, concordance, adjudications)
}

ethnologue_proxy_plain_labels <- function(ethnologue_proxy) {
  tree <- plain_chr(ethnologue_proxy$indo_european_tree)
  quoted <- regmatches(tree, gregexpr("'(?:[^']|'')*'", tree, perl = TRUE))[[1]]
  if (!length(quoted)) return(character())
  labels <- substring(quoted, 2L, nchar(quoted) - 1L)
  labels <- gsub("''", "'", labels, fixed = TRUE)
  labels <- sub("\\s+\\[i-.*$", "", labels)
  unique(normalize_language_label(labels))
}

ethnologue_proxy_match_status <- function(mother_tongue, canonical_language, labels) {
  terms <- census_language_match_terms(mother_tongue, canonical_language)
  hit <- terms[terms$normalized_name %in% normalize_language_match_name(labels), , drop = FALSE]
  if (!nrow(hit)) return("not_found")
  if (any(hit$term_source == "mother_tongue")) return("exact_mother_tongue")
  if (any(hit$term_source == "mother_tongue_component")) return("exact_mother_tongue_component")
  "canonical_fallback"
}

build_shastry_extension_candidates <- function(
  crosswalk,
  glottolog,
  historical_linguistics,
  concordance = read_shastry_language_distance(),
  lexical_index = read_lexical_language_index(),
  adjudications = read_shastry_language_adjudications()
) {
  validate_census_glottolog_crosswalk(crosswalk, glottolog$languoids)
  accepted <- grepl("^accepted_", plain_chr(crosswalk$review_status))
  direct <- crosswalk_direct_shastry_degree(crosswalk, concordance, adjudications)
  reviewed <- plain_chr(crosswalk$mother_tongue_code) %in%
    plain_chr(adjudications$mother_tongue_code)
  targets <- crosswalk[accepted & !is.finite(direct) & !reviewed, , drop = FALSE]
  if (!nrow(targets)) return(data.frame())

  proxy_labels <- ethnologue_proxy_plain_labels(historical_linguistics$ethnologue_proxy)
  evidence <- lexical_language_evidence(
    targets$mother_tongue,
    historical_linguistics$dyen_hindi,
    lexical_index
  )
  asjp <- historical_linguistics$asjp_review_summary %||% NULL
  kogan <- historical_linguistics$kogan_anchor_similarity %||% NULL

  safe_bind_rows(lapply(seq_len(nrow(targets)), function(i) {
    row <- targets[i, , drop = FALSE]
    kogan_row <- kogan_anchor_summary(kogan, evidence$kogan_code[[i]])
    asjp_row <- asjp_review_summary_row(asjp, row$mother_tongue_code[[1]])
    data.frame(
      mother_tongue_code = row$mother_tongue_code,
      mother_tongue = row$mother_tongue,
      canonical_language = row$canonical_language,
      language_glottocode = row$language_glottocode,
      candidate_degree = NA_real_,
      candidate_basis = "figure6_lsi_lexical_review",
      ethnologue_proxy_status = ethnologue_proxy_match_status(
        row$mother_tongue[[1]], row$canonical_language[[1]], proxy_labels
      ),
      dyen_list_name = evidence$dyen_list_name[[i]],
      dyen_cognate_pct_hindi = evidence$dyen_cognate_pct_hindi[[i]],
      kogan_code = evidence$kogan_code[[i]],
      kogan_nearest_anchor = kogan_row$nearest_anchor[[1]],
      kogan_nearest_degree = kogan_row$nearest_degree[[1]],
      kogan_nearest_similarity = kogan_row$nearest_similarity[[1]],
      kogan_margin_pct = kogan_row$margin_pct[[1]],
      asjp_nearest_anchor = asjp_row$nearest_anchor[[1]],
      asjp_nearest_degree = asjp_row$nearest_degree[[1]],
      asjp_nearest_ldnd = asjp_row$nearest_ldnd[[1]],
      asjp_margin_ldnd = asjp_row$margin_ldnd[[1]],
      asjp_status = asjp_row$status[[1]],
      review_status = "review_required",
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
