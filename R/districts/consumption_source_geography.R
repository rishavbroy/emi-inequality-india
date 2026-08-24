# Source-geography dictionaries for registered household-consumption surveys.
# This module resolves survey-frame state/district codes to named source units.
# Census-2001 lineage allocation remains the responsibility of the lineage layer.

consumption_code_key <- function(x, width = 2L) {
  value <- trimws(plain_chr(x))
  numeric_value <- suppressWarnings(as.integer(value))
  out <- rep(NA_character_, length(value))
  ok_numeric <- !is.na(numeric_value)
  out[ok_numeric] <- sprintf(paste0("%0", as.integer(width), "d"), numeric_value[ok_numeric])
  out[!ok_numeric & nzchar(value)] <- value[!ok_numeric & nzchar(value)]
  out
}

consumption_codebook_column <- function(data, candidates, context) {
  keys <- gsub("[^a-z0-9]+", "", tolower(names(data)))
  wanted <- gsub("[^a-z0-9]+", "", tolower(candidates))
  hit <- which(keys %in% wanted)
  if (length(hit) != 1L) {
    stop(context, " must contain exactly one of: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  names(data)[hit]
}

normalize_consumption_codebook <- function(
    data,
    state_name_col,
    state_code_col,
    district_name_col,
    district_code_col,
    source_id) {
  x <- safe_df(data)
  require_consumption_columns(
    x,
    c(state_name_col, state_code_col, district_name_col, district_code_col),
    paste0(source_id, " district codebook")
  )

  state_code <- consumption_code_key(x[[state_code_col]], 2L)
  district_code <- consumption_code_key(x[[district_code_col]], 2L)
  state_name_raw <- trimws(plain_chr(x[[state_name_col]]))
  district_name <- trimws(plain_chr(x[[district_name_col]]))
  state_name_raw[is.na(state_name_raw)] <- ""
  district_name[is.na(district_name)] <- ""

  # Excel codebooks use merged/blank state-name cells. Recover those names only
  # from another row carrying the same explicit state code; fall back to the
  # project's official Census-2001 state-code table when necessary.
  for (code in unique(state_code[!is.na(state_code)])) {
    idx <- which(state_code == code)
    labels <- unique(state_name_raw[idx][nzchar(state_name_raw[idx])])
    if (length(labels) > 1L) {
      stop(source_id, " district codebook has conflicting state names for state code ", code, ".", call. = FALSE)
    }
    label <- if (length(labels) == 1L) labels[[1]] else census_2001_state_name(code)
    if (is.na(label) || !nzchar(label)) {
      stop(source_id, " district codebook cannot resolve state code ", code, ".", call. = FALSE)
    }
    state_name_raw[idx] <- label
  }

  keep <- !is.na(state_code) & !is.na(district_code) & nzchar(district_name)
  out <- data.frame(
    source_id = rep(as.character(source_id), sum(keep)),
    state_code_source = state_code[keep],
    district_code_source = district_code[keep],
    state_name_source = state_name_raw[keep],
    district_name_source = district_name[keep],
    stringsAsFactors = FALSE
  )
  out$state_std <- canonicalize_state_name(out$state_name_source)
  out$district_std <- canonicalize_district_name(out$district_name_source)

  key <- paste(out$state_code_source, out$district_code_source, sep = "__")
  if (anyDuplicated(key)) {
    stop(source_id, " district codebook contains duplicate state/district codes.", call. = FALSE)
  }
  if (any(!nzchar(out$state_std)) || any(!nzchar(out$district_std))) {
    stop(source_id, " district codebook contains empty normalized names.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

read_consumption_district_codebook_excel <- function(path, source_id) {
  need_pkg("readxl", "consumption district codebooks")
  if (!file.exists(path)) stop("Consumption district codebook is missing: ", path, call. = FALSE)
  raw <- readxl::read_excel(path, .name_repair = "minimal")
  state_name_col <- consumption_codebook_column(raw, c("State Name", "State"), source_id)
  state_code_col <- consumption_codebook_column(raw, c("State Code"), source_id)
  district_name_col <- consumption_codebook_column(raw, c("District Name", "District"), source_id)
  district_code_col <- consumption_codebook_column(raw, c("District Code"), source_id)
  normalize_consumption_codebook(
    raw,
    state_name_col,
    state_code_col,
    district_name_col,
    district_code_col,
    source_id
  )
}

read_consumption_district_codebook_ddi <- function(path, source_id) {
  need_pkg("XML", "consumption district-code DDI metadata")
  if (!file.exists(path)) stop("Consumption district DDI is missing: ", path, call. = FALSE)
  doc <- XML::xmlParse(path)
  # DDI documents may declare a default namespace. Match by local element name so
  # the same parser handles both the distributed metadata and small test fixtures.
  vars <- XML::getNodeSet(
    doc,
    "//*[local-name()='var' and @name='District_Code']"
  )
  if (length(vars) != 1L) {
    stop(source_id, " DDI must contain exactly one District_Code variable.", call. = FALSE)
  }
  categories <- XML::getNodeSet(vars[[1]], "./*[local-name()='catgry']")
  category_text <- function(node, child) {
    hit <- XML::getNodeSet(node, paste0("./*[local-name()='", child, "']"))
    if (length(hit) != 1L) return(NA_character_)
    trimws(XML::xmlValue(hit[[1]]))
  }
  values <- vapply(categories, category_text, character(1), child = "catValu")
  labels <- vapply(categories, category_text, character(1), child = "labl")
  valid <- grepl("^[0-9]{4}$", values) & nzchar(labels)
  if (!any(valid)) stop(source_id, " DDI contains no labelled four-digit district codes.", call. = FALSE)

  state_code <- substr(values[valid], 1L, 2L)
  district_code <- substr(values[valid], 3L, 4L)
  raw <- data.frame(
    state_name = census_2001_state_name(state_code),
    state_code = state_code,
    district_name = labels[valid],
    district_code = district_code,
    stringsAsFactors = FALSE
  )
  normalize_consumption_codebook(
    raw, "state_name", "state_code", "district_name", "district_code", source_id
  )
}

consumption_household_state_code <- function(state, codebook) {
  value <- trimws(plain_chr(state))
  numeric_code <- suppressWarnings(as.integer(value))
  out <- rep(NA_character_, length(value))
  numeric <- !is.na(numeric_code)
  out[numeric] <- sprintf("%02d", numeric_code[numeric])
  if (any(!numeric)) {
    state_key <- canonicalize_state_name(value[!numeric])
    lookup <- unique(codebook[c("state_code_source", "state_std")])
    if (anyDuplicated(lookup$state_std)) {
      duplicate <- unique(lookup$state_std[duplicated(lookup$state_std)])
      stop("Consumption district codebook has ambiguous normalized state names: ", paste(duplicate, collapse = ", "), call. = FALSE)
    }
    out[!numeric] <- lookup$state_code_source[match(state_key, lookup$state_std)]
  }
  out
}

consumption_household_district_code <- function(district, state_code) {
  value <- trimws(plain_chr(district))
  numeric_value <- suppressWarnings(as.integer(value))
  out <- rep(NA_character_, length(value))
  for (i in seq_along(value)) {
    if (is.na(numeric_value[[i]])) next
    raw <- sprintf("%d", numeric_value[[i]])
    if (nchar(raw) <= 2L) {
      out[[i]] <- sprintf("%02d", numeric_value[[i]])
      next
    }
    padded <- sprintf("%04d", numeric_value[[i]])
    if (!is.na(state_code[[i]]) && substr(padded, 1L, 2L) == state_code[[i]]) {
      out[[i]] <- substr(padded, 3L, 4L)
    }
  }
  out
}

attach_consumption_source_district_identity <- function(households, codebook) {
  hh <- safe_df(households)
  cb <- safe_df(codebook)
  require_consumption_columns(hh, c("state_code_source", "district_code_source"), "Consumption households")
  require_consumption_columns(
    cb,
    c(
      "source_id", "state_code_source", "district_code_source", "state_name_source",
      "district_name_source", "state_std", "district_std"
    ),
    "Consumption district codebook"
  )
  key_cb <- paste(cb$state_code_source, cb$district_code_source, sep = "__")
  if (anyDuplicated(key_cb)) stop("Consumption district codebook contains duplicate state/district codes.", call. = FALSE)

  state_code <- consumption_household_state_code(hh$state_code_source, cb)
  district_code <- consumption_household_district_code(hh$district_code_source, state_code)
  key <- paste(state_code, district_code, sep = "__")
  pos <- match(key, key_cb)
  if (anyNA(pos)) {
    unresolved <- unique(paste(hh$state_code_source[is.na(pos)], hh$district_code_source[is.na(pos)], sep = "/"))
    stop(
      "Consumption source geography has unresolved survey codes: ",
      paste(utils::head(unresolved, 10L), collapse = ", "),
      call. = FALSE
    )
  }

  out <- hh
  out$source_state_code <- cb$state_code_source[pos]
  out$source_district_code <- cb$district_code_source[pos]
  out$source_state_name <- cb$state_name_source[pos]
  out$source_district_name <- cb$district_name_source[pos]
  out$state_std <- cb$state_std[pos]
  out$district_std <- cb$district_std[pos]
  out
}

consumption_codebook_name_anomalies <- function(codebook) {
  cb <- safe_df(codebook)
  require_consumption_columns(
    cb, c("state_name_source", "district_name_source", "state_std"), "Consumption district codebook"
  )
  state_names <- unique(cb[c("state_name_source", "state_std")])
  state_names <- state_names[nzchar(state_names$state_std), , drop = FALSE]
  flagged <- logical(nrow(cb))
  matched_state <- rep(NA_character_, nrow(cb))
  for (i in seq_len(nrow(cb))) {
    district_key <- canonicalize_district_name(cb$district_name_source[[i]])
    other <- state_names$state_std[state_names$state_std != cb$state_std[[i]]]
    padded_district <- paste0(" ", district_key, " ")
    compact_district <- gsub(" ", "", district_key, fixed = TRUE)
    hit <- other[vapply(other, function(state) {
      if (!nzchar(state)) return(FALSE)
      compact_state <- gsub(" ", "", state, fixed = TRUE)
      # Long state names embedded inside a district label are suspicious even
      # when a workbook corruption removes the separating whitespace (for
      # example, "Lakshadweephimpur"). Short names need token boundaries so
      # ordinary names such as "Goalpara" do not spuriously match "Goa".
      if (nchar(compact_state) >= 5L) {
        grepl(compact_state, compact_district, fixed = TRUE)
      } else {
        grepl(paste0(" ", state, " "), padded_district, fixed = TRUE)
      }
    }, logical(1))]
    if (length(hit)) {
      flagged[[i]] <- TRUE
      matched_state[[i]] <- hit[[1]]
    }
  }
  out <- cb[flagged, , drop = FALSE]
  out$foreign_state_text <- matched_state[flagged]
  rownames(out) <- NULL
  out
}
