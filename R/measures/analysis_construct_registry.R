# Cross-family construct ontology.
#
# The tracked variable dictionary is the semantic authority for variables that
# already exist in the district/panel architecture. Source-specific registries
# remain authoritative for source-only constructs (for example C-17 and DISE).
# This module projects those authorities onto one common schema; it does not
# redefine formulas, estimators, or admissible specification grids.

analysis_construct_columns <- function() {
  c(
    "construct_id", "variable", "label", "domain", "source", "vintage",
    "unit", "level", "denominator", "universe", "stage", "role",
    "preferred", "causal_status", "comparable_to", "alternative_to",
    "authority"
  )
}

analysis_construct_frame <- function(...) {
  out <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
  missing <- setdiff(analysis_construct_columns(), names(out))
  if (length(missing)) {
    stop(
      "Analysis-construct rows are missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  out <- out[analysis_construct_columns()]
  text <- setdiff(names(out), "preferred")
  for (nm in text) {
    out[[nm]] <- trimws(plain_chr(out[[nm]]))
    out[[nm]][is.na(out[[nm]])] <- ""
  }
  if (nrow(out) && (anyDuplicated(out$construct_id) || any(!nzchar(out$construct_id)))) {
    stop("Analysis-construct construct_id values must be nonempty and unique.", call. = FALSE)
  }
  if (nrow(out) && any(!nzchar(out$variable))) {
    stop("Analysis-construct variables must be nonempty.", call. = FALSE)
  }
  out
}

read_analysis_construct_registry <- function(
    path = file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "data", "metadata", "variable_dictionary.csv")) {
  if (!file.exists(path)) stop("Variable dictionary is missing: ", path, call. = FALSE)
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "variable", "label", "description", "domain", "vintage", "denominator",
    "universe", "stage", "role", "preferred", "causal_status",
    "comparable_to", "alternative_to", "unit", "level", "source"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Variable dictionary lacks construct semantics: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(x) || anyDuplicated(x$variable)) {
    stop("Variable dictionary must contain unique variables.", call. = FALSE)
  }
  preferred <- tolower(trimws(plain_chr(x$preferred)))
  if (any(!preferred %in% c("true", "false"))) {
    stop("Variable dictionary preferred flags must be TRUE or FALSE.", call. = FALSE)
  }
  required_text <- c("variable", "label", "domain", "source", "unit", "level", "stage", "role", "causal_status")
  if (any(vapply(x[required_text], function(v) any(is.na(v) | !nzchar(trimws(plain_chr(v)))), logical(1)))) {
    stop("Variable dictionary contains incomplete canonical construct semantics.", call. = FALSE)
  }
  analysis_construct_frame(
    construct_id = plain_chr(x$variable),
    variable = plain_chr(x$variable),
    label = plain_chr(x$label),
    domain = plain_chr(x$domain),
    source = plain_chr(x$source),
    vintage = plain_chr(x$vintage),
    unit = plain_chr(x$unit),
    level = plain_chr(x$level),
    denominator = plain_chr(x$denominator),
    universe = plain_chr(x$universe),
    stage = plain_chr(x$stage),
    role = plain_chr(x$role),
    preferred = preferred == "true",
    causal_status = plain_chr(x$causal_status),
    comparable_to = plain_chr(x$comparable_to),
    alternative_to = plain_chr(x$alternative_to),
    authority = rep("variable_dictionary", nrow(x))
  )
}

analysis_construct_rows <- function(registry, variables) {
  x <- analysis_construct_frame(safe_df(registry))
  variables <- plain_chr(variables)
  out <- x[match(variables, x$variable), , drop = FALSE]
  if (nrow(out) != length(variables) || any(is.na(out$variable))) {
    missing <- variables[is.na(match(variables, x$variable))]
    stop("Canonical construct registry is missing variables: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

analysis_constructs_from_english_opportunity <- function(registry, existing_variables = character()) {
  x <- safe_df(registry)
  keep <- !x$variable %in% plain_chr(existing_variables)
  x <- x[keep, , drop = FALSE]
  if (!nrow(x)) return(analysis_construct_frame(
    construct_id = character(), variable = character(), label = character(), domain = character(),
    source = character(), vintage = character(), unit = character(), level = character(),
    denominator = character(), universe = character(), stage = character(), role = character(),
    preferred = logical(), causal_status = character(), comparable_to = character(),
    alternative_to = character(), authority = character()
  ))
  analysis_construct_frame(
    construct_id = plain_chr(x$variable), variable = plain_chr(x$variable),
    label = plain_chr(x$interpretation), domain = rep("english_opportunity", nrow(x)),
    source = plain_chr(x$source), vintage = ifelse(x$source == "census_2001_c17", "2001", "2007-08"),
    unit = rep("percent", nrow(x)), level = plain_chr(x$unit),
    denominator = plain_chr(x$denominator), universe = plain_chr(x$population),
    stage = plain_chr(x$stage), role = plain_chr(x$paper_role),
    preferred = x$preferred %in% TRUE, causal_status = rep("descriptive_mechanism", nrow(x)),
    comparable_to = rep("", nrow(x)), alternative_to = rep("", nrow(x)),
    authority = rep("english_opportunity_measures", nrow(x))
  )
}

analysis_constructs_from_dise <- function(
    registry = dise_construct_registry(), existing_variables = character()) {
  x <- safe_df(registry)
  x <- x[!x$variable %in% plain_chr(existing_variables), , drop = FALSE]
  if (!nrow(x)) return(analysis_construct_frame(
    construct_id = character(), variable = character(), label = character(), domain = character(),
    source = character(), vintage = character(), unit = character(), level = character(),
    denominator = character(), universe = character(), stage = character(), role = character(),
    preferred = logical(), causal_status = character(), comparable_to = character(),
    alternative_to = character(), authority = character()
  ))
  analysis_construct_frame(
    construct_id = plain_chr(x$variable), variable = plain_chr(x$variable), label = plain_chr(x$label),
    domain = ifelse(x$domain == "management", "institution_choice", "schooling_treatment"),
    source = rep("DISE", nrow(x)), vintage = ifelse(grepl("0508", x$construct_id), "2005-06_to_2007-08", "2007-08"),
    unit = rep("percent", nrow(x)), level = rep("district", nrow(x)), denominator = rep("", nrow(x)),
    universe = rep("DISE elementary-school district aggregates", nrow(x)),
    stage = rep("school_system_structure", nrow(x)), role = plain_chr(x$paper_role),
    preferred = x$analysis_scope == "structural_iv", causal_status = rep("administrative_schooling_construct", nrow(x)),
    comparable_to = rep("", nrow(x)), alternative_to = rep("", nrow(x)), authority = rep("dise_construct_registry", nrow(x))
  )
}

compile_analysis_construct_registry <- function(
    variable_registry = read_analysis_construct_registry(),
    english_opportunity_registry = NULL,
    dise_registry = dise_construct_registry()) {
  base <- analysis_construct_frame(safe_df(variable_registry))
  rows <- list(base)
  existing <- base$variable
  if (!is.null(english_opportunity_registry)) {
    opportunity <- analysis_constructs_from_english_opportunity(
      english_opportunity_registry, existing
    )
    rows[[length(rows) + 1L]] <- opportunity
    existing <- union(existing, opportunity$variable)
  }
  dise <- analysis_constructs_from_dise(dise_registry, existing)
  rows[[length(rows) + 1L]] <- dise
  out <- safe_bind_rows(rows)
  if (anyDuplicated(out$variable)) {
    stop("Compiled analysis-construct registry contains duplicate variables.", call. = FALSE)
  }
  out <- out[order(out$domain, out$stage, out$variable), , drop = FALSE]
  rownames(out) <- NULL
  analysis_construct_frame(out)
}
