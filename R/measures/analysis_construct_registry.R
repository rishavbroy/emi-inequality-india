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

empty_analysis_construct_registry <- function() {
  analysis_construct_frame(
    construct_id = character(), variable = character(), label = character(), domain = character(),
    source = character(), vintage = character(), unit = character(), level = character(),
    denominator = character(), universe = character(), stage = character(), role = character(),
    preferred = logical(), causal_status = character(), comparable_to = character(),
    alternative_to = character(), authority = character()
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
  counts <- table(x$variable)
  ambiguous <- unique(variables[variables %in% names(counts)[counts > 1L]])
  if (length(ambiguous)) {
    stop(
      "Canonical construct variables are ambiguous across vintages/sources: ",
      paste(ambiguous, collapse = ", "),
      ". Select those constructs by construct_id instead.",
      call. = FALSE
    )
  }
  out <- x[match(variables, x$variable), , drop = FALSE]
  if (nrow(out) != length(variables) || any(is.na(out$variable))) {
    missing <- variables[is.na(match(variables, x$variable))]
    stop("Canonical construct registry is missing variables: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

analysis_construct_rows_by_id <- function(registry, construct_ids) {
  x <- analysis_construct_frame(safe_df(registry))
  construct_ids <- plain_chr(construct_ids)
  out <- x[match(construct_ids, x$construct_id), , drop = FALSE]
  if (nrow(out) != length(construct_ids) || any(is.na(out$construct_id))) {
    missing <- construct_ids[is.na(match(construct_ids, x$construct_id))]
    stop("Canonical construct registry is missing construct_id values: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

analysis_constructs_from_english_opportunity <- function(registry, existing_variables = character()) {
  x <- safe_df(registry)
  keep <- !x$variable %in% plain_chr(existing_variables)
  x <- x[keep, , drop = FALSE]
  if (!nrow(x)) return(empty_analysis_construct_registry())
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
  if (!nrow(x)) return(empty_analysis_construct_registry())
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



analysis_constructs_from_consumption_welfare <- function(
    iv_registry, welfare_registry, survey_registry, existing_construct_ids = character()) {
  iv <- safe_df(iv_registry)
  welfare <- safe_df(welfare_registry)
  surveys <- validate_consumption_survey_registry(safe_df(survey_registry))
  required_iv <- c("outcome_id", "outcome_round", "baseline_round")
  required_welfare <- c("outcome_id", "label", "unit", "role", "survey_ids")
  missing <- c(setdiff(required_iv, names(iv)), setdiff(required_welfare, names(welfare)))
  if (length(missing)) {
    stop(
      "Consumption construct projection lacks fields: ",
      paste(unique(missing), collapse = ", "), call. = FALSE
    )
  }

  rounds <- unique(c(plain_chr(iv$outcome_round), plain_chr(iv$baseline_round)))
  rounds <- rounds[nzchar(rounds)]
  rows <- list()
  k <- 0L
  for (survey_id in rounds) {
    survey <- consumption_survey_spec(surveys, survey_id)
    available <- consumption_welfare_registry_for_survey(welfare, survey_id)
    for (i in seq_len(nrow(available))) {
      outcome <- available[i, , drop = FALSE]
      construct_id <- paste("consumption", survey_id, outcome$outcome_id[[1L]], sep = "__")
      if (construct_id %in% plain_chr(existing_construct_ids)) next
      start_year <- format(survey$survey_start[[1L]], "%Y")
      end_year <- format(survey$survey_end[[1L]], "%y")
      survey_role <- plain_chr(survey$analysis_role[[1L]])
      stage <- if (survey_role == "core_baseline") {
        "pre_treatment_context"
      } else if (grepl("^long_post", survey_role)) {
        "long_run_welfare"
      } else {
        "post_treatment_intermediate"
      }
      k <- k + 1L
      rows[[k]] <- analysis_construct_frame(
        construct_id = construct_id,
        variable = plain_chr(outcome$outcome_id[[1L]]),
        label = paste(plain_chr(outcome$label[[1L]]), "—", plain_chr(survey$survey_label[[1L]])),
        domain = "welfare", source = plain_chr(survey$survey_label[[1L]]),
        vintage = paste0(start_year, "-", end_year), unit = plain_chr(outcome$unit[[1L]]),
        level = "district", denominator = "survey-weighted persons",
        universe = paste(plain_chr(survey$survey_label[[1L]]), "validated district welfare support"),
        stage = stage, role = "outcome", preferred = outcome$role[[1L]] == "primary",
        causal_status = if (stage == "pre_treatment_context") {
          "predetermined_welfare_baseline"
        } else {
          "post_treatment_outcome"
        },
        comparable_to = "", alternative_to = "",
        authority = "consumption_welfare_outcomes+consumption_survey_registry"
      )
    }
  }
  if (!length(rows)) return(empty_analysis_construct_registry())
  safe_bind_rows(rows)
}

analysis_constructs_from_mechanism_registry <- function(
    registry, domain, source, vintage, universe, authority,
    stage = "post_treatment_intermediate", role = "outcome",
    causal_status = "post_treatment_descriptive_mechanism",
    level = "district", existing_construct_ids = character()) {
  x <- safe_df(registry)
  required <- c("construct_id", "variable", "label", "unit", "denominator")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      authority, " lacks canonical construct fields: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  x <- x[!x$construct_id %in% plain_chr(existing_construct_ids), , drop = FALSE]
  if (!nrow(x)) return(empty_analysis_construct_registry())
  if (anyDuplicated(x$construct_id)) {
    stop(authority, " contains duplicate construct_id values.", call. = FALSE)
  }
  analysis_construct_frame(
    construct_id = plain_chr(x$construct_id),
    variable = plain_chr(x$variable),
    label = plain_chr(x$label),
    domain = rep(domain, nrow(x)),
    source = rep(source, nrow(x)),
    vintage = if (length(vintage) == 1L) rep(vintage, nrow(x)) else plain_chr(vintage),
    unit = plain_chr(x$unit),
    level = rep(level, nrow(x)),
    denominator = plain_chr(x$denominator),
    universe = rep(universe, nrow(x)),
    stage = rep(stage, nrow(x)),
    role = rep(role, nrow(x)),
    preferred = if ("tier" %in% names(x)) x$tier %in% "core" else rep(FALSE, nrow(x)),
    causal_status = rep(causal_status, nrow(x)),
    comparable_to = rep("", nrow(x)),
    alternative_to = rep("", nrow(x)),
    authority = rep(authority, nrow(x))
  )
}

analysis_constructs_from_nss64_social_group_gaps <- function(
    margin_registry = nss64_schooling_social_group_margin_registry(),
    existing_construct_ids = character()) {
  x <- safe_df(margin_registry)
  # Gap constructs describe access inequalities, not only the smaller subset
  # used for distance-heterogeneity regressions. Keep every registered schooling
  # margin so bounded descriptive cross-cuts can link to canonical constructs.
  if (!nrow(x)) return(empty_analysis_construct_registry())
  construct_id <- paste0("gap__", plain_chr(x$outcome))
  keep <- !construct_id %in% plain_chr(existing_construct_ids)
  x <- x[keep, , drop = FALSE]
  construct_id <- construct_id[keep]
  if (!nrow(x)) return(empty_analysis_construct_registry())
  analysis_construct_frame(
    construct_id = construct_id,
    variable = construct_id,
    label = paste("Disadvantaged-group gap in", plain_chr(x$label)),
    domain = rep("schooling_inequality", nrow(x)),
    source = rep("NSS 64 education", nrow(x)),
    vintage = rep("2007-08", nrow(x)),
    unit = rep("percentage_point_gap", nrow(x)),
    level = rep("district_social_group_contrast", nrow(x)),
    denominator = rep("group_specific_schooling_margin", nrow(x)),
    universe = rep("NSS 64 district schooling margins for registered disadvantaged groups", nrow(x)),
    stage = rep("schooling_access", nrow(x)),
    role = rep("descriptive_outcome", nrow(x)),
    preferred = rep(FALSE, nrow(x)),
    causal_status = rep("descriptive_heterogeneity_outcome", nrow(x)),
    comparable_to = rep("", nrow(x)),
    alternative_to = rep("", nrow(x)),
    authority = rep("nss64_schooling_social_group_margin_registry", nrow(x))
  )
}

compile_analysis_construct_registry <- function(
    variable_registry = read_analysis_construct_registry(),
    english_opportunity_registry = NULL,
    consumption_iv_registry = NULL,
    consumption_welfare_registry = NULL,
    consumption_survey_registry = NULL,
    dise_registry = dise_construct_registry(),
    migration_registry = census_migration_mechanism_registry(),
    housing_registry = census_housing_mechanism_registry(),
    economic_census_registry = economic_census_mechanism_registry(),
    labor_registries = list(
      nss66 = labor_mechanism_registry("nss66"),
      plfs_2017_18 = labor_mechanism_registry("plfs_2017_18")
    ),
    st_language_registry = census_1991_st_language_outcome_registry(),
    social_group_margin_registry = nss64_schooling_social_group_margin_registry()) {
  base <- analysis_construct_frame(safe_df(variable_registry))
  rows <- list(base)
  existing_variables <- base$variable
  existing_ids <- base$construct_id

  if (!is.null(english_opportunity_registry)) {
    opportunity <- analysis_constructs_from_english_opportunity(
      english_opportunity_registry, existing_variables
    )
    rows[[length(rows) + 1L]] <- opportunity
    existing_variables <- union(existing_variables, opportunity$variable)
    existing_ids <- union(existing_ids, opportunity$construct_id)
  }

  if (!is.null(consumption_iv_registry)) {
    if (is.null(consumption_welfare_registry) || is.null(consumption_survey_registry)) {
      stop(
        "Consumption construct projection requires welfare and survey registries.",
        call. = FALSE
      )
    }
    consumption <- analysis_constructs_from_consumption_welfare(
      consumption_iv_registry, consumption_welfare_registry, consumption_survey_registry, existing_ids
    )
    rows[[length(rows) + 1L]] <- consumption
    existing_ids <- union(existing_ids, consumption$construct_id)
  }

  dise <- analysis_constructs_from_dise(dise_registry, existing_variables)
  rows[[length(rows) + 1L]] <- dise
  existing_variables <- union(existing_variables, dise$variable)
  existing_ids <- union(existing_ids, dise$construct_id)

  mechanism_specs <- list(
    list(
      registry = migration_registry, domain = "migration", source = "Census 2011 migration tables",
      vintage = "2011", universe = "Census-2011 migration populations harmonized to Census-2001 districts",
      authority = "census_migration_mechanism_registry"
    ),
    list(
      registry = housing_registry, domain = "household_living_standards", source = "Census 2001/2011 housing tables",
      vintage = "2001_to_2011", universe = "Census household counts on exact concept-matched 2001-to-2011 support",
      authority = "census_housing_mechanism_registry"
    ),
    list(
      registry = economic_census_registry, domain = "local_economic_structure", source = "Economic Census 2005/2013",
      vintage = "2005_to_2013", universe = "Economic Census nonfarm establishments/employment harmonized to Census-2001 districts",
      authority = "economic_census_mechanism_registry"
    ),
    list(
      registry = st_language_registry, domain = "language_behavior", source = "Census 1991 ST-17",
      vintage = "1991", universe = "validated Scheduled-Tribe mother-tongue cells",
      authority = "census_1991_st_language_outcome_registry", stage = "language_behavior",
      role = "validation_outcome", causal_status = "predetermined_language_behavior_validation"
    )
  )
  for (spec in mechanism_specs) {
    args <- c(spec, list(existing_construct_ids = existing_ids))
    part <- do.call(analysis_constructs_from_mechanism_registry, args)
    rows[[length(rows) + 1L]] <- part
    existing_ids <- union(existing_ids, part$construct_id)
  }

  for (wave_id in names(labor_registries)) {
    temporal <- if (identical(wave_id, "nss66")) "2009-10" else "2017-18"
    part <- analysis_constructs_from_mechanism_registry(
      labor_registries[[wave_id]],
      domain = "labor_market",
      source = if (identical(wave_id, "nss66")) "NSS 66 employment" else "PLFS 2017-18",
      vintage = temporal,
      universe = "age-15-plus district labor outcomes on preferred reviewed geography",
      authority = "labor_mechanism_registry",
      existing_construct_ids = existing_ids
    )
    rows[[length(rows) + 1L]] <- part
    existing_ids <- union(existing_ids, part$construct_id)
  }

  gaps <- analysis_constructs_from_nss64_social_group_gaps(
    social_group_margin_registry, existing_ids
  )
  rows[[length(rows) + 1L]] <- gaps

  out <- safe_bind_rows(rows)
  if (anyDuplicated(out$construct_id)) {
    stop("Compiled analysis-construct registry contains duplicate construct_id values.", call. = FALSE)
  }
  out <- out[order(out$domain, out$stage, out$construct_id), , drop = FALSE]
  rownames(out) <- NULL
  analysis_construct_frame(out)
}
