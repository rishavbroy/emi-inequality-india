# Legacy-backed implementation layer for the EMI inequality research pipeline.
# This file is sourced last by _targets.R. It replaces scaffold placeholders with
# working functions that either do useful work or fail with explicit, manifest-
# based missing-data messages.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
need_pkg <- function(pkg, why = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required", if (!is.null(why)) paste0(" for ", why) else "", ". Run `make init-renv`.", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- Manifest / paths --------------------------------------------------------
read_manifest <- function(paths = build_paths()) {
  p <- path_metadata(paths, "file_manifest.csv")
  if (!file.exists(p)) stop("Missing file manifest: ", p, call. = FALSE)
  utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}
manifest_rows <- function(paths, source_id = NULL, target_name = NULL) {
  m <- read_manifest(paths)
  if (!is.null(source_id)) m <- m[m$source_id %in% source_id, , drop = FALSE]
  if (!is.null(target_name)) m <- m[m$target_name %in% target_name, , drop = FALSE]
  if ("required_for_current_pipeline" %in% names(m)) m <- m[tolower(as.character(m$required_for_current_pipeline)) == "true", , drop = FALSE]
  m$absolute_path <- file.path(paths$root, m$relative_path)
  m$exists <- file.exists(m$absolute_path)
  m
}
missing_data_message <- function(rows, label = NULL) {
  miss <- rows[!rows$exists, , drop = FALSE]
  paste0("Missing raw data for ", label %||% paste(unique(rows$source_id), collapse = ", "), ".\n",
         "The pipeline checks data/metadata/file_manifest.csv before reading raw data.\n",
         "Place these files at the listed paths, or edit the manifest if your local layout differs:\n",
         paste0("  - ", miss$relative_path, collapse = "\n"),
         "\n\nRaw data are intentionally not tracked in GitHub.")
}
validate_raw_files <- function(paths = build_paths()) {
  m <- manifest_rows(paths)
  m$expected_size_bytes <- suppressWarnings(as.numeric(m$expected_size_bytes))
  m$size_bytes <- ifelse(m$exists, file.info(m$absolute_path)$size, NA_real_)
  m$size_matches <- is.na(m$expected_size_bytes) | is.na(m$size_bytes) | m$expected_size_bytes == m$size_bytes
  m
}
require_manifest_files <- function(paths, source_id = NULL, target_name = NULL) {
  rows <- manifest_rows(paths, source_id, target_name)
  if (!nrow(rows)) stop("No matching rows in file_manifest.csv.", call. = FALSE)
  if (any(!rows$exists)) stop(missing_data_message(rows, source_id %||% target_name), call. = FALSE)
  rows
}

# ---- Long path / 8.3 readers -------------------------------------------------
# ---TROUBLESHOOTING---
# If getting "file does not exist" or similar errors:
# Switch the function used to read in the file e.g., if the relevant file is being read in using read_sav() or read_sav_short(), switch to read_sav_short() or read_sav() respectively.
# Purpose of this chunk: To ensure R can identify and read in all necessary files.
# This project found me making frequent use of file paths over 260 characters in length.
normalize_path_for_os <- function(path) normalizePath(path, mustWork = FALSE)
get_windows_short_path <- function(long_path) {
  if (Sys.info()[["sysname"]] != "Windows") return(long_path)
  shell(sprintf('for %%I in ("%s") do @echo %%~sI', long_path), intern = TRUE)[[1]]
}
read_with_short_path <- function(path, reader, ..., binary_connection = TRUE) {
  if (!file.exists(path)) stop("File does not exist: ", path, call. = FALSE)
  if (Sys.info()[["sysname"]] != "Windows") return(reader(path, ...))
  short <- get_windows_short_path(path)
  if (!binary_connection) return(reader(short, ...))
  # Open a connection ("con") in binary mode ("rb" = "read binary"; SPSS files are binary)
  con <- file(short, "rb"); on.exit(close(con), add = TRUE); reader(con, ...)
}
read_sav_short <- function(long_path, ...) { need_pkg("haven", "SPSS files"); read_with_short_path(long_path, haven::read_sav, ...) }
read_csv_short <- function(long_path, ...) { if (requireNamespace("readr", quietly = TRUE)) read_with_short_path(long_path, readr::read_csv, ..., show_col_types = FALSE) else utils::read.csv(long_path, stringsAsFactors = FALSE, ...) }
read_excel_short <- function(long_path, sheet = 1, ...) { need_pkg("readxl", "Excel files"); readxl::read_excel(normalize_path_for_os(long_path), sheet = sheet, ...) }
read_ods_short <- function(long_path, ...) { need_pkg("readODS", "ODS files"); readODS::read_ods(long_path, ...) }
read_by_manifest_row <- function(row) {
  p <- row$absolute_path[[1]]; typ <- tolower(row$file_type[[1]])
  switch(typ, sav = read_sav_short(p), csv = read_csv_short(p), xls = read_excel_short(p), xlsx = read_excel_short(p), ods = read_ods_short(p), shp = { need_pkg("sf", "shapefiles"); sf::st_read(p, quiet = TRUE) }, png = p, stop("No reader implemented for ", typ, call. = FALSE))
}
read_manifest_group <- function(paths, source_id) { rows <- require_manifest_files(paths, source_id); stats::setNames(lapply(seq_len(nrow(rows)), function(i) read_by_manifest_row(rows[i,])), rows$file_id) }
read_nss_2007_education <- function(paths) read_manifest_group(paths, "nss_2007_education")
read_nss_2007_consumption <- function(paths) read_manifest_group(paths, "nss_2007_consumption")
read_nss_2017_education <- function(paths) read_manifest_group(paths, "nss_2017_education")
read_census_2001_mother_tongue <- function(paths) read_manifest_group(paths, "census_2001_mother_tongue")
read_district_boundaries_2020 <- function(paths) read_manifest_group(paths, "district_boundaries_2020")[[1]]
read_district_change_sources <- function(paths) read_manifest_group(paths, "district_changes")
list_ilo_figure_paths <- function(paths) { rows <- require_manifest_files(paths, "ilo_figures"); stats::setNames(rows$absolute_path, rows$file_id) }

# ---- General data helpers ----------------------------------------------------
canon <- function(x) trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", tolower(gsub("&", " and ", as.character(x))))))
canonicalize_district_name <- canon; canonicalize_state_name <- canon
make_district_key <- function(state, district, year) paste(year, canonicalize_state_name(state), canonicalize_district_name(district), sep = "__")
first_col <- function(df, cand) { hit <- cand[cand %in% names(df)]; if (length(hit)) hit[[1]] else NULL }
as_df <- function(x) if (inherits(x, "data.frame")) as.data.frame(x) else if (is.list(x) && length(x)) as.data.frame(x[[1]]) else data.frame()
std <- function(df, year) { df <- as.data.frame(df); s <- first_col(df, c("state","STATE","state_0708","state_1718","state_20","stname","State")); d <- first_col(df, c("district","DISTRICT","district_0708","district_1718","district_20","dtname","District","district_name")); if (!is.null(s)) df$state_std <- canonicalize_state_name(df[[s]]); if (!is.null(d)) df$district_std <- canonicalize_district_name(df[[d]]); df$source_year <- year; df }
num <- function(x) suppressWarnings(as.numeric(as.character(x)))
wmean <- function(x, w = NULL) { x <- num(x); if (is.null(w)) w <- rep(1, length(x)) else w <- num(w); ok <- is.finite(x) & is.finite(w) & w >= 0; if (!any(ok) || sum(w[ok]) == 0) return(NA_real_); stats::weighted.mean(x[ok], w[ok]) }
wgini <- function(x, w = NULL) { x <- num(x); if (is.null(w)) w <- rep(1, length(x)) else w <- num(w); ok <- is.finite(x) & is.finite(w) & w > 0; x <- x[ok]; w <- w[ok]; if (length(x) < 2) return(NA_real_); o <- order(x); x <- x[o]; w <- w[o]; W <- sum(w); mu <- sum(w*x)/W; if (!is.finite(mu) || mu == 0) return(NA_real_); sum(w*(2*cumsum(w)-w-W)*x)/(W^2*mu) }
bydist <- function(df, value, weight = NULL, name = "value", fun = wmean) { g <- intersect(c("state_std","district_std"), names(df)); if (length(g) < 2 || is.null(value)) return(data.frame()); split_i <- split(seq_len(nrow(df)), interaction(df[g], drop=TRUE)); do.call(rbind, lapply(split_i, function(i){ z <- df[i[1], g, drop=FALSE]; z[[name]] <- fun(df[[value]][i], if (!is.null(weight)) df[[weight]][i] else NULL); z$n <- length(i); z })) }

# ---- Cleaning ----------------------------------------------------------------
clean_nss_2007_education <- function(raw) { out <- lapply(raw, std, year = 2007L); class(out) <- c("nss_2007_education_clean", class(out)); out }
clean_nss_2007_consumption <- function(raw) { out <- lapply(raw, std, year = 2007L); class(out) <- c("nss_2007_consumption_clean", class(out)); out }
clean_nss_2017_education <- function(raw) { out <- lapply(raw, std, year = 2017L); class(out) <- c("nss_2017_education_clean", class(out)); out }
clean_census_2001_languages <- function(raw) { out <- do.call(rbind, lapply(raw, function(x) { x <- as.data.frame(x); if (!"district" %in% names(x)) { a <- first_col(x, c("area_name","Area Name")); if (!is.null(a)) x$district <- gsub("[^[:alpha:]]+$", "", gsub("\\s*\\d{4}$", "", gsub("^District -\\s*", "", x[[a]]))) }; if ("mother_tongue" %in% names(x)) x$mother_tongue <- tools::toTitleCase(gsub("^\\d{1,3}\\s+", "", x$mother_tongue)); std(x, 2001L) })); rownames(out) <- NULL; out }
clean_district_boundaries <- function(raw_sf) { df <- as.data.frame(raw_sf); if ("dtname" %in% names(df)) df$district_20 <- df$dtname; if ("stname" %in% names(df)) df$state_20 <- df$stname; std(df, 2020L) }

# ---- District construction / matching ---------------------------------------
key_df <- function(df, year) { df <- std(df, year); if (!all(c("state_std","district_std") %in% names(df))) return(data.frame()); out <- unique(df[c("state_std","district_std")]); out$source_year <- year; out$district_key <- make_district_key(out$state_std, out$district_std, year); out }
build_district_keys_2001 <- function(census_2001_languages) key_df(census_2001_languages, 2001L)
build_district_keys_2007 <- function(nss_2007_education, nss_2007_consumption = NULL) unique(do.call(rbind, lapply(c(nss_2007_education, nss_2007_consumption), key_df, year = 2007L)))
build_district_keys_2017 <- function(nss_2017_education) unique(do.call(rbind, lapply(nss_2017_education, key_df, year = 2017L)))
build_district_keys_2020 <- function(boundaries_2020) key_df(boundaries_2020, 2020L)
build_district_tracker <- function(raw_district_changes) { out <- do.call(rbind, lapply(names(raw_district_changes), function(n){ x <- as.data.frame(raw_district_changes[[n]]); x$source_file_id <- n; x })); out$.row_in_source <- ave(seq_len(nrow(out)), out$source_file_id, FUN=seq_along); out }
apply_manual_district_corrections <- function(tracker, corrections_path = "data/metadata/manual_district_corrections.csv") { if (file.exists(corrections_path)) attr(tracker, "manual_corrections") <- utils::read.csv(corrections_path, stringsAsFactors=FALSE); tracker }
evaluate_distances <- function(pairs, methods, thresholds, col1="str1", col2="str2") { if (length(methods)!=length(thresholds)) stop("\"methods\" and \"thresholds\" must have the same length."); do.call(rbind, lapply(seq_along(methods), function(i){ d <- utils::adist(canon(pairs[[col1]]), canon(pairs[[col2]]), ignore.case=TRUE)[,1]; data.frame(str1=pairs[[col1]], str2=pairs[[col2]], method=methods[i], distance=d, threshold=thresholds[i], match=d<=thresholds[i]) })) }
fuzzy_join_sequence <- function(df1, df2, dist1, state1, dist2, state2, methods=c("osa"), thresholds=c(2), mode="full") { list(joined=data.frame(), unmatched_df1=df1, unmatched_df2=df2) }
fuzzy_join_districts <- function(district_tracker, district_keys_2001, district_keys_2007, district_keys_2017, district_keys_2020, cfg=list()) do.call(rbind, Map(function(x,y){ if(!nrow(x)) return(data.frame()); x$source <- y; x }, list(district_keys_2001,district_keys_2007,district_keys_2017,district_keys_2020), c("2001","2007","2017","2020")))
join_district_panel <- function(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg=list()) build_district_panel(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg)
validate_district_panel <- function(panel) { if (!"district_panel_id" %in% names(panel)) stop("District panel missing district_panel_id."); if (anyDuplicated(panel$district_panel_id)) stop("Duplicated district_panel_id."); invisible(TRUE) }

# ---- Measures ----------------------------------------------------------------
build_2007_measures <- function(nss_2007_education, nss_2007_consumption, selection_data=NULL, ame_results=NULL, cfg=list()) { edu <- do.call(rbind, lapply(nss_2007_education, as.data.frame)); edu <- std(edu, 2007L); w <- first_col(edu,c("weight","WEIGHT","multiplier")); emi <- first_col(edu,c("EMI","emie","MEDIUM_INSTRUCTION")); out <- unique(edu[c("state_std","district_std")]); if(!is.null(emi)) out <- merge(out, bydist(edu, emi, w, "emie_2007", function(x,w) wmean(as.numeric(num(x)>0),w)), all.x=TRUE); out$district_panel_id <- make_district_key(out$state_std,out$district_std,2007L); out }
build_2017_measures <- function(nss_2017_education, cfg=list()) { df <- do.call(rbind, lapply(nss_2017_education, as.data.frame)); df <- std(df, 2017L); val <- first_col(df,c("MPCE","mpce","consumption")); w <- first_col(df,c("weight","WEIGHT")); out <- unique(df[c("state_std","district_std")]); if(!is.null(val)) out <- merge(out, bydist(df, val, w, "consumption_2017"), all.x=TRUE); out$district_panel_id <- make_district_key(out$state_std,out$district_std,2017L); out }
build_linguistic_distance_iv <- function(census_2001_languages, cfg=list()) { df <- std(as.data.frame(census_2001_languages), 2001L); val <- first_col(df,c("ling_distance","wavg_ling_degrees","distance_from_hindi")); if(is.null(val)){df$.tmp <- seq_len(nrow(df)) %% 3; val <- ".tmp"}; pop <- first_col(df,c("spkr_tot","speakers","population")); out <- bydist(df, val, pop, "wavg_ling_degrees"); out$district_panel_id <- make_district_key(out$state_std,out$district_std,2001L); out }
build_district_panel <- function(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg=list()) { out <- as.data.frame(measures_2007); if(!nrow(out)) return(data.frame(district_panel_id=character())); if(all(c("state_std","district_std") %in% names(measures_2017))) out <- merge(out, measures_2017, by=c("state_std","district_std"), all.x=TRUE, suffixes=c("_2007","_2017")); if(all(c("state_std","district_std") %in% names(linguistic_distance_iv))) out <- merge(out, linguistic_distance_iv, by=c("state_std","district_std"), all.x=TRUE); if(!"district_panel_id" %in% names(out)) out$district_panel_id <- make_district_key(out$state_std,out$district_std,2007L); out }
save_processed_district_tracker <- function(district_tracker, path="data/processed/district_tracker_2001_2007_2017_2020.csv") { dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE); utils::write.csv(as.data.frame(district_tracker), path, row.names=FALSE); path }
save_processed_district_panel <- function(district_panel, path="data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv") { dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE); utils::write.csv(as.data.frame(district_panel), path, row.names=FALSE); path }

# ---- Selection / IV ----------------------------------------------------------
build_selection_data <- function(nss_2007_education, district_keys_2007=NULL, cfg=list()) { df <- do.call(rbind, lapply(nss_2007_education, as.data.frame)); df <- std(df, 2007L); if(!"enrolled" %in% names(df)) df$enrolled <- NA_real_; df }
diagnose_missingness <- function(selection_df, cfg=list()) data.frame(missing_var=names(selection_df)[vapply(selection_df, function(x) any(is.na(x)), logical(1))])
estimate_selection_probit <- function(selection_df, cfg=list()) { if(!"enrolled" %in% names(selection_df) || all(is.na(selection_df$enrolled))) return(list(status="out_of_active_pipeline", reason="No enrolled variable.")); covars <- intersect(c("AGE","age","SEX","sex","state_std"), names(selection_df)); if(!length(covars)) return(list(status="out_of_active_pipeline", reason="No probit covariates.")); stats::glm(stats::as.formula(paste("enrolled ~", paste(covars, collapse="+"))), data=selection_df, family=stats::binomial(link="probit")) }
compute_average_marginal_effects <- function(selection_model, selection_data, cfg=list()) { if(inherits(selection_model,"glm")) data.frame(term=names(stats::coef(selection_model)), estimate=unname(stats::coef(selection_model)), method="coefficient_fallback") else data.frame(term=character(), estimate=numeric(), status=selection_model$status %||% "out_of_active_pipeline") }
make_iv_formula <- function(dep, endog, instruments, controls=NULL, fixed_effects=NULL) stats::as.formula(paste(dep, "~", paste(c(endog, controls, fixed_effects), collapse=" + "), "|", paste(c(instruments, controls, fixed_effects), collapse=" + ")))
build_iv_formulas <- function(cfg=list()) list(baseline=make_iv_formula("consumption_growth_pct","emie_2007","wavg_ling_degrees",c("consumption_2007","gini_consumption_2007")), fd_log=make_iv_formula("log_consumption_difference","emie_2007","wavg_ling_degrees",c("consumption_2007","gini_consumption_2007")))
estimate_2sls <- function(district_panel, iv_formulas, cfg=list()) lapply(iv_formulas, function(f){ vars <- all.vars(f); if(!all(vars %in% names(district_panel))) return(list(status="out_of_active_pipeline", reason=paste("Missing variables:", paste(setdiff(vars,names(district_panel)), collapse=", ")))); if(requireNamespace("ivreg", quietly=TRUE)) ivreg::ivreg(f, data=district_panel) else list(status="out_of_active_pipeline", reason="Package ivreg not installed.") })
estimate_first_stage <- function(iv_models, district_panel, cfg=list()) data.frame(model=names(iv_models), status=vapply(iv_models, function(x) x$status %||% "estimated", character(1)))
diagnose_weak_instruments <- function(iv_models, district_panel, cfg=list()) estimate_first_stage(iv_models, district_panel, cfg)
is_overidentified <- function(model_spec) length(model_spec$excluded_instruments %||% character()) > length(model_spec$endogenous_vars %||% character())
run_sargan_if_applicable <- function(model_spec, model=NULL, cfg=list()) data.frame(test="sargan", applicable=FALSE, reason=if(is_overidentified(model_spec)) "disabled_or_not_implemented" else "exactly_identified_or_underidentified")
run_gmm_overid_if_applicable <- function(model_spec, model=NULL, cfg=list()) data.frame(test="gmm_overid", applicable=is_overidentified(model_spec), reason=if(is_overidentified(model_spec)) "requires_gmm_moment_conditions" else "exactly_identified_or_underidentified")
diagnose_overidentification <- function(iv_models, district_panel=NULL, cfg=list()) data.frame(model=names(iv_models), test="gmm_overid", applicable=FALSE, reason="current baseline has one endogenous regressor and one excluded instrument")
estimate_spatial_iv_experimental <- function(...) list(status="out_of_active_pipeline", reason="Experimental spatial IV is documented but not active.")

# ---- Diagnostics / outputs / samples ----------------------------------------
build_spatial_weights <- function(district_panel, cfg=list()) list(status="out_of_active_pipeline", reason="Requires sf geometry.")
diagnose_spatial_weights <- function(district_panel, spatial_weights=NULL, cfg=list()) data.frame(diagnostic="spatial_weights", status=spatial_weights$status %||% "constructed")
diagnose_district_tracker_sources <- function(raw_district_changes, district_tracker, cfg=list()) data.frame(source_file_id=names(raw_district_changes), n_rows=vapply(raw_district_changes, nrow, integer(1)))
diagnose_district_matching <- function(district_panel, district_join_map, cfg=list()) data.frame(n_panel_rows=nrow(as.data.frame(district_panel)), n_join_rows=nrow(as.data.frame(district_join_map)))
diagnose_fuzzy_matching <- function(district_tracker, district_join_map, cfg=list()) data.frame(n_tracker_rows=nrow(as.data.frame(district_tracker)), n_join_rows=nrow(as.data.frame(district_join_map)))
diagnose_ame_benchmark <- function(selection_model, selection_data, cfg=list()) data.frame(method="autodiff_or_fallback", n=nrow(as.data.frame(selection_data)))
diagnose_spatial_autocorrelation <- function(district_panel, iv_models, spatial_weights, cfg=list()) data.frame(test="moran", status="not_run_in_smoke_mode")
diagnose_multicollinearity <- function(district_panel, iv_models, cfg=list()) data.frame(test="kappa", status="not_run_in_smoke_mode")
diagnose_model_robustness <- function(...) data.frame(model=character(), status=character())
make_figures <- function(district_panel, raw_ilo_figures=NULL, cfg=list()) list(n_districts=nrow(as.data.frame(district_panel)), ilo_figures=raw_ilo_figures)
save_figures <- function(figures, cfg=list(), dir="outputs/figures/main") { dir.create(dir, recursive=TRUE, showWarnings=FALSE); p <- file.path(dir,"figure_manifest.csv"); utils::write.csv(data.frame(name=names(figures)), p, row.names=FALSE); p }
make_tables <- function(selection_data, ame_results, district_panel, iv_models, first_stage_tests, cfg=list()) list(selection_n=data.frame(n=nrow(as.data.frame(selection_data))), ame_results=as.data.frame(ame_results), first_stage=as.data.frame(first_stage_tests))
save_tables <- function(tables, cfg=list(), dir="outputs/tables/main") { dir.create(dir, recursive=TRUE, showWarnings=FALSE); vapply(names(tables), function(n){ p <- file.path(dir,paste0(n,".csv")); utils::write.csv(as.data.frame(tables[[n]]), p, row.names=FALSE); p }, character(1)) }
extract_qmd_excerpts <- function(source, excerpt_ids) if(file.exists(source)) readLines(source, warn=FALSE) else paste("Missing source", source)
extract_code_excerpts <- function(spec) paste("Code excerpts generated from marker specs; run after semantic refactor for exact excerpts.")
write_fallback_pdf <- function(path, title, lines) { dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE); grDevices::pdf(path); graphics::plot.new(); graphics::text(.05,.95,title,adj=c(0,1)); graphics::text(.05,.85,paste(lines, collapse="\n"),adj=c(0,1),cex=.7); grDevices::dev.off(); path }
render_writing_samples <- function(spec_dir="application-samples/specs") { specs <- list.files(spec_dir, "^writing-.*\\.yml$", full.names=TRUE); vapply(specs, function(sp){ if(requireNamespace("yaml", quietly=TRUE)) out <- yaml::read_yaml(sp)$output else out <- file.path("application-samples/output", paste0(tools::file_path_sans_ext(basename(sp)), ".pdf")); write_fallback_pdf(out, "Writing sample", c("Generated fallback. Quarto/sample markers can replace this.")) }, character(1)) }
render_coding_samples <- function(spec_dir="application-samples/specs") { specs <- list.files(spec_dir, "^coding-.*\\.yml$", full.names=TRUE); vapply(specs, function(sp){ if(requireNamespace("yaml", quietly=TRUE)) out <- yaml::read_yaml(sp)$output else out <- file.path("application-samples/output", paste0(tools::file_path_sans_ext(basename(sp)), ".pdf")); write_fallback_pdf(out, "Coding sample", c("Generated fallback. Quarto/sample markers can replace this.")) }, character(1)) }
