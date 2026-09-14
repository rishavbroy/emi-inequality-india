# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

table_output_dir <- function(cfg) {
  "outputs/tables/main"
}

table_formats <- function(cfg) {
  out <- cfg$output_formats$tables %||% "csv"
  # yaml::read_yaml() represents sequence values as lists, not necessarily as
  # atomic character vectors.  Calling as.character() directly on that list
  # emits "argument is not an atomic vector; coercing", which targets records as
  # a warning on the file target even when all table files are written correctly.
  out <- unlist(out, recursive = TRUE, use.names = FALSE)
  out <- as.character(out)
  unique(out[nzchar(out)])
}

table_label <- function(name) {
  gsub("_", "-", name, fixed = TRUE)
}

quarto_table_label <- function(name) {
  paste0("tbl-", table_label(name))
}

nice_column_name <- function(x) {
  x <- gsub("\\.", " ", x)
  x <- gsub("_", " ", x)
  x <- gsub("p value", "p-value", x, ignore.case = TRUE)
  x <- gsub("std error", "Std. Error", x, ignore.case = TRUE)
  tools::toTitleCase(x)
}

public_longtable_notes <- function(name) {
  note <- public_table_note(name)
  if (is.null(note)) return(regression_star_note())
  c(regression_star_note(), note)
}


wrap_table_text_columns <- function(df, name = NULL) {
  # Column widths below give LaTeX's tabular engine the wrapping constraints;
  # do not inject literal `\\` breaks into cell text.
  as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
}


table_column_or_default <- function(df, column, default) {
  if (!column %in% names(df)) return(rep(default, nrow(df)))
  out <- table_contract_column_strings(df[[column]])
  out[!nzchar(out)] <- default
  out
}

is_blank_table_column <- function(col) {
  values <- table_contract_column_strings(col)
  all(!nzchar(values))
}

drop_empty_output_columns <- function(out) {
  if (!nrow(out)) return(out)
  keep <- vapply(out, function(col) !is_blank_table_column(col), logical(1))
  out[, keep, drop = FALSE]
}

format_public_summary_columns <- function(out) {
  if (all(c("var", "label") %in% names(out))) {
    out$Variable <- out$label
    group <- startsWith(table_contract_column_strings(out$var), ".group_")
    out$var <- NULL
    out$label <- NULL
    if ("desc" %in% names(out)) names(out)[names(out) == "desc"] <- "Description"
    out <- out[, c("Variable", setdiff(names(out), "Variable")), drop = FALSE]
    if (any(group)) {
      for (nm in setdiff(names(out), "Variable")) out[[nm]][group] <- ""
    }
  } else if ("desc" %in% names(out)) {
    names(out)[names(out) == "desc"] <- "Description"
  }
  rename <- c("% Mode" = "Pct. Mode", "% Least Freq." = "Pct. Least Freq.")
  for (old in names(rename)) if (old %in% names(out)) names(out)[names(out) == old] <- rename[[old]]

  # Only summary-statistic tables use the canonical summary-column ordering.
  # A standalone N column is common in result tables and must not silently move
  # ahead of their inferential columns (for example, Result/Estimate/SE/p-value).
  summary_fields <- c(
    "Variable", "Description", "Values", "Mode", "Pct. Mode",
    "Least Freq.", "Pct. Least Freq.", "Min", "1Q", "Med", "3Q",
    "Max", "Mean", "SD"
  )
  if (!any(names(out) %in% summary_fields)) return(out)

  preferred <- c(summary_fields, "N")
  ordered <- c(intersect(preferred, names(out)), setdiff(names(out), preferred))
  out[, ordered, drop = FALSE]
}


is_status_only_table <- function(out) {
  if (!nrow(out) || !"status" %in% names(out)) return(FALSE)
  substantive <- setdiff(names(out), c("status", "reason", "method", "model"))
  if (!length(substantive)) return(TRUE)
  all(vapply(out[substantive], function(col) all(!nzchar(table_contract_column_strings(col))), logical(1)))
}

format_status_table_for_output <- function(out, public = TRUE) {
  status <- table_column_or_default(out, "status", "unavailable")
  reason <- table_column_or_default(out, "reason", "No completed model output is available.")
  model <- table_column_or_default(out, "model", "output")

  if (public) {
    data.frame(
      Term = model,
      Estimate = status,
      `Std. Error` = reason,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      model = model,
      status = status,
      reason = reason,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
}

format_table_for_output <- function(table, public = TRUE) {
  out <- as.data.frame(table, check.names = FALSE)
  if (!nrow(out)) return(data.frame(Note = "No rows to display.", stringsAsFactors = FALSE))

  if (is_status_only_table(out)) {
    return(format_status_table_for_output(out, public = public))
  }

  if (!public) {
    # Internal/diagnostic CSVs are audited by exact schema. Preserve machine-
    # readable names such as std.error, p.value, conf.low, and conf.high.
    formatted <- drop_empty_output_columns(out)
    if (!length(names(formatted))) formatted <- format_status_table_for_output(out, public = FALSE)
    return(formatted)
  }

  # Public paper tables should not expose pipeline status scaffolding when
  # substantive rows exist. Pure status tables are handled above so incomplete
  # final model outputs can still be rendered and audited instead of crashing in
  # the table writer.
  out$status <- NULL
  out$reason <- NULL
  if ("method" %in% names(out) && length(unique(stats::na.omit(out$method))) <= 1L) out$method <- NULL

  out <- format_public_summary_columns(out)
  out <- drop_empty_output_columns(out)
  if (!length(names(out))) return(data.frame(Note = "No displayable columns.", stringsAsFactors = FALSE))
  already_polished <- any(names(out) %in% c("Term", "Estimate", "Std. Error", "N", "Min", "1Q", "Med", "3Q", "Max", "Mean", "SD", "Variable", "Description", "Real Log Consumption Growth", "EMI Exposure", "Enrolled (1 = yes)"))
  if (!already_polished) names(out) <- vapply(names(out), nice_column_name, character(1))
  out
}


render_table_math_labels <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    if (!is.character(df[[nm]])) next
    df[[nm]] <- gsub("$\\%\\Delta\\text{Consumption}$", "Percent change in consumption", df[[nm]], fixed = TRUE)
    df[[nm]] <- gsub("$%\\Delta\\text{Consumption}$", "Percent change in consumption", df[[nm]], fixed = TRUE)
    df[[nm]] <- gsub("$\\Delta\\text{Gini}^{\\text{Consumption}}$", "Change in Gini of consumption", df[[nm]], fixed = TRUE)
  }
  df
}

table_alignments <- function(df, name) {
  if (name %in% c("probit_mfx", "fs_cons", "cons_iv")) return(c("l", rep("c", max(0, ncol(df) - 1L))))
  if (ncol(df) <= 1L) return("l")
  c("l", rep("c", ncol(df) - 1L))
}

modelsummary_align_string <- function(df, name) {
  paste(table_alignments(df, name), collapse = "")
}

stack_estimate_se_rows <- function(df, estimate_col = "Estimate", se_col = "Std. Error") {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!all(c("Term", estimate_col, se_col) %in% names(df))) return(df)
  terms <- table_contract_column_strings(df$Term)
  estimates <- table_contract_column_strings(df[[estimate_col]])
  ses <- table_contract_column_strings(df[[se_col]])

  out <- data.frame(
    Term = rep(terms, each = 2L),
    Estimate = as.vector(rbind(estimates, ses)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out$Term[seq.int(2L, nrow(out), by = 2L)] <- ""
  out <- out[nzchar(out$Estimate), , drop = FALSE]
  rownames(out) <- NULL
  out
}


regression_rows_for_modelsummary <- function(df) {
  df <- sanitize_table_for_kable(df)
  if (ncol(df) < 2L) return(df)
  model_col <- names(df)[[2]]
  out <- data.frame(
    Term = latex_escape_text(df[[1]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out[[model_col]] <- latex_escape_text(df[[2]])
  out$Term[!nzchar(out$Term)] <- " "
  out
}


public_datasummary_table_tex <- function(df, name) {
  body <- suppress_modelsummary_latex_preamble_warning(
    modelsummary::datasummary_df(
      df,
      output = "latex_tabular",
      fmt = identity,
      align = modelsummary_align_string(df, name)
    )
  )
  body <- paste(as.character(body), collapse = "\n")
  note <- public_table_note(name)
  note_tex <- if (!is.null(note)) {
    paste0("\n\\begin{tablenotes}[flushleft]\n\\footnotesize\n\\item ", note, "\n\\end{tablenotes}")
  } else {
    ""
  }
  paste0(
    "\\begin{table}[!h]\n",
    "\\centering\n",
    "\\caption{\\label{", quarto_table_label(name), "}", table_caption(name), "}\n",
    "\\begin{threeparttable}\n",
    body,
    note_tex,
    "\n\\end{threeparttable}\n",
    "\\end{table}"
  )
}


public_table2_column_widths <- function() {
  # Table 2 is the only public summary table with several long text columns.
  # Keep explicit column constraints, but centralize them here so small width
  # adjustments do not require duplicating page-bound assumptions in the writer.
  c("3.55cm", "6.25cm", "3.2cm", "2.05cm", "3.35cm", "2.35cm", "1.25cm")
}

apply_table_column_widths <- function(tex, widths) {
  for (i in seq_along(widths)) {
    tex <- kableExtra::column_spec(tex, i, width = widths[[i]])
  }
  tex
}

add_public_longtable_notes <- function(tex, name) {
  kableExtra::footnote(
    tex,
    general = public_longtable_notes(name),
    general_title = "",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE,
    escape = FALSE
  )
}

single_space_longtable_tex <- function(tex) {
  paste(c("\\begingroup\\singlespacing", as.character(tex), "\\endgroup"), collapse = "\n")
}

public_longtable_latex_options <- function() {
  c("repeat_header", "striped", "longtable")
}


ame_modelsummary_object <- function(table) {
  native <- attr(table, "marginaleffects_object", exact = TRUE)
  if (is.null(native) || !is_marginaleffects_object(native)) return(NULL)
  native
}

ame_gof_function <- function(table) {
  n <- attr(table, "marginaleffects_n", exact = TRUE)
  n <- suppressWarnings(as.numeric(n))
  if (!length(n) || !is.finite(n[[1]])) return(NULL)

  n <- round(n[[1]])
  function(model) {
    data.frame(nobs = n, check.names = FALSE)
  }
}


ame_gof_map <- function() {
  data.frame(
    raw = "nobs",
    clean = "Observations",
    fmt = 0,
    stringsAsFactors = FALSE
  )
}


ame_modelsummary_table <- function(table, name) {
  need_pkg("modelsummary", "native marginaleffects AME table rendering")
  mfx <- ame_modelsummary_object(table)
  if (is.null(mfx)) return(NULL)
  old_knit_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  old_opt <- getOption("modelsummary_format_numeric_latex")
  old_stars_note <- getOption("modelsummary_stars_note")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_knit_to), add = TRUE)
  on.exit(options(modelsummary_format_numeric_latex = old_opt, modelsummary_stars_note = old_stars_note), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  options(modelsummary_format_numeric_latex = "plain", modelsummary_stars_note = FALSE)

  args <- list(
    # Pass the marginaleffects object itself to modelsummary, following the
    # native marginaleffects -> modelsummary integration.  We keep the same
    # kableExtra styling path as the IV regression tables.  Observations are
    # supplied through modelsummary's GOF extension hook instead of
    # manually inserting a rendered table row.
    models = list(mfx),
    coef_rename = ame_modelsummary_label,
    gof_map = ame_gof_map(),
    gof_function = ame_gof_function(table),
    stars = regression_star_levels(),
    fmt = 3,
    title = table_caption(name),
    output = "kableExtra",
    longtable = TRUE,
    escape = FALSE,
    notes = NULL
  )
  tex <- suppress_modelsummary_latex_preamble_warning(do.call(modelsummary::modelsummary, args))
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = public_longtable_latex_options(),
    position = "center",
    full_width = FALSE
  )
  tex <- kableExtra::add_header_above(tex, c(" " = 1, "Enrolled (1 = yes)" = 1))
  tex <- add_public_longtable_notes(tex, name)
  single_space_longtable_tex(tex)
}

modelsummary_regression_table <- function(df, name) {
  need_pkg("modelsummary", "standard regression table rendering")
  df <- regression_rows_for_modelsummary(df)
  if (ncol(df) < 2L) return(NULL)
  model_col <- switch(name,
    probit_mfx = "Enrolled (1 = yes)",
    fs_cons = "EMI Exposure",
    cons_iv = "Real Log Consumption Growth",
    names(df)[[2]]
  )
  names(df) <- c("Term", model_col)
  old_opt <- getOption("modelsummary_format_numeric_latex")
  on.exit(options(modelsummary_format_numeric_latex = old_opt), add = TRUE)
  options(modelsummary_format_numeric_latex = "plain")
  public_datasummary_table_tex(df, name)
}

public_regression_coef_map <- function() {
  control_meta <- census_2001_control_metadata()
  c(
    "emi_exposure_all_children_0708" = "EMI exposure (fitted)",
    "ling_distance_nonzero_mean" = "Linguistic distance",
    "EMIE" = "EMI share among enrolled (legacy)",
    "wavg_ling_degrees" = "Linguistic distance (legacy)",
    stats::setNames(control_meta$label, control_meta$variable),
    "(Intercept)" = "Intercept"
  )
}

public_modelsummary_gof_map <- function(name) {
  if (identical(name, "fs_cons")) {
    return(list(
      list(raw = "nobs", clean = "Observations", fmt = 0),
      list(raw = "r.squared", clean = "$R^2$", fmt = 3),
      list(raw = "adj.r.squared", clean = "Adjusted $R^2$", fmt = 3),
      list(raw = "sigma", clean = "Residual Std. Error", fmt = 3),
      list(raw = "statistic", clean = "Model's F-Statistic", fmt = 2)
    ))
  }
  list(
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "r.squared", clean = "$R^2$", fmt = 3),
    list(raw = "adj.r.squared", clean = "Adjusted $R^2$", fmt = 3),
    list(raw = "sigma", clean = "Residual Std. Error", fmt = 3),
    list(raw = "waldtest", clean = "F-Statistic", fmt = 2)
  )
}

modelsummary_payload <- function(model, vcov_matrix = NULL) {
  coefs <- coefficient_frame(model, vcov_matrix)
  if (!nrow(coefs)) {
    stop("Cannot render a regression table without model coefficients.", call. = FALSE)
  }

  tidy <- data.frame(
    term = rownames(coefs),
    estimate = unname(suppressWarnings(as.numeric(coefs$Estimate))),
    std.error = unname(suppressWarnings(as.numeric(coefs[["Std. Error"]]))),
    statistic = unname(suppressWarnings(as.numeric(coefs$statistic))),
    p.value = unname(suppressWarnings(as.numeric(coefs[["Pr(>|t|)"]]))),
    stringsAsFactors = FALSE
  )

  structure(
    list(tidy = tidy, glance = model_gof_frame(model)),
    class = "modelsummary_list"
  )
}

public_modelsummary_table <- function(model, name, vcov_matrix = NULL, add_rows = NULL) {
  need_pkg("modelsummary", "legacy regression table rendering")
  old_knit_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  old_opt <- getOption("modelsummary_format_numeric_latex")
  old_stars_note <- getOption("modelsummary_stars_note")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_knit_to), add = TRUE)
  on.exit(options(
    modelsummary_format_numeric_latex = old_opt,
    modelsummary_stars_note = old_stars_note
  ), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  options(
    modelsummary_format_numeric_latex = "plain",
    modelsummary_stars_note = FALSE
  )
  args <- list(
    models = modelsummary_payload(model, vcov_matrix),
    coef_map = public_regression_coef_map(),
    gof_map = public_modelsummary_gof_map(name),
    stars = regression_star_levels(),
    fmt = 3,
    title = table_caption(name),
    output = "kableExtra",
    longtable = TRUE,
    escape = FALSE,
    notes = NULL
  )
  if (!is.null(add_rows)) args$add_rows <- add_rows
  tex <- suppress_modelsummary_latex_preamble_warning(do.call(modelsummary::modelsummary, args))
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = public_longtable_latex_options(),
    position = "center",
    full_width = FALSE
  )
  header <- switch(name,
    fs_cons = c(" " = 1, "EMI Exposure" = 1),
    cons_iv = c(" " = 1, "Real Log Consumption Growth" = 1)
  )
  tex <- kableExtra::add_header_above(tex, header)
  add_public_longtable_notes(tex, name)
}

regression_standard_error_rows <- function(df) {
  if (!"Term" %in% names(df) || ncol(df) < 2L) return(integer())
  terms <- table_contract_column_strings(df$Term)
  vals <- table_contract_column_strings(df[[2]])
  which((is.na(terms) | !nzchar(terms)) & grepl("^\\(", vals))
}

sanitize_table_for_kable <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(df)) df <- data.frame(Note = "No rows to display.", stringsAsFactors = FALSE)
  if (!length(names(df))) df <- data.frame(Note = rep("No displayable columns.", nrow(df)), stringsAsFactors = FALSE)
  for (nm in names(df)) {
    df[[nm]] <- table_contract_column_strings(df[[nm]])
  }
  render_table_math_labels(df)
}

escape_table_for_latex <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  for (nm in names(df)) df[[nm]] <- latex_escape_text(df[[nm]])
  names(df) <- latex_escape_text(names(df))
  df
}

regression_summary_start <- function(df) {
  if (!"Term" %in% names(df)) return(NA_integer_)
  terms <- table_contract_column_strings(df$Term)
  hit <- which(terms %in% c(
    "Observations", "R-squared", "Adjusted R-squared",
    "Instrument's clustered Wald F", "Montiel Olea-Pflueger effective F",
    "MOP 5% critical value (10% relative bias)",
    "Instrument's F-Statistic", "Model's F-Statistic", "F-Statistic"
  ))
  if (length(hit)) hit[[1]] else NA_integer_
}

style_regression_table <- function(tex, df, name) {
  if (!name %in% c("probit_mfx", "fs_cons", "cons_iv")) return(tex)
  if (nrow(df)) {
    tex <- kableExtra::row_spec(tex, seq_len(nrow(df)), background = "white")
  }
  # Keep standard-error rows in the same roman face as modelsummary's native
  # regression output. Italic grey SE rows made the AME table visually diverge
  # from the public modelsummary tables.
  start <- regression_summary_start(df)
  if (is.finite(start) && start > 1L) {
    tex <- kableExtra::row_spec(tex, start - 1L, hline_after = TRUE)
  }
  if (is.finite(start) && start <= nrow(df)) {
    tex <- kableExtra::row_spec(tex, start:nrow(df), background = "white")
  }
  tex
}

suppress_atomic_vector_coercion_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (identical(conditionMessage(w), "argument is not an atomic vector; coercing")) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

table_csv_data <- function(table, public = TRUE) {
  raw <- as.data.frame(table, check.names = FALSE, stringsAsFactors = FALSE)
  if (isTRUE(public) && is_status_only_table(raw)) {
    return(format_status_table_for_output(raw, public = TRUE))
  }

  payload <- attr(table, "csv_data", exact = TRUE)
  if (!is.null(payload)) {
    payload <- as.data.frame(payload, check.names = FALSE, stringsAsFactors = FALSE)
    if (nrow(payload)) return(payload)
  }

  # CSV is the machine-readable companion to the rendered table, not another
  # presentation format. Keep source-oriented names and values whenever a table
  # does not provide a more specific payload. LaTeX styling is applied only by
  # save_table_tex().
  format_table_for_output(table, public = FALSE)
}

save_table_csv <- function(table, path, public = TRUE) {
  utils::write.csv(table_csv_data(table, public = public), path, row.names = FALSE)
  path
}

is_formatted_status_table <- function(df) {
  all(c("Term", "Estimate", "Std. Error") %in% names(df)) &&
    any(table_contract_column_strings(df$Estimate) %in% c("out_of_active_pipeline", "unavailable", "not_run", "failed"))
}

suppress_modelsummary_latex_preamble_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("To compile a LaTeX document with this table", msg, fixed = TRUE) ||
          grepl("latex_siunitx_preamble", msg, fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

normalize_quarto_table_labels <- function(tex, name) {
  tex <- as.character(tex)
  label <- table_label(name)
  tex <- gsub(
    paste0("\\\\label\\{tab:", label, "\\}"),
    paste0("\\\\label{", quarto_table_label(name), "}"),
    tex
  )
  if (!any(grepl("\\\\label\\{", tex))) {
    tex <- sub(
      "\\\\caption\\{",
      paste0("\\\\caption{\\\\label{", quarto_table_label(name), "}"),
      tex
    )
  }
  tex
}

write_table_tex <- function(tex, path, name) {
  writeLines(normalize_quarto_table_labels(tex, name), path)
  path
}

paper_schooling_market_modelsummary_table <- function(table, name) {
  need_pkg("modelsummary", "schooling-market regression table rendering")
  csv <- safe_df(attr(table, "csv_data", exact = TRUE))
  if (!nrow(csv)) return(NULL)

  required <- c(
    "panel", "measure_id", "measure", "specification_id", "estimate",
    "std_error", "p_value", "n"
  )
  missing <- setdiff(required, names(csv))
  if (length(missing)) {
    stop(
      "Paper schooling-market regression table is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  measures <- paper_schooling_market_measure_registry()
  specs <- c("unadjusted", "region_main", "state_main")
  model_names <- c("(1)", "(2)", "(3)")
  state_rows <- csv[csv$panel == "state_organization", , drop = FALSE]

  models <- lapply(seq_len(nrow(measures)), function(i) {
    id <- measures$measure_id[[i]]
    rows <- csv[
      csv$panel == "association" & csv$measure_id == id &
        csv$specification_id %in% specs,
      , drop = FALSE
    ]
    rows <- rows[match(specs, rows$specification_id), , drop = FALSE]
    if (nrow(rows) != length(specs) || any(is.na(rows$specification_id))) {
      stop("Paper schooling-market regression table lost a registered specification for ", id, ".", call. = FALSE)
    }

    state_row <- state_rows[state_rows$measure_id == id, , drop = FALSE]
    state_r2 <- if (nrow(state_row) == 1L) num(state_row$estimate[[1L]]) else NA_real_

    outcome_models <- lapply(seq_along(specs), function(j) {
      structure(
        list(
          tidy = data.frame(
            term = "linguistic_distance",
            estimate = num(rows$estimate[[j]]),
            std.error = num(rows$std_error[[j]]),
            p.value = num(rows$p_value[[j]]),
            stringsAsFactors = FALSE
          ),
          glance = data.frame(
            region_fe = c("No", "Yes", "No")[[j]],
            state_fe = c("No", "No", "Yes")[[j]],
            controls = c("No", "Yes", "Yes")[[j]],
            nobs = as.integer(rows$n[[j]]),
            state_membership_r2 = state_r2,
            stringsAsFactors = FALSE
          )
        ),
        class = "modelsummary_list"
      )
    })
    names(outcome_models) <- model_names
    outcome_models
  })
  names(models) <- measures$label

  yes_no <- function(x) as.character(x)
  gof_map <- list(
    list(raw = "region_fe", clean = "Region fixed effects", fmt = yes_no),
    list(raw = "state_fe", clean = "State fixed effects", fmt = yes_no),
    list(raw = "controls", clean = "Predetermined controls", fmt = yes_no),
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "state_membership_r2", clean = "State-membership $R^2$", fmt = 3)
  )

  old_knit_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  old_opt <- getOption("modelsummary_format_numeric_latex")
  old_stars_note <- getOption("modelsummary_stars_note")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_knit_to), add = TRUE)
  on.exit(options(
    modelsummary_format_numeric_latex = old_opt,
    modelsummary_stars_note = old_stars_note
  ), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  options(
    modelsummary_format_numeric_latex = "plain",
    modelsummary_stars_note = FALSE
  )

  tex <- suppress_modelsummary_latex_preamble_warning(modelsummary::modelsummary(
    models = models,
    shape = "rbind",
    coef_map = c("linguistic_distance" = "Linguistic distance from Hindi"),
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    stars = regression_star_levels(),
    fmt = 3,
    gof_map = gof_map,
    title = table_caption(name),
    output = "kableExtra",
    longtable = TRUE,
    escape = FALSE,
    notes = NULL
  ))
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = c("repeat_header", "striped"),
    position = "center",
    full_width = FALSE,
    font_size = 9
  )
  tex <- tex |>
    kableExtra::column_spec(1, width = "6.0cm") |>
    kableExtra::column_spec(2:4, width = "2.4cm")
  note <- public_table_note(name)
  if (!is.null(note)) {
    tex <- kableExtra::footnote(
      tex,
      general = note,
      general_title = "",
      threeparttable = TRUE,
      footnote_as_chunk = TRUE,
      escape = FALSE
    )
  }
  single_space_longtable_tex(tex)
}

paper_economic_conversion_modelsummary_table <- function(table, name) {
  need_pkg("modelsummary", "economic-conversion regression table rendering")
  csv <- safe_df(attr(table, "csv_data", exact = TRUE))
  if (!nrow(csv)) return(NULL)

  required <- c(
    "panel", "measure", "predictor_id", "complement_id", "outcome_round",
    "estimand", "estimate", "std.error", "p.value_holm", "n"
  )
  missing <- setdiff(required, names(csv))
  if (length(missing)) {
    stop(
      "Paper economic-conversion regression table is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  columns <- paper_schooling_welfare_column_registry()
  treatments <- paper_schooling_welfare_treatment_labels()
  complement_registry <- paper_conversion_complement_registry()
  model_names <- paste0("(", seq_len(nrow(columns)), ")")

  schooling_terms <- setNames(
    paste0("schooling__", names(treatments)),
    names(treatments)
  )
  complement_labels <- c()
  for (predictor in c("all_child_emi", "private_emi")) {
    prefix <- if (predictor == "all_child_emi") "All-child EMI" else "Private EMI"
    for (i in seq_len(nrow(complement_registry))) {
      modifier <- complement_registry$modifier_id[[i]]
      term <- paste("interaction", predictor, modifier, sep = "__")
      complement_labels <- c(
        complement_labels,
        setNames(paste0(prefix, " $\\times$ ", complement_registry$label[[i]]), term)
      )
    }
  }

  coef_map <- c(
    setNames(unname(treatments), unname(schooling_terms)),
    complement_labels
  )

  models <- lapply(seq_len(nrow(columns)), function(j) {
    welfare <- csv[
      csv$panel == "schooling_welfare" &
        csv$outcome_round == columns$outcome_round[[j]] &
        csv$estimand == columns$estimand[[j]],
      , drop = FALSE
    ]
    welfare <- welfare[match(names(treatments), welfare$predictor_id), , drop = FALSE]
    if (nrow(welfare) != length(treatments) || any(is.na(welfare$predictor_id))) {
      stop("Paper economic-conversion table lost a registered schooling-welfare coefficient.", call. = FALSE)
    }

    tidy <- data.frame(
      term = unname(schooling_terms[welfare$predictor_id]),
      estimate = num(welfare$estimate),
      std.error = num(welfare$std.error),
      p.value = num(welfare$p.value_holm),
      stringsAsFactors = FALSE
    )

    if (columns$outcome_round[[j]] == "hces_2022_23" && columns$estimand[[j]] == "change") {
      complements <- csv[csv$panel == "predetermined_complements", , drop = FALSE]
      expected <- expand.grid(
        predictor_id = c("all_child_emi", "private_emi"),
        complement_id = complement_registry$modifier_id,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
      key <- paste(complements$predictor_id, complements$complement_id)
      expected_key <- paste(expected$predictor_id, expected$complement_id)
      complements <- complements[match(expected_key, key), , drop = FALSE]
      if (nrow(complements) != nrow(expected) || any(is.na(complements$predictor_id))) {
        stop("Paper economic-conversion table lost a registered complement coefficient.", call. = FALSE)
      }
      tidy <- rbind(
        tidy,
        data.frame(
          term = paste("interaction", complements$predictor_id, complements$complement_id, sep = "__"),
          estimate = num(complements$estimate),
          std.error = num(complements$std.error),
          p.value = num(complements$p.value_holm),
          stringsAsFactors = FALSE
        )
      )
    }

    nvals <- unique(welfare$n)
    if (length(nvals) != 1L) {
      stop("Paper economic-conversion table lost common support within a welfare column.", call. = FALSE)
    }
    structure(
      list(
        tidy = tidy,
        glance = data.frame(
          endpoint = c(
            hces_2022_23 = "HCES 2022-23",
            hces_2023_24 = "HCES 2023-24"
          )[[columns$outcome_round[[j]]]],
          estimand = if (columns$estimand[[j]] == "ancova") {
            "ANCOVA"
          } else if (columns$outcome_round[[j]] == "hces_2022_23") {
            "2004-05 to 2022-23 change"
          } else {
            "2004-05 to 2023-24 change"
          },
          state_fe = "Yes",
          controls = "Yes",
          nobs = as.integer(nvals[[1L]]),
          stringsAsFactors = FALSE
        )
      ),
      class = "modelsummary_list"
    )
  })
  names(models) <- model_names

  as_text <- function(x) as.character(x)
  gof_map <- list(
    list(raw = "endpoint", clean = "Endpoint", fmt = as_text),
    list(raw = "estimand", clean = "Estimand", fmt = as_text),
    list(raw = "state_fe", clean = "State fixed effects", fmt = as_text),
    list(raw = "controls", clean = "Predetermined controls", fmt = as_text),
    list(raw = "nobs", clean = "Observations", fmt = 0)
  )

  old_knit_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  old_opt <- getOption("modelsummary_format_numeric_latex")
  old_stars_note <- getOption("modelsummary_stars_note")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_knit_to), add = TRUE)
  on.exit(options(
    modelsummary_format_numeric_latex = old_opt,
    modelsummary_stars_note = old_stars_note
  ), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  options(
    modelsummary_format_numeric_latex = "plain",
    modelsummary_stars_note = FALSE
  )

  tex <- suppress_modelsummary_latex_preamble_warning(modelsummary::modelsummary(
    models = models,
    coef_map = coef_map,
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    stars = regression_star_levels(),
    fmt = 2,
    gof_map = gof_map,
    title = table_caption(name),
    output = "kableExtra",
    longtable = TRUE,
    escape = FALSE,
    notes = NULL
  ))
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = c("repeat_header", "striped"),
    position = "center",
    full_width = FALSE,
    font_size = 9
  )
  tex <- tex |>
    kableExtra::column_spec(1, width = "5.6cm") |>
    kableExtra::column_spec(2:5, width = "2.25cm")
  note <- public_table_note(name)
  if (!is.null(note)) {
    tex <- kableExtra::footnote(
      tex,
      general = note,
      general_title = "",
      threeparttable = TRUE,
      footnote_as_chunk = TRUE,
      escape = FALSE
    )
  }
  single_space_longtable_tex(tex)
}

paper_local_development_modelsummary_table <- function(table, name) {
  need_pkg("modelsummary", "local-development regression table rendering")
  csv <- safe_df(attr(table, "csv_data", exact = TRUE))
  if (!nrow(csv)) return(NULL)

  required <- c(
    "row_id", "outcome", "adjustment_id", "estimate", "std.error",
    "p.value_holm", "n"
  )
  missing <- setdiff(required, names(csv))
  if (length(missing)) {
    stop(
      "Paper local-development regression table is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  panel_specs <- list(
    "Panel A: Household capacity and assets" = data.frame(
      row_id = c(
        "household_literacy_depth", "household_graduate_access",
        "finance_banking_access", "asset_television_ownership"
      ),
      outcome = c(
        "2+ literates", "Graduate access", "Banking access", "Television ownership"
      ),
      period = rep("2001-11", 4L),
      stringsAsFactors = FALSE
    ),
    "Panel B: Migration and sectoral composition" = data.frame(
      row_id = c(
        "migration_skilled_recent_work", "migration_interstate_composition",
        "economic_services_share", "economic_manufacturing_share"
      ),
      outcome = c(
        "Skilled recent-work migrant share", "Interstate migrant share",
        "Services employment share", "Manufacturing employment share"
      ),
      period = c("2011", "2011", "2005-13", "2005-13"),
      stringsAsFactors = FALSE
    ),
    "Panel C: Employment scale and labor outcomes" = data.frame(
      row_id = c(
        "economic_nonfarm_employment", "labor_lfpr", "labor_employment_rate"
      ),
      outcome = c(
        "Log nonfarm employment", "Labor-force participation", "Employment rate"
      ),
      period = c("2005-13", "2017-18", "2017-18"),
      stringsAsFactors = FALSE
    )
  )

  make_model <- function(spec) {
    row <- csv[csv$row_id == spec$row_id, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(
        "Paper local-development regression table lost registered row ",
        spec$row_id, ".", call. = FALSE
      )
    }
    fixed_effects <- switch(
      row$adjustment_id[[1L]],
      state_main = "State",
      region_main = "Region",
      stop(
        "Paper local-development regression table has unsupported adjustment ",
        row$adjustment_id[[1L]], ".", call. = FALSE
      )
    )
    structure(
      list(
        tidy = data.frame(
          term = "linguistic_distance",
          estimate = num(row$estimate[[1L]]),
          std.error = num(row$std.error[[1L]]),
          p.value = num(row$p.value_holm[[1L]]),
          stringsAsFactors = FALSE
        ),
        glance = data.frame(
          outcome = spec$outcome,
          period = spec$period,
          fixed_effects = fixed_effects,
          controls = "Yes",
          nobs = as.integer(row$n[[1L]]),
          stringsAsFactors = FALSE
        )
      ),
      class = "modelsummary_list"
    )
  }

  panels <- lapply(panel_specs, function(specs) {
    models <- lapply(seq_len(nrow(specs)), function(i) make_model(specs[i, , drop = FALSE]))
    names(models) <- paste0("(", seq_along(models), ")")
    models
  })

  as_text <- function(x) as.character(x)
  gof_map <- list(
    list(raw = "outcome", clean = "Dependent variable", fmt = as_text),
    list(raw = "period", clean = "Period", fmt = as_text),
    list(raw = "fixed_effects", clean = "Fixed effects", fmt = as_text),
    list(raw = "controls", clean = "Predetermined controls", fmt = as_text),
    list(raw = "nobs", clean = "Observations", fmt = 0)
  )

  old_knit_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  old_opt <- getOption("modelsummary_format_numeric_latex")
  old_stars_note <- getOption("modelsummary_stars_note")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_knit_to), add = TRUE)
  on.exit(options(
    modelsummary_format_numeric_latex = old_opt,
    modelsummary_stars_note = old_stars_note
  ), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  options(
    modelsummary_format_numeric_latex = "plain",
    modelsummary_stars_note = FALSE
  )

  tex <- suppress_modelsummary_latex_preamble_warning(modelsummary::modelsummary(
    models = panels,
    shape = "rbind",
    coef_map = c("linguistic_distance" = "Linguistic distance from Hindi"),
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    stars = regression_star_levels(),
    fmt = 4,
    gof_map = gof_map,
    title = table_caption(name),
    output = "kableExtra",
    longtable = TRUE,
    escape = FALSE,
    notes = NULL
  ))
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = c("repeat_header", "striped"),
    position = "center",
    full_width = FALSE,
    font_size = 9
  )
  tex <- tex |>
    kableExtra::column_spec(1, width = "4.5cm") |>
    kableExtra::column_spec(2:5, width = "2.7cm")
  note <- public_table_note(name)
  if (!is.null(note)) {
    tex <- kableExtra::footnote(
      tex,
      general = note,
      general_title = "",
      threeparttable = TRUE,
      footnote_as_chunk = TRUE,
      escape = FALSE
    )
  }
  single_space_longtable_tex(tex)
}

paper_language_behavior_modelsummary_table <- function(table, name) {
  need_pkg("modelsummary", "language-behavior regression table rendering")
  csv <- attr(table, "csv_data", exact = TRUE)
  csv <- safe_df(csv)
  if (!nrow(csv)) return(NULL)

  required <- c(
    "model_number", "term", "estimate", "std.error", "p.value",
    "partial_r_squared", "n", "outcome", "population", "sample"
  )
  missing <- setdiff(required, names(csv))
  if (length(missing)) {
    stop(
      "Paper language-behavior regression table is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  csv <- csv[order(csv$model_number), , drop = FALSE]
  if (!identical(csv$model_number, seq_len(nrow(csv)))) {
    stop("Paper language-behavior model numbers must be consecutive.", call. = FALSE)
  }

  model_names <- paste0("(", csv$model_number, ")")
  models <- lapply(seq_len(nrow(csv)), function(i) {
    structure(
      list(
        tidy = data.frame(
          term = csv$term[[i]],
          estimate = csv$estimate[[i]],
          std.error = csv$std.error[[i]],
          p.value = csv$p.value[[i]],
          stringsAsFactors = FALSE
        ),
        glance = data.frame(nobs = csv$n[[i]], stringsAsFactors = FALSE)
      ),
      class = "modelsummary_list"
    )
  })
  names(models) <- model_names

  add_rows <- data.frame(
    term = c(
      "Outcome", "Population", "Observations", "Partial $R^2$",
      "State fixed effects", "Language-share controls"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(csv))) {
    add_rows[[model_names[[i]]]] <- c(
      csv$outcome[[i]],
      csv$population[[i]],
      format(csv$n[[i]], big.mark = ",", scientific = FALSE),
      sprintf("%.3f", csv$partial_r_squared[[i]]),
      "Yes",
      "Yes"
    )
  }

  old_knit_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  old_opt <- getOption("modelsummary_format_numeric_latex")
  old_stars_note <- getOption("modelsummary_stars_note")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_knit_to), add = TRUE)
  on.exit(options(
    modelsummary_format_numeric_latex = old_opt,
    modelsummary_stars_note = old_stars_note
  ), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  options(
    modelsummary_format_numeric_latex = "plain",
    modelsummary_stars_note = FALSE
  )

  tex <- suppress_modelsummary_latex_preamble_warning(modelsummary::modelsummary(
    models = models,
    coef_map = c(
      "shastry_degree" = "Linguistic distance from Hindi",
      "distance_distant" = "Distant-language indicator"
    ),
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    stars = regression_star_levels(),
    fmt = 3,
    gof_omit = ".*",
    add_rows = add_rows,
    title = table_caption(name),
    output = "kableExtra",
    longtable = FALSE,
    escape = FALSE,
    notes = NULL
  ))
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = c("HOLD_position", "striped"),
    position = "center",
    full_width = FALSE,
    font_size = 9
  )
  tex <- kableExtra::add_header_above(
    tex,
    c(" " = 1, "All states" = 4, "Hindi-belt states" = 3),
    bold = TRUE,
    escape = FALSE
  )
  tex <- tex |>
    kableExtra::column_spec(1, width = "4.2cm") |>
    kableExtra::column_spec(2:8, width = "2.35cm")
  note <- public_table_note(name)
  if (!is.null(note)) {
    tex <- kableExtra::footnote(
      tex,
      general = note,
      general_title = "",
      threeparttable = TRUE,
      footnote_as_chunk = TRUE,
      escape = FALSE
    )
  }
  tex
}

save_table_tex <- function(table, path, name, public = TRUE) {
  need_pkg("kableExtra", "LaTeX table output")
  table_model <- attr(table, "table_model")
  if (identical(name, "paper_schooling_market_geography")) {
    tex <- paper_schooling_market_modelsummary_table(table, name)
    if (!is.null(tex)) return(write_table_tex(tex, path, name))
  }
  if (identical(name, "paper_economic_conversion")) {
    tex <- paper_economic_conversion_modelsummary_table(table, name)
    if (!is.null(tex)) return(write_table_tex(tex, path, name))
  }
  if (identical(name, "paper_language_behavior")) {
    tex <- paper_language_behavior_modelsummary_table(table, name)
    if (!is.null(tex)) return(write_table_tex(tex, path, name))
  }
  if (identical(name, "paper_local_development")) {
    tex <- paper_local_development_modelsummary_table(table, name)
    if (!is.null(tex)) return(write_table_tex(tex, path, name))
  }
  if (name %in% c("fs_cons", "cons_iv") && !is.null(table_model) && !is_formatted_status_table(as.data.frame(table, check.names = FALSE))) {
    tex <- public_modelsummary_table(
      table_model,
      name,
      vcov_matrix = attr(table, "table_vcov"),
      add_rows = attr(table, "table_add_rows")
    )
    return(write_table_tex(tex, path, name))
  }
  if (identical(name, "probit_mfx") && !is_formatted_status_table(as.data.frame(table, check.names = FALSE))) {
    tex <- ame_modelsummary_table(table, name)
    if (!is.null(tex)) return(write_table_tex(tex, path, name))
  }
  df <- sanitize_table_for_kable(format_table_for_output(table, public = public))
  grouped <- summary_table_groups(df)
  df_render <- wrap_table_text_columns(grouped$data, name)
  single_page_wide_table <- identical(name, "paper_core_summary")
  landscape_longtable <- name %in% c(
    "sum_tbl_iv", "sum_tbl_probit_quant", "sum_tbl_probit_cat"
  )
  landscape_table <- landscape_longtable
  regression_table <- name %in% c("probit_mfx", "fs_cons", "cons_iv") && !is_formatted_status_table(df_render)
  compact_result_table <- identical(name, "paper_identification_boundary")
  appendix_compact_table <- name %in% c(
    "appendix_a3_lineage_source_hierarchy",
    "appendix_a6_linguistic_measures", "appendix_a7_consumption_construction",
    "appendix_b1_lineage_readiness", "appendix_b2_lineage_sensitivity", "appendix_b3_consumption_reconstruction", "appendix_b4_hces_consistency_summary", "appendix_b6_language_source_validation",
    "appendix_b8_census_universe_reconciliation",
    "appendix_c1_full_absorption_ladder", "appendix_c3_control_block_absorption",
    "appendix_c4_geographic_scale_sensitivity", "appendix_c5_alternative_scalar_distances",
    "appendix_c6_mapping_composition_sensitivity", "appendix_c7_historical_balance",
    "appendix_c9_historical_first_stage", "appendix_c11_multiple_instruments",
    "appendix_c13_robustness_family_census", "appendix_c14_exclusion_sensitivity",
    "appendix_d1_migration", "appendix_d2_migration_context",
    "appendix_d3_housing_assets", "appendix_d4_economic_census",
    "appendix_d5_labor", "appendix_d6_household_capacity",
    "appendix_d7_social_heterogeneity", "appendix_d9_residual_spatial_diagnostics",
    "appendix_e1_selection_sample", "appendix_e4_missingness"
  )
  appendix_long_table <- identical(name, "appendix_c1_full_absorption_ladder")
  if (!regression_table) {
    names(df_render) <- table_header_labels(df_render, name)
  }
  if (identical(name, "probit_mfx") && !is_formatted_status_table(df_render)) {
    df_render <- stack_estimate_se_rows(df_render)
    tex <- modelsummary_regression_table(df_render, name)
    return(write_table_tex(tex, path, name))
  }

  # kableExtra requires escape = FALSE below because header/group styling emits
  # LaTeX. Escape ordinary cell text first so `%`, `&`, `_`, and other LaTeX
  # metacharacters cannot corrupt the alignment. kableExtra's own documentation
  # explicitly requires manual escaping when raw LaTeX output is enabled.
  df_render <- escape_table_for_latex(df_render)
  tex <- kableExtra::kbl(
    df_render,
    format = "latex",
    booktabs = TRUE,
    longtable = landscape_longtable || regression_table || appendix_long_table,
    label = table_label(name),
    caption = caption_for_latex(name),
    escape = FALSE,
    linesep = "",
    digits = 3,
    align = table_alignments(df_render, name),
    row.names = FALSE
  )
  if (regression_table) {
    latex_options <- c("hold_position", "repeat_header", "striped")
  } else if (compact_result_table || appendix_compact_table) {
    # Keep compact paper-result tables visually neutral. Semantic panel grouping
    # and parenthesized standard errors already provide the needed row structure.
    latex_options <- c("repeat_header")
  } else if (single_page_wide_table) {
    # Short landscape tables stay non-breaking here; paper-new.qmd owns page
    # orientation through Quarto's native .landscape block.
    latex_options <- c("HOLD_position", "striped")
  } else if (landscape_longtable) {
    # Genuinely long landscape tables remain non-floating longtables so they
    # can break across pages without escaping the pdflscape environment.
    latex_options <- c("repeat_header", "striped")
  } else {
    latex_options <- c("striped", "repeat_header")
  }
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = latex_options,
    full_width = FALSE,
    position = "center",
    font_size = if (single_page_wide_table || landscape_table || regression_table || compact_result_table || appendix_compact_table) 9 else NULL
  )
  if (nrow(grouped$groups)) {
    for (i in rev(seq_len(nrow(grouped$groups)))) {
      tex <- kableExtra::pack_rows(
        tex,
        grouped$groups$label[[i]],
        grouped$groups$start[[i]],
        grouped$groups$end[[i]],
        bold = TRUE,
        italic = FALSE,
        background = "white",
        escape = FALSE
      )
    }
  }
  if (name == "sum_tbl_probit_cat") {
    tex <- apply_table_column_widths(tex, public_table2_column_widths())
  }
  if (name == "sum_tbl_iv") {
    tex <- tex |>
      kableExtra::column_spec(1, width = "3.5cm") |>
      kableExtra::column_spec(2, width = "5.0cm") |>
      kableExtra::column_spec(3:ncol(df_render), width = "1.55cm")
  }
  if (name == "sum_tbl_probit_quant") {
    tex <- tex |>
      kableExtra::column_spec(1, width = "5.4cm") |>
      kableExtra::column_spec(2:ncol(df_render), width = "2.0cm")
  }
  if (compact_result_table) {
    widths <- switch(
      name,
      paper_identification_boundary = c("3.5cm", "2.2cm", "1.1cm", "1.1cm", "1.3cm", "4.6cm")
    )
    tex <- apply_table_column_widths(tex, widths)
  }
  if (appendix_compact_table) {
    widths <- switch(
      name,
      appendix_a3_lineage_source_hierarchy = c("4.1cm", "3.5cm", "7.0cm"),
      appendix_a6_linguistic_measures = c("3.2cm", "3.6cm", "4.6cm", "2.5cm"),
      appendix_a7_consumption_construction = c("3.4cm", "1.9cm", "2.2cm", "2.2cm", "3.0cm", "2.0cm"),
      appendix_b1_lineage_readiness = c("3.6cm", "6.8cm", "2.0cm"),
      appendix_b2_lineage_sensitivity = c("3.2cm", "1.8cm", "2.2cm", "2.8cm", "2.5cm"),
      appendix_b3_consumption_reconstruction = c("2.3cm", "1.4cm", "1.3cm", "2.0cm", "2.0cm", "2.0cm"),
      appendix_b4_hces_consistency_summary = c("6.0cm", "1.8cm", "3.0cm"),
      appendix_b6_language_source_validation = c("4.0cm", "1.2cm", "2.7cm", "1.4cm", "3.3cm", "1.6cm"),
      appendix_b8_census_universe_reconciliation = c("1.8cm", "1.2cm", "4.4cm", "2.6cm", "3.6cm", "1.2cm"),
      appendix_c1_full_absorption_ladder = c("3.7cm", "1.0cm", "3.8cm", "1.1cm", "1.1cm", "1.0cm", "1.4cm", "0.9cm"),
      appendix_c3_control_block_absorption = c("1.7cm", "3.7cm", "2.1cm", "2.6cm", "2.2cm"),
      appendix_c4_geographic_scale_sensitivity = c("2.2cm", "3.0cm", "2.8cm", "1.6cm", "2.1cm", "2.2cm"),
      appendix_c5_alternative_scalar_distances = c("5.0cm", "3.0cm", "1.5cm", "1.7cm", "1.2cm"),
      appendix_c6_mapping_composition_sensitivity = c("3.0cm", "5.4cm", "2.2cm", "1.4cm", "1.7cm", "1.1cm"),
      appendix_c7_historical_balance = c("2.8cm", "2.5cm", "1.5cm", "1.4cm", "1.4cm", "1.2cm"),
      appendix_c9_historical_first_stage = c("5.0cm", "1.5cm", "1.9cm", "1.5cm", "1.9cm", "1.2cm"),
      appendix_c11_multiple_instruments = c("3.7cm", "1.3cm", "1.5cm", "1.5cm", "1.3cm", "1.8cm", "1.7cm", "1.0cm"),
      appendix_c13_robustness_family_census = c("3.5cm", "1.0cm", "1.5cm", "1.6cm", "1.5cm", "1.5cm", "1.5cm", "1.4cm"),
      appendix_c14_exclusion_sensitivity = c("1.4cm", "1.5cm", "2.4cm", "1.7cm", "1.8cm", "2.2cm", "2.3cm"),
      appendix_d1_migration = c("3.5cm", "2.0cm", "2.0cm", "1.3cm", "1.2cm", "1.2cm", "1.2cm", "1.0cm"),
      appendix_d2_migration_context = c("2.8cm", "5.0cm", "1.5cm", "1.3cm", "1.3cm", "1.0cm"),
      appendix_d3_housing_assets = c("3.5cm", "2.0cm", "2.0cm", "1.3cm", "1.2cm", "1.2cm", "1.2cm", "1.0cm"),
      appendix_d4_economic_census = c("3.5cm", "2.0cm", "2.0cm", "1.3cm", "1.2cm", "1.2cm", "1.2cm", "1.0cm"),
      appendix_d5_labor = c("2.7cm", "3.2cm", "1.8cm", "1.8cm", "1.1cm", "1.1cm", "1.1cm", "1.1cm", "0.9cm"),
      appendix_d6_household_capacity = c("2.5cm", "2.3cm", "1.9cm", "1.2cm", "1.1cm", "1.1cm", "1.1cm", "0.9cm"),
      appendix_d7_social_heterogeneity = c("2.4cm", "1.5cm", "2.6cm", "2.2cm", "1.8cm", "1.1cm", "1.0cm", "1.1cm", "0.9cm"),
      appendix_d9_residual_spatial_diagnostics = c("4.2cm", "1.4cm", "1.4cm", "1.0cm", "1.7cm", "1.7cm"),
      appendix_e1_selection_sample = c("3.8cm", "1.7cm", "1.2cm", "8.0cm"),
      appendix_e4_missingness = c("4.5cm", "1.6cm", "1.6cm", "1.7cm", "2.6cm")
    )
    tex <- apply_table_column_widths(tex, widths)
  }
  if (regression_table) {
    header <- switch(name,
      probit_mfx = c(" " = 1, "Enrolled (1 = yes)" = 1),
      fs_cons = c(" " = 1, "EMI Exposure" = 1),
      cons_iv = c(" " = 1, "Real Log Consumption Growth" = 1)
    )
    tex <- kableExtra::add_header_above(tex, header, escape = FALSE)
    tex <- tex |>
      kableExtra::column_spec(1, width = "5.8cm") |>
      kableExtra::column_spec(2, width = "2.6cm")
    tex <- style_regression_table(tex, df_render, name)
  }
  note <- public_table_note(name)
  if (!is.null(note)) {
    tex <- kableExtra::footnote(tex, general = note, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE)
  }
  if (landscape_table) {
    tex <- kableExtra::landscape(tex)
    tex <- paste0("\\clearpage\n", as.character(tex), "\n\\clearpage")
  }
  write_table_tex(tex, path, name)
}

#' save tables
#'
#' @return A character vector of generated table paths.
save_tables <- function(tables, cfg) {
  dir <- table_output_dir(cfg)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  formats <- table_formats(cfg)
  public_table_names <- setdiff(names(tables), c("ame_results", "first_stage", "selection_n"))
  paths <- character()

  append_path <- function(value) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
    if (!length(value)) return(invisible(NULL))
    value <- as.character(value)
    value <- value[nzchar(value)]
    if (length(value)) paths <<- c(paths, value)
    invisible(NULL)
  }

  for (n in names(tables)) {
    public <- n %in% public_table_names
    if ("csv" %in% formats) {
      append_path(suppress_atomic_vector_coercion_warning(
        save_table_csv(tables[[n]], file.path(dir, paste0(n, ".csv")), public = public)
      ))
    }
    if ("tex" %in% formats) {
      append_path(suppress_atomic_vector_coercion_warning(
        save_table_tex(
          table = tables[[n]],
          path = file.path(dir, paste0(n, ".tex")),
          name = n,
          public = public
        )
      ))
    }
  }

  unname(unique(paths))
}
