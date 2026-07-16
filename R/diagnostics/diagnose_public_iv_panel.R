# Public IV-panel diagnostics used by analysis notebooks.
# These are current-pipeline diagnostics, not legacy-parity checks.

save_public_iv_panel_diagnostics <- function(district_panel, tables = NULL, dir = "outputs/diagnostics/public") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  panel <- as.data.frame(district_panel)
  paths <- c(
    current_rows = write_diagnostic_csv(public_iv_panel_rows(panel), file.path(dir, "iv_panel_current_rows.csv")),
    match_summary = write_diagnostic_csv(public_iv_panel_match_summary(panel), file.path(dir, "iv_panel_match_summary.csv")),
    state_summary = write_diagnostic_csv(public_iv_panel_state_summary(panel), file.path(dir, "iv_panel_state_summary.csv")),
    extreme_rows = write_diagnostic_csv(public_iv_panel_extreme_rows(panel), file.path(dir, "iv_panel_extreme_rows.csv")),
    keyed_summary_rows = write_diagnostic_csv(public_iv_summary_keyed_rows(panel, tables), file.path(dir, "iv_summary_keyed_rows.csv"))
  )
  unname(unlist(paths, use.names = FALSE))
}

public_iv_panel_diagnostic_columns <- function(panel) {
  present_cols(panel, c(
    "district_panel_id", "state_20", "district_20", "state_17", "district_17",
    "state_07", "district_07", "state_01", "district_01", "state_std", "district_std",
    "EMIE", "wavg_ling_degrees", "npeople_0708", "consumption_0708",
    "dependency_ratio", "consumption_1718", "consumption_pct_change",
    ".matched_2001", ".matched_2007", ".matched_2017"
  ))
}

public_iv_panel_rows <- function(panel) {
  panel <- as.data.frame(panel)
  cols <- public_iv_panel_diagnostic_columns(panel)
  if (!length(cols)) return(data.frame())
  panel[cols]
}

public_iv_panel_numeric_vars <- function(panel) {
  present_cols(panel, c("EMIE", "wavg_ling_degrees", "npeople_0708", "consumption_0708", "dependency_ratio"))
}

public_iv_panel_match_summary <- function(panel) {
  panel <- as.data.frame(panel)
  if (!nrow(panel)) return(data.frame())
  group_cols <- present_cols(panel, c(".matched_2001", ".matched_2007", ".matched_2017"))
  if (!length(group_cols)) return(data.frame(n_rows = nrow(panel)))
  aggregate_panel_summary(panel, group_cols, public_iv_panel_numeric_vars(panel))
}

public_iv_panel_state_summary <- function(panel) {
  panel <- as.data.frame(panel)
  if (!nrow(panel)) return(data.frame())
  state_col <- first_col(panel, c("state_20", "state_17", "state_07", "state_01", "state_std"))
  if (is.null(state_col)) return(data.frame(n_rows = nrow(panel)))
  panel$.diagnostic_state <- as.character(panel[[state_col]])
  aggregate_panel_summary(panel, ".diagnostic_state", public_iv_panel_numeric_vars(panel))
}

aggregate_panel_summary <- function(panel, group_cols, numeric_vars) {
  panel <- as.data.frame(panel)
  split_key <- interaction(panel[group_cols], drop = TRUE, lex.order = TRUE)
  groups <- split(seq_len(nrow(panel)), split_key)
  rows <- lapply(groups, function(idx) {
    group_vals <- panel[idx[[1]], group_cols, drop = FALSE]
    out <- as.list(group_vals)
    out$n_rows <- length(idx)
    for (var in numeric_vars) out[[paste0("mean_", var)]] <- mean(num(panel[[var]][idx]), na.rm = TRUE)
    as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  })
  out <- safe_bind_rows(rows)
  if (".diagnostic_state" %in% names(out)) names(out)[names(out) == ".diagnostic_state"] <- "state"
  out
}

public_iv_panel_extreme_rows <- function(panel) {
  panel <- as.data.frame(panel)
  if (!nrow(panel)) return(data.frame())
  row_cols <- public_iv_panel_diagnostic_columns(panel)
  vars <- present_cols(panel, c("EMIE", "wavg_ling_degrees", "npeople_0708"))
  rows <- list()
  for (var in vars) {
    value <- num(panel[[var]])
    ok <- is.finite(value)
    if (!any(ok)) next
    ord <- order(value[ok])
    idx_ok <- which(ok)[ord]
    for (label in c("lowest", "highest")) {
      idx <- if (identical(label, "lowest")) head(idx_ok, 10L) else tail(idx_ok, 10L)
      if (!length(idx)) next
      x <- panel[idx, row_cols, drop = FALSE]
      x$diagnostic_set <- paste(label, var, sep = "_")
      x$diagnostic_value <- value[idx]
      rows[[length(rows) + 1L]] <- x[c("diagnostic_set", "diagnostic_value", row_cols)]
    }
  }
  safe_bind_rows(rows)
}

public_iv_summary_keyed_rows <- function(panel, tables = NULL) {
  table <- NULL
  if (is.list(tables) && "sum_tbl_iv" %in% names(tables)) table <- tables$sum_tbl_iv
  if (is.null(table)) table <- tryCatch(make_iv_summary_table(panel), error = function(e) data.frame())
  table <- as.data.frame(table, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(table)) return(data.frame())
  label_col <- if ("label" %in% names(table)) "label" else names(table)[[1]]
  var_col <- if ("var" %in% names(table)) "var" else label_col
  group <- rep(NA_character_, nrow(table))
  current_group <- NA_character_
  for (i in seq_len(nrow(table))) {
    var <- as.character(table[[var_col]][[i]])
    label <- as.character(table[[label_col]][[i]])
    is_group <- startsWith(var, ".group_") || grepl(":$", label)
    if (isTRUE(is_group)) {
      current_group <- sub(":$", "", label)
    } else {
      group[[i]] <- current_group
    }
  }
  vars <- as.character(table[[var_col]])
  is_group_row <- !is.na(vars) & startsWith(vars, ".group_")
  out <- table[!is_group_row, , drop = FALSE]
  out$group <- group[!is_group_row]
  out$variable <- as.character(out[[var_col]])
  out[c("group", "variable", setdiff(names(out), c("group", "variable")))]
}
