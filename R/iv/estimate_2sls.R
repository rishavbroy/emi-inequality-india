# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

iv_cluster_column <- function(data) {
  first_col(
    as.data.frame(data),
    c("state_2001_cluster", "state_20", "state_std", "state_0708")
  )
}

iv_model_row_indices <- function(model, data) {
  frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  data <- as.data.frame(data)
  if (is.null(frame) || !nrow(frame) || !nrow(data)) return(integer())

  frame_rows <- rownames(frame)
  data_rows <- rownames(data)
  if (!is.null(frame_rows) && !is.null(data_rows)) {
    matched <- match(frame_rows, data_rows)
    if (length(matched) == nrow(frame) && all(!is.na(matched))) {
      return(as.integer(matched))
    }
  }

  if (nrow(frame) == nrow(data)) seq_len(nrow(data)) else integer()
}

iv_model_cluster <- function(model, data) {
  cluster_col <- iv_cluster_column(data)
  if (is.null(cluster_col)) return(NULL)

  rows <- iv_model_row_indices(model, data)
  if (!length(rows)) return(NULL)

  cluster <- as.data.frame(data)[[cluster_col]][rows]
  if (
    length(cluster) != stats::nobs(model) ||
      anyNA(cluster) ||
      length(unique(cluster)) < 2L
  ) {
    return(NULL)
  }
  as.vector(cluster)
}

#' estimate 2sls
#'
estimate_2sls <- function(district_panel, formulas, cfg) {
  lapply(formulas, function(formula) {
    vars <- all.vars(formula)
    missing <- setdiff(vars, names(district_panel))
    if (length(missing)) {
      return(list(status = "out_of_active_pipeline", reason = paste("Missing variables:", paste(missing, collapse = ", "))))
    }
    if (!requireNamespace("ivreg", quietly = TRUE)) return(list(status = "out_of_active_pipeline", reason = "Package ivreg not installed."))
    fit <- ivreg::ivreg(
      formula,
      data = district_panel,
      model = TRUE,
      x = TRUE,
      y = TRUE
    )
    cluster <- iv_model_cluster(fit, district_panel)
    if (!is.null(cluster)) attr(fit, "cluster_state") <- cluster
    fit
  })
}
