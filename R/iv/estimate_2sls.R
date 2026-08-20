# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

iv_cluster_column <- function(data) {
  first_col(
    as.data.frame(data),
    c("state_code_2001", "state_2001_cluster", "state_20", "state_std", "state_0708")
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


iv_structural_model_matrix <- function(model) {
  stored <- tryCatch(model$x$regressors, error = function(e) NULL)
  if (!is.null(stored) && length(dim(stored)) == 2L) return(stored)
  if (inherits(model, "ivreg")) {
    return(stats::model.matrix(model, component = "regressors"))
  }
  stats::model.matrix(model)
}

iv_clustered_inference <- function(model, cluster) {
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    return(list(
      vcov = NULL,
      status = "unavailable",
      reason = "Package 'sandwich' is not installed."
    ))
  }
  cluster <- as.vector(cluster)
  if (
    length(cluster) != stats::nobs(model) ||
      anyNA(cluster) ||
      length(unique(cluster)) < 2L
  ) {
    return(list(
      vcov = NULL,
      status = "unavailable",
      reason = "Cluster vector is incomplete or not aligned to fitted observations."
    ))
  }
  out <- tryCatch(
    sandwich::vcovCL(model, cluster = cluster, type = "HC1"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    return(list(
      vcov = NULL,
      status = "unavailable",
      reason = conditionMessage(out)
    ))
  }
  list(vcov = out, status = "estimated", reason = NA_character_)
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
    fitted_rows <- iv_model_row_indices(fit, district_panel)
    if (!length(fitted_rows)) {
      stop("Could not align fitted IV observations to the district panel.", call. = FALSE)
    }
    prediction_vars <- unique(all.vars(formula))
    prediction_data <- as.data.frame(district_panel)[
      fitted_rows, prediction_vars, drop = FALSE
    ]
    rownames(prediction_data) <- NULL
    attr(fit, "prediction_data") <- prediction_data

    cluster <- iv_model_cluster(fit, district_panel)
    if (is.null(cluster) && is_final_mode(cfg)) {
      stop(
        "State-clustered IV inference is required in final mode, but no complete aligned state cluster was available.",
        call. = FALSE
      )
    }
    if (!is.null(cluster)) {
      inference <- iv_clustered_inference(fit, cluster)
      attr(fit, "cluster_state") <- cluster
      attr(fit, "cluster_vcov") <- inference$vcov
      attr(fit, "cluster_inference_status") <- inference$status
      attr(fit, "cluster_inference_reason") <- inference$reason
      if (
        identical(inference$status, "unavailable") &&
          is_final_mode(cfg)
      ) {
        stop(
          paste0(
            "Clustered IV inference is unavailable: ",
            inference$reason
          ),
          call. = FALSE
        )
      }
    }
    fit
  })
}
