# Poster-only figure construction.
#
# This module is sourced with the rest of R/output, but none of its functions
# enter the target graph unless EMI_RENDER_POSTER is enabled. Shared manuscript
# figures remain in make_figures()/save_figures().

poster_second_stage_data <- function(district_panel) {
  need_pkg("ivreg", "poster second-stage specifications")
  need_pkg("marginaleffects", "poster second-stage predictions")
  need_pkg("sandwich", "poster second-stage clustered covariance")
  data <- as.data.frame(district_panel)
  outcome <- "real_log_consumption_change"
  treatment <- "emi_exposure_all_children_0708"
  instrument <- "ling_distance_nonzero_mean"
  specs <- geographic_adjustment_specs()
  controls <- adjustment_spec_controls(specs)
  required <- unique(c(outcome, treatment, instrument, "state_code_2001", "region", controls))
  if (length(setdiff(required, names(data)))) return(data.frame())
  data <- data[stats::complete.cases(data[, required, drop = FALSE]), required, drop = FALSE]
  if (nrow(data) < 25L || length(unique(data$state_code_2001)) < 2L) return(data.frame())
  grid <- data.frame(
    emi_exposure_all_children_0708 = unname(stats::quantile(
      data[[treatment]], probs = seq(0.05, 0.95, by = 0.10), names = FALSE
    ))
  )

  out <- lapply(names(specs), function(id) {
    spec <- specs[[id]]
    fe <- adjustment_fixed_effect_term(spec$fixed_effect)
    rhs <- c(treatment, spec$controls, fe)
    iv_rhs <- c(instrument, spec$controls, fe)
    fit <- ivreg::ivreg(
      stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + "), "|", paste(iv_rhs, collapse = " + "))),
      data = data,
      model = TRUE,
      x = TRUE,
      y = TRUE
    )
    vcov <- sandwich::vcovCL(fit, cluster = data$state_code_2001, type = "HC1")
    pred <- marginaleffects::avg_predictions(
      fit,
      newdata = data,
      variables = stats::setNames(list(grid[[treatment]]), treatment),
      vcov = vcov,
      type = "response"
    )
    pred <- as.data.frame(pred)
    if (!treatment %in% names(pred)) pred[[treatment]] <- grid[[treatment]]
    pred$specification <- spec$label
    pred$specification_id <- id
    pred$n <- stats::nobs(fit)
    pred
  })
  safe_bind_rows(out)
}

save_poster_second_stage_figure <- function(district_panel, cfg) {
  need_pkg("ggplot2", "poster second-stage specification plot")
  plot_data <- poster_second_stage_data(district_panel)
  if (!nrow(plot_data)) {
    stop("Poster second-stage figure could not build any specification ribbons.", call. = FALSE)
  }

  treatment <- "emi_exposure_all_children_0708"
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[treatment]], y = estimate)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), fill = "#9b59b6", alpha = 0.16) +
    ggplot2::geom_line(color = "#6a3d9a", linewidth = 1.05) +
    ggplot2::facet_wrap(~ specification, scales = "free_y", nrow = 1) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::labs(
      x = "District EMI exposure",
      y = "Predicted real log consumption change",
      caption = "Ribbons show 95% confidence intervals; all specifications use the same complete district sample."
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      plot.caption = ggplot2::element_text(size = 9, hjust = 0),
      axis.title = ggplot2::element_text(face = "bold")
    )

  dir <- file.path("posters", "2026_predoc_conference", "generated")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path_base <- file.path(dir, "poster_second_stage_specs")
  save_plot_formats(p, path_base, figure_formats(cfg), width = 9.6, height = 3.9, dpi = 300)
}
