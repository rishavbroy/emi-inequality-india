# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

figure_output_dir <- function(cfg) {
  "outputs/figures/main"
}

figure_formats <- function(cfg) {
  out <- cfg$output_formats$figures %||% "png"
  unique(as.character(out))
}

save_status_figure <- function(spec, path, district_panel = NULL) {
  need_pkg("ggplot2", "figure generation")
  panel <- as.data.frame(district_panel %||% data.frame())
  label <- if (!is.null(spec$variable) && spec$variable %in% names(panel)) spec$variable else "unavailable"
  n_rows <- nrow(panel)
  n_observed <- if (!is.null(spec$variable) && spec$variable %in% names(panel)) {
    sum(!is.na(panel[[spec$variable]]))
  } else {
    0L
  }
  plot_data <- data.frame(
    metric = c("panel rows", "observed values"),
    value = c(n_rows, n_observed)
  )
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(metric, value)) +
    ggplot2::geom_col(fill = c("#325d79", "#f28e2b"), width = 0.65) +
    ggplot2::labs(
      title = spec$title,
      subtitle = spec$subtitle %||% paste("Draft diagnostic for", label),
      x = NULL,
      y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(path, p, width = 7, height = 4.5, dpi = 300)
  path
}

save_distribution_figure <- function(spec, path, district_panel) {
  need_pkg("ggplot2", "figure generation")
  panel <- as.data.frame(district_panel)
  if (is.null(spec$variable) || !spec$variable %in% names(panel)) {
    return(save_status_figure(spec, path, panel))
  }
  x <- panel[[spec$variable]]
  if (is.numeric(x)) {
    plot_data <- data.frame(value = x)
    subtitle <- if (has_sf_geometry(district_panel)) "District map input distribution." else "District distribution; geometry join remains under validation."
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(value)) +
      ggplot2::geom_histogram(bins = 30, fill = "#4e79a7", color = "white", na.rm = TRUE) +
      ggplot2::labs(title = spec$title, subtitle = subtitle, x = spec$variable, y = "Districts") +
      ggplot2::theme_minimal(base_size = 12)
  } else {
    plot_data <- as.data.frame(sort(table(x), decreasing = TRUE))
    names(plot_data) <- c("value", "n")
    plot_data <- head(plot_data, 20)
    subtitle <- if (has_sf_geometry(district_panel)) "District map input categories." else "District categories; geometry join remains under validation."
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(stats::reorder(value, n), n)) +
      ggplot2::geom_col(fill = "#59a14f", width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = spec$title, subtitle = subtitle, x = NULL, y = "Districts") +
      ggplot2::theme_minimal(base_size = 12)
  }
  ggplot2::ggsave(path, p, width = 7, height = 4.5, dpi = 300)
  path
}

save_district_tracker_summary <- function(spec, path) {
  need_pkg("ggplot2", "district tracker figure")
  tracker_path <- "data/processed/district_tracker_2001_2007_2017_2020.csv"
  if (!file.exists(tracker_path)) return(save_status_figure(spec, path))
  tracker <- utils::read.csv(tracker_path, stringsAsFactors = FALSE)
  source <- first_col(tracker, c("source_file_id", "source", "Source"))
  if (is.null(source)) return(save_status_figure(spec, path, tracker))
  plot_data <- as.data.frame(sort(table(tracker[[source]], useNA = "ifany"), decreasing = TRUE))
  names(plot_data) <- c("source", "rows")
  plot_data <- head(plot_data, 20)
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(stats::reorder(source, rows), rows)) +
    ggplot2::geom_col(fill = "#7f7f7f", width = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = spec$title,
      subtitle = spec$subtitle %||% "Source coverage in district tracker inputs.",
      x = NULL,
      y = "Rows"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(path, p, width = 7, height = 4.5, dpi = 300)
  path
}

save_ilo_collage <- function(spec, path) {
  sources <- spec$sources[file.exists(spec$sources)]
  if (!length(sources)) return(save_status_figure(spec, path))
  need_pkg("magick", "ILO figure collage")
  imgs <- lapply(sources, function(p) magick::image_scale(magick::image_read(p), "1300"))
  collage <- magick::image_append(magick::image_join(imgs), stack = TRUE)
  magick::image_write(collage, path = path)
  path
}

save_collage <- function(spec, path, written) {
  inputs <- unname(written[spec$inputs])
  inputs <- inputs[file.exists(inputs)]
  if (!length(inputs)) return(save_status_figure(spec, path))
  need_pkg("magick", "figure collage")
  imgs <- lapply(inputs, function(p) magick::image_scale(magick::image_read(p), "900"))
  rows <- split(imgs, ceiling(seq_along(imgs) / 2))
  row_imgs <- lapply(rows, function(row) magick::image_append(magick::image_join(row), stack = FALSE))
  collage <- magick::image_append(magick::image_join(row_imgs), stack = TRUE)
  magick::image_write(collage, path = path)
  path
}

#' save figures
#'
#' @return A character vector of generated figure and manifest paths.
save_figures <- function(figures, cfg) {
  dir <- figure_output_dir(cfg)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  written <- character()

  for (name in names(figures)) {
    spec <- figures[[name]]
    if (identical(spec$kind, "collage")) next
    path <- file.path(dir, spec$file)
    written[[name]] <- switch(
      spec$kind,
      ilo_collage = save_ilo_collage(spec, path),
      status = save_status_figure(spec, path),
      district_tracker_summary = save_district_tracker_summary(spec, path),
      save_distribution_figure(spec, path, attr(figures, "district_panel") %||% data.frame())
    )
  }

  district_panel <- attr(figures, "district_panel") %||% data.frame()
  for (name in names(figures)) {
    spec <- figures[[name]]
    if (!identical(spec$kind, "collage")) next
    path <- file.path(dir, spec$file)
    written[[name]] <- save_collage(spec, path, written)
  }

  manifest <- data.frame(
    name = names(written),
    path = unname(written),
    stringsAsFactors = FALSE
  )
  manifest_path <- file.path(dir, "figure_manifest.csv")
  utils::write.csv(manifest, manifest_path, row.names = FALSE)
  c(unname(written), manifest_path)
}

#' save figure pdf png
#'
#' @return Generated file paths.
save_figure_pdf_png <- function(plot, path_base, width = 7, height = 5, dpi = 300) {
  ggplot2::ggsave(paste0(path_base, ".png"), plot, width = width, height = height, dpi = dpi)
  ggplot2::ggsave(paste0(path_base, ".pdf"), plot, width = width, height = height)
  c(paste0(path_base, ".png"), paste0(path_base, ".pdf"))
}

#' save map pdf png
#'
#' @return Generated file paths.
save_map_pdf_png <- function(plot, path_base) {
  save_figure_pdf_png(plot, path_base)
}

#' save diagnostic figure
#'
#' @return Generated file paths.
save_diagnostic_figure <- function(plot, path_base) {
  save_figure_pdf_png(plot, path_base)
}
