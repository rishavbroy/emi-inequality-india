# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

figure_output_dir <- function(cfg) {
  if (identical(cfg$mode, "final")) "outputs/figures/main" else "outputs/diagnostics/figures"
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

figure_path_base <- function(dir, file) {
  file.path(dir, tools::file_path_sans_ext(file))
}

format_path <- function(path_base, format) {
  paste0(path_base, ".", format)
}

save_plot_formats <- function(plot, path_base, formats, width = 7, height = 5, dpi = 300) {
  paths <- vapply(formats, function(format) {
    path <- format_path(path_base, format)
    ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi)
    path
  }, character(1))
  unname(paths)
}

save_magick_formats <- function(image, path_base, formats) {
  # Flatten alpha before writing so XeLaTeX never rejects RGBA PNGs as an
  # unrecognized image format. For PDF, do not ask ImageMagick to write a PDF:
  # those files have repeatedly failed LaTeX embedding. Instead, draw the raster
  # with grDevices/grid so the PDF is a normal R graphics-device PDF.
  image <- magick::image_background(image, "white", flatten = TRUE)
  image <- magick::image_convert(image, colorspace = "sRGB")
  paths <- vapply(formats, function(format) {
    path <- format_path(path_base, format)
    if (identical(tolower(format), "pdf")) {
      info <- magick::image_info(image)
      width <- max(4, info$width / 150)
      height <- max(4, info$height / 150)
      grDevices::pdf(path, width = width, height = height, onefile = TRUE)
      grid::grid.newpage()
      grid::grid.raster(as.raster(image), width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE)
      grDevices::dev.off()
    } else {
      magick::image_write(image, path = path, format = format)
    }
    path
  }, character(1))
  unname(paths)
}

primary_figure_path <- function(paths) {
  png <- paths[grepl("\\.png$", paths)]
  if (length(png)) png[[1]] else paths[[1]]
}

map_palette <- function(variable) {
  switch(
    variable,
    emie_2007 = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    consumption_growth_pct = c("#fff5f0", "#fcbba1", "#fb6a4a", "#cb181d", "#67000d"),
    pucca_share_2007 = c("#f6eee3", "#d7b98e", "#a87845", "#6f3f1d"),
    head_secondary_plus_2007 = c("#f7fcf5", "#c7e9c0", "#74c476", "#238b45", "#00441b"),
    wavg_ling_degrees = c("#f7fcf5", "#c7e9c0", "#41ab5d", "#006d2c"),
    c("#f7fbff", "#9ecae1", "#3182bd", "#08519c")
  )
}

save_map_figure <- function(spec, path_base, district_panel, formats) {
  if (!has_sf_geometry(district_panel)) {
    stop("Map figure '", spec$name, "' requires an sf district_panel with validated geometry.", call. = FALSE)
  }
  if (is.null(spec$variable) || !spec$variable %in% names(district_panel)) {
    stop("Map figure '", spec$name, "' is missing variable '", spec$variable, "'.", call. = FALSE)
  }

  need_pkg("ggplot2", "sf map figure")
  plot_data <- district_panel
  plot_data$.map_value <- plot_data[[spec$variable]]
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .map_value), color = "grey35", linewidth = 0.08) +
    ggplot2::labs(fill = spec$title) +
    ggplot2::theme_void(base_size = 9) +
    ggplot2::theme(
      legend.position = "right",
      plot.margin = ggplot2::margin(3, 3, 3, 3)
    )

  if (is.numeric(plot_data$.map_value)) {
    scale_args <- list(colors = map_palette(spec$variable), na.value = "grey90")
    if (identical(spec$variable, "emie_2007")) {
      scale_args$breaks <- c(0, 2.5, 10, 25, 50, 100)
      scale_args$limits <- c(0, 100)
    }
    p <- p + do.call(ggplot2::scale_fill_gradientn, scale_args)
  } else {
    plot_data$.map_value <- as.factor(plot_data$.map_value)
    p <- ggplot2::ggplot(plot_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .map_value), color = "grey35", linewidth = 0.08) +
      ggplot2::scale_fill_discrete(na.value = "grey90") +
      ggplot2::labs(fill = spec$title) +
      ggplot2::theme_void(base_size = 9) +
      ggplot2::theme(legend.position = "right", plot.margin = ggplot2::margin(3, 3, 3, 3))
  }

  save_plot_formats(p, path_base, formats, width = 6, height = 5, dpi = 300)
}

read_carveout_shift_data <- function(path = "data/raw/district_changes/District Carve-Outs and Renamings 1961-2001.csv") {
  if (!file.exists(path) && nzchar(Sys.getenv("EMI_PROJECT_ROOT"))) {
    path <- file.path(Sys.getenv("EMI_PROJECT_ROOT"), path)
  }
  if (!file.exists(path)) return(data.frame())
  out <- utils::read.csv(
    path,
    header = FALSE,
    col.names = c("district_1991", "pop_1991", "district_2001", "pct_01in91", "pct_91in01"),
    stringsAsFactors = FALSE
  )
  if (nrow(out)) {
    for (i in seq_len(nrow(out))) {
      if ((is.na(out$district_1991[[i]]) || !nzchar(out$district_1991[[i]])) && i > 1L) {
        out$district_1991[[i]] <- out$district_1991[[i - 1L]]
      }
      if ((is.na(out$pop_1991[[i]]) || !nzchar(out$pop_1991[[i]])) && i > 1L) {
        out$pop_1991[[i]] <- out$pop_1991[[i - 1L]]
      }
    }
  }
  out$pct_91in01 <- num(out$pct_91in01)
  out[is.finite(out$pct_91in01), , drop = FALSE]
}

save_district_carveouts_shifts <- function(spec, path_base, formats) {
  need_pkg("ggplot2", "district carve-outs figure")
  carveouts <- read_carveout_shift_data()
  if (!nrow(carveouts)) stop("District carve-out source data is unavailable.", call. = FALSE)
  binwidth <- diff(range(carveouts$pct_91in01, na.rm = TRUE)) / 40
  if (!is.finite(binwidth) || binwidth <= 0) binwidth <- 1
  p <- ggplot2::ggplot(carveouts, ggplot2::aes(x = pct_91in01)) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = "goldenrod", color = "white") +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      y = "Number of 2001 Districts",
      x = "Percentage of a 1991 District's Population in the 2001 District"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  save_plot_formats(p, path_base, formats, width = 7, height = 4.5, dpi = 300)
}

save_ilo_collage <- function(spec, path_base, formats) {
  sources <- spec$sources[file.exists(spec$sources)]
  if (!length(sources)) return(save_status_figure(spec, format_path(path_base, "png")))
  need_pkg("magick", "ILO figure collage")
  imgs <- lapply(sources, function(p) magick::image_scale(magick::image_read(p), "1300"))
  collage <- magick::image_append(magick::image_join(imgs), stack = TRUE)
  save_magick_formats(collage, path_base, formats)
}

save_collage <- function(spec, path_base, written, formats) {
  inputs <- unname(written[spec$inputs])
  inputs <- inputs[file.exists(inputs)]
  if (!length(inputs)) return(save_status_figure(spec, format_path(path_base, "png")))
  need_pkg("magick", "figure collage")
  imgs <- lapply(inputs, function(p) magick::image_scale(magick::image_read(p), "900"))
  rows <- split(imgs, ceiling(seq_along(imgs) / 2))
  row_imgs <- lapply(rows, function(row) magick::image_append(magick::image_join(row), stack = FALSE))
  collage <- magick::image_append(magick::image_join(row_imgs), stack = TRUE)
  save_magick_formats(collage, path_base, formats)
}

#' save figures
#'
#' @return A character vector of generated figure and manifest paths.
save_figures <- function(figures, cfg) {
  dir <- figure_output_dir(cfg)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  formats <- figure_formats(cfg)

  # Prevent stale draft-map artifacts from masquerading as final public figures
  # after map references have been withheld from the report.
  if (identical(cfg$mode, "final")) {
    stale <- list.files(dir, pattern = "^(map_|collage_.*maps)", full.names = TRUE)
    if (length(stale)) unlink(stale)
  }

  primary <- character()
  all_written <- character()

  for (name in names(figures)) {
    spec <- figures[[name]]
    if (identical(spec$kind, "collage")) next
    path_base <- figure_path_base(dir, spec$file)
    paths <- switch(
      spec$kind,
      ilo_collage = save_ilo_collage(spec, path_base, formats),
      map = save_map_figure(spec, path_base, attr(figures, "district_panel") %||% data.frame(), formats),
      district_carveouts_shifts = save_district_carveouts_shifts(spec, path_base, formats),
      status = save_status_figure(spec, format_path(path_base, "png")),
      save_distribution_figure(spec, format_path(path_base, "png"), attr(figures, "district_panel") %||% data.frame())
    )
    primary[[name]] <- primary_figure_path(paths)
    all_written <- c(all_written, paths)
  }

  for (name in names(figures)) {
    spec <- figures[[name]]
    if (!identical(spec$kind, "collage")) next
    path_base <- figure_path_base(dir, spec$file)
    paths <- save_collage(spec, path_base, primary, formats)
    primary[[name]] <- primary_figure_path(paths)
    all_written <- c(all_written, paths)
  }

  manifest <- data.frame(
    path = unname(all_written),
    stringsAsFactors = FALSE
  )
  manifest$name <- tools::file_path_sans_ext(basename(manifest$path))
  manifest$format <- tools::file_ext(manifest$path)
  manifest_path <- file.path(dir, "figure_manifest.csv")
  utils::write.csv(manifest, manifest_path, row.names = FALSE)
  c(unname(all_written), manifest_path)
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
