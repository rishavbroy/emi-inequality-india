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

legacy_map_style <- function(variable) {
  switch(
    variable,
    emie_2007 = list(
      palette = "brewer.blues",
      title = "EMI Exposure",
      style = "fixed",
      breaks = c(0, 2.5, 10, 25, 50, 100),
      labels = c("0-2.5", "2.5-10", "10-25", "25-50", "50-100"),
      legend.hist = TRUE
    ),
    consumption_growth_pct = list(
      palette = "brewer.reds",
      title = "%Δ Consumption",
      style = "cont",
      breaks = NULL,
      labels = NULL,
      legend.hist = FALSE
    ),
    pucca_share_2007 = list(
      palette = "brown",
      title = "% Pucca Homes",
      style = NULL,
      breaks = NULL,
      labels = NULL,
      legend.hist = TRUE
    ),
    head_secondary_plus_2007 = list(
      palette = "brewer.greens",
      title = "% HH Head w/ Sec.+",
      style = "cont",
      breaks = NULL,
      labels = NULL,
      legend.hist = FALSE
    ),
    region = list(
      palette = "brewer.dark2",
      title = "Region",
      style = NULL,
      breaks = NULL,
      labels = NULL,
      legend.hist = FALSE
    ),
    wavg_ling_degrees = list(
      palette = "carto.emrld",
      title = "Linguistic Distance",
      style = NULL,
      breaks = NULL,
      labels = NULL,
      legend.hist = TRUE
    ),
    list(
      palette = "brewer.blues",
      title = variable,
      style = NULL,
      breaks = NULL,
      labels = NULL,
      legend.hist = FALSE
    )
  )
}

complete_map_geometry <- function(district_panel, boundaries_2020, variable) {
  panel <- district_panel
  if (!has_sf_geometry(panel)) return(panel)
  if (!inherits(boundaries_2020, "sf")) return(panel)
  b <- boundaries_2020
  geom_col <- attr(b, "sf_column")
  if (is.null(geom_col) || !geom_col %in% names(b)) return(panel)

  panel_df <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel)
  keys <- if (all(c("state_20", "district_20") %in% names(panel_df)) && all(c("state_20", "district_20") %in% names(b))) {
    c("state_20", "district_20")
  } else if (all(c("state_std", "district_std") %in% names(panel_df)) && all(c("state_std", "district_std") %in% names(b))) {
    c("state_std", "district_std")
  } else {
    character()
  }
  if (!length(keys)) return(panel)

  keep <- unique(c(keys, variable, "region"))
  keep <- intersect(keep, names(panel_df))
  panel_df <- panel_df[!duplicated(panel_df[keys]), keep, drop = FALSE]
  out <- merge(b, panel_df, by = keys, all.x = TRUE, sort = FALSE)
  out <- sf::st_as_sf(out, sf_column_name = geom_col)

  panel_nonmissing <- if (variable %in% names(panel_df)) sum(!is.na(panel_df[[variable]])) else 0L
  out_nonmissing <- if (variable %in% names(out)) sum(!is.na(out[[variable]])) else 0L
  # Preserve the validated matched-panel geometry when a full-boundary merge
  # would make almost the entire map missing. This follows the legacy maps more
  # closely: only districts missing a map variable are greyed out.
  if (panel_nonmissing > 0L && out_nonmissing < max(1L, floor(0.5 * panel_nonmissing))) {
    return(panel)
  }
  out
}

prepare_legacy_map_data <- function(plot_data, variable) {
  if (!variable %in% names(plot_data)) plot_data[[variable]] <- NA
  if (identical(variable, "region")) {
    valid_regions <- c("North", "Central", "East", "West", "South")
    value <- as.character(plot_data[[variable]])
    value[!value %in% valid_regions] <- NA_character_
    plot_data[[variable]] <- factor(value, levels = valid_regions)
  }
  plot_data
}

build_legacy_tmap <- function(plot_data, spec) {
  need_pkg("tmap", "classified choropleth maps")
  style <- legacy_map_style(spec$variable)
  fill_args <- list(
    col = spec$variable,
    palette = style$palette,
    title = style$title,
    colorNA = "grey82",
    textNA = "No data",
    legend.hist = isTRUE(style$legend.hist)
  )
  if (!is.null(style$style)) fill_args$style <- style$style
  if (!is.null(style$breaks)) fill_args$breaks <- style$breaks
  if (!is.null(style$labels)) fill_args$labels <- style$labels

  suppressWarnings(suppressMessages({
    tmap::tm_shape(plot_data) +
      do.call(tmap::tm_fill, fill_args) +
      tmap::tm_borders(alpha = 0.2) +
      tmap::tm_layout(
        frame = FALSE,
        asp = 0,
        legend.outside = TRUE,
        legend.outside.position = "right",
        legend.title.size = 1.0,
        legend.text.size = 0.8,
        inner.margins = 0.01
      )
  }))
}

save_tmap_formats <- function(map_plot, path_base, formats, width = 8, height = 6, dpi = 300) {
  unlink("Rplots.pdf")
  paths <- vapply(formats, function(format) {
    path <- format_path(path_base, format)
    suppressWarnings(suppressMessages(
      tmap::tmap_save(
        tm = map_plot,
        filename = path,
        width = width,
        height = height,
        units = "in",
        dpi = dpi
      )
    ))
    unlink("Rplots.pdf")
    path
  }, character(1))
  unname(paths)
}

save_map_figure <- function(spec, path_base, district_panel, formats, boundaries_2020 = NULL) {
  if (!has_sf_geometry(district_panel)) {
    stop("Map figure '", spec$name, "' requires an sf district_panel with validated geometry.", call. = FALSE)
  }
  if (is.null(spec$variable) || !spec$variable %in% names(district_panel)) {
    stop("Map figure '", spec$name, "' is missing variable '", spec$variable, "'.", call. = FALSE)
  }

  need_pkg("tmap", "classified choropleth maps")
  plot_data <- complete_map_geometry(district_panel, boundaries_2020, spec$variable)
  plot_data <- prepare_legacy_map_data(plot_data, spec$variable)
  p <- build_legacy_tmap(plot_data, spec)
  save_tmap_formats(p, path_base, formats, width = 8, height = 6, dpi = 300)
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
  imgs <- lapply(inputs, function(p) magick::image_scale(magick::image_read(p), "1900"))
  collage <- magick::image_append(magick::image_join(imgs), stack = TRUE)
  save_magick_formats(collage, path_base, formats)
}

#' save figures
#'
#' @return A character vector of generated figure and manifest paths.
save_figures <- function(figures, cfg) {
  unlink("Rplots.pdf")
  on.exit(unlink("Rplots.pdf"), add = TRUE)
  dir <- figure_output_dir(cfg)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  formats <- figure_formats(cfg)

  primary <- character()
  all_written <- character()

  for (name in names(figures)) {
    spec <- figures[[name]]
    if (identical(spec$kind, "collage")) next
    path_base <- figure_path_base(dir, spec$file)
    paths <- switch(
      spec$kind,
      ilo_collage = save_ilo_collage(spec, path_base, formats),
      map = save_map_figure(spec, path_base, attr(figures, "district_panel") %||% data.frame(), formats, attr(figures, "boundaries_2020")),
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
