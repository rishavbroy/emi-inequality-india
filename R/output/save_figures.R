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

save_plot_file <- function(plot, path, width = 7, height = 5, dpi = 300) {
  fmt <- tolower(tools::file_ext(path))
  if (identical(fmt, "pdf")) {
    # Use the base PDF device directly. Calling cairo_pdf/capabilities() on
    # macOS can attempt to load X11/Cairo shared libraries and emit warnings
    # that strict targets treats as failures. The map labels are ASCII now, so
    # the standard PDF device is sufficient and fully explicit.
    grDevices::pdf(file = path, width = width, height = height, onefile = TRUE, useDingbats = FALSE)
    dev <- grDevices::dev.cur()
    on.exit(if (grDevices::dev.cur() == dev) grDevices::dev.off(), add = TRUE)
    print(plot)
    return(path)
  }
  if (identical(fmt, "png")) {
    grDevices::png(filename = path, width = width, height = height, units = "in", res = dpi, bg = "white")
    dev <- grDevices::dev.cur()
    on.exit(if (grDevices::dev.cur() == dev) grDevices::dev.off(), add = TRUE)
    print(plot)
    return(path)
  }
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = dpi, bg = "white")
  path
}

save_plot_formats <- function(plot, path_base, formats, width = 7, height = 5, dpi = 300) {
  paths <- vapply(formats, function(format) {
    path <- format_path(path_base, format)
    save_plot_file(plot, path, width = width, height = height, dpi = dpi)
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

emi_exposure_map_style <- function(title) {
  list(
    palette = "brewer.blues",
    title = paste0(title, " (%)"),
    style = "fixed",
    breaks = c(0, 2.5, 10, 25, 50, 100),
    labels = c("0-2.5", "2.5-10", "10-25", "25-50", "50-100")
  )
}

public_map_style <- function(variable) {
  switch(
    variable,
    emi_exposure_all_children_0708 = emi_exposure_map_style("All-child EMI exposure"),
    emi_share_enrolled_0708 = emi_exposure_map_style("EMI share among enrolled"),
    public_emi_exposure_all_children_0708 = emi_exposure_map_style("Public EMI exposure"),
    private_emi_exposure_all_children_0708 = emi_exposure_map_style("Private EMI exposure"),
    real_log_consumption_change = list(
      palette = "poster.consumption",
      title = "Real Log Consumption Change",
      style = "continuous",
      breaks = NULL,
      labels = NULL
    ),
    pct_pucca = list(
      palette = "brown",
      title = "% Pucca Homes",
      style = NULL,
      breaks = NULL,
      labels = NULL
    ),
    pct_head_secondary_plus = list(
      palette = "brewer.greens",
      title = "% HH Head w/ Sec.+",
      style = "fixed",
      breaks = c(0, 20, 40, 60, 80),
      labels = c("0-20", "20-40", "40-60", "60-80")
    ),
    region = list(
      palette = "poster.region",
      title = "Region",
      style = "cat",
      breaks = NULL,
      labels = NULL
    ),
    ling_distance_nonzero_mean = list(
      palette = "carto.emrld",
      title = "Linguistic Distance",
      style = "continuous",
      breaks = NULL,
      labels = NULL
    ),
    resid_emi_exposure_region_expanded = list(
      palette = "poster.diverging.emi",
      title = "Residual EMI Exposure",
      style = "diverging",
      breaks = NULL,
      labels = NULL
    ),
    resid_ling_distance_region_expanded = list(
      palette = "poster.diverging.iv",
      title = "Residual Linguistic Distance",
      style = "diverging",
      breaks = NULL,
      labels = NULL
    ),
    resid_ling_distance_state_main = list(
      palette = "poster.diverging.iv",
      title = "Residual Linguistic Distance",
      style = "diverging",
      breaks = NULL,
      labels = NULL
    ),
    paper_real_mean_mpce_2022_23 = list(
      palette = "poster.consumption.positive",
      title = "Real consumption per person\n(2011-12 Rs.)",
      style = "continuous",
      breaks = NULL,
      labels = NULL
    ),
    paper_real_log_mpce_change_2004_05_2022_23 = list(
      palette = "poster.diverging.emi",
      title = "Log real consumption change",
      style = "diverging",
      breaks = NULL,
      labels = NULL
    ),
    list(
      palette = "brewer.blues",
      title = variable,
      style = NULL,
      breaks = NULL,
      labels = NULL
    )
  )
}

prepare_public_map_data <- function(plot_data, variable) {
  if (!variable %in% names(plot_data)) plot_data[[variable]] <- NA
  if (identical(variable, "region")) {
    levels <- panel_region_levels()
    value <- as.character(plot_data[[variable]])
    value[!value %in% levels] <- NA_character_
    plot_data[[variable]] <- factor(value, levels = levels)
  }
  plot_data
}

map_palette_values <- function(palette, n) {
  n <- max(1L, as.integer(n))
  base <- switch(
    palette,
    brewer.blues = c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c"),
    brewer.reds = c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15"),
    brewer.greens = c("#edf8e9", "#bae4b3", "#74c476", "#31a354", "#006d2c"),
    brewer.dark2 = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"),
    poster.region = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#6A3D9A"),
    brown = c("#f6eee3", "#dfc29d", "#bf8f59", "#8c5a2b", "#543005"),
    carto.emrld = c("#d3f2a3", "#97e196", "#6cc08b", "#4c9b82", "#217a79"),
    poster.consumption = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    poster.consumption.positive = c("#f7f7f7", "#f4a582", "#b2182b"),
    poster.diverging.emi = c("#2166ac", "#92c5de", "#f7f7f7", "#f4a582", "#b2182b"),
    poster.diverging.iv = c("#762a83", "#af8dc3", "#f7f7f7", "#7fbf7b", "#1b7837"),
    c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")
  )
  if (n == length(base)) return(base)
  grDevices::colorRampPalette(base)(n)
}

map_pretty_breaks <- function(x, n = 5L) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NULL)
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng))) return(NULL)
  if (isTRUE(all.equal(rng[[1]], rng[[2]]))) {
    delta <- if (rng[[1]] == 0) 1 else abs(rng[[1]]) * 0.01
    return(c(rng[[1]] - delta, rng[[2]] + delta))
  }
  br <- pretty(rng, n = n)
  br <- br[br >= rng[[1]] & br <= rng[[2]]]
  br <- unique(c(rng[[1]], br, rng[[2]]))
  if (length(br) < 2L) br <- pretty(rng, n = n)
  br
}

map_cut_label_number <- function(x) {
  out <- trimws(formatC(x, format = "fg", digits = 4))
  out <- sub("\\.0+$", "", out)
  out <- sub("(\\.\\d*?)0+$", "\\1", out)
  out <- sub("\\.$", "", out)
  out <- sub("^-$", "0", out)
  out
}

map_cut_labels <- function(breaks) {
  if (length(breaks) < 2L) return(character())
  paste0(
    map_cut_label_number(head(breaks, -1L)),
    "-",
    map_cut_label_number(tail(breaks, -1L))
  )
}

map_no_data_colour <- function() "#bdbdbd"

map_disputed_no_data_colour <- function() "#eeeeee"

map_district_boundary_linewidth <- function() 0.04

map_major_boundary_linewidth <- function() 0.25

map_disputed_boundary_linewidth <- function() 0.10

map_squish <- function(x, limits) {
  if (is.null(limits) || length(limits) != 2L || !all(is.finite(limits))) return(x)
  pmax(pmin(x, limits[[2]]), limits[[1]])
}

map_continuous_limits <- function(values, style) {
  values <- values[is.finite(values)]
  if (!length(values)) return(NULL)
  if (identical(style$style, "diverging")) {
    lim <- suppressWarnings(stats::quantile(abs(values), 0.98, na.rm = TRUE, names = FALSE))
    if (!is.finite(lim) || lim <= 0) lim <- max(abs(values), na.rm = TRUE)
    lim <- signif(lim, 2)
    return(c(-lim, lim))
  }
  limits <- suppressWarnings(stats::quantile(values, c(0.02, 0.98), na.rm = TRUE, names = FALSE))
  if (!all(is.finite(limits)) || limits[[1]] >= limits[[2]]) limits <- range(values, na.rm = TRUE)
  rounded <- pretty(limits, n = 4L)
  range(rounded, na.rm = TRUE)
}

public_map_fill <- function(plot_data, variable, style) {
  values <- plot_data[[variable]]
  if (identical(style$style, "continuous") || identical(style$style, "diverging")) {
    values <- suppressWarnings(as.numeric(values))
    limits <- map_continuous_limits(values, style)
    plot_data$.map_value <- map_squish(values, limits)
    return(list(
      data = plot_data, fill = ".map_value",
      colors = map_palette_values(style$palette, 7L), title = style$title,
      continuous = TRUE, limits = limits, diverging = identical(style$style, "diverging")
    ))
  }
  if (is.factor(values) || is.character(values) || identical(style$style, "cat")) {
    fac <- as.factor(values)
    levels <- levels(fac)
    if (!"No data" %in% levels) levels <- c(levels, "No data")
    plot_data$.map_fill <- as.character(fac)
    plot_data$.map_fill[is.na(plot_data$.map_fill) | !nzchar(plot_data$.map_fill)] <- "No data"
    plot_data$.map_fill <- factor(plot_data$.map_fill, levels = levels)
    colors <- stats::setNames(c(map_palette_values(style$palette, length(levels) - 1L), map_no_data_colour()), levels)
    return(list(data = plot_data, fill = ".map_fill", colors = colors, title = style$title))
  }

  breaks <- style$breaks
  labels <- style$labels
  if (is.null(breaks)) {
    breaks <- map_pretty_breaks(values, n = 5L)
    labels <- map_cut_labels(breaks)
  }
  if (is.null(breaks) || length(breaks) < 2L) {
    levels <- "No data"
    plot_data$.map_fill <- factor("No data", levels = levels)
    colors <- stats::setNames(map_no_data_colour(), levels)
  } else {
    if (is.null(labels) || length(labels) != length(breaks) - 1L) labels <- map_cut_labels(breaks)
    levels <- c(labels, "No data")
    plot_data$.map_fill <- as.character(cut(values, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = labels))
    plot_data$.map_fill[is.na(plot_data$.map_fill) | !nzchar(plot_data$.map_fill)] <- "No data"
    plot_data$.map_fill <- factor(plot_data$.map_fill, levels = levels)
    colors <- stats::setNames(c(map_palette_values(style$palette, length(labels)), map_no_data_colour()), levels)
  }
  list(data = plot_data, fill = ".map_fill", colors = colors, title = style$title)
}

map_overlay_rows <- function(plot_data, fill_column = ".map_fill") {
  if (!fill_column %in% names(plot_data)) return(rep(FALSE, nrow(plot_data)))
  fill <- plot_data[[fill_column]]
  if (is.numeric(fill)) return(is.finite(fill))
  fill <- as.character(fill)
  !is.na(fill) & nzchar(fill) & fill != "No data"
}

public_map_colorbar_dimensions <- function() {
  list(
    height = grid::unit(92, "pt"),
    width = grid::unit(10, "pt")
  )
}

public_map_colorbar_guide <- function() {
  dimensions <- public_map_colorbar_dimensions()
  ggplot2::guide_colorbar(
    title.position = "top",
    label.position = "right",
    theme = ggplot2::theme(
      legend.key.height = dimensions$height,
      legend.key.width = dimensions$width
    )
  )
}

map_legend_override <- function(colors) {
  list(
    fill = unname(colors),
    color = rep("grey35", length(colors)),
    linewidth = rep(0.25, length(colors)),
    alpha = rep(1, length(colors))
  )
}

public_map_reference_layer <- function(plot_data, boundary_reference, name) {
  layer <- boundary_reference[[name]]
  if (!inherits(plot_data, "sf") || !inherits(layer, "sf") || !nrow(layer)) {
    return(NULL)
  }
  if (is.na(sf::st_crs(plot_data)) || is.na(sf::st_crs(layer))) {
    stop("Public map reference geometries must have defined coordinate reference systems.", call. = FALSE)
  }
  sf::st_transform(layer, sf::st_crs(plot_data))
}

public_map_disputed_display <- function(plot_data, boundary_reference = NULL) {
  public_map_reference_layer(plot_data, boundary_reference, "disputed_display")
}

mask_public_map_disputed_areas <- function(plot_data, disputed_areas = NULL) {
  if (!inherits(plot_data, "sf") || !inherits(disputed_areas, "sf") || !nrow(disputed_areas)) {
    return(plot_data)
  }
  mask <- sf::st_union(sf::st_geometry(disputed_areas))
  # Use sf's binary-operation method rather than replacing the geometry column
  # manually. A mask may erase a district completely; st_difference.sf() then
  # drops that empty feature while preserving the attributes of surviving pieces.
  # District attributes are constant over each display polygon by construction.
  out <- plot_data
  sf::st_agr(out) <- "constant"
  sf::st_difference(out, mask)
}

public_map_state_polygons <- function(plot_data) {
  if (!inherits(plot_data, "sf") || !nrow(plot_data)) return(NULL)
  if (!"state_code_2001" %in% names(plot_data)) {
    stop("Public map geometry is missing canonical Census-2001 state codes.", call. = FALSE)
  }
  state <- plain_chr(plot_data$state_code_2001)
  keep <- !is.na(state) & nzchar(state)
  if (!any(keep)) return(NULL)

  data <- plot_data[keep, attr(plot_data, "sf_column"), drop = FALSE]
  states <- stats::aggregate(
    data,
    by = list(state_code_2001 = state[keep]),
    FUN = length
  )
  sf::st_make_valid(states)
}

public_map_exterior_lines <- function(polygons) {
  if (!inherits(polygons, "sf") || !nrow(polygons)) return(NULL)
  polygons <- polygons[!sf::st_is_empty(polygons), , drop = FALSE]
  if (!nrow(polygons)) return(NULL)

  # Remove internal holes before converting polygon shells to linework. This
  # prevents geometry gaps/slivers from being promoted to major map borders.
  shells <- sf::st_exterior_ring(polygons)
  outlines <- sf::st_boundary(shells)
  outlines <- outlines[!sf::st_is_empty(outlines), , drop = FALSE]
  if (!nrow(outlines)) return(NULL)
  outlines
}

public_map_state_outlines <- function(plot_data) {
  states <- public_map_state_polygons(plot_data)
  if (is.null(states)) return(NULL)

  outlines <- public_map_exterior_lines(states)
  if (!is.null(outlines)) outlines$boundary_role <- "state_outline"
  outlines
}

build_public_ggplot_map <- function(plot_data, spec, boundary_reference = NULL) {
  need_pkg("ggplot2", "classified choropleth maps")
  style <- public_map_style(spec$variable)
  fill <- public_map_fill(plot_data, spec$variable, style)
  plot_data <- fill$data
  overlay <- plot_data[map_overlay_rows(plot_data, fill$fill), , drop = FALSE]
  if (!nrow(overlay)) {
    stop("Map figure '", spec$name, "' has no non-missing overlay districts for variable '", spec$variable, "'.", call. = FALSE)
  }

  disputed_display <- public_map_disputed_display(plot_data, boundary_reference)
  display_data <- mask_public_map_disputed_areas(plot_data, disputed_display)
  state_outlines <- public_map_state_outlines(display_data)
  base <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = display_data, ggplot2::aes(fill = .data[[fill$fill]]),
      color = "grey55", linewidth = map_district_boundary_linewidth(),
      linetype = "solid"
    )
  if (!is.null(disputed_display) && nrow(disputed_display)) {
    base <- base + ggplot2::geom_sf(
      data = disputed_display, fill = map_disputed_no_data_colour(),
      color = "grey45", linewidth = map_disputed_boundary_linewidth(),
      linetype = "solid"
    )
  }
  if (!is.null(state_outlines) && nrow(state_outlines)) {
    base <- base + ggplot2::geom_sf(
      data = state_outlines, color = "grey15",
      linewidth = map_major_boundary_linewidth(), linetype = "solid"
    )
  }
  base <- base +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(fill = fill$title) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.height = grid::unit(16, "pt"),
      legend.spacing.y = grid::unit(4, "pt"),
      plot.margin = grid::unit(c(2, 2, 2, 2), "pt")
    )

  if (isTRUE(fill$continuous)) {
    return(base + ggplot2::scale_fill_gradientn(
      colours = fill$colors,
      limits = fill$limits,
      na.value = map_no_data_colour(),
      guide = public_map_colorbar_guide()
    ))
  }

  base +
    ggplot2::scale_fill_manual(
      values = fill$colors,
      breaks = names(fill$colors),
      limits = names(fill$colors),
      drop = FALSE,
      na.translate = TRUE,
      na.value = map_no_data_colour()
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = map_legend_override(fill$colors)
      )
    )
}

complete_public_map_geometry <- function(district_panel, map_geometry) {
  if (!has_sf_geometry(map_geometry)) {
    stop("Public maps require the complete Census-2001 district geometry.", call. = FALSE)
  }
  key <- "target_unit_2001"
  if (!key %in% names(map_geometry) && "unit_id" %in% names(map_geometry)) {
    map_geometry[[key]] <- map_geometry$unit_id
  }
  if (!key %in% names(map_geometry) || !key %in% names(district_panel)) {
    stop("Public map geometry and panel must contain a Census-2001 unit key.", call. = FALSE)
  }
  if (anyDuplicated(map_geometry[[key]])) {
    stop("Complete Census-2001 map geometry must contain one row per district.", call. = FALSE)
  }
  attributes <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else safe_df(district_panel)
  attributes <- attributes[!duplicated(attributes[[key]]), , drop = FALSE]
  out <- merge(map_geometry, attributes, by = key, all.x = TRUE, sort = FALSE)

  # State identity belongs to the complete Census geometry, not to the analysis
  # panel: districts absent from a particular estimand must still participate in
  # state/exterior linework. Canonical IDs encode the 2001 state code directly.
  canonical_state <- public_map_state_code_2001(out[[key]])
  if ("state_code_2001" %in% names(out)) {
    panel_state <- plain_chr(out$state_code_2001)
    mismatch <- !is.na(panel_state) & nzchar(panel_state) & panel_state != canonical_state
    if (any(mismatch)) {
      stop("Public map panel state codes disagree with canonical district IDs.", call. = FALSE)
    }
  }
  out$state_code_2001 <- canonical_state
  out
}

save_map_plot_formats <- function(map_plot, path_base, formats, width = 8, height = 6, dpi = 300) {
  save_plot_formats(map_plot, path_base, formats, width = width, height = height, dpi = dpi)
}

save_map_figure <- function(spec, path_base, district_panel, map_geometry, boundary_reference, formats) {
  if (!has_sf_geometry(district_panel)) {
    stop("Map figure '", spec$name, "' requires an sf district_panel with validated geometry.", call. = FALSE)
  }
  if (is.null(spec$variable) || !spec$variable %in% names(district_panel)) {
    stop("Map figure '", spec$name, "' is missing variable '", spec$variable, "'.", call. = FALSE)
  }

  plot_data <- complete_public_map_geometry(district_panel, map_geometry)
  plot_data <- prepare_public_map_data(plot_data, spec$variable)
  p <- build_public_ggplot_map(plot_data, spec, boundary_reference = boundary_reference)
  save_map_plot_formats(p, path_base, formats, width = 7.2, height = 5.2, dpi = 300)
}

read_carveout_shift_data <- function(path = "data/raw/district_changes/District Carve-Outs and Renamings 1961-2001.csv") {
  if (!file.exists(path) && nzchar(Sys.getenv("EMI_PROJECT_ROOT"))) {
    path <- file.path(Sys.getenv("EMI_PROJECT_ROOT"), path)
  }
  if (!file.exists(path)) return(data.frame())
  out <- read_district_carveouts(path)
  out[is.finite(out$pct_91in01), , drop = FALSE]
}

save_district_carveouts_shifts <- function(spec, path_base, formats) {
  need_pkg("ggplot2", "district carve-outs figure")
  carveouts <- read_carveout_shift_data()
  if (!nrow(carveouts)) stop("District carve-out source data is unavailable.", call. = FALSE)
  binwidth <- diff(range(carveouts$pct_91in01, na.rm = TRUE)) / 40
  if (!is.finite(binwidth) || binwidth <= 0) binwidth <- 1
  p <- ggplot2::ggplot(carveouts, ggplot2::aes(x = pct_91in01)) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = "goldenrod", color = NA) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      y = "Number of 2001 Districts",
      x = "Percentage of a 1991 District's Population in the 2001 District"
    ) +
    ggplot2::theme_grey(base_size = 10)
  save_plot_formats(p, path_base, formats, width = 4.8, height = 3.0, dpi = 300)
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
  imgs <- lapply(inputs, function(p) magick::image_scale(magick::image_read(p), "1200"))
  rows <- split(imgs, ceiling(seq_along(imgs) / 2))
  row_imgs <- lapply(rows, function(row) magick::image_append(magick::image_join(row), stack = FALSE))
  collage <- magick::image_append(magick::image_join(row_imgs), stack = TRUE)
  save_magick_formats(collage, path_base, formats)
}


geographic_adjustment_specs <- function() {
  adjustments <- iv_adjustment_sets()
  ids <- c(raw = "unadjusted", region = "region_main", state = "state_main")
  labels <- c(
    raw = "No geographic FE",
    region = "Region FE + Census controls",
    state = "State FE + Census controls"
  )
  lapply(names(ids), function(id) {
    spec <- adjustments[[ids[[id]]]]
    list(
      label = labels[[id]],
      adjustment_id = ids[[id]],
      fixed_effect = spec$fixed_effect,
      controls = spec$controls
    )
  }) |> stats::setNames(names(ids))
}

first_stage_absorption_specs <- geographic_adjustment_specs

adjustment_spec_controls <- function(specs = geographic_adjustment_specs()) {
  unique(unlist(lapply(specs, `[[`, "controls"), use.names = FALSE))
}

first_stage_absorption_common_sample <- function(data, specs, treatment, instrument) {
  controls <- adjustment_spec_controls(specs)
  required <- unique(c(treatment, instrument, "state_code_2001", "region", controls))
  if (length(setdiff(required, names(data)))) return(data.frame())
  out <- data[stats::complete.cases(data[, required, drop = FALSE]), required, drop = FALSE]
  rownames(out) <- NULL
  out
}

residualize_for_adjustment <- function(data, variable, fixed_effect, controls) {
  residualize_variable(data, variable, adjustment_residual_terms(fixed_effect, controls))
}

first_stage_absorption_bins <- function(x, y, bins = 20L) {
  bins <- max(2L, min(as.integer(bins), length(x)))
  group <- dplyr::ntile(x, bins)
  out <- stats::aggregate(
    data.frame(x = x, y = y),
    by = list(bin = group),
    FUN = mean
  )
  out[order(out$bin), , drop = FALSE]
}

first_stage_absorption_data <- function(district_panel, bins = 20L) {
  need_pkg("sandwich", "first-stage absorption figure")
  df <- as.data.frame(district_panel)
  y <- "emi_exposure_all_children_0708"
  z <- "ling_distance_nonzero_mean"
  specs <- first_stage_absorption_specs()
  dat <- first_stage_absorption_common_sample(df, specs, y, z)
  if (nrow(dat) < 25L || length(unique(dat$state_code_2001)) < 2L) return(data.frame())

  out <- lapply(names(specs), function(id) {
    spec <- specs[[id]]
    if (identical(spec$fixed_effect, "none") && !length(spec$controls)) {
      x <- dat[[z]]
      response <- dat[[y]]
      fit <- stats::lm(response ~ x)
      intercept <- unname(stats::coef(fit)[[1L]])
      slope_index <- 2L
    } else {
      response <- residualize_for_adjustment(dat, y, spec$fixed_effect, spec$controls)
      x <- residualize_for_adjustment(dat, z, spec$fixed_effect, spec$controls)
      fit <- stats::lm(response ~ 0 + x)
      intercept <- 0
      slope_index <- 1L
    }
    vcov <- sandwich::vcovCL(fit, cluster = dat$state_code_2001, type = "HC1")
    beta <- unname(stats::coef(fit)[[slope_index]])
    se <- sqrt(vcov[slope_index, slope_index])
    binned <- first_stage_absorption_bins(x, response, bins = bins)
    data.frame(
      specification_id = id,
      adjustment_id = spec$adjustment_id,
      specification = spec$label,
      bin = binned$bin,
      x = binned$x,
      y = binned$y,
      intercept = intercept,
      beta = beta,
      se = se,
      f_stat = (beta / se)^2,
      n = nrow(dat),
      stringsAsFactors = FALSE
    )
  })
  safe_bind_rows(out)
}

save_first_stage_absorption <- function(spec, path_base, formats, district_panel) {
  need_pkg("ggplot2", "first-stage absorption figure")
  plot_data <- first_stage_absorption_data(district_panel)
  if (!nrow(plot_data)) stop("First-stage absorption figure could not build any specification panels.", call. = FALSE)
  labels <- plot_data[!duplicated(plot_data$specification_id), c("specification_id", "specification", "f_stat"), drop = FALSE]
  labels$panel_label <- paste0(
    labels$specification, "
F = ",
    formatC(labels$f_stat, format = "f", digits = 2)
  )
  plot_data$panel_label <- labels$panel_label[match(plot_data$specification_id, labels$specification_id)]
  plot_data$panel_label <- factor(plot_data$panel_label, levels = labels$panel_label)

  line_data <- plot_data[!duplicated(plot_data$specification_id), , drop = FALSE]
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, color = "grey70") +
    ggplot2::geom_abline(
      data = line_data,
      ggplot2::aes(intercept = intercept, slope = beta),
      inherit.aes = FALSE,
      linewidth = 0.8
    ) +
    ggplot2::geom_point(size = 2.6, alpha = 0.65) +
    ggplot2::facet_wrap(~ panel_label, scales = "free", nrow = 1) +
    ggplot2::labs(
      x = "Linguistic distance (raw or residualized)",
      y = "EMI exposure (raw or residualized)",
      caption = paste(
        "Points are equal-frequency bin means on one common sample; lines are OLS first-stage fits.",
        "F statistics use state-clustered covariance. Baseline covariates are from Census 2001."
      )
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 10.5),
      plot.caption = ggplot2::element_text(size = 8.5, hjust = 0),
      axis.title = ggplot2::element_text(face = "bold")
    )
  save_plot_formats(p, path_base, formats, width = 9.6, height = 4.0, dpi = 300)
}


consumption_dynamic_round_label <- function(round_id) {
  labels <- c(
    nss_2009_10_type2 = "2009-10",
    nss_2011_12_type2 = "2011-12",
    hces_2022_23 = "2022-23",
    hces_2023_24 = "2023-24"
  )
  out <- unname(labels[plain_chr(round_id)])
  out[is.na(out)] <- plain_chr(round_id)[is.na(out)]
  out
}

consumption_iv_dynamic_figure_data <- function(dynamics) {
  if (is.null(dynamics) || !is.list(dynamics) || is.null(dynamics$summary)) {
    return(data.frame())
  }
  x <- safe_df(dynamics$summary)
  required <- c(
    "outcome_round", "estimand",
    "reduced_form_estimate", "reduced_form_std.error"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Dynamic consumption reduced-form figure input is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  estimand_order <- c("ancova", "change")
  x <- x[plain_chr(x$estimand) %in% estimand_order, , drop = FALSE]
  if (!nrow(x)) {
    stop("Dynamic consumption reduced-form figure has no registered ANCOVA/change rows.", call. = FALSE)
  }
  round_order <- c(
    "nss_2009_10_type2", "nss_2011_12_type2",
    "hces_2022_23", "hces_2023_24"
  )
  key <- paste(plain_chr(x$outcome_round), plain_chr(x$estimand), sep = "__")
  expected <- as.vector(outer(round_order, estimand_order, paste, sep = "__"))
  if (anyDuplicated(key) || !setequal(key, expected)) {
    stop(
      "Dynamic consumption reduced-form figure requires exactly one ANCOVA and one change row at each planned horizon.",
      call. = FALSE
    )
  }
  x <- x[match(expected, key), , drop = FALSE]

  estimate <- num(x$reduced_form_estimate)
  std_error <- num(x$reduced_form_std.error)
  if (any(!is.finite(estimate)) || any(!is.finite(std_error))) {
    stop(
      "Dynamic consumption reduced-form figure requires finite estimates and standard errors.",
      call. = FALSE
    )
  }
  estimand_labels <- c(ancova = "ANCOVA", change = "Long change")
  horizon_labels <- consumption_dynamic_round_label(round_order)
  data.frame(
    outcome_round = plain_chr(x$outcome_round),
    horizon = factor(
      consumption_dynamic_round_label(x$outcome_round),
      levels = horizon_labels
    ),
    estimand = factor(
      unname(estimand_labels[plain_chr(x$estimand)]),
      levels = unname(estimand_labels[estimand_order])
    ),
    estimate = estimate,
    std.error = std_error,
    conf.low = estimate - stats::qnorm(0.975) * std_error,
    conf.high = estimate + stats::qnorm(0.975) * std_error,
    stringsAsFactors = FALSE
  )
}

save_consumption_iv_dynamic_figure <- function(
    spec, path_base, formats, dynamics) {
  need_pkg("ggplot2", "dynamic consumption reduced-form figure")
  plot_data <- consumption_iv_dynamic_figure_data(dynamics)
  dodge <- ggplot2::position_dodge(width = 0.35)

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = horizon, y = estimate,
      group = estimand, shape = estimand, linetype = estimand
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, linetype = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      width = 0.10, linewidth = 0.55, position = dodge
    ) +
    ggplot2::geom_line(linewidth = 0.55, position = dodge) +
    ggplot2::geom_point(size = 2.4, position = dodge) +
    ggplot2::labs(
      title = spec$title,
      subtitle = spec$subtitle,
      x = "Outcome horizon",
      y = "Coefficient on linguistic distance",
      shape = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = "top"
    )

  save_plot_formats(
    p, path_base, formats,
    width = 7.2, height = 4.8, dpi = 300
  )
}

paper_schooling_access_outcome_registry <- function() {
  data.frame(
    outcome = c(
      "enrollment_rate_0708",
      "emi_share_enrolled_0708",
      "private_share_enrolled_0708",
      "private_emi_exposure_all_children_0708"
    ),
    label = c(
      "Enrollment",
      "EMI among enrolled",
      "Private enrollment",
      "Private EMI exposure"
    ),
    panel_a = TRUE,
    panel_b = c(FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
}

paper_schooling_access_group_labels <- function(x) {
  labels <- c(
    "Other Backward Class" = "OBC",
    "Scheduled Caste" = "SC",
    "Scheduled Tribe" = "ST"
  )
  out <- unname(labels[as.character(x)])
  out[is.na(out)] <- as.character(x)[is.na(out)]
  out
}

paper_schooling_access_plot_data <- function(diagnostic) {
  registry <- paper_schooling_access_outcome_registry()
  groups <- c("Other Backward Class", "Scheduled Caste", "Scheduled Tribe")

  main <- safe_df(diagnostic$access_summary %||% data.frame())
  required <- c("social_group", "reference_group", "outcome", "mean_district_gap_percentage_points")
  if (length(setdiff(required, names(main)))) return(data.frame())
  main <- main[
    main$social_group %in% groups &
      main$reference_group == "Other" &
      main$outcome %in% registry$outcome[registry$panel_a],
    , drop = FALSE
  ]
  main$panel <- "A. Overall"
  main$stratum <- "All children"

  cross <- safe_df(diagnostic$access_crosscuts %||% data.frame())
  cross_required <- c(required, "crosscut", "stratum")
  if (length(setdiff(cross_required, names(cross)))) return(data.frame())
  cross <- cross[
    cross$social_group %in% groups &
      cross$reference_group == "Other" &
      cross$crosscut == "sector" &
      cross$stratum %in% c("Rural", "Urban") &
      cross$outcome %in% registry$outcome[registry$panel_b],
    , drop = FALSE
  ]
  cross$panel <- "B. Rural versus urban"

  keep <- c("social_group", "outcome", "mean_district_gap_percentage_points", "panel", "stratum")
  out <- safe_bind_rows(list(main[keep], cross[keep]))
  if (!nrow(out)) return(out)
  out$group <- paper_schooling_access_group_labels(out$social_group)
  out$outcome_label <- registry$label[match(out$outcome, registry$outcome)]
  out$gap <- suppressWarnings(as.numeric(out$mean_district_gap_percentage_points))
  out <- out[is.finite(out$gap) & !is.na(out$outcome_label), , drop = FALSE]
  out$group <- factor(out$group, levels = c("OBC", "SC", "ST"))
  out$panel <- factor(
    out$panel,
    levels = c("A. Overall", "B. Rural versus urban")
  )
  out$stratum <- factor(out$stratum, levels = c("All children", "Rural", "Urban"))
  out$outcome_label <- factor(
    out$outcome_label,
    levels = rev(registry$label)
  )
  rownames(out) <- NULL
  out
}

save_schooling_access_figure <- function(spec, path_base, formats, diagnostic) {
  data <- paper_schooling_access_plot_data(diagnostic)
  if (!nrow(data)) return(save_status_figure(spec, format_path(path_base, "png")))

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = gap, y = outcome_label, shape = stratum, color = stratum)
  ) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.45, linetype = 2) +
    ggplot2::geom_point(size = 3.0, position = ggplot2::position_dodge(width = 0.45)) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::facet_grid(panel ~ group, scales = "free_y", space = "free_y") +
    ggplot2::labs(
      x = "Mean difference relative to Other (percentage points)",
      y = NULL,
      shape = "Sample",
      color = "Sample"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )

  save_plot_formats(p, path_base, formats, width = 8.2, height = 6.2, dpi = 300)
}


prune_stale_figure_files <- function(dir, expected_paths) {
  existing <- list.files(
    dir, pattern = "\\.(pdf|png)$", full.names = TRUE, ignore.case = TRUE
  )
  stale <- setdiff(existing, expected_paths)
  if (length(stale)) unlink(stale)
  invisible(stale)
}


save_figures <- function(figures, cfg) {
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
      map = save_map_figure(
        spec, path_base,
        attr(figures, "district_panel") %||% data.frame(),
        attr(figures, "map_geometry") %||% data.frame(),
        attr(figures, "map_boundary_reference") %||% list(),
        formats
      ),
      district_carveouts_shifts = save_district_carveouts_shifts(spec, path_base, formats),
      first_stage_absorption = save_first_stage_absorption(spec, path_base, formats, attr(figures, "district_panel") %||% data.frame()),
      consumption_iv_dynamics = save_consumption_iv_dynamic_figure(
        spec, path_base, formats,
        attr(figures, "consumption_iv_dynamics")
      ),
      schooling_access = save_schooling_access_figure(
        spec, path_base, formats,
        attr(figures, "schooling_access")
      ),
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

  prune_stale_figure_files(dir, unname(all_written))

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
