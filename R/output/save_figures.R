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

public_map_style <- function(variable) {
  switch(
    variable,
    emi_exposure_all_children_0708 = list(
      palette = "brewer.blues",
      title = "EMI Exposure",
      style = "fixed",
      breaks = c(0, 2.5, 10, 25, 50, 100),
      labels = c("0-2.5", "2.5-10", "10-25", "25-50", "50-100")
    ),
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

build_public_ggplot_map <- function(plot_data, spec) {
  need_pkg("ggplot2", "classified choropleth maps")
  style <- public_map_style(spec$variable)
  fill <- public_map_fill(plot_data, spec$variable, style)
  plot_data <- fill$data
  overlay <- plot_data[map_overlay_rows(plot_data, fill$fill), , drop = FALSE]
  if (!nrow(overlay)) {
    stop("Map figure '", spec$name, "' has no non-missing overlay districts for variable '", spec$variable, "'.", call. = FALSE)
  }

  base <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot_data, ggplot2::aes(fill = .data[[fill$fill]]), color = "grey35", linewidth = 0.05) +
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
  merge(map_geometry, attributes, by = key, all.x = TRUE, sort = FALSE)
}

save_map_plot_formats <- function(map_plot, path_base, formats, width = 8, height = 6, dpi = 300) {
  save_plot_formats(map_plot, path_base, formats, width = width, height = height, dpi = dpi)
}

save_map_figure <- function(spec, path_base, district_panel, map_geometry, formats) {
  if (!has_sf_geometry(district_panel)) {
    stop("Map figure '", spec$name, "' requires an sf district_panel with validated geometry.", call. = FALSE)
  }
  if (is.null(spec$variable) || !spec$variable %in% names(district_panel)) {
    stop("Map figure '", spec$name, "' is missing variable '", spec$variable, "'.", call. = FALSE)
  }

  plot_data <- complete_public_map_geometry(district_panel, map_geometry)
  plot_data <- prepare_public_map_data(plot_data, spec$variable)
  p <- build_public_ggplot_map(plot_data, spec)
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


poster_emie_percentiles <- function(district_panel, probs = seq(0.05, 0.95, by = 0.10)) {
  treatment <- preferred_iv_variables()$treatment
  values <- suppressWarnings(as.numeric(as.data.frame(district_panel)[[treatment]]))
  values <- values[is.finite(values)]
  if (!length(values)) return(data.frame())
  out <- data.frame(
    percentile = probs,
    value = unname(stats::quantile(values, probs = probs, na.rm = TRUE, names = FALSE)),
    stringsAsFactors = FALSE
  )
  names(out)[names(out) == "value"] <- treatment
  out
}

first_estimable_iv_model <- function(iv_models) {
  if (inherits(iv_models, "ivreg")) return(iv_models)
  if (!is.list(iv_models)) return(NULL)
  hits <- Filter(function(x) inherits(x, "ivreg"), iv_models)
  if (length(hits)) hits[[1]] else NULL
}

poster_prediction_vcov <- function(model) {
  if (!inherits(model, "ivreg")) {
    stop("Poster prediction covariance requires an ivreg model.", call. = FALSE)
  }

  # targets restores fitted models from RDS without loading the package that
  # registered their S3 methods. Load both namespaces before sandwich dispatch.
  need_pkg("ivreg", "poster expected-values inference")
  need_pkg("sandwich", "poster expected-values inference")

  cluster <- attr(model, "cluster_state", exact = TRUE)
  if (!is.null(cluster) && length(cluster) == stats::nobs(model) && !anyNA(cluster)) {
    return(sandwich::vcovCL(model, cluster = cluster, type = "HC1"))
  }
  sandwich::vcovHC(model, type = "HC1")
}

poster_prediction_data <- function(model) {
  data <- attr(model, "prediction_data", exact = TRUE)
  if (is.null(data)) {
    stop(
      "Poster expected-values inference requires fitted-sample prediction data stored with the IV model.",
      call. = FALSE
    )
  }
  data <- as.data.frame(data)
  treatment <- preferred_iv_variables()$treatment
  if (!nrow(data) || !treatment %in% names(data)) {
    stop("Stored IV prediction data are empty or missing the preferred EMI exposure.", call. = FALSE)
  }
  data
}

poster_expected_value_predictions <- function(model, grid) {
  need_pkg("marginaleffects", "poster expected-values figure")
  treatment <- preferred_iv_variables()$treatment
  marginaleffects::avg_predictions(
    model,
    newdata = poster_prediction_data(model),
    variables = stats::setNames(list(grid[[treatment]]), treatment),
    vcov = poster_prediction_vcov(model),
    type = "response"
  )
}

poster_model_specs <- function() {
  list(
    raw = list(label = "Raw", fixed_effect = "none", controls = character()),
    region = list(label = "Region FE + Census controls", fixed_effect = "region", controls = census_2001_absorption_controls()),
    state = list(label = "State FE + Census controls", fixed_effect = "state", controls = census_2001_absorption_controls())
  )
}

poster_first_stage_specs <- poster_model_specs

poster_first_stage_common_sample <- function(data, specs, treatment, instrument) {
  controls <- unique(unlist(lapply(specs, `[[`, "controls"), use.names = FALSE))
  required <- unique(c(treatment, instrument, "state_code_2001", "region", controls))
  if (length(setdiff(required, names(data)))) return(data.frame())
  out <- data[stats::complete.cases(data[, required, drop = FALSE]), required, drop = FALSE]
  rownames(out) <- NULL
  out
}

poster_residualize_for_spec <- function(data, variable, fixed_effect, controls) {
  poster_residualize(data, variable, poster_residual_terms(fixed_effect, controls))
}

poster_first_stage_spec_data <- function(district_panel) {
  need_pkg("sandwich", "poster first-stage specifications")
  df <- as.data.frame(district_panel)
  y <- "emi_exposure_all_children_0708"
  z <- "ling_distance_nonzero_mean"
  specs <- poster_first_stage_specs()
  dat <- poster_first_stage_common_sample(df, specs, y, z)
  if (nrow(dat) < 25L || length(unique(dat$state_code_2001)) < 2L) return(data.frame())

  out <- lapply(names(specs), function(id) {
    spec <- specs[[id]]
    y_resid <- poster_residualize_for_spec(dat, y, spec$fixed_effect, spec$controls)
    z_resid <- poster_residualize_for_spec(dat, z, spec$fixed_effect, spec$controls)
    fit <- stats::lm(y_resid ~ 0 + z_resid)
    vcov <- sandwich::vcovCL(fit, cluster = dat$state_code_2001, type = "HC1")
    beta <- unname(stats::coef(fit)[[1]])
    se <- sqrt(vcov[1, 1])
    xs <- seq(stats::quantile(z_resid, 0.05, na.rm = TRUE), stats::quantile(z_resid, 0.95, na.rm = TRUE), length.out = 80L)
    estimate <- beta * xs
    margin <- 1.96 * abs(xs) * se
    data.frame(
      specification_id = id,
      specification = spec$label,
      z_resid = xs,
      estimate = estimate,
      conf.low = estimate - margin,
      conf.high = estimate + margin,
      beta = beta,
      se = se,
      f_stat = (beta / se)^2,
      n = nrow(dat),
      stringsAsFactors = FALSE
    )
  })
  safe_bind_rows(out)
}

save_poster_first_stage_specs <- function(spec, path_base, formats, district_panel) {
  need_pkg("ggplot2", "poster first-stage specification plot")
  plot_data <- poster_first_stage_spec_data(district_panel)
  if (!nrow(plot_data)) stop("Poster first-stage figure could not build any specification lines.", call. = FALSE)
  label_data <- plot_data[!duplicated(plot_data$specification), c("specification", "f_stat"), drop = FALSE]
  label_data$label <- paste0(label_data$specification, " (F=", formatC(label_data$f_stat, format = "f", digits = 1), ")")
  plot_data$label <- label_data$label[match(plot_data$specification, label_data$specification)]
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = z_resid, y = estimate)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), fill = "#c5050c", alpha = 0.14) +
    ggplot2::geom_line(color = "#7a0019", linewidth = 1.05) +
    ggplot2::facet_wrap(~ label, scales = "free", nrow = 1) +
    ggplot2::labs(
      x = "Residualized linguistic distance",
      y = "Predicted residual EMI exposure",
      caption = "Treatment is unconditional EMI exposure among children ages 5-19; controls are measured in Census 2001."
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      plot.caption = ggplot2::element_text(size = 9, hjust = 0),
      axis.title = ggplot2::element_text(face = "bold")
    )
  save_plot_formats(p, path_base, formats, width = 9.6, height = 3.9, dpi = 300)
}


poster_second_stage_specs <- poster_model_specs

poster_second_stage_spec_data <- function(district_panel) {
  need_pkg("ivreg", "poster second-stage specifications")
  need_pkg("marginaleffects", "poster second-stage predictions")
  need_pkg("sandwich", "poster second-stage clustered covariance")
  data <- as.data.frame(district_panel)
  outcome <- "real_log_consumption_change"
  treatment <- "emi_exposure_all_children_0708"
  instrument <- "ling_distance_nonzero_mean"
  specs <- poster_second_stage_specs()
  controls <- unique(unlist(lapply(specs, `[[`, "controls"), use.names = FALSE))
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
    fe <- poster_fixed_effect_term(spec$fixed_effect)
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

save_poster_second_stage_specs <- function(spec, path_base, formats, district_panel) {
  need_pkg("ggplot2", "poster second-stage specification plot")
  plot_data <- poster_second_stage_spec_data(district_panel)
  if (!nrow(plot_data)) stop("Poster second-stage figure could not build any specification ribbons.", call. = FALSE)
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
  save_plot_formats(p, path_base, formats, width = 9.6, height = 3.9, dpi = 300)
}

save_emie_expected_values <- function(spec, path_base, formats, district_panel, iv_models) {
  need_pkg("ggplot2", "poster expected-values figure")
  model <- first_estimable_iv_model(iv_models)
  grid <- poster_emie_percentiles(district_panel)
  if (is.null(model) || !nrow(grid)) {
    stop("Poster expected-values figure requires an estimated ivreg model and observed preferred EMI exposure.", call. = FALSE)
  }

  treatment <- preferred_iv_variables()$treatment
  predictions <- poster_expected_value_predictions(model, grid)
  plot_data <- as.data.frame(predictions)
  if (!treatment %in% names(plot_data)) plot_data[[treatment]] <- grid[[treatment]]

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[treatment]], y = estimate)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), fill = "#c5050c", alpha = 0.14) +
    ggplot2::geom_line(color = "#7a0019", linewidth = 1.15) +
    ggplot2::geom_point(color = "#7a0019", size = 2.5) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::labs(
      x = "District EMI exposure",
      y = "Adjusted consumption growth (%)",
      caption = "Points mark the 5th through 95th percentiles; ribbon shows 95% confidence intervals."
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 10, hjust = 0),
      axis.title = ggplot2::element_text(face = "bold")
    )
  save_plot_formats(p, path_base, formats, width = 7.4, height = 4.8, dpi = 300)
}

#' save figures
#'
#' @return A character vector of generated figure and manifest paths.
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
        formats
      ),
      district_carveouts_shifts = save_district_carveouts_shifts(spec, path_base, formats),
      emie_expected_values = save_emie_expected_values(spec, path_base, formats, attr(figures, "district_panel") %||% data.frame(), attr(figures, "iv_models")),
      poster_first_stage_specs = save_poster_first_stage_specs(spec, path_base, formats, attr(figures, "district_panel") %||% data.frame()),
      poster_second_stage_specs = save_poster_second_stage_specs(spec, path_base, formats, attr(figures, "district_panel") %||% data.frame()),
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
