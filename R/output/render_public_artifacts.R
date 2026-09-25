# Render public-facing Quarto artifacts from within the {targets} graph.

#' Render a public PDF from a Quarto source
#'
#' @param qmd Path to the public QMD.
#' @param dependencies Objects or file targets that must be current before the
#'   document renders. The renderer does not interpret them; forcing the list
#'   makes the publication dependency explicit to {targets}.
#' @param reference_aux Build a stable LaTeX auxiliary file containing the
#'   rendered document's labels for application-sample references.
#' @return Character vector of rendered file paths for a `format = "file"`
#'   target.
render_public_pdf <- function(qmd, dependencies = list(), reference_aux = FALSE) {
  force(dependencies)

  if (!file.exists(qmd)) {
    stop("Public PDF source QMD does not exist: ", qmd, call. = FALSE)
  }
  if (!nzchar(Sys.which("quarto"))) {
    stop("Quarto CLI was not found on PATH; cannot render ", qmd, call. = FALSE)
  }

  pdf_path <- file.path(dirname(qmd), paste0(tools::file_path_sans_ext(basename(qmd)), ".pdf"))
  args <- c("render", qmd, "--to", "pdf")
  if (isTRUE(reference_aux)) {
    # Quarto reliably preserves the generated .tex file with keep-tex. The
    # renderer then makes a separate no-PDF XeLaTeX pass to retain the label
    # file needed by writing excerpts. This avoids relying on Quarto's cleanup
    # policy for temporary .aux files.
    args <- c(args, "-M", "keep-tex:true")
  }
  status <- system2("quarto", args)
  if (!identical(status, 0L)) {
    stop("quarto render ", qmd, " --to pdf failed with status ", status, call. = FALSE)
  }
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) {
    stop("quarto render did not create a non-empty ", pdf_path, call. = FALSE)
  }

  if (!isTRUE(reference_aux)) return(pdf_path)

  tex_path <- file.path(dirname(qmd), paste0(tools::file_path_sans_ext(basename(qmd)), ".tex"))
  aux_path <- materialize_latex_reference_aux(tex_path)
  c(pdf_path, aux_path)
}

#' Materialize a stable LaTeX label file from a rendered `.tex` document
#'
#' XeLaTeX writes cross-reference labels during its first pass. Running it with
#' `-no-pdf` and a distinct job name leaves the already-rendered PDF untouched
#' while producing the `.aux` file consumed by the application-sample renderer.
#'
#' @param tex_path Path to a rendered LaTeX file.
#' @param job_name Stable basename for the retained auxiliary file.
#' @return Path to the non-empty auxiliary file.
materialize_latex_reference_aux <- function(
    tex_path,
    job_name = paste0(tools::file_path_sans_ext(basename(tex_path)), "-reference-labels")
) {
  if (!file.exists(tex_path) || file.info(tex_path)$size <= 0L) {
    stop("Rendered LaTeX file is missing or empty: ", tex_path, call. = FALSE)
  }
  engine_path <- Sys.which("xelatex")
  if (!nzchar(engine_path)) {
    stop("xelatex was not found on PATH; cannot materialize cross-reference labels.", call. = FALSE)
  }

  tex_path <- normalizePath(tex_path, winslash = "/", mustWork = TRUE)
  output_dir <- dirname(tex_path)
  aux_path <- file.path(output_dir, paste0(job_name, ".aux"))
  transient_paths <- file.path(output_dir, paste0(job_name, c(".log", ".out", ".toc", ".xdv")))
  # A stable job name is useful to downstream sample rendering, but XeLaTeX also
  # reads an existing auxiliary file with that name. Remove prior pass files so
  # the retained label index can only describe the current manuscript render.
  unlink(c(aux_path, transient_paths), force = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(output_dir)

  status <- system2(
    engine_path,
    c(
      "-no-pdf",
      "-interaction=nonstopmode",
      "-halt-on-error",
      paste0("-jobname=", job_name),
      shQuote(basename(tex_path))
    )
  )
  if (!identical(status, 0L)) {
    stop("xelatex failed while materializing LaTeX reference labels with status ", status, call. = FALSE)
  }

  if (!file.exists(aux_path) || file.info(aux_path)$size <= 0L) {
    stop("LaTeX reference-label pass did not create a non-empty ", aux_path, call. = FALSE)
  }

  unlink(transient_paths, force = TRUE)
  aux_path
}


poster_typst_template_paths <- function(poster_qmd) {
  extension_dir <- file.path(dirname(poster_qmd), "_extensions", "poster")
  c(
    template = file.path(extension_dir, "typst-template.typ"),
    show = file.path(extension_dir, "typst-show.typ")
  )
}



validate_poster_typst_templates <- function(poster_qmd) {
  paths <- poster_typst_template_paths(poster_qmd)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Poster Typst template file(s) missing: ",
      paste(unname(missing), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(paths)
}

poster_required_assets <- function() {
  c(
    "assets/uw-logo-horizontal-full-color-print.pdf",
    "assets/repo-qr.svg"
  )
}


#' Render the conference poster PDF and a PNG preview
#'
#' @param poster_qmd Path to the poster QMD.
#' @param figure_files Generated figure dependencies.
#' @param poster_assets Poster image dependencies.
#' @param project_root Repository root used as Typst's project root.
#' @return Rendered poster paths.
render_poster_pdf <- function(poster_qmd, figure_files, poster_assets, project_root = ".") {
  force(figure_files)
  force(poster_assets)
  if (!file.exists(poster_qmd)) stop("Poster source QMD does not exist: ", poster_qmd, call. = FALSE)
  if (!nzchar(Sys.which("quarto"))) stop("Quarto CLI was not found on PATH; cannot render ", poster_qmd, call. = FALSE)
  validate_poster_typst_templates(poster_qmd)
  typst_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  required_assets <- poster_required_assets()
  missing <- required_assets[!file.exists(file.path(typst_root, required_assets))]
  if (length(missing)) stop("Poster asset(s) missing: ", paste(missing, collapse = ", "), call. = FALSE)
  pdf_path <- file.path(dirname(poster_qmd), "poster.pdf")
  status <- system2(
    "quarto",
    c("render", poster_qmd),
    env = paste0("TYPST_ROOT=", shQuote(typst_root))
  )
  if (!identical(status, 0L)) stop("quarto render ", poster_qmd, " failed with status ", status, call. = FALSE)
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) stop("Poster render did not create a non-empty ", pdf_path, call. = FALSE)
  png_path <- render_poster_png(pdf_path, file.path(dirname(pdf_path), "RishavRoy-Education.png"))
  c(pdf_path, png_path)
}

render_poster_png <- function(pdf_path, png_path = sub("\\.pdf$", ".png", pdf_path), dpi = 220) {
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) {
    stop("Poster PDF does not exist or is empty: ", pdf_path, call. = FALSE)
  }
  if (nzchar(Sys.which("pdftoppm"))) {
    prefix <- sub("\\.png$", "", png_path)
    status <- system2(
      "pdftoppm",
      c("-singlefile", "-png", "-r", as.character(dpi), shQuote(pdf_path), shQuote(prefix))
    )
    if (!identical(status, 0L)) stop("pdftoppm failed to create poster PNG with status ", status, call. = FALSE)
  } else {
    need_pkg("magick", "poster PNG preview rendering")
    image <- magick::image_read_pdf(pdf_path, density = dpi)
    image <- magick::image_background(image[[1]], "white", flatten = TRUE)
    magick::image_write(image, path = png_path, format = "png")
  }
  if (!file.exists(png_path) || file.info(png_path)$size <= 0L) {
    stop("Poster PNG preview was not created: ", png_path, call. = FALSE)
  }
  png_path
}
