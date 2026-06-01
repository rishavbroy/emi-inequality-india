# fig.pos='p': Placed on page with only floats (figs, tables, etc.)


files <- c(
  "Average Monthly Real Earnings Over Time - Total.png",
  "LFPR WPR and Unemployment for All Over Time.png",
  "Unemployment Rate By General Education.png"
)
paths <- file.path("580 Paper Images", files)

width_px <- 1300 # Reduce further if needed
imgs <- lapply(paths, function(p) image_read(p) |> image_scale(paste0(width_px)))

collage <- image_append(image_join(imgs), stack = TRUE)

out_path <- "ILO-fig.png"
image_write(collage, path = out_path)
knitr::include_graphics(out_path)