# Read the images
img1 <- image_read("map_EMIE.png")
img2 <- image_read("map_consumption.png")
img3 <- image_read("map_pucca.png")
img4 <- image_read("map_edu.png")

# Set a standard width, then pad all to a common size
target_width <- 1200
target_height <- 900

# Resize each image by width while preserving aspect ratio
img1 <- image_resize(img1, geometry = paste0(target_width))
img2 <- image_resize(img2, geometry = paste0(target_width))
img3 <- image_resize(img3, geometry = paste0(target_width))
img4 <- image_resize(img4, geometry = paste0(target_width))

# # Then pad with white (or transparent) space to ensure equal height & width for alignment
# img1 <- image_extent(img1, paste0(target_width, "x", target_height), gravity = "center", color = "white")
# img2 <- image_extent(img2, paste0(target_width, "x", target_height), gravity = "center", color = "white")
# img3 <- image_extent(img3, paste0(target_width, "x", target_height), gravity = "center", color = "white")
# img4 <- image_extent(img4, paste0(target_width, "x", target_height), gravity = "center", color = "white")

# # Combine
top_row <- image_append(c(img1, img2))
bottom_row <- image_append(c(img3, img4))
collage <- image_append(c(top_row, bottom_row), stack = TRUE)
#collage <- image_append(c(img1, img2, img3, img4), stack = TRUE)

image_write(collage, path = "collage1_map.png")



# Read the images
img5 <- image_read("map_region.png")
img6 <- image_read("map_ling_dist.png")

# Resize each image by width while preserving aspect ratio
img5 <- image_resize(img5, geometry = paste0(target_width))
img6 <- image_resize(img6, geometry = paste0(target_width))

# # Then pad with white (or transparent) space to ensure equal height & width for alignment
# img5 <- image_extent(img5, paste0(target_width, "x", target_height), gravity = "center", color = "white")
# img6 <- image_extent(img6, paste0(target_width, "x", target_height), gravity = "center", color = "white")

# Combine
collage <- image_append(c(img5, img6), stack = FALSE)

image_write(collage, path = "collage2_map.png")
