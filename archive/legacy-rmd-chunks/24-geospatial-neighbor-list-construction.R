joined_df <- joined_df %>% drop_na()

# Test for best contiguity measure
# Code is based on @liChapter14Spatial2019
# remotes::install_github("spatialanalysis/sfExtras")
# library(sfExtras)
# rook_neighbors <- joined_geom %>% st_rook()
# rook_neighbors %>% lengths() %>% mean()
# 4.780165 average neighbors per district
# queen_neighbors <- joined_geom %>% st_queen()
# queen_neighbors %>% lengths() %>% mean()
# 4.783471

# Build a rook‐contiguity neighbor list
# start.time <- Sys.time()
nb_2020 <- joined_df %>%  
  poly2nb(queen = FALSE)
# end.time <- Sys.time()
# end.time - start.time
# With queen = TRUE: 32.64976 secs
# With queen = FALSE: 31.15051 secs

# Turn that into a binary adjacency matrix
W_2020 <- nb2mat(nb_2020, style = "B", zero.policy = TRUE)
#    W_2020[i,j] == 1 if districts i and j share a border, 0 otherwise
