# For the names of all color palette:
#cols4all::c4a_palettes()
# To explore the palettes:
#cols4all::c4a_gui()
# To plot a palette:
#cols4all::c4a("zissou1continuous", 7) %>% cols4all::c4a_plot()
# To view the color codes of a palette:
#cols4all::c4a("rd_yl_bu", 5)
# To explore some of the palettes supported by tmap:
# tmaptools::palette_explorer()

joined_df <- joined_df_tracker


# Based on geoemetry from @bhatiaMergingUpdatedDistrictlevel2020, and code from @nowosadElegantInformativeMaps2021

asp = 0

map_consumption_pct_change <- tm_shape(joined_df) +
  tm_fill("consumption_pct_change", palette = "brewer.reds", title = "%Δ Consumption", style = "cont", legend.hist = FALSE) + 
  tm_borders(alpha = 0.2) +
  tm_layout(frame = FALSE, asp = asp)
# tmap_save(tm = map_consumption_pct_change, filename = "map_consumption_pct_change.png", width = 8, height = 6, units = "in", dpi = 300) # dots per inch (300 dpi is print quality)
# Recall that this change does not account for inflation!

map_EMIE <- tm_shape(joined_df) +
  tm_fill("EMIE", palette = "brewer.blues", title = "EMI Exposure", style = "fixed", breaks = c(0, 2.5, 10, 25, 50, 100), legend.hist = TRUE) + 
  tm_borders(alpha = 0.2) +
  tm_layout(frame = FALSE, asp = asp)
# tmap_save(tm = map_EMIE, filename = "map_EMIE.png", width = 8, height = 6, units = "in", dpi = 300) # dots per inch (300 dpi is print quality)
# To help determine where breaks should be:
# tapply((joined_df %>% drop_na(EMIE))$EMIE,  (kmeans((joined_df %>% drop_na(EMIE))$EMIE, centers = 6))$cluster,  range)

map_pucca <- tm_shape(joined_df) +
  tm_fill("pct_pucca", palette = "brown", title = "% Pucca Homes", legend.hist = TRUE) + 
  tm_borders(alpha = 0.2) +
  tm_layout(frame = FALSE, asp = asp)
# tmap_save(tm = map_pucca, filename = "map_pucca.png", width = 8, height = 6, units = "in", dpi = 300) # dots per inch (300 dpi is print quality)

map_pct_head_secondary_plus <- tm_shape(joined_df) +
  tm_fill("pct_head_secondary_plus", palette = "brewer.greens", title = "% HH Head w/ Sec.+", style = "cont", legend.hist = FALSE) + 
  tm_borders(alpha = 0.2) +
  tm_layout(frame = FALSE, asp = asp)
# tmap_save(tm = map_pct_head_secondary_plus, filename = "map_pct_head_secondary_plus.png", width = 8, height = 6, units = "in", dpi = 300) # dots per inch (300 dpi is print quality)

map_region <- tm_shape(joined_df) +
  tm_fill("region", palette = "brewer.dark2", title = "Region", legend.hist = TRUE) + 
  tm_borders(alpha = 0.2) +
  tm_layout(frame = FALSE, asp = asp)


map_ling_dist <- tm_shape(joined_df) +
  tm_fill("wavg_ling_degrees", palette = "carto.emrld", title = "Linguistic Distance", legend.hist = TRUE) + 
  tm_borders(alpha = 0.2) +
  tm_layout(frame = FALSE, asp = asp)


# To print all of the maps:
# for(nm in ls(pattern = "^map_")) {print(get(nm))}


#### Save the maps ####
# Uncomment when/if final_draft == TRUE

# width = 6
# height = 5
# # Save each tmap object as a PNG file
# tmap_save(map_EMIE, "map_EMIE.png", width = width, height = height, units = "in", dpi = 300)
# tmap_save(map_consumption_pct_change, "map_consumption.png", width = width, height = height, units = "in", dpi = 300)
# tmap_save(map_pucca, "map_pucca.png", width = width, height = height, units = "in", dpi = 300)
# tmap_save(map_pct_head_secondary_plus, "map_edu.png", width = width, height = height, units = "in", dpi = 300)
# tmap_save(map_region, "map_region.png", width = width, height = height, units = "in", dpi = 300)
# tmap_save(map_ling_dist, "map_ling_dist.png", width = width, height = height, units = "in", dpi = 300)