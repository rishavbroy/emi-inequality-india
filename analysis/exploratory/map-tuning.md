# Map and Palette Tuning


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

For the names of all color palette: `cols4all::c4a_palettes()`. To
explore the palettes: `cols4all::c4a_gui()`. To plot a palette:
`cols4all::c4a("zissou1continuous", 7) %>% cols4all::c4a_plot()`. To
view the color codes of a palette: `cols4all::c4a("rd_yl_bu", 5)`. To
explore some of the palettes supported by `tmap`:
`tmaptools::palette_explorer()`.

Based on geometry from @bhatiaMergingUpdatedDistrictlevel2020, and code
from @nowosadElegantInformativeMaps2021. Recall that this change does
not account for inflation! To help determine where breaks should be:
`tapply((joined_df %>% drop_na(EMIE))$EMIE, (kmeans((joined_df %>% drop_na(EMIE))$EMIE, centers = 6))$cluster, range)`.

To print all of the maps:
`for(nm in ls(pattern = "^map_")) {print(get(nm))}`. Save the maps.
Uncomment when/if `final_draft == TRUE`. Save each `tmap` object as a
PNG file with print-quality `dpi = 300`.

``` r
analysis_deviation_note("The current note keeps the legacy palette/break/export prose and renders the active figure_files outputs rather than rerunning GUI palette tools or writing duplicate map-generation code in analysis/.")
```

**Deviation note.** The current note keeps the legacy
palette/break/export prose and renders the active figure_files outputs
rather than rerunning GUI palette tools or writing duplicate
map-generation code in analysis/.

``` r
figure_outputs <- tryCatch(analysis_read_target("figure_files"), error = function(e) character())
figure_manifest <- if (file.exists(analysis_path("outputs", "figures", "main", "figure_manifest.csv"))) {
  read_analysis_csv("figures", "main", "figure_manifest.csv")
} else {
  data.frame(path = figure_outputs, stringsAsFactors = FALSE)
}
figure_manifest
```

                                                     path                      name
    1             outputs/figures/main/fig_ilo_trends.pdf            fig_ilo_trends
    2             outputs/figures/main/fig_ilo_trends.png            fig_ilo_trends
    3  outputs/figures/main/district_carveouts_shifts.pdf district_carveouts_shifts
    4  outputs/figures/main/district_carveouts_shifts.png district_carveouts_shifts
    5           outputs/figures/main/map_emi_exposure.pdf          map_emi_exposure
    6           outputs/figures/main/map_emi_exposure.png          map_emi_exposure
    7     outputs/figures/main/map_consumption_growth.pdf    map_consumption_growth
    8     outputs/figures/main/map_consumption_growth.png    map_consumption_growth
    9                  outputs/figures/main/map_pucca.pdf                 map_pucca
    10                 outputs/figures/main/map_pucca.png                 map_pucca
    11             outputs/figures/main/map_education.pdf             map_education
    12             outputs/figures/main/map_education.png             map_education
    13                outputs/figures/main/map_region.pdf                map_region
    14                outputs/figures/main/map_region.png                map_region
    15   outputs/figures/main/map_linguistic_distance.pdf   map_linguistic_distance
    16   outputs/figures/main/map_linguistic_distance.png   map_linguistic_distance
    17         outputs/figures/main/collage_main_maps.pdf         collage_main_maps
    18         outputs/figures/main/collage_main_maps.png         collage_main_maps
    19    outputs/figures/main/collage_iv_region_maps.pdf    collage_iv_region_maps
    20    outputs/figures/main/collage_iv_region_maps.png    collage_iv_region_maps
       format
    1     pdf
    2     png
    3     pdf
    4     png
    5     pdf
    6     png
    7     pdf
    8     png
    9     pdf
    10    png
    11    pdf
    12    png
    13    pdf
    14    png
    15    pdf
    16    png
    17    pdf
    18    png
    19    pdf
    20    png

The current figure target produced 21 files available to this render.
The map-collage previews below are target outputs, not copied legacy
exploratory graphics.

``` r
figure_manifest[grepl("collage|map", figure_manifest$path, ignore.case = TRUE), , drop = FALSE]
```

                                                   path                    name
    5         outputs/figures/main/map_emi_exposure.pdf        map_emi_exposure
    6         outputs/figures/main/map_emi_exposure.png        map_emi_exposure
    7   outputs/figures/main/map_consumption_growth.pdf  map_consumption_growth
    8   outputs/figures/main/map_consumption_growth.png  map_consumption_growth
    9                outputs/figures/main/map_pucca.pdf               map_pucca
    10               outputs/figures/main/map_pucca.png               map_pucca
    11           outputs/figures/main/map_education.pdf           map_education
    12           outputs/figures/main/map_education.png           map_education
    13              outputs/figures/main/map_region.pdf              map_region
    14              outputs/figures/main/map_region.png              map_region
    15 outputs/figures/main/map_linguistic_distance.pdf map_linguistic_distance
    16 outputs/figures/main/map_linguistic_distance.png map_linguistic_distance
    17       outputs/figures/main/collage_main_maps.pdf       collage_main_maps
    18       outputs/figures/main/collage_main_maps.png       collage_main_maps
    19  outputs/figures/main/collage_iv_region_maps.pdf  collage_iv_region_maps
    20  outputs/figures/main/collage_iv_region_maps.png  collage_iv_region_maps
       format
    5     pdf
    6     png
    7     pdf
    8     png
    9     pdf
    10    png
    11    pdf
    12    png
    13    pdf
    14    png
    15    pdf
    16    png
    17    pdf
    18    png
    19    pdf
    20    png

``` r
analysis_image("figure_files", "collage_main_maps.png", "Current main-map collage")
```

![Current main-map
collage](../../outputs/figures/main/collage_main_maps.png)

``` r
analysis_image("figure_files", "collage_iv_region_maps.png", "Current IV and region map collage")
```

![Current IV and region map
collage](../../outputs/figures/main/collage_iv_region_maps.png)

``` r
analysis_table(figure_manifest, "Current figure manifest", max_rows = 30)
```

| path | name | format |
|:---|:---|:---|
| outputs/figures/main/fig_ilo_trends.pdf | fig_ilo_trends | pdf |
| outputs/figures/main/fig_ilo_trends.png | fig_ilo_trends | png |
| outputs/figures/main/district_carveouts_shifts.pdf | district_carveouts_shifts | pdf |
| outputs/figures/main/district_carveouts_shifts.png | district_carveouts_shifts | png |
| outputs/figures/main/map_emi_exposure.pdf | map_emi_exposure | pdf |
| outputs/figures/main/map_emi_exposure.png | map_emi_exposure | png |
| outputs/figures/main/map_consumption_growth.pdf | map_consumption_growth | pdf |
| outputs/figures/main/map_consumption_growth.png | map_consumption_growth | png |
| outputs/figures/main/map_pucca.pdf | map_pucca | pdf |
| outputs/figures/main/map_pucca.png | map_pucca | png |
| outputs/figures/main/map_education.pdf | map_education | pdf |
| outputs/figures/main/map_education.png | map_education | png |
| outputs/figures/main/map_region.pdf | map_region | pdf |
| outputs/figures/main/map_region.png | map_region | png |
| outputs/figures/main/map_linguistic_distance.pdf | map_linguistic_distance | pdf |
| outputs/figures/main/map_linguistic_distance.png | map_linguistic_distance | png |
| outputs/figures/main/collage_main_maps.pdf | collage_main_maps | pdf |
| outputs/figures/main/collage_main_maps.png | collage_main_maps | png |
| outputs/figures/main/collage_iv_region_maps.pdf | collage_iv_region_maps | pdf |
| outputs/figures/main/collage_iv_region_maps.png | collage_iv_region_maps | png |

Current figure manifest
