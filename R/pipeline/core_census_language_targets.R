# Publication Census language-behavior target declarations.
#
# The main paper reports the C-17 language-acquisition evidence directly, so the
# minimal acquisition/harmonization/mechanism chain is a core publication
# dependency. Broader Census population, migration, and age diagnostics remain
# in extended_census_target_definitions().

core_census_language_target_definitions <- function() {
  list(
    tar_target(
      census_2001_download_manifest_file,
      path_metadata(paths, "census_2001_download_manifest.tsv"),
      format = "file"
    ),
    tar_target(
      census_2001_c17_files,
      census_c17_manifest_files(paths, census_2001_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2001_c16_state_language_totals,
      census_2001_state_language_totals(raw_census_2001)
    ),
    tar_target(
      census_2001_c17_state_languages,
      read_census_c17_state_languages(
        census_2001_c17_files, census_2001_c16_state_language_totals
      )
    ),
    tar_target(
      census_2001_c17_mechanism,
      diagnose_census_c17_mechanism(
        census_2001_c17_state_languages,
        glottolog_5_3,
        census_glottolog_crosswalk,
        historical_linguistic_sources,
        shastry_concordance = shastry_language_distance,
        lexical_index = lexical_language_index
      )
    )
  )
}
