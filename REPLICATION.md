# Replication data contract

Raw data are intentionally not tracked in this repository. The active pipeline reads `data/metadata/file_manifest.csv` before attempting to load raw data. If a required file is missing, the pipeline should fail with a manifest-based message listing the exact missing path.

## Required local files

Place required current-pipeline files under `data/raw/` and static image assets under `assets/` according to `data/metadata/file_manifest.csv`.

The active manifest currently covers:

- NSS 2007-08 Participation and Expenditure in Education, 64th Round;
- NSS 2007-08 Household Consumer Expenditure Survey, 64th Round;
- NSS 2017-18 Household Social Consumption: Education, 75th Round;
- Census of India 2001 C-16 mother-tongue files, `PC01_C16_01.xls` through `PC01_C16_35.xls`;
- District Boundaries 2020 shapefile components;
- district-change tracker and validation sources;
- static ILO image assets used in the paper.

## System dependencies

The R package dependencies are declared in `DESCRIPTION`, and `renv.lock` records exact package versions. Spatial packages such as `sf` and `spdep` may also require GDAL, GEOS, PROJ, and `pkg-config` system libraries. On macOS, these are commonly available through Homebrew as `gdal`, `geos`, `proj`, and `pkg-config`.

The setup script checks for `gdal-config`, `geos-config`, and `pkg-config` and prints this reminder if they are missing. Existing local installations may still work if binary R packages are already installed, but a fresh machine may need these system libraries before `make init-renv` can install spatial dependencies.

## Public processed outputs

The only processed data products intended to be tracked at this stage are:

- `data/processed/district_tracker_2001_2007_2017_2020.csv`
- `data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv`

## Expected behavior without raw data

`make pipeline-draft` may stop early on a fresh clone without raw data. That is acceptable only if the error clearly names `data/metadata/file_manifest.csv` and lists the missing files. Cryptic path errors from `read_sav()`, `read_excel()`, `sf::st_read()`, or similar readers should be treated as bugs.

## Commands

```bash
make init-renv
make test
make pipeline-draft
make report
make samples
```

`make test` should pass without local raw data. The full pipeline requires the local-only raw files listed in the manifest.
