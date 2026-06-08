# Replication data contract

Raw data are intentionally not tracked in this repository. The active pipeline reads `data/metadata/file_manifest.csv` before attempting to load raw data. If a required file is missing, the pipeline should fail with a manifest-based message listing the exact missing path.

See `DATA_AVAILABILITY.md` for the source-by-source availability table, redistribution notes, expected local paths, and reconstruction targets.

## Required local files

Place required current-pipeline files under the canonical `data/raw/` source directories listed in `data/metadata/file_manifest.csv`, and place static image assets under `assets/`.

If your local copy still uses the older long raw-folder names from the pre-refactor project layout, first run a dry run:

```bash
Rscript scripts/rename_raw_data_from_manifest.R
```

Review the printed plan and `outputs/diagnostics/raw_data_rename_plan.csv`. To move local raw directories after review, run the same script with `--execute`. The script refuses to overwrite existing canonical directories and does not delete raw data.

The active manifest currently covers:

- NSS 2007-08 Participation and Expenditure in Education, 64th Round;
- NSS 2007-08 Household Consumer Expenditure Survey, 64th Round;
- NSS 2017-18 Household Social Consumption: Education, 75th Round;
- Census of India 2001 C-16 mother-tongue files, `PC01_C16_01.xls` through `PC01_C16_35.xls`;
- District Boundaries 2020 shapefile components;
- district-change tracker and validation sources;
- static ILO image assets used in the paper.

The canonical raw source directories are:

- `data/raw/nss_2007_education_64/`
- `data/raw/nss_2007_consumption_64/`
- `data/raw/nss_2017_education_75/`
- `data/raw/census_2001_mother_tongue/`
- `data/raw/district_boundaries_2020/`
- `data/raw/district_changes/`

## System dependencies

The R package dependencies are declared in `DESCRIPTION`, and `renv.lock` records exact package versions. Spatial packages such as `sf` and `spdep` may also require GDAL, GEOS, PROJ, and `pkg-config` system libraries. On macOS, these are commonly available through Homebrew as `gdal`, `geos`, `proj`, and `pkg-config`.

The setup script checks for `gdal-config`, `geos-config`, and `pkg-config` and prints this reminder if they are missing. Existing local installations may still work if binary R packages are already installed, but a fresh machine may need these system libraries before `make init-renv` can install spatial dependencies.

## Public processed outputs

The only processed data products intended to be tracked at this stage are:

- `data/processed/district_tracker_2001_2007_2017_2020.csv`
- `data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv`

Checksums for tracked metadata and processed CSV files are recorded in `data/metadata/checksums.csv`. Refresh them with:

```bash
Rscript scripts/update_checksums.R
```

## Expected behavior without raw data

`make pipeline-draft` may stop early on a fresh clone without raw data. That is acceptable only if the error clearly names `data/metadata/file_manifest.csv` and lists the missing files. Cryptic path errors from `read_sav()`, `read_excel()`, `sf::st_read()`, or similar readers should be treated as bugs.

## Commands

```bash
make init-renv
make test
make pipeline-draft
make report
make samples
make check-public-draft
```

`make test` should pass without local raw data. The full pipeline requires the local-only raw files listed in the manifest.

`make check-public-draft` is the current public-render smoke check. It tolerates explicitly deferred geometry/map work but still fails on scaffold prose, broken application-sample specs, render failures, and rendered placeholder phrases. `make check-public-final` uses `config/final.yml`, audits all legacy inline report quantities, audits final output artifacts, renders application samples, checks PDF text when `pdftotext` or `pdftools` is available, and fails on visible public-document cross-reference artifacts or incomplete report values/cross-references. `make check-public-final-no-samples` runs the same final checks but skips application-sample targets, rendering, text checks, and output requirements.

For the scripted audit, `bash scripts/run_public_build_audit.sh` defaults to the faster no-samples mode and writes `review.zip` without `application-samples/output/`. Use `bash scripts/run_public_build_audit.sh --with-samples` before a full submission/review bundle; that mode renders the application samples and requires their PDFs in `review.zip`.

## Review archive contract

`scripts/make_review_archive.sh` is intentionally not a substitute for the final public checks. It writes `review.zip` by default and refuses to package the repository unless `.public-final-ok` exists, which is written only after a final public check completes successfully. By default the archive script includes application-sample PDFs; pass `--without-samples` only when the audit intentionally skipped rendering them and the archive should omit `application-samples/output/` rather than risk packaging stale sample PDFs. This prevents a review archive from mixing regenerated source files with stale PDFs or outputs from an earlier run.
