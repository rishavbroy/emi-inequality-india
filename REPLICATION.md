# Replication data contract

Raw data are intentionally not tracked in this repository. The active pipeline reads [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv) before attempting to load raw data. If a required file is missing, the pipeline should fail with a manifest-based message listing the exact missing path.

See [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md) for the source-by-source availability table, redistribution notes, expected local paths, and reconstruction targets.

## Required local files

Place required current-pipeline files under the canonical `data/raw/` source directories listed in [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv), and place static image assets under [`assets/`](assets/).

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

## Optional district-lineage v2 inputs

The extended-diagnostics audit discovers the local LGD, SHRUG, Census-locality, boundary, literature-derived, and published-concordance files described by `district_lineage_v2_input_specs()`. They are intentionally not required for the existing production report and are not part of the main raw-file manifest. Their source catalog and acquisition routes are recorded in [`data/metadata/data_sources.csv`](data/metadata/data_sources.csv); the full methodology and work plan are in [`docs/DISTRICT_LINEAGE_V2.md`](docs/DISTRICT_LINEAGE_V2.md).

When present, `--with-extended-diagnostics` writes a reviewable parallel rebuild under `outputs/diagnostics/extended/district_lineage_v2/`. Current LGD registries, compact modification rosters, SHRUG keys, Census-2011 district geometry, candidate trackers, and tracked adjudication ledgers are loaded for review. Large village/Census attribute tables, post-2018 LGD change history, and SHRID/village polygon archives are inventoried but are not loaded during every audit. A later dedicated geography target should dissolve local SHRID polygons into a compact, validated 2001-district GeoPackage.

The v2 system must remain parallel until source identities, administrative events, exclusions, and any sensitivity weights are adjudicated. Exact or fuzzy name matches are review candidates, not production geography.

## System dependencies

The R package dependencies are declared in [`DESCRIPTION`](DESCRIPTION), and [`renv.lock`](renv.lock) records exact package versions. Spatial packages such as `sf` and `spdep` may also require GDAL, GEOS, PROJ, and `pkg-config` system libraries. On macOS, these are commonly available through Homebrew as `gdal`, `geos`, `proj`, and `pkg-config`.

The setup script checks for `gdal-config`, `geos-config`, and `pkg-config` and prints this reminder if they are missing. Existing local installations may still work if binary R packages are already installed, but a fresh machine may need these system libraries before `make init-renv` can install spatial dependencies.

## Public processed outputs

The only public district data products intended to be tracked at this stage are:

- [`data/metadata/district_harmonization_crosswalk.csv`](data/metadata/district_harmonization_crosswalk.csv), the single tracked district harmonization authority
- [`data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv`](data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv)

Checksums for tracked metadata and processed CSV files are recorded in [`data/metadata/checksums.csv`](data/metadata/checksums.csv). Refresh them with:

```bash
Rscript scripts/update_checksums.R
```

## Expected behavior without raw data

`make pipeline-draft` may stop early on a fresh clone without raw data. That is acceptable only if the error clearly names [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv) and lists the missing files. Cryptic path errors from `read_sav()`, `read_excel()`, `sf::st_read()`, or similar readers should be treated as bugs.

## Commands

The recommended replication entry point is the [scripted public-build audit](scripts/run_public_build_audit.sh), because it checks the current public QMD sources, runs tests, executes the final public checks, checks report values, and packages a review archive. Lower-level [`Makefile`](Makefile) targets remain useful for development, but they are not a substitute for the audit script before sharing a bundle.

```bash
# Fast contract tests; should pass without local raw data.
make test

# Full reviewer-facing archive with a log and a debug archive if anything fails.
bash scripts/run_public_build_audit.sh --with-samples --archive-on-error 2>&1 | tee full_output.txt

# Faster cache-preserving iteration without application samples.
bash scripts/run_public_build_audit.sh --without-samples --incremental --archive-on-error 2>&1 | tee full_output.txt

# Full optional diagnostics/benchmarking run for methodological review.
bash scripts/run_public_build_audit.sh --with-samples --incremental --archive-on-error --with-extended-diagnostics --with-benchmarks 2>&1 | tee full_output_with_diagnostics_benchmarks.txt
```

`make test` should pass without local raw data. The full pipeline requires the local-only raw files listed in the manifest. [`bash scripts/run_public_build_audit.sh`](scripts/run_public_build_audit.sh) defaults to the faster no-samples mode and writes `review.zip` without [`application-samples/output/`](application-samples/output/); pass `--with-samples` before a full submission/review bundle so the application samples are rendered and required in the archive. Commit intentional regenerated outputs after a proof run; the audit does not require a clean working tree because public PDFs and sample PDFs are tracked deliverables that may be regenerated.

Useful lower-level Makefile targets are:

```bash
make init-renv
make pipeline-draft
make report
make samples
make check-public-draft
make check-public-final
make check-public-final-no-samples
```

`make check-public-draft` is the public-render smoke check. It tolerates explicitly deferred geometry/map work but still fails on scaffold prose, broken application-sample specs, render failures, and rendered placeholder phrases. `make check-public-final` uses [`config/final.yml`](config/final.yml), checks all current report quantities, audits final output artifacts, relies on cached `{targets}` render targets for the report, docs, and application samples, checks PDF text when `pdftotext` or `pdftools` is available, and fails on visible public-document cross-reference artifacts or incomplete report values/cross-references. `make check-public-final-no-samples` runs the same final checks but omits application-sample targets, text checks, and output requirements.

On Windows, run the same audit commands through WSL or Git Bash. From PowerShell, replace `tee` with `Tee-Object -FilePath full_output.txt`; from `cmd.exe`, redirect with `> full_output.txt 2>&1`.

## Review archive contract

[`scripts/make_review_archive.sh`](scripts/make_review_archive.sh) is intentionally not a substitute for the final public checks. It writes `review.zip` by default and refuses to package the repository unless `.public-final-ok` exists, which is written only after a final public check completes successfully. When [`scripts/run_public_build_audit.sh`](scripts/run_public_build_audit.sh) `--archive-on-error` fails, it intentionally calls the archive script in `--allow-incomplete` mode to produce a debug archive; that archive is for diagnosis and LLM-assisted debugging, not for reviewer submission. By default the archive script includes application-sample PDFs; pass `--without-samples` only when the audit intentionally skipped rendering them and the archive should omit [`application-samples/output/`](application-samples/output/) rather than risk packaging stale sample PDFs. This prevents a review archive from mixing regenerated source files with stale PDFs or outputs from an earlier run.

### Lineage-source execution

Extended district-lineage diagnostics track and cache each loaded raw source independently. Large LGD SpreadsheetML changed-unit rosters are streamed into their canonical columns, and SHRUG key readers retain only columns needed by the bridge. Inventory-only geometry and locality-attribute archives remain visible in the source inventory without being loaded into the general lineage bundle. Incremental audits therefore reread only sources whose specification, reader, or file changed; village changed-unit coverage remains included.
