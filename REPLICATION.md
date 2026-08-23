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
- Census of India 2001 C-01, C-08, C-14, and H-09 state/UT control-table directories;
- District Boundaries 2020 shapefile components;
- district-change tracker and validation sources;
- static ILO image assets used in the paper.

The canonical raw source directories are:

- `data/raw/nss_2007_education_64/`
- `data/raw/nss_2007_consumption_64/`
- `data/raw/nss_2017_education_75/`
- `data/raw/census_2001/languages/C16/`
- `data/raw/census_2001/religion/C01/`
- `data/raw/census_2001/education/C08/`
- `data/raw/census_2001/age/C14/`
- `data/raw/census_2001/age/C13/` (planned elementary-age exposure)
- `data/raw/census_2011/age/C13/` (planned elementary-age exposure)
- `data/raw/census_2001/housing/H09/`
- `data/raw/district_boundaries_2020/`
- `data/raw/district_changes/`

Census 2001 and 2011 state/UT workbooks in the tracked acquisition manifests can be restored without redownloading nonempty files already present:

```bash
make download-census-tables
```

This runs `bash scripts/download_census_tables.sh`, which processes both `data/metadata/census_2001_download_manifest.tsv` and `data/metadata/census_2011_download_manifest.tsv` by default. The downloader creates missing destination directories, contacts Census of India only for missing or empty files, and writes through a temporary `.part` file before the final rename. A specific manifest can still be supplied explicitly as a script argument.

The 2001 C-13 workbooks are stored under `data/raw/census_2001/age/C13/`; the 2011 C-13 workbooks are stored under `data/raw/census_2011/age/C13/`. These acquisition manifests are intentionally broader than the production `data/metadata/file_manifest.csv`: downloading a planned table does not make it a current build dependency.

## Optional district-lineage inputs

The production lineage reads its tracked source registry and reviewed metadata directly. Extended diagnostics additionally discover local LGD, SHRUG, Census-locality, historical boundary, literature-derived, and published-concordance files. Their source catalog and acquisition routes are recorded in [`data/metadata/data_sources.csv`](data/metadata/data_sources.csv); the final methodology and bounded follow-up work are documented in [`docs/DISTRICT_LINEAGE.md`](docs/DISTRICT_LINEAGE.md).

The audit builds or validates the compact Census-2001 GeoPackage from the code-keyed DataMeet Census-2001 district shapefile before the public pipeline. With `--with-extended-diagnostics`, it then writes lineage diagnostics under `outputs/diagnostics/extended/district_lineage/`, including conservative, primary, full-reviewed, and legacy-comparison artifacts. Current LGD registries, compact modification rosters, SHRUG keys, Census-2011 district geometry, candidate trackers, and tracked adjudication ledgers support those diagnostics. Large village/Census attribute tables, post-2018 LGD change history, and SHRID/village polygon archives remain inventoried rather than loaded during every audit. SHRUG geometry continues to support lineage validation but is not used to draw public maps.

The 573-district reviewed primary panel is the production geography. Exact or fuzzy name matches remain review candidates unless they are recorded in tracked adjudication or review metadata; the 408-district conservative and 587-district full-reviewed panels remain explicit robustness specifications.

## System dependencies

The R package dependencies are declared in [`DESCRIPTION`](DESCRIPTION), and [`renv.lock`](renv.lock) records exact package versions. Spatial packages such as `sf` and `spdep` may also require GDAL, GEOS, PROJ, and `pkg-config` system libraries. PDF text checks use Poppler's `pdftotext` executable. The conference poster additionally requires Quarto 1.9.18 or newer for its Typst custom format. The format follows Quarto's standard two-part template structure: `typst-template.typ` defines the poster function and `typst-show.typ` passes document metadata and body content to it. On macOS, these are commonly available through Homebrew as `gdal`, `geos`, `proj`, `pkg-config`, and `poppler`.
The `testthat` development dependency in the project's `Suggests` field is intentionally locked through `snapshot.dev = true`. Recursive package installation uses renv's standard strong dependency fields (`Imports`, `Depends`, and `LinkingTo`) rather than installing the optional `Suggests` of every dependency. To avoid repeating the same synchronization scan in every child R process, [`.Rprofile`](.Rprofile) disables renv's activation-time check; [`scripts/check_source_syntax.sh`](scripts/check_source_syntax.sh) runs one explicit `renv::status(dev = TRUE)` gate instead.

On a new machine, restore the project library from the tracked lockfile with `make restore`. This delegates directly to `renv::restore()` and does not modify `renv.lock`. If renv must build packages from source, the corresponding system requirements must already be installed. In this lockfile, `s2` declares CMake and OpenSSL requirements, while the spatial stack may also require GDAL, GEOS, PROJ, and `pkg-config`. On macOS, use the standard CRAN R toolchain for the installed R version; Homebrew provides `cmake` and `pkgconf` (which supplies `pkg-config`).

Project paths are relative to the repository root by default. Moving or renaming the project directory should therefore not require edits to tracked configuration or deletion of the `{targets}` store. Functions that are explicitly given another root still resolve paths under that root, which keeps temporary-directory tests and external callers predictable.

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
make restore
make pipeline-draft
make report
make samples
Rscript scripts/run_targets_checked.R poster
make check-public-draft
make check-public-final
make check-public-final-no-samples
```

`make check-public-draft` is the public-render smoke check. It tolerates explicitly deferred geometry/map work but still fails on scaffold prose, broken application-sample specs, render failures, and rendered placeholder phrases. `make check-public-final` uses [`config/final.yml`](config/final.yml), checks all current report quantities, audits final output artifacts, relies on cached `{targets}` render targets for the report, conference poster, docs, and application samples, checks PDF text when the Poppler `pdftotext` executable is available, and fails on visible public-document cross-reference artifacts or incomplete report values/cross-references. `make check-public-final-no-samples` runs the same final checks but omits application-sample targets, text checks, and output requirements.

On Windows, run the same audit commands through WSL or Git Bash. From PowerShell, replace `tee` with `Tee-Object -FilePath full_output.txt`; from `cmd.exe`, redirect with `> full_output.txt 2>&1`.

## Review archive contract

[`scripts/make_review_archive.sh`](scripts/make_review_archive.sh) is intentionally not a substitute for the final public checks. It writes `review.zip` by default and refuses to package the repository unless `.public-final-ok` exists, which is written only after a final public check completes successfully. When [`scripts/run_public_build_audit.sh`](scripts/run_public_build_audit.sh) `--archive-on-error` fails, it intentionally calls the archive script in `--allow-incomplete` mode to produce a debug archive; that archive is for diagnosis and LLM-assisted debugging, not for reviewer submission. By default the archive script includes application-sample PDFs; pass `--without-samples` only when the audit intentionally skipped rendering them and the archive should omit [`application-samples/output/`](application-samples/output/) rather than risk packaging stale sample PDFs. This prevents a review archive from mixing regenerated source files with stale PDFs or outputs from an earlier run.

### Lineage-source execution

Extended district-lineage diagnostics track and cache each loaded raw source independently. Large LGD SpreadsheetML changed-unit rosters are streamed into their canonical columns, and SHRUG key readers retain only columns needed by the bridge. Inventory-only geometry and locality-attribute archives remain visible in the source inventory without being loaded into the general lineage bundle. Incremental audits therefore reread only sources whose specification, reader, or file changed; village changed-unit coverage remains included.


The project disables renv's automatic synchronization message at activation and runs one explicit development-aware `renv::status(dev = TRUE)` check during `scripts/check_source_syntax.sh`. This avoids repeating the same warning in every child R process while preserving a failing audit gate for genuine library/lockfile drift.

The poster extension follows Quarto's documented two-part Typst format. `typst-show.typ` forwards metadata and the rendered document body to the poster function, while `typst-template.typ` contains the upstream `typst-poster` layout directly so project images resolve from the generated document without a separate package root. Content remains in `poster.qmd`; reusable presentation settings such as colors, text sizes, header offsets, and logo/title proportions are format metadata forwarded through `typst-show.typ`. The template retains the earlier Wisconsin poster layout for margins, footer, section bands, typography, and column spacing. This keeps routine visual changes out of the template implementation. Poster content flows through the template's columns automatically; the source does not force column breaks, because a break issued from the final column starts a new page. Pandoc resolves citations in the text, while `suppress-bibliography: true` keeps the full reference list in the linked paper rather than duplicating it on the poster. During poster drafting, `render_poster_pdf()` requires only a successful Quarto render and a non-empty PDF. Page-count and section-presence gates are intentionally deferred until the poster content is frozen, so normal drafting changes can overflow or rename sections without failing the full project build.
