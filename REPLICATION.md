# Replication

This document describes the supported ways to reproduce the analysis. For access, redistribution, and acquisition details by data source, see [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md). For build flags and maintainer commands, see [`docs/BUILD.md`](docs/BUILD.md).

## Replication paths

The repository supports two replication paths.

### Processed-data replication

Use this path to rerun the district-level paper analyses supported by tracked processed inputs.

```bash
make restore
make replicate-processed
```

The processed replication is defined in [`_targets_processed.R`](_targets_processed.R) and uses its own `_targets_processed/` store. It reads:

- [`data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv`](data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv);
- [`data/processed/consumption_district_welfare.csv`](data/processed/consumption_district_welfare.csv); and
- the tracked metadata required to reconstruct the registered district-level specifications.

It reruns the consumption-IV dynamics, schooling-to-consumption analysis, alternative-distance first stages, and checks of how controls absorb first-stage variation. Analyses that require untracked individual-level or raw-source data remain part of the full reconstruction.

Results are written under `outputs/replication/processed/`.

If a full analysis `_targets/` store is also available, compare the five shared results with:

```bash
make verify-processed-replication
```

The comparison is numerical and component-wise; it writes `outputs/replication/processed/verification.csv` and fails if a shared result differs beyond the configured numerical tolerance.

### Full source reconstruction

Use this path when the required local inputs are available:

```bash
make all
```

`make all` runs the standard verified build with [`config/final.yml`](config/final.yml). The build restores the locked R environment, prepares automatically retrievable inputs, checks source syntax and metadata, runs the test suite, constructs required analysis targets, renders the paper and application samples, performs final checks, verifies the processed replication, and writes `review.zip` unless archive creation is disabled.

A fresh clone starts without a `{targets}` store. Ordinary runs keep the store so `{targets}` can reuse computations whose inputs and commands are still current. Use `--from-clean-slate` when verifying reconstruction after deleting generated state.

## Prerequisites

### R environment

R package dependencies are declared in [`DESCRIPTION`](DESCRIPTION), and exact package records are stored in [`renv.lock`](renv.lock). Restore the project library with:

```bash
make restore
```

This delegates to `renv::restore(prompt = FALSE)`. The full build performs the same restore before its explicit synchronization check.

### External tools

A full build requires the external tools used by the selected render and validation paths:

- Quarto for QMD rendering;
- XeLaTeX/TeX Live for PDF rendering and reference-label extraction;
- Poppler's `pdftotext` for rendered-text checks;
- Python 3 for the maintained extraction/materialization helpers; and
- system libraries required by the installed spatial R packages, commonly GDAL, GEOS, PROJ, `pkg-config`, CMake, and OpenSSL.

Application-sample selection requires Quarto 1.4 or newer. The optional Typst conference-poster format requires Quarto 1.9.18 or newer.

On macOS, the spatial/PDF build tools are commonly available through Homebrew. Use the standard CRAN R toolchain appropriate for the installed R version when packages must be built from source.

## Data setup

Raw research data are intentionally not committed to this repository. [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv) lists the local files expected by the build. Missing required inputs are reported before downstream readers run.

### Automatically retrievable inputs

Prepare the sources with maintained downloaders using:

```bash
make prepare-data
```

This currently runs the Census-workbook and Natural Earth download steps. The downloader reuses existing nonempty Census workbooks.

To refresh only the Census workbook families registered in the acquisition manifests:

```bash
make download-census-tables
```

### Restricted or local inputs

Inputs that cannot be redistributed or downloaded automatically must be supplied under the paths registered in [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv). The inventory, redistribution status, and acquisition notes for each data source are in [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md).

The scripts use the paths recorded in the tracked manifest; those paths take precedence over examples in prose.

### Materialized proprietary containers

Some NSS/PLFS releases are stored locally in Nesstar containers and use the reviewed conversion specifications in [`data/metadata/nesstar_conversion_contracts.csv`](data/metadata/nesstar_conversion_contracts.csv). Convert them explicitly before running the corresponding readers. For example:

```bash
python3 scripts/materialize_nesstar.py nss66_eus
python3 scripts/materialize_nesstar.py plfs_2017_18
```

The generated conversion directories are local intermediates and are not tracked as raw data. See [`docs/LABOR_MARKET.md`](docs/LABOR_MARKET.md) for the survey-specific input and weighting definitions.

## Build configurations

The supported scientific configurations are:

- [`config/final.yml`](config/final.yml) for paper/release work; and
- [`config/fast.yml`](config/fast.yml) for faster code iteration while retaining the registered estimands and variable definitions.

Validation checks, benchmarks, application samples, and poster rendering can be enabled independently of the scientific configuration. See [`docs/BUILD.md`](docs/BUILD.md) for the command and flag reference.

## Missing-data behavior

The full reconstruction stops when a required registered input is missing. `make prepare-data` can recover files covered by the maintained download manifests; other missing required files are reported during input validation.

The processed-input path is the supported replication route for redistributable district-level inputs. Its scope is limited to analyses whose required inputs are tracked in the repository; the individual-level education-selection model and raw-data reconstruction remain in the full build.

Optional validation analyses may require additional local inputs. Required inputs for the paper build remain those declared by the paper's dependencies.

## Verification

Use the following checks according to scope:

```bash
make test
make check-public-final
make verify-processed-replication
```

`make test` runs the complete `testthat` suite. `make check-public-final` runs the final paper/sample build and its publication checks. `make all` is the normal integrated release audit and also verifies the processed replication after a successful final build.

The full build also writes build and output metadata under `outputs/build/`, including target warnings, target metadata, and the final output manifest.

## Review archive

[`scripts/make_review_archive.sh`](scripts/make_review_archive.sh) packages tracked source material, the current `analysis/` notebooks, and selected review outputs while excluding raw data, dependency libraries, target stores, and local caches. The notebooks are included from the working tree even though `analysis/` is intentionally Git-ignored. The ordinary full build writes `review.zip` on success and also packages the current failed state when possible.

For an immediate snapshot after an interrupted or incomplete run:

```bash
scripts/make_review_archive.sh --without-samples --allow-incomplete --output review.zip
```

New source files should be staged or committed before relying on the archive to include them.

## Troubleshooting

### A previous `{targets}` process is still recorded

The full build checks recorded `{targets}` process metadata before starting the analysis. Dead metadata is cleared automatically. If a live process is detected, the build stops and prints its PID. Inspect that process and terminate it only if it is an abandoned run.

### Package restoration fails

Install the system requirements for the package that failed, then rerun `make restore`. The lockfile determines the requested R package records; system libraries remain an operating-system prerequisite.

### A required input is missing

Read the reported path, then consult [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv) and [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md). Use the registered path reported by the build.

### A full rebuild is desired

```bash
scripts/run_full_build.sh --from-clean-slate
```

This deletes generated outputs and the target stores before reconstruction. Use it specifically to verify reconstruction from scratch.

## Related documentation

- [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md) — access, redistribution, acquisition, and expected local paths.
- [`docs/BUILD.md`](docs/BUILD.md) — build configurations, commands, optional families, and validation sequence.
- [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) — module boundaries, target composition, registries, and output conventions.
- [`data/metadata/README.md`](data/metadata/README.md) — metadata file roles and editing rules.
- [`docs/DISTRICT_LINEAGE.md`](docs/DISTRICT_LINEAGE.md) — district identity and lineage methodology.
- [`docs/LABOR_MARKET.md`](docs/LABOR_MARKET.md) — NSS/PLFS source materialization and survey design.
- [`archive/refactoring/docs/replication-data-contract-before-documentation-rewrite.md`](archive/refactoring/docs/replication-data-contract-before-documentation-rewrite.md) — historical pre-rewrite replication/status notes retained for reference; it is not an active execution guide.
