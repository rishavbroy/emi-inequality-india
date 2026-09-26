# Replication

This document describes the supported ways to reproduce the analysis. For source-by-source access, redistribution, and acquisition details, see [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md). For build flags and maintainer commands, see [`docs/BUILD.md`](docs/BUILD.md).

## Replication paths

The repository supports two replication paths.

### Processed-data replication

Use this path to rerun the district-level paper analyses from tracked processed inputs without the restricted individual-level survey files.

```bash
make restore
make replicate-processed
```

The processed graph uses [`_targets_processed.R`](_targets_processed.R) and its own `_targets_processed/` store. It reads:

- [`data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv`](data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv);
- [`data/processed/consumption_district_welfare.csv`](data/processed/consumption_district_welfare.csv); and
- the tracked metadata required to reconstruct the registered district-level specifications.

It reruns the consumption-IV dynamics, schooling-to-consumption bridge and conversion analyses, alternative-distance first stages, and first-stage absorption diagnostics. It intentionally excludes the individual-level education-selection model and source-level reconstruction that require untracked raw data.

Results are written under `outputs/replication/processed/`.

If a full-source `_targets/` store is also available, compare the five shared result objects with:

```bash
make verify-processed-replication
```

The comparison is numerical and component-wise; it writes `outputs/replication/processed/verification.csv` and fails if a shared result differs beyond the configured numerical tolerance.

### Full source reconstruction

Use this path when the required local inputs are available:

```bash
make all
```

`make all` runs the standard verified build with [`config/final.yml`](config/final.yml). The build restores the locked R environment, prepares automatically retrievable inputs, checks source syntax and metadata, runs the test suite, constructs required analysis targets, renders the paper and application samples, performs final checks, verifies the processed-data replication, and writes `review.zip` unless archive creation is disabled.

A fresh clone starts without a `{targets}` store. Ordinary runs keep the store so `{targets}` can reuse computations whose inputs and commands are still current. Use a clean-slate run only when deliberately testing reconstruction from generated-state zero.

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

Raw research data are intentionally not committed to this repository. [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv) is the active local-file registry used by source preflight. Missing required inputs fail before downstream readers run and report the expected path.

### Automatically retrievable inputs

Prepare the sources with maintained downloaders using:

```bash
make prepare-data
```

This currently runs the Census-workbook and Natural Earth download steps. Existing nonempty Census workbooks are retained rather than redownloaded.

To refresh only the Census workbook families registered in the acquisition manifests:

```bash
make download-census-tables
```

### Restricted or local inputs

Inputs that cannot be redistributed or downloaded automatically must be supplied under the paths registered in [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv). The authoritative source-by-source inventory, redistribution status, and acquisition notes are in [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md).

Do not infer a required local path from prose when it disagrees with the manifest; the tracked manifest is the execution authority.

### Materialized proprietary containers

Some NSS/PLFS releases are stored locally in Nesstar containers and use the reviewed conversion contracts in [`data/metadata/nesstar_conversion_contracts.csv`](data/metadata/nesstar_conversion_contracts.csv). Materialization is explicit rather than an implicit reader fallback. For example:

```bash
python3 scripts/materialize_nesstar.py nss66_eus
python3 scripts/materialize_nesstar.py plfs_2017_18
```

The generated conversion directories are local intermediates and are not tracked as raw data. See [`docs/LABOR_MARKET.md`](docs/LABOR_MARKET.md) for the survey-specific source and weighting contracts.

## Build configurations

The supported scientific configurations are:

- [`config/final.yml`](config/final.yml) for paper/release work; and
- [`config/fast.yml`](config/fast.yml) for faster code iteration while retaining the registered estimands and variable definitions.

Optional diagnostics, benchmarks, application samples, and poster rendering are build-family choices rather than separate scientific configurations. See [`docs/BUILD.md`](docs/BUILD.md) for the command and flag reference.

## Missing-data behavior

The full source graph is fail-closed for required registered inputs. `make prepare-data` may recover sources covered by the maintained download manifests; other absent required files stop during source validation.

The processed-data path is the supported raw-data-light alternative. It does not claim to reproduce analyses whose required microdata are not redistributed, including the individual-level education-selection model and source-level reconstruction/validation families.

Optional extended diagnostics may have additional local inputs. Their availability does not redefine the required core inputs unless a paper-facing target depends on them.

## Verification

Use the following checks according to scope:

```bash
make test
make check-public-final
make verify-processed-replication
```

`make test` runs the complete `testthat` suite. `make check-public-final` runs the final paper/sample target build and its publication checks without the extended diagnostics or benchmarks. `make all` is the normal integrated release audit and also performs processed-data verification after a successful final build.

The full build additionally writes machine-readable build and output metadata under `outputs/build/`, including target warnings, target metadata, and the final output manifest.

## Review archive

[`scripts/make_review_archive.sh`](scripts/make_review_archive.sh) packages tracked source material and selected review outputs while excluding raw data, dependency libraries, target stores, and local caches. The ordinary full build writes `review.zip` on success and also packages the current failed state when possible.

For an immediate snapshot after an interrupted or incomplete run:

```bash
scripts/make_review_archive.sh --without-samples --allow-incomplete --output review.zip
```

New source files should be staged or committed before relying on the archive to include them.

## Troubleshooting

### A previous `{targets}` process is still recorded

The full build checks recorded `{targets}` process metadata before starting the analysis. Dead metadata is cleared automatically. If a live process is detected, the build stops and prints its PID rather than terminating it automatically. Inspect that process and terminate it only if it is an abandoned run.

### Package restoration fails

Install the system requirements for the package that failed, then rerun `make restore`. The lockfile determines the requested R package records; system libraries remain an operating-system prerequisite.

### A required input is missing

Read the reported path, then consult [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv) and [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md). Do not add reader-side path fallbacks for a missing registered file.

### A full rebuild is desired

```bash
scripts/run_full_build.sh --from-clean-slate
```

This deletes generated outputs and the target stores before reconstruction. It is an exceptional verification step, not the ordinary development mode.

## Related documentation

- [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md) — access, redistribution, acquisition, and expected local paths.
- [`docs/BUILD.md`](docs/BUILD.md) — build configurations, commands, optional families, and validation sequence.
- [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) — module boundaries, target composition, registries, and output conventions.
- [`data/metadata/README.md`](data/metadata/README.md) — metadata file roles and editing rules.
- [`docs/DISTRICT_LINEAGE.md`](docs/DISTRICT_LINEAGE.md) — district identity and lineage methodology.
- [`docs/LABOR_MARKET.md`](docs/LABOR_MARKET.md) — NSS/PLFS source materialization and survey design.
- [`archive/refactoring/docs/replication-data-contract-before-documentation-rewrite.md`](archive/refactoring/docs/replication-data-contract-before-documentation-rewrite.md) — historical pre-rewrite replication/status notes retained for reference; it is not an active execution guide.
