# Inequality in a Potential Equalizer of Opportunity

This repository contains the research code, processed outputs, paper source, and replication materials for *Inequality in a Potential Equalizer of Opportunity: Variation in the Accessibility and Net Benefits of English-Medium Instruction in India*. The project studies how inherited linguistic conditions, access to English-medium instruction (EMI), and local economic conditions vary across Indian districts and relate to inequality.

[Paper](paper/paper.pdf) · [Replication guide](REPLICATION.md) · [Data availability](DATA_AVAILABILITY.md) · [Application samples](application-samples/README.md)

A browser-hosted copy of the current paper is available at <https://rishavbroy.github.io/emi-inequality-india/paper.pdf>.

## Research overview

The analysis uses Census-2001 districts as the principal geographic unit. It combines NSS education and consumer-expenditure surveys, Census language and socioeconomic tables, DISE/UDISE schooling data, modern HCES data, reviewed district-lineage harmonization, and spatial and temporal price adjustments.

The empirical work examines three margins: inherited linguistic conditions, access to EMI, and local economic conditions associated with the potential gains from EMI. The repository contains descriptive analysis, survey-weighted models, selection analysis, instrumental-variable specifications, weak-identification-robust inference, historical validation, district-lineage checks, and robustness analyses. The paper is the primary guide to the economic argument and interpretation; the documentation linked below describes the implementation in more detail.

## Start here

**To read the research:** open [`paper/paper.pdf`](paper/paper.pdf) or the [browser-hosted paper](https://rishavbroy.github.io/emi-inequality-india/paper.pdf).

**To reproduce the analysis:** start with [`REPLICATION.md`](REPLICATION.md) and [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md). The former gives the supported replication paths and system requirements; the latter records source-by-source access and redistribution information.

**To inspect the code:** start with [`_targets.R`](_targets.R), [`R/`](R/), and [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md). The target script defines the main research computation, while the architecture guide explains the module boundaries and analysis-design registries.

**To review application materials:** see [`application-samples/output/`](application-samples/output/) for the generated PDFs and [`application-samples/README.md`](application-samples/README.md) for how they are selected and rendered.

## Reproducing the project

There are two supported replication paths.

### Processed-data replication

The tracked processed inputs can reproduce the district-level consumption, conversion-gradient, linguistic-distance first-stage, and weak-IV analyses without the restricted raw microdata used elsewhere in the project.

```sh
make restore
make replicate-processed
```

If a full-source `_targets/` store is also available, compare the shared reported results from the processed-data build with the full analysis using:

```sh
make verify-processed-replication
```

The processed-data build uses `_targets_processed.R` and its own `_targets_processed/` store, so it does not modify the full analysis store. See [`REPLICATION.md`](REPLICATION.md) for its exact scope and exclusions.

### Full reconstruction

The ordinary complete build is:

```sh
make all
```

`make all` uses `config/final.yml`, restores the project library from `renv.lock`, prepares automatically retrievable inputs, runs the validated research build, renders the paper and application samples, performs final checks, verifies the processed-data replication against the full analysis, and writes `review.zip`. Inputs that cannot be redistributed or downloaded automatically must already be present at the paths documented in [`REPLICATION.md`](REPLICATION.md) and [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv).

A fresh clone has no `{targets}` store. Ordinary builds keep the store and let `{targets}` rerun only computations that are out of date; use the destructive clean-slate option only for an explicit reconstruction check. Advanced build modes and options are documented in [`docs/BUILD.md`](docs/BUILD.md).

## Common commands

| Command | Purpose |
|---|---|
| `make restore` | Restore the R project library from `renv.lock` |
| `make test` | Run the unit tests |
| `make pipeline` | Run the final research targets without application samples or the poster |
| `make pipeline-fast` | Run the faster development configuration |
| `make paper` | Build and validate the paper without application samples |
| `make samples` | Build the named and anonymous application samples |
| `make replicate-processed` | Reproduce the supported district-level analyses from tracked processed inputs |
| `make verify-processed-replication` | Compare shared processed-tier results with an existing full-source build |
| `make all` | Run the ordinary complete build |

Optional diagnostics, benchmarks, poster rendering, clean-slate reconstruction, and other maintainer-oriented commands are documented in [`docs/BUILD.md`](docs/BUILD.md).

## Data requirements

Raw microdata are not committed to the repository. The project distinguishes among tracked processed inputs, inputs that can be downloaded automatically, and local files whose redistribution rights are not asserted here.

`make prepare-data` downloads the Census workbooks covered by the tracked acquisition manifests and the Natural Earth boundary data used for manuscript cartography. Other required inputs must be placed at the paths declared in [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv). The build validates required inputs before downstream readers run and reports missing required paths explicitly.

See [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md) for source-by-source access and redistribution notes and [`REPLICATION.md`](REPLICATION.md) for local file requirements, system dependencies, and expected behavior when raw inputs are absent.

## Repository structure

```text
R/                    Research functions
data/                 Tracked metadata/processed data; local raw data are gitignored
config/               Final and development research configurations
paper/                Paper source and rendered PDF
application-samples/  Sample definitions, rendering support, and generated PDFs
outputs/              Generated tables, figures, diagnostics, and replication results
docs/                 Build, architecture, and methodological documentation
scripts/              Build, validation, acquisition, and maintenance entry points
archive/              Frozen historical material
tests/                testthat suite
```

[`_targets.R`](_targets.R) defines the main research dependency structure. [`_targets_processed.R`](_targets_processed.R) defines the separate processed-data replication. Local raw-data and working directories such as `data/raw/`, `data/raw_future/`, and `data/interim/` are gitignored.

## Analysis architecture

The implementation is organized around input adapters, harmonized measures, district-lineage construction, model estimation and inference, and generated research outputs. Cross-cutting empirical specifications are centralized in analysis-design registries rather than repeated inside individual figure, table, or diagnostic functions.

See [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) for the module structure, target families, analysis-design ontology, output conventions, and strict final-mode behavior.

## Main outputs

- [`paper/paper.pdf`](paper/paper.pdf) — current paper.
- [`application-samples/output/`](application-samples/output/) — named and anonymous writing and coding samples.
- [`outputs/tables/main/`](outputs/tables/main/) — principal generated tables used by the paper.
- [`outputs/figures/main/`](outputs/figures/main/) — principal generated figures used by the paper.
- [`outputs/replication/processed/verification.csv`](outputs/replication/processed/verification.csv) — comparison of the shared processed-data and full-source reported results when both builds are available.

## Application samples

Writing and coding samples are generated from the same paper and research code used by the main build rather than maintained as independent documents. [`application-samples/samples.yml`](application-samples/samples.yml) contains the sample definitions, and [`application-samples/output/`](application-samples/output/) contains the generated PDFs.

For a quick review, see the named [`10-page writing sample`](application-samples/output/RishavRoy_WritingSample_10pg.pdf) and [`long code sample`](application-samples/output/RishavRoy_CodeSample_Long.pdf). [`application-samples/README.md`](application-samples/README.md) documents section selection, code markers, anonymity rules, paper numbering, and publication behavior.

## Validation

The ordinary complete build checks source syntax, the `renv` lockfile and project library, unit tests, target execution, report-value consistency, cross-references, required rendered outputs, and processed-data replication. Optional extended diagnostics and benchmarks can be enabled separately without changing the selected research configuration.

The repository also records machine-readable build metadata and packages a review archive while excluding local raw data and caches. See [`docs/BUILD.md`](docs/BUILD.md) for the exact validation stages and optional build families.

## Documentation

- [`REPLICATION.md`](REPLICATION.md) — supported replication paths, required local inputs, and system dependencies.
- [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md) — data access, redistribution status, and reconstruction notes.
- [`docs/BUILD.md`](docs/BUILD.md) — build configurations, commands, and optional build families.
- [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) — code organization, registries, target groups, and output conventions.
- [`docs/DISTRICT_LINEAGE.md`](docs/DISTRICT_LINEAGE.md) — district harmonization and reviewed lineage construction.
- [`docs/EDUCATION_SELECTION.md`](docs/EDUCATION_SELECTION.md) — enrollment selection and average marginal effects.
- [`docs/IV_DIAGNOSTICS.md`](docs/IV_DIAGNOSTICS.md) — identification and weak-instrument diagnostics.
- [`docs/SPATIAL_ANALYSIS.md`](docs/SPATIAL_ANALYSIS.md) — spatial weights and residual spatial-dependence checks.
- [`application-samples/README.md`](application-samples/README.md) — application-sample generation and publication.
