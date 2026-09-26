# Inequality in a Potential Equalizer of Opportunity

This repository contains the code, outputs, and replication materials for *Inequality in a Potential Equalizer of Opportunity: Variation in the Accessibility and Net Benefits of English-Medium Instruction in India*. The project studies how inherited linguistic conditions, access to English-medium instruction (EMI), and local economic conditions vary across Indian districts and relate to inequality.

[Paper](paper/paper.pdf) · [Replication guide](REPLICATION.md) · [Data availability](DATA_AVAILABILITY.md) · [Application samples](application-samples/README.md)

The current paper is available at <https://rishavbroy.github.io/emi-inequality-india/paper.pdf>.

## Research overview

2001 Census districts are the principal geographic unit. I combine them with NSS surveys on education and consumer expenditure, Census tables on linguistic and socioeconomic variables, DISE/UDISE schooling censuses, and HCES data. A reviewed cross-vintage harmonization reconstructs district identities across changing boundaries, while separate spatial and temporal price adjustments place consumption measures on a common basis.

The empirical work studies inherited linguistic conditions, access to EMI, and local economic conditions associated with the potential gains from EMI. It combines district descriptions and survey-weighted models with selection analysis and instrumental-variable estimates, including inference that remains valid under weak identification. Historical data and reconstructed district lineages provide validation and robustness checks. The paper is the primary guide to the economic argument and interpretation; the documentation linked below describes the implementation.

## Start here

**To read the research:** open [`paper/paper.pdf`](paper/paper.pdf) or the [browser-hosted paper](https://rishavbroy.github.io/emi-inequality-india/paper.pdf).

**To reproduce the analysis:** start with [`REPLICATION.md`](REPLICATION.md) and [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md). The former gives the supported replication paths and system requirements; the latter records access and redistribution information for each data source.

**To inspect the code:** start with [`_targets.R`](_targets.R), [`R/`](R/), and [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md). The target script defines the main research computation, while the architecture guide explains the module boundaries and shared analysis registries.

**To review application materials:** see [`application-samples/output/`](application-samples/output/) for the generated PDFs and [`application-samples/README.md`](application-samples/README.md) for how they are selected and rendered.

## Reproducing the project

There are two supported replication paths.

### Processed-data replication

The tracked processed inputs reproduce the district consumption results, the relationship between schooling and later consumption, the linguistic-distance first stages, and the weak-IV analyses.

```sh
make restore
make replicate-processed
```

If a full analysis `_targets/` store is also available, compare the shared reported results from the processed replication using:

```sh
make verify-processed-replication
```

The processed replication uses `_targets_processed.R` and an independent `_targets_processed/` store. See [`REPLICATION.md`](REPLICATION.md) for its exact scope.

### Full reconstruction

The ordinary complete build is:

```sh
make all
```

`make all` uses `config/final.yml`, restores the project library from `renv.lock`, prepares automatically retrievable inputs, and runs the research build. It then renders the paper and application samples, performs the final checks, compares the processed replication with the full analysis, and writes `review.zip`. Inputs that cannot be redistributed or downloaded automatically must already be present at the paths documented in [`REPLICATION.md`](REPLICATION.md) and [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv).

A fresh clone has no `{targets}` store. Ordinary builds keep the store and let `{targets}` rerun only computations that are out of date. To verify reconstruction from scratch, delete generated state with the dedicated option documented in [`docs/BUILD.md`](docs/BUILD.md).

## Common commands

| Command | Purpose |
|---|---|
| `make restore` | Restore the R project library from `renv.lock` |
| `make test` | Run the unit tests |
| `make pipeline` | Run the final research analysis without application samples or the poster |
| `make pipeline-fast` | Run the faster development configuration |
| `make paper` | Build and validate the paper without application samples |
| `make samples` | Build the named and anonymous application samples |
| `make replicate-processed` | Reproduce the supported district-level analyses from tracked processed inputs |
| `make verify-processed-replication` | Compare shared results from the processed replication with an existing full analysis |
| `make all` | Run the ordinary complete build |

Additional validation checks, benchmarks, poster rendering, reconstruction from scratch, and maintainer commands are documented in [`docs/BUILD.md`](docs/BUILD.md).

## Data requirements

Some data are tracked in processed form, some can be downloaded automatically, and restricted or uncertain-to-redistribute inputs must be supplied locally.

`make prepare-data` downloads the Census workbooks covered by the tracked acquisition manifests and the Natural Earth boundary data used for manuscript cartography. Other required inputs must be placed at the paths declared in [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv). The build validates required inputs before downstream readers run and reports missing required paths explicitly.

See [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md) for access and redistribution notes by data source and [`REPLICATION.md`](REPLICATION.md) for local file requirements, system dependencies, and expected behavior when raw inputs are absent.

## Repository structure

```text
R/                    Research functions
data/                 Tracked metadata/processed data; local raw data are gitignored
config/               Final and development research configurations
paper/                Paper source and rendered PDF
application-samples/  Sample definitions, rendering support, and generated PDFs
outputs/              Generated tables, figures, validation results, and replication results
docs/                 Build, architecture, and methodological documentation
scripts/              Build, validation, acquisition, and maintenance entry points
archive/              Frozen historical material
tests/                testthat suite
```

[`_targets.R`](_targets.R) defines the main research dependency structure. [`_targets_processed.R`](_targets_processed.R) defines the separate replication from processed inputs. Local raw-data and working directories such as `data/raw/`, `data/raw_future/`, and `data/interim/` are gitignored.

## Analysis architecture

The implementation separates data input, measure construction, district harmonization, estimation and inference, and research outputs. Shared model specifications are defined once and reused by tables, figures, and validation checks.

See [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) for the module structure, analysis definitions, output conventions, and final-build behavior.

## Main outputs

- [`paper/paper.pdf`](paper/paper.pdf) — current paper.
- [`application-samples/output/`](application-samples/output/) — named and anonymous writing and coding samples.
- [`outputs/tables/main/`](outputs/tables/main/) — principal generated tables used by the paper.
- [`outputs/figures/main/`](outputs/figures/main/) — principal generated figures used by the paper.
- [`outputs/replication/processed/verification.csv`](outputs/replication/processed/verification.csv) — comparison of shared results from the processed replication and full analysis when both are available.

## Application samples

Writing and coding samples are generated directly from the paper and research code used by the main build. [`application-samples/samples.yml`](application-samples/samples.yml) contains the sample definitions, and [`application-samples/output/`](application-samples/output/) contains the generated PDFs.

For a quick review, see the named [`10-page writing sample`](application-samples/output/RishavRoy_WritingSample_10pg.pdf) and [`long code sample`](application-samples/output/RishavRoy_CodeSample_Long.pdf). [`application-samples/README.md`](application-samples/README.md) documents section selection, code markers, anonymity rules, paper numbering, and publication behavior.

## Validation

The ordinary complete build checks source syntax, the `renv` lockfile and project library, unit tests, target execution, report values, cross-references, required rendered outputs, and the processed replication. Additional validation checks and benchmarks can be enabled separately without changing the selected research configuration.

The repository also records build metadata and packages a review archive with the current `analysis/` notebooks while excluding local raw data and caches. See [`docs/BUILD.md`](docs/BUILD.md) for the validation sequence and optional steps.

## Documentation

- [`REPLICATION.md`](REPLICATION.md) — supported replication paths, required local inputs, and system dependencies.
- [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md) — data access, redistribution status, and reconstruction notes.
- [`docs/BUILD.md`](docs/BUILD.md) — build configurations, commands, and optional build families.
- [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) — code organization, registries, target groups, and output conventions.
- [`docs/DISTRICT_LINEAGE.md`](docs/DISTRICT_LINEAGE.md) — district harmonization and reviewed lineage construction.
- [`docs/EDUCATION_SELECTION.md`](docs/EDUCATION_SELECTION.md) — enrollment selection and average marginal effects.
- [`docs/IV_DIAGNOSTICS.md`](docs/IV_DIAGNOSTICS.md) — identification checks and weak-instrument inference.
- [`docs/CONSUMPTION_MEASUREMENT.md`](docs/CONSUMPTION_MEASUREMENT.md), [`docs/PRICE_DEFLATION.md`](docs/PRICE_DEFLATION.md), and [`docs/CONSUMPTION_ANALYSIS.md`](docs/CONSUMPTION_ANALYSIS.md) — consumption construction, prices, and the welfare analysis used in the paper.
- [`docs/LINGUISTIC_DISTANCE.md`](docs/LINGUISTIC_DISTANCE.md) and [`docs/EMI_MEASUREMENT.md`](docs/EMI_MEASUREMENT.md) — core treatment/instrument measurement.
- [`docs/SPATIAL_ANALYSIS.md`](docs/SPATIAL_ANALYSIS.md) — spatial weights and residual spatial-dependence checks.
- [`application-samples/README.md`](application-samples/README.md) — application-sample generation and publication.
