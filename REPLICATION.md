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

- `data/raw/nss/nss_2007_education_64/`
- `data/raw/nss/nss_2007_consumption_64/`
  - Detailed NSS-64 Schedule 1.0 Block-3 household consumption; the canonical welfare pipeline uses `Household Characteristics.sav` and validates its released MPCE against NSS Report 530 before deflation.
- `data/raw/nss/nss_2017_education_75/`
- `data/raw/census_2001/languages/C16/`
- `data/raw/census_2001/religion/C01/`
- `data/raw/census_2001/education/C08/`
- `data/raw/census_2001/age/C14/`
- `data/raw/census_2001/age/C13/` (elementary-age DISE exposure denominator)
- `data/raw/census_2011/age/C13/` (elementary-age DISE exposure denominator)
- `data/raw/census_2001/housing/H09/`
- `data/raw/district_boundaries_2020/`
- `data/raw/district_changes/`

Census 2001 and 2011 state/UT workbooks in the tracked acquisition manifests can be restored without redownloading nonempty files already present:

```bash
make download-census-tables
```

This runs `bash scripts/download_census_tables.sh`, which processes both `data/metadata/census_2001_download_manifest.tsv` and `data/metadata/census_2011_download_manifest.tsv` by default. The downloader creates missing destination directories, contacts Census of India only for missing or empty files, and writes through a temporary `.part` file before the final rename. A specific manifest can still be supplied explicitly as a script argument.

The 2001 acquisition manifest stores H-04 Appendix (`PC01_H04a`) under `data/raw/census_2001/housing/H04A/`. All 35 workbooks have been inspected: the permanent/semi-permanent/temporary/unclassifiable partition and temporary serviceability subpartition close exactly, and all 593 district household totals match Census 2001 H-09. H04A and Census 2011 HL-13 therefore form the active structural-durability longitudinal pair; durability changes remain descriptive rather than expanding the fixed housing weak-IV registry.

The 2001 C-13 workbooks are stored under `data/raw/census_2001/age/C13/`; the 2011 C-13 workbooks are stored under `data/raw/census_2011/age/C13/`. They are required by the extended DISE diagnostics that construct the ages-6-13 administrative exposure denominator. The acquisition manifests remain separate from the production `data/metadata/file_manifest.csv` because the public core pipeline does not require these workbooks unless extended diagnostics are enabled.

## Optional district-lineage inputs

The production lineage reads its tracked source registry and reviewed metadata directly. Extended diagnostics additionally discover local LGD, SHRUG, Census-locality, historical boundary, literature-derived, and published-concordance files. Their source catalog and acquisition routes are recorded in [`data/metadata/data_sources.csv`](data/metadata/data_sources.csv); the final methodology and bounded follow-up work are documented in [`docs/DISTRICT_LINEAGE.md`](docs/DISTRICT_LINEAGE.md).

The audit builds or validates the compact Census-2001 GeoPackage from the code-keyed DataMeet Census-2001 district shapefile before the public pipeline. With `--with-extended-diagnostics`, it then writes lineage diagnostics under `outputs/diagnostics/extended/district_lineage/`, including conservative, primary, full-reviewed, and legacy-comparison artifacts. Current LGD registries, compact modification rosters, SHRUG keys, Census-2011 district geometry, candidate trackers, and tracked adjudication ledgers support those diagnostics. Large village/Census attribute tables, post-2018 LGD change history, and SHRID/village polygon archives remain inventoried rather than loaded during every audit. SHRUG geometry continues to support lineage validation but is not used to draw public maps.

The 573-district reviewed primary panel is the production geography. Exact or fuzzy name matches remain review candidates unless they are recorded in tracked adjudication or review metadata; the 408-district conservative and 587-district full-reviewed panels remain explicit robustness specifications.

## System dependencies

The R package dependencies are declared in [`DESCRIPTION`](DESCRIPTION), and [`renv.lock`](renv.lock) records exact package versions. Spatial packages such as `sf` and `spdep` may also require GDAL, GEOS, PROJ, and `pkg-config` system libraries. PDF text checks use Poppler's `pdftotext` executable. The conference poster additionally requires Quarto 1.9.18 or newer for its Typst custom format. The format follows Quarto's standard two-part template structure: `typst-template.typ` defines the poster function and `typst-show.typ` passes document metadata and body content to it. On macOS, these are commonly available through Homebrew as `gdal`, `geos`, `proj`, `pkg-config`, and `poppler`.
The `testthat` development dependency in the project's `Suggests` field is intentionally locked through `snapshot.dev = true`. Recursive package installation uses renv's standard strong dependency fields (`Imports`, `Depends`, and `LinkingTo`) rather than installing the optional `Suggests` of every dependency. To avoid repeating the same synchronization scan in every child R process, [`.Rprofile`](.Rprofile) disables renv's activation-time check; [`scripts/check_source_syntax.sh`](scripts/check_source_syntax.sh) runs one explicit `renv::status(dev = TRUE)` gate instead.

On a new machine, restore the project library from the tracked lockfile with `make restore`. This delegates directly to `renv::restore()` and does not modify `renv.lock`. The canonical public-build audit also runs this restore step before its explicit `renv::status(dev = TRUE)` synchronization gate, so a newly committed runtime dependency does not cause a guaranteed first audit failure merely because the local project library predates the lockfile. If renv must build packages from source, the corresponding system requirements must already be installed. In this lockfile, `s2` declares CMake and OpenSSL requirements, while the spatial stack may also require GDAL, GEOS, PROJ, and `pkg-config`. On macOS, use the standard CRAN R toolchain for the installed R version; Homebrew provides `cmake` and `pkgconf` (which supplies `pkg-config`).

If an audit is interrupted manually while `{targets}` is running, its callr child can outlive the shell process. The next canonical audit checks the recorded `_targets` process immediately after restoring the project library. Dead/stale process metadata is cleared with `targets::tar_unblock_process()`. A genuinely live process is never terminated automatically: the audit stops early, prints the PID, and shows `kill <PID>` so the user can inspect and terminate only the process they intentionally abandoned.

For an immediate debug snapshot after an interrupted run, `bash scripts/make_review_archive.sh --without-samples --allow-incomplete --output review.zip` copies current working-tree versions of tracked source files together with useful intermediate review artifacts, including `outputs/diagnostics`, `outputs/benchmarking`, `data/processed`, and rendered analysis Markdown. Raw data and local caches remain excluded. Newly created source files should be staged or committed before relying on the archive to carry them; the archive does not recursively sweep arbitrary untracked source files.

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

The public target stages request up to four parallel consumption-domain workers for the expensive design-based distributional welfare statistics. Override this with `EMI_CONSUMPTION_DOMAIN_CORES=1` for serial execution or a smaller value on memory-constrained machines; the R helper clamps the request to detected physical cores and Windows remains serial. Core and distributional welfare targets are separately cached, so `--incremental` avoids recomputing core welfare when only a distributional robustness specification changes.

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


The project disables renv's automatic synchronization message at activation. The canonical audit restores the tracked lockfile once, then `scripts/check_source_syntax.sh` runs one explicit development-aware `renv::status(dev = TRUE)` verification. This avoids repeating the same synchronization scan in every child R process while preserving a failing gate for a restore that did not produce the locked environment.

The poster extension follows Quarto's documented two-part Typst format. `typst-show.typ` forwards metadata and the rendered document body to the poster function, while `typst-template.typ` contains the upstream `typst-poster` layout directly so project images resolve from the generated document without a separate package root. Content remains in `poster.qmd`; reusable presentation settings such as colors, text sizes, header offsets, and logo/title proportions are format metadata forwarded through `typst-show.typ`. The template retains the earlier Wisconsin poster layout for margins, footer, section bands, typography, and column spacing. This keeps routine visual changes out of the template implementation. Poster content flows through the template's columns automatically; the source does not force column breaks, because a break issued from the final column starts a new page. Pandoc resolves citations in the text, while `suppress-bibliography: true` keeps the full reference list in the linked paper rather than duplicating it on the poster. During poster drafting, `render_poster_pdf()` requires only a successful Quarto render and a non-empty PDF. Page-count and section-presence gates are intentionally deferred until the poster content is frozen, so normal drafting changes can overflow or rename sections without failing the full project build.


### NSS 64 labor source validation

Extended diagnostics read the NSS 64 Schedule 10.2 DDI and person-level Block 4/Block 6 sources from `data/raw/nss/`. They validate the common person universe, matching cross-block geography/design fields, source schema, survey-design identifiers, and positive combined survey weights before any district labor outcome is constructed. The labor source then reuses the reviewed `nss_2007_08` district lineage through the published five-digit `SSRDD` identity and accepts only deterministic mappings. Source validation is written to `outputs/diagnostics/extended/labor/nss64_source_validation.csv`; source-district lineage/support is written to `nss64_lineage_support.csv`; pooled Census-2001 target support, including distinct FSUs and Kish effective sample size, is written to `nss64_target_support.csv`.

NSS64 labor diagnostics now include a predeclared design-based district outcome family. The estimator uses reviewed deterministic Census-2001 lineage, the posted combined multiplier, state/sector-nested strata and FSUs, standard principal-plus-subsidiary usual-status classification, and denominator-specific support. `outputs/diagnostics/extended/labor/nss64_district_outcomes.csv` retains all estimable district points and flags preferred support at at least 5 FSUs and Kish effective N at least 100 rather than deleting thin domains.

### NSS66 proprietary microdata

The NSS66 employment/unemployment source is distributed as a `.Nesstar` container with companion DDI XML. Extended diagnostics validate the DDI directly. Before activating NSS66 person-level estimation, convert the container with the maintained `nesstar-converter` package rather than adding a repository-specific binary parser, then inspect the generated F4/F5/F6 tables against the DDI case counts and schema.

The canonical adapter uses the original schedule geography fields (`State_Region` and `District`) rather than catalog-generated `STATE`/`DISTRICT_CODE` helper columns. `State_Region` encodes the two-digit state followed by the one-digit NSS region; the adapter derives state from that field and keeps `District` as the survey district code. This avoids treating redundant catalog foreign-key fields as an undocumented equality constraint.

### Materialize Nesstar sources (NSS66 first)

NSS66 Schedule 10 is the first active analytical source for which the
proprietary `.Nesstar` container is unavoidable. Earlier NSS64 consumption,
education, and labor sources also ship `.Nesstar` files, but production reads
their companion `.sav`/open tables instead. The repository therefore keeps one
generic Nesstar materialization boundary rather than a source-specific binary
parser or a growing family of `convert_<survey>.py` scripts.

`nesstar-converter` requires Python 3.10+ and exposes a standard console CLI.
On Homebrew-managed macOS, the base Python may be marked externally managed
under PEP 668; do not use `--break-system-packages`. The reproducible default is
a project-local virtual environment:

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
python -m pip install 'nesstar-converter==1.0.4'
python scripts/materialize_nesstar.py nss66_eus
```

If NSS66 was already converted with the earlier source-specific script, do not
reconvert solely to update `conversion_manifest.csv`: the audit accepts that
exact legacy sidecar shape, validates its paths/rows/bytes/version against the
current source-keyed contract, and records `manifest_schema = legacy_v1`. Future
materializations write the current versioned sidecar automatically.

`.venv/` is gitignored. A `pipx` or `uv tool` installation is also valid: the
materializer locates the `nesstar-converter` console script on `PATH` and reads
package metadata from the Python environment that owns that executable, rather
than requiring the converter package to be installed into the Python interpreter
running the wrapper.

The command writes `data/interim/nss66_eus/F4.csv`, `F5.csv`, `F6.csv`, and a
local conversion manifest with SHA-256 hashes. `data/interim/` is gitignored;
these are reproducible local intermediates, not tracked source data. Use
`--force` only when intentionally replacing a prior conversion. Source IDs,
converter versions, raw manifest IDs, block signatures, expected row counts,
and output paths are frozen in
`data/metadata/nesstar_conversion_contracts.csv`. Future PLFS or Economic Census
Nesstar sources should be added there only when their `.Nesstar` binary is the
chosen production source and the required companion metadata is available.

### PLFS 2017-18 materialization

The registered PLFS 2017-18 source package now contains the official MoSPI
`.Nesstar` binary, fixed-width layout workbook, and catalog DDI/XML. The extended
audit validates all three byte contracts, checks the reviewed layout byte
positions, and validates the DDI first-visit person file (`F1`,
`hh_per_fv_2017-18`, 433,339 cases) against the analytical field registry.

Materialize only the annual first-visit person file through the same generic
converter used for NSS66:

```bash
source .venv/bin/activate
python scripts/materialize_nesstar.py plfs_2017_18
```

This writes `data/interim/plfs_2017_18/F1.csv` plus a versioned local conversion
manifest. The conversion contract deliberately selects only `F1`: annual
usual-status outcomes use the first-visit person universe, while PLFS revisit
records belong to a separate current-weekly-status/panel analysis. Do not pool
revisits into the annual usual-status sample.

The full audit discovers this gitignored PLFS materialization on every run and
writes `outputs/diagnostics/extended/labor/plfs_2017_18_materialization.csv`.
Before conversion it records the expected non-error `not_materialized` state. A
partial F1 materialization or a missing/mismatched conversion manifest is a hard
error. Once F1 and its sidecar validate, inspect the realized table's multiplier,
geography, design, person-identity, and status distributions before adding the
canonical PLFS adapter. The audit records provenance without packaging the large
local interim CSV in `review.zip`.


After `plfs_2017_18` materialization is `ready`, the extended labor audit reads only the contracted F1 columns, applies the official annual multiplier rule from the bundled PLFS 2017-18 README, and constructs the documented first-visit person identity. Preferred geography uses the reviewed 2017-18 `primary_source_crosswalk`; the stricter deterministic `conservative_source_crosswalk` is run separately as a sensitivity. Unrestricted population-allocation rows are not treated as resolved PLFS geography. Deterministic rows inside the primary bridge retain deterministic lineage status, while accepted single-target upgrades are labeled separately. The audit writes the preferred `plfs_2017_18_*` diagnostics/outcomes, parallel `plfs_2017_18_conservative_*` files, and `plfs_2017_18_variant_comparison.csv`, which summarizes coverage and overlapping-estimate sensitivity by outcome.

### Labor mechanism inference

The design-based labor district outputs are broader than the causal labor family. The registered IV mechanism family is fixed to labor-force participation and employment for NSS66 (`early_post`) and PLFS 2017-18 (`long_run_post`). Both outcomes use the same age-15+ denominator and therefore avoid letting the much thinner unemployment/employed-composition domains determine the common causal sample. NSS64 remains a near-treatment reference only.

The extended audit routes those registered outcomes through the shared post-treatment mechanism inference layer used elsewhere in the repository. It writes `nss66_mechanism_*` and `plfs_2017_18_mechanism_*` files containing the registry, common-sample coverage, first stage, reduced form, weak-IV/2SLS summaries, and Anderson-Rubin grids. The PLFS deterministic geography sensitivity is written separately under the `plfs_2017_18_conservative_mechanism_*` prefix. These files are diagnostics/reviewer evidence; they do not alter the preferred control set or treat post-treatment labor outcomes as controls.
### Cross-family post-treatment mechanism evidence

Extended diagnostics write compact cross-family mechanism summaries to `outputs/diagnostics/extended/mechanisms/evidence_grid.csv` and `family_summary.csv`. These combine only families already routed through the shared post-treatment mechanism engine; they do not reinterpret descriptive household/worker outputs as causal mediation. Pointwise Anderson--Rubin inversion grids remain cached in the corresponding target objects and are not serialized, because the persisted weak-IV tables already contain the beta-zero test, confidence-set bounds/components, grid truncation, disconnection, and zero-containment fields needed for review.


### Specification-governance ledger

The full extended audit writes `outputs/diagnostics/extended/iv/candidate_design_ledger.csv`. This is governance metadata, not a batch-estimation request. It follows the methodological reference plan in order and records, for each family, the scientific question, design axis, execution policy, multiplicity family, prerequisites, admissibility, and implementation status. The ledger is intentionally comprehensive-by-theory: broad first-stage relevance families are visible because attenuation across geography, controls, treatment definitions, distance bases, and vintages is itself evidence; causal robustness families are visible only where the estimand remains interpretable. Mechanically crossable but unjustified interactions remain explicit non-goals. Public headline IV models are generated from `public_iv_specification_registry()` and retain their historical model names so public tables and report values are unchanged.


### Specification execution aliases

The first-stage governance layer distinguishes scientific specification labels from unique model executions. Symmetric block interventions can duplicate older cumulative or leave-one-block-out formulas. The build therefore retains all named candidate questions, executes each formula/sample signature once, and writes `outputs/diagnostics/extended/instrument_relevance/first_stage_absorption_aliases.csv` so semantic aliases remain auditable. The candidate-design ledger records scientific candidate/implemented cells separately from unique execution cells.
