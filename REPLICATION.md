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
- `data/raw/dise_internet_archive/` (main-paper DISE administrative validation)
- `data/raw/census_2001/languages/C16/`
- `data/raw/census_2001/religion/C01/`
- `data/raw/census_2001/education/C08/`
- `data/raw/census_2001/age/C14/`
- `data/raw/census_2001/age/C13/` (elementary-age DISE exposure denominator)
- `data/raw/census_2011/age/C13/` (elementary-age DISE exposure denominator)
- `data/raw/census_2001/housing/H09/`
- `data/raw/district_boundaries_2020/`
- `data/raw/district_changes/`

Census 1991, 2001, and 2011 workbooks in the tracked acquisition manifests can be restored without redownloading nonempty files already present:

```bash
make download-census-tables
```

This runs `bash scripts/download_census_tables.sh`, which discovers and processes every `data/metadata/census_*_download_manifest.tsv` by default (1991, 2001, and 2011). The downloader creates missing destination directories, contacts Census of India only for missing or empty files, and writes through a temporary `.part` file before the final rename. Acquisition manifests may include source families with narrower published state coverage, such as Scheduled Tribe language tables; readers must declare that scope explicitly rather than assuming 35 files. A specific manifest can still be supplied explicitly as a script argument.

The 2001 acquisition manifest stores H-04 Appendix (`PC01_H04a`) under `data/raw/census_2001/housing/H04A/`. All 35 workbooks have been inspected: the permanent/semi-permanent/temporary/unclassifiable partition and temporary serviceability subpartition close exactly, and all 593 district household totals match Census 2001 H-09. H04A and Census 2011 HL-13 therefore form the active structural-durability longitudinal pair; durability changes remain descriptive rather than expanding the fixed housing weak-IV registry.

The 2001 C-13 workbooks are stored under `data/raw/census_2001/age/C13/`; the 2011 C-13 workbooks are stored under `data/raw/census_2011/age/C13/`. They are required by the extended DISE diagnostics that construct the ages-6-13 administrative exposure denominator. The acquisition manifests remain separate from the production `data/metadata/file_manifest.csv` because the public core pipeline does not require these workbooks unless extended diagnostics are enabled.

## Optional district-lineage inputs

The production lineage reads its tracked source registry and reviewed metadata directly. Extended diagnostics additionally discover local LGD, SHRUG, Census-locality, historical boundary, literature-derived, and published-concordance files. Their source catalog and acquisition routes are recorded in [`data/metadata/data_sources.csv`](data/metadata/data_sources.csv); the final methodology and bounded follow-up work are documented in [`docs/DISTRICT_LINEAGE.md`](docs/DISTRICT_LINEAGE.md).

The audit builds or validates the compact canonical Census-2001 GeoPackage and the display-only 99/99 map scaffold from the code-keyed DataMeet Census-2001 district shapefile before the public pipeline. With `--with-extended-diagnostics`, it then writes lineage diagnostics under `outputs/diagnostics/extended/district_lineage/`, including conservative, primary, full-reviewed, and legacy-comparison artifacts. Current LGD registries, compact modification rosters, SHRUG keys, Census-2011 district geometry, candidate trackers, and tracked adjudication ledgers support those diagnostics. Large village/Census attribute tables, post-2018 LGD change history, and SHRID/village polygon archives remain inventoried rather than loaded during every audit. SHRUG geometry continues to support lineage validation but is not used to draw public maps.

The 573-district reviewed primary panel is the production geography. Exact or fuzzy name matches remain review candidates unless they are recorded in tracked adjudication or review metadata; the 408-district conservative and 587-district full-reviewed panels remain explicit robustness specifications.

## System dependencies

The R package dependencies are declared in [`DESCRIPTION`](DESCRIPTION), and [`renv.lock`](renv.lock) records exact package versions. Spatial packages such as `sf` and `spdep` may also require GDAL, GEOS, PROJ, and `pkg-config` system libraries. PDF text checks use Poppler's `pdftotext` executable. The conference poster additionally requires Quarto 1.9.18 or newer for its Typst custom format. The format follows Quarto's standard two-part template structure: `typst-template.typ` defines the poster function and `typst-show.typ` passes document metadata and body content to it. On macOS, these are commonly available through Homebrew as `gdal`, `geos`, `proj`, `pkg-config`, and `poppler`.
The `testthat` development dependency in the project's `Suggests` field is intentionally locked through `snapshot.dev = true`. Recursive package installation uses renv's standard strong dependency fields (`Imports`, `Depends`, and `LinkingTo`) rather than installing the optional `Suggests` of every dependency. To avoid repeating the same synchronization scan in every child R process, [`.Rprofile`](.Rprofile) disables renv's activation-time check; [`scripts/check_source_syntax.sh`](scripts/check_source_syntax.sh) runs one explicit `renv::status(dev = TRUE)` gate instead.

On a new machine, restore the project library from the tracked lockfile with `make restore`. This delegates directly to `renv::restore()` and does not modify `renv.lock`. The canonical public-build audit also runs this restore step before its explicit `renv::status(dev = TRUE)` synchronization gate, so a newly committed runtime dependency does not cause a guaranteed first audit failure merely because the local project library predates the lockfile. If renv must build packages from source, the corresponding system requirements must already be installed. In this lockfile, `s2` declares CMake and OpenSSL requirements, while the spatial stack may also require GDAL, GEOS, PROJ, and `pkg-config`. On macOS, use the standard CRAN R toolchain for the installed R version; Homebrew provides `cmake` and `pkgconf` (which supplies `pkg-config`).

If an audit is interrupted manually while `{targets}` is running, its callr child can outlive the shell process. The next canonical audit checks the recorded `_targets` process immediately after restoring the project library. Dead/stale process metadata is cleared with `targets::tar_unblock_process()`. A genuinely live process is never terminated automatically: the audit stops early, prints the PID, and shows `kill <PID>` so the user can inspect and terminate only the process they intentionally abandoned.

For an immediate debug snapshot after an interrupted run, `bash scripts/make_review_archive.sh --without-samples --allow-incomplete --output review.zip` copies current working-tree versions of tracked source files together with useful intermediate review artifacts, including `outputs/diagnostics`, `outputs/benchmarking`, and `data/processed`. Raw data and local caches remain excluded. Newly created source files should be staged or committed before relying on the archive to carry them; the archive does not recursively sweep arbitrary untracked source files.

Project paths are relative to the repository root by default. Moving or renaming the project directory should therefore not require edits to tracked configuration or deletion of the `{targets}` store. Functions that are explicitly given another root still resolve paths under that root, which keeps temporary-directory tests and external callers predictable.

## Public processed outputs

The tracked district-level replication inputs are:

- [`data/metadata/district_harmonization_crosswalk.csv`](data/metadata/district_harmonization_crosswalk.csv), the single tracked district harmonization authority;
- [`data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv`](data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv), the harmonized Census-2001 district panel; and
- [`data/processed/consumption_district_welfare.csv`](data/processed/consumption_district_welfare.csv), district-by-round welfare estimates and support flags for the registered NSS/HCES rounds.

SHA-256 digests for tracked metadata CSV/TSV files are recorded in [`data/metadata/checksums.csv`](data/metadata/checksums.csv). `data/metadata/file_manifest.csv` also supports SHA-256 identities for stable raw inputs; the source preflight enforces registered byte sizes and hashes before readers run. Hashes are left blank when the author-used bytes have not yet been verified. Maintainers can populate or refresh one or more verified source families with `Rscript scripts/update_file_manifest_hashes.R SOURCE_ID [SOURCE_ID ...]`, then refresh the metadata digest registry. Generated processed outputs are validated by the pipeline and output checks rather than pinned to a pre-build digest. Refresh metadata digests with:

```bash
Rscript scripts/update_checksums.R
```

## Expected behavior without raw data

The full build begins with `make prepare-data`, which downloads missing Census workbooks covered by the tracked acquisition manifests. Sources that cannot be redistributed or downloaded automatically must still be supplied under the paths in [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv). A missing local source should therefore fail during source validation with the exact required path rather than later inside a reader.

`make prepare-data` also downloads the Natural Earth 1:10m breakaway/disputed-area polygon theme used only for manuscript cartography. `data/metadata/map_disputed_areas.csv` assigns `display_disputed` to Aksai Chin, Azad Kashmir, Gilgit-Baltistan, Shaksam Valley (displayed as the Trans-Karakoram Tract), and Siachen Glacier, and assigns `administered_reference` to Natural Earth's Jammu-and-Kashmir polygon. The latter is never rendered directly. The lineage-geometry builder separately retains DataMeet's noncanonical 99/99 "Data Not Available" feature as `data/processed/geography/district_2001_map_scaffold.gpkg`. The boundary-reference module compares complete canonical DataMeet state-01 geometry with the Natural Earth administered reference plus the five display polygons; remaining DataMeet J&K polygon components that share a one-dimensional boundary with another Census state stay ordinary geography, while residual components without such a state side join the disputed/no-district-estimate class. That residual, the five registered disputed polygons, and the DataMeet 99/99 scaffold are unioned once into the final display mask. Public maps subtract that mask from district polygons first, draw the disputed class, and only then derive ordinary state/external outlines from the clipped district geometry. Thus the retained J&K polygon receives the same solid black state outline as other states without reconstructing a special cross-source seam. The canonical 593-district GeoPackage remains the sole analytical geography, so joins, samples, spatial weights, and estimates are unchanged. The required Natural Earth 5.1.1 shapefile components are registered with exact byte sizes and SHA-256 digests, so an upstream change cannot silently alter the figures.

A raw-data-less clone can rerun the district-level consumption, conversion-gradient, linguistic-distance first-stage, and weak-IV analyses from the tracked processed inputs with `make replicate-processed`. That command uses a separate `_targets_processed.R` script and `_targets_processed/` store, so it does not depend on or mutate the full-reconstruction target store. Processed CSV readers preserve Census district and state identifiers as character fields, including leading zeroes, while allowing ordinary measurement columns to use base R type inference. When a full-source `_targets/` store is also available, `make verify-processed-replication` updates the processed graph and compares the reported empirical components of the five shared result objects with numerical tolerance. Final full builds run this verification automatically. The processed tier intentionally excludes the individual-level education-selection model because the repository does not assert redistribution rights for the underlying NSS microdata. Full reconstruction, selection estimation, DISE reconstruction, Census mechanism rebuilding, and other source-level validation still require the original inputs listed in the raw manifest.

## Commands

The human-facing build commands are documented in [`docs/BUILD.md`](docs/BUILD.md). The principal commands are:

```bash
# Unit tests.
make test

# Verified target build with the final scientific configuration.
make pipeline

# District-level analysis replication from tracked processed inputs only.
make replicate-processed

# Compare shared processed-tier results with an existing full-source build.
make verify-processed-replication

# Faster target build that omits expensive full AME computation.
make pipeline-fast

# Paper-facing outputs and checks.
make paper

# Application samples only.
make samples

# Complete repository build. Equivalent to the script below.
make all
bash scripts/run_full_build.sh
```

`make samples` generates the named and anonymous writing/coding variants declared in [`application-samples/samples.yml`](application-samples/samples.yml). Writing excerpts come from current-paper section IDs rather than a separately maintained manuscript.

`run_full_build.sh` uses [`config/final.yml`](config/final.yml), includes application samples, writes `review.zip`, and leaves the conference poster out unless requested. Useful options are:

```bash
# The command used for routine development review after source changes.
caffeinate -dimsu bash scripts/run_full_build.sh \
  --no-samples \
  --with-extended-diagnostics \
  --with-benchmarks \
  2>&1 | tee full_output.txt

# Include the conference poster.
bash scripts/run_full_build.sh --with-poster

# Use the faster scientific configuration.
bash scripts/run_full_build.sh --fast

# Delete generated outputs and the {targets} store before reconstruction.
bash scripts/run_full_build.sh --from-clean-slate
```

The ordinary build keeps the `{targets}` store and existing generated files. `{targets}` determines which steps are out of date, so routine development does not need an "incremental" mode. `--from-clean-slate` is an exceptional reconstruction check. A fresh clone already begins without a `{targets}` store.

The main target build requests up to four parallel consumption-domain workers for expensive design-based distributional welfare statistics. Set `EMI_CONSUMPTION_DOMAIN_CORES=1` for serial execution or a smaller value on a memory-constrained machine; the R helper clamps the request to detected physical cores and Windows remains serial.

`make pipeline-fast` changes the scientific configuration to [`config/fast.yml`](config/fast.yml). It does not select a different set of optional target families. Extended diagnostics, benchmarks, application samples, and the poster are selected separately by the build command. See [`docs/BUILD.md`](docs/BUILD.md) for the distinction.

On Windows, run the shell entry point through WSL or Git Bash. From PowerShell, replace `tee` with `Tee-Object -FilePath full_output.txt`; from `cmd.exe`, redirect with `> full_output.txt 2>&1`.

## Review archive

[`scripts/make_review_archive.sh`](scripts/make_review_archive.sh) writes `review.zip`. Outside failure/debug mode it requires `.public-final-ok`, which is written only after a verified final build. `run_full_build.sh` replaces the archive on every run and can package the current failed state with `--allow-incomplete` when a build stops early. Use `--no-archive` on the full build when no archive should be created.

Archive contents follow the selected build profile. Application-sample PDFs are included by default; conference-poster renders are included only when requested. Raw data, dependency libraries, target caches, and other local caches are excluded.

### Lineage-source execution

Extended district-lineage checks track each loaded raw source independently. Large LGD SpreadsheetML changed-unit rosters are streamed into the columns needed by the lineage code, and SHRUG key readers retain only the fields used by the bridge. Large geometry and locality-attribute archives that are not required by a selected run remain inventoried without being loaded. The reviewed Census-2001 geometry is stored at `data/processed/geography/district_2001.gpkg` and can be reconstructed from its tracked source inputs.

The project disables renv's automatic synchronization message at activation. The full build restores the tracked lockfile once, then `scripts/check_source_syntax.sh` runs one explicit development-aware `renv::status(dev = TRUE)` verification. This avoids repeating the same synchronization scan in every child R process while preserving a failing gate when restoration does not produce the locked environment.

The conference poster uses Quarto's Typst custom format. It is intentionally outside the ordinary build and renders only when `--with-poster` or `make poster` is requested.

### NSS 64 labor source validation

Extended diagnostics read the NSS 64 Schedule 10.2 DDI and person-level Block 4/Block 6 sources from `data/raw/nss/`. They validate the common person universe, matching cross-block geography/design fields, source schema, survey-design identifiers, and positive combined survey weights before any district labor outcome is constructed. The labor source then reuses the reviewed `nss_2007_08` district lineage through the published five-digit `SSRDD` identity and accepts only deterministic mappings. Source validation is written to `outputs/diagnostics/extended/labor/nss64_source_validation.csv`; source-district lineage/support is written to `nss64_lineage_support.csv`; pooled Census-2001 target support, including distinct FSUs and Kish effective sample size, is written to `nss64_target_support.csv`.

NSS64 labor diagnostics now include a predeclared design-based district outcome family. The estimator uses reviewed deterministic Census-2001 lineage, the posted combined multiplier, state/sector-nested strata and FSUs, standard principal-plus-subsidiary usual-status classification, and denominator-specific support. `outputs/diagnostics/extended/labor/nss64_district_outcomes.csv` retains all estimable district points and flags preferred support at at least 5 FSUs and Kish effective N at least 100 rather than deleting thin domains.

### NSS66 proprietary microdata

The NSS66 employment/unemployment source is distributed as a `.Nesstar` container with companion DDI XML. The final-paper local-development synthesis now consumes the primary NSS66 labor object, so its contracted materialization is a strict publication prerequisite rather than an optional extended diagnostic. Convert the container with the maintained `nesstar-converter` package rather than adding a repository-specific binary parser, then inspect the generated F4/F5/F6 tables against the DDI case counts and schema.

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
Because the final-paper synthesis consumes the primary PLFS labor object, a missing
or partial F1 materialization, or a missing/mismatched conversion manifest, is now a
strict build error. Once F1 and its sidecar validate, inspect the realized table's
multiplier, geography, design, person-identity, and status distributions. The audit
records provenance without packaging the large local interim CSV in `review.zip`.


After `plfs_2017_18` materialization is `ready`, the core publication analysis reads only the contracted F1 columns, applies the official annual multiplier rule from the bundled PLFS 2017-18 README, and constructs the documented first-visit person identity. Preferred geography uses the reviewed 2017-18 `primary_source_crosswalk`; the stricter deterministic `conservative_source_crosswalk` remains an extended sensitivity. Unrestricted population-allocation rows are not treated as resolved PLFS geography. Deterministic rows inside the primary bridge retain deterministic lineage status, while accepted single-target upgrades are labeled separately. The extended audit persists the preferred `plfs_2017_18_*` diagnostics/outcomes, parallel `plfs_2017_18_conservative_*` files, and `plfs_2017_18_variant_comparison.csv`, which summarizes coverage and overlapping-estimate sensitivity by outcome.

### Labor local-development inference

The paper-facing labor family is fixed to labor-force participation and employment for NSS66 (`early_post`) and PLFS 2017-18 (`long_run_post`). Both outcomes use the same age-15+ denominator, avoiding sample selection through the much thinner unemployment/employed-composition domains. The primary NSS66 and PLFS reduced-form objects are strict publication dependencies because their null results are deliberately retained in the broader local-development synthesis; NSS64 remains a near-treatment reference only.

The shared post-treatment inference layer still computes the accompanying first-stage, weak-IV/2SLS, and Anderson-Rubin objects, but the paper uses these labor results as descriptive reduced-form evidence rather than as identified EMI mechanisms. Extended diagnostics persist `nss66_mechanism_*` and `plfs_2017_18_mechanism_*` bundles and write the deterministic PLFS geography sensitivity separately under the `plfs_2017_18_conservative_mechanism_*` prefix. These files are diagnostics/reviewer evidence; they do not alter the preferred control set or treat later labor outcomes as controls.
### Cross-family post-treatment mechanism evidence

Extended diagnostics write compact cross-family mechanism summaries to `outputs/diagnostics/extended/mechanisms/evidence_grid.csv` and `family_summary.csv`. These combine only families already routed through the shared post-treatment mechanism engine; they do not reinterpret descriptive household/worker outputs as causal mediation. Pointwise Anderson--Rubin inversion grids remain cached in the corresponding target objects and are not serialized, because the persisted weak-IV tables already contain the beta-zero test, confidence-set bounds/components, grid truncation, disconnection, and zero-containment fields needed for review.


### Specification-governance ledger

The full extended audit writes `outputs/diagnostics/extended/iv/candidate_design_ledger.csv`. This is governance metadata, not a batch-estimation request. It follows the methodological reference plan in order and records, for each family, the scientific question, design axis, execution policy, multiplicity family, prerequisites, admissibility, and implementation status. The ledger is intentionally comprehensive-by-theory: broad first-stage relevance families are visible because attenuation across geography, controls, treatment definitions, distance bases, and vintages is itself evidence; causal robustness families are visible only where the estimand remains interpretable. Mechanically crossable but unjustified interactions remain explicit non-goals. Public headline IV models are generated from `public_iv_specification_registry()` and retain their historical model names so public tables and report values are unchanged.


### Specification execution aliases

The first-stage governance layer distinguishes scientific specification labels from unique model executions. Symmetric block interventions can duplicate older cumulative or leave-one-block-out formulas. The build therefore retains all named candidate questions, executes each formula/sample signature once, and writes `outputs/diagnostics/extended/instrument_relevance/first_stage_absorption_aliases.csv` so semantic aliases remain auditable. The candidate-design ledger records scientific candidate/implemented cells separately from unique execution cells.

The Shastry Hindi-belt relevance check is a separate two-cell diagnostic rather than another entry in the broad absorption ladder. The historical state definition follows Shastry's published specification and is frozen on Census-2001 codes. The audit estimates the preferred nonzero-mean linguistic-distance first stage with main Census controls plus the Hindi-belt indicator under no geographic FE and six-region FE, using one common support, and writes paired changes relative to the otherwise identical baselines. State FE are intentionally absent because the state-level indicator would be absorbed. Outputs are `hindi_belt_first_stage_comparison.csv`, `hindi_belt_state_definition.csv`, and `hindi_belt_first_stage_common_support.csv` under `outputs/diagnostics/extended/instrument_relevance/`.

The expanded first-stage control audit writes both execution-level and scientific-question-level artifacts. `first_stage_absorption_ladder.csv` contains the 49 unique fitted specifications; `first_stage_absorption_aliases.csv` maps all 55 named questions to those executions; and `first_stage_absorption_semantic_summary.csv` carries the execution results back onto every scientific question. Exact aliases are therefore visible but never refit.

### Scalar-IV consumption robustness

After the symmetric first-stage control audit is available, the extended IV graph estimates the registered 48-cell scalar robustness family: eight endpoint/estimand designs crossed with region/state compact-2001 adjustment (historical `main` IDs) and Shastry/Glottolog/Dyen scalar distance measures. The compact vector is held fixed to isolate instrument/geography robustness; it is not treated as the uniquely valid causal adjustment set. The six designs within each endpoint/estimand share one complete-case district sample. Extended diagnostics write `consumption_scalar_iv_robustness.csv` and `consumption_scalar_iv_robustness_common_support.csv`. The summary includes Holm-adjusted reduced-form and Anderson--Rubin beta-zero p-values both within each six-design welfare family and across the full 48-cell family. Pointwise AR grids are not serialized for this robustness family because the summary already retains the weak-IV confidence-set diagnostics needed for review.

A separate control-strategy family compares region/state geography-only specifications, compact-2001 adjustment, and compact-2001 adjustment without human capital using the preferred Shastry scalar instrument. It is activated only after the alternative-welfare family is reviewed, so the response and control axes remain sequential rather than Cartesian. The eight registered real-mean-MPCE endpoint/estimand designs are each estimated on one common sample across the six control strategies. The audit writes `consumption_control_strategy_robustness.csv` and `consumption_control_strategy_robustness_common_support.csv`, with Holm-adjusted reduced-form and Anderson-Rubin beta-zero p-values within endpoint and across the full 48-cell `consumption_control_strategy` family. This remains distinct from the finite control-parameterization family (secondary-plus versus literacy; compact versus decomposed economic structure) and from 1991-vintage adjustment robustness.


Extended diagnostics are selected by the `diag_ext_` target prefix. Registered robustness computations can use ordinary internal target names, but every durable extended artifact must terminate in a `diag_ext_` file target so `scripts/run_targets_checked.R --starts-with diag_ext_` actually traverses the computation. The scalar-consumption robustness files and the persisted cross-family `analysis_design_registry.csv` follow this contract.


### Alternative welfare robustness

Extended IV diagnostics derive the alternative-welfare registry from the eight registered consumption endpoint/estimand templates and the survey-aware welfare metadata. The welfare metadata distinguish the survey estimator `transform` from `iv_analysis_transform`: mean-log MPCE enters the IV layer on its existing log scale, while positive monetary median and bottom-40 means are logged for IV analysis. Unsupported survey pairs are never generated. The resulting 20 response templates are each crossed with the six registered scalar IV/geography designs and estimated on template-specific common support.

The audit writes `consumption_alternative_welfare_robustness.csv` and `consumption_alternative_welfare_robustness_common_support.csv`. Holm-adjusted reduced-form and Anderson-Rubin beta-zero p-values are reported both within each six-design response template and across the full 120-cell `consumption_welfare_robustness` family. Pointwise AR inversion grids are not persisted.


The realized control-strategy family is followed by a separate compact-2001 parameterization family. It re-estimates the benchmark secondary-plus/compact-economic controls together with three registered substitutions—literacy, decomposed economic structure, and both—under region and state FE. The eight adjustments within each endpoint/estimand use one common sample and the preferred Shastry scalar instrument. Across eight real-mean-MPCE endpoint/estimand designs the audit therefore estimates 64 cells, writes `consumption_control_parameterization_robustness.csv` plus its common-support table, and applies Holm adjustment within each eight-adjustment endpoint and across the full `consumption_control_parameterization` family. The benchmark is deliberately re-estimated inside the family so parameterization comparisons do not rely on a differently selected sample.

After reviewing that family, the extended audit also runs a remote historical-adjustment comparison. `historical_baseline_g2_sensitivity` already constructs population-interpolated PCA91 district controls on Census-2001 production geography; the causal robustness family freezes the existing 99% source-coverage threshold and attaches those PCA91 controls to the consumption IV panel. Each of the eight registered real-mean-MPCE endpoint/estimand designs is re-estimated under four adjustments: region/state FE with the compact-2001 benchmark and region/state FE with the PCA91 baseline. All four share one common sample within endpoint. The audit writes `consumption_historical_adjustment_robustness.csv` and `consumption_historical_adjustment_robustness_common_support.csv` and applies Holm adjustment within endpoint and across the 32-cell `consumption_historical_adjustment` family. This is a remote-baseline robustness exercise rather than a pure same-variable vintage substitution because the PCA91 and compact-2001 concept sets overlap but are not identical.

The extended consumption graph also supports a concept-matched historical-control check using the verified Vanneman-Barnes 1991 `dist91` archive. `build_vanneman_1991_control_sufficient_statistics()` reads the registered fixed-width Census records, and `build_population_interpolated_vanneman_baseline_1991_from_counts()` applies the same frozen G2 1991-to-2001 population allocation to those extensive sufficient statistics at the 99% source-coverage threshold before constructing ratios. The resulting controls add 1991 urbanization, Muslim share, matriculation, agricultural-worker structure, age dependency, and household electricity to the population/SC/ST concepts. The audit estimates compact-2001, PCA91, and Vanneman adjustments under region/state FE on a common six-design sample and writes `consumption_historical_concept_matched_robustness.csv` plus its common-support table. Its Holm family is separate from the earlier PCA91 robustness family because the Vanneman source was introduced later.


### Census 1991 primary-source validation

`data/metadata/census_1991_download_manifest.tsv` freezes the official ORGI workbooks selected to validate the consequential historical-control robustness results: B-01(S) worker status, C-02 total and urban education, C-06 age structure, and C-09 religion. B-01(S), C-02/C-02U, and C-06 came from the reviewed state-workbook acquisition set; C-09 was added separately because those tables do not independently validate the compact-control Muslim-share concept. ORGI catalog ID `1991-C09T-01` publishes a single all-India workbook, `1991-C09T-0100.xlsx`, whose documented geography includes districts. Readers keep district rows only; an aggregate-only small-UT workbook is allowed to contribute an empty district slice rather than converting its state-total `00` code into a fictitious district. Cross-source completeness is checked after the state workbooks are combined against the Vanneman validation universe. The reviewed 1991 state-workbook series omit state code 10 (Jammu & Kashmir). Download missing files with `make download-census-tables`.

The strict final-paper graph parses those files into source-district sufficient statistics and compares them directly with the cached Vanneman `dist91` counts before any 1991-to-2001 allocation. Extended diagnostics reuse the same validated object for forensic persistence. Exact contracts cover total and urban population, secondary-plus education counts, main workers, dependent and working-age population, Muslim population, and the C-09 religion-category population sum. C-02U may omit an all-rural district only when Vanneman independently reports zero urban population. Published C-09 total population is retained as a non-fatal diagnostic because the Dhule row differs from its own religion-category sum; the Muslim and religion-category counts remain the validation quantities. The resulting files are written under `outputs/diagnostics/extended/instrument_relevance/census_1991_primary_validation_*.csv`.

The official tables validate source concepts only; they do not retrospectively replace the already-observed Vanneman/PCA91 causal-control family. The 1991 H-4/H-5 catalog products are only country/state-level, so they are not downloaded as district-control sources. For electricity, the current district-level historical evidence remains Vanneman H-4 counts plus SHRUG 1991 village/town directory power-availability fields; those can be used for independent validation without pretending the state H-series is district data.

### Intensive-margin EMI robustness

Extended diagnostics estimate the predeclared 48-cell intensive-margin treatment family after the historical-source validation gate. It uses the eight registered primary-welfare endpoint/estimand designs, `emi_share_enrolled_0708` as treatment, and the same six region/state × Shastry/Glottolog/Dyen scalar-IV designs used by the scalar robustness family. The six designs are re-estimated on one common sample within endpoint/estimand. Outputs are written under `outputs/diagnostics/extended/consumption/consumption_treatment_robustness*.csv`; pointwise Anderson--Rubin grids are not persisted.

After all registered consumption robustness families are available, `consumption_robustness_evidence` combines their already-adjusted summaries without recomputing estimates or p-values. The audit writes `consumption_robustness_evidence_grid.csv` and `consumption_robustness_family_summary.csv` in the same extended consumption directory. These files are the preferred cross-family reviewer summary; source family CSVs remain authoritative for model-level details and common-support diagnostics.


### Shastry comparison controls and future projects

The extended relevance audit includes a paired Census-2001 child-population comparison. `log_child_population_5_19_2001` is built from exact C-14 five-year district age bands (5-9, 10-14, 15-19) and is not promoted into the preferred main-control vector. On the realized 573-district support it barely changes the first stage: F moves from about 14.29 to 13.61 without geographic FE and from about 3.238 to 3.227 with six-region FE. The paired Shastry-control engine is shared with the Hindi-belt comparison in `R/diagnostics/diagnose_shastry_control_first_stage.R`. The DISE district-report-card archive does not support a genuine school-offering EMI supply rate: its medium fields are enrollment counts and are incomplete for non-reporting schools. The candidate-design ledger therefore distinguishes that unavailable supply measure from the separately deferred IHDS, low-cost-private-school, and mother-tongue-learning follow-on projects.


### Paper-facing identification synthesis

The public paper is synchronized to the completed registered design program rather than treating Census migration, Economic Census, labor outcomes, weak-IV inference, or Shastry comparison controls as future work. The report deliberately does not read extended diagnostic CSV files at render time: the core public build remains self-contained, while exact cross-family robustness counts, weak-IV classifications, and mechanism summaries remain reviewer-facing artifacts under `outputs/diagnostics/extended/`. Paper prose therefore states the stable qualitative conclusions frozen by the design ledgers and realized audits: conditional first-stage relevance is weak, neither the Hindi-belt nor age-5-19 comparison rescues it after broad-region adjustment, and isolated weak-IV-robust beta-zero rejections are interpreted together with confidence-set boundedness and multiplicity rather than as conventional 2SLS confirmation.
