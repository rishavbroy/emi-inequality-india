# Legacy diagnostics and benchmarking coverage

This file records how diagnostic and tuning/benchmarking logic from the legacy Rmd is represented in the targets/Quarto refactor.  The organizing rule is run policy, not file location: final-paper-required diagnostics run in the public pipeline, extended diagnostics run only when requested, and benchmarking/tuning runs only when requested.

## Public diagnostics

- Chunk 24/29 spatial weights and Moran's I diagnostics: `R/diagnostics/diagnose_spatial_weights.R` and `R/diagnostics/diagnose_spatial_autocorrelation.R`; output under `outputs/diagnostics/public/` for values used in the final paper.
- Chunk 28 multicollinearity checks: `R/diagnostics/diagnose_multicollinearity.R`; output is consumed by report values and diagnostics.
- First-stage weak-instrument checks from the IV section: `R/iv/diagnose_weak_instruments.R`; these remain in the IV module because they are core model diagnostics.

## Extended diagnostics

- Chunk 8 missingness diagnostics: `R/selection/diagnose_missingness.R`; output under `outputs/diagnostics/extended/missingness/`.
  - Missing-variable counts for probit-relevant rows.
  - Regional missingness rankings for enrollment cost, distance to primary school, and father education.
  - Missingness correlation matrices for all rows and enrolled rows.
  - One-logit-per-missing-variable screens with Benjamini-Hochberg adjustment.
  - Notes for commented Rajasthan/Southern case-study views and chi-square checks.
- Chunks 5, 6, 20, and 21 district-matching/source diagnostics: `R/diagnostics/diagnose_district_tracker_sources.R` and `R/diagnostics/diagnose_district_matching.R`; outputs under `outputs/diagnostics/extended/district_tracker_sources/` and `outputs/diagnostics/extended/district_matching/`.
  - Source coverage, state/UT changes, in-period district-name changes, same-name district diagnostics, unmatched rows, many-to-many/flagged rows, and searchable all-row source tables.
- Chunk 16 fuzzy matching diagnostics: `R/diagnostics/diagnose_fuzzy_matching.R`; outputs under `outputs/diagnostics/extended/fuzzy_matching/`.
  - Legacy methods and thresholds: soundex = 0, qgram = 0, jw <= 0.15, dl <= 2, osa <= 1.
  - Legacy troublesome district-name pairs and match-status counts.
- Chunk 24 spatial-weight extended diagnostics: `R/diagnostics/diagnose_spatial_weights.R`; outputs under `outputs/diagnostics/extended/spatial/`.
  - Rook/queen comparison and island/neighbor-count summaries.

## Benchmarks and tuning outputs

- Chunk 10 AME runtime/tuning: `R/diagnostics/diagnose_ame_benchmark.R` called through `R/benchmarking/benchmarking_targets.R`; output under `outputs/benchmarking/ame/`.
  - Retains final choice `marginaleffects_parallel = FALSE`, `set.seed(999)`, sample-size timing checks, forward-difference comparison, and documented failed future parallelization.
- Chunk 16 fuzzy-match threshold benchmarking: `R/benchmarking/benchmarking_targets.R`; output under `outputs/benchmarking/fuzzy_matching/`.
- Chunk 24 rook-versus-queen spatial-weight benchmarking: `R/benchmarking/benchmarking_targets.R`; output under `outputs/benchmarking/spatial_weights/`.
- Chunk 30 experimental spatial IV attempts: `R/iv/estimate_spatial_iv_experimental.R` called through `R/benchmarking/benchmarking_targets.R`; output under `outputs/benchmarking/spatial_iv/`.
  - The spatial lags `W_consY`, `W_giniY`, `W_EMIE`, `W_wLing`, `W2_wLing`, and lagged controls are generated when inputs are available.
  - The model attempts remain opt-in benchmarks because the legacy chunk was `eval=FALSE` and documented as failing or unsuitable for final use.

## Legacy logic intentionally documented rather than force-run

- View-only or GUI-only exploratory code, such as `View()` calls, Tabula inspection, palette GUI exploration, and commented `tmap_save()` experiments, is represented as notes or benchmark metadata rather than executed automatically.
- Expensive or unstable commented paths, especially the future/marginaleffects parallel attempt and spatial-IV model attempts, are opt-in benchmark artifacts and not part of the normal public build.
- Normal public builds preserve `outputs/diagnostics/extended/` and `outputs/benchmarking/`; only `outputs/diagnostics/build/` and `outputs/diagnostics/public/` are reset automatically.

## Correctness follow-up notes

The diagnostics/benchmarking correctness pass adds the following guardrails after the first full optional run exposed incomplete parity:

- Optional target groups are now run through `scripts/run_targets_checked.R`, which inspects selected target metadata and exits non-zero if any requested `diag_ext_` or `bench_` target recorded an error.  This prevents benchmark failures from being hidden by a successful review archive.
- Chunk 30 spatial-IV formulas are built through `make_iv_formula(dep, endog, instruments, controls)`, matching the current IV adapter while preserving the legacy choice to treat `W_consY`/`W_giniY`, `EMIE`, and `W_EMIE` as endogenous and to use `wavg_ling_degrees`, `W_wLing`, and `W2_wLing` as excluded instruments.
- Chunk 20 district-matching diagnostics read matcher attributes before data-frame coercion so explicit empty `unmatched_rows` attributes are not replaced by broad `match_status` fallbacks.
- Chunk 16 fuzzy-matching benchmarks now include active tracker/join candidate pairs in addition to the hand-written troublesome pairs from the legacy comments.
- Chunk 24 spatial-weight outputs now include the legacy commented rook/queen mean-neighbor references and current-vs-legacy deltas.
- Chunk 10 AME benchmarking preserves the legacy `vcov = TRUE` method attempt but records a derivative-only `vcov = FALSE` fallback when the current `marginaleffects` package fails on sub-sampled uncertainty calculations, so the benchmark reports both the incompatibility and a current runtime comparison.

## Parity-correction notes

- Missingness diagnostics intentionally retain `glm.fit: fitted probabilities numerically 0 or 1 occurred` as a discoverable warning in the full optional audit/review archive; it is not currently promoted into another code change.
- Spatial-weight and Moran's I current-vs-legacy differences are documented in `docs/refactor/spatial_diagnostics_context.md` rather than in the paper or README.
- Legacy Chunk 22 map palette/break/tmap-save exploration is out of scope for the current diagnostic/benchmarking parity correction.
- Legacy Chunk 3 long-path and 8.3 filename troubleshooting comments are represented in `analysis/io/long-paths-and-8-3-filenames.qmd`.

## Analysis note coverage added after rendered-note review

- Legacy Chunk 8 is rendered in `analysis/diagnostics/missingness-diagnostics.qmd`, with current missingness-count, regional-missingness, and logit-summary tables from `outputs/diagnostics/extended/missingness/`.
- Legacy Chunk 15 is rendered in `analysis/exploratory/instrument-exploration.qmd`, with current IV-panel diagnostic tables from `outputs/diagnostics/`.
- Legacy Chunk 20 is rendered in `analysis/diagnostics/district-matching-diagnostics.qmd`, with current unmatched-row, source-key-inventory, key-comparison, and many-to-many diagnostics from `outputs/diagnostics/extended/district_matching/`.
- Legacy Chunk 6 is rendered without the earlier line-220 truncation so the same-name-district summary comments are included.  The expected 6--10 same-name-district benchmark is now emitted as `tracker_legacy_expected_same_name_districts.csv`.
- Rendered analysis prose uses legacy comments as the source of truth.  Code-like commented blocks are fenced for readability and to prevent Quarto/GFM parsing artifacts; prose deviations are limited to explicit deviation notes immediately adjacent to current target-backed outputs.

## Current-vs-legacy reconciliation status

The goal of these diagnostics is methodological parity with the legacy checks, not forcing identical numeric output when the active cleaned inputs differ.  The following differences are therefore handled explicitly rather than hidden:

- Missingness regional rankings fall back to state-level rankings when the cleaned selection data no longer expose `region_0708`.  This preserves the legacy "where are misses concentrated?" diagnostic instead of writing empty regional CSVs.
- District tracker state-change, in-period district-name-change, and same-name-district outputs read the active tracker and, when needed, the processed tracker files.  If the cleaned tracker has resolved an ambiguity that was visible in raw legacy comments, the legacy expected rows remain in explicit reference CSVs for comparison.
- District matching separates true unmatched rows from fallback source-key inventory rows and emits key-role counts so source-key inventory is not misinterpreted as failed final-panel matches.
- Fuzzy-matching benchmarks expand from the nine hand-picked legacy examples to tracker transition pairs and fallback source-key inventory candidate pairs whenever those active inputs are present.
- AME benchmarking now samples the fitted model frame and its explicit AME weights, matching the production AME path.  If the current `marginaleffects` version still fails, the failure is retained as a package-compatibility result rather than relabeled as a successful timing benchmark.
- Spatial-weight and Moran's I differences remain documented in `docs/refactor/spatial_diagnostics_context.md`; the paper reports the active rook-based results and preserves queen comparisons in diagnostics, not in paper prose.
- Spatial-IV attempts remain opt-in benchmarks.  Returning an `ivreg()` object is not treated as methodological success unless coefficient, clustered-SE, and diagnostics outputs are also numerically meaningful.
