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
