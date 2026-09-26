# IV diagnostics and weak identification

## Purpose

This document describes the registered instrumental-variable specifications and the diagnostics used to evaluate relevance, observed-balance evidence, weak identification, monotonicity, and overidentifying restrictions. The specification layer is centralized so diagnostics do not independently reconstruct controls, fixed effects, instruments, language adjustments, clustering, or sample rules.

## Preferred IV specification

Each IV row is self-describing: outcome, endogenous treatment, excluded instrument construction, included language adjustments, controls, fixed effects, clustering variable, sample rule, panel/geography variant, and diagnostic tier are declared in the shared IV/analysis-design registries. `state_code_2001` is the current cluster variable for the registered paper designs.

The linguistic-distance measure and included language-composition adjustments are separate registries. Stable construction IDs name only the admissible pairings used by the project; they are compatibility identifiers, not a license to cross every measure and adjustment mechanically.

## Candidate-design governance

Scientific questions and execution cells are distinct. A named question may reduce to the same formula/sample as an existing cell. The candidate registry retains the semantic question, the execution registry de-duplicates identical designs, and the alias relation records which questions share an execution. This preserves reviewer visibility without fitting duplicate regressions.

New candidates should enter because they represent a distinct identification or measurement question. They should not be added merely because another variable can be crossed into the grid or because a candidate produces a stronger first stage.

## First-stage diagnostics

Registered relevance diagnostics include the excluded-instrument first-stage Wald statistic, individual first-stage coefficients, partial R-squared, and Montiel Olea--Pflueger effective F where the structural IV model makes that statistic applicable. The effective-F calculation uses `momentfit::MOPtest()` on the fitted structural specification/sample; it is not invented from an outcome-free first-stage-only comparison.

The project reports the MOP statistic and the ordinary clustered first-stage statistic side by side because their covariance conventions need not coincide. A threshold is diagnostic evidence, not a rule for choosing among instrument constructions.

## Weak-identification-robust inference

Anderson--Rubin inference is the principal weak-identification-robust safeguard for structural IV rows. The shared implementation records the beta-zero test and inverted confidence-set topology. Confidence sets are interpreted from their accepted components rather than only from one p-value.

Raw pointwise AR grids are retained only for compact predeclared analyses where the acceptance path is useful for review. Broad alternative-design families persist compact summaries to avoid writing large redundant grids.

Bounded exclusion-restriction sensitivity is a separate imperfect-IV analysis. Because that procedure contains custom econometric machinery, its derivation and finite-sample inference must remain explicit and tested; it should not be treated as interchangeable with ordinary AR inference.

## Observed-balance and exclusion evidence

Specification-matched covariate balance and omnibus holdout balance provide evidence about observed independence. Historical controls, placebo-style outcomes, migration, geography, and other diagnostics add evidence about possible exclusion pathways. None of these directly proves exogeneity or exclusion.

Overidentification uses the standard Sargan diagnostic only when the number of excluded instruments exceeds the number of endogenous regressors. Applicability is recorded explicitly rather than forcing a diagnostic onto an inapplicable design.

## Monotonicity evidence

Scalar first-stage shape diagnostics use residualized first-stage variation, binned means, isotonic fit, and state-specific slopes. They are inapplicable to genuinely multi-instrument constructions unless a defensible scalar ordering is separately declared.

## Spatial/model diagnostics

Residual spatial-autocorrelation checks are documented in [`SPATIAL_ANALYSIS.md`](SPATIAL_ANALYSIS.md). The experimental spatial-IV estimator remains outside the preferred specification because spatial lags introduce additional identifying assumptions.

## Robustness families

The active IV design families are finite and registry driven:

| Family | Question |
|---|---|
| Alternative linguistic distance | Does relevance/identification depend on the declared distance construction? |
| Absorption/control blocks | Which predeclared control blocks attenuate or preserve the first stage? |
| Control parameterization | Do conceptually equivalent proxy choices alter inference? |
| Treatment definitions | Does the result depend on the registered EMI exposure margin? |
| Historical adjustment | Does concept-matched predetermined adjustment alter the preferred result? |
| Outcome/welfare variants | Does the conclusion depend on the declared consumption outcome? |

The registries own exact cells; this table describes scientific purpose only.

## Publication outputs

Paper-facing outputs report preferred/candidate first-stage evidence, weak-identification-robust inference, and compact identification summaries. Extended diagnostic files retain broader candidate and balance families for review without promoting every fitted model into the paper.

## Interpretation limits

Weak relevance cannot be repaired by adding many controls, selecting the strongest ex post instrument, or interpreting conventional 2SLS t statistics as reliable. Balance and overidentification tests are evidence, not proofs of the exclusion restriction. Post-treatment mechanism IV results remain especially sensitive to relevance and should be presented with their weak-identification diagnostics.

## Implementation

The specification registries and shared inference helpers are under `R/iv/` and the cross-family analysis-design layer. Candidate/balance/robustness execution is under `R/diagnostics/`. Paper formatting belongs under `R/output/`.

## Related documentation

- [`LINGUISTIC_DISTANCE.md`](LINGUISTIC_DISTANCE.md)
- [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md)
- [`HISTORICAL_BASELINE_VALIDATION.md`](HISTORICAL_BASELINE_VALIDATION.md)
- [`SPATIAL_ANALYSIS.md`](SPATIAL_ANALYSIS.md)
- [pre-rewrite IV notes](../archive/research-log/documentation-before-2026-09-rewrite/docs/IV_DIAGNOSTICS.md)
