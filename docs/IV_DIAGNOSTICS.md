# IV checks and weak identification

## Purpose

This document describes the registered instrumental-variable specifications and the checks used to evaluate relevance, observed balance, weak identification, monotonicity, and overidentifying restrictions. Shared specifications keep controls, fixed effects, instruments, language adjustments, clustering, and sample rules consistent across those checks.

## Preferred IV specification

Each IV row records its outcome, endogenous treatment, excluded instrument construction, language adjustments, controls, fixed effects, clustering variable, sample rule, panel/geography variant, and validation tier in the shared IV registries. `state_code_2001` is the current cluster variable for the registered paper designs.

The linguistic-distance measure and included language-composition adjustments are separate registries. Stable construction IDs name the admissible pairings used by the project.

## Candidate-design governance

Scientific questions and execution cells are distinct. A named question may reduce to the same formula/sample as an existing cell. The candidate registry retains the substantive question, while the execution registry avoids fitting identical designs twice and records which questions share a fit. This preserves reviewer visibility without fitting duplicate regressions.

New candidates should enter because they represent a distinct identification or measurement question. They should not be added merely because another variable can be crossed into the grid or because a candidate produces a stronger first stage.

## First-stage checks

Registered relevance checks include the excluded-instrument first-stage Wald statistic, individual first-stage coefficients, partial R-squared, and Montiel Olea--Pflueger effective F where the structural IV model makes that statistic applicable. State-clustered coefficient tests use `clubSandwich` CR2 covariance with Satterthwaite degrees of freedom; joint Wald tests use its HTZ small-sample correction. Anderson--Rubin tests and bounded-exclusion sensitivity use that same HTZ reference distribution, including its effective denominator degrees of freedom, so exact exclusion is nested by construction. The effective-F calculation uses `momentfit::MOPtest()` on the fitted structural specification and sample. The conventional first-stage regression used for CR2 coefficient and joint inference is fit directly on `ivreg`'s stored instrument model matrix and fitted observation set. This makes the fitted IV object the single owner of factor expansion, interactions, transformations, contrasts, excluded-instrument columns, and complete-case selection rather than re-evaluating formulas downstream.

The project reports the MOP statistic and the CR2 first-stage statistic side by side because their covariance conventions need not coincide. Thresholds summarize instrument relevance; instrument constructions are chosen from the predeclared design.

## Weak-identification-robust inference

Anderson--Rubin inference is the principal weak-identification-robust safeguard for structural IV rows. The shared implementation always supports the beta-zero test and performs confidence-set inversion only when the caller consumes the resulting topology. Confidence sets are interpreted from their accepted components rather than only from one p-value.

Confidence-set inversion is demand-driven because it repeats the clustered test over many candidate structural effects. Publication-facing analyses that report AR confidence sets request inversion. Broad registered sensitivity families that consume only the beta-zero test skip the unused grid while retaining the same CR2/HTZ point-null inference. This includes DISE construct permutations, alternative-distance comparisons, and consumption robustness families; the headline consumption specifications still invert the set because the appendix reports its components. Standard alternatives such as `ivmodel::AR.test()` and `ivDiag::AR_test()` use different variance/reference conventions, so substituting them only for speed would change the inferential procedure.

AR accepted sets are not assumed to have a fixed topology. Reporting code accepts connected, disconnected, empty, and grid-truncated sets and checks that the stored summary agrees with the underlying acceptance grid. Acceptance at a grid edge is reported as numerical truncation rather than extrapolated to an infinite interval.

Raw pointwise AR grids are retained only for compact predeclared analyses where the acceptance path is useful for review. Broad alternative-design families persist compact summaries to avoid writing large redundant grids.

Bounded exclusion-restriction sensitivity is a separate imperfect-IV analysis. For one scalar excluded instrument, the project bounds the coefficient on a direct instrument-to-outcome path, `gamma`, while maintaining the registered controls, fixed effects, sample, and state-clustered CR2/HTZ reference distribution. At each candidate structural effect, the closest admissible `gamma` determines the union-AR test; widening the admissible interval can therefore only weakly enlarge the accepted set. Exact exclusion (`gamma = 0`) nests ordinary AR by construction. The beta grid is used only to discover accepted-set topology, and finite transition points are refined with the same `stats::uniroot()` boundary solver used for ordinary AR.

This parameterization is not interchangeable with `ivmodel::ARsens.test()`. That package implements the Wang--Jiang--Zhang--Small sensitivity model through a noncentral-F allowance for instrument invalidity under its residualized classical linear-IV reference model; its sensitivity parameter can represent bounded invalidity from direct effects and instrument--unmeasured-confounder association. The present analysis instead isolates a bounded direct-effect coefficient and retains the project's clustered CR2/HTZ inference. `ivmodel::ARsens.test()` is therefore a methodological comparator, not a drop-in implementation for the registered estimand.

## Observed-balance and exclusion evidence

Specification-matched covariate balance and omnibus holdout balance provide evidence about observed independence. Historical controls, placebo outcomes, migration, geography, and other checks add evidence about possible exclusion pathways. None of these directly proves exogeneity or exclusion.

Overidentification uses the standard Sargan test when the number of excluded instruments exceeds the number of endogenous regressors. Other designs record the test as inapplicable.

## Monotonicity evidence

Scalar first-stage shape checks use residualized first-stage variation, binned means, isotonic fit, and state-specific slopes. They are inapplicable to genuinely multi-instrument constructions unless a defensible scalar ordering is separately declared.

## Spatial and model checks

Residual spatial-autocorrelation checks are documented in [`SPATIAL_ANALYSIS.md`](SPATIAL_ANALYSIS.md). The experimental spatial-IV estimator remains outside the preferred specification because spatial lags introduce additional identifying assumptions.

## Robustness families

The active IV specifications are predeclared in the registries:

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

Paper outputs report preferred and candidate first-stage evidence, weak-identification-robust inference, and compact identification summaries. Extended validation files retain broader candidate and balance results for review.

## Interpretation limits

Weak first stages require weak-identification-robust inference; adding controls or selecting an ex post instrument does not supply relevance. Balance and overidentification tests provide evidence about observed implications of the design; the exclusion restriction still depends on the substantive identification argument. Post-treatment IV results are presented with the corresponding weak-identification evidence.

## Implementation

The specification registries and shared inference helpers are under `R/iv/`. Candidate, balance, and robustness checks are under `R/diagnostics/`, and paper formatting is under `R/output/`.

## Related documentation

- [`LINGUISTIC_DISTANCE.md`](LINGUISTIC_DISTANCE.md)
- [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md)
- [`HISTORICAL_BASELINE_VALIDATION.md`](HISTORICAL_BASELINE_VALIDATION.md)
- [`SPATIAL_ANALYSIS.md`](SPATIAL_ANALYSIS.md)
- [pre-rewrite IV notes](../archive/research-log/documentation-before-2026-09-rewrite/docs/IV_DIAGNOSTICS.md)

Interior Anderson--Rubin acceptance/rejection transitions are refined with `stats::uniroot()`, so finite confidence-set endpoints solve the AR inversion threshold rather than inheriting the nearest coarse grid point. The regular grid remains only for topology discovery; grid-edge components remain explicitly marked as search-truncated rather than extrapolated to infinity.
