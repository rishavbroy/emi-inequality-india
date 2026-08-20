# Instrumental-variable diagnostic architecture

The extended IV diagnostics use one canonical specification layer rather than reconstructing fixed effects, controls, language adjustments, and excluded instruments inside each diagnostic.

`R/iv/specification_registry.R` defines:

- admissible fixed-effect terms;
- Census-2001 control blocks and the first-stage absorption ladder;
- the main and expanded adjustment sets;
- alternative linguistic-distance constructions;
- structural specification metadata with treatment, outcome, excluded instruments, included language controls, clustering variable, sample rule, panel variant, and diagnostic tier;
- a de-duplicated diagnostic specification registry combining the alternative-instrument grid with the additional absorption/control-block specifications;
- a diagnostic registry and saved applicability relation.

The registry is intentionally not the Cartesian product of every imaginable project option. It contains theoretically motivated designs already used by the project. Specifications that are algebraically identical are de-duplicated before the general diagnostic suite runs.

The `cluster` field is part of that self-describing specification contract. Registry-driven relevance, balance, reduced-form, and Anderson-Rubin inference use the declared cluster variable directly rather than substituting a hard-coded state column. The current registry declares `state_code_2001` throughout, so this is a structural invariant rather than a change to the preferred inference.

## Diagnostic families

The current registry distinguishes:

- **relevance**: joint excluded-instrument tests, individual first-stage coefficients, and partial R-squared;
- **independence evidence**: specification-matched covariate balance plus an omnibus holdout-covariate balance test;
- **weak-identification-robust inference**: Anderson-Rubin tests and inverted grids for structural IV specifications;
- **monotonicity evidence**: residualized scalar first-stage shape, isotonic fit, binned means, and state-specific slopes;
- **overidentification**: the standard Sargan overidentifying-restrictions diagnostic for specifications with more excluded instruments than endogenous regressors.

Applicability and implementation are separate fields. `will_run` is true only when a diagnostic is methodologically applicable and implemented. Scalar first-stage shape is explicitly inapplicable to multi-instrument constructions rather than being silently coerced into a one-dimensional ordering.

Balance is evidence about the independence argument, not a separate IV identifying assumption. Exogeneity and exclusion are not directly testable from observed data; historical balance, placebo outcomes, migration, geography, and related exercises provide cumulative evidence rather than proof.

## Relevance and specification contract

The alternative linguistic-distance grid and the richer first-stage absorption ladder now obtain their specification metadata from the same IV registry layer. Historical output files are retained where useful for compatibility, but fixed effects, controls, instrument sets, and control-block definitions are no longer independently declared inside the two diagnostic modules.

The de-duplicated `iv_specification_registry.csv` records the general diagnostic universe. `iv_diagnostic_registry.csv` records diagnostic capabilities, and `iv_diagnostic_applicability.csv` records which specification-diagnostic pairs run and why others do not.

## Balance contract

For each registered specification and predetermined Census variable, the covariate-level balance diagnostic uses the same fixed effects, language controls, and nuisance controls as the IV specification, except that the variable being tested is removed from its own nuisance-control set. Controls linked to the tested variable by an exact Census accounting identity are removed as well. For example, agricultural-worker share equals cultivator share plus agricultural-labourer share when all three use workers as the denominator, so the component shares are not conditioned on when agricultural-worker share is the balance outcome.

The omnibus balance diagnostic asks whether predetermined covariates that are *not already conditioned on by the specification* jointly predict each excluded instrument, conditional on the specification's fixed effects, included language controls, and remaining nuisance controls. This avoids mechanically "testing" covariates that the specification has already partialled out. The test uses the same state-clustered Wald machinery as the other linear diagnostics.

`instrument_balance.csv` contains the conditional specification-by-variable diagnostics and jointly tests all excluded instruments for each tested covariate. `instrument_balance_joint.csv` contains the complementary omnibus reverse-regression test for scalar instruments: the instrument is regressed on all predetermined holdout covariates and the holdout coefficients are tested jointly with state-clustered covariance. Multi-instrument specifications remain covered by the covariate-by-covariate joint-instrument tests; no scalar reverse-regression omnibus is reported for them.

## Anderson-Rubin contract

Anderson-Rubin inference is implemented once in `R/iv/weak_identification.R` and applied to the de-duplicated structural diagnostic registry. Historical alternative-distance filenames are retained for compatibility, but the implementation is not tied to a single linguistic-distance construction.

The grid inversion is a numerical summary of the acceptance region over the recorded search range. Truncation flags identify cases where the accepted set reaches either edge of the grid; those should not be read as finite confidence-set endpoints.

The preferred state-FE/main-control specification is also saved to `outputs/diagnostics/public/anderson_rubin_preferred.csv`, so weak-identification-robust inference for the headline design is part of the strict public build rather than only an extended diagnostic. The full specification grid remains extended-only.

The AR confidence set is obtained by inverting the clustered AR test over the saved beta grid. Because weak-IV confidence sets can be disconnected or extend beyond the search grid, `ar_95_lower` and `ar_95_upper` are populated only when the accepted grid points form one bounded interior component. Otherwise those interval fields are `NA`, while `ar_95_n_components`, `ar_95_disconnected`, `ar_95_contains_zero`, the grid-edge truncation flags, and `ar_95_components` describe the observed acceptance set. A grid-edge flag means that the corresponding endpoint is unresolved by the finite search grid; it must not be read as a confidence bound.

## Overidentifying-restrictions contract

Overidentified specifications use the Sargan statistic provided by `summary.ivreg(..., diagnostics = TRUE)`. The statistic is reported as an **overidentifying-restrictions diagnostic**, not as proof that the instruments are exogenous.

The Sargan statistic is the conventional homoskedastic diagnostic and is not state-cluster-robust or weak-identification-robust. That limitation is material in this project because several first stages are weak. Results therefore belong in the extended validity evidence alongside first-stage strength and Anderson-Rubin inference, not as a pass/fail validity gate. Exactly identified specifications are marked `not_applicable`.

## Monotonicity / first-stage-shape contract

Counterfactual monotonicity cannot be observed directly in this continuous district-level design. For scalar instruments the code therefore reports empirical implications of a monotone first stage rather than claiming to test the LATE assumption itself.

For each applicable specification the diagnostic:

1. residualizes the scalar excluded instrument and EMI exposure on the specification's included language controls, Census controls, and fixed effects;
2. reports the residualized linear slope and Spearman rank correlation;
3. fits base R's increasing isotonic regression and reports its fit relative to a constant;
4. reports equal-count binned residualized first-stage means and the share of adjacent bin changes that are non-decreasing;
5. reports state-specific residualized slopes where there is enough within-state variation.

The multi-instrument distance-share constructions do not have a unique scalar ordering, so this shape diagnostic is marked inapplicable to them. Their individual and joint first-stage coefficients remain in the relevance outputs, and the language-decomposition/leave-one-language-out diagnostics continue to provide instrument-composition evidence.

These shape summaries are diagnostics for plausibility. Negative local slopes or non-monotone bins can challenge a simple monotone-response story, but noisy signs do not identify latent "defiers."

## Public multicollinearity contract

Public multicollinearity diagnostics operate on the structural-regressor matrix. The condition number excludes the intercept and standardizes nonconstant regressors before calculation, so it is invariant to arbitrary regressor units. Term-level VIF/GVIF diagnostics use `car::vif()` on the fitted `ivreg` object after explicitly loading the `ivreg` namespace so its S3 covariance methods are registered even when a cached fitted model is read in a fresh targets process. The final-output audit requires finite, estimated GVIF diagnostics rather than silently accepting an unavailable diagnostic.
