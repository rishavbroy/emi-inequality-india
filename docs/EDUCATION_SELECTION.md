# Education selection

## Purpose

The education-selection analysis separates school enrollment from EMI exposure conditional on enrollment using the NSS 2007-08 education microdata.

## Data and sample

The selection sample is constructed from the registered education microdata with the declared age/eligibility restrictions and survey-design fields. Missingness diagnostics are run separately from the outcome model so sample attrition is visible rather than folded into the probit specification.

## Model

The preferred model is a survey-weighted probit for enrollment. Model covariates, factor handling, weights, strata, PSUs, and the lonely-PSU convention are methodological inputs and should be declared centrally rather than inferred from whichever columns happen to be present.

Final mode should fail when the required survey design cannot be constructed; it should not change the estimator to an ordinary unweighted probit as a convenience fallback.

## Average marginal effects

Paper-facing AMEs are evaluated on the fitted model's estimation sample and with the corresponding survey weights. The preferred implementation should use one standard, verified marginal-effects method. If that method fails, final mode should report the failure rather than silently substitute a different approximation. Any custom delta-method calculation must include the full derivative of the nonlinear estimand with respect to the coefficient vector and use the full covariance matrix.

`config/fast.yml` may omit expensive paper quantities for iteration, but it should not redefine the final estimand.

## Missingness analysis

Missingness diagnostics compare included/excluded observations and identify which required fields drive sample loss. They are descriptive diagnostics, not another selection correction.

## Validation

Tests should protect the estimation sample, survey-design use, model formula, AME weighting/sample identity, and failure behavior. They should not freeze helper placement or explanatory prose.

## Outputs

Selection-model and AME tables are produced through the shared output layer. Missingness diagnostics are retained under the diagnostic output tree and may be included in the appendix where declared.

## Implementation

- `R/selection/build_selection_data.R` — estimation sample and survey fields.
- `R/selection/estimate_selection_probit.R` — preferred probit specification and fit.
- `R/selection/compute_average_marginal_effects.R` — AMEs.
- `R/selection/diagnose_missingness.R` — attrition/missingness diagnostics.

## Related documentation

- [`EMI_MEASUREMENT.md`](EMI_MEASUREMENT.md)
- [`../tests/README.md`](../tests/README.md)
