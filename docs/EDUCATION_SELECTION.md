# Education selection

## Purpose

The education-selection analysis separates school enrollment from EMI exposure conditional on enrollment using the NSS 2007-08 education microdata.

## Data and sample

The selection sample is constructed from the registered education microdata with the declared age/eligibility restrictions and survey-design fields. Missingness comparisons are run separately from the outcome model so the sources of sample attrition remain visible.

## Model

The preferred model is a survey-weighted probit for enrollment. Model covariates, factor handling, weights, strata, PSUs, and the lonely-PSU convention are methodological inputs and should be declared centrally rather than inferred from whichever columns happen to be present. NSS 64 forms rural and urban strata separately and selects four FSUs from each sub-stratum, so the analysis stratum key includes state, sector, stratum, and sub-stratum. `survey::svydesign(..., nest = TRUE)` relabels cluster identifiers to enforce nesting within those strata. Single-PSU analytic strata are therefore not treated as certainty strata. The shared survey context uses `survey.lonely.psu = "adjust"` with domain adjustment, the conservative grand-mean centering rule documented by the `survey` package.

Final mode requires the complete survey design before fitting the survey-weighted probit. The design does not supply a finite-population correction, so `survey` uses its with-replacement variance approximation.

## Average marginal effects

AMEs reported in the paper are evaluated on the fitted model frame with the corresponding survey weights. The implementation supplies that frame explicitly through `newdata` and uses the recovered sampling weights through `wts`; `marginaleffects` uses those weights when averaging unit-level quantities. Continuous regressors are summarized by average slopes, and binary and categorical regressors are summarized by discrete comparisons. Standard errors use the fitted model's covariance matrix with the package's delta-method calculation. Final mode requires this calculation to succeed.

`config/fast.yml` may omit expensive paper quantities for iteration, but it should not redefine the final estimand.

## Missingness analysis

Missingness comparisons examine included and excluded observations and identify which required fields drive sample loss. These comparisons describe how the estimation sample differs from excluded observations.

## Validation

Tests should protect the estimation sample, survey-design use, model formula, AME weighting/sample identity, and failure behavior. They should not freeze helper placement or explanatory prose.

## Outputs

Selection-model and AME tables are produced through the shared output layer. Missingness results are retained under the validation outputs and may be included in the appendix where declared.

## Implementation

- `R/selection/build_selection_data.R` — estimation sample and survey fields.
- `R/selection/estimate_selection_probit.R` — preferred probit specification and fit.
- `R/selection/compute_average_marginal_effects.R` — AMEs.
- `R/selection/diagnose_missingness.R` — attrition and missingness checks.

## Related documentation

- [`EMI_MEASUREMENT.md`](EMI_MEASUREMENT.md)
- [`../tests/README.md`](../tests/README.md)
