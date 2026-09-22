# Education selection

The education-selection analysis studies enrollment using the NSS 2007-08 education microdata. The implementation is split across four source modules:

- `R/selection/build_selection_data.R` constructs the estimation sample and survey variables.
- `R/selection/estimate_selection_probit.R` defines the probit covariates and fits the survey-weighted enrollment model.
- `R/selection/compute_average_marginal_effects.R` computes average marginal effects on the model's estimation sample.
- `R/selection/diagnose_missingness.R` evaluates selection-sample missingness separately from the outcome model.

The final configuration fits the survey model with the declared design information and computes full average marginal effects. `config/fast.yml` keeps the same registered model variables but returns coefficient output instead of repeating the expensive full AME calculation. Fast mode is for iteration; paper quantities are generated under `config/final.yml`.

For AMEs, the code passes the fitted model's own estimation sample to `marginaleffects::avg_slopes()` and supplies explicit weights when the fitted model exposes them. This avoids silently evaluating marginal effects on rows that the model omitted. Survey lonely-PSU handling is set locally around the calculation so a target loaded in a new R process receives the same rule used during estimation.

The main source-level checks are in `tests/testthat/test-selection.R`, `tests/testthat/test-average-marginal-effects.R`, and `tests/testthat/test-methodology-contract.R`.
