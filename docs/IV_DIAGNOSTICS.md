# Instrumental-variable diagnostic architecture

The extended IV diagnostics use one canonical specification registry rather than reconstructing fixed effects, controls, language adjustments, and excluded instruments inside each diagnostic.

`R/iv/specification_registry.R` defines:

- the admissible fixed-effect terms;
- the main and expanded Census-2001 adjustment sets;
- the alternative linguistic-distance constructions;
- a specification registry with treatment, outcome, excluded instruments, included language controls, clustering variable, and diagnostic tier;
- a diagnostic registry and a saved applicability relation.

The registry is intentionally not the Cartesian product of every imaginable project option. It contains the theoretically motivated adjustment and instrument constructions used by the current extended identification analysis.

## Diagnostic families

The current registry distinguishes:

- **relevance**: joint excluded-instrument tests and partial R-squared;
- **independence evidence**: specification-matched balance regressions for predetermined Census controls; an omnibus joint-balance test is recorded as pending rather than silently implied;
- **weak-identification-robust inference**: Anderson-Rubin tests and inverted grids for structural IV specifications;
- **monotonicity**: applicability is recorded, while shape diagnostics remain pending;
- **overidentification**: applicability is recorded when excluded instruments outnumber endogenous regressors, while a clustered robust test remains pending.

Applicability and implementation are separate fields. `will_run` is true only when a diagnostic is both methodologically applicable and implemented, so a missing diagnostic is visible rather than silently skipped.

Balance is evidence about the independence argument, not a separate IV identifying assumption. Exogeneity and exclusion are not directly testable from observed data; historical balance, placebo outcomes, migration, geography, and related exercises provide cumulative evidence rather than proof.

## Balance contract

For each registered specification and predetermined Census variable, the balance diagnostic uses the same fixed effects, language controls, and nuisance controls as the IV specification, except that the variable being tested is removed from its own nuisance-control set. Excluded instruments are tested jointly with state-clustered covariance. Scalar-instrument specifications additionally report a standardized partial association.

The generated `instrument_balance.csv` is therefore a conditional specification-by-variable diagnostic, not a table of national raw correlations. An omnibus test of all balance covariates is not yet implemented and is marked explicitly in the applicability registry.

## Anderson-Rubin contract

Anderson-Rubin inference is implemented once in `R/iv/weak_identification.R` and applied to all registered structural IV specifications. The alternative-distance outputs retain their historical filenames for compatibility, but the implementation is no longer tied to a single linguistic-distance diagnostic.

## Overidentification limitation

The applicability registry marks multi-instrument specifications as overidentified, but the project does not yet report a robust overidentifying-restrictions statistic. The built-in `ivreg` Sargan diagnostic is not used as a substitute because the production design relies on clustered inference and currently weak first stages. A future implementation should select and document an overidentification procedure whose covariance assumptions match the production design.

## Monotonicity limitation

Monotonicity/first-stage-shape diagnostics are not yet centralized. Planned diagnostics include residualized first-stage shape plots, state-specific slopes, ordered distance-share responses, and theoretically motivated instrument-subset checks. These are diagnostics for the plausibility of monotone treatment response; they cannot observe counterfactual district treatment responses directly.
