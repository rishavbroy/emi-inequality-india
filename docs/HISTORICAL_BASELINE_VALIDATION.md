# Historical baseline validation

## Purpose

This module evaluates whether the preferred linguistic-distance/instrument constructions are related to predetermined district characteristics and whether historical source choices materially alter that assessment.

## Role in the paper

These analyses examine historical balance and pre-treatment evidence relevant to the identification argument. Balance on observed covariates cannot establish the exclusion restriction.

## Official Census-1991 baseline

The primary historical baseline is constructed from registered official Census-1991 tables where the source/concept can be matched to the desired predetermined characteristics. Source-specific readers validate district identities, totals, and table accounting before any harmonization.

## PCA controls

A compact PCA summary is constructed only from the registered predetermined baseline variables and declared support. PCA is used to summarize a multidimensional historical baseline; the underlying source variables remain available for balance interpretation and validation.

## Vanneman comparison

Vanneman historical district data provide an independent benchmark and alternative concept-matched baseline. Comparisons distinguish source differences from geography differences by using the registered Vanneman and harmonized-geography variants rather than collapsing them into one label.

## Expected source coverage

Each historical source family has explicit expected coverage and key uniqueness. Exact-required comparisons stop when a required district, key, or accounting identity is missing. Predeclared non-fatal discrepancies remain visible in the validation results.

## External benchmarks

External historical linguistic and socioeconomic references are compared with the constructed baseline on overlapping districts. They provide independent checks of levels, direction, and coverage and are kept separate from the preferred model's controls.

## Pre-treatment trends

Where repeated historical outcomes exist, pre-treatment trend analysis is used to examine whether the preferred distance/instrument predicts differential changes before the EMI exposure period. Geography is held to a declared comparable support for each trend test.

## Balance and robustness families

Historical balance and adjustment specifications are predeclared according to the information they add: compact 2001 Census controls, historical PCA, Vanneman controls, and related registered variants. Their interpretation follows those substantive differences, with first-stage and outcome results reported afterward.

## Validation

The module verifies official-table accounting and district overlap, then checks geography support, PCA inputs, historical variable units and direction, and the reproducibility of registered comparisons.

## Interpretation and limits

Observed historical balance can rule out some confounding stories but cannot establish independence from unobserved historical conditions. Pre-trend evidence addresses observed pre-treatment patterns but cannot test every exclusion pathway.

## Implementation

Relevant code is under `R/diagnostics/` historical-baseline/Vanneman modules and historical metadata under `data/metadata/`.

## Related documentation

- [`HISTORICAL_LANGUAGE_DATA.md`](HISTORICAL_LANGUAGE_DATA.md)
- [`HISTORICAL_GEOGRAPHY.md`](HISTORICAL_GEOGRAPHY.md)
- [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md)
