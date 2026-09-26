# Historical baseline validation

## Purpose

This module evaluates whether the preferred linguistic-distance/instrument constructions are related to predetermined district characteristics and whether historical source choices materially alter that assessment.

## Role in the paper

These analyses are identification diagnostics and historical validation. They do not turn balance on observed covariates into proof of the exclusion restriction.

## Official Census-1991 baseline

The primary historical baseline is constructed from registered official Census-1991 tables where the source/concept can be matched to the desired predetermined characteristics. Source-specific readers validate district identities, totals, and table accounting before any harmonization.

## PCA controls

A compact PCA summary is constructed only from the registered predetermined baseline variables and declared support. PCA is used to summarize a multidimensional historical baseline; the underlying source variables remain available for balance interpretation and validation.

## Vanneman comparison

Vanneman historical district data provide an independent benchmark and alternative concept-matched baseline. Comparisons distinguish source differences from geography differences by using the registered Vanneman and harmonized-geography variants rather than collapsing them into one label.

## Source-count contracts

Each historical source family has explicit expected coverage and key uniqueness. Exact-required source comparisons fail when a required district/key or accounting identity is missing. Predeclared non-fatal source discrepancies remain visible as diagnostics rather than being silently corrected.

## External benchmarks

External historical linguistic and socioeconomic references are used to check whether the constructed baseline behaves plausibly on overlapping support. They are validation anchors, not additional controls automatically inserted into the preferred model.

## Pre-treatment trends

Where repeated historical outcomes exist, pre-treatment trend analysis is used to examine whether the preferred distance/instrument predicts differential changes before the EMI exposure period. Geography is held to a declared comparable support for each trend test.

## Balance and robustness families

Historical balance/adjustment families are finite and concept-driven. Compact Census-2001 adjustment, historical PCA, Vanneman adjustment, and related registered variants answer distinct sensitivity questions. They should not be ranked by whichever produces the strongest modern first stage or outcome coefficient.

## Validation

The module checks official-source accounting, cross-source district overlap, geography support, PCA input completeness, consistency of historical variable direction/units, and reproducibility of the registered comparison cells.

## Interpretation and limits

Observed historical balance can rule out some confounding stories but cannot establish independence from unobserved historical conditions. Pre-trend evidence is likewise diagnostic rather than a direct test of every exclusion pathway.

## Implementation

Relevant code is under `R/diagnostics/` historical-baseline/Vanneman modules and historical metadata under `data/metadata/`.

## Related documentation

- [`HISTORICAL_LANGUAGE_DATA.md`](HISTORICAL_LANGUAGE_DATA.md)
- [`HISTORICAL_GEOGRAPHY.md`](HISTORICAL_GEOGRAPHY.md)
- [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md)
