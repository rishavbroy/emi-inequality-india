# Consumption analysis

## Purpose

This document defines how the paper relates schooling/EMI measures and the registered linguistic-distance instruments to later district consumption outcomes. Consumption construction itself is documented in [`CONSUMPTION_MEASUREMENT.md`](CONSUMPTION_MEASUREMENT.md); price adjustment is documented in [`PRICE_DEFLATION.md`](PRICE_DEFLATION.md).

## Role in the paper

The preferred consumption analysis is part of the paper's economic-conversion evidence. It is interpreted together with the access and inherited-linguistic-condition results. Descriptive schooling-to-welfare regressions and IV specifications answer different questions and remain labeled separately.

## Paper estimands

The active paper family uses registered district consumption outcomes and a predeclared set of EMI/schooling treatment measures. Specification rows define the outcome, treatment/endogenous variable, instrument construction, adjustment strategy, fixed effects, clustering, and sample rule. Those choices are compiled from registries rather than assembled independently inside output functions.

## Descriptive schooling-to-consumption bridge

The descriptive bridge relates observed schooling margins to later real consumption on a common district sample. It is not interpreted as causal and does not substitute for the IV design. Its purpose is to show how enrollment, EMI conditional on enrollment, and related access measures covary with later welfare.

## Preferred IV specification

The preferred consumption IV rows use the shared analysis registry and the weak-identification checks described in [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md). The final paper should report weak first-stage evidence directly and use weak-identification-robust inference where required.

## Registered robustness families

Robustness analyses are limited to predeclared families: alternative welfare outcomes, treatment definitions, control specifications, historical concept-matched adjustments, and related comparisons. Each family answers a stated scientific question.

## Common-support policy

Comparisons within a robustness family use an explicit common support rule so coefficient differences are not mechanically driven by changing samples. If multiple families intentionally share the identical support definition, that support should be computed once or the equivalence should be declared so output checks remain meaningful.

## Validation

The analysis checks estimation-sample identity, instrument/treatment availability, cluster support, weak-instrument statistics, and registered output completeness. Processed-data replication reruns the shared district-level consumption/IV results through a separate target store and compares them to the full-route outputs.

## Outputs

Main tables and figures are under `outputs/tables/main/` and `outputs/figures/main/`; appendix and validation results are stored in their corresponding directories. Output code formats registered results while the statistical specifications remain defined upstream.

## Interpretation and limits

The consumption IV design depends on instrument relevance and exclusion assumptions. Weak-identification-robust inference reports what the data support when relevance is limited. Post-treatment outcomes describe associated district changes unless a separate identifying argument is stated. Alternative welfare and control specifications show how the main results change across predeclared choices.

## Implementation

Consumption analysis code is primarily under `R/iv/`, `R/diagnostics/`, `R/consumption/`, and the consumption target modules in `R/pipeline/`. Shared design rows are defined in the analysis registry, and output formatting is under `R/output/`.

## Related documentation

- [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md)
- [`CONSUMPTION_MEASUREMENT.md`](CONSUMPTION_MEASUREMENT.md)
- [`PRICE_DEFLATION.md`](PRICE_DEFLATION.md)
- [`EDUCATION_SELECTION.md`](EDUCATION_SELECTION.md)
