# DISE/UDISE treatment measures

## Purpose

DISE/UDISE provides administrative measures of EMI enrollment/provision and school quality used to validate and extend the household-survey schooling measures.

## Raw and report-card sources

The active inputs include historical district report-card workbooks/PDFs and the registered administrative extracts preserved under the DISE archival source paths. Acquisition/provenance is recorded in the metadata inventory; raw archival files remain local where redistribution is uncertain.

## Medium classification

English-medium and non-English-medium categories are classified from the published administrative fields using centralized code/label rules. Unknown/missing categories remain explicit rather than being assigned by name similarity.

## Baseline measures

Baseline administrative measures summarize EMI enrollment/provision on the source district/year support needed for paper validation and first-stage/treatment sensitivity. Denominators are constructed from the appropriate total enrollment/school universe for each series.

## Longitudinal measures

Longitudinal EMI enrollment ratios are constructed only where numerator and denominator series are comparable across years. The code preserves the fact that some administrative ratios begin later or have different coverage rather than forcing a balanced panel through imputation.

## School-quality measures

Registered school-quality outcomes describe administrative resources/conditions that can be compared with EMI provision. They are mechanism/context measures, not components of the preferred EMI treatment unless separately declared.

## Missingness and repair policy

The 2010-11 denominator problem is handled by the documented source/repair rule and validated against report-card evidence. Repairs are deterministic, source-based corrections; they are not inferred from downstream model fit. Diagnostic caches remain separate from strict publication inputs.

## Geography

Administrative district counts are harmonized to Census-2001 geography only through the reviewed lineage rules. Child-district aggregation is allowed only when complete deterministic ancestry supports pooling. Ratios are formed after count aggregation.

## Validation

Validation includes report-card/workbook reconciliation, numerator/denominator accounting, medium-category coverage, expected year support, source-page provenance for repaired series, and lineage completeness.

## Paper-facing role

DISE/UDISE supplies administrative validation of EMI access/provision and finite alternative treatment definitions. Candidate treatment constructs enter the shared IV design registry; they are not ranked by which produces the strongest first stage.

## Outputs

Main validation exhibits and registered treatment-definition diagnostics are retained under the paper/diagnostic output directories. Weak-IV diagnostic outputs remain compact rather than persisting every pointwise grid for every treatment candidate.

## Limitations

Administrative enrollment/provision is not identical to household-reported EMI exposure. Coverage and denominator definitions vary by year/source, so cross-series comparisons use only declared comparable measures.

## Implementation

Readers/builders live in the DISE I/O/measure modules, with shared report-generation helpers in `scripts/` and treatment construction declared through the analysis registries.

## Related documentation

- [`EMI_MEASUREMENT.md`](EMI_MEASUREMENT.md)
- [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md)
- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
