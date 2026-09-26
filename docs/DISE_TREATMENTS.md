# DISE/UDISE treatment measures

## Purpose

DISE/UDISE provides administrative measures of EMI enrollment/provision and school quality used to validate and extend the household-survey schooling measures.

## Raw and report-card sources

The active inputs include historical district report-card workbooks/PDFs and the registered administrative extracts preserved under the DISE archival source paths. Acquisition details and source records are stored in the metadata inventory; raw archival files remain local where redistribution is uncertain.

## Medium classification

English-medium and non-English-medium categories are classified from the published fields using centralized code and label rules. Unknown or missing categories remain explicit; name similarity is used only where the documented classification rules allow it.

## Baseline measures

Baseline administrative measures summarize EMI enrollment and provision on the district-year support used for paper validation and treatment sensitivity. Denominators use the corresponding total enrollment or school count for each series.

## Longitudinal measures

Longitudinal EMI enrollment ratios are constructed where numerator and denominator series are comparable across years. Series with later starts or narrower coverage retain their observed support.

## School-quality measures

Registered school-resource and school-condition outcomes are used to study how EMI provision varies with the local schooling environment. The preferred EMI treatment is defined separately.

## Missingness and repair policy

The 2010-11 denominator problem is handled by the documented source/repair rule and validated against report card evidence. Repairs follow documented fields and reviewed source evidence. Cached validation results remain separate from the publication inputs.

## Geography

Administrative district counts are harmonized to 2001 Census geography only through the reviewed lineage rules. Child-district aggregation is allowed only when complete deterministic ancestry supports pooling. Ratios are formed after count aggregation.

## Validation

Validation reconciles report cards with workbooks, checks numerator and denominator accounting, verifies medium-category and year coverage, records the published page used for repaired series, and checks lineage completeness.

## Paper-facing role

DISE/UDISE supplies administrative validation of EMI access/provision and finite alternative treatment definitions. Candidate treatment constructs enter the shared IV design registry; they are not ranked by which produces the strongest first stage.

## Outputs

Main validation results and registered treatment-definition checks are retained under the paper and validation output directories. Weak-IV validation outputs retain the summaries needed for review instead of every pointwise grid for every treatment candidate.

## Limitations

DISE records school-reported enrollment and provision, while NSS records household-reported schooling. Coverage and denominator definitions vary by year/source, so cross-series comparisons use only declared comparable measures.

## Implementation

The relevant code is in the DISE input and measure modules, with shared report-generation helpers in `scripts/` and treatment construction declared in the analysis registries.

## Related documentation

- [`EMI_MEASUREMENT.md`](EMI_MEASUREMENT.md)
- [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md)
- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
