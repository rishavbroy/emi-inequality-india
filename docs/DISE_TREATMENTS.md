# DISE/UDISE treatment measures

## Purpose

DISE/UDISE provides administrative measures of EMI enrollment/provision and school quality used to validate and extend the household-survey schooling measures.

## Raw and report-card inputs

The active inputs include historical district report-card workbooks/PDFs and registered DISE extracts retained with the project data. Acquisition details and file records are stored in the metadata inventory; raw archival files remain local where redistribution is uncertain.

For 2008--09 through 2014--15, `scripts/build_dise_report_language_enrollment.py` reconstructs reviewed English/Hindi enrollment counts from registered PDF pages. `pdftotext -layout` retains the page spacing used by the two supported table layouts. Row-oriented tables require a nearby `Total` header and use the final numeric entry on each English or Hindi row. Column-oriented tables locate the language headings and read the nearest numeric entry on an explicit `Total` or `Grand Total` row. When several reports cover the same district-year, the recorded report priority must select one. The script compares rebuilt rows with the tracked count table when run without `--output`.

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

### Dynamic relevance inference

The longitudinal DISE event studies cluster uncertainty by 2001 district. Because
these regressions have hundreds of district clusters and high-dimensional district
and year fixed effects, they use the conventional large-cluster HC1 covariance
from `sandwich::vcovCL()`. Coefficient tests use `lmtest::coeftest()` with
cluster degrees of freedom (`G - 1`), and joint event-study tests use
`car::linearHypothesis()` with the same denominator degrees of freedom. The CR2
small-cluster correction is reserved for the state-clustered IV specifications,
where the number of clusters is modest and the correction is substantively
important.

### Weak-IV computation in construct permutations

The extended DISE construct permutations report the weak-IV-robust Anderson--Rubin
test of `beta = 0`, but they do not report an inverted Anderson--Rubin confidence
set. The DISE archive has no consumer or persisted output for the pointwise
inversion grid, so these broad permutations skip that repeated computation.
Publication-facing analyses that report Anderson--Rubin confidence sets continue
to request full inversion with the same CR2/HTZ test convention.
