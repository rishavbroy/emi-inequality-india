# EMI measurement

## Purpose

This module defines the household/student and administrative measures of English-medium instruction (EMI), schooling access, and related language-acquisition outcomes used in the paper.

## Role in the paper

NSS measures household-reported schooling and social-group access. DISE/UDISE measures school-reported enrollment, provision, and school conditions. Census C-17 provides a related language-acquisition outcome. Together they describe distinct parts of EMI access and English acquisition.

## NSS education exposure

The NSS education data identify school enrollment and medium of instruction for the supported population/round. The preferred EMI treatment and any intensive-margin variants are declared in the analysis registry, including denominator and age/sample restrictions.

## Selection and access

Enrollment selection is modeled separately from EMI conditional on enrollment. Social-group comparisons use survey weights and the declared common sample/denominator. The selection/AME model is documented in [`EDUCATION_SELECTION.md`](EDUCATION_SELECTION.md).

## DISE/UDISE administrative measures

Administrative measures include baseline English-medium enrollment/provision, longitudinal enrollment, school-quality mechanisms, and the validated 2010-11/report-card series. Source and repair rules are documented in [`DISE_TREATMENTS.md`](DISE_TREATMENTS.md).

## Intensive-margin variants

Alternative EMI treatments are predeclared variants such as conditional EMI shares or school-supply measures where the data support them. Each variant answers a distinct measurement question.

## Census C-17 language acquisition

2001 Census C-17 measures provide a related outcome for English acquisition and are analyzed separately from administrative EMI enrollment.

## Geography and denominators

Every measure records its native geography and substantive denominator before harmonization. Ratios are constructed from pooled counts after geographic aggregation when the source permits deterministic aggregation. Aggregate survey-frame geography is never relabeled as a district.

## Validation

Validation includes denominator/accounting checks, medium-category coverage, survey-design support, administrative source reconciliation, and explicit missingness/repair rules for problematic DISE years.

## Outputs

EMI measures feed the main access tables and figures, selection analysis, first-stage and IV specifications, social-group access results, and historical and school-administration validation.

## Interpretation and limits

EMI enrollment, EMI school supply, and English acquisition are related but distinct constructs. The code and paper should preserve those distinctions in variable names, tables, and causal interpretation.

## Implementation

Relevant code is under education/selection/DISE modules, `R/measures/`, `R/io/`, and the shared analysis registries. Reviewed medium/source metadata live under `data/metadata/`.

## Related documentation

- [`DISE_TREATMENTS.md`](DISE_TREATMENTS.md)
- [`EDUCATION_SELECTION.md`](EDUCATION_SELECTION.md)
- [`LINGUISTIC_DISTANCE.md`](LINGUISTIC_DISTANCE.md)
