# Linguistic distance

## Purpose

This module defines the district-level linguistic-distance measures used to characterize inherited linguistic conditions and to construct the paper's instrument candidates.

## Role in the paper

The preferred distance is an inherited/predetermined linguistic condition. Alternative genealogical/cognate constructions are robustness and identification diagnostics. Candidate strength does not determine which construction is declared preferred.

## Language inputs

District language composition comes from the registered Census language sources and reviewed mappings. Language labels are normalized through tracked identity metadata rather than ad hoc substitutions in estimation code.

## Preferred distance

The preferred 0--5 mapping is frozen in reviewed metadata. District distance aggregates language composition against that mapping using the declared weighting rule. The mapping is treated as a substantive measurement decision and changes only with documented evidence.

## Glottolog layer

Glottolog supplies reviewed genealogical identities/relationships used both to validate language mappings and to construct an alternative distance basis. The code consumes the reviewed crosswalk rather than querying a mutable external taxonomy during estimation.

## Dyen/Shastry and cognate-distance robustness

Dyen/Shastry cognate evidence supplies an alternative Indo-European distance basis on supported languages. Coverage restrictions and reviewed adjudications remain explicit; unsupported languages are not assigned invented cognate distances.

## Historical references

Historical Ethnologue, Dyen, Vanneman, Atlas-1991, and related evidence are used for source validation and historical sensitivity. Their roles are documented in [`HISTORICAL_LANGUAGE_DATA.md`](HISTORICAL_LANGUAGE_DATA.md).

## Adjudication

Ambiguous or conflicting language identities are resolved in tracked review ledgers. Sensitivity scenarios alter only predeclared ambiguous decisions; they do not remap languages after observing first-stage or outcome estimates.

## Validation

The module checks mapping coverage, district language-share accounting, expected range/monotonicity of distance measures, cross-source agreement on overlapping languages, and reproducibility of alternative construction IDs.

## Instrument interpretation

Linguistic distance is used as a source of variation in EMI exposure only under the paper's relevance and exclusion arguments. Alternative distance measures form a finite candidate family. Weak relevance and exclusion sensitivity are handled in [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md), not by selecting the strongest ex post construction.

## Outputs

District measures feed the main paper, first-stage tables, weak-identification diagnostics, historical validation, and application-sample code excerpts. Review outputs preserve the construction ID so results from different distance definitions cannot be conflated.

## Implementation

Relevant code is under `R/language/`, measure construction modules, analysis-design registries, and language metadata under `data/metadata/`.

## Related documentation

- [`EMI_MEASUREMENT.md`](EMI_MEASUREMENT.md)
- [`HISTORICAL_LANGUAGE_DATA.md`](HISTORICAL_LANGUAGE_DATA.md)
- [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md)
