# Historical language data

## Purpose

This document records the historical language evidence used to validate and contextualize the preferred linguistic-distance construction before the treatment period. Geography-specific transformation is documented in [`HISTORICAL_GEOGRAPHY.md`](HISTORICAL_GEOGRAPHY.md).

## Role in the paper

Historical language evidence is validation and predetermined-baseline support. It is not a second treatment definition chosen after observing modern outcomes.

## 1991 Language Atlas

The Census of India Language Atlas 1991 is the principal historical language source. The repository retains extraction metadata and reviewed cell decisions rather than treating OCR/extraction output as automatically authoritative. Extracted state/district/language cells enter analysis only after the registered validation and adjudication rules are satisfied.

## Extraction and review

The extraction process separates machine-readable capture from reviewed acceptance. Reviewed cells record the published label/value, any ambiguity, and the decision used downstream. The active analysis should consume the reviewed table, not a fresh unreviewed extraction.

## Historical language allocation

Historical language shares are assigned to the historical geographic units supported by the source and then transformed to the analysis geography through the registered historical-geography rules. Allocation occurs before linguistic-distance aggregation so totals and language composition can be validated on the native geography.

## Preferred and alternative language references

Historical validation uses the same conceptual language-distance family as the modern analysis: reviewed language identities, Glottolog genealogical information, Dyen/Shastry cognate-distance evidence where applicable, and the frozen preferred 0--5 mapping. Alternative references are sensitivity checks rather than competing definitions selected by first-stage strength.

## Reviewed adjudication

Ambiguous Indo-European identities and historical labels are resolved in tracked metadata ledgers. The ledger records evidence and the accepted identity; code should not contain hidden one-off string substitutions that bypass those reviewed decisions.

## Validation

Validation checks include extraction/accounting consistency, coverage of reviewed historical language cells, language-share support, agreement of alternative historical references where they overlap, and stability of the district-level historical-distance signal across declared constructions.

## Outputs

Historical language diagnostics are retained under extended diagnostic outputs and feed the baseline/pre-trend analyses described in [`HISTORICAL_BASELINE_VALIDATION.md`](HISTORICAL_BASELINE_VALIDATION.md).

## Interpretation and limits

Historical language composition is used to establish persistence/predetermination and to stress-test the modern linguistic-distance construction. It should not be interpreted as a complete census of every local language variety or as independent causal identification.

## Implementation

Relevant code is under `R/language/`, `R/io/`, historical diagnostic modules, and tracked language metadata in `data/metadata/`. The Python atlas extractor is a source-materialization tool; reviewed metadata remain the downstream authority.

## Related documentation

- [`LINGUISTIC_DISTANCE.md`](LINGUISTIC_DISTANCE.md)
- [`HISTORICAL_GEOGRAPHY.md`](HISTORICAL_GEOGRAPHY.md)
- [`HISTORICAL_BASELINE_VALIDATION.md`](HISTORICAL_BASELINE_VALIDATION.md)
