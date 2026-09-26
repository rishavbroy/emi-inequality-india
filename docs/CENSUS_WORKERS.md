# Census worker structure

## Purpose

The worker module uses Census-2001 tables for predetermined industrial/occupational validation and Census-2011 tables for post-treatment local-development mechanisms.

## Tables used

The 2001 family uses B-04, B-25, and B-26. The 2011 family uses B-04, B-06, B-25A, and B-25B. Exact table files and acquisition status are declared in the manifest.

## Geography

Census-2001 outcomes are native to the reference 593 districts. Census-2011 counts are pooled to Census-2001 parents only through complete deterministic lineage; shares are formed after count pooling.

## Constructed measures

The module constructs registered industrial and occupational shares/counts from the source tables. B-25/B-26-type occupation measures retain their published worker universe rather than being normalized by an unrelated population denominator.

## Accounting and validation

B-25 provides an independent check on compatible B-26 main-worker occupation counts in 2001. In 2011, B-25 universes are checked against B-04/B-06 where definitions overlap. Published totals and mutually exclusive categories must reconcile before harmonization.

## Longitudinal comparability

Only concepts with comparable worker universes are used for change-style interpretation. Differences in census occupation/industry classification are kept visible rather than forced into a false exact match.

## Inferential role

The 2001 worker block supplies predetermined balance/validity evidence. The 2011 worker block supplies post-treatment industrial/occupational mechanism outcomes through the shared finite inference family. Post-treatment worker structure is not a control for the preferred causal model.

## Outputs

Validated worker measures and registered mechanism summaries are retained under the corresponding diagnostic outputs.

## Limitations

Census worker measures describe resident workers under census definitions, while Economic Census measures describe employment at establishments. They are complementary rather than interchangeable.

## Implementation

Readers are in the Census worker I/O modules; construction/harmonization uses shared count-pooling helpers and the common post-treatment inference layer.

## Related documentation

- [`ECONOMIC_CENSUS.md`](ECONOMIC_CENSUS.md)
- [`LABOR_MARKET.md`](LABOR_MARKET.md)
