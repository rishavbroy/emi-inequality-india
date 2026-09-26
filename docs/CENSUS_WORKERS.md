# Census worker structure

## Purpose

The worker module uses 2001 Census tables for predetermined industrial/occupational validation and Census-2011 tables for post-treatment local-development mechanisms.

## Tables used

The 2001 family uses B-04, B-25, and B-26. The 2011 family uses B-04, B-06, B-25A, and B-25B. Exact table files and acquisition status are declared in the manifest.

## Geography

2001 Census outcomes are native to the reference 593 districts. Census-2011 counts are pooled to 2001 Census parents only through complete deterministic lineage; shares are formed after count pooling.

## Constructed measures

The module constructs registered industrial and occupational shares and counts from the published tables. B-25/B-26 occupation measures use their published worker denominator.

## Accounting and validation

B-25 provides an independent check on compatible B-26 main-worker occupation counts in 2001. In 2011, B-25 universes are checked against B-04/B-06 where definitions overlap. Published totals and mutually exclusive categories must reconcile before harmonization.

## Longitudinal comparability

Only concepts with comparable worker denominators are used for changes over time. Differences in Census occupation and industry classification remain explicit.

## Inferential role

The 2001 worker block supplies predetermined balance evidence. The 2011 worker block supplies later industrial and occupational outcomes through the shared inference specifications.

## Outputs

Validated worker measures and registered summaries are retained under the corresponding validation outputs.

## Limitations

Census worker measures describe resident workers, while Economic Census measures describe jobs located at establishments. The two therefore measure different populations.

## Implementation

Readers are in the Census worker I/O modules; construction/harmonization uses shared count-pooling helpers and the common post-treatment inference layer.

## Related documentation

- [`ECONOMIC_CENSUS.md`](ECONOMIC_CENSUS.md)
- [`LABOR_MARKET.md`](LABOR_MARKET.md)
