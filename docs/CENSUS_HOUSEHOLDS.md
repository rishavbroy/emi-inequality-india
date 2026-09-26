# Census household capacity

## Purpose

This module constructs concept-matched 2001-to-2011 changes in household literacy depth, matriculate/graduate access, and worker intensity from HH-08/HH-10/HH-11 and their 2001 counterparts.

## Tables used

The 2011 family uses HH-08, HH-10, and HH-11. The matched 2001 baseline uses the registered HH-09/HH-13/HH-15 material and independent appendix checks where available. Exact filenames and acquisition status are declared in the metadata manifest.

## Geography

Native table accounting is validated before any geography transformation. Census-2011 counts are pooled to Census-2001 parents only through complete deterministic lineage relationships. Shares are calculated after counts are pooled.

## Constructed measures

- **Literacy depth:** shares of households with none, one, two, three, or four-plus literate members, plus coarse summaries that do not assign a synthetic value to the open-ended `4+` category.
- **Matriculate/graduate access:** household shares with matriculate/graduate access using the table's substantive age-eligible denominator; overlapping sex-specific categories are never added as though mutually exclusive.
- **Worker intensity:** workerless and multi-worker household shares/intensities from the exhaustive worker-count categories.

## Accounting and validation

HH-08 and HH-11 must reconcile their household totals. HH-10's relevant partitions must reconcile with the same household universe and its own published subtotals. Worker counts and main/marginal components are checked where the table publishes both. Cross-table disagreement is surfaced before harmonization.

## Longitudinal comparability

Only concepts with defensible 2001/2011 counterparts enter changes. The module avoids imposing cardinal values on open-ended household categories or summing overlapping access categories.

## Inferential role

Household-capacity changes are descriptive co-evolving development evidence. A bounded paper-facing synthesis uses predeclared changes, but these outcomes are not preferred controls and are not interpreted as identified mediation effects.

## Outputs

Validated baseline, harmonized 2011, and change files are retained under `outputs/diagnostics/extended/census_households/` together with compact inference summaries where registered.

## Limitations

Household-capacity measures describe household composition/access, not individual schooling quality or earnings. Post-treatment changes may reflect many local-development channels.

## Implementation

Table-specific readers and measure builders live under Census I/O/measure modules; pooling uses the shared 2011-to-2001 harmonization utilities.
