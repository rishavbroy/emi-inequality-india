# Census housing and living-standard diagnostics

## Scope

The first housing phase builds a longitudinal district diagnostic from three direct conceptual pairs:

- Census 2001 H-09 and Census 2011 HL-07: main source of lighting;
- Census 2001 H-12 and Census 2011 HL-11: household electricity and latrine availability within the drinking-water table;
- Census 2001 H-13 and Census 2011 HL-12: banking services and specified household assets.

The module is an extended diagnostic. It does not add post-treatment housing variables to the preferred IV control set.

## Geographic contract

Census 2001 measures are read directly on the 593-district Census-2001 geography. Census 2011 district counts are mapped backward only through `build_complete_deterministic_transition_2011_to_2001()`. A 2001 parent is retained only when every contributing 2011 district is wholly and deterministically assigned to that parent. Counts are summed before any share is computed.

This is the same administrative-count rule used by the migration and worker modules. Fractional or incomplete 2011 reconstructions are withheld rather than interpreted as complete household totals.

## Source accounting

The readers enforce the published household universes before harmonization.

For H-09/HL-07, the mutually exclusive lighting categories must sum exactly to total households. For H-12, electricity availability and latrine availability each form their own exhaustive binary partition. HL-11 publishes the four joint electricity-by-latrine cells; the reader checks that the four cells exhaust households and then obtains the electricity and latrine marginals by summation.

Within each census year, the module additionally checks:

1. H-09/HL-07 household totals against H-12/HL-11 totals district by district;
2. the lighting-table electricity count against the utility-table electricity count district by district;
3. H-09/HL-07 household totals against H-13/HL-12 totals district by district.

The attached official 2001 files reveal a source-coverage exception: H-09 and H-13 contain all 593 Census-2001 districts, while H-12 has 587 district rows and omits Andhra Pradesh district codes 18-23 (Prakasam, Nellore, Cuddapah, Kurnool, Anantapur, and Chittoor). H-12 is therefore validated as a subset of H-09 on its 587 observed districts; it is not used to truncate the otherwise complete 2001 housing panel. Baseline latrine values, and consequently latrine changes, remain missing where H-12 is absent.

These cross-table identities are source-level parser checks, not model assumptions.

## Assets and comparability

H-13 and HL-12 asset columns are not an exhaustive partition because one household may own several assets. Each asset count is therefore validated only as a nonnegative subcount of total households; the code never adds overlapping asset counts to create an artificial total.

The longitudinal common set is deliberately narrow: banking, radio, television, broad telephone access, bicycle, motorcycle/scooter, and car/jeep/van. In 2011, broad telephone access is the sum of the mutually exclusive `landline only`, `mobile only`, and `both` cells. Computer ownership and computer-with-internet access are retained as 2011-only mechanisms because no directly comparable 2001 H-13 category exists.

Telephone access should be interpreted as a broad communications-access measure rather than a technologically identical service across censuses, because mobile telephony expanded rapidly between 2001 and 2011.

## Longitudinal estimands

For every completely reconstructed Census-2001 parent, the diagnostic reports 2001 and 2011 household shares and the arithmetic change

\[
\Delta s_d = s_{d,2011} - s_{d,2001}.
\]

The first phase reports changes for electricity, kerosene lighting, solar lighting, no lighting, latrine availability, banking, radio, television, telephone access, bicycle, motorcycle/scooter, and car/jeep/van. Changes are retained as missing when either year's underlying share is unavailable; `change_coverage.csv` reports this support outcome by variable. These are descriptive/mechanism-ready quantities; they are not automatically promoted to causal outcome regressions.

## Deferred tables

H-05/HL-04, H-08/HL-06, H-10/HL-08+HL-09, H-11/HL-10, and HL-13 remain acquisition-only in this phase. Their concepts are useful, but each requires its own denominator/category harmonization. Keeping them out of the first patch prevents same-number or superficially similar table labels from being mistaken for an already-validated longitudinal estimand.
