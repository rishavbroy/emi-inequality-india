# Census worker structure: 2001 validity and 2011 mechanisms

This module uses Census B-series tables for two distinct roles: Census 2001 worker structure is predetermined validity evidence for the linguistic-distance design, while Census 2011 worker structure measures post-treatment local industrial and occupational mechanisms separately from migration. It is deliberately not used to fabricate the unpublished D-08/D-09 migrant-by-industry or migrant-by-occupation cells.

## Current scope

Predetermined 2001 diagnostics use:

- **B-04**: main workers by age, industrial category, sex, and residence;
- **B-25**: occupational classification of main workers other than cultivators and agricultural labourers; and
- **B-26**: occupational classification of main and marginal workers other than cultivators and agricultural labourers by age, sex, and residence.

Post-treatment 2011 diagnostics use four district-level tables:

- **B-04**: main workers by age, industrial category, sex, and residence;
- **B-06**: marginal workers by duration worked, age, industrial category, sex, and residence;
- **B-25A**: occupational classification of main workers other than cultivators and agricultural labourers; and
- **B-25B**: the corresponding occupational classification of marginal workers.

Official Census catalog descriptions state that B-04 and B-06 are available for districts and classify workers by industrial category, while B-25A/B are district-level occupational tables. The source workbooks are acquired through `data/metadata/census_2011_download_manifest.tsv` and restored with `make download-census-tables`.

Readers live in `R/io/read_census_workers.R`; count pooling and derived shares live in `R/measures/build_census_workers.R`; diagnostics are written by `R/diagnostics/diagnose_census_workers.R`.



## Census 2001 validity block

The 2001 tables are already on the analytical Census-2001 district geography, so no lineage allocation is applied. All 35 state/UT workbooks yield exactly 593 district rows.

B-04 uses the published NIC-1998 section groupings. Because the 2001 table combines some sections that are split in 2011, the validity block keeps the 2001 publication structure rather than inventing a retrospective 2001-to-2011 industry crosswalk. The exhaustive groups are agriculture; mining; manufacturing; utilities; construction; trade; accommodation/food; transport/communication; finance/real-estate/business; and public/social/other services. They must sum exactly to published main workers.

B-26 supplies both main and marginal occupation counts. Divisions 1-9 plus the published `X` unclassified division must exhaust each district's main and marginal totals exactly. B-25 independently validates the B-26 main-worker occupation counts. Unclassified workers remain in every occupation denominator.

The balance family is intentionally compact. It tests manufacturing, construction, trade, transport/communication, finance/business, public/social services, managers/professionals/technicians, clerical/service/sales, craft/machine, and elementary occupations. Agricultural composition and broad worker participation already enter the established Census-2001 control/balance architecture and are not duplicated here.

These variables are diagnostic balance outcomes, not automatic preferred controls. Balance is evaluated under the same candidate IV specification registry as the migration validity block, with Holm adjustment within specification and an omnibus joint balance test. The exercise asks whether linguistic distance already predicted detailed economic specialization in 2001; it does not claim that same-named 2001 and 2011 industrial groups are directly longitudinally comparable.

## Industrial structure

B-04 and B-06 are reduced to district, residence = Total, age = Total, persons. B-04 supplies main-worker counts and B-06 supplies marginal-worker counts. The published industrial groups are retained in transparent aggregates:

- agriculture (cultivators, agricultural labourers, and other agriculture/forestry/fishing);
- mining;
- manufacturing;
- utilities;
- construction;
- trade;
- transport;
- accommodation and food;
- information and communication;
- finance, real estate, and professional activities;
- administrative support and public administration;
- education and health; and
- other services.

Household-industry and non-household-industry columns are summed only within their published industrial section. In the 2011 layout, section H is accommodation/food and section I is transport/storage; the parser preserves that published ordering explicitly. Main and marginal counts are then added. The industrial categories must exhaust total main workers in B-04 and total marginal workers in B-06 exactly before harmonization.

Shares are computed only after count pooling. The module reports each industrial group as a share of all main plus marginal workers, together with main- and marginal-worker shares among workers.

## Occupational structure

B-25A/B are reduced to district-level top occupation divisions. The published universe excludes cultivators and agricultural labourers, so its denominator is explicitly named `workers_excl_cultivators_aglab_total`; it is not called total employment or non-agricultural employment.

Divisions 1-9 plus `X` (workers not classified by occupation) must exhaust the B-25 table total exactly. Two attached official B-25B district workbooks omit a top-level division whose count is zero; the reader therefore treats an absent division as zero only when the remaining published divisions still sum exactly to the district total. `X` remains in the denominator rather than being dropped or proportionally redistributed.

Initial mechanism shares are deliberately broad:

- managers + professionals + technicians/associate professionals (divisions 1-3);
- clerical + service/sales workers (4-5);
- skilled agricultural/fishery workers (6);
- craft workers + plant/machine operators (7-8);
- elementary occupations (9); and
- occupation not classified (X).

These are observed Census composition measures. They are not labeled "English-intensive" without a separately justified occupation-to-English-intensity mapping.

## Cross-table accounting

Two source-level identities protect the B-25 universe definition:

`B25A total = B04 main workers - B04 cultivators - B04 agricultural labourers`

and

`B25B total = B06 marginal workers - B06 cultivators - B06 agricultural labourers`.

The diagnostic build fails if district coverage differs or either identity fails. This makes the occupational parser auditable against an independent B-series table before geography harmonization.

## Geography and interpretation

All 2011 counts use the same `build_complete_deterministic_transition_2011_to_2001()` contract as C-13 and Census migration. A Census-2001 parent is retained only when every contributing 2011 district is wholly and deterministically assigned to that parent. Counts are pooled first and shares are recomputed afterward.

These variables are post-treatment local economic-structure mechanisms. They are not controls in the preferred welfare equation. The current worker branch deliberately stops at measurement plus 2001 balance diagnostics rather than running the generic Census mechanism-IV grid. Two facts make that the more conservative contract: the 2001 B-04 baseline covers **main workers**, while the 2011 industry measure combines B-04 main and B-06 marginal workers, and the existing 2001 worker-structure joint-balance tests reject strongly for the scalar instruments. A later worker-outcome model should therefore resolve the longitudinal denominator/baseline-adjustment question instead of treating a 2011 composition level as a clean causal change outcome.

They complement the D-series migration branch by separating two questions:

1. did exposure change who moved and why; and
2. did the economic structure of the district itself change?

The module does not multiply migration totals by destination-district B-series shares and does not present such a product as migrant industry or occupation.

B-01 remains acquisition-only in this phase because its broad worker-status quantities overlap existing worker controls and because the immediate missing mechanism is detailed local industrial/occupational structure. A later phase can add B-01 if worker/non-worker or marginal-work intensity becomes a registered mechanism outcome.
