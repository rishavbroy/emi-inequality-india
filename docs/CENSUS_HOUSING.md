# Census housing and living standards

## Purpose

This module constructs longitudinal 2001-to-2011 household living-standard measures from concept-matched Census housing/houselisting tables and validates the published household universes before geographic harmonization.

## Tables used

The active matched families include the registered H-04/H-05/H-08/H-09/H-10/H-11/H-12/H-13 sources and their 2011 HL counterparts. Exact table IDs and required/optional status are recorded in `data/metadata/file_manifest.csv`.

## Geography

2001 Census measures are native to the 593-district reference geography. Census-2011 counts are pooled backward only when the reviewed lineage yields a complete deterministic parent. Partial parents are withheld. Shares/rates are computed after pooling counts.

## Constructed measures

The longitudinal family covers structural durability, rooms/crowding, drinking-water source/location, lighting/electricity, bathroom/latrine/drainage, kitchen and cooking fuel, banking, and a narrow set of comparable durable assets.

Richer 2011 categories are collapsed only to an explicit 2001 counterpart. The code avoids relabeling coarse historical categories with stronger modern concepts when the source does not support them; for example, the 2001 water table does not justify an "improved water" classification.

## Accounting and validation

Each table's mutually exclusive categories must reconcile to its published household total before harmonization. Asset tables are treated differently because households may own multiple assets: each asset count is checked as a valid subcount. Independent table families provide cross-checks where their denominators overlap.

## Longitudinal comparability

Only directly comparable categories enter 2001-to-2011 changes. Source-specific coverage gaps remain explicit. Telephone and similar technology measures are interpreted as broad access concepts when the underlying technology changed substantially across censuses.

## Inferential role

A finite registry carries selected 2001–2011 living-standard changes into the shared post-treatment analysis. These changes describe local development associated with the treatment-period measures; mediation is not identified by these regressions.

## Outputs

Validated native and harmonized measures, source-accounting checks, and registered summaries are retained under the housing validation outputs.

## Limitations

Category harmonization cannot eliminate changes in census wording/technology. Housing changes can reflect many forms of local development unrelated to EMI.

## Implementation

Housing readers are in `R/io/read_census_housing.R`; measure/harmonization and shared inference are downstream in the Census and validation modules.
