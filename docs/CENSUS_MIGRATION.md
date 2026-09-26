# Census migration

## Purpose

The migration module constructs 2011 Census district migration outcomes and selected 2001-to-2011 migration-rate changes for descriptive analysis of later district composition.

## Tables used

The active family uses D-02 through D-07 according to their published universes. D-02 provides origin/duration/population-rate measures; D-03 migration reasons; D-04 migrant education; D-05 age selectivity; D-06 economic activity; and D-07 recent work-migrant origin/skill composition. Exact files are declared in the acquisition manifest.

## Geography

Native Census-2011 counts are validated first and then pooled to 2001 Census parents only through complete deterministic lineage. Counts are pooled before rates/shares. Source tables that do not support district detail for a desired concept are not reconstructed from unrelated tables.

## Constructed measures

The retained measures include migration rates and changes, reason composition, migrant education, age selectivity, economic activity, and recent work-migrant skill/origin composition. D-02 rate changes use the registered population denominator rather than an improvised denominator from another migration table.

## Accounting and validation

Readers enforce the published sex/duration/reason/education/activity partitions and uniqueness of district keys. Cross-table comparisons are used only when universes are compatible. Missing/incompatible source rows remain unavailable rather than being imputed.

## Registered inference

The predeclared D-02, D-03, D-04, and D-07 outcomes are analyzed on common support with the shared weak-IV inference. D-05, D-06, and additional rate changes are reported descriptively unless separately registered.

The registered 2011 local-development reduced forms and any weak-IV mechanism IVs use the same central design metadata for controls, fixed effects, clustering, and instrument construction.

## Inferential role

Migration outcomes are possible post-treatment channels or local-development correlates. They are not used as predetermined controls and do not establish mediation. A predeclared Hindi-belt skilled-migration restriction is a bounded sensitivity analysis rather than a new preferred sample.

## Outputs

Validated and harmonized migration measures and registered reduced-form and IV summaries are retained in the migration validation outputs and selected appendix tables.

## Limitations

Published migration tables have different universes and cannot always be combined to recover unreported cells. The module deliberately refuses reconstructions such as multiplying migration totals by destination worker shares.

## Implementation

Readers are in `R/io/read_census_migration.R`; construction and inference are in the Census migration and validation modules using the shared 2011-to-2001 harmonization and post-treatment inference helpers.
