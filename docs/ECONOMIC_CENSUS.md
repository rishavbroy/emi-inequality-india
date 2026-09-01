# Economic Census source contract

## Production sources

The 2005 production source is Development Data Lab SHRUG v2.1's published district product `ec05_pc01dist.csv`, distributed inside `data/raw/shrug/shrug-ec05-csv.zip`. It is already aggregated on the project's analytical geography: `pc01_state_id` and `pc01_district_id` are Census-2001 district identifiers. Reaggregating the raw Fifth Economic Census is therefore unnecessary for the current estimand.

The standardized 2013 SHRUG district product is the intended production source for the follow-up wave. Development Data Lab documents `ec13_pc11dist.dta` as a complete 640-district Census-2011 product keyed by `pc11_state_id` and `pc11_district_id`. It should be pooled through the project's existing complete-child Census-2011-to-2001 geography before any 2013 shares or 2005-2013 changes are computed.

The raw Fifth/Sixth Economic Census archives under `data/raw/ec/` are validation and reconstruction resources, not a parallel production aggregation path when a documented SHRUG district product represents the required estimand.

## Canonical measure semantics

The active EC05 adapter retains only a predeclared core needed for later firm-location and labor-demand mechanisms:

- total non-farm employment;
- total establishments;
- female employment;
- hired employment;
- private-sector employment;
- informal-sector employment;
- manufacturing employment;
- services employment.

Shares use total non-farm employment as the denominator. These categories are not treated as a single partition: private and informal employment can overlap conceptually, and manufacturing plus services need not exhaust non-farm employment. The shared Economic Census validator therefore enforces unique complete district keys, finite nonnegative counts, and positive employment/firm denominators without inventing unsupported accounting identities.

## EC05 geography and coverage

The SHRUG EC05 district product has 591 Census-2001 districts. The canonical project registry has 593. The source is left-joined to the complete canonical district universe, with `source_available = FALSE` and missing measures for uncovered districts rather than imputation or fuzzy recovery. Extra source districts outside the canonical Census-2001 registry are fatal. Production requires at least 99 percent canonical district coverage.

For the currently inspected SHRUG v2.1 source, the two uncovered canonical districts are Mumbai (`27/23`) and Nicobars (`35/02`). This is a source-coverage fact, not a lineage-correction task.

## Sixth Economic Census raw-source validation

The official Sixth Economic Census DDI at `data/raw/ec/EC 2013-2014 Sixth Economic Census/survey0/data/ddi.xml` is now an active extended validation input. The raw archive itself remains in proprietary Nesstar format and is not parsed by the R pipeline.

Direct inspection of the DDI finds 36 state/UT datasets and 58,495,359 establishment records in total. Every state dataset exposes the common fields needed to reconstruct the intended district measures if that ever becomes necessary: state and district identifiers, broad activity/NIC, ownership, hired and non-hired employment by sex, total workers, and sector. The DDI is slightly inconsistent in capitalization (`Total_worker` versus `TOTAL_WORKER`), so schema validation is intentionally case-insensitive.

The raw state coding also demonstrates why a bespoke raw aggregation should not be the default production route: the archive contains codes `36` (Telangana) and `37` (Andhra Pradesh), rather than simply reproducing the original Census-2011 state coding. Development Data Lab explicitly documents that the 2013 district product is linked to Census-2011 geography; the published SHRUG aggregation therefore remains the preferred boundary-normalized source.

## Current inferential role

Economic Census inference remains deliberately inactive. EC05 is currently source-validated and measurement-ready; EC13 raw metadata are validated, but the standardized `ec13_pc11dist` data file is not yet local in the tracked source inventory. Before any EC05 balance model or EC05-EC13 change model is activated:

1. acquire and inspect the standardized EC13 Census-2011 district product;
2. harmonize its counts to complete Census-2001 parents before computing shares or changes;
3. predeclare a small common EC05/EC13 outcome family;
4. reuse the shared mechanism specification/inference layer rather than create an Economic-Census-specific estimator.

Economic Census employment is interpreted as employment located at establishments in the district, not resident-worker employment. NSS/PLFS labor outcomes therefore remain a separate mechanism family.
