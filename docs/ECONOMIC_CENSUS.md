# Economic Census source contract

## 2005 source

The first Economic Census phase uses Development Data Lab SHRUG v2.1's published district product `ec05_pc01dist.csv`, distributed inside `data/raw/shrug/shrug-ec05-csv.zip`. The source is already aggregated on the project's analytical geography: `pc01_state_id` and `pc01_district_id` are Census-2001 district identifiers. The raw Fifth Economic Census archive under `data/raw/ec/` remains a source-validation resource rather than a second production aggregation path.

The production adapter retains only a predeclared core needed for later firm-location and labor-demand mechanisms:

- total non-farm employment;
- total establishments;
- female employment;
- hired employment;
- private-sector employment;
- informal-sector employment;
- manufacturing employment;
- services employment.

Shares use total non-farm employment as the denominator. These categories are not treated as a single partition: for example, private and informal employment can overlap conceptually, so the code does not require their sum to equal total employment.

## Geography and coverage

The SHRUG district product has 591 Census-2001 districts. The canonical project registry has 593. The source therefore remains left-joined to the complete canonical district universe, with `source_available = FALSE` and missing measures for uncovered districts rather than imputation or fuzzy recovery. Extra source districts outside the canonical Census-2001 registry are fatal. Production requires at least 99 percent canonical district coverage.

For the currently inspected SHRUG v2.1 source, the two uncovered canonical districts are Mumbai (`27/23`) and Nicobars (`35/02`). This is a documented source-coverage fact, not a lineage correction task.

## Current inferential role

This phase deliberately stops at source validation and canonical measurement. It does not add Economic Census outcomes to the IV mechanism registry. Before any EC05 balance or later EC13 change models are activated, the outcome family should be predeclared and the distinction between near-treatment 2005 economic structure and post-treatment firm dynamics should remain explicit.

Economic Census employment is interpreted as employment located at establishments in the district, not resident-worker employment. NSS/PLFS labor outcomes therefore remain a separate mechanism family.
