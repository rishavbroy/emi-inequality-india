# Geography harmonization architecture

The project separates **geography evidence**, **change-of-support mathematics**,
and **variable semantics**.

Source-specific modules remain responsible for evidence acquisition and review:
SHRUG stable localities, LGD, reviewed NSS lineage, Kumar--Somanathan district
history, and related registries. Canonical transition edges normalize those
accepted relationships. The harmonization layer then connects transitions
across vintages without deciding how a particular economic or demographic
quantity should be fractionally allocated.

## Why this separation matters

A boundary relationship is not itself an allocation rule. Population counts,
rates, survey outcomes, land quantities, and point facilities do not support the
same interpolation assumptions.

The allocation-semantics registry therefore distinguishes:

- extensive human quantities: aggregate counts; population is the permitted
  generic fractional basis;
- human rates and shares: allocate/aggregate numerator and denominator, never
  the final rate;
- survey microdata: reweight actual records using reviewed lineage allocation;
- land quantities and spatial surfaces: area-based allocation is admissible;
- point facilities: use actual coordinates or locality identity rather than
  polygon shares.

EMIE and linguistic distance are registered as ratio-like quantities because
their existing implementations reconstruct them from enrolled/eligible weights
and speaker-distance sufficient statistics respectively. Consumption welfare is
registered as survey microdata because the project already allocates household
records rather than district means.

This prevents a calendar rule such as “population before 2001, area after
2001.” The relevant question is the quantity's support and sufficient
statistics, not its year.

## Geography specifications

Geographic assumptions are explicit analysis variants:

- `G0_exact_only`: direct/exact geography only;
- `G1_deterministic_amalgamation`: exact closed constant-boundary components;
- `G2_population_interpolated`: G1 plus population allocation where the
  registered quantity permits it;
- `G3_area_interpolated`: G1 plus area allocation where the registered quantity
  permits it;
- `G4_reviewed_fractional`: G1 plus externally reviewed source-specific
  fractional allocation.

The registry defines policy only. This phase does **not** implement G2 or G3.

## Multi-vintage graph

`build_multivintage_geography_inventory()` binds canonical transition graphs
with explicit `transition_id` provenance and computes undirected connected
components across all supplied vintages. A unit that is the target of one
transition and the source of another is represented once with `side = bridge`.

The first production inventory connects the existing 1991↔2001 historical
transition with the production 2011↔2001 lineage transition and reports whether
each connected component contains units from 1991, 2001, and 2011.

This graph is diagnostic rather than an automatic exact crosswalk. Coverage
evidence differs by source, and a component spanning all vintages is not by
itself proof that every boundary relationship is exact. Fractional allocation
must therefore be added later through an explicit geography specification and a
compatible allocation semantic.

## External methodological anchors

The architecture follows the same broad distinction used by official
geographic relationship products: relationships over time describe how
geographies compare across vintages, rather than supplying one universal
statistical allocation rule. IPUMS spatial harmonization similarly constructs
larger stable units where historical boundaries do not align, while
Kumar--Somanathan construct amalgamated Indian districts with constant
boundaries for long census panels.

Accordingly, exact constant-boundary aggregation remains preferable to
fractional interpolation whenever a component closes. Population- and
area-based interpolation are explicit later sensitivities, not hidden
preprocessing defaults.


## Exact three-vintage certification

The production 2011↔2001 canonical transition now derives both source and target
coverage from the underlying SHRUG stable-locality bridge after the
evidence-priority transition has been selected. This separates two concepts:

- `mapping_class` / `evidence_source` say why an edge is accepted;
- `source_coverage` / `target_coverage` say whether stable-locality evidence
  closes the participating district on each side.

A raw `shrid_coverage = 1` placeholder on an official or reviewed edge is
therefore not enough to certify canonical locality coverage. The canonical
layer recomputes coverage from SHRUG in both directions.

`extract_exact_geography_transition()` retains entire pairwise components only
when every source and target unit closes. `build_exact_multivintage_geography()`
then connects those already-certified pairwise components across vintages. A
component enters `exact_multivintage_crosswalk.csv` only when it contains every
required vintage.

The crosswalk is labeled `G1_deterministic_amalgamation` and contains no
fractional population or area allocation. Components that fail pairwise closure
remain candidates for later G2/G3 sensitivities rather than being renormalized
into G1.


## G2 population interpolation

The exact three-vintage G1 crosswalk is intentionally conservative and therefore
small. G2 provides the next assumption-explicit geography for **human
quantities only** by expressing 1991 and 2011 source districts on Census-2001
boundaries using SHRUG population shares.

Before constructing G2, the production 2011↔2001 canonical transition replaces
placeholder LGD/reviewed weights with population and area shares derived from
the SHRUG stable-locality bridge for the same accepted source-target edge.
Evidence provenance remains unchanged. This prevents an official ancestry edge
with a placeholder weight of one from masquerading as a measured population
allocation.

`population_interpolation_crosswalk.csv` contains raw source-to-2001 population
shares plus Census-2001 identity rows. Source shares are **not renormalized**.
If observed deterministic population shares sum to 0.93, the crosswalk records
0.93 coverage and 0.07 unallocated population rather than forcing the observed
shares to sum to one. Material source partitions above one fail validation.

The generic allocator is limited by the allocation-semantics registry.
`extensive_human` and `ratio_human` families may use G2. Ratio-like quantities
must supply at least numerator and denominator sufficient statistics; the final
rate is never interpolated directly. This preserves the existing EMIE
enrolled/eligible and linguistic-distance speaker/component semantics.

Allocated records retain the complete canonical target identity:
`target_vintage`, `target_state_code`, `target_district_code`, and
`target_unit_id`. The allocator validates that the unit ID agrees with the
administrative codes before returning rows. This keeps change-of-support
mathematics separate from downstream joins and prevents callers from
reconstructing or guessing target geography metadata.

Survey microdata are **not** eligible for generic population interpolation.
Consumption and person-level survey records continue to require reviewed record
allocation/lineage weights. Land quantities, point facilities, and spatial
surfaces likewise remain outside G2.

The G2 diagnostics report source population coverage without choosing an
arbitrary analysis cutoff. Counts above 90, 95, and 99 percent are reported to
show the sensitivity frontier. A later analysis may impose a documented
coverage threshold, but the geography layer itself does not silently discard or
renormalize partial source partitions.


### First G2 analysis: Census-1991 PCA human baseline

The first downstream G2 sensitivity is intentionally restricted to
Census-1991 PCA human quantities. PCA population, sex, age, SC/ST, literacy,
worker, cultivator, and agricultural-labourer counts are retained as sufficient
statistics, population-allocated to Census-2001 districts, summed, and only
then converted back to log population and shares.

The analysis reports source-coverage thresholds of 90, 95, and 99 percent
without renormalizing accepted source shares. A district with 99.5 percent
observed population support contributes 99.5 percent of each count, and the
coverage artifact records the resulting population shortfall.

Village- and town-directory facility rates are deliberately excluded from this
generic G2 sensitivity. Schools, hospitals, health centres, and banks are
point-like facilities; splitting their district totals by population share
would contradict the `point_location` semantic. Their later harmonization
requires locality identifiers/coordinates or a reviewed source-specific rule.

The G2 balance outputs therefore compare eventual EMI exposure and Census-2001
linguistic distance against the human PCA domains only. This provides a
population-interpolated geography robustness check without pretending that all
1991 controls admit the same change-of-support operation.
