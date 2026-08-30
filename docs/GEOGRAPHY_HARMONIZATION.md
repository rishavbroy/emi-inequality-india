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
