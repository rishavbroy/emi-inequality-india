# Census housing and living-standard diagnostics

## Scope

The housing diagnostic now builds longitudinal district measures from seven direct conceptual pairs:

- Census 2001 H-05 and Census 2011 HL-04: household size and number of dwelling rooms;
- Census 2001 H-08 and Census 2011 HL-06: main drinking-water source and source location;
- Census 2001 H-09 and Census 2011 HL-07: main source of lighting;
- Census 2001 H-10 and Census 2011 HL-08/HL-09: bathroom, latrine type, and drainage;
- Census 2001 H-11 and Census 2011 HL-10: separate kitchen and cooking fuel;
- Census 2001 H-12 and Census 2011 HL-11: independent electricity/latrine cross-checks within the drinking-water table;
- Census 2001 H-13 and Census 2011 HL-12: banking services and specified household assets.

The module is an extended diagnostic. It does not add post-treatment housing variables to the preferred IV control set.

## Geographic contract

Census 2001 measures are read directly on the 593-district Census-2001 geography. Census 2011 district counts are mapped backward only through `build_complete_deterministic_transition_2011_to_2001()`. A 2001 parent is retained only when every contributing 2011 district is wholly and deterministically assigned to that parent. Counts are summed before any share is computed.

This is the same administrative-count rule used by the migration and worker modules. Fractional or incomplete 2011 reconstructions are withheld rather than interpreted as complete household totals.

## Dwelling rooms and conservative overcrowding

H-05 and HL-04 publish household size crossed with number of dwelling rooms. Every retained row must partition its household total exactly across no exclusive room, one, two, three, four, five, and six-or-more rooms, and the detailed household-size rows must reproduce the all-households room distribution. HL-04 additionally partitions the all-household room distribution by ownership status; owned, rented, and other tenure must reproduce the total exactly.

The directly comparable longitudinal margins are no-exclusive-room and one-room household shares. The module also reports a deliberately conservative lower bound for the standard greater-than-two-persons-per-room overcrowding concept. Because household size is grouped at `6-8` and `9+`, ambiguous cells are not classified as overcrowded. Only cells that necessarily exceed two persons per room for every household represented in that published cell enter the numerator. `No exclusive room` is reported separately rather than assigned an artificial persons-per-room value.

The attached 2001 H-05 files contain 590 district rows. Chandigarh (`04/01`), Daman (`26/01`), and Lakshadweep (`31/01`) are absent, while the other active 2001 housing sources contain 593 districts. H-05 is therefore treated as a validated subset: missing baseline room/crowding values remain missing without truncating the rest of the housing panel. HL-04 contains all 640 Census-2011 districts. Ownership shares are retained as 2011-only descriptors because H-05 does not provide the same ownership dimension.

## Drinking-water source and location

H-08 and HL-06 have directly comparable location rows: total, within premises, near premises, and away. Those location rows must reproduce the total household count and each source count exactly before any harmonization.

For longitudinal source comparisons the richer 2011 categories are collapsed only where the 2001 table has an exact broader counterpart: treated plus untreated tap water becomes `tap`; covered plus uncovered wells becomes `well`; handpump plus tubewell/borehole is retained as a combined groundwater-access category; river/canal plus tank/pond/lake becomes `surface water`. Spring and residual other-source counts remain source-accounting components but are not separately promoted into the compact longitudinal outcome set.

The module deliberately does **not** label the resulting 2001/2011 source measure as `improved drinking water`. H-08 does not distinguish covered from uncovered wells, so applying a modern improved/unimproved classification would create false longitudinal comparability. The retained longitudinal margins are tap, well, handpump/tubewell, surface-water, within-premises, and away-source household shares. Both H-08 and HL-06 have complete native district coverage (593 and 640 districts respectively).

## Sanitation, bathing, and drainage

H-10 publishes bathroom availability, an exhaustive four-way latrine partition (pit, water closet, other latrine, no latrine), and an independent exhaustive drainage partition (closed, open, no drainage). HL-08 is the dedicated 2011 latrine table; its detailed flush/pour-flush, pit, open-drain, and service-latrine cells must exhaust households with a latrine within the premises, while public-latrine and open alternatives must exhaust households without a latrine within the premises. HL-09 separately requires bathroom, enclosure-without-roof, and no-bathing-facility counts to exhaust households, and independently requires closed, open, and no drainage to exhaust the same household universe.

For longitudinal comparison the richer 2011 latrine detail is collapsed only to categories that H-10 directly supports: flush/pour-flush becomes the counterpart of water closet, and slab/open-pit variants become a broad pit-latrine count. Closed drainage and no drainage are directly comparable margins. `bathroom_share_households` uses the published bathroom count in both censuses, but interpretation should acknowledge a location-scope difference: H-10 describes a bathroom within the house, whereas HL-09 classifies bathing facilities within the premises.

The dedicated H-10/HL-08 latrine tables now provide the primary latrine count. H-12/HL-11 remain valuable independent cross-checks: where H-12 is published, its latrine-availability count must agree exactly with H-10; HL-11 and HL-08 must agree exactly across the full 2011 district universe. This removes the six-district H-12 source gap from the longitudinal latrine measure without weakening source validation.

## Kitchen and cooking fuel

H-11 and HL-10 both publish total households by kitchen availability and an exhaustive cooking-fuel distribution. Every kitchen row must exhaust its household count across the published fuels. H-11 additionally requires `Available`, `Not available`, `Cooking in Open`, and `No Cooking` to reconstruct the total household/fuel distribution. HL-10 has a hierarchy: cooking inside and outside the house must each equal their `Has Kitchen` plus `Does not have kitchen` children, and inside + outside + no cooking must reconstruct the total for households and every fuel.

The direct kitchen margin is a separate kitchen within the house: H-11 `Available` and HL-10 `Cooking inside house: Has Kitchen`. Two compact fuel groups are retained. `solid_cooking_fuel_share_households` sums firewood, crop residue, cowdung cake, and coal/lignite/charcoal. `clean_cooking_fuel_share_households` sums LPG/PNG, electricity, and biogas. The gas category is technologically broader in 2011 because HL-10 combines LPG and PNG while H-11 publishes LPG only; the longitudinal variable should therefore be read as a broad clean-fuel access measure rather than a technologically identical gas service.

## Source accounting

The readers enforce the published household universes before harmonization.

For H-09/HL-07, the mutually exclusive lighting categories must sum exactly to total households. For H-12, electricity availability and latrine availability each form their own exhaustive binary partition. HL-11 publishes the four joint electricity-by-latrine cells; the reader checks that the four cells exhaust households and then obtains the electricity and latrine marginals by summation.

Within each census year, the module additionally checks:

1. H-09/HL-07 household totals against H-12/HL-11 totals district by district;
2. the lighting-table electricity count against the utility-table electricity count district by district;
3. H-09/HL-07 household totals against H-13/HL-12 totals district by district.

The attached official 2001 files reveal two source-coverage exceptions. H-09, H-08, H-10, H-11, and H-13 contain all 593 Census-2001 districts. H-12 has 587 district rows and omits Andhra Pradesh district codes 18-23 (Prakasam, Nellore, Cuddapah, Kurnool, Anantapur, and Chittoor). H-05 has 590 district rows and omits Chandigarh (`04/01`), Daman (`26/01`), and Lakshadweep (`31/01`). H-12 and H-05 are therefore validated as subsets of the full household universe; neither is used to truncate otherwise-supported outcomes. H-10 supplies complete baseline latrine coverage, while H-12 remains an independent overlap validation. Room/crowding baselines and changes remain missing only where H-05 itself is absent.

HL-08, HL-09, and HL-10 each contain all 640 Census-2011 districts, and their household totals agree exactly with HL-07 across that universe in the attached source files.

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

The longitudinal table now also reports bathroom, flush/water-closet latrine, pit latrine, closed/no drainage, separate-kitchen, solid-fuel, and clean-cooking-fuel changes in addition to room/crowding, drinking-water, lighting/electricity, latrine availability, banking, and asset changes. Changes are retained as missing when either year's underlying share is unavailable; `change_coverage.csv` reports this support outcome by variable.

## Mechanism inference

A deliberately smaller registry carries eight pre-existing longitudinal changes into extended mechanism diagnostics: electricity access, no-lighting, banking, television, telephone, bicycle, motorcycle/scooter, and car/jeep/van. Kerosene and solar lighting are omitted because they are components of the same exhaustive lighting partition already represented by electricity/no-lighting. Newly completed latrine, sanitation/drainage, kitchen/fuel, room, and water changes remain descriptive in this phase. H-10 now removes the old H-12 latrine coverage constraint, but the inferential family is intentionally not expanded after inspecting earlier weak first stages; measurement completion is not treated as an automatic invitation to add hypotheses.

The registered housing outcomes use the same common-support scalar-IV engine as Census migration: six region/state-FE × Shastry/Glottolog/Dyen specifications, one fixed sample across the eight housing changes, state-clustered reduced forms, conventional 2SLS estimates for scale, effective-F diagnostics, and Anderson-Rubin tests/confidence sets with within-specification Holm adjustment. Because the migration-sample first stages are already weak, Anderson-Rubin inference remains the interpretation-first result rather than conventional 2SLS significance.

These regressions use **changes**, not 2011 levels, so the estimand is local improvement in the measured housing/living-standard margin between 2001 and 2011. They remain extended mechanism diagnostics and are not controls in the preferred welfare equation or claims of identified mediation.

## Deferred table

HL-13 remains acquisition-only. Structural-durability measures should not be activated until the correct 2001 conceptual counterpart and denominator contract are established explicitly rather than inferred from table numbering.
