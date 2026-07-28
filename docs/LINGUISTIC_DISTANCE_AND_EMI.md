# Linguistic-distance and EMI constructions

## Phase 1 status

The pipeline now preserves the full mutually exclusive Census 2001 C-16 mother-tongue distribution. C-16 language-group rows whose codes end in `000` are subtotals and are not observations in the analytical distribution. Their labels are carried to the child mother-tongue rows so that the group-level classifications reported by Shastry (2012, pp. 294–295) can be applied without double counting.

`data/metadata/shastry_language_distance.csv` is the sole maintained concordance. It records the explicit zero-to-four categories shown in Shastry's Table 1, treats Hindi and Urdu as the same zero-distance language for the primary convention, assigns degree five only to languages explicitly classified as non-Indo-European, and leaves unsupported Indo-European groups unmapped. Unmapped speaker mass remains in share denominators and is reported; it is never silently assigned to degree five or renormalized away.

District constructions include:

- `ling_distance_nonzero_mean`: speaker-weighted mean distance among mapped speakers with distance above zero;
- `ling_share_distance_0` through `ling_share_distance_5`;
- `ling_share_distance_ge3`;
- `hindi_share`, `urdu_share`, and `hindi_urdu_share`;
- mapped and unmapped speaker coverage;
- `ling_distance_top3_legacy` and its retained-speaker coverage for descriptive comparison.

The five nonzero distance shares are the proposed excluded-instrument set, with distance zero omitted as the compositional reference. Phase 1 does not yet promote any alternative construction to the public model. The compatibility field `wavg_ling_degrees` remains an alias for the top-three legacy construction until the Phase 2 relevance comparisons are complete.

## Education exposure

All district education-exposure margins are derived from the same weighted NSS child universe and common sufficient statistics. The primary age range is 5–19; 6–17 and 6–14 are supplementary constructions.

- `enrollment_rate_0708`: enrolled children divided by all age-eligible children;
- `emi_share_enrolled_0708`: English-medium children divided by enrolled children with known medium;
- `emi_exposure_all_children_0708`: English-medium enrolled children divided by all age-eligible children;
- `unknown_medium_share_enrolled_0708`: enrolled children whose medium is unavailable.

The historical field `EMIE` remains a compatibility alias for EMI among enrolled children during Phase 1. The preferred future treatment is `emi_exposure_all_children_0708`. The exact decomposition into enrollment and the intensive EMI margin is validated only when medium is fully observed; unknown medium is reported rather than classified as non-English.

## Alternative first-stage specifications

Extended diagnostics estimate the preferred all-child EMI treatment against every
Phase 1 distance construction on one common district support. The registry covers
the nonzero weighted mean, the share at distance three or higher, the legacy
top-three mean, scalar specifications with combined or separate Hindi and Urdu
composition controls, and a joint first stage for the five nonzero distance shares.
Distance zero is the omitted compositional reference in the joint specification.

Each construction is estimated unadjusted and with six-region or state fixed effects
combined with the main and expanded Census control sets. The absorption ladder also
repeats every sequential thematic control block under six-region fixed effects, so
the human-capital step can be compared directly with the preceding specification.
All coefficient standard errors and joint excluded-instrument tests cluster by
Census 2001 state. These remain diagnostic specifications and do not change the
public IV model.
