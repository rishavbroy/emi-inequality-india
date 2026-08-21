# Linguistic-distance and EMI constructions

## Public specification

The pipeline now preserves the full mutually exclusive Census 2001 C-16 mother-tongue distribution. C-16 language-group rows whose codes end in `000` are subtotals and are not observations in the analytical distribution. Their labels are carried to the child mother-tongue rows so that the group-level classifications reported by Shastry (2012, pp. 294–295) can be applied without double counting.

`data/metadata/shastry_language_distance.csv` is the sole maintained concordance. It records the explicit zero-to-four categories shown in Shastry's Table 1, treats Hindi and Urdu as the same zero-distance language for the primary convention, assigns degree five only to languages explicitly classified as non-Indo-European, and leaves unsupported Indo-European groups unmapped. Unmapped speaker mass remains in share denominators and is reported; it is never silently assigned to degree five or renormalized away.

District constructions include:

- `ling_distance_nonzero_mean`: speaker-weighted mean distance among mapped speakers with distance above zero;
- `ling_share_distance_0` through `ling_share_distance_5`;
- `ling_share_distance_ge3`;
- `hindi_share`, `urdu_share`, `hindi_urdu_share`, and `native_english_share`;
- mapped and unresolved non-English speaker coverage, with native English speakers reported separately;
- `ling_distance_top3_legacy` and its retained-speaker coverage for descriptive comparison.

The public scalar instrument is `ling_distance_nonzero_mean`, the speaker-weighted mean Shastry distance among mapped speakers with positive distance from Hindi. The five nonzero distance shares remain an extended diagnostic set, with distance zero omitted as the compositional reference. `wavg_ling_degrees` is retained only as a compatibility alias for the top-three legacy construction used in historical comparisons.

Native English speakers are an intentional special category rather than unresolved language mass. They do not enter the numerator or denominator of Shastry-style weighted-distance means, even if a genealogical distance is available from Glottolog, and their district share is reported separately for composition adjustment. Hindi and Urdu remain the zero-distance reference categories.

## Glottolog 5.3 source layer

The versioned Glottolog 5.3 source bundle is validated before downstream language-crosswalk work. The direct `languoid.csv` parent graph is the canonical genealogy source because it supplies stable Glottocodes, parent IDs, family IDs, and languoid levels without requiring a Newick parser. The CLDF archive supplies `languages.csv` and `names.csv` for primary and alternative names; `languages_and_dialects_geo.csv` is disambiguation-only, and `tree_glottolog_newick.txt` is an independent representation for later validation. The pipeline anchors Hindi at Glottocode `hind1269`/ISO `hin`, rejects missing parents or parent cycles, resolves dialects to language-level nodes, and defines cross-family robustness distance through one synthetic super-root. No Glottolog taxonomy row automatically overrides the maintained Shastry/Jasanoff concordance.

Extended diagnostics generate `census_glottolog_match_candidates.csv`, a non-authoritative review queue keyed to Census mother-tongue leaf identities. Candidate generation uses exact normalized primary or alternative Glottolog names, plus slash-delimited Census label components; it does not use fuzzy similarity and never promotes a candidate into production automatically. Ambiguous exact matches remain ambiguous, and every candidate remains `unreviewed` until a separate reviewed metadata decision is made. National speaker mass and district coverage are attached to the queue so manual adjudication can prioritize consequential languages rather than alphabetical convenience.

## Education exposure

All district education-exposure margins are derived from the same weighted NSS child universe and common sufficient statistics. The primary age range is 5–19; 6–17 and 6–14 are supplementary constructions.

- `enrollment_rate_0708`: enrolled children divided by all age-eligible children;
- `emi_share_enrolled_0708`: English-medium children divided by enrolled children with known medium;
- `emi_exposure_all_children_0708`: English-medium enrolled children divided by all age-eligible children;
- `unknown_medium_share_enrolled_0708`: enrolled children whose medium is unavailable.

The public treatment is `emi_exposure_all_children_0708`, the survey-weighted share of children ages 5-19 who are both enrolled and observed in English-medium instruction. The historical field `EMIE` remains only for compatibility with legacy comparisons and measures EMI among enrolled children. The exact decomposition into enrollment and the intensive EMI margin is validated only when medium is fully observed; unknown medium is reported rather than classified as non-English.

## Alternative first-stage specifications

Extended diagnostics estimate the preferred all-child EMI treatment against every
Distance constructions on one common district support. The registry covers
the nonzero weighted mean, the share at distance three or higher, the legacy
top-three mean, scalar specifications with combined or separate Hindi and Urdu
composition controls, a Shastry-adjusted scalar specification that also controls
for native-English share, and a joint first stage for the five nonzero distance shares.
Distance zero is the omitted compositional reference in the joint specification.

Each construction is estimated unadjusted and with six-region or state fixed effects
combined with the main and expanded Census control sets. The absorption ladder also
repeats every sequential thematic control block under six-region fixed effects, so
the human-capital step can be compared directly with the preceding specification.
All coefficient standard errors and joint excluded-instrument tests cluster by
Census 2001 state. These remain diagnostic specifications and do not change the
public IV model.

## Mapping coverage and Shastry comparability

`ling_mapped_speaker_share` is the percentage of a district's mutually exclusive C-16 speaker mass whose canonical language can be assigned to one of Shastry's degree categories using the auditable concordance. It is a classification-coverage measure, not Census response coverage and not district-panel match coverage.

The project preserves unmapped mass rather than silently assigning it a degree. It now reports three complementary nonlinear specifications:

- all-speaker distance shares, with unmapped mass left visible;
- all-speaker distance shares with unresolved non-English and native-English shares included as controls, so distance zero is the omitted mapped category;
- mapped-speaker shares renormalized to sum to 100, used only as a sensitivity because renormalization hides unmapped mass.

Results are repeated with minimum mapped-speaker shares of 0, 90, 95, and 99 percent. Distance-four results are decomposed by underlying canonical language and repeated after removing each distance-four language contribution.

Shastry used the 1991 Census classification of 114 languages, assigned all non-Indo-European languages to degree five, and assigned unlisted Indo-European languages the value of the closest language on a language tree. She explicitly preferred 1991 to 1961 because the latter listed 1,652 languages that were difficult to classify. The present project uses the more detailed Census 2001 C-16 hierarchy but only the published degree categories from Shastry's article; consequently, its conservative concordance leaves some detailed categories unmapped instead of reconstructing her unpublished closest-language-tree assignments.

The exploratory outcome output reports conventional clustered 2SLS alongside the reduced-form joint test and an Anderson-Rubin test of a zero treatment effect. Because the scalar state-fixed-effect first stages and the five-share joint first stages are weak, the conventional 2SLS coefficient is not treated as decisive.
