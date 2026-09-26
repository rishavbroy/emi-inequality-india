# Census 2001 controls

## Purpose

This module constructs the compact predetermined district controls used by the main specifications and the finite alternative parameterizations used in balance/absorption diagnostics.

## Tables used

The active control build combines the registered Census-2001 PCA district archive with official C-01, C-08, C-14, and H-09 workbooks. Exact manifest/source details belong in [`../DATA_AVAILABILITY.md`](../DATA_AVAILABILITY.md) and `data/metadata/`.

## Geography

Controls are defined on the native 593-district Census-2001 geography. State and district codes are normalized jointly; district numbers are never interpreted without their state code.

## Main-paper controls

The compact main family contains the registered predetermined variables, including population scale, urbanization, adult human capital, and the declared economic/social structure controls. The machine-readable control registry is authoritative for exact variable membership and parameterization.

## Alternative parameterizations

Alternative controls are finite sensitivity designs, including declared proxy substitutions and symmetric control-block interventions. Historical specification IDs may retain legacy labels for output compatibility, but those labels should not be interpreted as an ordered hierarchy in which more controls are automatically preferable.

## Accounting and validation

Each required count source must contain the expected Census-2001 state-district keys with no duplicates. Ratios are constructed only after source counts have passed key/accounting checks. Final-mode construction stops for incomplete or unexpected required coverage rather than dropping districts silently.

## Inferential role

These variables are predetermined adjustment/balance variables. They do not repair weak relevance and should not be selected by whichever set yields the strongest first stage. Historical baseline controls are documented separately in [`HISTORICAL_BASELINE_VALIDATION.md`](HISTORICAL_BASELINE_VALIDATION.md).

## Outputs

The controls feed the shared analysis-design registry, first-stage/IV models, balance diagnostics, and sensitivity families. Diagnostic summaries are retained under the extended output tree where declared.

## Implementation

The registry and construction code are under Census control/measure modules and the IV specification layer. Worker-derived control concepts are described in [`CENSUS_WORKERS.md`](CENSUS_WORKERS.md).
