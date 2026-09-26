# Census 2001 controls

## Purpose

This module constructs the predetermined district controls used by the main specifications and the alternative control sets used in balance and first-stage sensitivity analyses.

## Tables used

The active control build combines the registered 2001 Census PCA district archive with official C-01, C-08, C-14, and H-09 workbooks. Exact manifest/source details belong in [`../DATA_AVAILABILITY.md`](../DATA_AVAILABILITY.md) and `data/metadata/`.

## Geography

Controls are defined on the native 593-district 2001 Census geography. State and district codes are normalized jointly; district numbers are never interpreted without their state code.

## Main-paper controls

The main control set contains the registered predetermined variables, including population scale, urbanization, adult human capital, and the declared economic and social structure controls. The control registry records the exact variable membership and parameterization.

## Alternative parameterizations

Alternative controls are finite sensitivity designs, including declared proxy substitutions and symmetric control-block interventions. Historical specification IDs may retain legacy labels for output compatibility, but those labels should not be interpreted as an ordered hierarchy in which more controls are automatically preferable.

## Accounting and validation

Each required count source must contain the expected 2001 Census state-district keys with no duplicates. Ratios are constructed only after source counts have passed key/accounting checks. Final-mode construction stops when required coverage is incomplete or unexpected.

## Inferential role

These variables provide predetermined adjustment and balance checks. Control specifications are predeclared independently of first-stage strength. Historical baseline controls are documented separately in [`HISTORICAL_BASELINE_VALIDATION.md`](HISTORICAL_BASELINE_VALIDATION.md).

## Outputs

The controls feed the shared analysis registry, first-stage and IV models, balance checks, and sensitivity analyses. Extended summaries are retained in the validation outputs where declared.

## Implementation

The registry and construction code are under Census control/measure modules and the IV specification layer. Worker-derived control concepts are described in [`CENSUS_WORKERS.md`](CENSUS_WORKERS.md).
