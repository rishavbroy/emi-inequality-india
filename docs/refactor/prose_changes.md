# Prose Change Log

This log records postprocessing changes that intentionally diverge from the legacy Rmd prose. The goal is to keep these patches visible while the semantic refactor replaces temporary wording with faithful computed outputs.

| file | location / legacy text start | new text start | reason | status |
|---|---|---|---|---|
| `paper/report.qmd` | `Geospatial data used to construct the maps in this paper...` | `Geospatial data intended for maps and spatial autocorrelation measures...` | The active final pipeline now refuses to produce final maps until the district panel has validated geometry coverage. The prose names that blocker instead of implying completed maps. | temporary |
| `paper/report.qmd` | `Looking deeper into the supply side of education...` | `Looking deeper into the supply side of education, it seems that all variation...` | Preserves the legacy interpretation while replacing unavailable inline AME expressions with report-value lookups that the audit can verify. | temporary |
| `paper/report.qmd` | `Summary statistics for all of the variables in this model...` | `Summary statistics for all of the variables in this model... are provided in @tbl-sum-tbl-iv.` | Converts bookdown references to Quarto references and removes duplicate prose prefixes that render as `Table Table` / `Figure Figure`. | permanent syntax migration |
| `paper/report.qmd` and `docs/district-matching.qmd` | `As was evident from the maps of Figures...` | `As was evident from @fig-map1-fig and @fig-map2-fig...` | Quarto cross-reference syntax already renders object type names; the prose is rewritten to avoid duplicate rendered labels. | permanent syntax migration |
| `paper/report.qmd` | `We can control for this spatial autocorrelation...` | `We can control for this spatial autocorrelation by incorporating spatial lags...` | The active pipeline has not validated the 2001/2007-to-2020 geometry join, so Moran statistics are not reported as final. | temporary |
| `docs/district-matching.qmd` | `Sec. @sec-iv-iv` / `Sec. @sec-intro` | `the IV section of the main report` / `the introduction of the main report` | Standalone district-matching note cannot cross-reference sections only present in the full report. | permanent standalone-note adaptation |
