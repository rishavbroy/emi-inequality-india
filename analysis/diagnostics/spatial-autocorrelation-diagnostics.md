# Spatial Autocorrelation Diagnostics


## Legacy comments

### Legacy Chunk 29: Moran’s I checks

Buil row‑standardized spatial weights (listw) object

``` r
zero.policy=TRUE to let it handle any islands, zero‑neighbor units
```

Extract residuals from IV models

Global Moran’s I on residuals

Before more controls were added in: m_cons_resid$p.value
2.779572e-23
m_gini_resid$p.value 2.033012e-40 m_fscons_resid$p.value
1.189148e-105
m_fsgini_resid$p.value 1.189148e-105; obviously the same

Moran’s I on explanatory variable and IV

m_EMIE$p.value
8.990354e-180
m_wavg_ling_degrees$p.value 1.721903e-254

Each of the following are named; put them in unname() to get the raw
number

``` r
m_EMIE$statistic = z-score
m_EMIE$estimate[1] = Moran's I statistic
m_EMIE$estimate[2] = expected value of Moran's I under the null (no spatial autocorrelation i.e., randomized locations)
m_EMIE$estimate[3] = variance under the null
```

Test Moran’s I on the response variables

m_cons$p.value
1.608813e-26
m_gini$p.value 8.51626e-22

``` r
Repeat for controls which may have a strong degree of spatial autocorrelation (infrastructure, poverty, etc.)
```

``` r
View all the above statistics' p-values
ls(pattern = "^m_") %>% sapply(.,function(name){get(name)$p.value}, simplify = TRUE) %>% print
```

All of these Moran’s I stats are ridiculously, suspiciously high

Estimate p-vals using Monte Carlo

``` r
moran.test() assumes asymptotic normality
```

``` r
set.seed(999)
num_m = 9999
mc <- moran.mc(resid_cons, listw_2020, nsim = num_m)
plot(mc)
mc$p.value
```

**Deviation note.** The table below uses the current active panel and
geometry. The current p-values are substantively consistent with the
legacy conclusion of strong positive spatial autocorrelation, but are
not exact legacy parity; that context is documented in
`docs/refactor/spatial_diagnostics_context.md`.

## Current targets-backed results

| legacy_name | estimand | variable | source | test | status | statistic | estimate | expected | variance | p.value | method | alternative | n | contiguity | weights_style | matrix_style | zero_policy | n_spatial_rows | n_islands | mean_neighbors | warnings | reason | legacy_note |
|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|:---|:---|---:|:---|:---|:---|:---|---:|---:|---:|:---|:---|:---|
| m_cons_resid | consumption_iv_residual | resid_cons | second_stage_residual | moran | estimated | 5.525 | 0.190 | -0.002 | 0.001 | 0.000 | Moran I test under randomisation | greater | 482 | rook | W | B | TRUE | 482 | 0 | 4.029 | some observations have no neighbours; |  |  |

Current Moran’s I diagnostics

if this seems unexpected, try increasing the snap argument.; neighbour
object has 21 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \|NA \|Final-paper residual p-value:
residuals(model_consumption_iv). Legacy comments reported a pre-control
value of 2.779572e-23. \| \|m_gini_resid \|gini_iv_residual \|resid_gini
\|second_stage_residual \|moran \|estimated \| 5.631\| 0.195\| -0.002\|
0.001\| 0.000\|Moran I test under randomisation \|greater \| 482\|rook
\|W \|B \|TRUE \| 482\| 0\| 4.029\|some observations have no neighbours;
if this seems unexpected, try increasing the snap argument.; neighbour
object has 21 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \|NA \|Legacy residual diagnostic:
residuals(model_gini_iv). Legacy comments reported a pre-control value
of 2.033012e-40. \| \|m_fscons_resid \|consumption_first_stage_residual
\|resid_fscons \|first_stage_residual \|moran \|estimated \| 13.312\|
0.462\| -0.002\| 0.001\| 0.000\|Moran I test under randomisation
\|greater \| 482\|rook \|W \|B \|TRUE \| 482\| 0\| 4.029\|some
observations have no neighbours; if this seems unexpected, try
increasing the snap argument.; neighbour object has 21 sub-graphs; if
this sub-graph count seems unexpected, try increasing the snap argument.
\|NA \|Legacy first-stage residual diagnostic:
residuals(first_stage_consumption). Legacy comments reported a
pre-control value of 1.189148e-105. \| \|m_fsgini_resid
\|gini_first_stage_residual \|resid_fsgini \|first_stage_residual
\|moran \|estimated \| 13.312\| 0.462\| -0.002\| 0.001\| 0.000\|Moran I
test under randomisation \|greater \| 482\|rook \|W \|B \|TRUE \| 482\|
0\| 4.029\|some observations have no neighbours; if this seems
unexpected, try increasing the snap argument.; neighbour object has 21
sub-graphs; if this sub-graph count seems unexpected, try increasing the
snap argument. \|NA \|Legacy first-stage residual diagnostic:
residuals(first_stage_gini). Legacy comments noted the same pre-control
value as first-stage consumption. \| \|m_EMIE \|emie \|EMIE \|treatment
\|moran \|estimated \| 20.494\| 0.714\| -0.002\| 0.001\| 0.000\|Moran I
test under randomisation \|greater \| 482\|rook \|W \|B \|TRUE \| 482\|
0\| 4.029\|some observations have no neighbours; if this seems
unexpected, try increasing the snap argument.; neighbour object has 21
sub-graphs; if this sub-graph count seems unexpected, try increasing the
snap argument. \|NA \|Legacy comments reported p = 8.990354e-180 for
EMIE. \| \|m_wavg_ling_degrees \|linguistic_distance \|wavg_ling_degrees
\|instrument \|moran \|estimated \| 25.533\| 0.894\| -0.002\| 0.001\|
0.000\|Moran I test under randomisation \|greater \| 482\|rook \|W \|B
\|TRUE \| 482\| 0\| 4.029\|some observations have no neighbours; if this
seems unexpected, try increasing the snap argument.; neighbour object
has 21 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \|NA \|Legacy comments reported p =
1.721903e-254 for weighted average linguistic distance. \| \|m_cons
\|consumption_growth \|consumption_pct_change \|outcome \|moran
\|estimated \| 6.674\| 0.231\| -0.002\| 0.001\| 0.000\|Moran I test
under randomisation \|greater \| 482\|rook \|W \|B \|TRUE \| 482\| 0\|
4.029\|some observations have no neighbours; if this seems unexpected,
try increasing the snap argument.; neighbour object has 21 sub-graphs;
if this sub-graph count seems unexpected, try increasing the snap
argument. \|NA \|Final-paper outcome p-value: consumption_pct_change.
Legacy comments reported p = 1.608813e-26. \| \|m_gini \|gini_change
\|gini_change \|outcome \|moran \|estimated \| 3.041\| 0.104\| -0.002\|
0.001\| 0.001\|Moran I test under randomisation \|greater \| 482\|rook
\|W \|B \|TRUE \| 482\| 0\| 4.029\|some observations have no neighbours;
if this seems unexpected, try increasing the snap argument.; neighbour
object has 21 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \|NA \|Legacy outcome diagnostic:
gini_change. Legacy comments reported p = 8.51626e-22. \|
