# Drop geoemetry
# joined_df <- joined_df %>% st_drop_geometry()
# Seems like this isn't needed, actually


#### Control vectors ####
controls_needlag <- c("consumption_0708", "gini_cons_0708")
controls_nolag <- c( 
  "pct_urban", "avg_hh_size", "dependency_ratio",
  "pct_fem_head", 
  "pct_hindu", "pct_muslim", 
  "pct_st", "pct_sc", "pct_obc", 
  "pct_small_land", "pct_medium_land", "pct_large_land", 
  "pct_head_lit_to_primary", 
  "pct_head_secondary_plus")
controls_lagged <- paste0("W_", controls_needlag)


#### IV Formula ####
make_iv_formula <- function(dep, endog, exog, inst) {
  as.formula(paste0(
    dep, " ~ ",
    paste(c(endog, exog), collapse = " + "),
    " | ",
    paste(c(inst, exog), collapse = " + ")
  ))
}
