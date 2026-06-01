X <- c("EMIE", controls_needlag, controls_nolag) %>%
  reformulate() %>%
  model.matrix(data = joined_df)
 
# # Number of independent columns:
# qr(X)$rank
# # Num. total columns:
# ncol(X)
# # Both 18. If qr(X)$rank < ncol(X), exact collinearity.
# 
# # To see which columns are linear combinations of others:
# alias(lm(consumption_pct_change ~ X - 1, data = joined_df))
# 
# # If < 30 / in (30,100) / > 100; then mild / moderate / severe collinearity
kappa <- kappa(X, exact = TRUE)

 
# vif(lm(
#   reformulate(
#     c("EMIE", controls_needlag, controls_nolag),
#     response = "consumption_pct_change"
#   ),
#   data = joined_df
# ))
# 
# vif(model_consumption_iv)
# vif(model_gini_iv)
# # Rule of thumb: VIF > 10 implies problematic collinearity
