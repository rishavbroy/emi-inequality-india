# Buil row‑standardized spatial weights (listw) object
# zero.policy=TRUE to let it handle any islands, zero‑neighbor units
listw_2020 <- nb2listw(nb_2020, style = "W", zero.policy = TRUE)

#Extract residuals from IV models
resid_cons <- residuals(model_consumption_iv)  
resid_gini <- residuals(model_gini_iv)
resid_fscons <- residuals(first_stage_consumption)
resid_fsgini <- residuals(first_stage_gini)

# Global Moran’s I on residuals
m_cons_resid <- moran.test(resid_cons, listw_2020, zero.policy = TRUE)
m_gini_resid <- moran.test(resid_gini, listw_2020, zero.policy = TRUE)
m_fscons_resid <- moran.test(resid_fscons, listw_2020, zero.policy = TRUE)
m_fsgini_resid <- moran.test(resid_fsgini, listw_2020, zero.policy = TRUE)

# Before more controls were added in:
# m_cons_resid$p.value
# 2.779572e-23
# m_gini_resid$p.value
# 2.033012e-40
# m_fscons_resid$p.value
# 1.189148e-105
# m_fsgini_resid$p.value
# 1.189148e-105; obviously the same

# Moran’s I on explanatory variable and IV
m_EMIE   <- moran.test(joined_df$EMIE,             listw_2020, zero.policy = TRUE)
m_wavg_ling_degrees   <- moran.test(joined_df$wavg_ling_degrees, listw_2020, zero.policy = TRUE)

# m_EMIE$p.value
# 8.990354e-180
# m_wavg_ling_degrees$p.value
# 1.721903e-254

# Each of the following are named; put them in unname() to get the raw number
# m_EMIE$statistic = z-score
# m_EMIE$estimate[1] = Moran's I statistic
# m_EMIE$estimate[2] = expected value of Moran's I under the null (no spatial autocorrelation i.e., randomized locations)
# m_EMIE$estimate[3] = variance under the null


# Test Moran’s I on the response variables
m_cons <- moran.test(joined_df$consumption_pct_change, listw_2020, zero.policy=TRUE)
m_gini <- moran.test(joined_df$gini_change,             listw_2020, zero.policy=TRUE)

# m_cons$p.value
# 1.608813e-26
# m_gini$p.value
# 8.51626e-22

# Repeat for controls which may have a strong degree of spatial autocorrelation (infrastructure, poverty, etc.)


# View all the above statistics' p-values
# ls(pattern = "^m_") %>% sapply(.,function(name){get(name)$p.value}, simplify = TRUE) %>% print


# All of these Moran's I stats are ridiculously, suspiciously high

# Estimate p-vals using Monte Carlo
# moran.test() assumes asymptotic normality


# set.seed(999)
# num_m = 9999
# mc <- moran.mc(resid_cons, listw_2020, nsim = num_m)
# plot(mc)
# mc$p.value

