# Preliminary test of IV strength:
# Dotplot of EMIE values by district_code
plt_EMI_across_stats_and_districts <- ggplot(
  df0708 %>% mutate(
    district_prefix = substr(as.character(district_code_0708), 1, 2)
    ), 
  aes(x = district_code_0708, y = EMIE, color = district_prefix)
  ) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  labs(title = "Percentage of Students in EMI by District, 2007-08",
       x = "District (sorted by state and within-state region)", y = "Percentage in EMI",
       color = "State Code") +
  theme_minimal()
# EMIE has three peaks: in Jammu and Kashmir; in Sikkim, Arunachal Pradesh, Nagaland, Manipur, Mizoran, Tripura, maybe Meghalaya; and in Andhra Pradesh, Karnataka, Goa, Lakshadweep, Kerala, Tamil Nadu, Pondicheri, and Andaman & Nicobar! The regions which historically were the furthest from Hindi!
# Many districts in the second group seem to have EMIE around 1, and the range of EMIE outside peaks is between 0.4 and 0.1. Justification for looking at smaller units of analysis?



# Process the dataframe and compute the weighted average ling_distance for each (state, district) group.
mother_tongues_01 <- census01 %>%
  # Group by state and district to ensure that same district names in different states are treated separately
  group_by(state, district) %>%
  # For each group, keep only the top three rows by spkr_tot
  slice_max(order_by = spkr_tot, n = 3) %>%
  # Ungroup before applying new transformations
  ungroup() %>%
  # Create the ling_distance column based on the mother_tongue values and @shastry2012a's 0-5 measure of degrees of linguistic distance
  mutate(
    ling_degrees = case_when(
      mother_tongue %in% c("Hindi", "Urdu") ~ 0,
      mother_tongue %in% c("Gujarati", "Punjabi", "Rajasthani") ~ 1,
      mother_tongue %in% c("Konkani", "Marathi") ~ 2,
      mother_tongue %in% c("Assamese", "Bengali", "Bihari", "Oriya") ~ 3, # Oriya was renamed as Odia in 2011
      mother_tongue %in% c("Kashmiri", "Sindhi", "Sinhalese") ~ 4,
      TRUE ~ 5 # Why TRUE?
    )
  ) %>%
  # Group again by state and district to compute group summaries
  group_by(state, district) %>%
  # Calculate the weighted average ling_distance for each group. For each langauge $\ell$ which is among the top three most spoken in district $d$: $$\frac{\sum_{\ell}(\text{linguistic distance of }\ell \times \text{num. speakers of }\ell\text{ in }d)}{\sum_{\ell}(\text{num. speakers of }\ell)}$$
  summarize(
    wavg_ling_degrees = sum(ling_degrees * spkr_tot, na.rm = TRUE) / sum(spkr_tot, na.rm = TRUE),
    .groups = "drop" # Makes ungroup() redundant?
  ) %>% 
  ungroup() %>% 
  rename(state_01 = state, district_01 = district)

# census01 %>% mutate(StateDistrict = paste0(state_code, district_code)) %>% distinct(StateDistrict) %>% nrow() - mother_tongues_01 %>% mutate(StateDistrict = paste0(state_01, district_01)) %>% distinct(StateDistrict) %>% nrow()
# Number of districts did not change in this operation
