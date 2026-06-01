# Join geometry

# For merged data based on district_tracker 
joined_df_tracker <- district_bnds_20 %>%
  select(JID, geometry) %>%
  full_join(
    joined_df_tracker,
    by = "JID"
  )

joined_df_tracker <- joined_df_tracker %>%
  mutate(
    consumption_pct_change = (consumption_1718 - consumption_0708) / consumption_0708 * 100,
    gini_change = gini_cons_1718 - gini_cons_0708
  )

joined_df_tracker <- joined_df_tracker %>%
  left_join(region_df, by = "state_20")


# For merged data based on district_timeseries
joined_df_timeseries <- district_bnds_20 %>%
  select(JID, geometry) %>%
  full_join(
    joined_df_timeseries,
    by = "JID"
  )

joined_df_timeseries <- joined_df_timeseries %>%
  mutate(
    consumption_pct_change = (consumption_1718 - consumption_0708) / consumption_0708 * 100,
    gini_change = gini_cons_1718 - gini_cons_0708
  )

joined_df_timeseries <- joined_df_timeseries %>%
  left_join(region_df, by = join_by(state_24 == state_20))



#### Compare districts in district_tracker merge vs. district_timeseries merge ####

# tm_shape(joined_df_timeseries) + tm_fill("consumption_pct_change", palette = "Reds", title = "%ΔConsumption") + tm_borders(alpha = 0.2) + tm_layout(frame = FALSE)

# tm_shape(joined_df_tracker) + tm_fill("consumption_pct_change", palette = "Reds", title = "%ΔConsumption") + tm_borders(alpha = 0.2) + tm_layout(frame = FALSE)