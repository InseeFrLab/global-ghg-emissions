
# ----------------------------- PARAMETERS -------------------------------------

last_eurostat_year <- 2021


# ------------------------------------------------------------------------------
# ---------------------------- LOADING SOURCES ---------------------------------
# ------------------------------------------------------------------------------

# AEAs estimated by EUROSTAT ---------------------------------------------------

df_global_aea_estimated_by_eurostat <- read_rds(paste0(path_frmt_source, "/global_aea_estimated_by_eurostat.rds")) %>%
  filter(time_period <= last_eurostat_year) %>%
  rename(obs = ghg_emission)

# AEAs estimated in this project -----------------------------------------------

df_global_aea_replication <- read_rds(paste0(path_results,"/global_aea_replicate_eurostat.rds")) %>%
  rename(estimate = ghg_emission)


# ------------------------------------------------------------------------------
# -------------------- EXTENSION OF EUROSTAT'S ESTIMATE ------------------------
# ------------------------------------------------------------------------------

# Chainlinking in YoY % change
df_global_aea_estimated_by_eurostat_extended <- full_join(df_global_aea_estimated_by_eurostat,
                                                          df_global_aea_replication,
                                                          by = join_by(gas_type, time_period, emis_country, emis_industry)) %>%
  arrange(gas_type, emis_country, emis_industry, time_period) %>%
  group_by(gas_type, emis_country, emis_industry) %>%
  mutate(ghg_emission = if_else(!is.na(obs),
                                obs,
                                estimate * sum(obs * (time_period == last_eurostat_year), na.rm = TRUE) / sum(estimate * (time_period == last_eurostat_year)))) %>%
  ungroup()

# Control 1 : no NAs
df_CTRL_eurostat_global_aea_extended_NA <- df_global_aea_estimated_by_eurostat_extended %>%
  filter(is.na(ghg_emission), estimate > 0.001)

# Control 2 : result = 0 in T+1 whereas the last observed value in T is non zero
#  Result : a small glitch in PFC_CO2E / FR / HH in 2022 ? but otherwise OK
df_CTRL_eurostat_global_aea_extended_by_0 <- df_global_aea_estimated_by_eurostat_extended %>%
  filter(time_period %in% c(last_eurostat_year, last_eurostat_year + 1)) %>%
  arrange(gas_type, emis_country, emis_industry, time_period) %>%
  group_by(gas_type, emis_country, emis_industry) %>%
  mutate(zero_extension = (time_period == last_eurostat_year + 1 & ghg_emission == 0 & lag(obs) > 0)) %>%
  filter(any(zero_extension)) %>%
  ungroup()

# Control 3 : series which have not been extended
#   Result : very small amounts, notably in CH - we can live with it
nb_year_obs <- length(unique(df_global_aea_estimated_by_eurostat$time_period))
df_CTRL_eurostat_global_aea_non_extended_series <- df_global_aea_estimated_by_eurostat_extended %>%
  add_count(gas_type, emis_country, emis_industry, name = "nb_years_series") %>%
  filter(nb_years_series == nb_year_obs)

# Final formatting
df_global_aea_estimated_by_eurostat_extended <- df_global_aea_estimated_by_eurostat_extended %>%
  select(gas_type, time_period, emis_country, emis_industry, ghg_emission) %>%
  mutate(ghg_emission = replace_na(ghg_emission, 0))


# ----------------------------------- SAVE -------------------------------------

write_rds(df_global_aea_estimated_by_eurostat_extended,
          paste0(path_results,"/global_aea_estimated_by_eurostat_extended.rds"))
