# SOURCE CONTENT : PRIMAP version 2.6

# HOW TO UPDATE THIS SOURCE ? **************************************************

# link : https://zenodo.org/records/13752654

# In the "Files" section, download the following file :
#  Guetschow_et_al_2024a-PRIMAP-hist_v2.6_final_no_rounding_13-Sep-2024.csv


# ******************************************************************************


# ------------------------------ NOMENCLATURE ----------------------------------

df_primap_countries <- read_xlsx("resources/nomenclatures.xlsx", "primap_countries") %>%
  select(country_code_iso3, country_code_figaro)

# ------------------------------ LOAD SOURCE -----------------------------------

df_primap_emissions_raw <- read_csv(paste0(local_path_sources, "/PRIMAP/Guetschow_et_al_2024a-PRIMAP-hist_v2.6_final_no_rounding_13-Sep-2024.csv"))

# ------------------------------ FORMATTING ------------------------------------

# Step 1 : basic formatting and agregation to FIGARO country nomenclature

df_primap_emissions <- df_primap_emissions_raw %>%
  # Removing unnecessary information
  select(scenario = `scenario (PRIMAP-hist)`,
         area = `area (ISO3)`,
         category = `category (IPCC2006_PRIMAP)`,
         gas_type = entity,
         starts_with("20")) %>%
  # Filtering GHGs
  filter(gas_type %in% c("CO2", "CH4", "N2O", "FGASES (AR5GWP100)", "HFCS (AR5GWP100)", "PFCS (AR5GWP100)")) %>%
  # FIGARO country nomenclature
  left_join(df_primap_countries, by = c("area" = "country_code_iso3")) %>%
  group_by(scenario, country_code_figaro, gas_type, category) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rename(emis_country = country_code_figaro) %>%
  # pivot to a longer table (years in row instead of columns)
  pivot_longer(where(is.numeric), names_to = "time_period", values_to = "ghg_emission")

# Step 2 : recover NF3_SF6_CO2E from the different F-gases information

df_primap_emissions_FLG_CO2E <- df_primap_emissions %>%
  filter(gas_type %in% c("FGASES (AR5GWP100)", "HFCS (AR5GWP100)", "PFCS (AR5GWP100)")) %>%
  pivot_wider(names_from = gas_type, values_from = ghg_emission) %>%
  rename(FLG_CO2E = `FGASES (AR5GWP100)`,
         HFC_CO2E = `HFCS (AR5GWP100)`,
         PFC_CO2E = `PFCS (AR5GWP100)`) %>%
  mutate(NF3_SF6_CO2E = FLG_CO2E - HFC_CO2E - PFC_CO2E) %>%
  select(-FLG_CO2E) %>%
  pivot_longer(where(is.numeric), names_to = "gas_type", values_to = "ghg_emission")

# Step 3 : row-binding F-gases and non-F-gases

df_primap_emissions_with_FLG <- df_primap_emissions %>%
  filter(gas_type %in% c("CO2", "CH4", "N2O")) %>%
  bind_rows(df_primap_emissions_FLG_CO2E)


# ---------------------------------- SAVE --------------------------------------

write_rds(df_primap_emissions_with_FLG, paste0(path_frmt_source, "/primap_emissions.rds"))
