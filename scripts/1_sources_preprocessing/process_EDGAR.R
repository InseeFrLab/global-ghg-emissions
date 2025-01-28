# SOURCE CONTENT : EDGAR 8.0 database

# HOW TO UPDATE THIS SOURCE ? **************************************************

# link : https://edgar.jrc.ec.europa.eu/dataset_ghg80

# In "Time series (1970-2022)", download the following files (.xlsx) :
# - EDGAR CH4
# - IEA-EDGAR CO2
# - EDGAR F-gases
# - EDGAR N2O

# ******************************************************************************


# ------------------------------ NOMENCLATURE ----------------------------------

df_edgar_countries <- read_xlsx("resources/nomenclatures.xlsx", "edgar_countries") %>%
  select(Country_code_A3, country_code_figaro)

# ------------------------------ LOAD SOURCE -----------------------------------

df_edgar_emissions_CO2 <- read_xlsx(paste0(local_path_sources, "/EDGAR/IEA_EDGAR_CO2_1970_2022.xlsx"),
                                    sheet = "IPCC 2006", skip = 9)
df_edgar_emissions_CH4 <- read_xlsx(paste0(local_path_sources, "/EDGAR/EDGAR_CH4_1970_2022.xlsx"),
                                    sheet = "IPCC 2006", skip = 9)
df_edgar_emissions_N2O <- read_xlsx(paste0(local_path_sources, "/EDGAR/EDGAR_N2O_1970_2022.xlsx"),
                                    sheet = "IPCC 2006", skip = 9)
df_edgar_emissions_GF <- read_xlsx(paste0(local_path_sources, "/EDGAR/EDGAR_F-gases_AR5g_1990_2022.xlsx"),
                                   sheet = "IPCC 2006", skip = 9)

df_edgar_emissions_raw <- bind_rows(df_edgar_emissions_CO2,
                                    df_edgar_emissions_CH4,
                                    df_edgar_emissions_N2O,
                                    df_edgar_emissions_GF)

# ------------------------------ FORMATTING ------------------------------------

df_edgar_emissions <- df_edgar_emissions_raw %>%
  filter(fossil_bio == "fossil") %>%
  select(Country_code_A3, ipcc_code_2006 = ipcc_code_2006_for_standard_report,
         gas_type = Substance,
         starts_with("Y_")) %>%
  # FIGARO country nomenclature
  left_join(df_edgar_countries, by = "Country_code_A3") %>%
  group_by(country_code_figaro, ipcc_code_2006, gas_type) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rename(emis_country = country_code_figaro) %>%
  # recode F-gases names
  mutate(gas_type = if_else(grepl("GWP_100_AR5_", gas_type), paste0(str_sub(gas_type, 13), "_CO2E"), gas_type),
         gas_type = if_else(gas_type == "HCFC_CO2E", "HFC_CO2E", gas_type)) %>%
  # agregation NF3 + SF6
  mutate(gas_type = if_else(gas_type %in% c("NF3_CO2E", "SF6_CO2E"), "NF3_SF6_CO2E", gas_type)) %>%
  group_by(emis_country, ipcc_code_2006, gas_type) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  # pivot to a longer table (years in row instead of columns)
  pivot_longer(where(is.numeric), names_to = "time_period", values_to = "ghg_emission") %>%
  mutate(time_period = str_sub(time_period, 3))
  

# ---------------------------------- SAVE --------------------------------------

write_rds(df_edgar_emissions, paste0(path_frmt_source, "/edgar_emissions.rds"))
