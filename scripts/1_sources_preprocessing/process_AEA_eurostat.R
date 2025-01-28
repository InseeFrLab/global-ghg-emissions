# SOURCE CONTENT : official Air Emission Accounts available from EUROSTAT
#  (complete for EU 27 countries + incomplete for CH, NO and TR)

# HOW TO UPDATE THIS SOURCE ? **************************************************

# Data available here : https://ec.europa.eu/eurostat/web/main/data/database
# 
# In the "Data navigation tree" :
# Detailed datasets
#   Environment and energy
#     Environment (env)
#       Emissions of greenhouse gases and air pollutants (env_air)
#         Air emission accounts (env_air_aa)
#           Air emissions accounts by NACE Rev. 2 activity (env_ac_ainah_r2)

# OPEN THE DATABROWSER and select the following dimensions :
#  airpol 	  = 	CO2, CH4, N2O, HFC_CO2E, PFC_CO2E, NF3_SF6_CO2E
#  geo 		    = 	all available categories
#  nace_r2 	  =   all available categories
#  time       =   from 2010 to the most recent year available
#  unit       =   THS_T (Thousand tons)

# Then download the resulting dataset, with the following options :
#  "All selected dimensions"
#  "SDMX-CSV"
#  And in "Advanced settings", select "File format = SDMX-CSV 2.0"
#    to make sure you get codes and labels in separated columns

# The resulting file's name will typically be like : "estat_env_ac_ainah_r2_filtered_en.csv"

# ******************************************************************************


# ------------------------------- PARAMETERS -----------------------------------

vc_gas_types <- c("CO2", "CH4", "N2O", "HFC_CO2E", "PFC_CO2E", "NF3_SF6_CO2E")

# ------------------------------ NOMENCLATURE ----------------------------------

df_nace_a64 <- read_xlsx("resources/nomenclatures.xlsx", "cpa_nace_a64") %>%
  select(code_a64_figaro, code_a64_eurostat_aea)

# ------------------------------ LOAD SOURCE -----------------------------------

df_eurostat_official_aea_raw <- read.csv(paste0(local_path_sources, "/AEA_eurostat/estat_env_ac_ainah_r2_filtered_en.csv"), dec = ",")

# ------------------------------ FORMATTING ------------------------------------

df_eurostat_official_aea <- df_eurostat_official_aea_raw %>%
  # basic formatting
  rename(gas_type = airpol,
         time_period = TIME_PERIOD, code_a64_eurostat_aea = nace_r2,
         emis_country = geo, ghg_emission = OBS_VALUE) %>%
  mutate(time_period = as.character(time_period),
         ghg_emission = as.numeric(ghg_emission)) %>%
  filter(gas_type %in% vc_gas_types) %>%
  # FIGARO country nomenclature
  mutate(emis_country = case_when(emis_country == "EL" ~ "GR", # for some reason, the code for Greece differs in FIGARO compared to the Eurostat usual code
                                  TRUE ~ emis_country)) %>%
  # FIGARO industry nomenclature
  left_join(df_nace_a64, by = "code_a64_eurostat_aea") %>%
  filter(!is.na(code_a64_figaro)) %>%
  # column selection
  select(gas_type, time_period, emis_country, emis_industry = code_a64_figaro, ghg_emission)

# ---------------------------------- SAVE --------------------------------------

write_rds(df_eurostat_official_aea, paste0(path_frmt_source, "/eurostat_official_aea.rds"))
