# SOURCE CONTENT : OECD Air transport CO2 emissions

# HOW TO UPDATE THIS SOURCE ? **************************************************

# Data available here : https://data-explorer.oecd.org/
# 
# In the "OECD data explorer" :
# Environment and climate change
#   Air and Climate
#     Air transport CO2 emissions (experimental)

# Select the following dimensions :
#   Frequency of observation : Annual
#   Reference area           : select all categories
#   Flight types             : All flights
#   Emissions                : Air emission accounts : air transport
#   Methodology              : SEEA (residence principle)

# Then download the resulting dataset (will be a file in .csv format)

# ******************************************************************************

# ------------------------------ NOMENCLATURE ----------------------------------

df_oecd_figaro_codes_countries <- read_xlsx("resources/nomenclatures.xlsx", "figaro_countries") %>%
  select(country_code_figaro, country_code_oecd)

# ------------------------------ LOAD SOURCE -----------------------------------

df_oecd_air_transport_raw <- read_csv(paste0(local_path_sources, "/OECD_air_transport/OECD_air_transport_download_27_01_2025.csv"))

# ------------------------------ FORMATTING ------------------------------------

# Data formatting, countries at the 'alpha 3' level
df_oecd_air_transport_alpha3 <- df_oecd_air_transport_raw %>%
  filter(FREQ == "A",
         ! REF_AREA %in% c("OECD", "W")) %>%
  mutate(time_period = as.character(TIME_PERIOD),
         value = OBS_VALUE / 1000) %>% # convert to kT
  select(REF_AREA, time_period, value) %>%
  left_join(df_oecd_figaro_codes_countries, by = c("REF_AREA" = "country_code_oecd"))

# WORLD total excluding FIGARO 'Rest of the world'
df_oecd_air_transport_WORLD_excl_FIGW1 <- df_oecd_air_transport_alpha3 %>%
  filter(!is.na(country_code_figaro)) %>%
  group_by(time_period) %>%
  summarise(value_excl_FIGW1 = sum(value)) %>%
  ungroup()

# Computing FIGARO 'rest of the world' as a residual
df_oecd_air_transport_FIGW1 <- df_oecd_air_transport_alpha3 %>%
  group_by(time_period) %>%
  summarise(value_world = sum(value)) %>%
  ungroup() %>%
  left_join(df_oecd_air_transport_WORLD_excl_FIGW1, by = "time_period") %>%
  mutate(value_FIGW1 = value_world - value_excl_FIGW1)

# Row-binding : all FIGARO countries + 'Rest of the World'
df_oecd_air_transport <- df_oecd_air_transport_alpha3 %>%
  filter(!is.na(country_code_figaro)) %>%
  bind_rows(df_oecd_air_transport_FIGW1 %>%
              mutate(country_code_figaro = "FIGW1") %>%
              select(country_code_figaro, time_period, value = value_FIGW1)) %>%
  select(emis_country = country_code_figaro, time_period, oecd_value = value)

# ---------------------------------- SAVE --------------------------------------

write_rds(df_oecd_air_transport, paste0(path_frmt_source, "/oecd_air_transport.rds"))
