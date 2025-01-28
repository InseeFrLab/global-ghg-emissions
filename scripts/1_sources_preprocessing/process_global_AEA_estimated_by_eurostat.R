# SOURCE CONTENT : global AEA estimated by Eurostat in the context of the FIGARO GHG footprint calculation

# HOW TO UPDATE THIS SOURCE ? **************************************************

# Data available here : https://ec.europa.eu/eurostat/web/main/data/database
# 
# In the "Data navigation tree" :
# Detailed datasets
#   Environment and energy
#     Environment (env)
#       Environmental footprints - modelling results (env_fp)
# 
# - Carbon dioxide emission footprints (FIGARO application) (env_ac_co2fp)
# - Methane emission footprints (CH4 in CO2 equivalent, FIGARO application) (env_ac_ch4fp)
# - Nitrous oxide emission footprints (N2O in CO2 equivalent, FIGARO application) (env_ac_n2ofp)
# - Hydrofluorocarbon emission footprints (HFC in CO2 equivalent, FIGARO application) (env_ac_hfcfp)
# - Perfluorocarbon emission footprints (PFC in CO2 equivalent, FIGARO application) (env_ac_pfcfp)
# - Nitrogen trifluoride and sulphur hexafluoride emission footprints (NF3-SF6 in CO2 equivalent, FIGARO application) (env_ac_nf3sf6fp)
# 
# CLICK DIRECTLY ON THE DOWNLOAD BUTTON from the navigation tree, without opening the databrowser
# This way, you directly get a .tsv file with the whole dataset

# ******************************************************************************


# ------------------------------- PARAMETERS -----------------------------------

vc_gas_types <- c("CO2", "CH4", "N2O", "HFC_CO2E", "PFC_CO2E", "NF3_SF6_CO2E")

# GWP coefficients used by Eurostat : see paragraph 3.2 in
#   https://ec.europa.eu/eurostat/cache/metadata/en/env_ac_ainah_r2_sims.htm#stat_pres1718207468349

ch4_gwp_figaro_footprint <- 28
n2o_gwp_figaro_footprint <- 265

# ------------------------------ NOMENCLATURE ----------------------------------

df_nace_a64 <- read_xlsx("resources/nomenclatures.xlsx", "cpa_nace_a64") %>%
  select(code_a64_figaro, code_a64_eurostat_aea)

vc_figaro_countries <- read_xlsx("resources/nomenclatures.xlsx", "figaro_countries") %>%
  pull(country_code_figaro)

# ----------------------------- LOAD FUNCTION ----------------------------------

read_figaro_footprint <- function(file_name){
  
  # User information
  print(paste0("Reading Eurostat's FIGARO footprint results - ", file_name))
  
  # Source load from .tsv
  df_figaro_footprint <- read_tsv(paste0(local_path_sources, "/global_AEA_estimated_by_eurostat/", file_name))
  
  # Colnames identification
  col_names <- str_split_1(names(df_figaro_footprint)[1], ",")
  col_names <- str_replace(col_names, "\\\\TIME_PERIOD", "")
  names(df_figaro_footprint)[1] <- "first_col"
  
  # Columns split
  df_figaro_footprint <- df_figaro_footprint %>%
    separate_wider_delim(first_col,
                         delim = ",",
                         names = col_names)
}


# ------------------------------- LOAD CALL ------------------------------------

df_figaro_footprint_CO2 <- read_figaro_footprint("estat_env_ac_co2fp.tsv") %>% mutate(gas_type = "CO2")
df_figaro_footprint_CH4 <- read_figaro_footprint("estat_env_ac_ch4fp.tsv") %>%
  mutate(across(where(is.numeric), ~ .x / ch4_gwp_figaro_footprint)) %>%
  mutate(gas_type = "CH4")
df_figaro_footprint_N2O <- read_figaro_footprint("estat_env_ac_n2ofp.tsv") %>%
  mutate(across(where(is.numeric), ~ .x / n2o_gwp_figaro_footprint)) %>%
  mutate(gas_type = "N2O")
df_figaro_footprint_HFC_CO2E <- read_figaro_footprint("estat_env_ac_hfcfp.tsv") %>% mutate(gas_type = "HFC_CO2E")
df_figaro_footprint_PFC_CO2E <- read_figaro_footprint("estat_env_ac_pfcfp.tsv") %>% mutate(gas_type = "PFC_CO2E")
df_figaro_footprint_NF3_SF6_CO2E <- read_figaro_footprint("estat_env_ac_nf3sf6fp.tsv") %>% mutate(gas_type = "NF3_SF6_CO2E")

df_figaro_footprint <- bind_rows(df_figaro_footprint_CO2,
                                 df_figaro_footprint_CH4,
                                 df_figaro_footprint_N2O,
                                 df_figaro_footprint_HFC_CO2E,
                                 df_figaro_footprint_PFC_CO2E,
                                 df_figaro_footprint_NF3_SF6_CO2E)


# --------- EXTRACTION OF THE GLOBAL AEA FROM THE FOOTPRINT DATASET ------------

df_global_aea_estimated_by_eurostat <- df_figaro_footprint %>%
  # basic formatting
  rename(emis_country = c_orig, code_a64_eurostat_aea = nace_r2) %>%
  filter(!code_a64_eurostat_aea %in% c("TOTAL", "TOTAL_HH"),
         c_dest == "WORLD", na_item == "TOTAL") %>%
  # agregating emissions by emis_country x emis_industry
  group_by(gas_type, emis_country, code_a64_eurostat_aea) %>%
  summarise(across(where(is.numeric), sum)) %>%
  ungroup() %>%
  # FIGARO country nomenclature
  mutate(emis_country = case_when(emis_country == "EL" ~ "GR",
                                  emis_country == "UK" ~ "GB",
                                  emis_country == "WRL_REST" ~ "FIGW1",
                                  TRUE ~ emis_country)) %>%
  filter(emis_country %in% vc_figaro_countries) %>%
  # FIGARO industry nomenclature + keeping the households total (HH)
  left_join(df_nace_a64, by = "code_a64_eurostat_aea") %>%
  filter(!is.na(code_a64_figaro)) %>%
  # pivot wider (years as rows instead of columns)
  pivot_longer(where(is.numeric), names_to = "time_period", values_to = "ghg_emission") %>%
  # column selection
  select(gas_type, time_period, emis_country, emis_industry = code_a64_figaro, ghg_emission)


# ---------------------------------- SAVE --------------------------------------

write_rds(df_global_aea_estimated_by_eurostat, paste0(path_frmt_source, "/global_aea_estimated_by_eurostat.rds"))
