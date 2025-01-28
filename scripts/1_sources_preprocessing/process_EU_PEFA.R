# SOURCE CONTENT : physical energy flow accounts (PEFA) for the EU as a whole, products P14 and P17

# HOW TO UPDATE THIS SOURCE ? **************************************************

# Data available here : https://ec.europa.eu/eurostat/web/main/data/database
# 
# In the "Data navigation tree" :
# Detailed datasets
#   Environment and energy
#     Environment (env)
#       Physical energy flow accounts (env_pefa)
#         Energy supply and use by NACE Rev. 2 activity (env_ac_pefasu)

# OPEN THE DATABROWSER and select the following dimensions :
#  prod_nrg 	= 	P14 , P17 ("Motor spirit" and "Transport diesel")
#  geo 		    = 	EU27_2020
#  nace_r2 	  =   all available categories
#  stk_flow 	= 	USE
#  time       =   all available categories

# Then download the resulting dataset, with the following options :
#  "All selected dimensions"
#  "SDMX-CSV"
#  And in "Advanced settings", select "File format = SDMX-CSV 2.0"
#    to make sure you get codes and labels in separated columns

# The resulting file's name will typically be like : "estat_env_ac_pefasu_filtered_en.csv"

# ******************************************************************************


# ------------------------------ NOMENCLATURE ----------------------------------

df_nace_a64 <- read_xlsx("resources/nomenclatures.xlsx", "cpa_nace_a64")

vc_a64_industries_and_HH <- unique(df_nace_a64$code_a64_figaro)

df_eurostat_figaro_codes_a64 <- df_nace_a64 %>%
  filter(!grepl("HH", code_a64_figaro)) %>%
  select(code_a64_figaro, code_a64_eurostat_aea)

# ------------------------------ LOAD SOURCE -----------------------------------

df_EU27_pefa_use_P14_P17_raw <- read_csv(paste0(local_path_sources, "/EU_PEFA/estat_env_ac_pefasu_filtered_en.csv"))

# ------------------------------ FORMATTING ------------------------------------

df_EU27_pefa_use_P14_P17 <- df_EU27_pefa_use_P14_P17_raw %>%
  # formatting
  mutate(time_period = as.character(TIME_PERIOD)) %>%
  filter(geo == "EU27_2020") %>%
  left_join(df_eurostat_figaro_codes_a64, by = c("nace_r2" = "code_a64_eurostat_aea")) %>%
  select(time_period, emis_industry = code_a64_figaro, prod_nrg, nrg_consumption = OBS_VALUE) %>%
  filter(emis_industry %in% vc_a64_industries_and_HH) %>%
  # aggregation of the 2 products P14 and P17
  group_by(time_period, emis_industry) %>%
  summarise(nrg_consumption = sum(nrg_consumption)) %>%
  ungroup()

# ---------------------------------- SAVE --------------------------------------

write_rds(df_EU27_pefa_use_P14_P17, paste0(path_frmt_source, "/EU27_pefa_use_P14_P17.rds"))
