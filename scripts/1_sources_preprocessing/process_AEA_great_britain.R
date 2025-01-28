# SOURCE CONTENT : official Air Emission Accounts for the UK


# HOW TO UPDATE THIS SOURCE ? **************************************************

# link : https://www.ons.gov.uk/economy/environmentalaccounts/datasets/ukenvironmentalaccountsatmosphericemissionsgreenhousegasemissionsbyeconomicsectorandgasunitedkingdom

# Dataset : Atmospheric emissions: greenhouse gases by industry and gas

# As of January 2025, the name of the file is "provisionalatmoshpericemissionsghg.xlsx"
#  but the name may vary depending on wether the dataset is provisional or definitive

# PLEASE NOTE that the source file format changed between June 2024 (time series as rows)
#   and October 2024 (time series as columns)
# The load function below accommodates the new format
# The script may need further adjustments in the future if the source format changes again !

# ******************************************************************************


# ------------------------------- PARAMETERS -----------------------------------

vc_gas_types_detailled <- c("Co2", "CH4", "N2O", "HFC", "PFC", "NF3", "SF6")
ch4_gwp_gb_official_aea <- 28
n2o_gwp_gb_official_aea <- 265


# ------------------ BRIDGE FROM ONS SOURCE FILE TO A64 ------------------------

df_bridge_a64_to_AEA_uk <- read_xlsx("resources/bridge_a64_to_AEA_uk.xlsx")


# ----------------------- LOAD FUNCTION (from XLSX) ----------------------------

load_gb_official_aea_from_xlsx <- function(gas_type_g) {
  
  df_gb_official_aea <- read_xlsx(paste0(local_path_sources, "/AEA_great_britain/provisionalatmoshpericemissionsghg.xlsx"),
                                  sheet = gas_type_g,
                                  skip = 44) %>%
    # basic formatting
    rename(time_period = `SIC(07) group`) %>%
    filter(time_period != "Industry name") %>%
    # pivot ISIC groups to rows
    mutate(across(!time_period, as.character)) %>%
    pivot_longer(!time_period, names_to = "sic_07_group", values_to = "ghg_emission") %>%
    mutate(ghg_emission = as.numeric(ghg_emission),
           sic_a88 = case_when(sic_07_group %in% c("100", "101") ~ sic_07_group,
                               TRUE ~ str_sub(sic_07_group, 1, 2))) %>%
    # add gas_type information
    mutate(gas_type = toupper(gas_type_g))
  
}

# ------------------------------- LOAD CALL ------------------------------------

df_gb_official_aea_raw <- map(vc_gas_types_detailled, load_gb_official_aea_from_xlsx) %>%
  list_rbind()

# ------------------------------ FORMATTING ------------------------------------

df_gb_official_aea <- df_gb_official_aea_raw  %>%
  select(gas_type, time_period, sic_a88, ghg_emission) %>%
  # adding A64 nace level
  left_join(df_bridge_a64_to_AEA_uk %>%
              select(emis_industry, sic_a88) %>%
              distinct(),
            by = "sic_a88") %>%
  filter(!is.na(emis_industry)) %>%
  # regrouping NF3-SF6
  mutate(gas_type = if_else(gas_type %in% c("NF3", "SF6"), "NF3_SF6", gas_type)) %>%
  # agregation : emis_industry -> A64 and gas_type -> NF3-SF6
  group_by(gas_type, time_period, emis_industry) %>%
  summarise(ghg_emission = sum(ghg_emission)) %>%
  ungroup() %>%
  # convert CH4 and N2O from CO2 equivalent back to masses of each gas
  mutate(ghg_emission = case_when(gas_type == "CH4" ~ ghg_emission / ch4_gwp_gb_official_aea,
                                  gas_type == "N2O" ~ ghg_emission / n2o_gwp_gb_official_aea,
                                  TRUE ~ ghg_emission)) %>%
  # recoding F-gases names
  mutate(gas_type = if_else(gas_type %in% c("HFC", "PFC", "NF3_SF6"),
                            paste0(gas_type, "_CO2E"), gas_type)) %>%
  # adding country information
  mutate(emis_country = "GB")

# ---------------------------------- SAVE --------------------------------------

write_rds(df_gb_official_aea, paste0(path_frmt_source, "/gb_official_aea.rds"))
