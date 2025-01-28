# ---------------------------- LOAD GLOBAL AEA ---------------------------------

df_global_aea <- read_rds(paste0(path_results,"/global_aea_replicate_eurostat.rds"))


# ------------------------- LOAD EDGAR DATABASE --------------------------------

df_edgar_emissions <- read_rds(paste0(path_frmt_source, "/edgar_emissions.rds"))


# -------------------------- LOAD NOMENCLATURES --------------------------------

df_countries <- read_xlsx("resources/nomenclatures.xlsx", "figaro_countries")

vc_fully_estimated_countries <- df_countries %>%
  filter(eurostat_aea_available == "not_avlbl", country_code_figaro != "GB") %>%
  pull(country_code_figaro)


# --------------------------- CORRECT COLNAMES ---------------------------------

expected_colnames <- c("gas_type", "time_period", "emis_country", "emis_industry", "ghg_emission")
actual_colnames <- colnames(df_global_aea)

unexpected_colnames <- actual_colnames[!actual_colnames %in% expected_colnames]
missing_colnames <- expected_colnames[!expected_colnames %in% actual_colnames]

if(length(unexpected_colnames) > 0) stop(paste0("UNEXPECTED COLNAMES : ", unexpected_colnames))
if(length(missing_colnames) > 0) stop(paste0("MISSING COLNAMES : ", missing_colnames))


# ------------------------------ NO NA's ---------------------------------------

df_NAs_gas_type <- df_global_aea %>% filter(is.na(gas_type))
df_NAs_time_period <- df_global_aea %>% filter(is.na(time_period))
df_NAs_emis_country <- df_global_aea %>% filter(is.na(emis_country))
df_NAs_emis_industry <- df_global_aea %>% filter(is.na(emis_industry))
df_NAs_ghg_emission <- df_global_aea %>% filter(is.na(ghg_emission))

if(nrow(df_NAs_gas_type)) stop(paste0(nrow(df_NAs_gas_type), " rows with NAs in gas_type"))
if(nrow(df_NAs_time_period)) stop(paste0(nrow(df_NAs_time_period), " rows with NAs in time_period"))
if(nrow(df_NAs_emis_country)) stop(paste0(nrow(df_NAs_emis_country), " rows with NAs in emis_country"))
if(nrow(df_NAs_emis_industry)) stop(paste0(nrow(df_NAs_emis_industry), " rows with NAs in emis_industry"))
if(nrow(df_NAs_ghg_emission)) stop(paste0(nrow(df_NAs_ghg_emission), " rows with NAs in ghg_emission"))


# --------------------------- NUMBER OF MODALITIES -----------------------------

num_gas_types <- length(unique(df_global_aea$gas_type))
num_emis_countries <- length(unique(df_global_aea$emis_country))
num_emis_industries <- length(unique(df_global_aea$emis_industry))

if(num_gas_types != 6) stop("There should be exactly 6 different gas types")
if(num_emis_countries != 46) stop("There should be exactly 46 emitting countries")
if(num_emis_industries != 65) stop("There should be exactly 65 emitting industries (64 nace + households)")


# -------------- TOTAL EMISSIONS FOR FULLY ESTIMATED AEAs = EDGAR --------------

# Only for GHGs other than CO2, because CO2 totals have been modified by AIR and SEA transport OECD data

df_total_emissions_from_aea <- df_global_aea %>%
  filter(emis_country %in% vc_fully_estimated_countries) %>%
  group_by(gas_type, time_period, emis_country) %>%
  summarise(total_aea = sum(ghg_emission)) %>%
  ungroup()

df_total_emissions_from_edgar <- df_edgar_emissions %>%
  filter(emis_country %in% vc_fully_estimated_countries) %>%
  group_by(gas_type, time_period, emis_country) %>%
  summarise(total_edgar = sum(ghg_emission)) %>%
  ungroup()

df_comp_aea_vs_edgar <- full_join(df_total_emissions_from_aea, df_total_emissions_from_edgar,
                                  by = c("gas_type", "time_period", "emis_country")) %>%
  mutate(diff = total_aea - total_edgar)

df_diff_aea_vs_edgar <- df_comp_aea_vs_edgar %>%
  filter(gas_type != "CO2", abs(diff) > 0.001)
  
if(nrow(df_diff_aea_vs_edgar)) stop(paste0(nrow(df_diff_aea_vs_edgar), " rows where AEA and EDGAR total emissions differ"))


# ----------------------------- OTHER TESTS ... --------------------------------

# There are certainly other useful tests to write ...
