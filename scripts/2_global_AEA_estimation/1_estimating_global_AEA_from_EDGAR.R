# -------------------------------- PARAMETERS ----------------------------------

vc_estimation_years <- as.character(seq(2010, 2022))


# ------------------------------- NOMENCLATURES --------------------------------

df_countries <- read_xlsx("resources/nomenclatures.xlsx", "figaro_countries") %>%
  mutate(annex1 = as.logical(annex1))

vc_complete_eurostat_aea <- df_countries %>%
  filter(eurostat_aea_available == "complete") %>%
  pull(country_code_figaro)

vc_incomplete_eurostat_aea <- df_countries %>%
  filter(eurostat_aea_available == "incomplete") %>%
  pull(country_code_figaro)

vc_aea_estimation_countries <- df_countries %>%
  filter(eurostat_aea_available != "complete", country_code_figaro != "GB") %>%
  pull(country_code_figaro)

vc_annex1_countries <- df_countries %>%
  filter(annex1) %>%
  pull(country_code_figaro)

vc_EU27_countries <- df_countries %>%
  filter(EU27_esa_code == "SIS21") %>%
  pull(country_code_figaro)


# ------------------------------------------------------------------------------
# ------------------------------ SOURCES LOAD ----------------------------------
# ------------------------------------------------------------------------------

df_eurostat_official_aea <- read_rds(paste0(path_frmt_source, "/eurostat_official_aea.rds"))
df_gb_official_aea <- read_rds(paste0(path_frmt_source, "/gb_official_aea.rds"))

df_edgar_emissions <- read_rds(paste0(path_frmt_source, "/edgar_emissions.rds"))

df_figaro_output <- read_rds(paste0(path_frmt_source, "/figaro_output.rds"))

df_oecd_air_transport <- read_rds(paste0(path_frmt_source, "/oecd_air_transport.rds"))
df_oecd_maritime_transport <- read_rds(paste0(path_frmt_source, "/oecd_maritime_transport.rds"))

df_EU27_pefa_use_P14_P17 <- read_rds(paste0(path_frmt_source, "/EU27_pefa_use_P14_P17.rds"))


# ------------------------------------------------------------------------------
# -------------------- CRF -> NACE ALLOCATION COEFFICIENTS ---------------------
# ------------------------------------------------------------------------------

# Households coefficients, applicable to all countries -------------------------

df_HH_global_coeff <- read_xlsx("resources/HH_global_coeff.xlsx") %>%
  # recoding F-gases names
  mutate(gas_type = if_else(gas_type %in% c("HFC", "PFC", "NF3_SF6"),
                            paste0(gas_type, "_CO2E"), gas_type))

# Coefficients CRF 1.A.4.b / 1.A.4 ---------------------------------------------

df_HH_residential_coeff <- read_xlsx("resources/HH_residential_coeff_CRF_1A4.xlsx")

# Main bridge tables CRF <-> NACE ----------------------------------------------

load_CRF_allocation_to_NACE_from_xls <- function(gas_type_g) {
  df_CRF_allocation_to_NACE <- read_xlsx("resources/CRF_allocation_to_NACE.xlsx", gas_type_g) %>%
    mutate(gas_type = gas_type_g)
}

vc_gas_types <- c("CO2", "CH4", "N2O", "HFC", "PFC", "NF3_SF6")

df_CRF_allocation_to_NACE <- map(vc_gas_types, load_CRF_allocation_to_NACE_from_xls) %>%
  list_rbind() %>%
  filter(code_a64_figaro != "HH") %>%
  select(gas_type, ipcc_code_2006, emis_industry = code_a64_figaro) %>%
  # recoding F-gases names
  mutate(gas_type = if_else(gas_type %in% c("HFC", "PFC", "NF3_SF6"),
                            paste0(gas_type, "_CO2E"), gas_type))



# ------------------------------------------------------------------------------
# -------------------------- GLOBAL AEA ESTIMATION -----------------------------
# ------------------------------------------------------------------------------


# SPLIT HOUSEHOLDS vs. INDUSTRIES ----------------------------------------------

# Split HH vs industries
df_edgar_HH_vs_industries <- df_edgar_emissions %>%
  filter(emis_country %in% vc_aea_estimation_countries,
         time_period %in% vc_estimation_years) %>%
  # Non country specific HH coefficients 
  left_join(df_HH_global_coeff %>%
              filter(!is.na(HH_coeff)) %>%
              select(gas_type, ipcc_code_2006, HH_coeff_global = HH_coeff),
            by = c("gas_type", "ipcc_code_2006")) %>%
  # Country specific HH coefficients for 1.A.4 (residential HH vs. tertiary industries)
  mutate(emis_country_1A4 = if_else(! emis_country %in% vc_annex1_countries,
                                    "EU27", emis_country)) %>%
  left_join(df_HH_residential_coeff %>%
              filter(time_period == "2021") %>%
              mutate(HH_coeff_1A4 = unfccc_1A4b / unfccc_1A4,
                     ipcc_code_2006 = "1.A.4") %>%
              select(gas_type, emis_country_1A4 = emis_country, ipcc_code_2006, HH_coeff_1A4),
            by = c("gas_type", "emis_country_1A4", "ipcc_code_2006")) %>%
  # Synthesis
  mutate(HH_coeff_global = replace_na(HH_coeff_global, 0),
         HH_coeff_1A4 = replace_na(HH_coeff_1A4, 0),
         HH_coeff =  HH_coeff_global + HH_coeff_1A4,
         industries_coeff = 1 - HH_coeff)

# CTRL that the bridge table is complete
df_edgar_HH_vs_industries_NA <- df_edgar_HH_vs_industries %>%
  filter(is.na(industries_coeff))

if (nrow(df_edgar_HH_vs_industries_NA) > 0) stop("Bridge table CRF -> NACE / HH incomplete")

# Total HH results
df_edgar_HH <- df_edgar_HH_vs_industries %>%
  filter(HH_coeff > 0) %>%
  mutate(ghg_emission = HH_coeff * ghg_emission) %>%
  select(emis_country, ipcc_code_2006, gas_type, time_period, ghg_emission)

df_estimated_aea_HH <- df_edgar_HH %>%
  group_by(time_period, gas_type, emis_country) %>%
  summarise(ghg_emission = sum(ghg_emission)) %>%
  ungroup() %>%
  mutate(emis_industry = "HH")

# Total industries results
df_edgar_industries <- df_edgar_HH_vs_industries %>%
  filter(industries_coeff > 0) %>%
  mutate(ghg_emission = industries_coeff * ghg_emission) %>%
  select(emis_country, ipcc_code_2006, gas_type, time_period, ghg_emission)


# INDUSTRIES ALLOCATION TO NACE A64 --------------------------------------------

# Emission intensities by industry in the EU27 (= reference)

df_EU27_figaro_output <- df_figaro_output %>%
  filter(sply_country %in% vc_EU27_countries) %>%
  group_by(time_period, sply_industry) %>%
  summarise(output = sum(output)) %>%
  ungroup()

df_EU27_aea <- df_eurostat_official_aea %>%
  filter(emis_country %in% vc_EU27_countries) %>%
  group_by(time_period, gas_type, emis_industry) %>%
  summarise(ghg_emission = sum(ghg_emission)) %>%
  ungroup()

df_EU27_avg_emission_intensity <- full_join(df_EU27_figaro_output %>% rename(emis_industry = sply_industry),
                                            df_EU27_aea,
                                            by = c("time_period", "emis_industry")) %>%
  # average over the 2014-2018 period (cf. methodological footnote number 7)
  filter(time_period %in% as.character(seq(2014, 2018))) %>%
  group_by(gas_type, emis_industry) %>%
  summarise(ghg_emission = sum(ghg_emission),
            output = sum(output)) %>%
  ungroup() %>%
  # computing intensity
  mutate(emission_intensity = ghg_emission / output)

# Special case Road transport : intensity of fuel consumption in the EU27

df_EU27_avg_road_fuel_intensity <- full_join(df_EU27_figaro_output %>% rename(emis_industry = sply_industry),
                                             df_EU27_pefa_use_P14_P17,
                                             by = c("time_period", "emis_industry")) %>%
  # average over the 2014-2018 period (cf. methodological footnote number 7)
  filter(time_period %in% as.character(seq(2014, 2018))) %>%
  group_by(emis_industry) %>%
  summarise(nrg_consumption = sum(nrg_consumption),
            output = sum(output)) %>%
  ungroup() %>%
  # computing intensity
  mutate(nrg_intensity = nrg_consumption / output,
         ipcc_code_2006 = "1.A.3.b_noRES")

# Average output by industry - 2014-2018 average

df_avg_figaro_output <- df_figaro_output %>%
  filter(time_period %in% as.character(seq(2014, 2018))) %>%
  select(emis_country = sply_country, emis_industry = sply_industry, output) %>%
  group_by(emis_country, emis_industry) %>%
  summarise(output = sum(output)) %>%
  ungroup()

# Allocation CRF -> NACE (where the magic happens)

df_estimated_aea_industries <- df_edgar_industries %>%
  rename(emission_all_industries = ghg_emission) %>%
  # expanding along the NACE A64 nomenclature
  left_join(df_CRF_allocation_to_NACE, by = c("gas_type", "ipcc_code_2006"), relationship = "many-to-many") %>%
  filter(!is.na(emis_industry)) %>%
  # adding national output (avg 2014-2018)
  left_join(df_avg_figaro_output %>% select(emis_country, emis_industry, output),
            by = c("emis_country", "emis_industry")) %>%
  # adding EU27 emission intensity
  left_join(df_EU27_avg_emission_intensity %>% select(gas_type, emis_industry, EU27_intensity = emission_intensity),
            by = c("gas_type", "emis_industry")) %>%
  # special case Road transport (1.A.3.b_noRES) - adding intensity of fuel consumption in the EU27
  left_join(df_EU27_avg_road_fuel_intensity %>% select(ipcc_code_2006, emis_industry, EU27_intensity_1A3b_noRES = nrg_intensity),
            by = c("ipcc_code_2006", "emis_industry")) %>%
  # emission breakdown by NACE industry
  mutate(distr_key = case_when(ipcc_code_2006 == "1.A.3.b_noRES" ~ output * EU27_intensity_1A3b_noRES,
                               ipcc_code_2006 == "1.A.3.a" ~ 1,
                               TRUE ~ output * EU27_intensity)) %>%
  group_by(emis_country, ipcc_code_2006, gas_type, time_period) %>%
  mutate(ghg_emission = emission_all_industries * distr_key / sum(distr_key)) %>%
  ungroup()

# Agregation to NACE A64 (dropping the IPCC codes)

df_estimated_aea_a64_before_air_sea_transp <- df_estimated_aea_industries %>%
  group_by(time_period, gas_type, emis_country, emis_industry) %>%
  summarise(ghg_emission = sum(ghg_emission)) %>%
  ungroup()

# ALLOCATION OF CO2 INTERNATIONAL MARITIME TRANSPORT ---------------------------

#  Comes as a complement (not a replacement) to H50 emissions from 1.A.3.d (inner navigation only)
#  Here we make an assumption : years not covered in the OECD dataset are modelled after year 2019 (= fixed structure)

vc_oecd_maritime_transport_missing_years <- vc_estimation_years[! vc_estimation_years %in% unique(df_oecd_maritime_transport$time_period)]

df_oecd_maritime_transport_missing_years <- df_oecd_maritime_transport %>%
  filter(time_period == "2019") %>%
  select(emis_country, oecd_value) %>%
  expand_grid(time_period = vc_oecd_maritime_transport_missing_years)

df_oecd_maritime_transport_all_years <- df_oecd_maritime_transport %>%
  bind_rows(df_oecd_maritime_transport_missing_years)

# Allocation of the EDGAR total following the OECD distribution key by country
df_H50_international <- df_edgar_emissions %>%
  # World total "marine bunker" in EDGAR
  filter(emis_country == "EDGAR_SEA", gas_type == "CO2",
         time_period %in% vc_estimation_years) %>%
  select(time_period, global_emission = ghg_emission) %>%
  # adding the OECD table
  left_join(df_oecd_maritime_transport_all_years, by = "time_period") %>%
  # emission allocation
  group_by(time_period) %>%
  mutate(ghg_emission = global_emission * oecd_value / sum(oecd_value)) %>%
  ungroup()

# ALLOCATION OF CO2 AIR TRANSPORT ----------------------------------------------

#  Comes as a replacement for the H51 emissions from 1.A.3.a
#  But for the years before 2013, not covered in the OECD dataset,
#    we keep the same yoy % change as in the edgar dataset

df_H51_from_1.A.3.a <- df_estimated_aea_a64_before_air_sea_transp %>%
  filter(emis_industry == "H51_from_1.A.3.a") %>%
  rename(from_edgar_value = ghg_emission) %>%
  left_join(df_oecd_air_transport, by = c("time_period", "emis_country")) %>%
  group_by(emis_country) %>%
  mutate(ghg_emission = if_else(!is.na(oecd_value),
                                oecd_value,
                                from_edgar_value * sum(oecd_value * (time_period == "2013"), na.rm = TRUE) / sum(from_edgar_value * (time_period == "2013")))) %>%
  ungroup()

# ROW-BINDING INDUSTRIES + MARITIME + AIR --------------------------------------

df_estimated_aea_a64 <- df_estimated_aea_a64_before_air_sea_transp %>%
  # H50 : adding OECD maritime transport
  bind_rows(df_H50_international %>%
              filter(emis_country %in% vc_aea_estimation_countries) %>%
              mutate(gas_type = "CO2", emis_industry = "H50") %>%
              select(time_period, gas_type, emis_country, emis_industry, ghg_emission)) %>%
  # H51 : replacing by OECD air transport
  filter(emis_industry != "H51_from_1.A.3.a") %>%
  bind_rows(df_H51_from_1.A.3.a %>%
              filter(emis_country %in% vc_aea_estimation_countries) %>%
              mutate(emis_industry = "H51") %>%
              select(time_period, gas_type, emis_country, emis_industry, ghg_emission)) %>%
  # re-agregate to A64 (because rows for H50 et H51 are now doubled)
  group_by(time_period, gas_type, emis_country, emis_industry) %>%
  summarise(ghg_emission = sum(ghg_emission)) %>%
  ungroup()

# Row binding households + industries
df_estimated_aea <- bind_rows(df_estimated_aea_HH, df_estimated_aea_a64)

# ------------------------------------------------------------------------------
# --------------------- GAP FILLING of INCOMPLETE AEAs  ------------------------
# ------------------------------------------------------------------------------

# For CH, NO and TR
#  !!! for now, we have not implemented the gap-filling procedure described in the Eurostat's methodological note
#  so we stick to the results of the previous estimation !!!

df_incomplete_aea <- df_estimated_aea %>%
  filter(emis_country %in% vc_incomplete_eurostat_aea)

# Completely missing years -----------------------------------------------------

#  TBD

# Partially missing years ------------------------------------------------------

#  TBD

# ------------------------------------------------------------------------------
# -------------------------------- FORMATTING ----------------------------------
# ------------------------------------------------------------------------------

# AEAs fully available from Eurostat
df_complete_eurostat_aea <- df_eurostat_official_aea %>%
  filter(emis_country %in% vc_complete_eurostat_aea,
         time_period %in% vc_estimation_years) %>%
  #  We put 'U' to zero, as in Eurostat's own estimates
  mutate(ghg_emission = if_else(emis_industry == "U", 0, ghg_emission))

# AEAs fully estimated
df_fully_estimated_aea <- df_estimated_aea %>%
  filter(! emis_country %in% vc_incomplete_eurostat_aea)

# Row-binding all countries
df_global_aea <- bind_rows(df_complete_eurostat_aea,
                           df_incomplete_aea,
                           df_fully_estimated_aea,
                           df_gb_official_aea %>%
                             filter(time_period %in% vc_estimation_years))

# ------------------------------------------------------------------------------
# ---------------------------------- SAVING ------------------------------------
# ------------------------------------------------------------------------------

write_rds(df_global_aea,
          paste0(path_results,"/global_aea_replicate_eurostat.rds"))
