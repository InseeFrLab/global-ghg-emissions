# -------------------------------- PARAMETERS ----------------------------------

vc_estimation_years <- as.character(seq(2010, 2022))


# ------------------------------- NOMENCLATURES --------------------------------

df_CRF_primap_vs_edgar <- read_xlsx("resources/CRF_primap_vs_edgar.xlsx", "CRF_primap_vs_edgar") %>%
  select(EDGAR_CRF_level, PRIMAP_CRF_level)

vc_figaro_countries <- read_xlsx("resources/nomenclatures.xlsx", "figaro_countries") %>%
  pull(country_code_figaro) %>%
  unique()

vc_usefull_primap_categories_excl_2 <- df_CRF_primap_vs_edgar %>%
  pull(PRIMAP_CRF_level) %>%
  unique()

df_CRF_primap_by_default <- read_xlsx("resources/CRF_primap_vs_edgar.xlsx", "default_CRF_when_missing") %>%
  select(gas_type, PRIMAP_CRF_level, EDGAR_CRF_level_default)


# ------------------------------------------------------------------------------
# ------------------------------ SOURCES LOAD ----------------------------------
# ------------------------------------------------------------------------------

df_primap_emissions <- read_rds(paste0(path_frmt_source, "/primap_emissions.rds")) %>%
  filter(time_period %in% vc_estimation_years) %>%
  # filtering to only the countries and ipcc categories strictly necessary
  filter(emis_country %in% vc_figaro_countries,
         category %in% vc_usefull_primap_categories_excl_2 |
           (gas_type %in% c("HFC_CO2E", "PFC_CO2E", "NF3_SF6_CO2E") & category == "2"))

df_edgar_emissions <- read_rds(paste0(path_frmt_source, "/edgar_emissions.rds")) %>%
  filter(time_period %in% vc_estimation_years)


# ------------------------------------------------------------------------------
# ------------ PRIMAP EXPANSION TO MATCH EDGAR's CRF LEVEL OF DETAIL -----------
# ------------------------------------------------------------------------------

df_edgar_emissions_with_primap_crf_level <- df_edgar_emissions %>%
  rename(edgar_ghg_emission = ghg_emission) %>%
  # bridge table for CO2, CH4 and N2O
  left_join(df_CRF_primap_vs_edgar, by = c("ipcc_code_2006" = "EDGAR_CRF_level")) %>%
  # bridge table for F-gases
  mutate(PRIMAP_CRF_level = if_else(gas_type %in% c("HFC_CO2E", "PFC_CO2E", "NF3_SF6_CO2E"),
                                    "2",
                                    PRIMAP_CRF_level))

df_primap_emissions_expanded_without_default <- df_primap_emissions  %>%
  # expanding to EDGAR level of detail
  rename(primap_ghg_emission_agr = ghg_emission) %>%
  left_join(df_edgar_emissions_with_primap_crf_level,
            by = c("emis_country", "gas_type", "time_period", "category" = "PRIMAP_CRF_level"),
            relationship = "many-to-many")

# Some entries in PRIMAP do not have a correspondence in EDGAR !
# This is the case in particular for :
#   1.B.3 (for CO2 and CH4)
#   2.H (for CO2 and CH4)
#   M.AG.ELV (for CH4)
#   1.B.1 (for N2O)
# The overall amounts are very small at the global level
# Still we choose to put some 'default' allocations

replace_all_0s_with_1s <- function(vc_edgar_ghg_emission){
  if(all(vc_edgar_ghg_emission == 0)) vc_edgar_ghg_emission <- rep(1, length(vc_edgar_ghg_emission))
  return(vc_edgar_ghg_emission)
}

df_primap_emissions_expanded_from_EDGAR <- df_primap_emissions_expanded_without_default %>%
  filter(!is.na(ipcc_code_2006)) %>%
  # allocating emissions from PRIMAP to EDGAR's level of detail
  group_by(scenario, emis_country, gas_type, time_period, category) %>%
  mutate(edgar_ghg_emission = replace_all_0s_with_1s(edgar_ghg_emission), # to avoid 'NaN' when computing the allocation key below
         primap_ghg_emission = primap_ghg_emission_agr * edgar_ghg_emission / sum(edgar_ghg_emission)) %>%
  ungroup()

df_primap_emissions_expanded_by_default <- df_primap_emissions_expanded_without_default %>%
  filter(is.na(ipcc_code_2006)) %>%
  select(-ipcc_code_2006, -edgar_ghg_emission) %>%
  # adding a default CRF matching with EDGAR level of detail when there is no correspondence
  left_join(df_CRF_primap_by_default, by = c("gas_type", "category" = "PRIMAP_CRF_level"))


# ------------------------------------------------------------------------------
# -------------------------------- FORMATTING ----------------------------------
# ------------------------------------------------------------------------------

df_primap_emissions_expanded <- bind_rows(df_primap_emissions_expanded_from_EDGAR %>%
                                            select(scenario, emis_country, gas_type, time_period, ipcc_code_2006, primap_ghg_emission),
                                          df_primap_emissions_expanded_by_default %>%
                                            select(scenario, emis_country, gas_type, time_period,
                                                   ipcc_code_2006 = EDGAR_CRF_level_default,
                                                   primap_ghg_emission = primap_ghg_emission_agr)) %>%
  # re-agregating (because some rows may be doubled)
  group_by(scenario, emis_country, gas_type, time_period, ipcc_code_2006) %>%
  summarise(primap_ghg_emission = sum(primap_ghg_emission)) %>%
  ungroup()

# ------------------------------------------------------------------------------
# ---------------------------------- SAVING ------------------------------------
# ------------------------------------------------------------------------------

write_rds(df_primap_emissions_expanded,
          paste0(path_results,"/df_primap_emissions_expanded.rds"))


# ------------------------------------------------------------------------------
# ---------------------------------- TESTS -------------------------------------
# ------------------------------------------------------------------------------

# Test that total PRIMAP emissions have not been changed during the procedure

df_primap_emissions_expanded_totals <- df_primap_emissions_expanded %>%
  group_by(scenario, emis_country, gas_type, time_period) %>%
  summarise(total_after_expansion = sum(primap_ghg_emission)) %>%
  ungroup()

df_primap_emissions_non_expanded_totals <- df_primap_emissions %>%
  group_by(scenario, emis_country, gas_type, time_period) %>%
  summarise(total_before_expansion = sum(ghg_emission)) %>%
  ungroup()

df_primap_emissions_comp_totals <- full_join(df_primap_emissions_expanded_totals,
                                             df_primap_emissions_non_expanded_totals,
                                             by = join_by(scenario, emis_country, gas_type, time_period)) %>%
  mutate(diff = total_after_expansion - total_before_expansion)

# Test result : OK, no difference and no NAs