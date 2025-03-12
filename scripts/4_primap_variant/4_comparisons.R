# ---------------------------- PARAMETERS --------------------------------------

vc_estimation_years <- as.character(seq(2010, 2022))


# --------------------------- NOMENCLATURES ------------------------------------

df_countries <- read_xlsx("resources/nomenclatures.xlsx", "figaro_countries") %>%
  mutate(annex1 = as.logical(annex1))

vc_fully_estimated_countries <- df_countries %>%
  filter(eurostat_aea_available == "not_avlbl", country_code_figaro != "GB") %>%
  pull(country_code_figaro)


# ------------------------------------------------------------------------------
# ---------------------------- LOADING SOURCES ---------------------------------
# ------------------------------------------------------------------------------

df_global_aea <- read_rds(paste0(path_results,"/global_aea_based_on_edgar_and_primap.rds")) %>%
  filter(emis_country %in% vc_fully_estimated_countries)


# ------------------------------------------------------------------------------
# ------------------------------- COMPARISONS ----------------------------------
# ------------------------------------------------------------------------------

# --------------- COMPARISON OF TOTAL EMISSIONS BY INDUSTRY---------------------

# Computing total emissions by industry

df_comp_totals_by_industry <- df_global_aea %>%
  group_by(scenario, gas_type, time_period, emis_industry) %>%
  summarise(ghg_emission = sum(ghg_emission)) %>%
  ungroup()

# PLOTS - comparaison in absolute level for one reference year

ref_year <- '2015'

ggplot(df_comp_totals_by_industry %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "CO2"),
       aes(x = emis_industry, y = ghg_emission, fill = scenario)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - CO2"))

ggplot(df_comp_totals_by_industry %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "CH4"),
       aes(x = emis_industry, y = ghg_emission, fill = scenario)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - CH4"))

ggplot(df_comp_totals_by_industry %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "N2O"),
       aes(x = emis_industry, y = ghg_emission, fill = scenario)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - N2O"))

ggplot(df_comp_totals_by_industry %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "HFC_CO2E"),
       aes(x = emis_industry, y = ghg_emission, fill = scenario)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - HFC"))

ggplot(df_comp_totals_by_industry %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "PFC_CO2E"),
       aes(x = emis_industry, y = ghg_emission, fill = scenario)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - PFC"))

ggplot(df_comp_totals_by_industry %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "NF3_SF6_CO2E"),
       aes(x = emis_industry, y = ghg_emission, fill = scenario)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - NF3_SF6"))


# PLOTS - comparison for the whole time series (industries from A to H)

# CO2 : Significantly different trend on C20 (CR vs. the other two)
ggplot(df_comp_totals_by_industry %>%
         filter(emis_industry < "I") %>%
         filter(gas_type == "CO2"),
       aes(x = time_period, y = ghg_emission, color = scenario, group = scenario)) +
  geom_line() +
  facet_wrap(vars(emis_industry), scales = "free") +
  ggtitle("Total emission by industry - CO2 (kT)")

# CH4
ggplot(df_comp_totals_by_industry %>%
         filter(emis_industry < "I") %>%
         filter(gas_type == "CH4"),
       aes(x = time_period, y = ghg_emission, color = scenario, group = scenario)) +
  geom_line() +
  facet_wrap(vars(emis_industry), scales = "free") +
  ggtitle("Total emission by industry - CH4 (kT)")

# N2O : Unreliable trends 2010 to 2014 for CR
ggplot(df_comp_totals_by_industry %>%
         filter(emis_industry < "I") %>%
         filter(gas_type == "N2O"),
       aes(x = time_period, y = ghg_emission, color = scenario, group = scenario)) +
  geom_line() +
  facet_wrap(vars(emis_industry), scales = "free") +
  ggtitle("Total emission by industry - N2O (kT)")


# ---------------- COMPARISON OF TOTAL EMISSIONS BY COUNTRY---------------------

# Computing countries total emissions

df_comp_totals_by_country <- df_global_aea %>%
  group_by(scenario, gas_type, time_period, emis_country) %>%
  summarise(ghg_emission = sum(ghg_emission)) %>%
  ungroup()

# PLOTS

ggplot(df_comp_totals_by_country %>%
         filter(gas_type == "CO2"),
       aes(x = time_period, y = ghg_emission, color = scenario, group = scenario)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("CO2 (kT) - Total emission by country, incl. HH")

ggplot(df_comp_totals_by_country %>%
         filter(gas_type == "CH4"),
       aes(x = time_period, y = ghg_emission, color = scenario, group = scenario)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("CH4 (kT) - Total emission by country, incl. HH")

ggplot(df_comp_totals_by_country %>%
         filter(gas_type == "N2O"),
       aes(x = time_period, y = ghg_emission, color = scenario, group = scenario)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("N2O (kT) - Total emission by country, incl. HH")

ggplot(df_comp_totals_by_country %>%
         filter(gas_type == "HFC_CO2E"),
       aes(x = time_period, y = ghg_emission, color = scenario, group = scenario)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("HFC (kT CO2eq) - Total emission by country, incl. HH")

ggplot(df_comp_totals_by_country %>%
         filter(gas_type == "PFC_CO2E"),
       aes(x = time_period, y = ghg_emission, color = scenario, group = scenario)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("PFC (kT CO2eq) - Total emission by country, incl. HH")

ggplot(df_comp_totals_by_country %>%
         filter(gas_type == "NF3_SF6_CO2E"),
       aes(x = time_period, y = ghg_emission, color = scenario, group = scenario)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("NF3_SF6 (kT CO2eq) - Total emission by country, incl. HH")
