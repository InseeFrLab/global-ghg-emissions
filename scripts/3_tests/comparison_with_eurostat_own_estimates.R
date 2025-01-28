# ---------------------------- PARAMETERS --------------------------------------

vc_estimation_years <- as.character(seq(2010, 2022))


# --------------------------- NOMENCLATURES ------------------------------------

df_countries <- read_xlsx("resources/nomenclatures.xlsx", "figaro_countries") %>%
  mutate(annex1 = as.logical(annex1))

vc_complete_eurostat_aea <- df_countries %>%
  filter(eurostat_aea_available == "complete") %>%
  pull(country_code_figaro)

vc_aea_full_estimation_countries <- df_countries %>%
  filter(eurostat_aea_available == "not_avlbl") %>%
  pull(country_code_figaro)

vc_EU27_countries <- df_countries %>%
  filter(EU27_esa_code == "SIS21") %>%
  pull(country_code_figaro)


# ------------------------------------------------------------------------------
# ---------------------------- LOADING SOURCES ---------------------------------
# ------------------------------------------------------------------------------

# AEAs estimated by EUROSTAT ---------------------------------------------------

df_global_aea_estimated_by_eurostat <- read_rds(paste0(path_frmt_source, "/global_aea_estimated_by_eurostat.rds"))

# AEAs estimated in this project -----------------------------------------------

df_global_aea_replication <- read_rds(paste0(path_results,"/global_aea_replicate_eurostat.rds"))


# ------------------------------------------------------------------------------
# ------------------------------- COMPARISONS ----------------------------------
# ------------------------------------------------------------------------------

# Joining the two datasets

df_comp_global_aea <- full_join(df_global_aea_estimated_by_eurostat %>% rename(eurostat = ghg_emission),
                                df_global_aea_replication %>% rename(replication = ghg_emission),
                                by = c("gas_type", "time_period", "emis_country", "emis_industry"))

# Countries where the AEA are available from Eurostat = we expect no difference
#  If we use the same vintage of the AEAs transmitted by the countries, we will only find rounding differences
#  If, however, the vintage Eurostat used and the one we use are different, we will find some differences
df_comp_complete_eurostat_aea <- df_comp_global_aea %>%
  filter(emis_country %in% vc_complete_eurostat_aea) %>%
  mutate(diff = replication - eurostat,
         diff_pc = (replication / eurostat - 1) * 100)

# Countries where AEAs are fully estimated

df_comp_estimation_countries <- df_comp_global_aea %>%
  filter(emis_country %in% vc_aea_full_estimation_countries) %>%
  mutate(diff = replication - eurostat,
         diff_pc = (replication / eurostat - 1) * 100)


# --------------- COMPARISON OF TOTAL EMISSIONS BY INDUSTRY---------------------

# Computing total emissions by industry (only for estimated countries)

df_comp_totals_by_industry <- df_comp_global_aea %>%
  filter(emis_country %in% vc_aea_full_estimation_countries) %>%
  group_by(gas_type, time_period, emis_industry) %>%
  summarise(across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(diff = replication - eurostat,
         diff_pc = (replication / eurostat - 1) * 100)

# PLOTS - comparaison in absolute level for one reference year

ref_year <- '2015'

df_comp_totals_by_industry_for_graph <- df_comp_totals_by_industry %>%
  select(gas_type, time_period, emis_industry,
         eurostat, replication) %>%
  pivot_longer(where(is.numeric), names_to = "version", values_to = "value")

ggplot(df_comp_totals_by_industry_for_graph %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "CO2"),
       aes(x = emis_industry, y = value, fill = version)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - CO2"))

ggplot(df_comp_totals_by_industry_for_graph %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "CH4"),
       aes(x = emis_industry, y = value, fill = version)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - CH4"))

ggplot(df_comp_totals_by_industry_for_graph %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "N2O"),
       aes(x = emis_industry, y = value, fill = version)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - N2O"))

ggplot(df_comp_totals_by_industry_for_graph %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "HFC_CO2E"),
       aes(x = emis_industry, y = value, fill = version)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - HFC"))

ggplot(df_comp_totals_by_industry_for_graph %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "PFC_CO2E"),
       aes(x = emis_industry, y = value, fill = version)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - PFC"))

ggplot(df_comp_totals_by_industry_for_graph %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "NF3_SF6_CO2E"),
       aes(x = emis_industry, y = value, fill = version)) +
  geom_col(position = "dodge") +
  ggtitle(paste0("Total emission by industry year ", ref_year," - NF3_SF6"))


# PLOTS - comparaison in difference for one reference year

df_comp_diff_by_industry_for_graph <- df_comp_totals_by_industry  %>%
  select(gas_type, time_period, emis_industry, diff)

ggplot(df_comp_diff_by_industry_for_graph %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "CO2"), 
       aes(x = emis_industry, y = diff)) +
  geom_col() +
  ggtitle(paste0("Total emission by industry - diff btw Eurostat and replication - year ", ref_year," - CO2"))

ggplot(df_comp_diff_by_industry_for_graph %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "CH4"), 
       aes(x = emis_industry, y = diff)) +
  geom_col() +
  ggtitle(paste0("Total emission by industry - diff btw Eurostat and replication - year ", ref_year," - CH4"))

ggplot(df_comp_diff_by_industry_for_graph %>%
         filter(time_period == ref_year) %>%
         filter(gas_type == "N2O"), 
       aes(x = emis_industry, y = diff)) +
  geom_col() +
  ggtitle(paste0("Total emission by industry - diff btw Eurostat and replication - year ", ref_year," - N2O"))


# PLOTS - comparison for the whole time series (industries from A to H)

ggplot(df_comp_totals_by_industry_for_graph %>%
         filter(emis_industry < "I") %>%
         filter(gas_type == "CO2"),
       aes(x = time_period, y = value, color = version, group = version)) +
  geom_line() +
  facet_wrap(vars(emis_industry), scales = "free") +
  ggtitle("Total emission by industry - CO2 (kT)")

ggplot(df_comp_totals_by_industry_for_graph %>%
         filter(emis_industry < "I") %>%
         filter(gas_type == "CH4"),
       aes(x = time_period, y = value, color = version, group = version)) +
  geom_line() +
  facet_wrap(vars(emis_industry), scales = "free") +
  ggtitle("Total emission by industry - CH4 (kT)")

ggplot(df_comp_totals_by_industry_for_graph %>%
         filter(emis_industry < "I") %>%
         filter(gas_type == "N2O"),
       aes(x = time_period, y = value, color = version, group = version)) +
  geom_line() +
  facet_wrap(vars(emis_industry), scales = "free") +
  ggtitle("Total emission by industry - N2O (kT)")


# ---------------- COMPARISON OF TOTAL EMISSIONS BY COUNTRY---------------------

# Computing countries total emissions

df_comp_totals_by_country <- df_comp_global_aea %>%
  group_by(gas_type, time_period, emis_country) %>%
  summarise(across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(diff = replication - eurostat,
         diff_pc = (replication / eurostat - 1) * 100)

# PLOTS

df_comp_totals_by_country_for_graph <- df_comp_totals_by_country %>%
  select(gas_type, time_period, emis_country,
         eurostat, replication) %>%
  pivot_longer(where(is.numeric), names_to = "version", values_to = "value")

ggplot(df_comp_totals_by_country_for_graph %>%
         filter(gas_type == "CO2"),
       aes(x = time_period, y = value, color = version, group = version)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("CO2 (kT) - Total emission by country, incl. HH")

ggplot(df_comp_totals_by_country_for_graph %>%
         filter(gas_type == "CH4"),
       aes(x = time_period, y = value, color = version, group = version)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("CH4 (kT) - Total emission by country, incl. HH")

ggplot(df_comp_totals_by_country_for_graph %>%
         filter(gas_type == "N2O"),
       aes(x = time_period, y = value, color = version, group = version)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("N2O (kT) - Total emission by country, incl. HH")

ggplot(df_comp_totals_by_country_for_graph %>%
         filter(gas_type == "HFC_CO2E"),
       aes(x = time_period, y = value, color = version, group = version)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("HFC (kT CO2eq) - Total emission by country, incl. HH")

ggplot(df_comp_totals_by_country_for_graph %>%
         filter(gas_type == "PFC_CO2E"),
       aes(x = time_period, y = value, color = version, group = version)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("PFC (kT CO2eq) - Total emission by country, incl. HH")

ggplot(df_comp_totals_by_country_for_graph %>%
         filter(gas_type == "NF3_SF6_CO2E"),
       aes(x = time_period, y = value, color = version, group = version)) +
  geom_line() +
  facet_wrap(vars(emis_country), scales = "free") +
  ggtitle("NF3_SF6 (kT CO2eq) - Total emission by country, incl. HH")
