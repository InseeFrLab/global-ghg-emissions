# SOURCE CONTENT : FIGARO multi regional input-output tables (industry X industry)

# HOW TO UPDATE THIS SOURCE ? **************************************************

# link : https://ec.europa.eu/eurostat/fr/web/esa-supply-use-input-tables/database

# FIGARO tables (2024 edition): annual EU inter-country supply, use and input-output tables
#   Input-output tables industry by industry
#     CSV flat format (for 64 industries)

# Download all files (one for each year)

# ******************************************************************************


# ------------------------------ PARAMETERS ------------------------------------

vc_figaro_years <- as.character(seq(2010, 2022))

# ---------------------------- LOAD FUNCTION -----------------------------------

load_figaro_industry_output_year_y <- function(year_y) {

  # User information
  print(paste0("Loading and formatting FIGARO IND x IND year ", year_y))
  
  # Load from csv
  df_IOT_ind_by_ind_raw <- read.csv2(paste0(local_path_sources, "/FIGARO/flatfile_eu-ic-io_ind-by-ind_24ed_", year_y, ".csv"), sep=",")
  
  # Formatting
  df_IOT_ind_by_ind <- df_IOT_ind_by_ind_raw %>%
    mutate(time_period = year_y) %>%
    select(time_period,
           sply_country = refArea,
           sply_industry = rowIi,
           use_country = counterpartArea,
           use_type = colIi,
           value = obsValue) %>%
    mutate(sply_industry = str_replace(sply_industry, "CPA_", ""),
           use_type = str_replace(use_type, "CPA_", ""),
           value = as.numeric(value))

  # Extracting the output by industry
  df_output_by_ind <- df_IOT_ind_by_ind %>%
    group_by(time_period, sply_country, sply_industry) %>%
    summarise(output = sum(value)) %>%
    ungroup()
  
  return(df_output_by_ind)
    
}

# -------------------------------- LOAD CALL -----------------------------------

df_figaro_output <- map(vc_figaro_years, load_figaro_industry_output_year_y) %>%
  list_rbind()

# ---------------------------------- SAVE --------------------------------------

write_rds(df_figaro_output, paste0(path_frmt_source, "/figaro_output.rds"))
