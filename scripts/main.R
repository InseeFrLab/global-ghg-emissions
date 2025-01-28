# 1. Libraries -----------------------------------------------------------------

library(tidyverse)
library(readxl)


# 2. Data paths initialization -------------------------------------------------

source("scripts/data_paths.R")

# Util function : removes all objects from the global environment except the 'path' declarations and the function itself
clean_global_env <- function() {
  ls_all_objects <- ls(.GlobalEnv)
  ls_objects_to_remove <- ls_all_objects[!grepl("*path*", ls_all_objects) &
                                           !(ls_all_objects %in% c("clean_global_env"))]
  rm(list = ls_objects_to_remove, envir = .GlobalEnv)
}

# 3. Sources pre-processing ----------------------------------------------------

source("scripts/1_sources_preprocessing/process_AEA_eurostat.R")
clean_global_env()
source("scripts/1_sources_preprocessing/process_AEA_great_britain.R")
clean_global_env()
source("scripts/1_sources_preprocessing/process_EDGAR.R")
clean_global_env()
source("scripts/1_sources_preprocessing/process_EU_PEFA.R")
clean_global_env()
source("scripts/1_sources_preprocessing/process_FIGARO.R") # this one is a bit longer to run
clean_global_env()
source("scripts/1_sources_preprocessing/process_OECD_AIR_transport.R")
clean_global_env()
source("scripts/1_sources_preprocessing/process_OECD_MARITIME_transport.R")
clean_global_env()

source("scripts/1_sources_preprocessing/process_global_AEA_estimated_by_eurostat.R")
clean_global_env()


# 4. Global AEA estimation -----------------------------------------------------

source("scripts/2_global_AEA_estimation/1_estimating_global_AEA_from_EDGAR.R")
clean_global_env()
source("scripts/2_global_AEA_estimation/2_extending_eurostat_estimate.R")
clean_global_env()


# 5. Test of the output quality : did everything went right ? ------------------

source("scripts/3_tests/quality_tests.R")
clean_global_env()


# 6. [For information only] Comparaison with Eurostat's own estimate -----------

source("scripts/3_tests/comparison_with_eurostat_own_estimates.R")
clean_global_env()

