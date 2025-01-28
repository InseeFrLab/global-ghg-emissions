#------------------------------------------------------------------------------#
#                            Data path initialization                          #
#------------------------------------------------------------------------------#

# Root path of the data (sources and output) -> declared in a script specific to each user

source("scripts/local_path_not_shared.R")

# ******************************************************************************

# This script "local_path_not_shared.R" is not shared on the GIT repo,
#  because it is specific to each user of the code
# It is only expected to contain two declarations :

# local_path_sources <- "C:/.../the_folder_where_my_sources_are"
# local_path_output <- "C:/.../the_folder_where_i_want_the_results_to_be"

#   ... and that's it

# ******************************************************************************


# Data sources after pre-processing --------------------------------------------

path_frmt_source <- paste0(local_path_output, "/A. Formatted sources")
if (!dir.exists(path_frmt_source)) dir.create(path_frmt_source)

# Global AEA results --------------------------------------------------------

path_results <- paste0(local_path_output, "/B. Output")
if (!dir.exists(path_results)) dir.create(path_results)


