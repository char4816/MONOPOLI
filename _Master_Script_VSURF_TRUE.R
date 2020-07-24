# Script workflow

# Collect COVID data
# devtools::install_github("RamiKrispin/coronavirus", force=TRUE)
source('./Scripts/virus_data.R')

# Collect static variables
source('./Scripts/Gather_GHSindex_data_static.R')

# Clean the data for implementation in the ML model
source('./Scripts/data_cleaning_oxford.R')

# Variable Selection Using VSURF
source('./Scripts/run_VSURF_and_WT_oxford_VSURF_TRUE.R')

source('./Scripts/save_LOO_models.R')

source('./Scripts/Analysis_2_Model_R0_5_14_WT_SEIR_ribbon_function_oxford.R')
