# Script workflow

# Collect COVID data
# devtools::install_github("RamiKrispin/coronavirus", force=TRUE)
source('./Scripts/virus_data.R')

# Collect static variables
source('./Scripts/Gather_GHSindex_data_static.R')

# Clean the data for implementation in the ML model
source('./Scripts/data_cleaning_oxford.R')

# # Make United States GIF plots
# source('./Scripts/United_States_Animation.R')
# 
# # Make Country GIF plots
# source('./Scripts/MountainRangeAnimationGIFs.R')

# Variable Selection Using VSURF
source('./Scripts/run_VSURF_and_WT_oxford_VSURF_TRUE.R')

source('./Scripts/save_LOO_models.R')

# Run a specific scenario
# source('./Scripts/Analysis_2_Model_R0_5_14_WT_SEIR_ribbon.R')
# Run a specific scenario
# source('./Scripts/Analysis_2_Model_R0_5_14_WT_SEIR_ribbon_function_SHINY.R')


# cd C:/Users/chris/OneDrive/Desktop/COVID-19_ML
# git config --global http.postBuffer 524288000
# C:/Program\ Files/Git/bin/git.exe push origin refs/heads/Big_branch


