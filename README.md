# MONOPOLI
Modeling Of NOnPharmaceutical Observed Longterm Interventions

The scripts used to build the MONOPOLI framework were run in the following order:

### Gather reported case counts for each geographic region
./Scripts/virus_data.R

### Organize demobraphic variables
./Scripts/Gather_GHSindex_data_static.R

### Clean the data for implementation in the ML model
./Scripts/data_cleaning_oxford.R

### Variable Selection Using VSURF, run on an HPC using a system-specific bash script
./Scripts/run_VSURF_and_WT_oxford_VSURF_TRUE.R

### Using the selected variables, train a leave-one-out random forests model for each geographic region
./Scripts/save_LOO_models_CHUNKS.R

### Plot the empirical data, generate the 4 scenarios, and estimate the SEIR infection curve
./Scripts/Analysis_2_Model_R0_5_14_WT_SEIR_ribbon_function_oxford.R
