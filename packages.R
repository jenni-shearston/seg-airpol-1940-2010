# Packages to Load
# Early Life Social & Environmental Exposures and ADRD
# Jenni A. Shearston 
# Updated 01/16/2025

# List of packages to use
list.of.packages = c('broom',           # tidy objects
                     'corrplot',        # correlation plots
                     'data.table',      # fast manipulation of large data
                     'DHARMa',          # model diagnostic package
                     'dlnm',            # creating crossbases
                     'fst',             # write/read fst file types
                     'ggalluvial',      # make alluvial and senke charts
                     'gtools',          # cut cont. variable by ptile
                     'haven',           # read Stata file types
                     'here',            # set and track directories
                     'itsadug',         # get_coefs function 
                     'lme4',            # mixed models
                     'marginaleffects', # predicting from mixed models
                     'mgcv',            # mixed GAM models
                     'mgcViz',          # GAM model visualization
                     'patchwork',       # create multi-panel plots
                     'readxl',          # read Excel files
                     'rgeoda',          # spatial clusters
                     'sf',              # GIS
                     'splines',         # add splines to models
                     'tidycensus',      # access to Census data
                     'tidyverse',       # data management
                     'viridis'          # color palette
)

# Check if packages in list are installed, and if not, install 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
lapply(list.of.packages, require, character.only = TRUE)


