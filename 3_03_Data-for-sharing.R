# Data for Sharing Publicly with Manuscript
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 11/21/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Review and select variables
# 2: Save out files as RDS or shp

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we prepare data to be shared publicly. We remove any variables
# that are unnecessary to run the analyses. The data saved out here is sufficient
# for re-creating all manuscript tables, plots, and models. 

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & clean environment
source(paste0(project.folder, 'scripts/packages.R'))
rm(list.of.packages, new.packages)

# 0c Set up filepath(s)
merged_data_path <- paste0(project.folder, 'data/merged_data/')
spatial_data_path <- paste0(project.folder, 'data/county_shapefiles/')
sharing_data_path <- paste0(project.folder, 'outputs/data_to_share/')

# 0d Load all env exp, seg, sociodemo, and spatial data
#    Note: CRS is an Albers Equal Area Conic with parameters (meters)
data <- read_fst(paste0(merged_data_path, 'env_seg_sociodemo_allyears.fst'))
senke_data_strat <- read_fst(paste0(merged_data_path, 'segQuantilesForSenkesStratifiedData.fst'))
consistent_seg_data <- read_rds(paste0(merged_data_path, 'ConsistentSegDataStratified.RDS'))
county1940 <- st_read(paste0(spatial_data_path, 
                             'FINAL_1940_county_shapefile_clean/counties_contiguous_1940.shp')) %>% 
  janitor::clean_names()
county2010 <- st_read(paste0(spatial_data_path, 
                             'FINAL_2010_county_shapefile_clean/counties_contiguous_2010.shp')) %>% 
  janitor::clean_names()

####************************************
#### 1: Review and select variables #### 
####************************************

# 1a 2010 shapefile
names(county2010)
head(county2010)
county_shapefile_2010 <- county2010 %>% 
  dplyr::select(statefp10, countyfp10, name10, gisjoin,
                jfips10, jcnam10, geometry)
head(county_shapefile_2010)

# 1b 1940 shapefile
names(county1940)
head(county1940)
county_shapefile_1940 <- county1940 %>% 
  dplyr::select(nhgisnam, nhgisst, nhgiscty, icpsrst, icpsrcty, icpsrnam, statenam,
                gisjoin, x_centroid, y_centroid, jnhgis40, jname40, geometry)
head(county_shapefile_1940)

# 1c Main dataset
names(data)
head(data)
data_for_main_analysis <- data %>% 
  dplyr::select(-fospp_idw, -ogw_idw,
                -nbrTwo, -raceICE, -raceEduICE_sc10)
head(data_for_main_analysis)

# 1d Dataset for creating senke charts
names(senke_data_strat)
head(senke_data_strat)
data_for_senke_charts <- senke_data_strat 

# 1e Dataset for running consistently highly segregated analysis
consistent_seg_data <- consistent_seg_data %>% unnest(cols = data)
names(consistent_seg_data)
head(consistent_seg_data)
data_for_consistent_segregation_analysis <- consistent_seg_data

####*************************************
#### 2: Save out files as RDS or shp #### 
####*************************************

county_shapefile_2010 %>% st_write(paste0(sharing_data_path, 'county_shapefile_2010.shp'), append=FALSE)
county_shapefile_1940 %>% st_write(paste0(sharing_data_path, 'county_shapefile_1940.shp'), append=FALSE)
data_for_main_analysis %>% write_rds(paste0(sharing_data_path, 'data_for_main_analysis.rds'))
data_for_senke_charts %>% write_rds(paste0(sharing_data_path, 'data_for_senke_charts.rds'))
data_for_consistent_segregation_analysis %>% write_rds(paste0(sharing_data_path, 'data_for_consistent_segregation_analysis.rds'))

