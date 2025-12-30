# Combine 1940 & 2010 Env Exp, Segregation Metrics, and Socio-demographics 
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 09/21/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Convert env exp dataset to unique county-year rows & clean
# 2: Segregation data 1940: DI and ICE
# 3: Segregation data 1940: Neighbor Metric
# 4: Keep only needed variables: seg2010, sociodemo1940, rural
# 5: Merge all datasets & review missing 
# 6: Revise / Calculate variables for analyses
# 7: Save out file

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we merge all environmental exposure, segregation,
# and covariate data for both 1940 and 2010 to create a main 
# dataset for analyses. We also review missingness extensively, and 
# calculate additional variables needed for later analysis, such as
# county area, population density, and if a 2010 county previously
# had an oil/gas well in its spatial area in 1940.

# Nb Number of counties (n) in dataset
# The final number of counties in 2010 should be n = 3,109

# Nc Resources
# Helpful resource for merging: "A Consistent County-Level Crosswalk for US Spatial
# Data since 1790," by Eckert, Gvirtz, and Peters (2018)
# http://fpeckert.me/papers/egp-spatialcrosswalk.pdf

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & clean environment
source(paste0(project.folder, 'scripts/packages.R'))
rm(list.of.packages, new.packages)

# 0c Set up filepath(s)
envExp_data_path <- paste0(project.folder, 'data/county_envExp_data/')
sociodemo1940_data_path <- paste0(project.folder, 'data/county_sociodemo_data/')
seg_data_path <- paste0(project.folder, 'data/county_seg_data/')
rural_data_path <- paste0(project.folder, 'data/uhl_ruralurban_data/')
merged_data_path <- paste0(project.folder, 'data/merged_data/')
prelim_plot_path <- paste0(project.folder, 'outputs/prelim_plots/')
spatial_data_path <- paste0(project.folder, 'data/county_shapefiles/')

# 0d Load all environmental exposure and segregation data
envExp <- fread(paste0(envExp_data_path, '1940_2010_county_exposure_NHGIS_last_update_Jun_30_2025.csv'),
                stringsAsFactors = FALSE, colClasses = "character") %>% 
  janitor::clean_names() %>% mutate(exposure_value = as.numeric(exposure_value)) %>% 
  dplyr::select(-v1)
rmpm2010 <- read_csv(paste0(envExp_data_path, 'V5GGL04-pm25_2010_county_NHGIS.csv')) %>% 
  janitor::clean_names() %>% dplyr::select(-x1)
# sensEE <- read_csv(paste0(envExp_data_path, '1940_2010_pp_wells_county_counts_buffer_5km_NHGIS.csv'), 
#                    col_types = list(.default = col_character())) %>% 
#   janitor::clean_names() %>% mutate(exposure_value = as.numeric(exposure_value)) %>% 
#   dplyr::select(-x)
seg1940 <- read_rds(paste0(seg_data_path, 'ipums1940_area_level_race_dat_clean.RDS'))
segNbr1940 <- haven::read_dta(paste0(seg_data_path, 'segregation-data-1880-and-1940.dta'))
seg2010 <- read_fst(paste0(seg_data_path, 'seg_2010_county.fst'))
sociodemo1940 <- read_fst(paste0(sociodemo1940_data_path, 'sociodemographic_1940_county.fst'))
rural <- read_fst(paste0(rural_data_path, 'plural_1940_2010_clean_imputed.fst'))

# 0e Load county shapefiles and crosswalk
#    Note: CRS is an Albers Equal Area Conic with parameters (meters)
county1940 <- st_read(paste0(spatial_data_path, 
                             'FINAL_1940_county_shapefile_clean/counties_contiguous_1940.shp')) %>% 
  janitor::clean_names()
county2010 <- st_read(paste0(spatial_data_path, 
                             'FINAL_2010_county_shapefile_clean/counties_contiguous_2010.shp')) %>% 
  janitor::clean_names()
cwalk <- read_fst(paste0(spatial_data_path, 'crosswalk/FINAL_county_crosswalk_clean_1940-2010.fst'))

####*******************************************************************
#### 1: Convert env exp dataset to unique county-year rows & clean #### 
####*******************************************************************

# Notes regarding Sherry's envExp dataframe:
# For categories under data_type = power plants (pp_) that are spelled differently
#   or are synonyms in different years or data_types: Coal = COAL, Petroleum = OIL, 
#   Gas = GAS, OIL & GAS = OIL_GAS (if present), Fossil_Fule = Fossil_Fuel
#   OTHRFOSL is included in Fossil_Fule in 2010 (OTHRFOSL didn't exist in 1940)
#   We will clean these categories later in the script.
# State and county codes come from shp files used in Sherry's analysis
#   2010, statefp10, countyfp10 = FIPS codes
#   1940, state, county = NHGIS codes

# 1a Review variables in env exp datasets, merge well/pp buffer file with main file (if needed),
#    keep only needed data types
# 1a.i Review vars, categories, data types, missingness
names(envExp)
table(envExp$category, envExp$data_type, envExp$year, useNA = 'always')
sum(is.na(envExp$state_code)); sum(is.na(envExp$county_code))
# 1a.ii Keep the following data_types and data_type/category combinations: 
#       - Keep all categories in CMIP6 data_type - we will average them later. 
#         These are the 11 model outputs from CMIP6
#       - Keep only the NOx category in the Automobile data_type
#       - For data_types that contain pp_ (power plants) or well_ (oil/gas wells),
#         keep only categories that represent oil or gas wells and fossil fuel 
#         power plants, exclude other well and power plant types, such as WIND,
#         GEOTHERMAL, etc.
envExp3 <- envExp %>% 
  filter(category %in% c('Coal', 'Fossil_Fule', 'Fossil_Fuel', 'Gas', 'GAS',  
                         'Petroleum', 'COAL', 'OTHRFOSL', 'BIOMASS', 'NOx', 
                         'OIL') | 
           data_type == 'CMIP6')
table(envExp3$category, envExp3$data_type, envExp3$year, useNA = 'always')

# 1b Clean id variables
#    ID vars in the envExp df come from the shapefiles cleaned in script 0_01
#      For 2010, state_code is statefp10, county_code is countyfp10 (fips)
#      For 1940, state_code is state, county_code is county (nhgis)
#    In this code chunk, a unique state-county-year identifier is created. Fixed 
#      widths are used for each year: 7 digits for 1940 to match NHGIS, 5 digits
#      for 2010 to match FIPS. 
envExp3 <- envExp3 %>% 
  mutate(
    state_code_1940 = ifelse(year == '1940', state_code, NA), 
    state_code_1940 = str_pad(state_code_1940, width = 3, side = "left", pad = "0"),
    county_code_1940 = ifelse(year == '1940', county_code, NA),
    county_code_1940 = str_pad(county_code_1940, width = 4, side = 'left', pad = '0'),
    jnhgis40 = ifelse(year == '1940', paste0(state_code_1940, county_code_1940), NA),
    state_code_2010 = ifelse(year == '2010', state_code, NA),
    state_code_2010 = str_pad(state_code_2010, width = 2, side = "left", pad = "0"),
    county_code_2010 = ifelse(year == '2010', county_code, NA),
    county_code_2010 = str_pad(county_code_2010, width = 3, side = 'left', pad = '0'),
    jfips10 = ifelse(year == '2010', paste0(state_code_2010, county_code_2010), NA),
    st_cnty_yr_id = case_when(year == '1940' ~ jnhgis40, year == '2010' ~ jfips10),
    st_cnty_yr_id_type = case_when(
      year == '1940' ~ 'state-county 1940 NHGIS code, jnhgis40',
      year == '2010' ~ 'state-county 2010 FIPS code, jfips10'))
length(unique(envExp3$st_cnty_yr_id)) # n = 6,209, as expected

# 1c Make data_type wide so each env exp is its own variable
#    Drop exposure_unit because it would otherwise also need to be widened
#    gasoline = kg/yr, pm = ug/m^3
#    Convert all state and county names to lowercase b/c some have capitals and
#    some do not which will result in duplicated rows
envExp3 <- envExp3 %>% 
  dplyr::select(-exposure_unit, - state_code, -county_code) %>% 
  mutate(county_name = tolower(county_name),
         state_name = tolower(state_name)) %>% 
  pivot_wider(names_from = c('data_type', 'category'), values_from = exposure_value)

# 1d Average air pollution predictions
#    Note: All air pollution estimates are pretty highly correlated, but also
#          they are from a climate model rather than an air pollution prediction
#          model, so we expect that they are not super precise. However, they
#          are likely the best available data. Will also make a standard deviation
#          variable to capture uncertainty between estimates from the 11 models
envExp3 <- envExp3 %>% 
  group_by(st_cnty_yr_id) %>% 
  mutate(meanPM2.5 = (CMIP6_Con_BCC.ESM1 + CMIP6_Con_MIROC.ES2L + CMIP6_Con_CESM2.WACCM 
                      + CMIP6_Con_CNRM.ESM2.1 + CMIP6_Con_HadGEM3.GC31.LL 
                      + CMIP6_Con_NorESM2.LM + CMIP6_Con_GFDL.ESM4 
                      + CMIP6_Con_GISS.E2.1.G + CMIP6_Con_MPI.ESM.1.2.HAM 
                      + CMIP6_Con_UKESM1.0.LL + CMIP6_Con_MRI.ESM2.0)/11,
         sdPM2.5 = sd(c(CMIP6_Con_BCC.ESM1, CMIP6_Con_MIROC.ES2L, CMIP6_Con_CESM2.WACCM, 
                        CMIP6_Con_CNRM.ESM2.1, CMIP6_Con_HadGEM3.GC31.LL, 
                        CMIP6_Con_NorESM2.LM, CMIP6_Con_GFDL.ESM4, 
                        CMIP6_Con_GISS.E2.1.G, CMIP6_Con_MPI.ESM.1.2.HAM, 
                        CMIP6_Con_UKESM1.0.LL, CMIP6_Con_MRI.ESM2.0))) %>% 
  ungroup() %>% 
  dplyr::select(-contains('_Con_'))

# 1e Clean id variables from Randall Martin PM2.5 data (2010 only) and keep
#    only needed variables
rmpm2010_clean <- rmpm2010 %>% 
  mutate(jfips10 = paste0(statefp10, countyfp10)) %>% 
  rename(pm2.5_rm = pm25) %>% 
  dplyr::select(jfips10, pm2.5_rm)

# 1f Merge Randall Martin PM2.5 with other env exposures
envExp4 <- envExp3 %>% 
  left_join(rmpm2010_clean, by = 'jfips10')

# 1g Review distribution of env exp vars
envExp4 %>% ggplot() + geom_histogram(aes(x = Automobile_NOx)) + facet_wrap(~year)
envExp4 %>% ggplot() + geom_histogram(aes(x = meanPM2.5)) + facet_wrap(~year)
envExp4 %>% ggplot() + geom_histogram(aes(x = pm2.5_rm)) + facet_wrap(~year) # 2010 only
envExp4 %>% ggplot() + geom_histogram(aes(x = pp_idw_Coal)) + facet_wrap(~year) # 1940 only
envExp4 %>% ggplot() + geom_histogram(aes(x = pp_idw_COAL)) + facet_wrap(~year) # 2010 only
envExp4 %>% ggplot() + geom_histogram(aes(x = pp_idw_Petroleum)) + facet_wrap(~year) # 1940 only
envExp4 %>% ggplot() + geom_histogram(aes(x = pp_idw_OIL)) + facet_wrap(~year) # 2010 only
envExp4 %>% ggplot() + geom_histogram(aes(x = pp_idw_Gas)) + facet_wrap(~year) # 1940 only
envExp4 %>% ggplot() + geom_histogram(aes(x = pp_idw_GAS)) + facet_wrap(~year) # 2010 only
envExp4 %>% ggplot() + geom_histogram(aes(x = pp_idw_OTHRFOSL)) + facet_wrap(~year) # 2010 only
envExp4 %>% ggplot() + geom_histogram(aes(x = pp_idw_BIOMASS)) + facet_wrap(~year) # 2010 only
envExp4 %>% ggplot() + geom_histogram(aes(x = pp_count_no_buffer_Fossil_Fule)) + facet_wrap(~year)
table(envExp4$pp_count_no_buffer_Fossil_Fule, envExp4$year)
envExp4 %>% ggplot() + geom_histogram(aes(x = pp_count_buffer_Fossil_Fuel)) + facet_wrap(~year)
envExp4 %>% ggplot() + geom_histogram(aes(x = well_idw_GAS)) + facet_wrap(~year)
envExp4 %>% ggplot() + geom_histogram(aes(x = well_idw_OIL)) + facet_wrap(~year)
envExp4 %>% ggplot() + geom_histogram(aes(x = well_count_no_buffer_GAS)) + facet_wrap(~year)
envExp4 %>% ggplot() + geom_histogram(aes(x = well_count_buffer_GAS)) + facet_wrap(~year)
envExp4 %>% ggplot() + geom_histogram(aes(x = well_count_no_buffer_OIL)) + facet_wrap(~year)
envExp4 %>% ggplot() + geom_histogram(aes(x = well_count_buffer_OIL)) + facet_wrap(~year)

# 1h Combine well and pp categories that are spelled differently but refer to the
#    same category
#    E.g., if both 'Gas' and 'GAS' are used for gas wells, combine into one column
envExp4 <- envExp4 %>% 
  mutate(
    coalpp_idw = ifelse(year == '1940', pp_idw_Coal, pp_idw_COAL),
    oilpp_idw = ifelse(year == '1940', pp_idw_Petroleum, pp_idw_OIL),
    gaspp_idw = ifelse(year == '1940', pp_idw_Gas, pp_idw_GAS)) %>% 
  dplyr::select(-pp_idw_Coal, -pp_idw_COAL, -pp_idw_Petroleum, -pp_idw_OIL,
                -pp_idw_Gas, -pp_idw_GAS)
names(envExp4)

# 1i Assign missing IDW and count values to be zero 
envExp4 <- envExp4 %>% 
  mutate(
    pp_count_buffer_Fossil_Fuel = ifelse(is.na(pp_count_buffer_Fossil_Fuel), 0, pp_count_buffer_Fossil_Fuel),
    pp_count_no_buffer_Fossil_Fule = ifelse(is.na(pp_count_no_buffer_Fossil_Fule), 0, pp_count_no_buffer_Fossil_Fule),
    well_count_buffer_OIL = ifelse(is.na(well_count_buffer_OIL), 0, well_count_buffer_OIL),
    well_count_buffer_GAS = ifelse(is.na(well_count_buffer_GAS), 0, well_count_buffer_GAS),
    well_count_no_buffer_OIL = ifelse(is.na(well_count_no_buffer_OIL), 0, well_count_no_buffer_OIL),
    well_count_no_buffer_GAS = ifelse(is.na(well_count_no_buffer_GAS), 0, well_count_no_buffer_GAS),
    well_idw_OIL = ifelse(is.na(well_idw_OIL), 0, well_idw_OIL),
    well_idw_GAS = ifelse(is.na(well_idw_GAS), 0, well_idw_GAS),
    pp_idw_OTHRFOSL = ifelse(is.na(pp_idw_OTHRFOSL), 0, pp_idw_OTHRFOSL),
    pp_idw_BIOMASS = ifelse(is.na(pp_idw_BIOMASS), 0, pp_idw_BIOMASS),
    coalpp_idw = ifelse(is.na(coalpp_idw), 0, coalpp_idw),
    oilpp_idw = ifelse(is.na(oilpp_idw), 0, oilpp_idw),
    gaspp_idw = ifelse(is.na(gaspp_idw), 0, gaspp_idw))
  
# 1j Average / sum gas and oil well IDW / count and aggregate all fossil fuel
#    power plant types. We will not consider gas wells and oil wells separately, 
#    or types of power plants separately.
#    Note: 'othrfosl' did not exist in the 1940 dataset, nor did combined oil/gas (if present), 
#           so we will not use them to make the idw averages for 1940. also did not 
#           include biomass in pp idw avg because not sure if truly "fossil fuel" 
# 1j.i Average / sum
envExp4 <- envExp4 %>% 
  mutate(
    ogw_idw = ((well_idw_OIL + well_idw_GAS)/2), 
    ogw_count = (well_count_no_buffer_OIL + well_count_no_buffer_GAS),
    ogwBuffer_count = (well_count_buffer_OIL + well_count_buffer_GAS),
    fospp_idw = ifelse(year == '1940', ((coalpp_idw + oilpp_idw + gaspp_idw)/3),
                       ((coalpp_idw + oilpp_idw + gaspp_idw + pp_idw_OTHRFOSL)/4)))
# 1j.ii Double check that not-buffered pp and well counts are equal to or less
#       than buffered counts
check <- envExp4 %>% 
  mutate(buff_check_ogw = ifelse(ogw_count > ogwBuffer_count, 1, 0),
         buff_check_pp = ifelse(pp_count_no_buffer_Fossil_Fule > pp_count_buffer_Fossil_Fuel, 1, 0)) %>% 
  dplyr::select(year:st_cnty_yr_id_type, ogw_count, ogwBuffer_count, 
                pp_count_no_buffer_Fossil_Fule, pp_count_buffer_Fossil_Fuel,
                buff_check_ogw, buff_check_pp)
unique(check$buff_check_pp); unique(check$buff_check_ogw) # both 0, no buffer > buffer

# 1k Rename variables
names(envExp4)
envExp4 <- envExp4 %>% 
  rename(nox_autos = Automobile_NOx,
         fospp_count = pp_count_no_buffer_Fossil_Fule,
         fosppBuffer_count = pp_count_buffer_Fossil_Fuel)

# 1l Keep only needed variables
envExp4 <- envExp4 %>% 
  dplyr::select(st_cnty_yr_id, st_cnty_yr_id_type, 
                meanPM2.5, sdPM2.5, pm2.5_rm, 
                nox_autos, 
                fospp_count, fospp_idw, fosppBuffer_count,
                ogw_count, ogw_idw, ogwBuffer_count)
names(envExp4)

# 1m Review missingness
#    Looks good.
sum(is.na(envExp4$st_cnty_yr_id)) # 0
sum(is.na(envExp4$st_cnty_yr_id_type)) # 0
sum(is.na(envExp4$meanPM2.5)) # 0
sum(is.na(envExp4$sdPM2.5)) # 0
sum(is.na(envExp4$pm2.5_rm)) # 3100 (correct, only for 2010)
sum(is.na(envExp4$nox_autos)) # 0
sum(is.na(envExp4$fospp_count)) # 0
sum(is.na(envExp4$fosppBuffer_count)) # 0
sum(is.na(envExp4$ogw_count)) # 0
sum(is.na(envExp4$ogwBuffer_count)) # 0

####******************************************
#### 2: Segregation data 1940: DI and ICE #### 
####******************************************

# 2a Keep only needed vars in 1940 dataset
seg1940 <- seg1940 %>% 
  dplyr::select(statefip, countyicp, cnty_di_bw, raceICE_cnty, raceeduICE_cnty,
                raceemp16ICE_cnty)

# 2b Flip ICE vars such that +1 corresponds to the group with less privilege
#    Note: We do this so that it is easier to compare the dissimilarity index 
#    and ICE vars later. This will result in +1 values of ICE corresponding to 
#    counties where all residents are Black, all residents are Black and did not 
#    graduate HS, or where all residents are Black and unemployed. To accomplish
#    this mathematically, we can just switch all positive values to be negative, 
#    and all negative values to be positive. 
seg1940 <- seg1940 %>% 
  mutate(raceICE_cnty2 = raceICE_cnty * -1,
         raceeduICE_cnty2 = raceeduICE_cnty * -1,
         raceemp16ICE_cnty2 = raceemp16ICE_cnty * -1)

# 2c Drop the original ICE var names and rename flipped ICE vars to original var 
#    names + year indicator
seg1940 <- seg1940 %>% 
  dplyr::select(-raceICE_cnty, -raceeduICE_cnty, -raceemp16ICE_cnty) %>% 
  rename(raceICE_cnty_1940 = raceICE_cnty2,
         raceeduICE_cnty_1940 = raceeduICE_cnty2,
         raceemp16ICE_cnty_1940 = raceemp16ICE_cnty2)

# 2d Remove duplicated rows so that each county/state has one row for 1940 dataset
#    Note: Only n=3099 obs when should be 3100 - will review in 2g
seg1940 <- seg1940 %>%  distinct()

# 2e Clean join variables for 1940 dataset
#    Notes: statefip and countyicp were included in the dataset, so must use these
#           statefip should be equivalent to nhgisst with a trailing 0 (3 characters)
#             so here we convert to 3 characters
#           countyicp in crosswalk has characters that range from 1-4, as in seg1940
# 2e.i Convert merge vars to characters and make 3-character fips (state NHGIS code)
seg1940 <- seg1940 %>% 
  mutate(statefip = as.character(statefip),
         statefip = str_pad(statefip, 2, pad = '0', side = c('left')),
         statefip_3char = str_pad(statefip, 3, pad = '0', side = c('right')),
         countyicp = as.character(countyicp))
# 2e.ii Address inconsistently-labeled Nevada counties Ormsby/Carson City and Pershing. 
#       According to IPUMS documentation
#       https://usa.ipums.org/usa/volii/ICPSR.shtml#nevada (full list) and
#       https://usa.ipums.org/usa-action/variables/COUNTYICP#codes_section
#       the ICPSR code for Pershing County Nevada should be 0250 (FIPS = 027), and 
#       the ICPSR code for Ormsby County should be 0510 (also Carson City) (FIPS = 025). 
#       It looks like the crosswalk actually has the fips codes for these counties
#       listed in the icpsrcty column. The shapefile downloaded from NHGIS also has
#       the Pershing County Nevada ICPSR code listed as 270 as well, and Ormsby
#       as 250 (NHGIS codes 0270 and 0250 respectively). To make sure all Nevada
#       counties merge, I do the following: 
#         In the seg1940 dataset, I make the following changes before merging:
#         First, Nevada countyicp 250 should be changed to 270 (NHGIS 0270) --> 
#           it is actually Pershing County
#         Second, Nevada countyicp 510 should then be changed to 250 (NHGIS 0250) --> 
#           it is actually historical Ormsby County (Carson City). 
seg1940 <- seg1940 %>% 
  mutate(countyicp = ifelse(statefip_3char == '320' & countyicp == '250', '270', countyicp))
seg1940 <- seg1940 %>% 
  mutate(countyicp = ifelse(statefip_3char == '320' & countyicp == '510', '250', countyicp))
# 2e.iii Address Union County Oregon
#        The seg1940 dataset has Oregon countyicp 605, but not 610
#        In the cwalk, we have icpsrcty 610 / nhgiscty 0610 listed as Union County, 
#        and there is no icpsrcty 605 / nhgiscty of 0605  listed
#        In the IPUMS ICPSR County Codes page for Oregon
#        https://usa.ipums.org/usa/volii/ICPSR.shtml#oregon
#        Union County is listed as 0605, and Umpqua is listed as 0610.
#        Umpqua is not included in the cwalk, likely because it is historical
#        and was dissolved completely by 1862:
#        https://en.wikipedia.org/wiki/Umpqua_County,_Oregon
#        Similarly, the 1940 shapefile does not have a Umpqua County Oregon, but
#        it does have a Union County Oregon, which is listed as icpsrcty 610 and 
#        nhgiscty 0610
#        Therefore, here I change the seg1940 dataset entry for this county, 
#        changing Oregon countyicp 605 to 610. 
seg1940 <- seg1940 %>% 
  mutate(countyicp = ifelse(statefip_3char == '410' & countyicp == '605', '610', countyicp))

# 2f Add 1940 merging variable using crosswalk
# 2f.i Extract all distinct 1940 rows with needed merging variables 
id40 <- cwalk %>% filter(year == '1940') %>% 
  dplyr::select(nhgisst, icpsrcty, jnhgis40, nhgisnam) %>% distinct()
# 2f.ii Add 1940 merging variable to 1940 segregation data
seg1940 <- seg1940 %>% left_join(id40, by = c('statefip_3char' = 'nhgisst', 
                                              'countyicp' = 'icpsrcty'))

# 2g.i Check for missing jnhgis variable & fips code
sum(is.na(seg1940$jnhgis40)) # 0 missing
length(unique(seg1940$jnhgis40)) # 3099 (should be 3100)
# 2g.ii Identify missing county (n = 1)
#       The missing county is Yellowstone National Park in Idaho, which had no population
mis_seg_1940 <- as.data.frame(county1940) %>% dplyr::select(-geometry)
mis_seg_1940 <- mis_seg_1940 %>% filter(!(jnhgis40 %in% seg1940$jnhgis40))

# 2h Rename variables to include year
seg1940 <- seg1940 %>% 
  rename(cnty_di_bw_1940 = cnty_di_bw,
         statefip_1940 = statefip)

# 2i Keep only needed variables
seg1940 <- seg1940 %>% dplyr::select(-countyicp, -statefip_3char, -nhgisnam)

####***********************************************
#### 3: Segregation data 1940: Neighbor Metric #### 
####***********************************************

# Link to data and code from Logan and Parman's Neighbor-based segregation metric
# https://jmparman.people.wm.edu/dataandcode.html
# The Neighbor-based do file link on the page above has a codebook inside the do file.
# Logan and Parman create 4 versions of their segregation metric.
# We use the variables that relies upon census page rather than street because this
# is more inclusive and has better spatial coverage. Similarly we use the variable
# that relies upon one neighbor rather than two because it also has better coverage.
# Therefore we will use alpha_po primarily (census page, one neighbor), and will 
# consider alpha_pb if needed (census page, both neighbors). The two variables based
# on stree will not be used (alpha_so and alpha_sb)

# 3a Filter to needed observations - 1940 and not missing state names
# 3a.i Filter to observations only in 1940
#      n=3109, but 50 are missing nearly all information, including seg measures
segNbr1940 <- segNbr1940 %>% 
  filter(year == 1940)
# 3a.ii Remove observations with blank state names, end with n = 3059
segNbr1940 <- segNbr1940 %>% 
  filter(!statestring == '')

# 3b Keep only needed vars
#    Note: I cannot figure out what type of code the 'county' var is. 
#          I don't think it is FIPs because it goes up to four characters
#          It does not align with ICPSR codes for Maryland, instead aligning with
#          FIPs codes (ICPSR codes are shifted down by two from FIPs codes starting 
#          at Calvert, except for Baltimore City which is 510 / 5100)
#          https://usa.ipums.org/usa/volii/ICPSR.shtml#maryland
#          https://usa.ipums.org/usa-action/variables/COUNTYICP#codes_section
#          NHGIS county codes are usually 7 characters, maybe this is the county
#          only portion? 
#          Because I'm not sure, I will use state and county names to merge
#          alpha_po = neighbor seg metric based on census page & one neighbor
#          alpha_pb = neighbor seg metric based on census page & both neighbors
segNbr1940 <- segNbr1940 %>% 
  dplyr::select(statestring, countystring, year,
                alpha_po, alpha_pb)

# 3c Merge state and county string variables to use as join variable and format
#    Removed spaces and made lowercase
segNbr1940 <- segNbr1940 %>%
  mutate(year = as.character(year),
         stateCountyString = paste0(statestring, countystring),
         stateCountyString = str_replace_all(stateCountyString, ' ', ''),
         stateCountyString = tolower(stateCountyString)) %>% 
  dplyr::select(-statestring, -countystring)

# 3d Add 1940 merging variable (jnhgis40) using crosswalk
# 3d.i Extract all distinct 1940 rows with 3 needed merging variables 
id40 <- cwalk %>% filter(year == '1940') %>% 
  dplyr::select(statenam, nhgisnam, jnhgis40) %>% distinct()
# 3d.ii Convert state and county name to one var, lowercase, no spaces
id40 <- id40 %>% mutate(stateCountyString = paste0(statenam, nhgisnam),
                        stateCountyString = str_replace_all(stateCountyString, ' ', ''),
                        stateCountyString = tolower(stateCountyString)) %>% 
  dplyr::select(-statenam, -nhgisnam)
# 3d.iii Add 1940 merging variable (jnhgis40) to 1940 neighbor segregation data
#        by joining via stateCountyString
segNbr1940 <- segNbr1940 %>% left_join(id40, by = 'stateCountyString')

# 3e Check for observations missing 1940 merging variable & correct
#    Using crosswalk: 
# 3e.i Check for missing jnhgis variable
sum(is.na(segNbr1940$jnhgis40))
# 3e.ii Review missing obs (n = 11)
mis_segNbr_1940 <- segNbr1940 %>% filter(is.na(jnhgis40))
# 3e.iii Correct missing obs where possible (corrected all n = 11 missing obs)
segNbr1940 <- segNbr1940 %>% 
  mutate(jnhgis40 = ifelse(stateCountyString == 'alabamacolbert', '0100330', jnhgis40),
         jnhgis40 = ifelse(stateCountyString == 'coloradoarapahoe', '0800050', jnhgis40),
         jnhgis40 = ifelse(stateCountyString == 'louisianavermilion', '2201130', jnhgis40),
         jnhgis40 = ifelse(stateCountyString == 'minnesotastlouis', '2701370', jnhgis40),
         jnhgis40 = ifelse(stateCountyString == 'missouricass', '2900370', jnhgis40),
         jnhgis40 = ifelse(stateCountyString == 'missourihenry', '2900830', jnhgis40),
         jnhgis40 = ifelse(stateCountyString == 'newyorkstlawrence', '3600890', jnhgis40),
         jnhgis40 = ifelse(stateCountyString == 'oklahomapottawatomie', '4001250', jnhgis40),
         jnhgis40 = ifelse(stateCountyString == 'virginiamathews', '5101150', jnhgis40),
         jnhgis40 = ifelse(stateCountyString == 'washingtonklickitat', '5300390', jnhgis40),
         jnhgis40 = ifelse(stateCountyString == 'westvirginiapendleton', '5400710', jnhgis40))

# 3f Keep only needed variables
segNbr1940 <- segNbr1940 %>% dplyr::select(-stateCountyString, -year)

####******************************************************************
#### 4: Keep only needed variables: seg2010, sociodemo1940, rural #### 
####******************************************************************

# 4a Keep only needed variables in seg2010 dataset
seg2010 <- seg2010 %>% 
  dplyr::select(stctya_2010, contains('Pop'), di_2010, contains('race'))

# 4b Keep only needed variables in sociodemo1940 dataset & rename vars w year
sociodemo1940 <- sociodemo1940 %>% 
  rename(black_1940 = black, 
         totPop_1940 = totPop,
         stateCountyString_1940 = stateCountyString) %>% 
  dplyr::select(jnhgis40, stateCountyString_1940, totPop_1940, black_1940)

# 4c Keep only needed variables in rural dataset & rename vars w dataset
rural <- rural %>% 
  rename(cnty_name_ruraldf = cnty_name) %>% 
  dplyr::select(-year)

####********************************************
#### 5: Merge all datasets & review missing #### 
####********************************************

# 5a Create case complete year-state-county dataset
#    this is built from the 1940 and 2010 shapefiles
# 5a.i 1940 - select unique identifiers from script 0_01
d40 <- as.data.frame(county1940)
d40 <- d40 %>% dplyr::select(jnhgis40, jname40) %>% mutate(year = '1940')
length(unique(d40$jnhgis40)); length(unique(d40$jname40))
# 5a.ii 2010 - select unique identifiers from script 0_01
d10 <- as.data.frame(county2010)
d10 <- d10 %>% dplyr::select(jfips10, jcnam10, statefp10) %>% mutate(year = '2010')
length(unique(d10$jfips10)) # jcnam10 is not unique until merged w state names
# 5a.iii Bind 1940 and 2010 together
allData <- d10 %>% bind_rows(d40)
# 5a.iv Combine ids into one column, with separate column for id type
allData <- allData %>% mutate(
  st_cnty_yr_id = case_when(
    year == '1940' ~ jnhgis40,
    year == '2010' ~ jfips10),
  st_cnty_yr_id_type = case_when(
    year == '1940' ~ 'state-county 1940 NHGIS code, jnhgis40',
    year == '2010' ~ 'state-county 2010 FIPS code, jfips10'),
  cnty_name = case_when(
    year == '1940' ~ jname40,
    year == '2010' ~ jcnam10)) %>% 
  dplyr::select(-jnhgis40, -jfips10, -jname40, -jcnam10)

# 5b Merge all datasets
allData <- allData %>% 
  left_join(seg2010, by = c('st_cnty_yr_id' = 'stctya_2010')) %>% 
  left_join(sociodemo1940, by = c('st_cnty_yr_id' = 'jnhgis40')) %>% 
  left_join(seg1940, by = c('st_cnty_yr_id' = 'jnhgis40')) %>% 
  left_join(segNbr1940, by = c('st_cnty_yr_id' = 'jnhgis40')) %>% 
  left_join(rural, by = c('st_cnty_yr_id', 'st_cnty_yr_id_type')) %>% 
  left_join(envExp4, by = c('st_cnty_yr_id', 'st_cnty_yr_id_type'))

# 5c Reorder variables
allData <- allData %>% 
  dplyr::select(year:cnty_name, stateCountyString_1940, cnty_name_ruraldf, 
                statefp10, statefip_1940,
                contains('totPop'), contains('black'), 
                cnty_di_bw_1940, di_2010, contains('ICE'), everything())

# 5d Combine key vars from 1940 and 2010 in one column
allData <- allData %>% 
  mutate(statefip = ifelse(year == '1940', statefip_1940, statefp10), .after = cnty_name) %>%
  mutate(totPop = ifelse(year == '1940', totPop_1940, totPop_2010), .after = cnty_name_ruraldf) %>%
  rename(nhwhitePop = nhwhitePop_2010) %>% 
  mutate(blackPop = ifelse(year == '1940', black_1940, blackPop_2010), .after = totPop) %>%
  mutate(di = ifelse(year == '1940', cnty_di_bw_1940, di_2010), .after = blackPop) %>%
  mutate(raceICE = ifelse(year == '1940', raceICE_cnty_1940, raceEthICE_2010), .after = di) %>% 
  mutate(raceEduICE_hsd10 = ifelse(year == '1940', raceeduICE_cnty_1940, raceEduICE_2010), .after = raceICE) %>% 
  mutate(raceEduICE_sc10 = ifelse(year == '1940', raceeduICE_cnty_1940, raceEduICE2_2010), .after = raceEduICE_hsd10) %>% 
  dplyr::select(-contains('1940'), -contains('2010'), -cnty_name_ruraldf, -statefp10)

# 5e Review missingness
#    Notes: -statefip: missing n = 1. Imputed with correct value - this was Yellowstone
#            National Park in Idaho id 1600875 (1940)
#           -totPop and blackPop: missing for Yellowstone National Park in Idaho 
#            also because there was a population of 0 for this county
#           -di: Missing Black/White Dissimilarity Index for 450 counties
#            all counties had <50 Black population, so will not be included in
#            analysis anyway, except: Okfuskee OK 1940 [4001070]
#            Taylor says this value is NA because there was 0 Black population in
#            the individual count data (in the county data there is a Black pop of
#            ~ 6k). Given that this is only one county and we use the individual
#            level data for DI calculation (from enumeration districts), we will
#            leave this as missing since we don't know the enumeration district
#            level Black population counts
#           -ICE: Missing all ICE vars for 1 county: Yellowstone National Park in 
#            Idaho id 1600875 (1940). There was a population of 0, so ICE could 
#            not be calculated
#           -nhwhite: Missing nhwhite population for all of 1940; this var not 
#            available in 1940
#           -alpha_po and alpha_pb: Missing neighbor index from census pages for 1 neighbor
#            for 3841-3109=732 counties, and for 2 neighbors for 3881-3109=772 counties. 
#            (neighbor index not available for 2010 b/c mail-in Census)
#            Most, but not all, are counties with very low Black population.
#            This is a limitation of the dataset and we cannot resolve it. Will
#            use this segregation variable as secondary analysis in part due to
#            its incomplete spatial coverage. 
#           -plural variables: Missing plural (remoteness) continuous data for 114  
#            counties, but these values have all been imputed (plural_imp) in script
#            1_06 to resolve this issue. It is correct that most plural_notes rows
#            will be missing because we only made notes for plural values that 
#            needed to be imputed
#           -pm2.5_rm: Missing pm2.5 from Randall Martin model for all of 1940
#            because the model is not available for that year
# 5e.i Use summary function to review all variables, then use sum(is.na) function 
#      to get counts for all missingness, including for character variables
#      Notes on missingness listed above, in order vars appear in df.
summary(allData)
sapply(allData, function(x) sum(is.na(x)))
# 5e.ii Fill in missing statefip value with 16 for Idaho. This is Yellowstone
#       National Park in Idaho. Reviewing this missing row also explains why totPop
#       and blackPop are missing 1 value -- this county did not have a population
#       in 1940
m <- allData %>% filter(is.na(statefip))
allData <- allData %>% mutate(statefip = ifelse(st_cnty_yr_id == '1600875', '16', statefip))
# 5e.iii Review state/county/years missing DI values
#        All obs have <50 Black population and so will not be used in analysis OTHER
#        than Okfuskee OK 1940 [4001070]
o <- allData %>% filter(is.na(di))
# 5e.iv Confirm that all rows missing nhwhitePop are in 1940
q <- allData %>% filter(is.na(nhwhitePop)); table(q$year, useNA = 'always')
# 5e.v Review rows missing neighbor index to look for any patterns
#      Note: generally these are counties with a smaller Black pop (though not always)
r <- allData %>% filter(is.na(alpha_po) & year == '1940')
# 5e.vi Confirm that all rows missing pm2.5_rm are in 1940
s <- allData %>% filter(is.na(pm2.5_rm)); table(s$year, useNA = 'always')

####**************************************************
#### 6: Revise / Calculate variables for analyses #### 
####**************************************************

# 6a Calculate population density
# 6a.i Calculate area of each county & convert to km^2
county1940$area = st_area(county1940)            # m^2
county1940$area = county1940$area/1000000        # km^2    
county2010$area = st_area(county2010)            # m^2
county2010$area = county2010$area/1000000        # km^2  
# 6a.ii Keep only gisjoin and area vars
area_1940 <- county1940 %>% as_tibble() %>% dplyr::select(jnhgis40, area) %>% 
  mutate(area = as.numeric(area))
area_2010 <- county2010 %>% as_tibble() %>% dplyr::select(jfips10, area) %>% 
  mutate(area = as.numeric(area))
# 6a.iii Add area to exposure dataframe
allData <- allData %>% 
  left_join(area_1940, by = c('st_cnty_yr_id' = 'jnhgis40')) %>% 
  left_join(area_2010, by = c('st_cnty_yr_id' = 'jfips10'))             
# 6a.iv Combine area vars from 1940 and 2010 in one column
allData <- allData %>% 
  mutate(area = ifelse(year == '1940', area.x, area.y)) %>% 
  dplyr::select(-area.x, -area.y)
# 6a.v Calculate pop density (people / km^2)
allData <- allData %>% 
  mutate(popDens = totPop / area, .after = totPop)

# 6b Create indicator variable for if a year-state-county had at least one oil/gas
#    well in 1940 and a separate variable for powerplants
# 6b.i Identify 1940 counties with at least one oil/gas well or powerplant
allData <- allData %>% 
  mutate(
    ogw_in_1940 = case_when(
      year == '1940' & ogw_count > 0 ~ 'og well in 1940', 
      year == '1940' & ogw_count < 1 ~ 'no og well in 1940',
      year == '2010' ~ NA),
    pp_in_1940 = case_when(
      year == '1940' & fospp_count > 0 ~ 'fossil pp in 1940', 
      year == '1940' & fospp_count < 1 ~ 'no fossil pp in 1940',
      year == '2010' ~ NA))
# 6b.ii Use crosswalk to id 2010 counties equivalent to 1940 counties with at
#       least one oil/gas well and at least one powerplant
#       Note: If any of the 1940 counties in a given 2010 county had a oil/gas 
#             well or pp, the entire 2010 county will be assigned 
#             'og well in 1940' or 'fossil pp in 1940', respectively
ogw1940 <- allData %>% filter(year == '1940') %>% 
  dplyr::select(st_cnty_yr_id, ogw_in_1940) %>% 
  full_join(cwalk, by = c('st_cnty_yr_id' = 'jnhgis40')) %>% 
  dplyr::select(ogw_in_1940, jfips10) %>% 
  group_by(jfips10) %>% 
  mutate(mixed_county = n_distinct(ogw_in_1940)) %>% 
  ungroup() %>% 
  mutate(ogw_in_1940_to_collapse = ifelse(mixed_county == 2, 'og well in 1940', ogw_in_1940)) %>% 
  dplyr::select(ogw_in_1940_to_collapse, jfips10) %>% 
  distinct()
fospp1940 <- allData %>% filter(year == '1940') %>% 
  dplyr::select(st_cnty_yr_id, pp_in_1940) %>% 
  full_join(cwalk, by = c('st_cnty_yr_id' = 'jnhgis40')) %>% 
  dplyr::select(pp_in_1940, jfips10) %>% 
  group_by(jfips10) %>% 
  mutate(mixed_county = n_distinct(pp_in_1940)) %>% 
  ungroup() %>% 
  mutate(pp_in_1940_to_collapse = ifelse(mixed_county == 2, 'fossil pp in 1940', pp_in_1940)) %>% 
  dplyr::select(pp_in_1940_to_collapse, jfips10) %>% 
  distinct()
# 6b.iii Add oil/gas well in 1940 variable to 2010 counties in full dataset
allData <- allData %>% 
  left_join(ogw1940, by = c('st_cnty_yr_id' = 'jfips10')) %>% 
  left_join(fospp1940, by = c('st_cnty_yr_id' = 'jfips10')) %>% 
  mutate(ogw_in_1940 = ifelse(year == '1940', ogw_in_1940, ogw_in_1940_to_collapse),
         pp_in_1940 = ifelse(year == '1940', pp_in_1940, pp_in_1940_to_collapse)) %>% 
  dplyr::select(-ogw_in_1940_to_collapse, -pp_in_1940_to_collapse)

# 6c Rename variables for ease and consistency
allData <- allData %>% 
  rename(nbrOne = alpha_po,
         nbrTwo = alpha_pb)

####**********************
#### 7: Save out file #### 
####**********************

# 7a Save merged file
allData %>% write_fst(paste0(merged_data_path, 'env_seg_sociodemo_allyears.fst'))



