# Review of 1940 and 2010 shapefiles & crosswalk
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 08/28/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Filter to contiguous US & harmonize unique identifiers
# 2: Map and save 1940 and 2010 shapefiles and crosswalk

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we restrict both 1940 and 2010 shapefiles to the contiguous
# US and determine the appropriate number of counties for each year:
# 1940: 3,100
# 2010: 3,109
# We also use a county level crosswalk created by Fabian Eckert et al for
# census data from 1790 onward to determine unique harmonized identifiers to use
# in the study. 

# Nb Unique and harmonized geographic identifiers
# In 1940, we must use NHGIS codes as unique identifiers, as FIPS codes did not
# yet exist. Using the county level crosswalk created by Fabian Eckert et al for
# census data from 1790 onward, we use three sets of crosswalked identifiers:
# NHGIS codes in 1940, NHGIS codes in 2010 (which have different lengths / number 
# of characters than those in 1940 in Eckert's dataset), and FIPS codes in 2010. 
# Of note, the 1940 shapefile downloaded from NHGIS has NHGIS codes, while the 2010 
# shapefile from NHGIS has FIPS codes but not NHGIS codes. Both files have a code
# called 'gisjoin' which is the letter G followed by the 3-character state NHGIS
# code and the 4-character county NHGIS code.
# 1940: NHGIS codes 'jnhgis40' (7 characters)
#       Create this identifier: 'jnhgis40 = paste0(nhgisst, nhgiscty)'
# 2010: FIPS codes 'jfips10' (5 characters)
#       Create this identifier: 'jfips10 = paste0(statefp10, countyfp10)'

# Nc The crosswalk (for use in all future merging)
# In the crosswalk, NHGIS 1940 codes have 3 characters for state and 4 for county,
# as expected. However, NHGIS 2010 codes have 2 or 3 characters for state and 3 
# for county. This seems odd, because NHGIS codes should be 3 characters for state
# and 4 for county. In the combined state-county variable I add a left pad of 0 
# to 2-character state codes and a right pad of 0 to the NHGIS county codes to 
# match typical NHGIS code lengths. I manually calculate FIPS codes by adding a
# left pad of 0 to the front of NHGIS 2010 state codes with only 2 characters and 
# then removing the trailing (right) zero from the NHGIS 2010 state code and combining 
# it with the 3-character NHGIS 2010 county code. I manually checked 4 examples.
# Create these identifiers:
# 'jnhgis40 = paste0(nhgisst, nhgiscty)'
# 'jnhgis10 = paste0(str_pad(nhgisst_2010, width = 3, side = "left", pad = "0"), str_pad(nhgiscty_2010, width = 4, side = 'right', pad = '0'))'
# 'jfips10 = paste0(str_sub(str_pad(nhgisst_2010, width = 3, side = "left", pad = "0"), end = -2), nhgiscty_2010)

# Nd Mapping counties in R
# The number of vertices to plot 3,108 US counties is more than 14 million,
# making plots so large that they take a very long time to render in Rstudio.
# Use sum(rapply(st_geometry(county1940), nrow)) to count vertices. My work-around
# is to make maps in R but not view them, instead saving them to pdf for viewing.
# To review the maps real-time, use QGIS.

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & clean environment
source(paste0(project.folder, 'scripts/packages.R'))
rm(list.of.packages, new.packages)

# 0c Set up filepath(s)
sf_data_path <- paste0(project.folder, 'data/county_shapefiles/')

# 0d Load 1940 shapefile and set CRS
#    Note: CRS is in the .prj file, USA_Contiguous_Albers_Equal_Area_Conic
county1940 <- st_read(paste0(sf_data_path, 'nhgis0037_shapefile_tl2008_us_county_1940/US_county_1940_conflated.shp')) %>% 
  janitor::clean_names()

# 0e Load 2010 county shapefile and set CRS
#    Note: CRS is in the .prj file, USA_Contiguous_Albers_Equal_Area_Conic
county2010 <- st_read(paste0(sf_data_path, 'nhgis0047_shapefile_tl2010_us_county_2010/US_county_2010.shp')) %>% 
  janitor::clean_names()

# 0f Load crosswalk
cwalk <- read_csv(paste0(sf_data_path, 'crosswalk/eglp_county_crosswalk_endyr_2010.csv'),
                  col_types = 'iccccccdccccccddl') %>% 
  janitor::clean_names()

####***************************************************************
#### 1: Filter to contiguous US & harmonize unique identifiers #### 
####***************************************************************

# Note: Use NHGIS codes (1940) and FIPS codes (2010) for merging.
#       ICPSR codes are not available for the 2010 shapefile.

# 1a Review names
names(county1940)
names(county2010)
names(cwalk)

# 1b Create combined state-county ids
#    Note: .shp files cannot have variable names >9 characters
# 1b.i 1940 (NHGIS codes): 3 characters for state and 4 for county
county1940 <- county1940 %>% 
  mutate(jnhgis40 = paste0(nhgisst, nhgiscty),
         jname40  = paste0(statenam, nhgisnam))
# 1b.ii 2010 (FIPS codes): 2 characters for state and 3 for county
county2010 <- county2010 %>% 
  mutate(jfips10 = paste0(statefp10, countyfp10),
         jcnam10 = name10)
# 1b.iii Crosswalk
#        Note: In the crosswalk:
#              NHGIS 1940 codes have 3 characters for state and 4 for county, as expected
#              NHGIS 2010 codes have 2 or 3 characters for state and 3 for county
#                This seems odd, because NHGIS codes should be 3 characters for
#                state and 4 for county. In the combined state-county variable I
#                add a left pad of 0 to 2-character state codes and a right pad
#                of 0 to all county codes to match typical NHGIS code lengths
#              I manually calculate FIPS codes by adding a left pad of 0 to the front
#                of NHGIS 2010 state codes with only 2 characters and then removing the 
#                trailing (right) zero from the NHGIS 2010 state code and combining 
#                it with the 3-character NHGIS 2010 county code. I manually checked 4 examples.
cwalk <- cwalk %>% 
  mutate(jnhgis40 = paste0(nhgisst, nhgiscty),
         jnhgis10 = paste0(str_pad(nhgisst_2010, width = 3, side = "left", pad = "0"), 
                           str_pad(nhgiscty_2010, width = 4, side = 'right', pad = '0')),
         jfips10 = paste0(str_sub(str_pad(nhgisst_2010, width = 3, side = "left", pad = "0"), end = -2), 
                          nhgiscty_2010))

# 1c Determine unique number of counties
length(unique(county1940$jnhgis40)) # 3,108
length(unique(county2010$jfips10)) # 3,221

# 1d Restrict crosswalk to 1940 and contiguous US
cwalk <- cwalk %>% filter(year == 1940) %>% 
  filter(! statenam %in% c('Alaska Territory', 'Hawaii Territory'))

# 1e Restrict to contiguous US
#    Note: 02 = Alaska, 15 = Hawaii, 60 = American Samoa, 66 = Guam, 
#          72 = Puerto Rico, 78 = Virgin Islands
# 1e.i 1940
county1940 <- as.data.frame(county1940)
county1940 <- county1940 %>% filter(! statenam %in% c('Alaska Territory', 'Hawaii Territory'))
county1940 <- st_as_sf(county1940)
# 1e.ii 2010
county2010 <- as.data.frame(county2010)
county2010 <- county2010 %>% filter(! statefp10 %in% c('02', '15', '60', '66', '72', '78'))
county2010 <- st_as_sf(county2010)

# 1f Re-determine unique number of counties
#    Notes: 2010: 3,143 counties and county equivalents in the 50 states + District of Columbia
#           29 county equivalents in AK and 5 in HI in 2010 --> expected n = 3,109
#           4 county equivalents in AK and 4 in HI in 1940 --> expected n = 3,100
length(unique(county1940$jnhgis40)) # 3,100
length(unique(county2010$jfips10)) # 3,109

# 1g Determine unique number of counties in crosswalk for each year
length(unique(cwalk$jnhgis40)) # 3,100
length(unique(cwalk$jnhgis10)) # 3,109
length(unique(cwalk$jfips10))  # 3,109

# 1h Confirm no duplicate rows in crosswalk
dim(distinct(cwalk)) # same as in environment: 20045 rows x 20 cols

####************************************************************
#### 2: Map and save 1940 and 2010 shapefiles and crosswalk #### 
####************************************************************

# 2a Map of 1940 counties in the contiguous US
map1940 <- county1940 %>% 
  ggplot(aes(geometry = geometry, fill = nhgisst)) + 
  geom_sf(color = 'white') + 
  theme_minimal() +
  theme(legend.position = 'none') +
  ggtitle('1940 Counties, Contiguous US')
ggsave(map1940,
       filename = paste0(sf_data_path, 'counties_contiguous_1940.pdf'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)

# 2a Map of 2010 counties in the contiguous US
map2010 <- county2010 %>% 
  ggplot(aes(geometry = geometry, fill = statefp10)) + 
  geom_sf(color = 'white') + 
  theme_minimal() +
  theme(legend.position = 'none') +
  ggtitle('2010 Counties, Contiguous US')
ggsave(map2010,
       filename = paste0(sf_data_path, 'counties_contiguous_2010.pdf'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)

# 2c Save shapefiles and crosswalk
st_write(county1940, paste0(sf_data_path, 'FINAL_1940_county_shapefile_clean/counties_contiguous_1940.shp'), append=FALSE)
st_write(county2010, paste0(sf_data_path, 'FINAL_2010_county_shapefile_clean/counties_contiguous_2010.shp'), append=FALSE)
write_fst(cwalk, paste0(sf_data_path, 'crosswalk/FINAL_county_crosswalk_clean_1940-2010.fst'))


