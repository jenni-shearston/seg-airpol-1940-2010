# Review PLURAL Metric for Rural/Urban Definition
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 09/18/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Choose a cutoff for a binary rural/urban definition
# 2: Assign PLURAL places to county boundaries in 1940 and 2010
# 3: Combine, create binary variables, & save

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we explore the continuous PLURAL index created by Uhl et al.
# We determine a cutoff for a binary definition of rural/urban. 
# We aggregate the PLURAL index (which was created at the Census Place level) to
# counties, for both 1940 county boundaries and 2010 county boundaries.

# Nb PLURAL Remoteness Index
# In short, the PLURAL remoteness index uses the population of Census places,
# and their distance to other Census places of varying population sizes,
# to determine a remoteness value (Plural1). A second index value, Plural2,
# uses a network approach rather than weighted distance to other places. 
# We use Plural1 because we found it easier to understand and to recreate
# computationally, if needed.
# The index values have been calculated with different weights and different 
# scaling features. We use values that were scaled across years because we will
# use more than one time point, and we use equal weights rather than prioritizing
# a value that puts more emphasis on certain kinds of places. 
# In email correspondence with Johannes Uhl (lead author on PLURAL development)
# and Dylan Connor, they used a binary cutoff of 0.55 for 1980. They also
# recommended doing sensitivity analyses for whatever cutoff value we choose
# 1 = more rural, 0 = more urban

# Nc Citations
# This is the paper describing the PLURAL index: 
# https://www.sciencedirect.com/science/article/pii/S0169204623000816
# This paper uses PLURAL in a study of social mobility with a binary cutoff of 0.55: 
# https://www.sciencedirect.com/science/article/pii/S0276562423000884

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & clean environment
source(paste0(project.folder, 'scripts/packages.R'))
rm(list.of.packages, new.packages)

# 0c Set up filepath(s)
rural_data_path <- paste0(project.folder, 'data/uhl_ruralurban_data/')
prelim_plot_path <- paste0(project.folder, 'outputs/prelim_plots/plots_for_dichot_plural/')
spatial_data_path <- paste0(project.folder, 'data/county_shapefiles/')

# 0d Load rural/urban data
#    Note: CRS is USA_Contiguous_Albers_Equal_Area_Conic
rural <- read_csv(paste0(rural_data_path, 'place_level_remoteness_plural1_equal_weights_scaled_across_years.csv'))
rural1940_place <- st_read(paste0(rural_data_path, 'PLURAL_SHP/plural_indices_scaled_across_years_1940.shp'))
rural2010_place <- st_read(paste0(rural_data_path, 'PLURAL_SHP/plural_indices_scaled_across_years_2010.shp'))

# 0e Load county shapefiles
#    Note: CRS is USA_Contiguous_Albers_Equal_Area_Conic
county1940 <- st_read(paste0(spatial_data_path, 
                             'FINAL_1940_county_shapefile_clean/counties_contiguous_1940.shp')) %>% 
  janitor::clean_names()
county2010 <- st_read(paste0(spatial_data_path, 
                             'FINAL_2010_county_shapefile_clean/counties_contiguous_2010.shp')) %>% 
  janitor::clean_names()
cwalk <- read_fst(paste0(spatial_data_path, 'crosswalk/FINAL_county_crosswalk_clean_1940-2010.fst'))

####************************************************************
#### 1: Choose a cutoff for a binary rural/urban definition #### 
####************************************************************

# 1a Review variables and identifiers
#    Will use county_fips as identifier -- already have built in crosswalk and 
#    in 2010 shapefile, and as this dataset spans decades from 1930-2018, the
#    PLURAL authors already had to contend with changing county boundaries and
#    identifiers, and chose to use fips codes, so I will too. PLURAL has
#    3,102 unique counties, which is similar to my 3100 for 1940 and 3109 for 2010
#    Will need to explore any counties missing a PLURAL measure.
names(rural)
length(unique(rural$county_fips)) # n = 3102

# 1b Aggregate to county level
rural_county <- rural %>% group_by(county_fips) %>% 
  summarize(plural1_1940 = mean(plural1_equal_weights_1940, na.rm = T),
            plural1_2010 = mean(plural1_equal_weights_2010, na.rm = T),
            state_fips = first(state_fips),
            state_name = first(state_name),
            county_name = first(county_name))

# 1c Review distribution
#    Note: 101 counties missing plural in 1940, and 2 in 2010. Will need to
#          explore reasons for missingness. 
ggplot(rural_county) + geom_histogram(aes(x = plural1_1940))
summary(rural_county$plural1_1940)
ggplot(rural_county) + geom_histogram(aes(x = plural1_2010))
summary(rural_county$plural1_2010)

# 1d Break PLURAL into multiple categorical variables
#    Note: We will dichotomize the PLURAL index but need to choose a cutoff
#          We select a cutoff based on a visual analysis of the below maps.
#            We wanted to make sure there were enough counties in each category
#            for a sound statistical analysis while also classifying major cities
#            as urban and major remote areas (e.g., Mississippi Delta) as rural
rural_county <- rural_county %>% 
  mutate(plural1_10_1940 = gtools::quantcut(plural1_1940, 10),
         plural1_5_1940 = gtools::quantcut(plural1_1940, 5),
         plural1_4_1940 = gtools::quantcut(plural1_1940, 4),
         plural1_.55_1940 = factor(ifelse(plural1_1940 >= 0.55, 1, 0)),
         plural1_.6_1940 = factor(ifelse(plural1_1940 >= 0.6, 1, 0)),
         plural1_.65_1940 = factor(ifelse(plural1_1940 >= 0.65, 1, 0)),
         plural1_.7_1940 = factor(ifelse(plural1_1940 >= 0.7, 1, 0)),
         plural1_.75_1940 = factor(ifelse(plural1_1940 >= 0.75, 1, 0)),
         plural1_.78_1940 = factor(ifelse(plural1_1940 >= 0.78, 1, 0)),
         plural1_.8_1940 = factor(ifelse(plural1_1940 >= 0.8, 1, 0)),
         plural1_.85_1940 = factor(ifelse(plural1_1940 >= 0.85, 1, 0)),
         plural1_.9_1940 = factor(ifelse(plural1_1940 >= 0.9, 1, 0)),
         plural1_4bin_1940 = factor(ifelse(plural1_1940 == '(0.799,0.979]', 1, 0)),
         plural1_5bin_1940 = factor(ifelse(plural1_1940 == '(0.811,0.979]', 1, 0)),
         plural1_.55_2010 = factor(ifelse(plural1_2010 >= 0.55, 1, 0)),
         plural1_.6_2010 = factor(ifelse(plural1_2010 >= 0.6, 1, 0)),
         plural1_.65_2010 = factor(ifelse(plural1_2010 >= 0.65, 1, 0)),
         plural1_.7_2010 = factor(ifelse(plural1_2010 >= 0.7, 1, 0)),
         plural1_.75_2010 = factor(ifelse(plural1_2010 >= 0.75, 1, 0)),)

# 1e Review distribution of new vars
summary(rural_county$plural1_10_1940)
summary(rural_county$plural1_5_1940)
summary(rural_county$plural1_4_1940)
table(rural_county$plural1_.55_1940)
table(rural_county$plural1_.6_1940)
table(rural_county$plural1_.65_1940)
table(rural_county$plural1_.7_1940)
table(rural_county$plural1_.75_1940)
table(rural_county$plural1_.78_1940)
table(rural_county$plural1_.8_1940)
table(rural_county$plural1_.85_1940)
table(rural_county$plural1_.9_1940)
table(rural_county$plural1_4bin_1940)
table(rural_county$plural1_5bin_1940)
table(rural_county$plural1_.55_2010)
table(rural_county$plural1_.6_2010)
table(rural_county$plural1_.65_2010)
table(rural_county$plural1_.7_2010)
table(rural_county$plural1_.75_2010)

# 1f Merge with shapefile
county2010 <- county2010 %>% 
  left_join(rural_county, by = c('jfips10' = 'county_fips'))

# 1g Create plots for each variable
#    Note: These plots may take some time to run due to the size of the shapefile.
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_1940), color = NA) + 
    scale_fill_viridis() + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_cont_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_10_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_10_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_5_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_5_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_4_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_4_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.55_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.55_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.6_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.6_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.65_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.65_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.7_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.7_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.75_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.75_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.78_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.78_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.8_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.8_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.85_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.85_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.9_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.9_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_4bin_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_4bin_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_5bin_1940), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_5bin_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.55_2010), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.55_2010.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.6_2010), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.6_2010.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.65_2010), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.65_2010.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.7_2010), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.7_2010.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
(ggplot() + geom_sf(data = county2010, aes(geometry = geometry, fill = plural1_.75_2010), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_.75_2010.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)

# Notes: Selected 0.65
#   0.9 - too few rural counties
#   0.85 - too few rural counties
#   0.8 - MS Delta clear, Appalacia may not be rural enough
#   0.78 - Appalacia still not classified as very rural
#   0.75 - Upstate NY and parts of CA are not rural that should be
#   0.7  - this seems to classify several areas we expect to be rural as rural,
#          while still keeping the group sizes large enough not to cause power issues
#   0.65 - even better at picking out rural areas and sufficient power
#   0.60 - not enough power; too few counties in one bin
#   0.55 - not enough power; too few counties in one bin 
#   top quintile (5 bin) - too few rural counties; MS Delta not clear
#   top quartile (4 bin) - almost identical to 0.8; prefer score-based cut

####*******************************************************************
#### 2: Assign PLURAL places to county boundaries in 1940 and 2010 ####
####*******************************************************************

# Notes:
# Now that we've chosen a cutoff, we will do some spatial work to make sure
# we get Census places into the right counties in 1940 and 2010 respectively.
# This is necessary because only one identifier (fips codes) is given for the csv
# version of the PLURAL file. We know that fips codes did not exist in 1940, but
# are not sure to what year's county boundaries the fips codes in the csv correspond. 
# We will instead use the shp files of census places provided with the PLURAL
# data, for 1940 and 2010 separately. We will then overlay each shp file with county
# boundaries for each year, and average the place PLURAL value within the county.
# We will then use our cutoffs to create binary values for each county and year.
# We determined the cutoff using only 2010 county boundaries, but suspect this 
# won't really change our cutoff choice. We will double check that our
# binary cutoff still looks good with both the 2010 and 1940 boundaries after
# aggregating to each set of boundaries.

# 2a Extract AZ counties to use as test plot
#    All US counties is too big to plot easily in R (too many vertices)
#    so instead I will plot AZ as a test
az1940 <- as.data.frame(county1940)
az1940 <- az1940 %>% filter(statenam == 'Arizona')
az1940 <- st_as_sf(az1940)
az2010 <- as.data.frame(county2010)
az2010 <- az2010 %>% filter(statefp10 == '04')
az2010 <- st_as_sf(az2010)

# 2b Plot points on top of counties for 1940
#    This will still take a minute or so. We will do all of 1940 first
#    and then 2010 so that we don't have to reload the plot to visually 
#    confirm the spatial join worked 
plot(rural1940_place$geometry, pch = 21, cex = 0.7, col = 'purple', bg = 'gray80')
plot(az1940$geometry, border = 'gray20', col = NA, add = T)

# 2c Find places within counties for 1940
places_in_counties1940 <- st_join(rural1940_place, county1940, join = st_within)
# Visually confirmed the following AZ counties:
# Cochise (1940): 5 places
# Apache (1940): 0 places (therefore not present?)
# Navajo (1940): 2 places

# 2d Plot points on top of counties for 1940
plot(rural2010_place$geometry, pch = 21, cex = 0.7, col = 'purple', bg = 'gray80')
plot(az2010$geometry, border = 'gray20', col = NA, add = T)

# 2e Find places within counties for 2010
places_in_counties2010 <- st_join(rural2010_place, county2010, join = st_within)

# 2f Aggregate plural values within county by averaging
rural1940_county <- as.data.frame(places_in_counties1940)
rural1940_county <- rural1940_county %>% group_by(jnhgis40) %>% 
  summarise(plural_cont = mean(plural1_ew)) # n = 2996
rural2010_county <- as.data.frame(places_in_counties2010)
rural2010_county <- rural2010_county %>% group_by(jfips10) %>% 
  summarise(plural_cont = mean(plural1_ew)) # n = 3101

# 2g Review missing counties
#    View places overlaid on counties in QGIS
mis_1940 <- setdiff(county1940$jnhgis40, rural1940_county$jnhgis40)
mis_2010 <- setdiff(county2010$jfips10, rural2010_county$jfips10)

# 2h Add notes for missing counties and convert to dataframe
#    Notes: Counties missing a plural value did not contain a place. In many, 
#           but not all cases, these counties are likely quite rural.
#    2010
#      -all missing New York counties (fips code starts with 36) are part of NYC. 
#       The lat/lon for NYC as a point was in Manhattan, so the other four counties 
#       in NYC did not have a place inside them. As they are all the same city, we  
#       should assign those counties the plural value of Manhattan (36061) = 0.01325047
#      -for other counties, should check population of county
#      -county 51081 had no place but did completely encircle a second county, 
#       which was tiny and had one place inside it
#    1940
#      -in 1940 a lot more counties do not have places. I suspect this is because
#       the US was more rural in 1940
#      -Same issue with NYC as in 2010
#      -JAS did not review all 105 missing counties, only a few. I think they all are
#       missing because they do not contain a place but did not visually confirm.
#       EB will confirm via visual inspection -- please see script 1_05
mis_2010 <- as.data.frame(mis_2010) %>% rename(jfips10 = mis_2010) %>% 
  mutate(plural_notes = case_when(
    str_detect(jfips10, '360') ~ 'NYC outer boroughs; assign same value as Manhattan, fips 36061',
    jfips10 == '50013' | jfips10 == '44001' | jfips10 == '51145' | jfips10 == '51095' ~ 'no place in county',
    jfips10 == '51081' ~ 'no place in county but completely encircles a second county w a place'))
mis_1940 <- as.data.frame(mis_1940) %>% rename(jnhgis40 = mis_1940) %>% 
  mutate(plural_notes = case_when(
    str_detect(jnhgis40, '3600') ~ 'NYC outer boroughs; assign same value as Manhattan, fips 36061',
    TRUE ~ 'no place in county'))

####*************************************************
#### 3: Combine, create binary variables, & save ####  
####*************************************************

# 3a Create case complete year-state-county dataset
#    this is built from the 1940 and 2010 shapefiles
# 3a.i 1940 - select unique identifiers from script 0_01
d40 <- as.data.frame(county1940)
d40 <- d40 %>% dplyr::select(jnhgis40, jname40) %>% mutate(year = '1940')
length(unique(d40$jnhgis40)); length(unique(d40$jname40))
# 3a.ii 2010 - select unique identifiers from script 0_01
d10 <- as.data.frame(county2010)
d10 <- d10 %>% dplyr::select(jfips10, jcnam10) %>% mutate(year = '2010')
length(unique(d10$jfips10)) # jcnam10 is not unique until merged w state names
# 3a.iii Bind 1940 and 2010 together
rural_county <- d10 %>% bind_rows(d40)
# 3a.iv Combine ids into one column, with separate column for id type
rural_county <- rural_county %>% mutate(
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

# 3b Merge plural values to case complete dataframe
rural_county <- rural_county %>% 
  left_join(rural1940_county, by = c('st_cnty_yr_id' = 'jnhgis40')) %>% 
  left_join(rural2010_county, by = c('st_cnty_yr_id' = 'jfips10')) %>% 
  mutate(plural_cont = case_when(
    year == '1940' ~ plural_cont.x,
    year == '2010' ~ plural_cont.y)) %>% 
  dplyr::select(-plural_cont.x, -plural_cont.y)

# 3c Merge missingness data to case complete dataframe
rural_county <- rural_county %>% 
  left_join(mis_1940, by = c('st_cnty_yr_id' = 'jnhgis40')) %>% 
  left_join(mis_2010, by = c('st_cnty_yr_id' = 'jfips10')) %>% 
  mutate(plural_notes = case_when(
    year == '1940' ~ plural_notes.x,
    year == '2010' ~ plural_notes.y)) %>% 
  dplyr::select(-plural_notes.x, -plural_notes.y)

# 3d Confirm all year-state-county obs have either a plural value or missing note
mis <- rural_county %>% filter(is.na(plural_cont) & is.na(plural_notes)) # 0 obs

# 3e Create binary plural variables
#    Note: ID 51081 has no place in county, and is a small city that is its own 
#          county. Subsequent review in script 1_05 confirms it is pretty rural
rural_county <- rural_county %>% 
  mutate(
    plural_bin = case_when(
      plural_cont >= 0.65 ~ '1',
      plural_cont < 0.65 ~ '0',
      plural_notes == 'NYC outer boroughs; assign same value as Manhattan, fips 36061' ~ '0',
      plural_notes == 'no place in county' ~ '1',
      st_cnty_yr_id == '51081' ~ '1'),
    plural_bin = factor(plural_bin, levels = c('0', '1'), labels = c('Urban', 'Rural')))

# 3f Confirm distribution of binary values
table(rural_county$plural_bin, useNA = 'always')
rural_county %>% ggplot() + geom_bar(aes(x = plural_bin)) + facet_wrap(~ year)

# 3g Confirm maps of binary variables look good
#    Note: Takes time to run this code -- for faster viewing, do not save
bintest1940 <- county1940 %>% left_join(rural_county, by = c('jnhgis40' = 'st_cnty_yr_id'))
(ggplot() + geom_sf(data = bintest1940, aes(geometry = geometry, fill = plural_bin), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_final_1940.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)
bintest2010 <- county2010 %>% left_join(rural_county, by = c('jfips10' = 'st_cnty_yr_id'))
(ggplot() + geom_sf(data = bintest2010, aes(geometry = geometry, fill = plural_bin), color = NA) + 
    scale_fill_viridis(discrete = TRUE) + theme_minimal()) %>% 
  ggsave(filename = paste0(prelim_plot_path, 'plural1_final_2010.pdf'), .,
         width = 3.5*2, height = 2.25*2, scale = 1.2)

# 3g Save data
#    Saved as fst to conserve leading zeros on id variable
rural_county %>% write_fst(paste0(rural_data_path, 'plural_1940_2010_clean.fst'))




