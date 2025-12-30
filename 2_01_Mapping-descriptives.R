# Mapping and descriptive analysis of 1940 and 2010 metrics
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 05/16/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Prepare data for mapping and plotting
# 2: Map all exposures both continuously and as quartiles
# 3: Histograms of exposure variables
# 4: Correlations between exposures
# 5: Distribution of Black population

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we run some basic descriptives of the data, including mapping, 
# histograms, correlations, and distribution. These maps and plots are for
# exploratory purposes, and not for inclusion in the manuscript. Additionally,
# we also review the distribution of the Black population in order to make 
# choices about the minimium Black population a county must have in order to have
# a valid Dissimilarity Index measure. 

# Nb Shapefiles with large numbers of vertices
# The number of vertices to plot 3,109 US counties is more than 14 million,
# making such plots so large they take a very long time to render in Rstudio.
# Use sum(rapply(st_geometry(data_sf), nrow)) to count vertices.
# To resolve this issue, we use st_simplify to reduce the tolerance of the 
# 1940 before mapping, and use a 2010 shapefile pulled from Tidycensus, which
# pulls the simplified shapefile from the Census database (a simplified 
# shapefile for 1940 is not available). 

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & clean environment
source(paste0(project.folder, 'scripts/packages.R'))
rm(list.of.packages, new.packages)

# 0c Set up filepath(s)
county1940_data_path <- paste0(project.folder, 'data/county_shapefiles/FINAL_1940_county_shapefile_clean/')
merged_data_path <- paste0(project.folder, 'data/merged_data/')
prelim_plot_path <- paste0(project.folder, 'outputs/prelim_plots/')

# 0d Load all exposure data
data <- read_fst(paste0(merged_data_path, 'env_seg_sociodemo_allyears.fst'))

# 0e Load 1940 county shapefile, set CRS, and reduce tolerance for faster mapping
#    Note: CRS is an Albers Equal Area Conic with parameters
county1940 <- st_read(paste0(county1940_data_path, 'counties_contiguous_1940.shp')) %>% 
  janitor::clean_names()
st_transform(county1940, 
             crs = st_crs('+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m'))
county1940 <- as.data.frame(county1940)
county1940 <- county1940 %>% filter(! statenam %in% c('Alaska Territory', 'Hawaii Territory'))
county1940 <- st_as_sf(county1940)
round(c(object.size(county1940)))
county1940_2 <- st_simplify(county1940, dTolerance = 1000, preserveTopology = TRUE)
round(c(object.size(county1940_2)))

# 0f Load 2010 shapefile from tidycensus for faster mapping
#    Note: Did not use census API key as only downloading once, so key not needed
county2010_tc <- get_decennial(variable = 'H006001', geography = "county", geometry = TRUE, year = 2010) %>% 
  filter(!str_detect(GEOID, '^02')) %>% 
  filter(!str_detect(GEOID, '^15')) %>% 
  filter(!str_detect(GEOID, '^72')) %>% 
  filter(!str_detect(GEOID, '^66'))
county2010_tc <- st_transform(county2010_tc, 
                              crs = st_crs('+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m'))

# 0g Source functions
source(paste0(project.folder, 'functions/descriptive_maps_plots.R'))

####***********************************************
#### 1: Prepare data for mapping and plotting  #### 
####***********************************************

# 1a Merge county shapefile and exposure data
county1940_2 <- county1940_2 %>% dplyr::select(-state, -county)
data_sf <- data %>% 
  left_join(county1940_2, by = c('st_cnty_yr_id' = 'jnhgis40')) %>% 
  left_join(county2010_tc, by = c('st_cnty_yr_id' = 'GEOID')) %>% 
  mutate(geometry = ifelse(year == '1940', geometry.x, geometry.y)) %>% 
  dplyr::select(-geometry.x, -geometry.y)

# 1b Create tibble of function inputs
#    Note: name vector must correspond exactly to variable names to work in
#          some functions later in the script
var_list <- tibble(
  var_list = list(data_sf$fospp_count, data_sf$ogw_count,
                  data_sf$nox_autos, data_sf$meanPM2.5, 
                  #data_sf$perEmploy, data_sf$perHS, data_sf$homeVal,
                  data_sf$raceEmpICE, data_sf$raceEduICE_hsd10, data_sf$raceICE,
                  data_sf$di, data_sf$nbrOne, data_sf$popDens),
  name_vector = c('fospp_count', 'ogw_count', 
                  'nox_autos', 'meanPM2.5', 
                  #'perEmploy', 'perHS', 'homeVal', 
                  'raceEmpICE', 'raceEduICE_hsd10', 'raceICE', 
                  'di', 'nbrOne', 'popDens'))

####**************************************************************
#### 2: Map all exposures both continuously and as quartiles  #### 
####**************************************************************

# 2a Create all continuous maps
# 2a.i 1940
for (i in 1:length(var_list$name_vector)){
  continuous_exposure_map(dataset = data_sf, 
                          variable_name = var_list$name_vector[[i]], 
                          year = '1940',
                          filepath = paste0(prelim_plot_path, 'maps_continuous/'))}
# 2a.ii 2010
for (i in 1:length(var_list$name_vector)){
  continuous_exposure_map(dataset = data_sf, 
                          variable_name = var_list$name_vector[[i]], 
                          year = '2010',
                          filepath = paste0(prelim_plot_path, 'maps_continuous/'))}

# 2b Create all categorical (quartile) maps
# 2b.i 1940
for (i in 1:length(var_list$name_vector)){
  quartile_exposure_map(dataset = data_sf, 
                        variable_name = var_list$name_vector[[i]], 
                        year = '1940', 
                        filepath = paste0(prelim_plot_path, 'maps_quartiles/'))}
# 2b.ii 2010
for (i in 1:length(var_list$name_vector)){
  quartile_exposure_map(dataset = data_sf, 
                        variable_name = var_list$name_vector[[i]], 
                        year = '1940', 
                        filepath = paste0(prelim_plot_path, 'maps_quartiles/'))}

####*****************************************
#### 3: Histograms of exposure variables #### 
####*****************************************

# 3a Plot histograms for all variables
for (i in 1:length(var_list$name_vector)){
  histogram_plot(dataset = data,
                 variable_name = var_list$name_vector[[i]], 
                 filepath = paste0(prelim_plot_path, 'histograms/'))
}

####***************************************
#### 4: Correlations between variables #### 
####***************************************

# 4a Prepare dataframe for correlations
cor_data <- data %>% 
  dplyr::select(all_of(var_list$name_vector)) 

# 4b Run correlations
#    Notes: Use Spearman because vars not normally distributed
cor <- cor(cor_data, method = c('spearman'), use = 'pairwise.complete.obs')

# 4c Review plot
corrplot(cor, method = 'circle', type = 'lower', order = 'alphabet', cl.cex = 1)

# 4d Identify correlations >= 0.8 or <= -0.8
tidy_cor <- as.data.frame(cor)
tidy_cor <- tidy_cor %>% 
  rownames_to_column(var = 'var1') %>% 
  as_tibble() %>% 
  pivot_longer(cols = !var1, names_to = 'var2', values_to = 'corr') %>% 
  filter(!var1 == var2) %>% 
  mutate(high_corr = ifelse(corr >= 0.8 & corr < 1, 1, 0),
         low_corr  = ifelse(corr <= -0.8, 1, 0))

# 4e Save correlation plots
tiff(paste0(prelim_plot_path, 'corplot.tiff'),
     units = "cm", width = 15, height = 15, res = 300)
corrplot(cor, method = 'circle', type = 'lower', order = 'alphabet', 
         cl.cex = 0.4, tl.cex = 0.5)
dev.off()

####*****************************************
#### 5: Distribution of Black population #### 
####*****************************************

# 5a Determine number of counties that would be included with each inclusion
#    strategy
#    Notes: Use 100
bp <- data_sf %>% filter(year == '1940') %>% 
  mutate(blackPopAll = ifelse(blackPop > 0, blackPop, NA),
         blackPop10 = ifelse(blackPop > 9, blackPop, NA),
         blackPop50 = ifelse(blackPop > 49, blackPop, NA),
         blackPop100 = ifelse(blackPop > 99, blackPop, NA),
         blackPop500 = ifelse(blackPop > 499, blackPop, NA),
         blackPop1000 = ifelse(blackPop > 999, blackPop, NA))
3108 - sum(is.na(bp$blackPopAll)) # n = 2793 included
3108 - sum(is.na(bp$blackPop10)) # 2333
3108 - sum(is.na(bp$blackPop50)) # 1989
3108 - sum(is.na(bp$blackPop100)) # 1834
3108 - sum(is.na(bp$blackPop500)) # 1407
3108 - sum(is.na(bp$blackPop1000)) # 1200

# 5b Convert dataframe to sf
bp_sf <- st_as_sf(bp)

# 5c Map of counties with any Black population
bpAll <- bp_sf %>% 
  ggplot(aes(geometry = geometry, fill = blackPopAll)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis(na.value = "azure2") +
  theme_minimal() +
  ggtitle('All Black Residents') 

# 5d Map of counties with Black population of 10 or greater
bp10 <- bp_sf %>% 
  ggplot(aes(geometry = geometry, fill = blackPop10)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis(na.value = "azure2") +
  theme_minimal() +
  ggtitle('10+ Black Residents')

# 5e Map of counties with Black population of 50 or greater
bp50 <- bp_sf %>% 
  ggplot(aes(geometry = geometry, fill = blackPop50)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis(na.value = "azure2") +
  theme_minimal() +
  ggtitle('50+ Black Residents')

# 5f Map of counties with Black population of 100 or greater
bp100 <- bp_sf %>% 
  ggplot(aes(geometry = geometry, fill = blackPop100)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis(na.value = "azure2") +
  theme_minimal() +
  ggtitle('100+ Black Residents')

# 5g Map of counties with Black population of 500 or greater
bp500 <- bp_sf %>% 
  ggplot(aes(geometry = geometry, fill = blackPop500)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis(na.value = "azure2") +
  theme_minimal() +
  ggtitle('500+ Black Residents')

# 5h Map of counties with Black population of 1000 or greater
bp1000 <- bp_sf %>% 
  ggplot(aes(geometry = geometry, fill = blackPop1000)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis(na.value = "azure2") +
  theme_minimal() +
  ggtitle('1000+ Black Residents')

# 5i Combine all maps
bp_full_map <- (bpAll | (bp10)) / (bp50 | bp100) / (bp500 | bp1000) +
  plot_annotation(title = '1940', 
                  theme = theme(plot.title = element_text(size = 12)))

# 5j Save map
ggsave(bp_full_map,
       filename = paste0(prelim_plot_path, 'map_blackPop_1940.pdf'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)




