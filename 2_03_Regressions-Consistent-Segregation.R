# Regressions between Segregation Metrics & Environmental Exposures by Consistently 
#   High Segregation
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 05/20/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Identify consistently highly segregated 2010 counties & create cross-year rurality variable
# 2: Prepare data for regressions 
# 3: Review consistently highly segregated distribution
# 4: Run regressions
# 5: Review model diagnostics

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we run regressions to compare consistently highly segregated counties
# (counties in the highest quartile of segregation in both 1940 and 2010) to all other
# counties. We use 2010 county boundaries for this analysis, and assign 1940 county 
# segrgation quartile values to 2010 counties using a spatial crosswalk that provides
# area weights for 1940 counties to 2010 counties.

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
model_path <- paste0(project.folder, 'outputs/models/')
plot_path <- paste0(project.folder, 'outputs/prelim_plots/regression_results/')
senke_plot_path <- paste0(project.folder, 'outputs/prelim_plots/exposure_senkes/')
prelim_map_path <- paste0(project.folder, 'outputs/prelim_plots/')
spatial_data_path <- paste0(project.folder, 'data/county_shapefiles/')

# 0d Load all env exp, seg, and sociodemo data
data <- read_fst(paste0(merged_data_path, 'env_seg_sociodemo_allyears.fst'))
cwalk <- read_fst(paste0(spatial_data_path, 'crosswalk/FINAL_county_crosswalk_clean_1940-2010.fst'))
county2010 <- st_read(paste0(spatial_data_path, 
                             'FINAL_2010_county_shapefile_clean/counties_contiguous_2010.shp')) %>% 
  janitor::clean_names()
state1940 <- st_read(paste0(project.folder, 'data/state_shapefiles/US_state_1940.shp')) %>% 
  janitor::clean_names()
state1940 <- as.data.frame(state1940)
state1940 <- state1940 %>% filter(! statenam %in% c('Alaska Territory', 'Hawaii Territory'))
state1940 <- st_as_sf(state1940) 
county2010_tc <- get_decennial(variable = 'H006001', geography = "county", geometry = TRUE, year = 2010) %>% 
  filter(!str_detect(GEOID, '^02')) %>% 
  filter(!str_detect(GEOID, '^15')) %>% 
  filter(!str_detect(GEOID, '^72')) %>% 
  filter(!str_detect(GEOID, '^66'))
county2010_tc <- st_transform(county2010_tc, 
                              crs = st_crs('+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m'))

####*****************************************************************************************************
#### 1: Identify consistently highly segregated 2010 counties & create cross-year rurality variable  #### 
####*****************************************************************************************************

# 1a Select only vars needed for identifying consistently highly segregated counties
data_persmarg1 <- data %>% 
  dplyr::select(year, st_cnty_yr_id,              # id vars
                di, raceEduICE_hsd10,             # seg metrics
                plural_bin_imp,                   # covars, strat
                blackPop)                         # restriction vars 
     
# 1b Split 1940 and 2010 seg data into separate dfs
seg40 <- data_persmarg1 %>% filter(year == '1940') %>% ungroup() %>%
  dplyr::select(-plural_bin_imp, -year) 
seg10 <- data_persmarg1 %>% filter(year == '2010') %>% ungroup() %>% 
  dplyr::select(-year) 

# 1c Assign 1940 county seg values to corresponding 2010 counties using the cwalk
#    weight - calculate a weighted average
seg40cwalk <- cwalk %>% full_join(seg40, by = c('jnhgis40' = 'st_cnty_yr_id')) %>% 
  mutate(raceEduICE_hsd10_1940_w = raceEduICE_hsd10*weight,
         di_1940_w = di*weight,
         blackPop_1940_w = blackPop*weight) %>% 
  group_by(jfips10) %>% 
  summarise(weight_sum = sum(weight),
            raceEduICE_hsd10_1940_ws = sum(raceEduICE_hsd10_1940_w, na.rm = T)/weight_sum,
            di_1940_ws = sum(di_1940_w, na.rm = T)/weight_sum,
            blackPop_1940_ws = sum(blackPop_1940_w, na.rm = T)/weight_sum) %>% 
  ungroup() %>% dplyr::select(-weight_sum)

# 1d Join crosswalked 1940 values to 2010 values
data_persmarg2 <- seg10 %>% 
  full_join(seg40cwalk, by = c('st_cnty_yr_id' = 'jfips10'))

# 1e Pivot longer and nest
#    Note: We want each dataframe to contain only one seg metric - year for the
#          non-stratified analysis, and one seg metric - year - 2010 rural cat
#          for the stratified analysis
# 1e.i Full dataset
data_persmarg2_all <- data_persmarg2 %>% dplyr::select(-plural_bin_imp) %>% 
  pivot_longer(cols = c('di', 'raceEduICE_hsd10', 'raceEduICE_hsd10_1940_ws', 'di_1940_ws'), 
               names_to = 'segMetric', values_to = 'segValue') %>% 
  group_by(segMetric) %>% 
  nest() 
# 1e.ii 2010 Rurality stratified dataset
data_persmarg2_strat <- data_persmarg2 %>% 
  pivot_longer(cols = c('di', 'raceEduICE_hsd10', 'raceEduICE_hsd10_1940_ws', 'di_1940_ws'), 
               names_to = 'segMetric', values_to = 'segValue') %>% 
  group_by(segMetric, plural_bin_imp) %>% 
  nest() 

# 1f Remove counties with min Black pop less than 100 for DI
data_persmarg2_all$data[[1]] = subset(data_persmarg2_all$data[[1]], blackPop > 99 & blackPop_1940_ws > 99)
data_persmarg2_all$data[[4]] = subset(data_persmarg2_all$data[[4]], blackPop > 99 & blackPop_1940_ws > 99)
data_persmarg2_strat$data[[1]] = subset(data_persmarg2_strat$data[[1]], blackPop > 99 & blackPop_1940_ws > 99)
data_persmarg2_strat$data[[4]] = subset(data_persmarg2_strat$data[[4]], blackPop > 99 & blackPop_1940_ws > 99)
data_persmarg2_strat$data[[5]] = subset(data_persmarg2_strat$data[[5]], blackPop > 99 & blackPop_1940_ws > 99)
data_persmarg2_strat$data[[8]] = subset(data_persmarg2_strat$data[[8]], blackPop > 99 & blackPop_1940_ws > 99)

# 1g Create quantiles of each seg metric
# 1g.i Full dataset
data_persmarg_breaks_all <- data_persmarg2_all %>% 
  mutate(min = map(.x = data, ~min(.x$segValue, na.rm = T)),
         q1 = map(.x = data, ~quantile(.x$segValue, probs = 0.25, na.rm = T, names = F)),
         q2 = map(.x = data, ~quantile(.x$segValue, probs = 0.5, na.rm = T, names = F)),
         q3 = map(.x = data, ~quantile(.x$segValue, probs = 0.75, na.rm = T, names = F)),
         max = map(.x = data, ~max(.x$segValue, na.rm = T))) %>% 
  dplyr::select(-data)
# 1g.ii 2010 Rurality stratified dataset
data_persmarg_breaks_strat <- data_persmarg2_strat %>% 
  mutate(min = map(.x = data, ~min(.x$segValue, na.rm = T)),
         q1 = map(.x = data, ~quantile(.x$segValue, probs = 0.25, na.rm = T, names = F)),
         q2 = map(.x = data, ~quantile(.x$segValue, probs = 0.5, na.rm = T, names = F)),
         q3 = map(.x = data, ~quantile(.x$segValue, probs = 0.75, na.rm = T, names = F)),
         max = map(.x = data, ~max(.x$segValue, na.rm = T))) %>% 
  dplyr::select(-data)

# 1h Join quintile breaks to main dataset
# 1h.i Full dataset
data_persmarg2_all <- data_persmarg2_all %>% 
  left_join(data_persmarg_breaks_all, by = c('segMetric')) 
rm(data_persmarg_breaks_all)
# 1h.ii 2010 Rurality stratified dataset
data_persmarg2_strat <- data_persmarg2_strat %>% 
  left_join(data_persmarg_breaks_strat, by = c('segMetric', 'plural_bin_imp')) 
rm(data_persmarg_breaks_strat)

# 1i Create categorical variables for each seg metric based on quantiles
# 1i.i Full dataset
for(i in 1:length(data_persmarg2_all$data)){
  data_persmarg2_all$data[[i]]$segMetric_quant = case_when(
    data_persmarg2_all$data[[i]]$segValue < data_persmarg2_all$q1[[i]] ~ 1,
    data_persmarg2_all$data[[i]]$segValue >= data_persmarg2_all$q1[[i]] & data_persmarg2_all$data[[i]]$segValue < data_persmarg2_all$q2[[i]] ~ 2,
    data_persmarg2_all$data[[i]]$segValue >= data_persmarg2_all$q2[[i]] & data_persmarg2_all$data[[i]]$segValue < data_persmarg2_all$q3[[i]] ~ 3,
    data_persmarg2_all$data[[i]]$segValue >= data_persmarg2_all$q3[[i]] ~ 4
  )
}
# 1i.ii 2010 Rurality stratified datset
for(i in 1:length(data_persmarg2_strat$data)){
  data_persmarg2_strat$data[[i]]$segMetric_quant = case_when(
    data_persmarg2_strat$data[[i]]$segValue < data_persmarg2_strat$q1[[i]] ~ 1,
    data_persmarg2_strat$data[[i]]$segValue >= data_persmarg2_strat$q1[[i]] & data_persmarg2_strat$data[[i]]$segValue < data_persmarg2_strat$q2[[i]] ~ 2,
    data_persmarg2_strat$data[[i]]$segValue >= data_persmarg2_strat$q2[[i]] & data_persmarg2_strat$data[[i]]$segValue < data_persmarg2_strat$q3[[i]] ~ 3,
    data_persmarg2_strat$data[[i]]$segValue >= data_persmarg2_strat$q3[[i]] ~ 4
  )
}

# 1j Unnest and keep only vars needed to make consistently highly segregated variable
#    and plural_bin to make new rural categorical variable
# 1j.i Full dataset
data_persmarg3_all <- data_persmarg2_all %>% dplyr::select(segMetric, data) %>% 
  unnest(data) %>% dplyr::select(-blackPop, -blackPop_1940_ws, -segValue) %>% 
  pivot_wider(names_from = segMetric, values_from = segMetric_quant)
# 1j.ii 2010 rurality stratified datset
data_persmarg3_strat <- data_persmarg2_strat %>% dplyr::select(plural_bin_imp, segMetric, data) %>% 
  unnest(data) %>% dplyr::select(-blackPop, -blackPop_1940_ws, -segValue) %>% 
  pivot_wider(names_from = segMetric, values_from = segMetric_quant)

# 1k Create consistently highly segregated variable
# 1k.i Full dataset
data_persmarg4_all <- data_persmarg3_all %>% 
  mutate(di_persmarg = case_when(
           di == 4 & di_1940_ws == 4 ~ TRUE,
           is.na(di) | is.na(di_1940_ws) ~ NA,
           TRUE ~ FALSE),
         ice_persmarg = case_when(
           raceEduICE_hsd10 == 4 & raceEduICE_hsd10_1940_ws == 4 ~ TRUE,
           is.na(raceEduICE_hsd10) | is.na(raceEduICE_hsd10_1940_ws) ~ NA,
           TRUE ~ FALSE)) %>% 
  dplyr::select(st_cnty_yr_id, di_persmarg, ice_persmarg)
# 1k.ii 2010 rurality stratified datset
data_persmarg4_strat <- data_persmarg3_strat %>% 
  mutate(di_persmarg = case_when(
           di == 4 & di_1940_ws == 4 ~ TRUE,
           is.na(di) | is.na(di_1940_ws) ~ NA,
           TRUE ~ FALSE),
         ice_persmarg = case_when(
           raceEduICE_hsd10 == 4 & raceEduICE_hsd10_1940_ws == 4 ~ TRUE,
           is.na(raceEduICE_hsd10) | is.na(raceEduICE_hsd10_1940_ws) ~ NA,
           TRUE ~ FALSE)) %>% 
  dplyr::select(st_cnty_yr_id, plural_bin_imp, di_persmarg, ice_persmarg)

# 1l Create new rural categorical variable that incorporates rurality in 1940 and
#    2010: level 1 = rural both years, level 2 = rural 1940 and urban 2010, 
#          level 3 = urban 1940 and rural 2010, level 4 = urban both years
#    Note: Only need this variable for the full (non-stratified) dataset
# 1l.i Separate 1940 data into its own df
plural40 <- data_persmarg1 %>% filter(year == '1940') %>% ungroup() %>%
  dplyr::select(st_cnty_yr_id, plural_bin_imp) %>% 
  rename(plural_bin_1940 = plural_bin_imp)
# 1l.ii Separate 2010 data into its own df
plural10 <- data_persmarg1 %>% filter(year == '2010') %>% ungroup() %>% 
  dplyr::select(st_cnty_yr_id, plural_bin_imp) %>% 
  rename(plural_bin_2010 = plural_bin_imp)
# 1l.iii Option 1: Assign 1940 county plural values to corresponding 2010 counties 
#        using the cwalk max weight
plural40cwalk_maxweight <- cwalk %>% full_join(plural40, by = c('jnhgis40' = 'st_cnty_yr_id')) %>% 
  group_by(jfips10) %>% mutate(max_weight = max(weight)) %>% 
  filter(weight == max_weight) %>% 
  dplyr::select(jfips10, plural_bin_1940)
# 1l.iv Option 2: Assign 1940 county plural values to corresponding 2010 counties 
#       using the cwalk full weights -- weighted average
plural40cwalk_weightedaverage <- cwalk %>% full_join(plural40, by = c('jnhgis40' = 'st_cnty_yr_id')) %>% 
  mutate(plural_bin_1940 = ifelse(plural_bin_1940 == 'Rural', 1, 0),
         plural_bin_1940w = plural_bin_1940*weight) %>% 
  group_by(jfips10) %>% 
  summarise(weight_sum = sum(weight, na.rm = T),
            plural_bin_1940ws = sum(plural_bin_1940w, na.rm = T)/weight_sum) %>% 
  ungroup() %>% 
  mutate(plural_bin_1940ws = round(plural_bin_1940ws, digits = 0))
# 1l.vi Create new 4-level plural var (used weighted average method)
data_pluralcat <- plural10 %>% 
  full_join(plural40cwalk_weightedaverage, by = c('st_cnty_yr_id' = 'jfips10')) %>% 
  mutate(plural_cat = case_when(
    plural_bin_1940ws == 1 & plural_bin_2010 == 'Rural' ~ 'Remained Rural',
    plural_bin_1940ws == 1 & plural_bin_2010 == 'Urban' ~ 'Became Urban',
    plural_bin_1940ws == 0 & plural_bin_2010 == 'Rural' ~ 'Became Rural',
    plural_bin_1940ws == 0 & plural_bin_2010 == 'Urban' ~ 'Remained Urban'),
    plural_cat = factor(plural_cat, 
                        levels = c('Remained Urban', 'Remained Rural', 'Became Urban', 'Became Rural'))) %>% 
  dplyr::select(st_cnty_yr_id, plural_cat)

# 1m Save out data for senke plots
# 1m.i Full dataset
senke_data_all <- data_persmarg3_all %>% 
  full_join(data_pluralcat, by = 'st_cnty_yr_id') %>% 
  write_fst(paste0(merged_data_path, 'segQuantilesForSenkesAllData.fst'))
# 1m.ii 2010 rurality stratified datset
senke_data_strat <- data_persmarg3_strat %>% 
  write_fst(paste0(merged_data_path, 'segQuantilesForSenkesStratifiedData.fst'))

####**************************************
#### 2: Prepare data for regressions  #### 
####**************************************

# 2a Select only vars needed for regressions
data_reg <- data %>% filter(year == '2010') %>% 
  dplyr::select(statefip, st_cnty_yr_id,                       # id vars
                fospp_count, ogw_count, nox_autos, meanPM2.5,  # env exps
                popDens)                                       # covars

# 2b Add lat/longs of county centroids to address spatial autocorrelation
# 2b.i Identify county centroids in 2010 (1940 already present)
centroids <- st_centroid(county2010)
county2010$x_centroid <- st_coordinates(centroids)[, 1]  # Longitude (X)
county2010$y_centroid <- st_coordinates(centroids)[, 2]  # Latitude (Y)
# ggplot(data = county2010) + geom_sf(aes(geometry = geometry), fill = 'lightgray', color = 'black') +
#   geom_point(aes(x = x_centroid, y = y_centroid), color = "blue") + 
#   theme_minimal()
# 2b.ii Save only county id and centroids
cent2010 <- as.data.frame(county2010) %>% dplyr::select(jfips10, x_centroid, y_centroid) %>% rename(st_cnty_yr_id = jfips10)
# 2b.iii Merge with other data for regressions
data_reg <- data_reg %>% 
  full_join(cent2010, by = 'st_cnty_yr_id')                                                   

# 2c Make sure state fip is a factor
data_reg <- data_reg %>% mutate(statefip = factor(statefip))
class(data_reg$statefip)

# 2d Log10 transform vehicle nox emissions
data_reg <- data_reg %>%
  mutate(nox_autos = ifelse(nox_autos == 0, 0.001, nox_autos),
         nox_autos = as.numeric(log10(nox_autos)))

# 2e Rescale units for env exp variables by IQR
IQR(data_reg$fospp_count, na.rm = T) # 1
IQR(data_reg$ogw_count, na.rm = T) # 206
IQR(data_reg$nox_autos, na.rm = T) # 0.79
IQR(data_reg$meanPM2.5, na.rm = T) # rounded to 3
data_reg <- data_reg %>% 
  mutate(nox_autos_iqrscale = nox_autos/0.79,
         fospp_count_iqrscale = fospp_count/1,
         ogw_count_iqrscale = ogw_count/206,
         pm2.5_iqrscale = meanPM2.5/3)

# 2f Add consistently highly segregated variable and plural categorical variable
# 2f.i Full dataset
data_reg_all <- data_reg %>% 
  full_join(data_pluralcat, by = 'st_cnty_yr_id') %>% 
  full_join(data_persmarg4_all, by = 'st_cnty_yr_id')
# 2f.ii 2010 rurality stratified dataset
data_reg_strat <- data_reg %>% 
  full_join(data_persmarg4_strat, by = 'st_cnty_yr_id')

# 2g Pivot longer and nest
#    Note: We want each dataframe to contain only one env exp var and one seg var
#          For the rurality stratified models, each dataframe should contain one
#            env exp var, one seg var, and an urban or rural designation
# 2g.i Full dataset
data_reg_all <- data_reg_all %>% 
  dplyr::select(-fospp_count, -ogw_count, -nox_autos, -meanPM2.5) %>% 
  pivot_longer(cols = c('fospp_count_iqrscale', 'ogw_count_iqrscale', 'nox_autos_iqrscale', 'pm2.5_iqrscale'), 
               names_to = 'envExp', values_to = 'expEst') %>% 
  pivot_longer(cols = c('di_persmarg', 'ice_persmarg'),
               names_to = 'segMetric', values_to = 'segValue') %>% 
  na.omit() %>% 
  group_by(envExp, segMetric) %>% 
  nest()
# 2g.ii 2010 rurality stratified dataset
data_reg_strat <- data_reg_strat %>% 
  dplyr::select(-fospp_count, -ogw_count, -nox_autos, -meanPM2.5) %>% 
  pivot_longer(cols = c('fospp_count_iqrscale', 'ogw_count_iqrscale', 'nox_autos_iqrscale', 'pm2.5_iqrscale'), 
               names_to = 'envExp', values_to = 'expEst') %>% 
  pivot_longer(cols = c('di_persmarg', 'ice_persmarg'),
               names_to = 'segMetric', values_to = 'segValue') %>% 
  na.omit() %>% 
  group_by(envExp, segMetric, plural_bin_imp) %>% 
  nest()

# 2h Add vars containing clean labels for plotting
# 2h.i Full dataset
data_reg_all <- data_reg_all %>% 
  mutate(
    envExpLab = case_when(
      envExp == 'fospp_count_iqrscale' ~ 'Fossil Fuel Plants (per 1 facility increase)',
      envExp == 'ogw_count_iqrscale' ~ 'Oil/Gas Wells (per 206 well increase)',
      envExp == 'nox_autos_iqrscale' ~ 'Log 10 Vehicle NOx (per 0.79 tons/yr increase)',
      envExp == 'pm2.5_iqrscale' ~ 'PM2.5 (per 3 ug/m^3 increase)'),
    segLab = case_when(
      segMetric == 'di_persmarg' ~ 'Dissimilarity Index',
      segMetric == 'ice_persmarg' ~ 'Black/White + Ed ICE'))
# 2h.ii 2010 rurality stratified dataset
data_reg_strat <- data_reg_strat %>% 
  mutate(
    envExpLab = case_when(
      envExp == 'fospp_count_iqrscale' ~ 'Fossil Fuel Plants (per 1 facility increase)',
      envExp == 'ogw_count_iqrscale' ~ 'Oil/Gas Wells (per 206 well increase)',
      envExp == 'nox_autos_iqrscale' ~ 'Log 10 Vehicle NOx (per 0.79 tons/yr increase)',
      envExp == 'pm2.5_iqrscale' ~ 'PM2.5 (per 3 ug/m^3 increase)'),
    segLab = case_when(
      segMetric == 'di_persmarg' ~ 'Dissimilarity Index',
      segMetric == 'ice_persmarg' ~ 'Black/White + Ed ICE'))

# 2i Assign model names
# 2i.i Full dataset
data_reg_all <- data_reg_all %>% 
  mutate(modelName = paste0(envExp, '_', segMetric))
# 2i.ii 2010 rurality stratified dataset
data_reg_strat <- data_reg_strat %>% 
  mutate(modelName = paste0(envExp, '_', segMetric, '_', plural_bin_imp))

# 2j Assign model distribution
# 2j.i Determine if Poisson or Negative Binomial should be used for counts
#      For Poisson, mean and variance should be equal, Neg Bi can have variance
#      substantially higher than mean
#      Fossil fuel powerplants
mean(data_reg_all$data[[1]]$expEst); var(data_reg_all$data[[1]]$expEst) # use Neg Bi
mean(data_reg_all$data[[2]]$expEst); var(data_reg_all$data[[2]]$expEst) # use Neg Bi
mean(data_reg_strat$data[[1]]$expEst); var(data_reg_strat$data[[1]]$expEst) # use Neg Bi
mean(data_reg_strat$data[[2]]$expEst); var(data_reg_strat$data[[2]]$expEst) # use Neg Bi
mean(data_reg_strat$data[[9]]$expEst); var(data_reg_strat$data[[9]]$expEst) # use Neg Bi
mean(data_reg_strat$data[[10]]$expEst); var(data_reg_strat$data[[10]]$expEst) # use Neg Bi
#      Oil/gas wells
mean(data_reg_all$data[[3]]$expEst); var(data_reg_all$data[[3]]$expEst) # use Neg Bi
mean(data_reg_all$data[[4]]$expEst); var(data_reg_all$data[[4]]$expEst) # use Neg Bi
mean(data_reg_strat$data[[3]]$expEst); var(data_reg_strat$data[[3]]$expEst) # use Neg Bi
mean(data_reg_strat$data[[4]]$expEst); var(data_reg_strat$data[[4]]$expEst) # use Neg Bi
mean(data_reg_strat$data[[11]]$expEst); var(data_reg_strat$data[[11]]$expEst) # use Neg Bi
mean(data_reg_strat$data[[12]]$expEst); var(data_reg_strat$data[[12]]$expEst) # use Neg Bi
# 2j.ii Assign each env exp - seg pair a model distribution
data_reg_all <- data_reg_all %>% 
  mutate(modelType = ifelse(str_detect(modelName, 'count'), 'negbin', 'gaussian')) 
data_reg_strat <- data_reg_strat %>% 
  mutate(modelType = ifelse(str_detect(modelName, 'count'), 'negbin', 'gaussian')) 

# 2k Save datasets
data_reg_all %>% 
  write_rds(paste0(merged_data_path, 'ConsistentSegData.RDS'))
data_reg_strat %>% 
  write_rds(paste0(merged_data_path, 'ConsistentSegDataStratified.RDS'))

####************************************************************
#### 3: Review consistently highly segregated distribution  #### 
####************************************************************

# 3a Review number of counties that are classified as consistently highly segregated 
# 3a.i DI
table(data_reg_all$data[[5]]$segValue, useNA = 'always')
table(data_reg_strat$data[[5]]$segValue, useNA = 'always')  # rural
table(data_reg_strat$data[[13]]$segValue, useNA = 'always') # urban
# 3a.ii ICE
table(data_reg_all$data[[6]]$segValue, useNA = 'always')
table(data_reg_strat$data[[6]]$segValue, useNA = 'always')  # rural
table(data_reg_strat$data[[14]]$segValue, useNA = 'always') # urban

# 3b Source functions
source(paste0(project.folder, 'functions/descriptive_maps_plots.R'))

# 3c Create boxplots of consistent segregation for each segMetric-envExp pair
# 3c.i Full dataset
data_reg_all$boxplot <- list(NA)
for(i in 1:length(data_reg_all$data)){
  data_reg_all$boxplot[[i]] = boxplot_persmarg(dataset   = data_reg_all$data[[i]],
                                               seg_label = data_reg_all$segLab[[i]],
                                               exp_label = data_reg_all$envExpLab[[i]])
}
data_reg_all$boxplot[[8]]
# 3c.ii 2010 rurality stratified dataset
data_reg_strat$boxplot <- list(NA)
for(i in 1:length(data_reg_strat$data)){
  data_reg_strat$boxplot[[i]] = boxplot_persmarg_strat(dataset   = data_reg_strat$data[[i]],
                                                       seg_label = data_reg_strat$segLab[[i]],
                                                       exp_label = data_reg_strat$envExpLab[[i]],
                                                       strata    = data_reg_strat$plural_bin_imp[[i]])
}
data_reg_strat$boxplot[[8]]

# 3d Create map of consistently highly segregated counties
# 3d.i Consistently highly segregated: DI
persmargmap_di <- data_reg_all$data[[5]] %>% 
  full_join(county2010_tc, by = c('st_cnty_yr_id' = 'GEOID')) %>% 
  ggplot(aes(geometry = geometry, fill = segValue), alpha = 0.8) + 
  geom_sf(color = 'gray80', linewidth = 0.1) + 
  scale_fill_viridis_d() + 
  labs(subtitle = 'A: Dissimilarity Index') +
  geom_sf(fill = "transparent", color = "white", linewidth = 0.3, 
          data = state1940) +
  guides(fill = guide_legend(title = 'Consistently Highly Segregated')) +
  theme_void() +
  theme(panel.grid.major = element_blank())
ggsave(persmargmap_di,
       filename = paste0(prelim_map_path, 'persmarg_di_map.pdf'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)
# 3d.ii Consistently highly segregated: ICE
persmargmap_ice <- data_reg_all$data[[6]] %>% 
  full_join(county2010_tc, by = c('st_cnty_yr_id' = 'GEOID')) %>% 
  ggplot(aes(geometry = geometry, fill = segValue), alpha = 0.8) + 
  geom_sf(color = 'gray80', linewidth = 0.1) + 
  scale_fill_viridis_d() + 
  labs(subtitle = 'B: Index of Concentration at the Extremes') +
  geom_sf(fill = "transparent", color = "white", linewidth = 0.3, 
          data = state1940) +
  guides(fill = guide_legend(title = 'Consistently Highly Segregated')) +
  theme_void() +
  theme(panel.grid.major = element_blank())
ggsave(persmargmap_ice,
       filename = paste0(prelim_map_path, 'persmarg_ice_map.pdf'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)

# 3e Create initial senke plots
#    Note: Warning about strata appearing at multiple axes is because the
#          strata in axis1 and axis2 are called the same (e.g., Q1)
# 3e.i DI
senke_di <- senke_data_all %>% 
  filter(!is.na(di)) %>% filter(!is.na(di_1940_ws)) %>% 
  mutate(di_1940_ws = factor(paste0('Q', di_1940_ws)),
         di = factor(paste0('Q', di))) %>% 
  ggplot(aes(axis1 = di_1940_ws, axis2 = di)) +
  scale_x_discrete(limits = c('DI Quantile in 1940', 'DI Quantile in 2010'), expand = c(.2, .05)) +
  xlab('') + ylab('Number of Counties') +
  geom_alluvium(aes(fill = di_1940_ws)) +
  geom_stratum() +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum))) + 
  scale_fill_manual(values = c(viridis(4)),
                    labels = c('Q1: Low Segregation', 'Q2', 'Q3', 'Q4: High Segregation')) + 
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(title = 'DI Quantile in 1940')) +
  theme(legend.position = 'right')
ggsave(senke_di,
       filename = paste0(senke_plot_path, 'di_senke.jpg'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)
# 3e.i ICE
senke_ice <- senke_data_all %>% 
  filter(!is.na(raceEduICE_hsd10)) %>% filter(!is.na(raceEduICE_hsd10_1940_ws)) %>% 
  mutate(raceEduICE_hsd10_1940_ws = factor(paste0('Q', raceEduICE_hsd10_1940_ws)),
         raceEduICE_hsd10 = factor(paste0('Q', raceEduICE_hsd10))) %>% 
  ggplot(aes(axis1 = raceEduICE_hsd10_1940_ws, axis2 = raceEduICE_hsd10)) +
  scale_x_discrete(limits = c('ICE Quantile in 1940', 'ICE Quantile in 2010'), expand = c(.2, .05)) +
  xlab('') + ylab('Number of Counties') +
  geom_alluvium(aes(fill = raceEduICE_hsd10_1940_ws)) +
  geom_stratum() +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum))) + 
  scale_fill_manual(values = c(viridis(4)),
                    labels = c('Q1: Low Segregation', 'Q2', 'Q3', 'Q4: High Segregation')) + 
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(title = 'ICE Quantile in 1940')) +
  theme(legend.position = 'right')
ggsave(senke_ice,
       filename = paste0(senke_plot_path, 'ice_senke.jpg'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)

# 3f Stratify senke plots by urban/rural using 4-level categorical variable
# 3f.i Review number of counties in each urban/rural category
table(senke_data_all$plural_cat, useNA = 'always')
# 3f.ii Remained urban
senke_ice_ru <- senke_data_all %>% 
  filter(plural_cat == 'Remained Urban') %>% 
  filter(!is.na(raceEduICE_hsd10)) %>% filter(!is.na(raceEduICE_hsd10_1940_ws)) %>% 
  mutate(raceEduICE_hsd10_1940_ws = factor(paste0('Q', raceEduICE_hsd10_1940_ws)),
         raceEduICE_hsd10 = factor(paste0('Q', raceEduICE_hsd10))) %>% 
  ggplot(aes(axis1 = raceEduICE_hsd10_1940_ws, axis2 = raceEduICE_hsd10)) +
  scale_x_discrete(limits = c('ICE Quintile in 1940', 'ICE Quintile in 2010'), expand = c(.2, .05)) +
  xlab('') + ylab('Number of Counties') +
  geom_alluvium(aes(fill = raceEduICE_hsd10_1940_ws)) +
  geom_stratum() +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum))) + 
  scale_fill_manual(values = c(viridis(4)),
                    labels = c('Q1: Low Segregation', 'Q2', 'Q3', 'Q4: High Segregation')) + 
  labs(subtitle = 'A: Remained Urban') +
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(title = 'ICE Quintile in 1940')) +
  theme(legend.position = 'right')
ggsave(senke_ice_ru,
       filename = paste0(senke_plot_path, 'ice_senke_remained_urban.jpg'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)
# 3f.iii Remained Rural
senke_ice_rr <- senke_data_all %>% 
  filter(plural_cat == 'Remained Rural') %>% 
  filter(!is.na(raceEduICE_hsd10)) %>% filter(!is.na(raceEduICE_hsd10_1940_ws)) %>% 
  mutate(raceEduICE_hsd10_1940_ws = factor(paste0('Q', raceEduICE_hsd10_1940_ws)),
         raceEduICE_hsd10 = factor(paste0('Q', raceEduICE_hsd10))) %>% 
  ggplot(aes(axis1 = raceEduICE_hsd10_1940_ws, axis2 = raceEduICE_hsd10)) +
  scale_x_discrete(limits = c('ICE Quintile in 1940', 'ICE Quintile in 2010'), expand = c(.2, .05)) +
  xlab('') + ylab('Number of Counties') +
  geom_alluvium(aes(fill = raceEduICE_hsd10_1940_ws)) +
  geom_stratum() +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum))) + 
  scale_fill_manual(values = c(viridis(4)),
                    labels = c('Q1: Low Segregation', 'Q2', 'Q3', 'Q4: High Segregation')) + 
  labs(subtitle = 'B: Remained Rural') +
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(title = 'ICE Quintile in 1940')) +
  theme(legend.position = 'right')
ggsave(senke_ice_rr,
       filename = paste0(senke_plot_path, 'ice_senke_remained_rural.jpg'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)
# 3f.iv Became Urban
senke_ice_bu <- senke_data_all %>% 
  filter(plural_cat == 'Became Urban') %>% 
  filter(!is.na(raceEduICE_hsd10)) %>% filter(!is.na(raceEduICE_hsd10_1940_ws)) %>% 
  mutate(raceEduICE_hsd10_1940_ws = factor(paste0('Q', raceEduICE_hsd10_1940_ws)),
         raceEduICE_hsd10 = factor(paste0('Q', raceEduICE_hsd10))) %>% 
  ggplot(aes(axis1 = raceEduICE_hsd10_1940_ws, axis2 = raceEduICE_hsd10)) +
  scale_x_discrete(limits = c('ICE Quintile in 1940', 'ICE Quintile in 2010'), expand = c(.2, .05)) +
  xlab('') + ylab('Number of Counties') +
  geom_alluvium(aes(fill = raceEduICE_hsd10_1940_ws)) +
  geom_stratum() +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum))) + 
  scale_fill_manual(values = c(viridis(4)),
                    labels = c('Q1: Low Segregation', 'Q2', 'Q3', 'Q4: High Segregation')) + 
  labs(subtitle = 'C: Became Urban') +
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(title = 'ICE Quintile in 1940')) +
  theme(legend.position = 'right')
ggsave(senke_ice_bu,
       filename = paste0(senke_plot_path, 'ice_senke_became_urban.jpg'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)
# 3f.v Became Rural
senke_ice_br <- senke_data_all %>% 
  filter(plural_cat == 'Became Rural') %>% 
  filter(!is.na(raceEduICE_hsd10)) %>% filter(!is.na(raceEduICE_hsd10_1940_ws)) %>% 
  mutate(raceEduICE_hsd10_1940_ws = factor(paste0('Q', raceEduICE_hsd10_1940_ws)),
         raceEduICE_hsd10 = factor(paste0('Q', raceEduICE_hsd10))) %>% 
  ggplot(aes(axis1 = raceEduICE_hsd10_1940_ws, axis2 = raceEduICE_hsd10)) +
  scale_x_discrete(limits = c('ICE Quintile in 1940', 'ICE Quintile in 2010'), expand = c(.2, .05)) +
  xlab('') + ylab('Number of Counties') +
  geom_alluvium(aes(fill = raceEduICE_hsd10_1940_ws)) +
  geom_stratum() +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum))) + 
  scale_fill_manual(values = c(viridis(4)),
                    labels = c('Q1: Low Segregation', 'Q2', 'Q3', 'Q4: High Segregation')) + 
  labs(subtitle = 'D: Became Rural') +
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(title = 'ICE Quintile in 1940')) +
  theme(legend.position = 'right')
ggsave(senke_ice_br,
       filename = paste0(senke_plot_path, 'ice_senke_became_rural.jpg'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)

# 3g Stratify senke plots by urban/rural using 2010 designation
# 3g.i DI
senke_di_strat <- senke_data_strat %>% 
  filter(!is.na(di)) %>% filter(!is.na(di_1940_ws)) %>% 
  mutate(di_1940_ws = factor(paste0('Q', di_1940_ws)),
         di = factor(paste0('Q', di))) %>% 
  ggplot(aes(axis1 = di_1940_ws, axis2 = di)) +
  scale_x_discrete(limits = c('DI Quantile in 1940', 'DI Quantile in 2010'), expand = c(.2, .05)) +
  xlab('') + ylab('Number of Counties') +
  geom_alluvium(aes(fill = di_1940_ws)) +
  geom_stratum() +
  facet_wrap(~plural_bin_imp) +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum))) + 
  scale_fill_manual(values = c(viridis(4)),
                    labels = c('Q1: Low Segregation', 'Q2', 'Q3', 'Q4: High Segregation')) + 
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(title = 'DI Quantile in 1940')) +
  theme(legend.position = 'right')
ggsave(senke_di_strat,
       filename = paste0(senke_plot_path, 'di_strat_senke.jpg'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)
# 3g.ii ICE 
senke_ice_strat <- senke_data_strat %>% 
  filter(!is.na(raceEduICE_hsd10)) %>% filter(!is.na(raceEduICE_hsd10_1940_ws)) %>% 
  mutate(raceEduICE_hsd10_1940_ws = factor(paste0('Q', raceEduICE_hsd10_1940_ws)),
         raceEduICE_hsd10 = factor(paste0('Q', raceEduICE_hsd10))) %>% 
  ggplot(aes(axis1 = raceEduICE_hsd10_1940_ws, axis2 = raceEduICE_hsd10)) +
  scale_x_discrete(limits = c('ICE Quantile in 1940', 'ICE Quantile in 2010'), expand = c(.2, .05)) +
  xlab('') + ylab('Number of Counties') +
  geom_alluvium(aes(fill = raceEduICE_hsd10_1940_ws)) +
  geom_stratum() +
  facet_wrap(~plural_bin_imp) +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum))) + 
  scale_fill_manual(values = c(viridis(4)),
                    labels = c('Q1: Low Segregation', 'Q2', 'Q3', 'Q4: High Segregation')) + 
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(title = 'ICE Quantile in 1940')) +
  theme(legend.position = 'right')
ggsave(senke_ice_strat,
       filename = paste0(senke_plot_path, 'ice_strat_senke.jpg'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)

####*************************
#### 4: Run regressions  #### 
####*************************

# 4a Run regressions for full dataset
regs <- data_reg_all %>% 
  mutate(model = list(NA)) %>% 
  arrange(modelType)
for(i in 1:4){
  regs$model[[i]] = mgcv::gam(expEst ~ segValue + popDens + plural_cat + s(statefip, bs = 're'), 
                              data = regs$data[[i]], family = gaussian(), method = 'REML')
  regs$tidy_mod[[i]] = tidy(regs$model[[i]], parametric = TRUE, conf.int = TRUE)}
for(i in 5:8){
  regs$model[[i]] = mgcv::gam(expEst ~ segValue + popDens + plural_cat + s(statefip, bs = 're'), 
                             data = regs$data[[i]], family = nb(), method = 'REML')
  regs$tidy_mod[[i]] = tidy(regs$model[[i]], parametric = TRUE, conf.int = TRUE, 
                            exponentiate = TRUE)}

# 4b Run regression for 2010 rurality stratified dataset
regs_strat <- data_reg_strat %>% 
  mutate(model = list(NA)) %>% 
  arrange(modelType)
for(i in 1:8){
  regs_strat$model[[i]] = mgcv::gam(expEst ~ segValue + popDens 
                                    + s(statefip, bs = 're') + s(y_centroid, x_centroid, bs = "tp"), 
                              data = regs_strat$data[[i]], family = gaussian(), method = 'REML')
  regs_strat$tidy_mod[[i]] = tidy(regs_strat$model[[i]], parametric = TRUE, conf.int = TRUE)}
for(i in 9:16){
  regs_strat$model[[i]] = mgcv::gam(expEst ~ segValue + popDens 
                                    + s(statefip, bs = 're') + s(y_centroid, x_centroid, bs = "tp"), 
                              data = regs_strat$data[[i]], family = nb(), method = 'REML')
  regs_strat$tidy_mod[[i]] = tidy(regs_strat$model[[i]], parametric = TRUE, conf.int = TRUE, 
                            exponentiate = TRUE)}

# 4c Save regressions
regs %>% write_rds(file = paste0(model_path, 'regs_persmarg.RDS'))
regs_strat %>% write_rds(file = paste0(model_path, 'regs_strat_persmarg.RDS'))

####**********************************
#### 5: Review model diagnostics  #### 
####**********************************

# 5a Load all regressions (needed if running this chunk at different time than models were run)
regcheck <- read_rds(paste0(model_path, 'regs_strat_persmarg.RDS'))

# 5b Review QQ plots and residuals
# 5b.i Nox (log transformed)
#      Notes: Not perfect, but far better than non-log-transformed models.
resids_nox_di_rural <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[1]], plot = T)
resids_nox_ice_rural <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[2]], plot = T)
resids_nox_di_urban <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[5]], plot = T)
resids_nox_ice_urban <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[6]], plot = T)
# 5b.ii Oil/gas wells 
#       Notes: Not great, but not too awful. Has same curved line as regressions by year,
#              likely caused by some residual spatial autocorrelation
resids_ogw_di_rural <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[11]], plot = T)
resids_ogw_ice_rural <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[12]], plot = T)
resids_ogw_di_urban <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[15]], plot = T)
resids_ogw_ice_urban <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[16]], plot = T)
# 5b.iii Fossil fuel power plants 
#        Notes: Looks good
resids_fospp_di_rural <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[9]], plot = T)
resids_fospp_ice_rural <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[10]], plot = T)
resids_fospp_di_urban <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[13]], plot = T)
resids_fospp_ice_urban <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[14]], plot = T)
# 5b.iv PM2.5 
#       Notes: Looks good
resids_pm_di_rural <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[3]], plot = T)
resids_pm_ice_rural <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[4]], plot = T)
resids_pm_di_urban <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[7]], plot = T)
resids_pm_ice_urban <- DHARMa::simulateResiduals(fittedModel = regcheck$model[[8]], plot = T)








