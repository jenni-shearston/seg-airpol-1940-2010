# Regressions between Segregation Metrics & Environmental Exposures by Year
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 10/01/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Prepare data for regressions
# 2: Grid search to determine best model parameters
# 3: Run regressions 
# 4: Review model diagnostics

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we run the models for the analysis and conduct model diagnostics.
# Sensitivity analysis regressions are also performed on this script. However,
# the consistently highly segregated analysis is performed on script 2_03.

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
spatial_data_path <- paste0(project.folder, 'data/county_shapefiles/')

# 0d Load all env exp, seg, and sociodemo data
data <- read_fst(paste0(merged_data_path, 'env_seg_sociodemo_allyears.fst'))

# 0e Calculate 1940 county-level Black population that is equivalent to same 
#    percentile of counties in 2010 that have a Black population of 1000+
#      Answer: 415 (46.8% of counties)
data_2010 <- data %>% filter(year == '2010') %>% 
  mutate(blackPopGrt1000 = ifelse(blackPop >= 1000, 1, 0))
table(data_2010$blackPopGrt1000, useNA = 'always') # 1=1454, 0=1655
(1454/3109)*100 # 46.77%
100-46.77 #53.23
data_1940 <- data %>% filter(year == '1940')
quantile(data_1940$blackPop, probs = c(0.5323), na.rm = T) # 415
data_1940 <- data_1940 %>% 
  mutate(blackPopGrt420 = ifelse(blackPop >= 415, 1, 0))
table(data_1940$blackPopGrt420, useNA = 'always') # 1=1450, 0=1649
(1450/3100)*100 # 46.77%
rm(data_2010, data_1940)

# 0f Load county shapefiles for moran's i model check
county1940 <- st_read(paste0(spatial_data_path, 
                             'FINAL_1940_county_shapefile_clean/counties_contiguous_1940.shp')) %>% 
  janitor::clean_names()
county2010 <- st_read(paste0(spatial_data_path, 
                             'FINAL_2010_county_shapefile_clean/counties_contiguous_2010.shp')) %>% 
  janitor::clean_names()

####**************************************
#### 1: Prepare data for regressions  #### 
####**************************************

# 1a Select only vars needed for regressions
#    Note: decided not to include IDW variables becuase they are difficult to 
#          interpret, but kept the variables in the df and ran models in case
#          we want to use them in the future or in response to reviewers
data_reg <- data %>% 
  dplyr::select(year, statefip, st_cnty_yr_id,                           # id vars
                fospp_count, ogw_count, nox_autos, meanPM2.5, pm2.5_rm,  # env exps
                nbrOne, di, raceEduICE_hsd10,                            # seg metrics
                popDens, plural_bin_imp,                                 # covars, strat
                blackPop, totPop, ogw_in_1940, pp_in_1940,               # restriction vars 
                fospp_idw, ogw_idw, ogwBuffer_count, fosppBuffer_count)  # sens analyses            

# 1b Add lat/longs of county centroids to address spatial autocorrelation
# 1b.i Identify county centroids in 2010 (1940 already present)
centroids <- st_centroid(county2010)
county2010$x_centroid <- st_coordinates(centroids)[, 1]  # Longitude (X)
county2010$y_centroid <- st_coordinates(centroids)[, 2]  # Latitude (Y)
# 1b.ii Plot to double check centroids look right 
#       Note: Plot may take a few seconds to run and more time to load
ggplot(data = county2010) + geom_sf(aes(geometry = geometry), fill = 'lightgray', color = 'black') +
  geom_point(aes(x = x_centroid, y = y_centroid), color = "blue") +
  theme_minimal()
# 1b.iii Save only county id and centroids
cent1940 <- as.data.frame(county1940) %>% dplyr::select(jnhgis40, x_centroid, y_centroid) %>% rename(st_cnty_yr_id = jnhgis40)
cent2010 <- as.data.frame(county2010) %>% dplyr::select(jfips10, x_centroid, y_centroid) %>% rename(st_cnty_yr_id = jfips10)
cents4010 <- cent1940 %>% bind_rows(cent2010)
# 1b.iv Merge with other data for regressions
data_reg <- data_reg %>% 
  full_join(cents4010, by = 'st_cnty_yr_id')

# 1c Make sure state fip is a factor
data_reg <- data_reg %>% mutate(statefip = factor(statefip))
class(data_reg$statefip)

# 1d Change units as needed 
#    For nox_autos effects per tons/yr is too small a unit change to
#      easily display in graphs. Convert to 10 tons / yr
#    For oil/gas wells, there are a lot of wells, so do per 100 wells
data_reg <- data_reg %>% mutate(nox_autos_10tpy = nox_autos/10,
                                ogw_count = ogw_count/100,
                                fospp_count = fospp_count/10,
                                ogwBuffer_count = ogwBuffer_count/100,
                                fosppBuffer_count = fosppBuffer_count/10)

# 1e Pivot longer and nest
#    Note: We want each dataframe to contain only one env exp var and one sociodemo var
data_reg <- data_reg %>% 
  pivot_longer(cols = c('fospp_count', 'ogw_count', 'nox_autos', 'nox_autos_10tpy', 'meanPM2.5', 'pm2.5_rm',
                        'fospp_idw', 'ogw_idw', 'ogwBuffer_count', 'fosppBuffer_count'), 
               names_to = 'envExp', values_to = 'expEst') %>% 
  pivot_longer(cols = c('nbrOne', 'di', 'raceEduICE_hsd10'),
               names_to = 'segMetric', values_to = 'segValue') %>% 
  na.omit() %>% 
  group_by(year, envExp, segMetric, plural_bin_imp) %>% 
  nest()

# 1f Add vars containing clean labels for plotting
data_reg <- data_reg %>% 
  mutate(
    envExpLab = case_when(
      envExp == 'fospp_count' ~ 'Fossil Fuel Plants (per 10 plants)',
      envExp == 'ogw_count' ~ 'Oil/Gas Wells (per 100 wells)',
      envExp == 'nox_autos' ~ 'Vehicle NOx (tons/yr)',
      envExp == 'nox_autos_10tpy' ~ 'Vehicle NOx (10 tons/yr)',
      envExp == 'meanPM2.5' ~ 'PM2.5 (ug/m^3)',
      envExp == 'pm2.5_rm' ~ 'RM PM2.5 (ug/m^3)',
      envExp == 'fospp_idw' ~ 'IDW Fossil Fuel Plant',
      envExp == 'ogw_idw' ~ 'IDW Oil/Gas Well',
      envExp == 'ogwBuffer_count' ~ 'Buffered Oil/Gas Wells (per 100 wells)',
      envExp == 'fosppBuffer_count' ~ 'Buffered Fossil Fuel Plants (per 10 plants)'),
    segLab = case_when(
      segMetric == 'nbrOne' ~ 'Neighbor Metric',
      segMetric == 'di' ~ 'Dissimilarity Index',
      segMetric == 'raceEduICE_hsd10' ~ 'Black/White + Ed ICE'))

# 1g Restrict dataframes for DI analyses to ensure sufficient Black population and
#    create DI sensitivity analysis dataframe
#    Note: The main DI analysis should include only counties with min Black population = 100
#          Sensitivity DI analyses should include only counties with min Black 
#            pop = 1000 for 2010 and a comparable number for 1940. E.g., for 1940,
#            identify what percentile of counties 1000 black population is for 2010
#            and use that same percentile in 1940 to identify the number of black 
#            population in a county to restrict to (math done in section 0e)
# 1g.i Create DI black pop sensitivity analysis dataframe
#      Note: Remove rows for other sensitivity analyses
data_reg_diSens <- data_reg %>% filter(segMetric == 'di') %>% 
  filter(!str_detect(envExp, '_idw')) %>% filter(!str_detect(envExp, 'Buffer')) %>% 
  filter(!(year == '2010' & envExp == 'pm2.5_rm'))
# 1g.ii Restrict DI sens analysis to counties with min Black pop of 1000 (2010)
#       and 416 (1940)
for(i in 1:10){data_reg_diSens$data[[i]] = subset(data_reg_diSens$data[[i]], blackPop > 999)}
for(i in 11:20){data_reg_diSens$data[[i]] = subset(data_reg_diSens$data[[i]], blackPop > 415)}
# 1g.iii Restrict main DI analysis to counties with min Black pop of 100
di = data_reg %>% ungroup() %>% dplyr::select(segMetric) %>% 
  mutate(di_rn = row_number(), is_di = str_detect(segMetric, 'di')) %>%
  filter(is_di == TRUE) %>% dplyr::select(di_rn)
for(i in di$di_rn){data_reg$data[[i]] = subset(data_reg$data[[i]], blackPop > 99)}
# 1g.iv Clean environment
rm(di)

# 1h Create dataframes for power plant (pp) and oil/gas well (ogw) sensitivity 
#    analyses where the regression is restricted to counties that had at least 
#    one oil/gas well or power plant in 1940 
# 1h.i Oil/gas wells
#      Note: Rows for Buffer sensitivity kept in case needed at reviewer request, 
#            rows for IDW analysis dropped because they are very unlikely to be used
data_reg_ogwSens <- data_reg %>% filter(str_detect(envExp, 'ogw'))
for(i in 1:30){data_reg_ogwSens$data[[i]] = subset(data_reg_ogwSens$data[[i]], ogw_in_1940 == 'og well in 1940')}
# 1h.ii Power plants
#       Note: Rows for Buffer sensitivity kept in case needed at reviewer request, 
#             rows for IDW analysis dropped because they are very unlikely to be used
data_reg_ppSens <- data_reg %>% filter(str_detect(envExp, 'fospp'))
for(i in 1:30){data_reg_ppSens$data[[i]] = subset(data_reg_ppSens$data[[i]], pp_in_1940 == 'fossil pp in 1940')}

# 1i Create dataframe for sensitivity analysis that excludes outlying seg and 
#    outcome values (outlier sensitivity analysis)
data_reg_outlierSens <- data_reg
for(i in 1:length(data_reg_outlierSens$data)){
  data_reg_outlierSens$data[[i]]$main_n = dim(data_reg_outlierSens$data[[i]])[1]
  data_reg_outlierSens$data[[i]]$segValue_mean = mean(data_reg_outlierSens$data[[i]]$segValue, na.rm = T)
  data_reg_outlierSens$data[[i]]$segValue_sd5 = sd(data_reg_outlierSens$data[[i]]$segValue, na.rm = T)*5
  data_reg_outlierSens$data[[i]]$expEst_mean = mean(data_reg_outlierSens$data[[i]]$expEst, na.rm = T)
  data_reg_outlierSens$data[[i]]$expEst_sd5 = sd(data_reg_outlierSens$data[[i]]$expEst, na.rm = T)*5
  data_reg_outlierSens$data[[i]] = subset(data_reg_outlierSens$data[[i]], expEst < expEst_sd5)
  data_reg_outlierSens$data[[i]] = subset(data_reg_outlierSens$data[[i]], segValue < segValue_sd5)
  data_reg_outlierSens$data[[i]]$perc_outlier = 
    (dim(data_reg_outlierSens$data[[i]])[1]/data_reg_outlierSens$data[[i]]$main_n)*100}

# 1j Log10 transform NOx vehicle emissions
#    Note: Model diagnostics from non-transformed NOx vehicle emissions indicate violation
#    of the assumption of normally distributed residuals. Here we create a log10
#    transformed variable to see if this works better for model fit.
#     - We will run these models but may not use them in the main analysis if the
#       results are very similar to non-transformed models. 
#     - If we do use log10 transformation as the main models we will also need to 
#       run sections 1j.ii and 1j.iii for the other sensitivity analyses and then
#       run those regresssions and plots later in this script.
#    All code is kept in this section because reviewers may request this transformation.
# 1j.i Create log10 transformed test dataset
#      Note: Add a very small value to two rows with a nox_autos value of 0
data_reg_log10nox <- data_reg %>% filter(envExp == 'nox_autos')
for(i in 1:length(data_reg_log10nox$data)){
  data_reg_log10nox$data[[i]]$nox_autos_mean = mean(data_reg_log10nox$data[[i]]$expEst, na.rm = T)
  data_reg_log10nox$data[[i]]$nox_autos_sd = sd(data_reg_log10nox$data[[i]]$expEst, na.rm = T)
  data_reg_log10nox$data[[i]] <- data_reg_log10nox$data[[i]] %>%
    mutate(expEst = ifelse(expEst == 0, 0.001, expEst)) %>% 
    mutate(expEst = as.numeric(log10(expEst)))
}
# 1j.ii Log10 transform nox_autos exposure in DI sensitivity analysis dataset
for(i in 1:length(data_reg_diSens$data)){
  if(str_detect(data_reg_diSens$envExp[[i]], 'nox_autos')){
    data_reg_diSens$data[[i]]$nox_autos_mean = mean(data_reg_diSens$data[[i]]$expEst, na.rm = T)
    data_reg_diSens$data[[i]]$nox_autos_sd = sd(data_reg_diSens$data[[i]]$expEst, na.rm = T)
    data_reg_diSens$data[[i]] <- data_reg_diSens$data[[i]] %>%
      mutate(expEst = ifelse(expEst == 0, 0.001, expEst)) %>% 
      mutate(expEst = as.numeric(log10(expEst)))
  }}
# 1j.iii Log10 transform nox_autos exposure in outlier sensitivity analysis dataset
for(i in 1:length(data_reg_outlierSens$data)){
  if(str_detect(data_reg_outlierSens$envExp[[i]], 'nox_autos')){
    data_reg_outlierSens$data[[i]]$nox_autos_mean = mean(data_reg_outlierSens$data[[i]]$expEst, na.rm = T)
    data_reg_outlierSens$data[[i]]$nox_autos_sd = sd(data_reg_outlierSens$data[[i]]$expEst, na.rm = T)
    data_reg_outlierSens$data[[i]] <- data_reg_outlierSens$data[[i]] %>%
      mutate(expEst = ifelse(expEst == 0, 0.001, expEst)) %>% 
      mutate(expEst = as.numeric(log10(expEst)))
  }}

# 1k Windsorize nox automobile variable to try in regressions 
#    Note: Skewed distribution is making model fit challenging, so we are trying
#          different transformations. There are a lot of counties with a really 
#          small value and a few with very large values
# 1k.i Review raw distribution for each year
data %>% filter(year == '1940') %>% ggplot() + geom_histogram(aes(x = nox_autos), binwidth = 15)
data %>% filter(year == '2010') %>% ggplot() + geom_histogram(aes(x = nox_autos), binwidth = 10)
# 1k.ii Create function to windsorize 
winsorize_var <- function(x, lower = 0.01, upper = 0.99) {
  q <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)}
# 1k.iii Create new dataframe with windsorized nox_autos values
data_reg_windsnox <- data_reg %>% filter(envExp == 'nox_autos')
for(i in 1:length(data_reg_windsnox$data)){
  data_reg_windsnox$data[[i]] <- data_reg_windsnox$data[[i]] %>%
    mutate(expEst = as.numeric(winsorize_var(expEst)))
}
# 1k.iv Re-check distribution to confirm quantile selection
#       Note: This appears to have helped a lot with reducing skewness.
data_reg_windsnox %>% unnest(cols = data) %>% filter(year == '1940') %>% ggplot() + geom_histogram(aes(x = expEst), binwidth = 15)
data_reg_windsnox %>% unnest(cols = data) %>% filter(year == '2010') %>% ggplot() + geom_histogram(aes(x = expEst), binwidth = 10)

# 1l Assign model names
data_reg <- data_reg %>% 
  mutate(modelName = paste0(envExp, '_', segMetric, '_', year, '_', plural_bin_imp))
data_reg_diSens <- data_reg_diSens %>% 
  mutate(modelName = paste0(envExp, '_', segMetric, '_', year, '_', plural_bin_imp))
data_reg_outlierSens <- data_reg_outlierSens %>% 
  mutate(modelName = paste0(envExp, '_', segMetric, '_', year, '_', plural_bin_imp))
data_reg_ogwSens <- data_reg_ogwSens %>% 
  mutate(modelName = paste0(envExp, '_', segMetric, '_', year, '_', plural_bin_imp))
data_reg_ppSens <- data_reg_ppSens %>% 
  mutate(modelName = paste0(envExp, '_', segMetric, '_', year, '_', plural_bin_imp))
data_reg_log10nox <- data_reg_log10nox %>% 
  mutate(modelName = paste0(envExp, '_', segMetric, '_', year, '_', plural_bin_imp)) 
data_reg_windsnox <- data_reg_windsnox %>% 
  mutate(modelName = paste0(envExp, '_', segMetric, '_', year, '_', plural_bin_imp)) 

# 1m Assign model distribution
#      We will also test a gamma distribution for the NOx exposure since it is 
#      so right skewed
# 1m.i Determine if Poisson or Negative Binomial should be used for count exposures
#      For Poisson, mean and variance should be equal, negative binomial (negbin) 
#      can have variance substantially higher than mean
mean(data$fospp_count); var(data$fospp_count) # use negative binomial
mean(data$ogw_count); var(data$ogw_count) # use negative binomial
mean(data$ogwBuffer_count); var(data$ogwBuffer_count) # use negative binomial
mean(data$fosppBuffer_count); var(data$fosppBuffer_count) # use negative binomial
# 1m.ii Assign each env exp - seg pair a model distribution of guassian or negative
#       binomial
data_reg <- data_reg %>% 
  mutate(modelType = ifelse(str_detect(modelName, 'count'), 'negbin', 'gaussian')) 
data_reg_diSens <- data_reg_diSens %>% 
  mutate(modelType = ifelse(str_detect(modelName, 'count'), 'negbin', 'gaussian')) 
data_reg_outlierSens <- data_reg_outlierSens %>% 
  mutate(modelType = ifelse(str_detect(modelName, 'count'), 'negbin', 'gaussian')) 
data_reg_ogwSens <- data_reg_ogwSens %>% 
  mutate(modelType = ifelse(str_detect(modelName, 'count'), 'negbin', 'gaussian')) 
data_reg_ppSens <- data_reg_ppSens %>% 
  mutate(modelType = ifelse(str_detect(modelName, 'count'), 'negbin', 'gaussian')) 
data_reg_log10nox <- data_reg_log10nox %>% mutate(modelType = 'gaussian')
data_reg_windsnox <- data_reg_windsnox %>% mutate(modelType = 'gaussian')
# 1m.iii Create separate dataframe for NOx exposure with a gamma distribution, for
#        comparison purposes
#        Note: Will need to remove two nox_autos values that are zero because the
#              gamma distribution includes a log link
data_reg_gamma <- data_reg %>% filter(envExp == "nox_autos") %>% 
  mutate(modelType = 'gamma')
for(i in 9:10){data_reg_gamma$data[[i]] = subset(data_reg_gamma$data[[i]], expEst > 0)}

# 1n Determine range of percent obs kept for outlier sensitivity analysis 
outlier_sens_range <- data_reg_outlierSens %>% ungroup() %>% 
  dplyr::select(modelName, data) %>% 
  unnest(data) %>% dplyr::select(modelName, perc_outlier) %>% distinct() %>% 
  filter(!str_detect(modelName, '_idw_')) %>% 
  filter(!str_detect(modelName, 'pm2.5_rm_'))

####********************************************************
#### 2: Grid search to determine best model parameters  #### 
####********************************************************

# 2a Build constraint grid
# 2a.i Specify constraints to test
#      We also tried 4df and 5df but they resulted in some plots that were 
#      non-sensically squiggly
constraintGrid <- data.frame(
  constraint = rep(c('lin', '2df', '3df'), each = 94))
# 2a.i Replicate regression data 6 times - one row for each constraint to test
data_gridsearch <- do.call('rbind', replicate(3, data_reg, simplify = FALSE)) 
# 2a.ii Bind constraints to replicated dataset 
data_gridsearch <- data_gridsearch %>% bind_cols(constraintGrid)

# 2b Run grid search 
# 2b.i Source regression function
source(paste0(project.folder, 'functions/regs_preds_prelimplots.R'))
# 2b.ii Run grid search (~ 4 min run time)
regs_gridsearch <- cb_reg(dataframe = data_gridsearch, gridsearch = 'yes')

# 2c Identify constraint that results in lowest AIC for each model name
results_gridsearch <- regs_gridsearch %>% 
  dplyr::select(-data, -nlME) %>% # remove so can View df w/out load time
  group_by(modelName) %>% 
  mutate(minAIC = min(aic))

# 2d Select only model constraints with lowest AIC to merge with regression data
results_gridsearch <- results_gridsearch %>% 
  filter(minAIC == aic) %>% 
  dplyr::select(modelName, constraint)

# 2e Repeat steps for sensitivity datasets
# 2e.i DI sensitivity analysis
constraintGrid_diSens <- data.frame(
  constraint = rep(c('lin', '2df', '3df'), each = 20))
data_gridsearch_diSens <- do.call('rbind', replicate(3, data_reg_diSens, simplify = FALSE)) 
data_gridsearch_diSens <- data_gridsearch_diSens %>% bind_cols(constraintGrid_diSens)
source(paste0(project.folder, 'functions/regs_preds_prelimplots.R'))
regs_gridsearch_diSens <- cb_reg(dataframe = data_gridsearch_diSens, gridsearch = 'yes')
results_gridsearch_diSens <- regs_gridsearch_diSens %>% 
  dplyr::select(-data, -nlME) %>% # remove so can View df w/out load time
  group_by(modelName) %>% 
  mutate(minAIC = min(aic))
results_gridsearch_diSens <- results_gridsearch_diSens %>% 
  filter(minAIC == aic) %>% 
  dplyr::select(modelName, constraint)
# 2e.ii Outlier sensitivity analysis
#       Note: three regressions had warning that fitting terminated with step failure
data_gridsearch_outlierSens <- do.call('rbind', replicate(3, data_reg_outlierSens, simplify = FALSE)) 
data_gridsearch_outlierSens <- data_gridsearch_outlierSens %>% bind_cols(constraintGrid)
source(paste0(project.folder, 'functions/regs_preds_prelimplots.R'))
regs_gridsearch_outlierSens <- cb_reg(dataframe = data_gridsearch_outlierSens, gridsearch = 'yes')
results_gridsearch_outlierSens <- regs_gridsearch_outlierSens %>% 
  dplyr::select(-data, -nlME) %>% # remove so can View df w/out load time
  group_by(modelName) %>% 
  mutate(minAIC = min(aic))
results_gridsearch_outlierSens <- results_gridsearch_outlierSens %>% 
  filter(minAIC == aic) %>% 
  dplyr::select(modelName, constraint)
# 2e.iii OGW sensitivity analysis
constraintGrid_ogwSens <- data.frame(
  constraint = rep(c('lin', '2df', '3df'), each = 30))
data_gridsearch_ogwSens <- do.call('rbind', replicate(3, data_reg_ogwSens, simplify = FALSE)) 
data_gridsearch_ogwSens <- data_gridsearch_ogwSens %>% bind_cols(constraintGrid_ogwSens)
source(paste0(project.folder, 'functions/regs_preds_prelimplots.R'))
regs_gridsearch_ogwSens <- cb_reg(dataframe = data_gridsearch_ogwSens, gridsearch = 'yes')
results_gridsearch_ogwSens <- regs_gridsearch_ogwSens %>% 
  dplyr::select(-data, -nlME) %>% # remove so can View df w/out load time
  group_by(modelName) %>% 
  mutate(minAIC = min(aic))
results_gridsearch_ogwSens <- results_gridsearch_ogwSens %>% 
  filter(minAIC == aic) %>% 
  dplyr::select(modelName, constraint)
# 2e.iv Powerplant sensitivity analysis
constraintGrid_ppSens <- data.frame(
  constraint = rep(c('lin', '2df', '3df'), each = 30))
data_gridsearch_ppSens <- do.call('rbind', replicate(3, data_reg_ppSens, simplify = FALSE)) 
data_gridsearch_ppSens <- data_gridsearch_ppSens %>% bind_cols(constraintGrid_ppSens)
source(paste0(project.folder, 'functions/regs_preds_prelimplots.R'))
regs_gridsearch_ppSens <- cb_reg(dataframe = data_gridsearch_ppSens, gridsearch = 'yes')
results_gridsearch_ppSens <- regs_gridsearch_ppSens %>% 
  dplyr::select(-data, -nlME) %>% # remove so can View df w/out load time
  group_by(modelName) %>% 
  mutate(minAIC = min(aic))
results_gridsearch_ppSens <- results_gridsearch_ppSens %>% 
  filter(minAIC == aic) %>% 
  dplyr::select(modelName, constraint)

# 2f Repeat steps for nox datasets
# 2f.i Log10 NOx exposures
constraintGrid_noxonly <- data.frame(
  constraint = rep(c('lin', '2df', '3df'), each = 10))
data_gridsearch_log10nox <- do.call('rbind', replicate(3, data_reg_log10nox, simplify = FALSE)) 
data_gridsearch_log10nox <- data_gridsearch_log10nox %>% bind_cols(constraintGrid_noxonly)
source(paste0(project.folder, 'functions/regs_preds_prelimplots.R'))
regs_gridsearch_log10nox <- cb_reg(dataframe = data_gridsearch_log10nox, gridsearch = 'yes')
results_gridsearch_log10nox <- regs_gridsearch_log10nox %>% 
  dplyr::select(-data, -nlME) %>% # remove so can View df w/out load time
  group_by(modelName) %>% 
  mutate(minAIC = min(aic))
results_gridsearch_log10nox <- results_gridsearch_log10nox %>% 
  filter(minAIC == aic) %>% 
  dplyr::select(modelName, constraint)
# 2f.ii Windsorized NOx exposures
data_gridsearch_windsnox <- do.call('rbind', replicate(3, data_reg_windsnox, simplify = FALSE)) 
data_gridsearch_windsnox <- data_gridsearch_windsnox %>% bind_cols(constraintGrid_noxonly)
source(paste0(project.folder, 'functions/regs_preds_prelimplots.R'))
regs_gridsearch_windsnox <- cb_reg(dataframe = data_gridsearch_windsnox, gridsearch = 'yes')
results_gridsearch_windsnox <- regs_gridsearch_windsnox %>% 
  dplyr::select(-data, -nlME) %>% # remove so can View df w/out load time
  group_by(modelName) %>% 
  mutate(minAIC = min(aic))
results_gridsearch_windsnox <- results_gridsearch_windsnox %>% 
  filter(minAIC == aic) %>% 
  dplyr::select(modelName, constraint)
# 2f.iii Gamma NOx exposures
data_gridsearch_gammanox <- do.call('rbind', replicate(3, data_reg_gamma, simplify = FALSE)) 
data_gridsearch_gammanox <- data_gridsearch_gammanox %>% bind_cols(constraintGrid_noxonly)
source(paste0(project.folder, 'functions/regs_preds_prelimplots.R'))
regs_gridsearch_gammanox <- cb_reg(dataframe = data_gridsearch_gammanox, gridsearch = 'yes')
results_gridsearch_gammanox <- regs_gridsearch_gammanox %>% 
  dplyr::select(-data, -nlME) %>% # remove so can View df w/out load time
  group_by(modelName) %>% 
  mutate(minAIC = min(aic))
results_gridsearch_gammanox <- results_gridsearch_gammanox %>% 
  filter(minAIC == aic) %>% 
  dplyr::select(modelName, constraint)

####*************************
#### 3: Run regressions  #### 
####*************************

# 3a Source functions for models, predictions, and plots to review model results
source(paste0(project.folder, 'functions/regs_preds_prelimplots.R'))

# 3b Add constraint column (from grid search) to regression dataframe
data_reg <- data_reg %>% left_join(results_gridsearch, by = 'modelName')

# 3c Run regressions with `dlnm` (using `crossbasis`) (~30 sec run time)
regs <- cb_reg(dataframe = data_reg, gridsearch = 'no')

# 3d Calculate predicted values for plots, extract needed values, and plot, using
#    `crosspred` and a counterfactual segValue dataframe to predict values 
regs$preds = list(NA)
regs$plots = list(NA)
for(i in 1:nrow(regs)){
  regs$preds[[i]] = extract_preds(obs_seg_values = regs$data[[i]]$segValue,
                                  cb = regs$data[[i]]$seg.cb, 
                                  mod = regs$nlME[[i]],
                                  model_path = model_path,
                                  model_name = regs$modelName[[i]],
                                  linear = regs$constraint[[i]],
                                  model_type = regs$modelType[[i]])
  regs$plots[[i]] = pred_plot_mediancentered(prediction_dataframe = regs$preds[[i]], 
                                             orig_seg_values = regs$data[[i]]$segValue,
                                             env_exp_label = regs$envExpLab[[i]],
                                             seg_label = regs$segLab[[i]],
                                             year = regs$year[[i]],
                                             rural_label = regs$plural_bin[[i]],
                                             model_name = regs$modelName[[i]],
                                             plot_path = plot_path,
                                             model_type = regs$modelType[[i]])}

# 3e Save regressions, predictions, plots
regs %>% write_rds(file = paste0(model_path, 'regs_plots_cbNS.RDS'))

# 3f Repeat steps for sensitivity analyses
# 3f.i DI sensitivity analysis
data_reg_diSens <- data_reg_diSens %>% left_join(results_gridsearch_diSens, by = 'modelName')
regs_diSens <- cb_reg(dataframe = data_reg_diSens, gridsearch = 'no')
regs_diSens$preds = list(NA)
regs_diSens$plots = list(NA)
for(i in 1:nrow(regs_diSens)){
  regs_diSens$preds[[i]] = extract_preds(obs_seg_values = regs_diSens$data[[i]]$segValue,
                                         cb = regs_diSens$data[[i]]$seg.cb, 
                                         mod = regs_diSens$nlME[[i]],
                                         model_path = model_path,
                                         model_name = regs_diSens$modelName[[i]],
                                         linear = regs_diSens$constraint[[i]],
                                         model_type = regs_diSens$modelType[[i]])
  regs_diSens$plots[[i]] = pred_plot_mediancentered(prediction_dataframe = regs_diSens$preds[[i]], 
                                                    orig_seg_values = regs_diSens$data[[i]]$segValue,
                                                    env_exp_label = regs_diSens$envExpLab[[i]],
                                                    seg_label = regs_diSens$segLab[[i]],
                                                    year = regs_diSens$year[[i]],
                                                    rural_label = regs_diSens$plural_bin[[i]],
                                                    model_name = regs_diSens$modelName[[i]],
                                                    plot_path = plot_path,
                                                    model_type = regs_diSens$modelType[[i]])}
regs_diSens %>% write_rds(file = paste0(model_path, 'regs_plots_cbNS_diSens.RDS'))
# 3f.ii Outlier sensitivity analysis
data_reg_outlierSens <- data_reg_outlierSens %>% left_join(results_gridsearch_outlierSens, by = 'modelName')
regs_outlierSens <- cb_reg(dataframe = data_reg_outlierSens, gridsearch = 'no')
regs_outlierSens$preds = list(NA)
regs_outlierSens$plots = list(NA)
for(i in 1:nrow(regs_outlierSens)){
  regs_outlierSens$preds[[i]] = extract_preds(obs_seg_values = regs_outlierSens$data[[i]]$segValue,
                                              cb = regs_outlierSens$data[[i]]$seg.cb, 
                                              mod = regs_outlierSens$nlME[[i]],
                                              model_path = model_path,
                                              model_name = regs_outlierSens$modelName[[i]],
                                              linear = regs_outlierSens$constraint[[i]],
                                              model_type = regs_outlierSens$modelType[[i]])
  regs_outlierSens$plots[[i]] = pred_plot_mediancentered(prediction_dataframe = regs_outlierSens$preds[[i]], 
                                                         orig_seg_values = regs_outlierSens$data[[i]]$segValue,
                                                         env_exp_label = regs_outlierSens$envExpLab[[i]],
                                                         seg_label = regs_outlierSens$segLab[[i]],
                                                         year = regs_outlierSens$year[[i]],
                                                         rural_label = regs_outlierSens$plural_bin[[i]],
                                                         model_name = regs_outlierSens$modelName[[i]],
                                                         plot_path = plot_path,
                                                         model_type = regs_outlierSens$modelType[[i]])}
regs_outlierSens %>% write_rds(file = paste0(model_path, 'regs_plots_cbNS_outlierSens.RDS'))
# 3f.iii OGW sensitivity analysis
data_reg_ogwSens <- data_reg_ogwSens %>% left_join(results_gridsearch_ogwSens, by = 'modelName')
regs_ogwSens <- cb_reg(dataframe = data_reg_ogwSens, gridsearch = 'no')
regs_ogwSens$preds = list(NA)
regs_ogwSens$plots = list(NA)
for(i in 1:nrow(regs_ogwSens)){
  regs_ogwSens$preds[[i]] = extract_preds(obs_seg_values = regs_ogwSens$data[[i]]$segValue,
                                         cb = regs_ogwSens$data[[i]]$seg.cb, 
                                         mod = regs_ogwSens$nlME[[i]],
                                         model_path = model_path,
                                         model_name = regs_ogwSens$modelName[[i]],
                                         linear = regs_ogwSens$constraint[[i]],
                                         model_type = regs_ogwSens$modelType[[i]])
  regs_ogwSens$plots[[i]] = pred_plot_mediancentered(prediction_dataframe = regs_ogwSens$preds[[i]], 
                                                    orig_seg_values = regs_ogwSens$data[[i]]$segValue,
                                                    env_exp_label = regs_ogwSens$envExpLab[[i]],
                                                    seg_label = regs_ogwSens$segLab[[i]],
                                                    year = regs_ogwSens$year[[i]],
                                                    rural_label = regs_ogwSens$plural_bin[[i]],
                                                    model_name = regs_ogwSens$modelName[[i]],
                                                    plot_path = plot_path,
                                                    model_type = regs_ogwSens$modelType[[i]])}
regs_ogwSens %>% write_rds(file = paste0(model_path, 'regs_plots_cbNS_ogwSens.RDS'))
# 3f.iv PP sensitivity analysis
data_reg_ppSens <- data_reg_ppSens %>% left_join(results_gridsearch_ppSens, by = 'modelName')
regs_ppSens <- cb_reg(dataframe = data_reg_ppSens, gridsearch = 'no')
regs_ppSens$preds = list(NA)
regs_ppSens$plots = list(NA)
for(i in 1:nrow(regs_ppSens)){
  regs_ppSens$preds[[i]] = extract_preds(obs_seg_values = regs_ppSens$data[[i]]$segValue,
                                         cb = regs_ppSens$data[[i]]$seg.cb, 
                                         mod = regs_ppSens$nlME[[i]],
                                         model_path = model_path,
                                         model_name = regs_ppSens$modelName[[i]],
                                         linear = regs_ppSens$constraint[[i]],
                                         model_type = regs_ppSens$modelType[[i]])
  regs_ppSens$plots[[i]] = pred_plot_mediancentered(prediction_dataframe = regs_ppSens$preds[[i]], 
                                                    orig_seg_values = regs_ppSens$data[[i]]$segValue,
                                                    env_exp_label = regs_ppSens$envExpLab[[i]],
                                                    seg_label = regs_ppSens$segLab[[i]],
                                                    year = regs_ppSens$year[[i]],
                                                    rural_label = regs_ppSens$plural_bin[[i]],
                                                    model_name = regs_ppSens$modelName[[i]],
                                                    plot_path = plot_path,
                                                    model_type = regs_ppSens$modelType[[i]])}
regs_ppSens %>% write_rds(file = paste0(model_path, 'regs_plots_cbNS_ppSens.RDS'))

# 3g Repeat steps for NOx datasets
# 3f.i Log10 NOx dataset
data_reg_log10nox <- data_reg_log10nox %>% left_join(results_gridsearch_log10nox, by = 'modelName')
regs_log10nox <- cb_reg(dataframe = data_reg_log10nox, gridsearch = 'no')
regs_log10nox$preds = list(NA)
regs_log10nox$plots = list(NA)
for(i in 1:nrow(regs_log10nox)){
  regs_log10nox$preds[[i]] = extract_preds(obs_seg_values = regs_log10nox$data[[i]]$segValue,
                                         cb = regs_log10nox$data[[i]]$seg.cb, 
                                         mod = regs_log10nox$nlME[[i]],
                                         model_path = model_path,
                                         model_name = regs_log10nox$modelName[[i]],
                                         linear = regs_log10nox$constraint[[i]],
                                         model_type = regs_log10nox$modelType[[i]])
  regs_log10nox$plots[[i]] = pred_plot_mediancentered(prediction_dataframe = regs_log10nox$preds[[i]], 
                                                    orig_seg_values = regs_log10nox$data[[i]]$segValue,
                                                    env_exp_label = regs_log10nox$envExpLab[[i]],
                                                    seg_label = regs_log10nox$segLab[[i]],
                                                    year = regs_log10nox$year[[i]],
                                                    rural_label = regs_log10nox$plural_bin[[i]],
                                                    model_name = regs_log10nox$modelName[[i]],
                                                    plot_path = plot_path,
                                                    model_type = regs_log10nox$modelType[[i]])}
regs_log10nox %>% write_rds(file = paste0(model_path, 'regs_plots_cbNS_log10nox.RDS'))
# 3f.ii Windsorized NOx dataset
data_reg_windsnox <- data_reg_windsnox %>% left_join(results_gridsearch_windsnox, by = 'modelName')
regs_windsnox <- cb_reg(dataframe = data_reg_windsnox, gridsearch = 'no')
regs_windsnox$preds = list(NA)
regs_windsnox$plots = list(NA)
for(i in 1:nrow(regs_windsnox)){
  regs_windsnox$preds[[i]] = extract_preds(obs_seg_values = regs_windsnox$data[[i]]$segValue,
                                           cb = regs_windsnox$data[[i]]$seg.cb, 
                                           mod = regs_windsnox$nlME[[i]],
                                           model_path = model_path,
                                           model_name = regs_windsnox$modelName[[i]],
                                           linear = regs_windsnox$constraint[[i]],
                                           model_type = regs_windsnox$modelType[[i]])
  regs_windsnox$plots[[i]] = pred_plot_mediancentered(prediction_dataframe = regs_windsnox$preds[[i]], 
                                                      orig_seg_values = regs_windsnox$data[[i]]$segValue,
                                                      env_exp_label = regs_windsnox$envExpLab[[i]],
                                                      seg_label = regs_windsnox$segLab[[i]],
                                                      year = regs_windsnox$year[[i]],
                                                      rural_label = regs_windsnox$plural_bin[[i]],
                                                      model_name = regs_windsnox$modelName[[i]],
                                                      plot_path = plot_path,
                                                      model_type = regs_windsnox$modelType[[i]])}
regs_windsnox %>% write_rds(file = paste0(model_path, 'regs_plots_cbNS_windsnox.RDS'))
# 3f.iii NOx dataset run with a Gamma family and log link
data_reg_gamma <- data_reg_gamma %>% left_join(results_gridsearch_gammanox, by = 'modelName')
regs_gammanox <- cb_reg(dataframe = data_reg_gamma, gridsearch = 'no')
regs_gammanox$preds = list(NA)
regs_gammanox$plots = list(NA)
for(i in 1:nrow(regs_gammanox)){
  regs_gammanox$preds[[i]] = extract_preds(obs_seg_values = regs_gammanox$data[[i]]$segValue,
                                           cb = regs_gammanox$data[[i]]$seg.cb, 
                                           mod = regs_gammanox$nlME[[i]],
                                           model_path = model_path,
                                           model_name = regs_gammanox$modelName[[i]],
                                           linear = regs_gammanox$constraint[[i]],
                                           model_type = regs_gammanox$modelType[[i]])
  regs_gammanox$plots[[i]] = pred_plot_mediancentered(prediction_dataframe = regs_gammanox$preds[[i]], 
                                                      orig_seg_values = regs_gammanox$data[[i]]$segValue,
                                                      env_exp_label = regs_gammanox$envExpLab[[i]],
                                                      seg_label = regs_gammanox$segLab[[i]],
                                                      year = regs_gammanox$year[[i]],
                                                      rural_label = regs_gammanox$plural_bin[[i]],
                                                      model_name = regs_gammanox$modelName[[i]],
                                                      plot_path = plot_path,
                                                      model_type = regs_gammanox$modelType[[i]])}
regs_gammanox %>% write_rds(file = paste0(model_path, 'regs_plots_cbNS_gammanox.RDS'))

####**********************************
#### 4: Review model diagnostics  #### 
####**********************************

# Resources for model diagnostics
# https://fromthebottomoftheheap.net/2017/05/04/compare-mgcv-with-glmmtmb/. # zero inflated with mgcv
# https://www.theanalysisfactor.com/poisson-or-negative-binomial-using-count-model-diagnostics-to-select-a-model/ # reviewing poisson and neg binomial resid plots  
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#recognizing-overunderdispersion # DHARMa vignette for checking models
# https://stats.stackexchange.com/questions/610084/dispersion-parameter-in-dharma # DHARMa dispersion parameter
# https://tuos-bio-data-skills.github.io/intro-stats-book/non-linear-regression-in-R.html#checking-the-assumptions # checking non-linear models

# Will check: 1. deviation from expected distribution (q-q plot, dharma resids vs fits)
#             2. spatial autocorrelation (moran's i)

# Already accounted for through sensitivity analysis or modelling plan:
#             1. outliers (sens analysis removing large outliers)
#             2. non-linearity (used AIC to choose whether model was linear or non-linear)

# Note that a traditional resids vs fitted plot will not necessarily have no pattern,
# as a negative binomial model does not assume normality of deviance residuals.
# Also, for mixed models, model transformations can result in residuals that have
# weird patterns in plots (see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html)
# Instead we will use the DHARMa package to simulate scaled residuals and do diagnostic tests,
# however we're not sure this is adequately addressing the mixed model because we code
# random effects using a spline rather than the random effects function in a model.
# It does give us an idea though.

# In general, we see some underdispersion-like patterns in some count model residuals.
# For power plants, this may be caused by the outcome acting more like a categorical
# variable, because while the data generating process is a continuous count, in 1940 
# there are only a max of 3 plants in a county. It may also be caused by the long
# right tail on the power plant distribution causing the negative binomial model
# to expect more 0 values than are present in the distribution.
# For PM or oil/gas wells, odd patterns may be caused by some residual spatial autocorrelation, which has been
# difficult to correct because of the stratification, which creates "island" counties
# with no neighbors. We do both include a random effect for state and a spline
# with lat and lon to help address this.
# For oil/gas wells specifically, model residuals show a non-linear vertical line
# in residuals, which is altered by removing the tensor plate spline. We think this
# residual pattern is caused by spatial clustering.

# There is also some heteroskedasticity for NOx, which is reduced when removing outliers 
# NOx model residuals are improved hugely by log10 transforming the data. The results remain
# the same direction, with confidence intervals becoming much tighter. When running
# NOx models with a Gamma family (log link) we see quite similar results to the log
# transformed model, but the residuals of the model are not as good.

# 4a Load all regressions (needed if running this chunk at different time than models were run)
#regs <- read_rds(paste0(model_path, 'regs_plots_cbNS.RDS'))

# 4b Create dataframes for linear and non-linear models because we will additionally
#    run a gam.check on the non-linear models
diag.linear <- regs %>% filter(constraint == 'lin') %>% 
  filter(!str_detect(modelName, '_idw_')) %>% filter(!str_detect(modelName, '_rm_')) %>% 
  filter(!str_detect(modelName, 'Buffer')) %>% filter(!str_detect(modelName, '_10tpy_'))
diag.nonlinear <- regs %>% filter(constraint == '2df' | constraint == '3df') %>% 
  filter(!str_detect(modelName, '_idw_')) %>% filter(!str_detect(modelName, '_rm_')) %>% 
  filter(!str_detect(modelName, 'Buffer')) %>% filter(!str_detect(modelName, '_10tpy_'))

# 4c Run residual plots 
#    We run DHARMa plots for linear models and DHARMa plus gam.check for nonlinear models
#    Conclusions:
#    Not seeing overdispersion, but are seeing underdispersion and some heteroskedasticity,
#    and occasionally some odd curved patterns in the residuals for count outcomes
#    Model residuals for NOx models are particularly bad
#    Model residuals for PM2.5 are pretty good
# 4c.i Outcome = fossil fuel power plants
diag_fospp_count_di_2010_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[1]], plot = T) # underdispersed, too many lg resids
diag_fospp_count_raceEduICE_hsd10_2010_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[2]], plot = T) # underdispersed, too mamny lg resids
diag_fospp_count_di_2010_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[7]], plot = T) # underdispersed, weird resid pattern
diag_fospp_count_nbrOne_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[10]], plot = T) # pretty good
diag_fospp_count_di_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[11]], plot = T) # pretty good
diag_fospp_count_raceEduICE_hsd10_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[12]], plot = T) # good enough, little bit of resid oddness but ok
diag_fospp_count_nbrOne_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[15]], plot = T) # good enough, little bit more resid oddness and qq deviation but ok
diag_fospp_count_di_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[16]], plot = T) # pretty good
diag_fospp_count_raceEduICE_hsd10_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[17]], plot = T) # pretty good
diag_fospp_count_raceEduICE_hsd10_2010_Urban <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[3]], plot = T) # underdispersed, weird resid pattern
gam.check(diag.nonlinear$nlME[[3]]) # underdispersion and some slight funnelling in resid plot (stripes are from counts)
# 4c.ii Outcome = oil/gas wells
diag_ogw_count_di_2010_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[3]], plot = T) # qq ok, wierd resid pattern
diag_ogw_count_nbrOne_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[13]], plot = T) # a little underdispersed & wierd resid pattern
diag_ogw_count_di_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[14]], plot = T) # a little underdispersed & wierd resid pattern
diag_ogw_count_nbrOne_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[18]], plot = T) # a little underdispersed & wierd resid pattern
diag_ogw_count_di_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[19]], plot = T) # a little underdispersed & wierd resid pattern
diag_ogw_count_raceEduICE_hsd10_2010_Rural <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[1]], plot = T) 
gam.check(diag.nonlinear$nlME[[1]]) # qq plot ok but resids plot has pattern and some funnelling
diag_ogw_count_di_2010_Urban <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[4]], plot = T) 
gam.check(diag.nonlinear$nlME[[4]]) # qq fine but resids has pattern
diag_ogw_count_raceEduICE_hsd10_2010_Urban <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[5]], plot = T) 
gam.check(diag.nonlinear$nlME[[5]]) # qq fine but resids has pattern
diag_ogw_count_raceEduICE_hsd10_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[8]], plot = T) 
gam.check(diag.nonlinear$nlME[[8]]) # qq ok, a little off, pattern in resids, lat/long spline not enough knots?, lg outlier
diag_ogw_count_raceEduICE_hsd10_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[15]], plot = T)
gam.check(diag.nonlinear$nlME[[15]]) # qq indicates underdispersion, resids have a weird pattern, lat/long spline not enough knots?
# 4c.iii Outcome = automobile NOx emissions
diag_nox_autos_raceEduICE_hsd10_2010_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[5]], plot = T) # underdispersed, wierd resid pattern
diag_nox_autos_raceEduICE_hsd10_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[20]], plot = T) # majorly underdispersed & wierd resids
diag_nox_autos_di_2010_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[4]], plot = T) # underdispersion & wierd resids
diag_nox_autos_di_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[10]], plot = T) # majorly underdispersed & wierd resids
diag_nox_autos_di_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[17]], plot = T)
gam.check(diag.nonlinear$nlME[[17]]) # q indicates underdispersion, resids funnelled
diag_nox_autos_di_2010_Urban <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[6]], plot = T) 
gam.check(diag.nonlinear$nlME[[6]]) # qq indicates underdispersion, resids funnelled, lat/long spline not enough knots?
diag_nox_autos_raceEduICE_hsd10_2010_Urban <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[7]], plot = T) 
gam.check(diag.nonlinear$nlME[[7]]) # qq indicates underdispersion, resids funnelled, lat/long spline not enough knots?
diag_nox_autos_nbrOne_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[16]], plot = T)
gam.check(diag.nonlinear$nlME[[16]]) # qq indicates underdispersion, wierd resids
diag_nox_autos_nbrOne_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[9]], plot = T) 
gam.check(diag.nonlinear$nlME[[9]]) # qq indicates underdispersion, wierd resids, lat/long spline not enough knots?
diag_nox_autos_raceEduICE_hsd10_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[11]], plot = T) 
gam.check(diag.nonlinear$nlME[[11]]) # qq indicates underdispersion, resids funnelled, lat/long spline not enough knots?
# 4c.iv Outcome = PM2.5 estimates
diag_meanPM2.5_di_2010_Rural <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[6]], plot = T) # resids a little wierd, but pretty good
diag_meanPM2.5_di_2010_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[8]], plot = T) # good enough, little bit of oddness but ok
diag_meanPM2.5_raceEduICE_hsd10_2010_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[9]], plot = T) # good enough, little bit of oddness but ok
diag_meanPM2.5_nbrOne_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[21]], plot = T) # good enough, little underdispersed, little off in resids
diag_meanPM2.5_di_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[22]], plot = T) # good enough, little underdispersed, little off in resids
diag_meanPM2.5_raceEduICE_hsd10_1940_Urban <- DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[23]], plot = T) # good enough, little underdispersed, little off in resids
diag_meanPM2.5_raceEduICE_hsd10_2010_Rural <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[2]], plot = T) 
gam.check(diag.nonlinear$nlME[[2]]) # qq plot ok and resids pretty good, some slight issue
diag_meanPM2.5_nbrOne_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[12]], plot = T) 
gam.check(diag.nonlinear$nlME[[12]]) # this looks pretty good
diag_meanPM2.5_di_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[13]], plot = T) 
gam.check(diag.nonlinear$nlME[[13]]) # this is also okay
diag_meanPM2.5_raceEduICE_hsd10_1940_Rural <- DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[14]], plot = T) 
gam.check(diag.nonlinear$nlME[[14]]) # this is ok, not great but decent

# 4d Run DHARMa dispersion test and zero inflation tests as needed
#    Conclusion: There doesn't seem to be an issue with zero inflation.
# 4d.i Outcome = fossil fuel power plants
testDispersion(diag_fospp_count_di_2010_Rural) # 0.16 and sig - zero inflation?                                  # more worrisome?
testZeroInflation(diag_fospp_count_di_2010_Rural) # model is overestimating zeros
testDispersion(diag_fospp_count_raceEduICE_hsd10_2010_Rural) # 0.16 and sig - zero inflation?                    # more worrisome?
testZeroInflation(diag_fospp_count_raceEduICE_hsd10_2010_Rural) # model is overestimating zeros
testDispersion(diag_fospp_count_di_2010_Urban) # 0.42 and sig - zero inflation?                                  # more worrisome?
testZeroInflation(diag_fospp_count_di_2010_Urban) # model is overestimating zeros
testDispersion(diag_ogw_count_di_2010_Rural) # 0.47 but not sig
testDispersion(diag_fospp_count_nbrOne_1940_Rural) # 0.1 and sig                                                 # more worrisome?
testZeroInflation(diag_fospp_count_nbrOne_1940_Rural) # model is overestimating zeros
testDispersion(diag_fospp_count_di_1940_Rural) # 0.1 and sig                                                     # more worrisome?
testDispersion(diag_fospp_count_raceEduICE_hsd10_1940_Rural) # 0.1 and sig                                       # more worrisome?
testZeroInflation(diag_fospp_count_raceEduICE_hsd10_1940_Rural) # model is overestimating zeros
testDispersion(diag_fospp_count_nbrOne_1940_Urban) # 0.1 and sig                                                # more worrisome?
testZeroInflation(diag_fospp_count_nbrOne_1940_Urban) # model is overestimating zeros
testDispersion(diag_fospp_count_di_1940_Urban) # 0.1 and sig                                                    # more worrisome?
testZeroInflation(diag_fospp_count_di_1940_Urban) # model is overestimating zeros
testDispersion(diag_fospp_count_raceEduICE_hsd10_1940_Urban) # 0.10                                             # more worrisome?
testZeroInflation(diag_fospp_count_raceEduICE_hsd10_1940_Urban) # model is overestimating zeros
testDispersion(diag_fospp_count_raceEduICE_hsd10_2010_Urban) # underdispersed, 0.45 and sig
# 4d.ii Outcome = oil/gas wells
testDispersion(diag_ogw_count_nbrOne_1940_Rural) # 0.8 and not sig
testDispersion(diag_ogw_count_di_1940_Rural) # 1.2 not sig
testDispersion(diag_ogw_count_nbrOne_1940_Urban) # 0.74 not sig
testDispersion(diag_ogw_count_di_1940_Urban) # 0.85 not sig
testDispersion(diag_ogw_count_raceEduICE_hsd10_2010_Rural) # underdispersed 0.56 and sig
# 4d.iii Outcome = vehicle NOx emissions
testDispersion(diag_nox_autos_raceEduICE_hsd10_2010_Rural) # 0.98 not sig
testDispersion(diag_nox_autos_di_1940_Rural) # 0.95 not sig
testDispersion(diag_nox_autos_raceEduICE_hsd10_1940_Urban) # 0.98 not sig
testDispersion(diag_nox_autos_di_2010_Urban) # 0.97 not sig
testDispersion(diag_nox_autos_raceEduICE_hsd10_2010_Urban) # # 0.97 not sig
# 4d.iv outcome = PM2.5
testDispersion(diag_meanPM2.5_di_2010_Rural) # 0.94 not sig
testDispersion(diag_meanPM2.5_di_2010_Urban) # 0.94
testDispersion(diag_meanPM2.5_raceEduICE_hsd10_2010_Urban) # 0.93
testDispersion(diag_meanPM2.5_nbrOne_1940_Urban) # 0.89 not sig
testDispersion(diag_meanPM2.5_di_1940_Urban) # 0.89 not sig
testDispersion(diag_meanPM2.5_raceEduICE_hsd10_1940_Urban) # 0.89 not sig
testDispersion(diag_meanPM2.5_nbrOne_1940_Rural) # 0.96 not sig

# 4e Review histograms & scatterplots to look for zero inflation in counts and
#    potential for nonlinearity, try to eyeball what's going on. (only in linear models)
#      Conclusions: 
#      There are only a few distinct values for powerplants in a lot of analysis (e.g., index 17, 2)
#      so this outcome might operate more like a categorical variable (3 pps, 7 pps)
#      statistically, however, the data generating process is truly continuous. 
#      Generally I think using a linear model is okay throughout, especially b/c we used 
#      AIC to choose linearity or non-linearity in the model selection process.
diag.linear$histogram = vector('list', 23)
diag.linear$scatterplot = vector('list', 23)
for(a in 1:23){
  #browser()  # Pauses execution for debugging
  temp_data <- diag.linear$data[[a]]
  diag.linear$histogram[[a]] =
    ggplot(temp_data, aes(x = expEst)) +
    geom_histogram() +
    ggtitle(diag.linear$modelName[[a]]) +
    theme_bw()
  diag.linear$scatterplot[[a]] = 
    ggplot(temp_data, aes(x = segValue, y = expEst)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    ggtitle(diag.linear$modelName[[a]]) +
    theme_bw()   
}
diag.linear$histogram[[14]]
diag.linear$scatterplot[[14]]

# 4f Investigate impact of removing outliers on residual plots
#      Conclusions:
#      NOx models with poor residual plots are substantially improved when removing outliers,
#      though there is still a funelling pattern (heteroskedasticity) 
#      This doesn't really help for powerplants, which have a curve suggesting an 
#        unidentified pattern or missing variable
# 4f.i nox_autos_di_1940_Rural
regs_cbNS_outlierSens <- read_rds(paste0(model_path, 'regs_plots_cbNS_outlierSens.RDS'))
DHARMa::simulateResiduals(fittedModel = regs_cbNS_outlierSens$nlME[[48]], plot = T) # outliers removed
DHARMa::simulateResiduals(fittedModel = diag.nonlinear$nlME[[10]], plot = T)        # original model
# 4f.ii nox_autos_raceEduICE_hsd10_2010_Rural
DHARMa::simulateResiduals(fittedModel = regs_cbNS_outlierSens$nlME[[6]], plot = T)  # outliers removed
DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[5]], plot = T)            # original model
# 4f.iii fospp_count_di_2010_Urban
DHARMa::simulateResiduals(fittedModel = regs_cbNS_outlierSens$nlME[[21]], plot = T) # outliers removed
DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[7]], plot = T)            # original model

# 4g Investigate impact of transformations on residual plots for NOx models
#    (Also previously tried log transforming PM2.5, code not shown. Did not help.)
#    Below we compare four model specifications for various NOx-segregation-year-rurality strata
#    by reviewing QQ plots from the DHARMa package (simulation based, robust to 
#    skewed distributions like Gamma), a manual fit vs resids plot, 
#    and looking at the model results to determine stability and consistency.
#    We don't use gam.check because it can look misleading for skewed distributions.
#      Conclusions:
#       -Tried center/scale, this didn't change anything (code not shown)
#       -Windsorizing didn't really help much
#       -Gamma helped sometimes but not always
#       -Log10 had much better residuals, and very similar model results as Gamma
#        Generally log10 was a tighter confidence interval, and way better model
#        diagnostics
# 4g.i Load models
regs_log10nox <- read_rds(paste0(model_path, 'regs_plots_cbNS_log10nox.RDS'))
regs_windsnox <- read_rds(paste0(model_path, 'regs_plots_cbNS_windsnox.RDS'))
regs_gammanox <- read_rds(paste0(model_path, 'regs_plots_cbNS_gammanox.RDS'))
# 4g.ii Create function for manual fits vs resids plot     
plot_resid_response <- function(model) {
  # Fitted values on response scale
  fitted_resp <- fitted(model, type = "response")
  # Residuals on response scale
  resid_resp <- residuals(model, type = "response")
  # Plot
  plot(fitted_resp, resid_resp,
       xlab = "Fitted values (response scale)",
       ylab = "Residuals (observed - fitted)",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red")}
#
# 4g.iii Strata: nox_autos_raceEduICE_hsd10_1940_Urban
#        NYC, a large outlier, blows up in predictions and makes resid plots hard
#        to read.
# 4g.iiia DHARMa residuals
DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[22]], plot = T)             # original model
DHARMa::simulateResiduals(fittedModel = regs_log10nox$nlME[[10]], plot = T)           # log10 model
DHARMa::simulateResiduals(fittedModel = regs_windsnox$nlME[[10]], plot = T)           # windsorized model
DHARMa::simulateResiduals(fittedModel = regs_gammanox$nlME[[10]], plot = T)           # Gamma model
# 4g.iiib Manual residual plot
plot_resid_response(diag.linear$nlME[[22]])                                           # original model
plot_resid_response(regs_log10nox$nlME[[10]])                                         # log10 model
plot_resid_response(regs_windsnox$nlME[[10]])                                         # windsorized model
plot_resid_response(regs_gammanox$nlME[[10]])                                         # Gamma model
# 4g.iiic review results
diag.linear$plots[[22]]                                                               # original model
regs_log10nox$plots[[10]]                                                             # log10 model
regs_windsnox$plots[[10]]                                                             # windsorized model
regs_gammanox$plots[[10]]                                                             # Gamma model
#
# 4g.iv Strata: nox_autos_di_2010_Rural
# 4g.iva DHARMa residuals
DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[4]], plot = T)             # original model
DHARMa::simulateResiduals(fittedModel = regs_log10nox$nlME[[1]], plot = T)           # log10 model
DHARMa::simulateResiduals(fittedModel = regs_windsnox$nlME[[1]], plot = T)           # windsorized model
DHARMa::simulateResiduals(fittedModel = regs_gammanox$nlME[[1]], plot = T)           # Gamma model
# 4g.ivb Manual residual plot
plot_resid_response(diag.linear$nlME[[4]])                                           # original model
plot_resid_response(regs_log10nox$nlME[[1]])                                         # log10 model
plot_resid_response(regs_windsnox$nlME[[1]])                                         # windsorized model
plot_resid_response(regs_gammanox$nlME[[1]])                                         # Gamma model
# 4g.ivc review results
diag.linear$plots[[4]]                                                               # original model
regs_log10nox$plots[[1]]                                                             # log10 model
regs_windsnox$plots[[1]]                                                             # windsorized model
regs_gammanox$plots[[1]]                                                             # Gamma model
#
# 4g.v Strata: nox_autos_raceEduICE_hsd10_2010_Rural
# 4g.va DHARMa residuals
DHARMa::simulateResiduals(fittedModel = diag.linear$nlME[[5]], plot = T)             # original model
DHARMa::simulateResiduals(fittedModel = regs_log10nox$nlME[[2]], plot = T)           # log10 model
DHARMa::simulateResiduals(fittedModel = regs_windsnox$nlME[[2]], plot = T)           # windsorized model
DHARMa::simulateResiduals(fittedModel = regs_gammanox$nlME[[2]], plot = T)           # Gamma model
# 4g.vb Manual residual plot
plot_resid_response(diag.linear$nlME[[5]])                                           # original model
plot_resid_response(regs_log10nox$nlME[[2]])                                         # log10 model
plot_resid_response(regs_windsnox$nlME[[2]])                                         # windsorized model
plot_resid_response(regs_gammanox$nlME[[2]])                                         # Gamma model
# 4g.vc review results
diag.linear$plots[[5]]                                                               # original model
regs_log10nox$plots[[2]]                                                             # log10 model
regs_windsnox$plots[[2]]                                                             # windsorized model
regs_gammanox$plots[[2]]                                                             # Gamma model

# 4h Calculate moran's i values
# 4h.i Set up dataframe to be used in for loop
diag <- regs %>% filter(!str_detect(modelName, '_idw_')) %>% 
  filter(!str_detect(modelName, 'Buffer')) %>% filter(!str_detect(modelName, '_rm_')) %>% 
  filter(!str_detect(modelName, 'nox')) %>% 
  bind_rows(regs_log10nox)
diag$moran = list(NA); diag$moran_stat = character(nrow(diag))
# 4h.ii Run for loop to calculate moran's i
#       Will take ~ 20-30 min to run
#       diag <- diag[1:2, ] # for debugging
for (i in 1:length(diag$modelName)) {
  
  #i = 1 #(for debugging)
  
  # Select year and model data and pull dharma residuals
  year = diag$year[[i]]
  mod_data = diag$data[[i]]
  dharma_object = DHARMa::simulateResiduals(diag$nlME[[i]])
  resids = residuals(dharma_object)
  
  # Confirm that data and dharma residuals are of same length
  a = length(resids)
  b = dim(mod_data)[1]
  c = a-b
  if(!c == 0){stop('length of data and resids not same')}
  
  # Merge data with dharma residuals
  mod_data$dharma_residuals <- resids
  
  # Merge data with appropriate shapefile
  if(str_detect(year, '1940')){
    county1940 <- as.data.frame(county1940)
    mod_data <- mod_data %>% 
      left_join(county1940, by = c('st_cnty_yr_id' = 'jnhgis40'))
    mod_data <- sf::st_as_sf(mod_data)
  }
  if(str_detect(year, '2010')){
    county2010 <- as.data.frame(county2010)
    mod_data <- mod_data %>% 
      left_join(county2010, by = c('st_cnty_yr_id' = 'jfips10'))
    mod_data <- sf::st_as_sf(mod_data)
  }
  
  # Define neighbors (queen [any contact])
  neighbors <- spdep::poly2nb(mod_data, queen = TRUE)
  
  # Calculate spatial weights for neighbor list
  weights <- spdep::nb2listw(neighbors, zero.policy = TRUE)
  
  # Calculate Moran's I 
  morans <- spdep::moran.test(mod_data$dharma_residuals, weights)
  
  # Save Moran's I object
  diag$moran[[i]] = morans
  
  # Save Moran's I and statistic and p-value in easily readable form
  diag$moran_stat[[i]] = paste0(round(morans$estimate[1], digits = 4), ' p = ', 
                                round(morans$p.value, digits = 4))
  
}

# 4i Save moran output
#    Conclusions:
#    For most models spatial autocorrelation does not seem to be an issue. 
#    Except for PM2.5 - these models have high moran's i values. After much
#    discussion and adding a tensor spline on county lat/lon to the model, to 
#    only a little improvement, we think the reason for this is that the spatial 
#    resolution of the PM2.5 grid is high, and so neighboring counties are 
#    falling within the same grid.
diag %>% ungroup() %>% 
  dplyr::select(envExpLab, segLab, plural_bin_imp, year,
                moran_stat, modelName, moran) %>% 
  write_rds(paste0(model_path, 'moran_wTP_oct25.RDS'))

# 4j Look at oil/gas well residuals by predictors to id variable causing non-linear
#    vertical pattern at higher values of model predictions
#    Conclusion:
#    specific pattern is caused by tensor plate spline on lat/lon, pattern in 
#    model residuals w/out tp spline likely from spatial autocorrelation
plot(diag_ogw_count_raceEduICE_hsd10_2010_Rural) # main -- has non-linear vertical pattern
DHARMa::plotResiduals(diag_ogw_count_raceEduICE_hsd10_2010_Rural, diag.nonlinear$data[[1]]$popDens) # not it
plot(DHARMa::recalculateResiduals(diag_ogw_count_raceEduICE_hsd10_2010_Rural, group = diag.nonlinear$data[[1]]$statefip)) # not it
DHARMa::plotResiduals(diag_ogw_count_raceEduICE_hsd10_2010_Rural, diag.nonlinear$data[[1]]$totPop) # not it
DHARMa::plotResiduals(diag_ogw_count_raceEduICE_hsd10_2010_Rural, diag.nonlinear$data[[1]]$blackPop) # not it
plot(DHARMa::recalculateResiduals(diag_ogw_count_raceEduICE_hsd10_2010_Rural, group = diag.nonlinear$data[[1]]$x_centroid)) # same as main 
plot(DHARMa::recalculateResiduals(diag_ogw_count_raceEduICE_hsd10_2010_Rural, group = diag.nonlinear$data[[1]]$y_centroid)) # same as main
newMod <- mgcv::gam(expEst ~ seg.cb + popDens + s(statefip, bs = 're'), #+ s(y_centroid, x_centroid, bs = "tp"),
                          data = diag.nonlinear$data[[1]],
                          family = nb(),
                          method = 'REML')
DHARMa::simulateResiduals(newMod, plot = T) # line still shows up but now doesn't have the same shape. Shape is being induced by tp spline

# 4k Look at powerplant residuals by predictors
#    No real conclusions here.
plot(diag_fospp_count_raceEduICE_hsd10_2010_Urban)
DHARMa::plotResiduals(diag_fospp_count_raceEduICE_hsd10_2010_Urban, diag.nonlinear$data[[3]]$popDens) # higher resids across all pop dens values
plot(DHARMa::recalculateResiduals(diag_fospp_count_raceEduICE_hsd10_2010_Urban, group = diag.nonlinear$data[[3]]$statefip)) # little bit of pattern
DHARMa::plotResiduals(diag_fospp_count_raceEduICE_hsd10_2010_Urban, diag.nonlinear$data[[3]]$totPop) # same as for pop dens
DHARMa::plotResiduals(diag_fospp_count_raceEduICE_hsd10_2010_Urban, diag.nonlinear$data[[3]]$blackPop) # same as for pop dens
plot(DHARMa::recalculateResiduals(diag_fospp_count_raceEduICE_hsd10_2010_Urban, group = diag.nonlinear$data[[3]]$x_centroid)) # same as main 
plot(DHARMa::recalculateResiduals(diag_fospp_count_raceEduICE_hsd10_2010_Urban, group = diag.nonlinear$data[[3]]$y_centroid)) # same as main
newMod2 <- mgcv::gam(expEst ~ seg.cb + popDens + s(statefip, bs = 're'), #+ s(y_centroid, x_centroid, bs = "tp"),
                    data = diag.nonlinear$data[[3]],
                    family = nb(),
                    method = 'REML')
DHARMa::simulateResiduals(newMod2, plot = T) # doesn't really change things
table(diag.nonlinear$data[[3]]$expEst)



#******************************************************************************** Playing w models to reduce Moran's I
# This code does not run on current dataframes, but is left in the script for reference
# in case needed again. In that case it will need to be edited to call in teh correct 
# dataframes.

# Example model: meanPM2.5_raceEduICE_hsd10_1940_Rural - row 14 in diag.nonlinear, row 55 in regs

county1940 <- st_read(paste0(spatial_data_path, 'FINAL_1940_county_shapefile_clean/counties_contiguous_1940.shp')) %>% janitor::clean_names() %>% st_make_valid()
county1940mi <- as.data.frame(county1940) %>% dplyr::select(jnhgis40, geometry)
test_data <- regs2$data[[30]] %>% 
  left_join(county1940mi, by = c('st_cnty_yr_id' = 'jnhgis40')) 
test_data$county_nb_id <- factor(1:nrow(test_data))  # Ensure IDs are factors
neighbors <- spdep::poly2nb(test_data$geometry, queen = TRUE)
seg.cb <- crossbasis(
  x = test_data$segValue,                   
  lag = 0,                                            
  argvar = list(fun = "ns", df = 3))            
mod = mgcv::gam(expEst ~ seg.cb + popDens + s(statefip, bs = 're') + s(y_centroid, x_centroid, bs = "tp"), #s(county_nb_id, bs = 'mrf', xt = list(nb = neighbors)),#s(y_centroid, x_centroid, bs = 'sos', k = 100),#s(y_centroid, x_centroid, k = 300),
                data = test_data,
                family = gaussian(),
                method = 'REML')
dharma_object = DHARMa::simulateResiduals(mod)
test_data$dharma_residuals <- residuals(dharma_object)
#test_data <- sf::st_as_sf(test_data) # may not need
#neighbors <- spdep::poly2nb(test_data, queen = TRUE)
weights <- spdep::nb2listw(neighbors, zero.policy = TRUE)
ggplot(test_data) + geom_sf(aes(geometry = geometry, fill = dharma_residuals))

                                                                   # 0.76 (value w no spline on lat long)
morans35 <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.66
morans45 <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.62
morans55 <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.61
morans85 <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.61
morans105 <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.49
morans125 <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.44
morans200 <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.32
morans300 <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.18 
moransSos50 <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.73 - isotropic smooth on the sphere
moransSos100 <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.73
moransMRFS <- spdep::moran.test(test_data$dharma_residuals, weights) # 
moransSpopdens <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.64

moransSpopdens <- spdep::moran.test(test_data$dharma_residuals, weights) # 0.64

preds = extract_preds(obs_seg_values = test_data$segValue,
                                cb = seg.cb, 
                                mod = mod,
                                model_path = model_path,
                                model_name = 'meanPM2.5_raceEduICE_hsd10_1940_Rural_TEST',
                                linear = '3df')
plot300 = pred_plot_mediancentered(prediction_dataframe = preds, 
                                           orig_seg_values = test_data$segValue,
                                           env_exp_label = 'PM2.5',
                                           seg_label = 'ICE',
                                           year = '1940',
                                           rural_label = 'Rural',
                                           model_name = 'meanPM2.5_raceEduICE_hsd10_1940_Rural_TEST',
                                           plot_path = plot_path)
plot300

## DECISION w JC ON DEC 16, 2023: use s(lat, lon, bs = 'tp'), include in supplement a table of all moran's i values, and maps for worst
# pm2.5 and oil/gas well residuals. decided to go ahead with the tensor plate because resolution of pm2.5 is the real problem, and oil/gas
# wells residuals looked fairly good on the map. add sentence to limitations about resolution of pm2.5 data. also do moran's i of
# randall martin model for comparison sake to confirm our hypothesis that the pm2.5 problem is the resolution of the grid used for the predictions
# otherwise we're gonna stop messing with this, and not worry about the other diagnostic issues from plots.

## SECOND DECISION w JC on MAY 22, 2024: we will go with these models because our sensitivity analyses, knowledge of the data structures,
# and testing with transformation and removing outliers reassures us. However, we will be conservative in our interpretation and only
# make claims about consistently strong associations across models and sensitivity analyses, given that some of the model diagnostic 
# plots show remaining heteroskedasticity

