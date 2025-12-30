# Clean 1940 Socio-demographic Data
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 08/28/2025

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation 
# 1: Clean & review education data
# 2: Clean & review home value data
# 3: Clean & review employment data
# 4: Clean & review race data
# 5: Clean & review urbanicity data
# 6: Merge metrics
# 7: Check correlations between vars
# 8: Finalize and save datasets

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we clean and review tabular sociodemographic data from the 1940
# Census. We also restrict to the contiguous US, create unique merging
# variables, and run correlations. We create a race ICE variable from tabular
# data to compare to the enumeration district ICE variable, as a data doublecheck. 

# Nb Index of Concentration at the Extremes Calculation
# ICEi = (Ai-Pi)/Ti
# where, say, in the case of the ICE for income,
# Ai is equal to the number of affluent persons in unit of analysis i 
# (e.g., in the 80th income percentile), 
# Pi is equal to the number of poor persons in unit of analysis i 
# (e.g., in the 20th income percentile), 
# Ti is equal to the total population with known income in unit of analysis i

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & clean environment
source(paste0(project.folder, 'scripts/packages.R'))
rm(list.of.packages, new.packages)

# 0c Set up filepath(s)
sociodemo1940_data_path <- paste0(project.folder, 'data/county_sociodemo_data/')

# 0d Load data
#    Note: Filtered out headers using GIS Join Match Code when present in dataset
ed1940 <- read_csv(paste0(sociodemo1940_data_path, 'edBySex_1940_county.csv'),
                   col_types = list(.default = col_character())) %>% 
  filter(!GISJOIN == 'GIS Join Match Code') %>% janitor::clean_names() %>% 
  mutate_at(vars(contains('bww')), list(as.numeric))
homeValue1940 <- read_csv(paste0(sociodemo1940_data_path, 'medianHomeValue_1940_county.csv')) %>% 
  janitor::clean_names() %>% mutate_at(vars(contains('byp')), list(as.numeric))
employ1940 <- read_csv(paste0(sociodemo1940_data_path, 'popEmployed_1940_county.csv')) %>% 
  janitor::clean_names() %>% mutate_at(vars(contains('bw7')), list(as.numeric))
percEmploy1940 <- read_csv(paste0(sociodemo1940_data_path, 'percEmployedBySex_1940_county.csv')) %>% 
  janitor::clean_names() %>% mutate_at(vars(contains('bw8')), list(as.numeric))
race1940 <- read_csv(paste0(sociodemo1940_data_path, 'raceByNativity_1940_county.csv'),
                     col_types = list(.default = col_character())) %>% 
  filter(!GISJOIN == 'GIS Join Match Code') %>% janitor::clean_names() %>% 
  mutate_at(vars(contains('bya')), list(as.numeric))
urban1940 <- read_csv(paste0(sociodemo1940_data_path, 'urbanicity_1940_county.csv')) %>% 
  janitor::clean_names() %>% mutate_at(vars(contains(c('bw', 'bx'))), list(as.numeric))
pop1940 <- read_csv(paste0(sociodemo1940_data_path, 'totPop_1940_county.csv')) %>% 
  janitor::clean_names() %>% mutate_at(vars(contains('bvu')), list(as.numeric))

####**************************************
#### 1: Clean & review education data #### 
####**************************************

# 1a Create percent w 4 years of HS or more variables
ed1940_clean <- ed1940 %>% 
  mutate(male_lessThanHS4yrs   = bww001 + bww002 + bww003 + bww004 + bww005,
         female_lessThanHS4yrs = bww010 + bww011 + bww012 + bww013 + bww014,
         all_lessThanHS4yrs    = male_lessThanHS4yrs + female_lessThanHS4yrs,
         male_HS4yrsPlus       = bww006 + bww007 + bww008,
         female_HS4yrsPlus     = bww015 + bww016 + bww017,
         all_HS4yrsPlus        = male_HS4yrsPlus + female_HS4yrsPlus,
         male_25PlusPop        = male_lessThanHS4yrs + male_HS4yrsPlus + bww009,
         female_25PlusPop      = female_lessThanHS4yrs + female_HS4yrsPlus + bww018,
         all_25PlusPop         = male_25PlusPop + female_25PlusPop,
         male_perHS4yrsPlus    = male_HS4yrsPlus / male_25PlusPop * 100,
         female_perHS4yrsPlus  = female_HS4yrsPlus / female_25PlusPop * 100,
         all_perHS4yrsPlus     = all_HS4yrsPlus / all_25PlusPop * 100)

# 1b Review distribution of key variables
#    Notes: Median per w 4 yrs of HS or more is ~20%, for male ~18% and for female ~24%
ed1940_clean %>% ggplot(aes(x = all_perHS4yrsPlus)) + geom_histogram() + 
  geom_vline(aes(xintercept = median(all_perHS4yrsPlus, na.rm = T)))
ed1940_clean %>% ggplot(aes(x = male_perHS4yrsPlus)) + geom_histogram() + 
  geom_vline(aes(xintercept = median(male_perHS4yrsPlus, na.rm = T)))
ed1940_clean %>% ggplot(aes(x = female_perHS4yrsPlus)) + geom_histogram() +
  geom_vline(aes(xintercept = median(female_perHS4yrsPlus, na.rm = T)))

# 1c Keep only needed variables
ed1940_clean <- ed1940_clean %>% 
  dplyr::select(gisjoin, year, state, statea, county, countya, stateicp, countyicp,  
                contains('all'), contains('male'), contains('female'))

####***************************************
#### 2: Clean & review home value data #### 
####***************************************

# 2a Rename median home value variable
homeValue1940_clean <- homeValue1940 %>% 
  rename(medHomeValue = byp001)

# 2b Review distribution
#    Notes: Median median home value is $1,448
homeValue1940_clean %>% ggplot(aes(x = medHomeValue)) + geom_histogram() + 
  geom_vline(aes(xintercept = median(medHomeValue, na.rm = T)))
summary(homeValue1940_clean$medHomeValue)

# 2c Keep only needed variables
homeValue1940_clean <- homeValue1940_clean %>% 
  dplyr::select(gisjoin, medHomeValue)

####***************************************
#### 3: Clean & review employment data #### 
####***************************************

# 3a Rename employment vars
employ1940_clean <- employ1940 %>% 
  mutate(male_employ       = bw7001,
         female_employ     = bw7002,
         all_employ        = male_employ + female_employ)

# 3b Clean population vars
perEmploy1940_clean <- percEmploy1940 %>% 
  mutate(male_perEmploy   = bw8001/100,
         female_perEmploy = bw8002/100) 

# 3c Merge population with employment
perEmploy1940_clean <- perEmploy1940_clean %>% 
  full_join(employ1940_clean, by = 'gisjoin')

# 3d Calculate percent employed by sex and total
#    Note: First we need to back calculate the total pop aged 14+
perEmploy1940_clean <- perEmploy1940_clean %>% 
  mutate(male_14PlusPop   = male_employ/male_perEmploy,
         female_14PlusPop = female_employ/female_perEmploy,
         all_14PlusPop    = male_14PlusPop + female_14PlusPop,
         all_perEmploy    = ((male_employ + female_employ) / all_14PlusPop) *100)

# 3e Review distribution
#    Notes: Median per employ is 49.3%, for male 79.5% and for female 17.2%
perEmploy1940_clean %>% ggplot(aes(x = all_perEmploy)) + geom_histogram() + 
  geom_vline(aes(xintercept = median(all_perEmploy, na.rm = T)))
perEmploy1940_clean %>% ggplot(aes(x = male_perEmploy)) + geom_histogram() + 
  geom_vline(aes(xintercept = median(male_perEmploy, na.rm = T)))
perEmploy1940_clean %>% ggplot(aes(x = female_perEmploy)) + geom_histogram() + 
  geom_vline(aes(xintercept = median(female_perEmploy, na.rm = T)))
summary(perEmploy1940_clean$all_perEmploy)
summary(perEmploy1940_clean$male_perEmploy)
summary(perEmploy1940_clean$female_perEmploy)

# 3f Keep only needed variables
perEmploy1940_clean <- perEmploy1940_clean %>% 
  dplyr::select(gisjoin, contains('all'), contains('male'), contains('female'))

####*********************************
#### 4: Clean & review race data #### 
####*********************************

# 4a Rename vars and calculate ICE
ice1940_clean <- race1940 %>% 
  mutate(white_tab       = bya001 + bya002,
         black_tab       = bya003,
         total_tab       = white_tab + black_tab + bya004,
         icebw_tab       = (white_tab - black_tab) / total_tab)

# 4b Review distribution
#    Notes: Median median home value is $1,448
ice1940_clean %>% ggplot(aes(x = icebw_tab)) + geom_histogram() + 
  geom_vline(aes(xintercept = median(icebw_tab, na.rm = T)))
summary(ice1940_clean$icebw_tab)

# 4c Keep only needed variables
ice1940_clean <- ice1940_clean %>% 
  dplyr::select(gisjoin, icebw_tab, white_tab, black_tab, total_tab)

####***************************************
#### 5: Clean & review urbanicity data #### 
####***************************************

# ACS and Geography Brief "Defining Rural at the US Census Bureau" Ratcliffe et al 2016
# https://www.researchgate.net/profile/Michael-Ratcliffe-2/publication/311533270_Defining_Rural_at_the_US_Census_Bureau/links/584aad3708aeb19dcb758910/Defining-Rural-at-the-US-Census-Bureau.pdf
# We decided to use a different measure of urbanicity, and so do not keep this 
# data in the output dataset. 

# 5a Rename vars
urban1940_clean <- urban1940 %>% 
  rename(urbPop = bw1001,
         popCit25k = bxd001,
         perUrbPop = bxq001,
         popDensSqMile = bwr001)

# 5b Review distribution
#    Note: perUrbPop ranges up to 1000 (instead of 100) - use total pop var from   
#          pop1940 dataframe to calculate percent
urban1940_clean %>% ggplot(aes(y = urbPop)) + geom_boxplot() 
summary(urban1940_clean$urbPop)
urban1940_clean %>% ggplot(aes(y = popCit25k)) + geom_boxplot() 
summary(urban1940_clean$popCit25k)
urban1940_clean %>% ggplot(aes(y = perUrbPop)) + geom_boxplot()
summary(urban1940_clean$perUrbPop)
urban1940_clean %>% ggplot(aes(y = popDensSqMile)) + geom_boxplot()
summary(urban1940_clean$popDensSqMile)

# 5c Add population variable and calculate percent urban
# 5c.i Merge tot pop and urbanicity data
urban1940_clean <- urban1940_clean %>% 
  left_join(pop1940) %>% rename(totPop = bvu001)
# 5c.ii Create new percent urban var
urban1940_clean <- urban1940_clean %>% 
  mutate(perUrbPop2 = (urbPop/totPop)*100)
# 5c.iii Review distribution
urban1940_clean %>% ggplot(aes(y = perUrbPop2)) + geom_boxplot()
urban1940_clean %>% ggplot(aes(x = perUrbPop2)) + geom_histogram()
summary(urban1940_clean$perUrbPop2)
# 5c.iv Review correlation (cor = 0.997)
cor(urban1940_clean$perUrbPop, urban1940_clean$perUrbPop2, use = 'pairwise.complete.obs')

# 5d Keep only needed variables
urban1940_clean <- urban1940_clean %>% dplyr::select(gisjoin, urbPop, popCit25k,
                                                     popDensSqMile, totPop, perUrbPop2)

####**********************
#### 6: Merge metrics #### 
####**********************

# 6a Merge education, home value, and employment metrics
sociodemo1940 <- ed1940_clean %>% 
  full_join(homeValue1940_clean, by = 'gisjoin') %>% 
  full_join(perEmploy1940_clean, by = 'gisjoin') %>% 
  full_join(ice1940_clean, by = 'gisjoin')

# 6b Review missingness
#    Notes: -n = 1 county missing all sociodemo data -- Yellowstone National Park in 
#            Idaho id 1600875
#           -n = 9 counties missing ICE data (Yellowstone in Idaho and all of 
#            Alaska and Hawaii)
#           -n = 70 counties missing median home value data
#           -n = 3,108 unique counties
summary(sociodemo1940)
missing <- sociodemo1940 %>% filter(!complete.cases(.))

# 6c Remove Alaska and Hawaii because they will not be used in analysis
#    n will be 3,100
sociodemo1940 <- sociodemo1940 %>% 
  filter(!state %in% c('Alaska Territory', 'Hawaii Territory')) 

####****************************************
#### 7: Check correlations between vars #### 
####****************************************

# The three sociodemographic variables are weak to moderately correlated, 0.21-0.47
# Will check correlation of ICE var later when other segregation vars are merged

# 7a Prepare dataframe for correlations
cor_data <- sociodemo1940 %>% 
  filter(!is.na(female_14PlusPop)) %>% 
  dplyr::select(all_perHS4yrsPlus, medHomeValue, all_perEmploy) 

# 7b Run correlations
#    Notes: Use Spearman because vars not normally distributed
cor <- cor(cor_data, method = c('spearman'),
           use = 'complete.obs')

# 7c Plot
corrplot(cor, method = 'circle', type = 'lower', order = 'alphabet', cl.cex = 1)

# 7d Identify correlations >= 0.8 or <= 0.8
tidy_cor <- cor %>% as.data.frame() %>% 
  rownames_to_column(var = 'sociodemo1') %>% 
  as_tibble() %>% 
  pivot_longer(cols = all_perHS4yrsPlus:all_perEmploy, names_to = 'sociodemo2', values_to = 'corr') %>% 
  filter(!sociodemo1 == sociodemo2) %>% 
  mutate(high_corr = ifelse(corr >= 0.8 & corr < 1, 1, 0),
         low_corr  = ifelse(corr <= -0.8, 1, 0))

####***********************************
#### 8: Finalize and save datasets #### 
####***********************************

# 8a Rename key variables
sociodemo1940 <- sociodemo1940 %>% 
  rename(perEmploy = all_perEmploy,
         perHS4yrsPlus = all_perHS4yrsPlus,
         black = black_tab,
         totPop = total_tab)

# 8b Clean unique id vars for merging later
#    Notes: statea is equivalent to state fips codes with a 0 added to the end
#             for MOST states: e.g., Alabama statea is 010 and Alabama fips is 01.
#             Exceptions: Alaska statea is 025, fips is 02
#                         Hawaii statea is 155, fips is 15
#             NHGIS stated in email:
#               statea and countya are codes NHGIS created and are based on fips, 
#               but with an additional character at the end to account for spatial units
#               that no longer exist. If the value ends in 0 the spatial unit still
#               exists. If it ends in a value other than 0, the spatial unit existed
#               at the time of that census but no longer today
#           countyicp is INCORRECT for:
#             Pershing Nevada - Listed as county fips plus trailing 0 (0270) when 
#               it should be 0250
#             Ormsby Nevada (historical) - Listed as county fips plus trailing 0 
#               (0250) when it should be 0510
#             Union Oregon - Listed as 0610 when it should be 0605
#           In NHGIS documentation online, gisjoin is explicitly defined to be
#             the state-county NHGIS codes with a 'G' added in front, so I will
#             use this variable to make my join 'jnhgis40' variable
sociodemo1940 <- sociodemo1940 %>% 
  mutate(countyicp = as.character(countyicp),
         countyicp = str_pad(countyicp, 4, pad = '0', side = c('left')),
         countyicp = ifelse(county == 'Pershing' & state == 'Nevada', '0250', countyicp),
         countyicp = ifelse(county == 'Ormsby' & state == 'Nevada', '0510', countyicp),
         countyicp = ifelse(county == 'Union' & state == 'Oregon', '0605', countyicp)) %>% 
  mutate(stateCountyString = tolower(paste0(state, county)),
         stateCountyString = gsub('[[:punct:]]+', '', stateCountyString),
         stateCountyString = str_replace_all(stateCountyString, ' ', '')) %>% 
  mutate(jnhgis40 = str_replace(gisjoin, 'G', '')) %>% 
  rename(countyicp_1940 = countyicp,
         stateicp_1940 = stateicp,
         countya_1940 = countya,
         statea_1940 = statea) %>% 
  dplyr::select(-state, -county)

# 8c Save datasets
sociodemo1940 %>% write_fst(paste0(sociodemo1940_data_path, 'sociodemographic_1940_county.fst'))
tidy_cor %>% write_csv(paste0(sociodemo1940_data_path, 'sociodemographicCorrs_1940_county.csv'))

