# Clean 2010 Socio-demographic Data
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 08/29/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create ICE Education and Race Var 
# 2: Create ICE Employment and Race Var
# 3: Create ICE race var
# 4: Calculate Dissimilarity Index
# 5: Merge Metrics
# 6: Check correlations between vars
# 7: Save datasets

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we tidy the 2010 sociodemographic data from the 2010 Census. 
# We also restrict to the contiguous US, create unique merging
# variables, and run correlations. We calculate the Dissimilarity Index and 
# create several options of ICE variables. 

# Nb Index of Concentration at the Extremes Calculation
# ICEi = (Ai-Pi)/Ti
# where, say, in the case of the ICE for income,
# Ai is equal to the number of affluent persons in unit of analysis i 
# (e.g., in the 80th income percentile), 
# Pi is equal to the number of poor persons in unit of analysis i 
# (e.g., in the 20th income percentile), 
# Ti is equal to the total population with known income in unit of analysis i
#   NOTE:
#   TO MAKE IT EASIER TO COMPARE BETWEEN ICE AND THE DI, I HAVE FLIPPED THE ICE
#   CALCULATION SUCH THAT +1 CORRESPONDS TO THE LESS PRIVILEGED GROUP, EG:
#   ICEi = (Pi-Ai)/Ti

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages & clean environment
source(paste0(project.folder, 'scripts/packages.R'))
rm(list.of.packages, new.packages)

# 0c Set up filepath(s)
sociodemo_data_path <- paste0(project.folder, 'data/county_sociodemo_data/')
seg_data_path <- paste0(project.folder, 'data/county_seg_data/')

# 0d Load data
# 0d.i Load education by race data
edByRace2010 <- read_csv(paste0(sociodemo_data_path, 
                                'edBySexByRaceEth_2010_county.csv')) %>% 
  janitor::clean_names() %>%
  dplyr::select(gisjoin, year, state, statea, county, countya, name_e,
                j16e001, j16e003, j16e008,
                j2ce001, j2ce004, j2ce005, j2ce006, j2ce009, j2ce010, j2ce011)
# 0d.ii Load employment by race data
empByRace2010 <- read_csv(paste0(sociodemo_data_path, 
                                 'empBySexByAgeByRaceEth_2010_county.csv')) %>% 
  janitor::clean_names() %>% 
  dplyr::select(gisjoin, j6se004, j6se008, j6se011, j6se013, j6se017, j6se021, j6se024, j6se026,
                j6ye004, j6ye005, j6ye007, j6ye011, j6ye012, j6ye017, j6ye018, j6ye020, j6ye024, j6ye025)
# 0d.iii Load race data (county)
race2010 <- read_csv(paste0(sociodemo_data_path, 
                            'raceEth_2010_county.csv')) %>% 
  janitor::clean_names() %>% 
  dplyr::select(gisjoin, h7z001, h7z003, h7z004, h7z011, h7z012)
# 0d. iv Load race data (census tract)
race2010_ct <- read_csv(paste0(sociodemo_data_path,
                               'raceEth_2010_tract.csv')) %>% 
  janitor::clean_names() %>%   
  dplyr::select(gisjoin, state, county, h7z001, h7z003, h7z004, h7z011, h7z012)
# 0d.v Load total population data
#      Will not use in this script, but loaded for comparisons and to evaluate 
#        missing if needed.
pop2010 <- read_csv(paste0(sociodemo_data_path, 
                           'totPop_2010_county.csv')) %>% 
  janitor::clean_names() %>% 
  dplyr::select(gisjoin, h7v001) %>% rename(totPop = h7v001)
# 0d.vi Load pop 25 yrs and older data
pop252010 <- read_csv(paste0(sociodemo_data_path,
                             'pop25_2010_county.csv')) %>% 
  janitor::clean_names() %>% 
  dplyr::select(gisjoin, jn9e001) %>% rename(totPop_25 = jn9e001)

####******************************************
#### 1: Create ICE Education and Race Var #### 
####******************************************

# Data from 2006-2010 ACS 5-year Estimates

# Groups are: Black or African American alone 25 yrs or older w less than high
#               school diploma (all ethnicity)
#             White alone, not Hispanic or Latino 25 yrs or older w a high
#               school diploma (raceEduICE_2010) or w some college (raceEduICE2_2010)
#             1940: 8th grade was the cutoff used, denominator is 25+
#             2010: tried two cutoffs (HSG, SomeCol), denominator is 25+
#             In var names, LTHS refers to less than high school,
#               HS or HSG refers to high school diploma/graduate, and 
#               SomeCol refers to having completed some college or a Bachelors
#             Denominator is population of people of any race 25 or older

# 1a Rename vars
edByRace2010_clean <- edByRace2010 %>% 
  rename(totPop_black25 = j16e001,
         maleLTHS_black = j16e003,
         femaleLTHS_black = j16e008,
         totPop_white25 = j2ce001,
         maleHS_white = j2ce004,
         maleSomeCol_white = j2ce005,
         maleBDG_white = j2ce006,
         femaleHS_white = j2ce009,
         femaleSomeCol_white = j2ce010,
         femaleBDG_white = j2ce011)

# 1b Sum variables to determine two groups for ICE
edByRace2010_clean <- edByRace2010_clean %>% 
  mutate(LTHS_black = maleLTHS_black + femaleLTHS_black,
         HSG_white  = maleHS_white + maleSomeCol_white + maleBDG_white +
                      femaleHS_white + femaleSomeCol_white + femaleBDG_white,
         SomeCol_white = maleSomeCol_white + maleBDG_white +
                         femaleSomeCol_white + femaleBDG_white)

# 1c Merge denominator (total pop 25 or older of any race) with ed by race data
edByRace2010_clean <- edByRace2010_clean %>% 
  full_join(pop252010, by = 'gisjoin')

# 1d Calculate ICE variable
edByRace2010_clean <- edByRace2010_clean %>% 
  mutate(raceEduICE_2010 = (LTHS_black - HSG_white) / (totPop_25),
         raceEduICE2_2010 = (LTHS_black - SomeCol_white) / (totPop_25))

# 1e Review distribution of ICE variable
edByRace2010_clean %>% ggplot(aes(x = raceEduICE_2010)) + geom_histogram()  
edByRace2010_clean %>% ggplot(aes(x = raceEduICE2_2010)) + geom_histogram()
summary(edByRace2010_clean$raceEduICE_2010)
summary(edByRace2010_clean$raceEduICE2_2010)

# 1f Keep only needed variables
edByRace2010_clean <- edByRace2010_clean %>% 
  dplyr::select(gisjoin, year, state, statea, county, countya, name_e,  
                raceEduICE_2010, raceEduICE2_2010)

####*******************************************
#### 2: Create ICE Employment and Race Var #### 
####*******************************************

# Data from 2006-2010 ACS 5-year Estimates

# Groups are: Black or African American alone in labor force & unemployed (16+)
#             White alone, not Hispanic or Latino employed or in armed forces (16+)
# 1940: Removed people not in labor force, include armed forces and all people
#       16 plus in the labor force as denominator; armed forces counts as
#       employment
# In var names, 16t64 refers to ages 16 to 64, 65P refers to ages 65 and older
#    Unemp refers to civilian unemployed, AF refers to armed forces, Emp refers to employed
#    Emp refers to civilian employed

# 2a Rename vars
empByRace2010_clean <- empByRace2010 %>% 
  rename(maleLaborForce_black16t64 = j6se004,
         maleLaborForce_black65P = j6se011,
         femaleLaborForce_black16t64 = j6se017,
         femaleLaborForce_black65P = j6se024,
         maleUnemp_black16t64 = j6se008,
         maleUnemp_black65P = j6se013,
         femaleUnemp_black16t64 = j6se021,
         femaleUnemp_black65P = j6se026,
         maleLaborForce_white16t64 = j6ye004,
         maleLaborForce_white65P = j6ye011,
         femaleLaborForce_white16t64 = j6ye017,
         femaleLaborForce_white65P = j6ye024,
         maleAF_white16t64 = j6ye005,
         maleEmp_white16t64 = j6ye007,
         maleEmp_white65P = j6ye012,
         femaleAF_white16t64 = j6ye018,
         femaleEmp_white16t64 = j6ye020,
         femaleEmp_white65P = j6ye025)

# 2b Sum variables to determine two groups for ICE
#    Note: This denominator is INCORRECT. The denominator should be ALL people
#          aged 16 and older, not only those who are Black alone or White alone Non-Hispanic
#          This has not been corrected because this variable was not used in analysis
empByRace2010_clean <- empByRace2010_clean %>% 
  mutate(totPopLaborForce_16P = maleLaborForce_black16t64 + maleLaborForce_black65P 
                                + femaleLaborForce_black16t64 + femaleLaborForce_black65P 
                                + maleLaborForce_white16t64 + maleLaborForce_white65P
                                + femaleLaborForce_white16t64 + femaleLaborForce_white65P,
         unemp_black          = maleUnemp_black16t64 + maleUnemp_black65P
                                + femaleUnemp_black16t64 + femaleUnemp_black65P,
         emp_white            = maleAF_white16t64 + maleEmp_white16t64 + maleEmp_white65P 
                                + femaleAF_white16t64 + femaleEmp_white16t64 + femaleEmp_white65P)

# 2c Calculate ICE variable
empByRace2010_clean <- empByRace2010_clean %>% 
  mutate(raceEmpICE_2010 = (unemp_black - emp_white) / (totPopLaborForce_16P))

# 2d Review distribution of ICE variable
#    Notes:  Almost no counties have values over 0; max is 0.39
#            Will not use in analysis because of tight distribution
empByRace2010_clean %>% ggplot(aes(x = raceEmpICE_2010)) + geom_histogram()  
summary(empByRace2010_clean$raceEmpICE_2010)

# 2e Keep only needed variables
empByRace2010_clean <- empByRace2010_clean %>% 
  dplyr::select(gisjoin, raceEmpICE_2010)

####****************************
#### 3: Create ICE race var #### 
####****************************

# Data from 2010 Decennial Census

# Groups are: Black or African American alone (Hispanic & not Hispanic or Latino) 
#             White alone, not Hispanic or Latino
# 1940 data do not consider Hispanic or Latino ethnicity at all for either race,
# but Taylor and Jenni decided to use NH-white and Black (all ethnicity) in 2010
# because we felt white (all ethnicity) didn't adequately capture 2010 systems of
# oppression

# 3a Rename vars
race2010_clean <- race2010 %>% 
  rename(totPop = h7z001,
         nhblack = h7z004,
         hblack= h7z012,
         nhwhite = h7z003,
         hwhite = h7z011)

# 3b Sum non-hispanic and hispanic Black population variables 
race2010_clean <- race2010_clean %>% 
  mutate(black = nhblack + hblack)
    
# 3c Calculate ICE variable
race2010_clean <- race2010_clean %>% 
  mutate(raceEthICE_2010 = (black - nhwhite) / (totPop))

# 3d Review distribution of ICE variable
race2010_clean %>% ggplot(aes(x = raceEthICE_2010)) + geom_histogram() 

# 3e Keep only needed variables
race2010_clean <- race2010_clean %>% 
  dplyr::select(gisjoin, raceEthICE_2010, black, nhwhite, totPop)

####**************************************
#### 4: Calculate Dissimilarity Index #### 
####**************************************

# Data from 2010 Decennial Census

# Groups are: Black or African American alone (Hispanic & not Hispanic or Latino) 
#             White alone, not Hispanic or Latino

# 4a Rename vars
race2010_ct_clean <- race2010_ct %>% 
  rename(totPop = h7z001,
         nhblack = h7z004,
         hblack= h7z012,
         nhwhite = h7z003,
         hwhite = h7z011)

# 4b Sum variables to determine total black and white regardless of ethnicity
race2010_ct_clean <- race2010_ct_clean %>% 
  mutate(black = nhblack + hblack,
         white = nhwhite + hwhite)

# 4c Calculate county level race variables
race2010_ct_clean <- race2010_ct_clean %>% 
  group_by(state, county) %>% 
  mutate(black_county = sum(black, na.rm = T),
         nhwhite_county = sum(nhwhite, na.rm = T)) %>% 
  ungroup()

# 4d Calculate dissimilarity index
race2010_ct_clean <- race2010_ct_clean %>% 
  mutate(diS1 = black / black_county,
         diS2 = nhwhite / nhwhite_county,
         diS3 = abs(diS1 - diS2)) %>% 
  group_by(state, county) %>% 
  summarise(di_2010 = sum(diS3)*.5) %>% 
  ungroup()

# 4e Review distribution
race2010_ct_clean %>% ggplot(aes(di_2010)) + geom_histogram() 
summary(race2010_ct_clean$di_2010)

####**********************
#### 5: Merge metrics #### 
####**********************

# 5a Merge education, home value, and employment metrics
seg2010 <- edByRace2010_clean %>% 
  full_join(empByRace2010_clean, by = 'gisjoin') %>% 
  full_join(race2010_clean, by = 'gisjoin') %>% 
  full_join(race2010_ct_clean, by = c('state', 'county')) 

# 5b Remove Alaska, Hawaii, Puerto Rico
seg2010 <- seg2010 %>% filter(!state %in% c('Alaska', 'Hawaii', 'Puerto Rico')) 

# 5c Review missingness (n = 3221 counties)
#    Notes: n = 27 counties missing DI; all have a Black population of 0 and
#           will be removed from regression analysis
summary(seg2010)
missing <- seg2010 %>% filter(!complete.cases(.))

# 5d Create merge variables
seg2010 <- seg2010 %>% 
  mutate(year = '2010',
         county = str_replace(county, ' County', ''),
         stateCountyString = tolower(paste0(state, county)),
         stateCountyString = gsub('[[:punct:]]+', '', stateCountyString),
         stateCountyString = str_replace_all(stateCountyString, ' ', ''),
         nhgis_2010 = str_replace(gisjoin, 'G', ''),
         stctya_2010 = paste0(statea, countya))

# 5e Keep only needed vars and rename vars to include year
#    Note: raceEmpIce_2010 excluded here because the denominator is incorrect
seg2010 <- seg2010 %>% 
  rename(statea_2010 = statea,
         countya_2010 = countya,
         blackPop_2010 = black,
         nhwhitePop_2010 = nhwhite,
         totPop_2010 = totPop) %>% 
  dplyr::select(-state, -county, -name_e, -raceEmpICE_2010)

####****************************************
#### 6: Check correlations between vars #### 
####****************************************

# 6a Prepare dataframe for correlations
cor_data <- seg2010 %>% 
  filter(!is.na(di_2010)) %>% 
  dplyr::select(raceEduICE_2010:di_2010) 

# 6b Run correlations
#    Notes: Use Spearman because vars not normally distributed
cor <- cor(cor_data, method = c('spearman'),
           use = 'complete.obs')

# 6c Plot
corrplot(cor, method = 'circle', type = 'lower', order = 'alphabet', cl.cex = 1)

# 6d Review correlation values
tidy_cor <- as.data.frame(cor)

####**********************
#### 7: Save datasets #### 
####**********************

# 7a Save datasets
seg2010 %>% write_fst(paste0(seg_data_path, 'seg_2010_county.fst'))
tidy_cor %>% write_csv(paste0(seg_data_path, 'segCorrs_2010_county.csv'))




