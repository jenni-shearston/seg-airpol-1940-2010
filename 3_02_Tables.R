# Tables
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 12/02/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Table 1 - Env and Seg Vars Stratified by Year and Rurality  
# 2: Table S5 - Effect estimates & 95% CI for Consistently Highly Segregated
# 3: Tables S2-S4 - Effect estimates & 95% CI for seg & air pollution
# 4: Table S1 - Moran's i of model residuals

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we prepare tables for the manuscript and supplement, such that 
# they can be exported as csv files and then copy/pasted into the word docs.
# Of note, Table 2 in the manuscript does not need prepartion in R; it is qualitative.

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
final_table_path <- paste0(project.folder, 'outputs/final_tables/')

# 0d Load all env exp, seg, sociodemo data
data <- read_fst(paste0(merged_data_path, 'env_seg_sociodemo_allyears.fst'))

# 0e Load all regressions, predictions, and plots
regs_cbNS <- read_rds(paste0(model_path, 'regs_plots_cbNS.RDS'))
regs_persmarg <- read_rds(paste0(model_path, 'regs_persmarg.RDS'))
regs_persmarg_strat <- read_rds(paste0(model_path, 'regs_strat_persmarg.RDS'))
regs_log10nox <- read_rds(paste0(model_path, 'regs_plots_cbNS_log10nox.RDS'))

# 0f Load moran's i values
morans <- read_rds(paste0(model_path, 'moran_wTP_oct25.RDS'))

####********************************************************************
#### 1: Table 1 - Env and Seg Vars Stratified by Year and Rurality  #### 
####********************************************************************

# 1a Summarise mean and SDs by year and rurality
t1_rurality <- data %>% 
  dplyr::select(year, plural_bin_imp, 
                fospp_count, ogw_count, meanPM2.5, nox_autos, 
                di, raceEduICE_hsd10, nbrOne) %>% 
  rename(pm25 = meanPM2.5) %>% 
  group_by(year, plural_bin_imp) %>% 
  summarise(fosppCount_mean      = mean(fospp_count, na.rm = T),
            fosppCount_sd        = sd(fospp_count, na.rm = T),
            ogwCount_mean        = mean(ogw_count, na.rm = T),
            ogwCount_sd          = sd(ogw_count, na.rm = T),
            pm25_mean            = mean(pm25, na.rm = T),
            pm25_sd              = sd(pm25, na.rm = T),
            noxAutos_mean         = mean(nox_autos, na.rm = T),
            noxAutos_sd           = sd(nox_autos, na.rm = T),
            di_mean              = mean(di, na.rm = T),
            di_sd                = sd(di, na.rm = T),
            raceEduICEhsd10_mean = mean(raceEduICE_hsd10, na.rm = T),
            raceEduICEhsd10_sd   = sd(raceEduICE_hsd10, na.rm = T),
            nbrOne_mean          = mean(nbrOne, na.rm = T),
            nbrOne_sd            = sd(nbrOne, na.rm = T))

# 1a Format rurality stratified columns to match table 1 
t1_rurality_formatted <- t1_rurality %>% 
  ungroup() %>% 
  pivot_longer(cols = c(fosppCount_mean:nbrOne_sd),
               names_to = 'variable_statistic',
               values_to = 'value') %>% 
  separate(variable_statistic, into = c('variable', 'statistic')) %>% 
  pivot_wider(names_from = statistic,
              values_from = value) %>% 
  filter(!is.na(sd)) %>% 
  mutate(rurality_year = paste0(plural_bin_imp, year)) %>% 
  dplyr::select(-year, -plural_bin_imp) %>% 
  pivot_wider(names_from = rurality_year,
              values_from = c(mean, sd)) %>% 
  mutate(absolute_change_urban = mean_Urban2010 - mean_Urban1940,
         absolute_change_rural = mean_Rural2010 - mean_Rural1940) %>% 
  mutate(percent_change_urban = (absolute_change_urban / abs(mean_Urban1940))*100,
         percent_change_rural = (absolute_change_rural / abs(mean_Rural1940))*100) %>% 
  mutate(across(mean_Rural1940:percent_change_rural, ~ round(.x, digits = 1)),
         across(c(percent_change_urban, percent_change_rural), ~ round(.x, digits = 0)),
         across(mean_Rural1940:percent_change_rural, ~ prettyNum(.x, big.mark = ','))) %>% 
  mutate(mean_sd_Urban1940 = paste0(mean_Urban1940, ' (', sd_Urban1940, ')'),
         mean_sd_Urban2010 = paste0(mean_Urban2010, ' (', sd_Urban2010, ')'),
         mean_sd_Rural1940 = paste0(mean_Rural1940, ' (', sd_Rural1940, ')'),
         mean_sd_Rural2010 = paste0(mean_Rural2010, ' (', sd_Rural2010, ')')) %>% 
  dplyr::select(variable, 
                mean_sd_Urban1940, mean_sd_Urban2010, absolute_change_urban, percent_change_urban,
                mean_sd_Rural1940, mean_sd_Rural2010, absolute_change_rural, percent_change_rural)
  
# 1c Summarise mean and SDs by year
t1_all <- data %>% 
  dplyr::select(year, 
                fospp_count, ogw_count, meanPM2.5, nox_autos, 
                di, raceEduICE_hsd10, nbrOne) %>% 
  rename(pm25 = meanPM2.5) %>% 
  group_by(year) %>% 
  summarise(fosppCount_mean      = mean(fospp_count, na.rm = T),
            fosppCount_sd        = sd(fospp_count, na.rm = T),
            ogwCount_mean        = mean(ogw_count, na.rm = T),
            ogwCount_sd          = sd(ogw_count, na.rm = T),
            pm25_mean            = mean(pm25, na.rm = T),
            pm25_sd              = sd(pm25, na.rm = T),
            noxAutos_mean        = mean(nox_autos, na.rm = T),
            noxAutos_sd          = sd(nox_autos, na.rm = T),
            di_mean              = mean(di, na.rm = T),
            di_sd                = sd(di, na.rm = T),
            raceEduICEhsd10_mean = mean(raceEduICE_hsd10, na.rm = T),
            raceEduICEhsd10_sd   = sd(raceEduICE_hsd10, na.rm = T),
            nbrOne_mean          = mean(nbrOne, na.rm = T),
            nbrOne_sd            = sd(nbrOne, na.rm = T))

# 1d Format columns to match table 1
t1_all_formatted <- t1_all %>% 
  ungroup() %>% 
  pivot_longer(cols = c(fosppCount_mean:nbrOne_sd),
               names_to = 'variable_statistic',
               values_to = 'value') %>% 
  separate(variable_statistic, into = c('variable', 'statistic')) %>% 
  pivot_wider(names_from = statistic,
              values_from = value) %>% 
  filter(!is.na(sd)) %>% 
  pivot_wider(names_from = year,
              values_from = c(mean, sd)) %>% 
  mutate(absolute_change = mean_2010 - mean_1940) %>% 
  mutate(percent_change = (absolute_change / abs(mean_1940))*100) %>% 
  mutate(across(mean_1940:absolute_change, ~ round(.x, digits = 1)),
         percent_change = round(percent_change, digits = 0),
         across(mean_1940:percent_change, ~ prettyNum(.x, big.mark = ','))) %>% 
  mutate(mean_sd_1940 = paste0(mean_1940, ' (', sd_1940, ')'),
         mean_sd_2010 = paste0(mean_2010, ' (', sd_2010, ')')) %>% 
  dplyr::select(mean_sd_1940, mean_sd_2010, absolute_change, percent_change)

# 1e Merge rurality stratified and full data columns to make full table 1
t1 <- t1_rurality_formatted %>% bind_cols(t1_all_formatted)

# 1f Save to csv in order to copy/paste into table
t1 %>% write_csv(paste0(final_table_path, 'table1.csv'))

####*********************************************************************************
#### 2: Table S5 - Effect estimates & 95% CI for Consistently Highly Segregated  #### 
####*********************************************************************************

# 2a Convert data to format of table
ts5 <- regs_persmarg_strat %>% ungroup() %>% 
  dplyr::select(plural_bin_imp, envExpLab, segLab, tidy_mod) %>% unnest(tidy_mod) %>% 
  filter(term == 'segValueTRUE') %>% 
  dplyr::select(-term, -std.error, -statistic, -p.value) %>% 
  mutate(envExpLab = factor(envExpLab, 
                            levels = c('Fossil Fuel Plants (per 1 facility increase)',
                                       'Oil/Gas Wells (per 206 well increase)',
                                       'Log 10 Vehicle NOx (per 0.79 tons/yr increase)',
                                       'PM2.5 (per 3 ug/m^3 increase)'))) %>% 
  arrange(envExpLab, segLab, plural_bin_imp) %>% 
  dplyr::select(envExpLab, segLab, plural_bin_imp, everything()) %>% 
  mutate(across(estimate:conf.high, ~round(.x, digits = 2)),
         conf.int = paste0('(', conf.low, ', ', conf.high, ')')) %>% 
  dplyr::select(-conf.low, -conf.high)
  
# 2b Save as csv so table can be copy/pasted
ts5 %>% write.csv(paste0(final_table_path, 'tableS5.csv'))

####**************************************************************************
#### 3: Tables S2-S4 - Effect estimates & 95% CI for seg & air pollution  #### 
####**************************************************************************

# For linear relationships, use the EE at a seg value of 0.1 (e.g., a per 0.1 unit increase)
# For non-linear relationship, use the EE for an increase in seg from the median to 0.75 value

# 3a Add log10 NOx data to main regression dataframe
reg_res <- regs_log10nox %>%
  mutate(envExpLab = 'Log 10 Vehicle NOx (tons/yr)') %>% 
  bind_rows(regs_cbNS) %>% 
  filter(!envExpLab == "Vehicle NOx (10 tons/yr)") %>% 
  filter(!envExpLab == "Vehicle NOx (tons/yr)")

# 3b Convert data to format of table
ts2to4 <- reg_res %>% ungroup() %>% 
  dplyr::select(constraint, year, plural_bin_imp, envExpLab, segLab, preds) %>% 
  unnest(preds) %>% 
  filter((constraint == 'lin' & label == 'increase0.1') |
           (constraint == '2df' & label == 'MedianPlusP75') |
           (constraint == '3df' & label == 'MedianPlusP75')) %>% 
  filter(!(envExpLab == 'RM PM2.5 (ug/m^3)')) %>% 
  filter(!(str_detect(envExpLab, 'IDW'))) %>% 
  filter(!(str_detect(envExpLab, 'Buffered'))) %>% 
  mutate(linear = ifelse(constraint == 'lin', 'Linear', 'Non-linear'),
         across(effect.est.lag0:uci.lag0, ~ round(.x, digits = 2)),
         envExpLab = factor(envExpLab, 
                            levels = c('Fossil Fuel Plants (per 10 plants)',
                                       'Oil/Gas Wells (per 100 wells)',
                                       'Log 10 Vehicle NOx (tons/yr)',
                                       'PM2.5 (ug/m^3)'))) %>% 
  mutate(conf.int = paste0('(', lci.lag0, ', ', uci.lag0, ')')) %>% 
  dplyr::select(segLab, envExpLab, plural_bin_imp, year, linear, effect.est.lag0, conf.int)

# 3c Separate DI data and re-order for table
ts2 <- ts2to4 %>% 
  filter(segLab == "Dissimilarity Index") %>% 
  filter(!str_detect(envExpLab, 'IDW')) %>% 
  arrange(envExpLab, plural_bin_imp, year)

# 3d Separate ICE data and order for table
ts3 <- ts2to4 %>% 
  filter(segLab == "Black/White + Ed ICE") %>% 
  filter(!str_detect(envExpLab, 'IDW')) %>% 
  arrange(envExpLab, plural_bin_imp, year)

# 3e Separate neighbor-based index data and order for table
ts4 <- ts2to4 %>% 
  filter(segLab == "Neighbor Metric") %>% 
  filter(!str_detect(envExpLab, 'IDW')) %>% 
  arrange(envExpLab, plural_bin_imp, year)

# 3f Save table S2, S3, S4
ts2 %>% write.csv(paste0(final_table_path, 'tableS2_oct25.csv'))
ts3 %>% write.csv(paste0(final_table_path, 'tableS3_oct25.csv'))
ts4 %>% write.csv(paste0(final_table_path, 'tableS4_oct25.csv'))

####*************************************************
#### 4: Table S1 - Moran's i of model residuals  #### 
####*************************************************

# 4a Convert data to format of table
ts1 <- morans %>% 
  filter(!str_detect(modelName, '_rm_')) %>% 
  dplyr::select(envExpLab, segLab, plural_bin_imp, year, moran_stat) %>% 
  mutate(envExpLab = ifelse(envExpLab == "Vehicle NOx (tons/yr)", 
                            "Log 10 Vehicle NOx (tons/yr)", envExpLab)) %>% 
  mutate(pvalue = str_split_i(moran_stat, " p = ", 2),
         moran_stat = str_split_i(moran_stat, " p = ", 1),
         envExpLab = factor(envExpLab, levels = c('Fossil Fuel Plants (per 10 plants)',
                                                  'Oil/Gas Wells (per 100 wells)',
                                                  'Log 10 Vehicle NOx (tons/yr)',
                                                  'PM2.5 (ug/m^3)'))) %>% 
  mutate(moran_stat = round(as.numeric(moran_stat), digits = 2),
         pvalue = round(as.numeric(pvalue), digits = 2)) %>% 
  arrange(envExpLab, plural_bin_imp, year, segLab)

# 4b Save table S1
ts1 %>% write.csv(paste0(final_table_path, 'tableS1_oct25.csv'))


