# Plots
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 10/07/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Figures 1, S2, S3 - Maps of 1940 and 2010 env exposures and segregation metrics 
# 2: Figures 2, S6 - Senke charts of segregation metrics
# 3: Figures 4, 5, S8, S12, S13, S15 - Regression result 2-curve plots + Buffer and RM Sensitivity
# 4: Figure 6 - Consistent segregation forest plot
# 5: Figure S5 - Map of consistently highly segregated counties
# 6: Figure S4 - Correlation matrix of all segregation and air pollution metrics
# 7: Figures S14 & S9-S11, S16-17 - Sens Analysis Results - DI, Removing Outliers, PP/OGW in 1940 counties
# 8: Figures 3, S7 - % Black and % White by Consistently High and Low Segregation
# 9: Figure S1 - Map of residuals for models with high Morans I values

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we make all figures for use in the manuscript and supplement. Figures
# made here are publication ready. 

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
prelim_plot_path <- paste0(project.folder, 'outputs/prelim_plots/regression_results/')
spatial_data_path <- paste0(project.folder, 'data/county_shapefiles/')
final_plot_path <- paste0(project.folder, 'outputs/final_plots/')
final_table_path <- paste0(project.folder, 'outputs/final_tables/')

# 0d Load all env exp, seg, sociodemo, and spatial data
#    Note: CRS is an Albers Equal Area Conic with parameters (meters)
data <- read_fst(paste0(merged_data_path, 'env_seg_sociodemo_allyears.fst'))
senke_data <- read_fst(paste0(merged_data_path, 'segQuantilesForSenkesAllData.fst'))
senke_data_strat <- read_fst(paste0(merged_data_path, 'segQuantilesForSenkesStratifiedData.fst'))
county1940 <- st_read(paste0(spatial_data_path, 
                             'FINAL_1940_county_shapefile_clean/counties_contiguous_1940.shp')) %>% 
  janitor::clean_names()
county1940_2 <- st_simplify(county1940, dTolerance = 100000, preserveTopology = TRUE)
#county1940_2 <- rmapshaper::ms_simplify(county1940, keep = 0.1, keep_shapes = TRUE)
county2010 <- st_read(paste0(spatial_data_path, 
                             'FINAL_2010_county_shapefile_clean/counties_contiguous_2010.shp')) %>% 
  janitor::clean_names()
state1940 <- st_read(paste0(project.folder, 'data/state_shapefiles/US_state_1940.shp')) %>% 
  janitor::clean_names()

# 0e Load all regressions, predictions, and plots
regs_cbNS <- read_rds(paste0(model_path, 'regs_plots_cbNS.RDS'))
regs_persmarg <- read_rds(paste0(model_path, 'regs_persmarg.RDS'))
regs_persmarg_strat <- read_rds(paste0(model_path, 'regs_strat_persmarg.RDS'))
regs_cbNS_diSens <- read_rds(paste0(model_path, 'regs_plots_cbNS_diSens.RDS'))
regs_cbNS_outlierSens <- read_rds(paste0(model_path, 'regs_plots_cbNS_outlierSens.RDS'))
regs_cbNS_ogwSens <- read_rds(paste0(model_path, 'regs_plots_cbNS_ogwSens.RDS'))
regs_cbNS_ppSens <- read_rds(paste0(model_path, 'regs_plots_cbNS_ppSens.RDS'))
regs_cbNS_log10nox <- read_rds(paste0(model_path, 'regs_plots_cbNS_log10nox.RDS'))

# 0f Load 2010 county shapefile obtained from tidycensus (less vertices)(1940 not available)
county2010_tc <- st_read(paste0(spatial_data_path,
                                'tidycensus_2010_county_shapefile/tc_counties_2010_clean.shp'))

####*****************************************************************************************
#### 1: Figures 1, S2, S3 - Maps of 1940 and 2010 env exposures and segregation metrics  #### 
####*****************************************************************************************

# 1a Remove Alaska and Hawaii from state shapefile
state1940 <- as.data.frame(state1940)
state1940 <- state1940 %>% filter(! statenam %in% c('Alaska Territory', 'Hawaii Territory'))
state1940 <- st_as_sf(state1940)  

# 1b Create separate dataset for 1940 & 2010 and merge to county shapefile 
data_map1940 <- data %>% filter(year == '1940')
datasf_map1940 <- county1940_2 %>% full_join(data_map1940, by = c('jnhgis40' = 'st_cnty_yr_id'))
data_map2010 <- data %>% filter(year == '2010') 
datasf_map2010 <- county2010_tc %>% full_join(data_map2010, by = c('GEOID' = 'st_cnty_yr_id'))

# 1c Source function to create maps
source(paste0(project.folder, 'functions/final_maps_plots.R'))

# 1d Create map for each variable of interest
di_map <- final_quartile_map(dataToMap = datasf_map1940, stateSF = state1940, 
                             variable_name = 'di', cat_num = '4', color = 'magma',
                             plot_title = 'E: Dissimilarity Index, 1940', cut_type = 'Quartiles')
ice_map <- final_quartile_map(dataToMap = datasf_map1940, stateSF = state1940, 
                              variable_name = 'raceEduICE_hsd10', cat_num = '4', color = 'magma',
                              plot_title = 'G: Black/White + Ed ICE, 1940', cut_type = 'Quartiles')
nbr_map <- final_quartile_map(dataToMap = datasf_map1940, stateSF = state1940, 
                              variable_name = 'nbrOne', cat_num = '4', color = 'magma',
                              plot_title = 'Neighbor Index', cut_type = 'Quartiles')
pm_map <- final_quartile_map(dataToMap = datasf_map1940, stateSF = state1940, 
                             variable_name = 'meanPM2.5', cat_num = '4', color = 'viridis',
                             plot_title = 'A: PM2.5 (µg/m^3), 1940', cut_type = 'Quartiles')
pm_map2 <- pm_map + labs(subtitle = bquote("A: PM"[2.5]~"("*µg~m^-3*"), 1940")) 
nox_map <- final_quartile_map(dataToMap = datasf_map1940, stateSF = state1940, 
                                  variable_name = 'nox_autos', cat_num = '4', color = 'viridis',
                                  plot_title = 'C: NOx from Automobiles (tons/yr), 1940', cut_type = 'Quartiles')
nox_map2 <- nox_map + labs(subtitle = bquote("C: NO"[x]~"from Automobiles (tons/yr), 1940")) 
fospp_map <- final_quartile_map(dataToMap = datasf_map1940, stateSF = state1940, 
                                variable_name = 'fospp_count', cat_num = '2', color = 'viridis',
                                plot_title = 'A: Fossil Fuel Powerplants (count), 1940', cut_type = 'PP')
ogw_map <- final_quartile_map(dataToMap = datasf_map1940, stateSF = state1940, 
                              variable_name = 'ogw_count', cat_num = '4', color = 'viridis',
                              plot_title = 'C: Oil/Gas Well (count), 1940', cut_type = 'OGW')
di_map10 <- final_quartile_map(dataToMap = datasf_map2010, stateSF = state1940, 
                             variable_name = 'di', cat_num = '4', color = 'magma',
                             plot_title = 'F: Dissimilarity Index, 2010', cut_type = 'Quartiles')
ice_map10 <- final_quartile_map(dataToMap = datasf_map2010, stateSF = state1940, 
                              variable_name = 'raceEduICE_hsd10', cat_num = '4', color = 'magma',
                              plot_title = 'H: Black/White + Ed ICE, 2010', cut_type = 'Quartiles')
pm_map10 <- final_quartile_map(dataToMap = datasf_map2010, stateSF = state1940, 
                             variable_name = 'meanPM2.5', cat_num = '4', color = 'viridis',
                             plot_title = 'B: PM2.5 (µg/m^3), 2010', cut_type = 'Quartiles')
pm_map10_2 <- pm_map10 +  labs(subtitle = bquote("B: PM"[2.5]~"("*µg~m^-3*"), 2010")) 
nox_map10 <- final_quartile_map(dataToMap = datasf_map2010, stateSF = state1940, 
                                  variable_name = 'nox_autos', cat_num = '4', color = 'viridis',
                                  plot_title = 'D: NOx from Automobiles (tons/yr), 2010', cut_type = 'Quartiles')
nox_map10_2 <- nox_map10 + labs(subtitle = bquote("D: NO"[x]~"from Automobiles (tons/yr), 2010")) 
fospp_map10 <- final_quartile_map(dataToMap = datasf_map2010, stateSF = state1940, 
                                variable_name = 'fospp_count', cat_num = '2', color = 'viridis',
                                plot_title = 'B: Fossil Fuel Powerplants (count), 2010', cut_type = 'PP')
ogw_map10 <- final_quartile_map(dataToMap = datasf_map2010, stateSF = state1940, 
                              variable_name = 'ogw_count', cat_num = '4', color = 'viridis',
                              plot_title = 'D: Oil/Gas Well (count), 2010', cut_type = 'OGW')

# 1e Combine select 1940 and 2010 maps for Figure 1
fig1 <-  (fospp_map | fospp_map10) / (ogw_map | ogw_map10) / (di_map | di_map10) / (ice_map | ice_map10) +
  plot_annotation(theme = theme(plot.title = element_text(size = 12)))

# 1f Combine all remaining maps (except for neighbor metric) for Figure S2
figS2 <- (pm_map2 | pm_map10_2) / (nox_map2 | nox_map10_2) +
  plot_annotation(theme = theme(plot.title = element_text(size = 12)))

# 1g Save maps
ggsave(fig1,
       filename = paste0(final_plot_path, 'fig1_mapSegExp_1940-2010.pdf'),
       width = 4*2, height = 3*2, scale = 1.2)
ggsave(figS2,
       filename = paste0(final_plot_path, 'figS2_mapSegExp_1940-2010.pdf'),
       width = 4*2, height = 3*2, scale = 1.2)
ggsave(nbr_map,
       filename = paste0(final_plot_path, 'figS3_mapNbrSeg_1940.pdf'),
       width = 4*2, height = 3*2, scale = 1.2)

####************************************************************
#### 2: Figures 2, S6 - Senke charts of segregation metrics #### 
####************************************************************

# 2a Create 2010-rurality-stratified senke plot: DI
#    Note: Warning about strata appearing at multiple axes is because the
#          strata in axis1 and axis2 are called the same (e.g., Q1)
senke_di_strat <- senke_data_strat %>% 
  filter(!is.na(di)) %>% filter(!is.na(di_1940_ws)) %>% 
  mutate(di_1940_ws = factor(paste0('Q', di_1940_ws)),
         di = factor(paste0('Q', di))) %>% 
  ggplot(aes(axis1 = di_1940_ws, axis2 = di)) +
  scale_x_discrete(limits = c('1940', '2010'), expand = c(.2, .05)) +
  xlab('DI Quartile') + ylab('Number of Counties') +
  geom_alluvium(aes(fill = di_1940_ws), alpha = 1) +
  geom_stratum() +
  facet_wrap(~plural_bin_imp) +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum))) + 
  scale_fill_manual(values = rev(c(viridis::magma(4))),
                    labels = c('Q1: Low Segregation', 'Q2', 'Q3', 'Q4: High Segregation')) +
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(title = 'DI Quartile in 1940')) +
  theme(legend.position = 'right')
senke_di_strat

# 2b ICE 
senke_ice_strat <- senke_data_strat %>% 
  filter(!is.na(raceEduICE_hsd10)) %>% filter(!is.na(raceEduICE_hsd10_1940_ws)) %>% 
  mutate(raceEduICE_hsd10_1940_ws = factor(paste0('Q', raceEduICE_hsd10_1940_ws)),
         raceEduICE_hsd10 = factor(paste0('Q', raceEduICE_hsd10))) %>% 
  ggplot(aes(axis1 = raceEduICE_hsd10_1940_ws, axis2 = raceEduICE_hsd10)) +
  scale_x_discrete(limits = c('1940', '2010'), expand = c(.2, .05)) +
  xlab('Black / White + Ed ICE Quartile') + ylab('Number of Counties') +
  geom_alluvium(aes(fill = raceEduICE_hsd10_1940_ws), alpha = 1) +
  geom_stratum() +
  facet_wrap(~plural_bin_imp) +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum))) + 
  scale_fill_manual(values = rev(c(viridis::magma(4))),
                    labels = c('Q1: Conc. of White\nPeople w Higher Ed. Attainment', 'Q2', 'Q3', 'Q4: Conc. of Black People\nWith Lower Ed. Attainment')) + 
  theme_minimal(base_size = 14) +
  guides(fill = guide_legend(title = 'Black / White + Ed\nICE Quartile in 1940')) +
  theme(legend.position = 'right',
        legend.key.height = unit(1, "cm"))
senke_ice_strat

# 2c Save plots
ggsave(senke_di_strat,
       filename = paste0(final_plot_path, 'fig2_DI_StratSenke.jpg'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)
ggsave(senke_ice_strat,
       filename = paste0(final_plot_path, 'figS6_ICE_StratSenke.jpg'),
       width = 3.6*2, height = 2.25*2, scale = 1.2)

####*******************************************************************************************************
#### 3: Figures 4, 5, S8, S12, S13, S15 - Regression result 2-curve plots + Buffer and RM Sensitivity  #### 
####*******************************************************************************************************

# DI and ICE regression result plots that have both 1940 and 2010 
# regression curves on the same plot

#***************************** First we run the results for DI and ICE

# 3a Set up dataframe for plots comparing 1940 and 2010 so that predictions from
#    each row are separate variables
# 3a.i 1940 datasets
comp_plots <- regs_cbNS %>% filter(year == '1940') %>% ungroup() %>% 
  filter(! segMetric == 'nbrOne') %>% filter(! str_detect(envExp, 'nox')) %>%  
  dplyr::select(-plots,  -year, -nlME, -constraint) %>% 
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)
results_nox1940 <- regs_cbNS_log10nox %>% filter(year == '1940') %>% ungroup() %>% 
  filter(! segMetric == 'nbrOne') %>% 
  dplyr::select(-plots,  -year, -nlME, -constraint) %>% 
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)
# 3a.ii 2010 datasets
#       Note: In the last mutate step, we make sure the 2010 Randall Martin PM2.5   
#       estimates will be compared with the 1940 climate model PM2.5 estimates, 
#       rather than using 2010 climate model PM2.5 estimates.
#       Update: As of Nov 19, 2024 we will use the CMP6 PM2.5 estimates for 2010
#       so that they are comparable to 1940
results2010 <- regs_cbNS %>% filter(year == '2010') %>% ungroup() %>% 
  filter(! segMetric == 'nbrOne') %>% filter(! envExp == 'pm2.5_rm') %>% 
  filter(! str_detect(envExp, 'nox')) %>%  
  dplyr::select(-plots, -envExpLab, -segLab, -year, -nlME, -modelType, -constraint) %>% 
  rename(preds_2010 = preds,
         data_2010 = data,
         modelName_2010 = modelName)
  #mutate(envExp = ifelse(envExp == 'pm2.5_rm', 'meanPM2.5', envExp))
results_nox2010 <- regs_cbNS_log10nox %>% filter(year == '2010') %>% ungroup() %>% 
  dplyr::select(-plots, -envExpLab, -segLab, -year, -nlME, -modelType, -constraint) %>% 
  rename(preds_2010 = preds,
         data_2010 = data,
         modelName_2010 = modelName)
# 3a.iii Merge 1940 and 2010 datasets
comp_plots <- comp_plots %>% bind_rows(results_nox1940)
results2010 <- results2010 %>% bind_rows(results_nox2010)
comp_plots <- comp_plots %>% 
  full_join(results2010, by = c('plural_bin_imp', 'envExp', 'segMetric')) %>% 
  mutate(double_spline_plots = list(NA))

# 3b Source function to create plots
source(paste0(project.folder, 'functions/final_maps_plots.R'))

# 3c Create plots
# 3c.i Create two curve plots
for(i in 1:length(comp_plots$envExp)){
  comp_plots$double_spline_plots[[i]] <- spline_2yr_plot(row = i)
}
# 3c.ii Run boxplots as a separate plot for each seg metric, stratified by urban/rural
for(i in 1:length(comp_plots$envExp)){
  comp_plots$boxplots[[i]] <- boxplot_2yr(row = i)
}

# 3d Reorder plots to make arranging in multi-panel figures easier
comp_plots <- comp_plots %>% 
  arrange(segLab, envExp, plural_bin_imp)
comp_plots$modelName_1940

# 3e Combine plots into two multi-panel figures (one DI, one ICE) 
# 3e.i DI
pm_a <- comp_plots$double_spline_plots[[23]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
pm_b <- comp_plots$double_spline_plots[[24]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
nox_a <- comp_plots$double_spline_plots[[25]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
nox_b <- comp_plots$double_spline_plots[[26]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
bp_rural_di <- comp_plots$boxplots[[23]] + xlab("Dissimilarity Index")
bp_urban_di <- comp_plots$boxplots[[23]] + xlab("Dissimilarity Index")
di_2spline_plot <- 
  (comp_plots$double_spline_plots[[19]] | comp_plots$double_spline_plots[[20]]) /
  (comp_plots$double_spline_plots[[29]] | comp_plots$double_spline_plots[[30]]) /
  (nox_a | nox_b) /
  (pm_a | pm_b) /
  (bp_rural_di | bp_urban_di)
# 3e.ii ICE
pm_c <- comp_plots$double_spline_plots[[7]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")"))
pm_d <- comp_plots$double_spline_plots[[8]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")"))
nox_c <- comp_plots$double_spline_plots[[9]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
nox_d <- comp_plots$double_spline_plots[[10]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
bp_rural_ice <- comp_plots$boxplots[[7]] + xlab("Black/White + Ed ICE")
bp_urban_ice <- comp_plots$boxplots[[8]] + xlab("Black/White + Ed ICE")
ice_2spline_plot <- 
  (comp_plots$double_spline_plots[[3]] | comp_plots$double_spline_plots[[4]]) /
  (comp_plots$double_spline_plots[[13]] | comp_plots$double_spline_plots[[14]]) /
  (nox_c | nox_d) /
  (pm_c | pm_d) /
  (bp_rural_ice | bp_urban_ice)

# 3f Save combined plots
# 3f.i DI
ggsave(wrap_elements(panel = di_2spline_plot) +
         labs(tag = 'Dissimilarity Index') +
         theme(plot.tag = element_text(size = rel(1)),
               plot.tag.position = 'bottom'),
       filename = paste0(final_plot_path, '/fig4_DI_allRegressionCurves_wTP_oct25.jpg'),
       width = 6, height = 8, scale = 1.2)
# 3f.ii ICE
ggsave(wrap_elements(panel = ice_2spline_plot) +
         labs(tag = 'Black/White + Ed ICE') +
         theme(plot.tag = element_text(size = rel(1)),
               plot.tag.position = 'bottom'),
       filename = paste0(final_plot_path, '/fig5_ICE_allRegressionCurves_wTP_oct25.jpg'),
       width = 6, height = 8, scale = 1.2)

#***************************** Second we run Neighbor metric regression result plots for 1940 only 

# 3g Set up dataframe for nbrOne metric plot, both urban/rural
# 3g.i Filter to only neighbor plots from the main regression dataframe
nbrOne_plots <- regs_cbNS %>% ungroup() %>% 
  filter(segMetric == 'nbrOne') %>% filter(! str_detect(envExp, 'nox'))
# 3g.ii Filter to only neighbor plots from the Log10 NOx dataframe
nbrOne_nox <- regs_cbNS_log10nox %>% ungroup() %>% 
  filter(segMetric == 'nbrOne')
# 3g.iii Merge two two datsets and rename variables
nbrOne_plots <- nbrOne_plots %>% 
  bind_rows(nbrOne_nox) %>% 
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)

# 3h Source function to create plots
source(paste0(project.folder, 'functions/final_maps_plots.R'))

# 3i Create plots
# 3i.i Create single curve plots
for(i in 1:length(nbrOne_plots$envExp)){
  nbrOne_plots$single_spline_plots[[i]] <- spline_1yr_plot(row = i)
}
# 3i.ii Run boxplots as a separate plot, stratified by urban/rural
for(i in 1:length(nbrOne_plots$envExp)){
  nbrOne_plots$boxplots[[i]] <- boxplot_1yr(row = i)
}

# 3j Reorder plots to make arranging in multi-panel figures easier
nbrOne_plots <- nbrOne_plots %>% 
  arrange(segLab, envExp, plural_bin_imp)
nbrOne_plots$modelName_1940

# 3k Combine plots into one multi-panel figure
pm_e <- nbrOne_plots$single_spline_plots[[7]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
pm_f <- nbrOne_plots$single_spline_plots[[8]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
nox_e <- nbrOne_plots$single_spline_plots[[9]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
nox_f <- nbrOne_plots$single_spline_plots[[10]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
nbrOne_1spline_plot <- 
  (nbrOne_plots$single_spline_plots[[3]] | nbrOne_plots$single_spline_plots[[4]]) /
  (nbrOne_plots$single_spline_plots[[13]] | nbrOne_plots$single_spline_plots[[14]]) /
  (nox_e | nox_f) /
  (pm_e | pm_f) /
  (nbrOne_plots$boxplots[[7]] | nbrOne_plots$boxplots[[8]])

# 3l Save combined plot
ggsave(wrap_elements(panel = nbrOne_1spline_plot) +
         labs(tag = 'Neighbor-based Segregation Metric') +
         theme(plot.tag = element_text(size = rel(1)),
               plot.tag.position = 'bottom'),
       filename = paste0(final_plot_path, '/figS8_neighbor_allRegressionCurves_wTP_may.jpg'),
       width = 6, height = 8, scale = 1.2)

#***************************** Third we create plots for the sens analysis with power plants and wells within 5km of county boundary

# 3m Combine 5km buffered plots - including all seg metrics in one plot
# 3m.i Fossil Fuel 5km buffer
ffbuf_2spline_plot <-
  (comp_plots$double_spline_plots[[17]] + ggtitle('DI: Rural')  | comp_plots$double_spline_plots[[18]] + ggtitle('DI: Urban') ) /
  (comp_plots$double_spline_plots[[1]] + ggtitle('ICE: Rural')  | comp_plots$double_spline_plots[[2]] + ggtitle('ICE: Urban') ) /
  (nbrOne_plots$single_spline_plots[[1]] + ggtitle('NEIGHBOR: Rural')  | nbrOne_plots$single_spline_plots[[2]] + ggtitle('NEIGHBOR: Urban') ) 
# 3m.ii Oil/Gas 5km buffer
ogbuf_2spline_plot <- 
  (comp_plots$double_spline_plots[[27]] + ggtitle('DI: Rural') | comp_plots$double_spline_plots[[28]] + ggtitle('DI: Urban') ) /
  (comp_plots$double_spline_plots[[11]] + ggtitle('ICE: Rural')  | comp_plots$double_spline_plots[[12]] + ggtitle('ICE: Urban') ) /
  (nbrOne_plots$single_spline_plots[[11]] + ggtitle('NEIGHBOR: Rural')  | nbrOne_plots$single_spline_plots[[12]] + ggtitle('NEIGHBOR: Urban') )

# 3n Save combined plots
ggsave(ffbuf_2spline_plot,
       filename = paste0(final_plot_path, '/figS12_fosFuelPpBuffer_allRegressionCurves_may.jpg'),
       width = 6, height = 8, scale = 1.2)
ggsave(ogbuf_2spline_plot,
       filename = paste0(final_plot_path, '/figS13_ogwBuffer_allRegressionCurves_may.jpg'),
       width = 6, height = 8, scale = 1.2)

#***************************** Fourth we create plots for the sens analysis with Randall Martin PM2.5 for 2010

# Note: dataframes are called comp_plots in three sections of this code chunk to work with function -- will need to re-run 
#       code for each section to make sure comp_plots contains the appropriate data

# 3o Set up dataframe for plots comparing 1940 and 2010 so that predictions from
#    each row are separate variables
# 3o.i 1940 dataset
comp_plots <- regs_cbNS %>% filter(year == '1940') %>% ungroup() %>% 
  filter(! segMetric == 'nbrOne') %>% 
  dplyr::select(-plots,  -year, -nlME, -constraint) %>% 
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)
# 3o.ii 2010 dataset
#       Note: In the last mutate step, we make sure the 2010 Randall Martin PM2.5   
#       estimates will be compared with the 1940 climate model PM2.5 estimates, 
#       rather than using 2010 climate model PM2.5 estimates.
results2010 <- regs_cbNS %>% filter(year == '2010') %>% ungroup() %>% 
  filter(! segMetric == 'nbrOne') %>% filter(! envExp == 'meanPM2.5') %>% 
  dplyr::select(-plots, -envExpLab, -segLab, -year, -nlME, -modelType, -constraint) %>% 
  rename(preds_2010 = preds,
         data_2010 = data,
         modelName_2010 = modelName) %>% 
  mutate(envExp = ifelse(envExp == 'pm2.5_rm', 'meanPM2.5', envExp))
# 3o.iii Merge 1940 and 2010 dataset
comp_plots <- comp_plots %>% 
  full_join(results2010, by = c('plural_bin_imp', 'envExp', 'segMetric')) %>% 
  mutate(double_spline_plots = list(NA))

# 3p Source function to create plots
source(paste0(project.folder, 'functions/final_maps_plots.R'))

# 3q Create two curve plots
for(i in 1:length(comp_plots$envExp)){
  comp_plots$double_spline_plots[[i]] <- spline_2yr_plot(row = i)
}

# 3r Combine all PM2.5 plots
comp_plots$modelName_2010
rm_plots <- 
  (comp_plots$double_spline_plots[[9]] + ggtitle('DI: Rural') + xlab('Dissimilarity Index') + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) + theme(text = element_text(size = 13)) | comp_plots$double_spline_plots[[27]] + ggtitle('DI: Urban') + xlab('Dissimilarity Index') + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) + theme(text = element_text(size = 13)) ) /
  (comp_plots$double_spline_plots[[10]] + ggtitle('Black/White + Ed ICE: Rural') + xlab('Black/White + Ed ICE') + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) + theme(text = element_text(size = 13)) | comp_plots$double_spline_plots[[28]] + ggtitle('Black/White + Ed ICE: Urban') + xlab('Black/White + Ed ICE') + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) + theme(text = element_text(size = 13)) )

# 3s Save combined plot
ggsave(rm_plots,
       filename = paste0(final_plot_path, '/figS15_rmPM2.5sens_allRegressionCurves_wTP_oct25.jpg'),
       width = 10, height = 8, scale = 1.2)

####******************************************************
#### 4: Figure 6 - Consistent segregation forest plot #### 
####******************************************************

# 4a Unnest stratified model results
persmarg_strat <- regs_persmarg_strat %>% ungroup() %>% 
  dplyr::select(plural_bin_imp, envExpLab, segLab, modelType, tidy_mod) %>% 
  unnest(tidy_mod) %>% filter(term == 'segValueTRUE') %>% 
  mutate(rurality_label = ifelse(envExpLab == 'Fossil Fuel Plants (per 1 facility increase)' & segLab == 'Black/White + Ed ICE', 
                                 plural_bin_imp, NA)) %>% 
  mutate(segLab = factor(segLab, levels = c('Dissimilarity Index', 'Black/White + Ed ICE')))

# 4b Forest plot of model results for 2010 rurality stratified data - gaussian
persmarg_plot_strat_gaussian <- persmarg_strat %>% 
  filter(modelType == 'gaussian') %>% 
  mutate(envExpLab_expression = ifelse(str_detect(envExpLab, 'PM2.5'), 
                                       "PM[2.5]~(per~3~µg~m^{-3}~increase)", 
                                       "Log~10~Vehicle~NO[x]~(per~0.79~tons/yr~increase)"),
         envExpLab_expression = factor(envExpLab_expression,
                                       levels = c("Log~10~Vehicle~NO[x]~(per~0.79~tons/yr~increase)", 
                    "PM[2.5]~(per~3~µg~m^{-3}~increase)"))) %>% 
  ggplot(aes(x = segLab, y = estimate, color = segLab, 
             shape = plural_bin_imp)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.2)) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  scale_color_manual(values = c('darkgoldenrod2', 'darkorchid')) +
  scale_shape_manual(values = c(1, 8)) + 
  #scale_alpha_manual(values = c(0.5, 1)) +
  facet_wrap(~envExpLab_expression, scales = 'free',
             labeller = label_parsed) +
  # scale_y_continuous(limits = c(-0.4, 1.0),
  #                    breaks = c(-0.25, 0, 0.25, 0.5, 0.75, 1.0)
  #                    ) +
  xlab('Consistently High Segregation (1940 to 2010)') + ylab('Estimated Mean Difference & 95% CI') +
  theme_bw() +
  guides(color = guide_legend(title = 'Segregation Metric:'),
         shape = guide_legend(title = 'Urban or Rural:'),
         alpha = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        legend.position = 'bottom')

persmarg_plot_strat_gaussian

# 4c Forest plot of model results for 2010 rurality stratified data - negative binomial
persmarg_plot_strat_negbin <- persmarg_strat %>% 
  filter(modelType == 'negbin') %>% 
  ggplot(aes(x = segLab, y = estimate, color = segLab, 
             shape = plural_bin_imp)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.2)) +
  geom_hline(aes(yintercept = 1), linetype = 'dashed') +
  geom_label(aes(label = rurality_label, y = estimate + 0.25), position = position_dodge(width = 0.2), 
            vjust = 0, hjust = -0.1,
            #nudge_y = 0.15, nudge_x = 0.2,
            color = 'black') + 
  scale_color_manual(values = c('darkgoldenrod2', 'darkorchid')) +
  scale_shape_manual(values = c(1, 8)) + 
  #scale_alpha_manual(values = c(0.5, 1)) +
  facet_wrap(~envExpLab, scales = 'free') +
  scale_y_continuous(trans = 'log10', labels = scales::comma) +
                     # limits = c(0.25, 2.5),
                     # breaks = c(0.5, 1.00, 1.50, 2.00, 2.50)
                     # #breaks = c(0.25, 0.5, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00, 2.25)
                     # ) +
  ylab('RR & 95% CI') +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'none')

persmarg_plot_strat_negbin

# 4d Combine plots for 2010 rurality stratified data
persmarg_plot_strat <- persmarg_plot_strat_negbin / persmarg_plot_strat_gaussian 
persmarg_plot_strat

# 4e Save plot for 2010 rurality stratified data
ggsave(persmarg_plot_strat,
       filename = paste0(final_plot_path, '/fig6_persistentSegStratified_forestplot_oct25.jpg'),
       width = 6.5, height = 2.3*3, scale = 1.2)

####*******************************************************************
#### 5: Figure S5 - Map of consistently highly segregated counties #### 
####*******************************************************************

# 5a Calculate number of consistently highly segregated counties for DI and ICE
#    Note: Rural DI [1]; Rural ICE [2]; Urban DI [5]; Urban ICE [6]
table(regs_persmarg_strat$data[[1]]$segValue, useNA = 'always')
table(regs_persmarg_strat$data[[2]]$segValue, useNA = 'always')
table(regs_persmarg_strat$data[[5]]$segValue, useNA = 'always')
table(regs_persmarg_strat$data[[6]]$segValue, useNA = 'always')

# 5b Set colors for maps and remove Alaska and Hawaii from state sf
scales::show_col(viridis_pal(option = 'A')(20))
magma_2 <- c('#FCFDBFFF', '#E85362FF')
state1940 <- as.data.frame(state1940)
state1940 <- state1940 %>% filter(! statenam %in% c('Alaska Territory', 'Hawaii Territory'))
state1940 <- st_as_sf(state1940)  

# 5c Create map of consistently highly segregated counties
# 5c.i DI Rural
ps_di_rural <- regs_persmarg_strat$data[[1]] %>% 
  full_join(county2010_tc, by = c('st_cnty_yr_id' = 'GEOID')) %>% 
  ggplot(aes(geometry = geometry, fill = segValue), alpha = 0.8) + 
  geom_sf(color = 'gray80', linewidth = 0.1) + 
  scale_fill_manual(values = magma_2) +
  labs(subtitle = 'A: Dissimilarity Index - Rural') +
  geom_sf(fill = "transparent", color = 'black', linewidth = 0.3,
          data = state1940) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        legend.position = 'none')
ps_di_rural
# 5c.ii DI Urban
ps_di_urban <- regs_persmarg_strat$data[[5]] %>% 
  full_join(county2010_tc, by = c('st_cnty_yr_id' = 'GEOID')) %>% 
  ggplot(aes(geometry = geometry, fill = segValue), alpha = 0.8) + 
  geom_sf(color = 'gray80', linewidth = 0.1) + 
  scale_fill_manual(values = magma_2) +
  labs(subtitle = 'B: Dissimilarity Index - Urban') +
  geom_sf(fill = "transparent", color = 'black', linewidth = 0.3,
          data = state1940) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        legend.position = 'none')
ps_di_urban
# 5b.iii ICE Rural
ps_ice_rural <- regs_persmarg_strat$data[[2]] %>% 
  full_join(county2010_tc, by = c('st_cnty_yr_id' = 'GEOID')) %>% 
  ggplot(aes(geometry = geometry, fill = segValue), alpha = 0.8) + 
  geom_sf(color = 'gray80', linewidth = 0.1) + 
  scale_fill_manual(values = magma_2) +
  labs(subtitle = 'C: Black/White + Ed ICE - Rural') +
  geom_sf(fill = "transparent", color = 'black', linewidth = 0.3,
          data = state1940) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        legend.position = 'none')
ps_ice_rural
# 5b.iv ICE Urban
ps_ice_urban <- regs_persmarg_strat$data[[6]] %>% 
  full_join(county2010_tc, by = c('st_cnty_yr_id' = 'GEOID')) %>% 
  ggplot(aes(geometry = geometry, fill = segValue), alpha = 0.8) + 
  geom_sf(color = 'gray80', linewidth = 0.1) + 
  scale_fill_manual(values = magma_2) +
  labs(subtitle = 'D: Black/White + Ed ICE - Urban') +
  geom_sf(fill = "transparent", color = 'black', linewidth = 0.3,
          data = state1940) +
  guides(fill = guide_legend(title = 'Consistently High Segregation')) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        legend.position = 'bottom')
ps_ice_urban

# 5c Combine plots
cons_seg_maps <- (ps_di_rural | ps_di_urban) / (ps_ice_rural | ps_ice_urban) +
  plot_annotation(theme = theme(plot.title = element_text(size = 12)))
cons_seg_maps

# 5d Save plot
ggsave(cons_seg_maps,
       filename = paste0(final_plot_path, 'figS5_mapConsSegStrat_oct25.jpg'),
       width = 3.5*2, height = 2.25*2, scale = 1.2)

####************************************************************************************
#### 6: Figure S4 - Correlation matrix of all segregation and air pollution metrics #### 
####************************************************************************************

# 6a Prepare dataframe for correlations
cor_data <- data %>% 
  dplyr::select(di, raceEduICE_hsd10, nbrOne, meanPM2.5, nox_autos:ogwBuffer_count) %>% 
  dplyr::select(-ogw_idw, -fospp_idw) %>% 
  dplyr::select(-fosppBuffer_count, -ogwBuffer_count) %>% 
  rename(DI = di,
         `Fossil PP Count` = fospp_count,
         `Vehicle NOx` = nox_autos,
         `Neighbor Metric` = nbrOne,
         `Oil Gas Well Count` = ogw_count,
         `PM2.5` = meanPM2.5,
         `Race Ed ICE` = raceEduICE_hsd10)

# 6b Run correlations
#    Notes: Use Spearman because vars not normally distributed
cor <- cor(cor_data, method = c('spearman'), use = 'pairwise.complete.obs')

# 6c Review plot
corrplot(cor, method = 'circle', type = 'lower', order = 'alphabet', cl.cex = 1)

# 6d Identify correlations >= 0.8 or <= -0.8
tidy_cor <- as.data.frame(cor)
tidy_cor <- tidy_cor %>% 
  rownames_to_column(var = 'var1') %>% 
  as_tibble() %>% 
  pivot_longer(cols = !var1, names_to = 'var2', values_to = 'corr') %>% 
  filter(!var1 == var2) %>% 
  mutate(high_corr = ifelse(corr >= 0.8 & corr < 1, 1, 0),
         low_corr  = ifelse(corr <= -0.8, 1, 0))

# 6e Save correlation plots and table
tiff(paste0(final_plot_path, 'figS4_corplot_oct25.tiff'),
     units = "cm", width = 15, height = 15, res = 300)
corrplot(cor, method = 'circle', type = 'lower', order = 'alphabet', 
         cl.cex = 0.8, tl.cex = 0.8)
dev.off()
tidy_cor %>% write_csv(paste0(final_table_path, 'env_seg_correlations_oct25.csv'))

####**************************************************************************************************************
#### 7: Figures S14 & S9-S11, S16-17 - Sens Analysis Results - DI, Removing Outliers, PP/OGW in 1940 counties #### 
####**************************************************************************************************************

# Note: Be sure to re-run commands where the comp_plots or nbr_one dataset is 
#       generated - this same name is used for the main results and for sens 
#       analyses. 

#################################################################### DI Sensitivity Analysis

# 7a Set up dataframe for plots comparing 1940 and 2010 so that predictions from
#    each row are separate variables
# 7a.i 1940 dataset
comp_plots <- regs_cbNS_diSens %>% filter(year == '1940') %>% ungroup() %>% 
  filter(! envExp == 'nox_autos_10tpy') %>% 
  dplyr::select(-plots,  -year, -nlME, -constraint) %>% 
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)
# 7a.ii 2010 dataset
results2010_diSens <- regs_cbNS_diSens %>% filter(year == '2010') %>% ungroup() %>% 
  filter(! envExp == 'pm2.5_rm') %>% filter(! envExp == 'nox_autos_10tpy') %>% 
  dplyr::select(-plots, -envExpLab, -segLab, -year, -nlME, -modelType, -constraint) %>% 
  rename(preds_2010 = preds,
         data_2010 = data,
         modelName_2010 = modelName) 
# 7a.iii Merge 1940 and 2010 dataset
comp_plots <- comp_plots %>% 
  full_join(results2010_diSens, by = c('plural_bin_imp', 'envExp', 'segMetric')) %>% 
  mutate(double_spline_plots = list(NA))

# 7b Source function to create plots
source(paste0(project.folder, 'functions/final_maps_plots.R'))

# 7c Create plots
# 3c.i Create two curve plots
for(i in 1:length(comp_plots$envExp)){
  comp_plots$double_spline_plots[[i]] <- spline_2yr_plot(row = i)
}
# 7c.ii Run boxplots as a separate plot for each seg metric, stratified by urban/rural
for(i in 1:length(comp_plots$envExp)){
  comp_plots$boxplots[[i]] <- boxplot_2yr(row = i)
}

# 7d Reorder plots to make arranging in multi-panel figures easier
comp_plots <- comp_plots %>% 
  arrange(envExp, plural_bin_imp)
comp_plots$modelName_1940

# 7e Combine plots into a multi-panel figure
pm_g <- comp_plots$double_spline_plots[[3]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
pm_h <- comp_plots$double_spline_plots[[4]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
nox_g <- comp_plots$double_spline_plots[[5]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
nox_h <- comp_plots$double_spline_plots[[6]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
di_2spline_plot_diSens <- 
  (comp_plots$double_spline_plots[[1]] | comp_plots$double_spline_plots[[2]]) /
  (comp_plots$double_spline_plots[[7]] | comp_plots$double_spline_plots[[8]]) /
  (nox_g | nox_h) /
  (pm_g | pm_h) /
  (comp_plots$boxplots[[1]] | comp_plots$boxplots[[2]])

# 7f Save combined plots
ggsave(wrap_elements(panel = di_2spline_plot_diSens) +
         labs(tag = 'Dissimilarity Index') +
         theme(plot.tag = element_text(size = rel(1)),
               plot.tag.position = 'bottom'),
       filename = paste0(final_plot_path, '/figS14_DIsens_allRegressionCurves_wTP_oct25.jpg'),
       width = 6, height = 8, scale = 1.2)

#################################################################### Outlier Sensitivity Analysis

# 7g Set up dataframe for plots comparing 1940 and 2010 so that predictions from
#    each row are separate variables
# 7g.i 1940 dataset
comp_plots <- regs_cbNS_outlierSens %>% filter(year == '1940') %>% ungroup() %>% 
  filter(! segMetric == 'nbrOne') %>% filter(! envExp == 'nox_autos_10tpy') %>% 
  dplyr::select(-plots,  -year, -nlME, -constraint) %>% 
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)
# 7g.ii 2010 dataset
results2010 <- regs_cbNS_outlierSens %>% filter(year == '2010') %>% ungroup() %>% 
  filter(! segMetric == 'nbrOne') %>% filter(! envExp == 'pm2.5_rm') %>% 
  filter(! envExp == 'nox_autos_10tpy') %>% 
  dplyr::select(-plots, -envExpLab, -segLab, -year, -nlME, -modelType, -constraint) %>% 
  rename(preds_2010 = preds,
         data_2010 = data,
         modelName_2010 = modelName) 
# 7g.iii Merge 1940 and 2010 dataset
comp_plots <- comp_plots %>% 
  full_join(results2010, by = c('plural_bin_imp', 'envExp', 'segMetric')) %>% 
  mutate(double_spline_plots = list(NA))

# 7h Source function to create plots
source(paste0(project.folder, 'functions/final_maps_plots.R'))

# 7i Create plots
# 7i.i Create two curve plots
for(i in 1:length(comp_plots$envExp)){
  comp_plots$double_spline_plots[[i]] <- spline_2yr_plot(row = i)
}
# 7i.ii Run boxplots as a separate plot for each seg metric, stratified by urban/rural
for(i in 1:length(comp_plots$envExp)){
  comp_plots$boxplots[[i]] <- boxplot_2yr(row = i)
}

# 7j Reorder plots to make arranging in multi-panel figures easier
comp_plots <- comp_plots %>% 
  arrange(segLab, envExp, plural_bin_imp)
comp_plots$modelName_1940

# 7k Combine plots into two multi-panel figures (one DI, one ICE) 
# 7k.i DI
pm_i <- comp_plots$double_spline_plots[[23]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
pm_j <- comp_plots$double_spline_plots[[24]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
nox_i <- comp_plots$double_spline_plots[[25]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
nox_j <- comp_plots$double_spline_plots[[26]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr")) 
di_2spline_plot_outlierSens <- 
  (comp_plots$double_spline_plots[[19]] | comp_plots$double_spline_plots[[20]]) /
  (comp_plots$double_spline_plots[[29]] | comp_plots$double_spline_plots[[30]]) /
  (nox_i | nox_j) /
  (pm_i | pm_j) /
  (comp_plots$boxplots[[25]] | comp_plots$boxplots[[26]])
# 7k.ii ICE
pm_k <- comp_plots$double_spline_plots[[7]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
pm_l <- comp_plots$double_spline_plots[[8]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
nox_k <- comp_plots$double_spline_plots[[9]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
nox_l <- comp_plots$double_spline_plots[[10]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr")) 
ice_2spline_plot_outlierSens <- 
  (comp_plots$double_spline_plots[[3]] | comp_plots$double_spline_plots[[4]]) /
  (comp_plots$double_spline_plots[[13]] | comp_plots$double_spline_plots[[14]]) /
  (nox_k | nox_l) /
  (pm_k | pm_l) /
  (comp_plots$boxplots[[9]] | comp_plots$boxplots[[10]])

# 7l Save combined plots
# 7l.i DI
ggsave(wrap_elements(panel = di_2spline_plot_outlierSens) +
         labs(tag = 'Dissimilarity Index') +
         theme(plot.tag = element_text(size = rel(1)),
               plot.tag.position = 'bottom'),
       filename = paste0(final_plot_path, '/figS9_DIoutlierSens_allRegressionCurves_wTP_oct25.jpg'),
       width = 6, height = 8, scale = 1.2)
# 7lf.ii ICE
ggsave(wrap_elements(panel = ice_2spline_plot_outlierSens) +
         labs(tag = 'Black/White + Ed ICE') +
         theme(plot.tag = element_text(size = rel(1)),
               plot.tag.position = 'bottom'),
       filename = paste0(final_plot_path, '/figS10_ICEoutlierSens_allRegressionCurves_wTP_oct25.jpg'),
       width = 6, height = 8, scale = 1.2)

# Now we run Neighbor metric outlier sens regression result plots for 1940 only 

# 7m Set up dataframe for nbrOne metric plot, both urban/rural
nbrOne_plots <- regs_cbNS_outlierSens %>% ungroup() %>% 
  filter(segMetric == 'nbrOne') %>% filter(! envExp == 'nox_autos_10tpy') %>% 
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)

# 7n Source function to create plots
source(paste0(project.folder, 'functions/final_maps_plots.R'))

# 7o Create plots
# 7o.i Create single curve plots
for(i in 1:length(nbrOne_plots$envExp)){
  nbrOne_plots$single_spline_plots[[i]] <- spline_1yr_plot(row = i)
}
# 7o.ii Run boxplots as a separate plot, stratified by urban/rural
for(i in 1:length(nbrOne_plots$envExp)){
  nbrOne_plots$boxplots[[i]] <- boxplot_1yr(row = i)
}

# 7p Reorder plots to make arranging in multi-panel figures easier
nbrOne_plots <- nbrOne_plots %>% 
  arrange(segLab, envExp, plural_bin_imp)
nbrOne_plots$modelName_1940

# 7q Combine plots into one multi-panel figure
pm_m <- nbrOne_plots$single_spline_plots[[7]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
pm_n <- nbrOne_plots$single_spline_plots[[8]] + ylab(bquote("PM"[2.5]~"("*µg~m^-3*")")) 
nox_m <- nbrOne_plots$single_spline_plots[[9]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
nox_n <- nbrOne_plots$single_spline_plots[[10]] + ylab(bquote("Log 10 Vehicle NO"[x]~"(tons/yr)")) 
nbrOne_1spline_plot_outlierSens <- 
  (nbrOne_plots$single_spline_plots[[3]] | nbrOne_plots$single_spline_plots[[4]]) /
  (nbrOne_plots$single_spline_plots[[13]] | nbrOne_plots$single_spline_plots[[14]]) /
  (nox_m | nox_n) /
  (pm_m | pm_n) /
  (nbrOne_plots$boxplots[[9]] | nbrOne_plots$boxplots[[10]])

# 7r Save combined plot
ggsave(wrap_elements(panel = nbrOne_1spline_plot_outlierSens) +
         labs(tag = 'Neighbor-based Segregation Metric') +
         theme(plot.tag = element_text(size = rel(1)),
               plot.tag.position = 'bottom'),
       filename = paste0(final_plot_path, '/figS11_neighborOutlierSens_allRegressionCurves_wTP_oct25.jpg'),
       width = 6, height = 8, scale = 1.2)

#################################################################### PP 1940 Counties Sensitivity Analysis

# 7s Set up dataframe for PP 1940 counties plots, both urban/rural
# 7s.i 1940 dataset
comp_plots <- regs_cbNS_ppSens %>% filter(year == '1940') %>% ungroup() %>% 
  filter(!str_detect(envExp, 'Buffer|idw')) %>% filter(!segMetric == 'nbrOne') %>% 
  dplyr::select(-plots,  -year, -nlME, -constraint) %>% 
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)
# 7s.ii 2010 dataset
results2010_ppSens <- regs_cbNS_ppSens %>% filter(year == '2010') %>% ungroup() %>% 
  filter(!str_detect(envExp, 'Buffer|idw')) %>% filter(!segMetric == 'nbrOne') %>% 
  dplyr::select(-plots, -envExpLab, -segLab, -year, -nlME, -modelType, -constraint) %>% 
  rename(preds_2010 = preds,
         data_2010 = data,
         modelName_2010 = modelName)
# 7s.iii Merge 1940 and 2010 dataset
comp_plots <- comp_plots %>% 
  full_join(results2010_ppSens, by = c('plural_bin_imp', 'envExp', 'segMetric')) %>% 
  mutate(double_spline_plots = list(NA))
# 7s.iv Prepare neighbor dataset
nbrOne_plots <- regs_cbNS_ppSens %>% ungroup() %>% 
  filter(segMetric == 'nbrOne') %>% filter(!str_detect(envExp, 'Buffer|idw')) %>%
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)
# 7s.v Review n for each model to include in figure legend
dim(comp_plots$data_1940[[1]]) # n = 36 Rural DI 1940
dim(comp_plots$data_1940[[2]]) # n = 100 Rural ICE 1940
dim(comp_plots$data_1940[[3]]) # n = 39 Urban DI 1940
dim(comp_plots$data_1940[[4]]) # n = 40 Urban ICE 1940
dim(comp_plots$data_2010[[1]]) # n = 195 Rural DI 2010
dim(comp_plots$data_2010[[2]]) # n = 428 Rural ICE 2010
dim(comp_plots$data_2010[[3]]) # n = 282 Urban DI 2010
dim(comp_plots$data_2010[[4]]) # n = 287 Urban ICE 2010
dim(nbrOne_plots$data_1940[[1]]) # n = 62 Rural Neighbor 1940
dim(nbrOne_plots$data_1940[[2]]) # n = 38 Urban Neighbor 1940

# 7t Source function to create plots
source(paste0(project.folder, 'functions/final_maps_plots.R'))

# 7u Create plots
# 7u.i Create double curve plots
for(i in 1:length(comp_plots$envExp)){
  comp_plots$double_spline_plots[[i]] <- spline_2yr_plot(row = i)
}
# 7u.ii Run boxplots as a separate plot, stratified by urban/rural
for(i in 1:length(comp_plots$envExp)){
  comp_plots$boxplots[[i]] <- boxplot_2yr(row = i)
}
# 7u.iii Create single curve plots
for(i in 1:length(nbrOne_plots$envExp)){
  nbrOne_plots$single_spline_plots[[i]] <- spline_1yr_plot(row = i)
}
# 7u.iv Run boxplots for neighbor plots separately, stratified by urban/rural
for(i in 1:length(nbrOne_plots$envExp)){
  nbrOne_plots$boxplots[[i]] <- boxplot_1yr(row = i)
}

# 7v Reorder plots to make arranging in multi-panel figures easier
# 7v.i DI and ICE
comp_plots <- comp_plots %>% 
  arrange(segLab, envExp, plural_bin_imp)
comp_plots$modelName_1940
# 7v.ii Neighbor index
nbrOne_plots <- nbrOne_plots %>% 
  arrange(segLab, envExp, plural_bin_imp)
nbrOne_plots$modelName_1940

# 7w Combine plots into one multi-panel figure
pp_2spline_plot_pp1940Sens <- 
  (comp_plots$double_spline_plots[[3]] + ggtitle('DI: Rural') + xlab('Dissimilarity Index') + theme(text = element_text(size = 13)) | comp_plots$double_spline_plots[[4]] + ggtitle('DI: Urban') + xlab('Dissimilarity Index') + theme(text = element_text(size = 13))) /
  (comp_plots$double_spline_plots[[1]] + ggtitle('ICE: Rural') + xlab('Black/White + Ed ICE') + theme(text = element_text(size = 13))| comp_plots$double_spline_plots[[2]] + ggtitle('ICE: Urban') + xlab('Black/White + Ed ICE') + theme(text = element_text(size = 13))) /
  (nbrOne_plots$single_spline_plots[[1]] + ggtitle('Neighbor: Rural') + xlab('Neighbor Metric') + theme(text = element_text(size = 13)) | nbrOne_plots$single_spline_plots[[2]] + ggtitle('Neighbor: Urban') + xlab('Neighbor Metric') + theme(text = element_text(size = 13))) 

# 7x Save combined plot
ggsave(pp_2spline_plot_pp1940Sens,
       filename = paste0(final_plot_path, '/figS16_pp1940Sens_allRegressionCurves_wTP_oct25.jpg'),
       width = 8, height = 8, scale = 1.2)

#################################################################### OGW 1940 Counties Sensitivity Analysis

# 7y Set up dataframe for OGW 1940 counties plots, both urban/rural
# 7y.i 1940 dataset
comp_plots <- regs_cbNS_ogwSens %>% filter(year == '1940') %>% ungroup() %>% 
  filter(!str_detect(envExp, 'Buffer|idw')) %>% filter(!segMetric == 'nbrOne') %>% 
  dplyr::select(-plots,  -year, -nlME, -constraint) %>% 
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)
# 7y.ii 2010 dataset
results2010_ogwSens <- regs_cbNS_ogwSens %>% filter(year == '2010') %>% ungroup() %>% 
  filter(!str_detect(envExp, 'Buffer|idw')) %>% filter(!segMetric == 'nbrOne') %>% 
  dplyr::select(-plots, -envExpLab, -segLab, -year, -nlME, -modelType, -constraint) %>% 
  rename(preds_2010 = preds,
         data_2010 = data,
         modelName_2010 = modelName)
# 7y.iii Merge 1940 and 2010 dataset
comp_plots <- comp_plots %>% 
  full_join(results2010_ogwSens, by = c('plural_bin_imp', 'envExp', 'segMetric')) %>% 
  mutate(double_spline_plots = list(NA))
# 7y.iv Prepare neighbor dataset
nbrOne_plots <- regs_cbNS_ogwSens %>% ungroup() %>% 
  filter(segMetric == 'nbrOne') %>% filter(!str_detect(envExp, 'Buffer|idw')) %>%
  rename(preds_1940 = preds,
         data_1940 = data,
         modelName_1940 = modelName)
# 7y.v Review n for each model to include in figure legend
dim(comp_plots$data_1940[[1]]) # n = 315 Rural DI 1940
dim(comp_plots$data_1940[[2]]) # n = 575 Rural ICE 1940
dim(comp_plots$data_1940[[3]]) # n = 139 Urban DI 1940
dim(comp_plots$data_1940[[4]]) # n = 145 Urban ICE 1940
dim(comp_plots$data_2010[[1]]) # n = 520 Rural DI 2010
dim(comp_plots$data_2010[[2]]) # n = 935 Rural ICE 2010
dim(comp_plots$data_2010[[3]]) # n = 441 Urban DI 2010
dim(comp_plots$data_2010[[4]]) # n = 452 Urban ICE 2010
dim(nbrOne_plots$data_1940[[1]]) # n = 450 Rural Neighbor 1940
dim(nbrOne_plots$data_1940[[2]]) # n = 143 Urban Neighbor 1940

# 7z Source function to create plots
source(paste0(project.folder, 'functions/final_maps_plots.R'))

# 7aa Create plots
# 7aa.i Create double curve plots
for(i in 1:length(comp_plots$envExp)){
  comp_plots$double_spline_plots[[i]] <- spline_2yr_plot(row = i)
}
# 7aa.ii Run boxplots as a separate plot, stratified by urban/rural
for(i in 1:length(comp_plots$envExp)){
  comp_plots$boxplots[[i]] <- boxplot_2yr(row = i)
}
# 7aa.iii Create single curve plots
for(i in 1:length(nbrOne_plots$envExp)){
  nbrOne_plots$single_spline_plots[[i]] <- spline_1yr_plot(row = i)
}
# 7aa.iv Run boxplots for neighbor plots separately, stratified by urban/rural
for(i in 1:length(nbrOne_plots$envExp)){
  nbrOne_plots$boxplots[[i]] <- boxplot_1yr(row = i)
}

# 7bb Reorder plots to make arranging in multi-panel figures easier
# 7bb.i DI and ICE
comp_plots <- comp_plots %>% 
  arrange(segLab, envExp, plural_bin_imp)
comp_plots$modelName_1940
# 7bb.ii Neighbor index
nbrOne_plots <- nbrOne_plots %>% 
  arrange(segLab, envExp, plural_bin_imp)
nbrOne_plots$modelName_1940

# 7cc Combine plots into one multi-panel figure
ogw_2spline_plot_ogw1940Sens <- 
  (comp_plots$double_spline_plots[[3]] + ggtitle('DI: Rural') + xlab('Dissimilarity Index') + theme(text = element_text(size = 13)) | comp_plots$double_spline_plots[[4]] + ggtitle('DI: Urban') + xlab('Dissimilarity Index') + theme(text = element_text(size = 13))) /
  (comp_plots$double_spline_plots[[1]] + ggtitle('ICE: Rural') + xlab('Black/White + Ed ICE') + theme(text = element_text(size = 13))| comp_plots$double_spline_plots[[2]] + ggtitle('ICE: Urban') + xlab('Black/White + Ed ICE') + theme(text = element_text(size = 13))) /
  (nbrOne_plots$single_spline_plots[[1]] + ggtitle('Neighbor: Rural') + xlab('Neighbor Metric') + theme(text = element_text(size = 13)) | nbrOne_plots$single_spline_plots[[2]] + ggtitle('Neighbor: Urban') + xlab('Neighbor Metric') + theme(text = element_text(size = 13))) 

# 7dd Save combined plot
ggsave(ogw_2spline_plot_ogw1940Sens,
       filename = paste0(final_plot_path, '/figS17_ogw1940Sens_allRegressionCurves_wTP_oct25.jpg'),
       width = 8, height = 8, scale = 1.2)


####*************************************************************************************
#### 8: Figures 3, S7 - % Black and % White by Consistently High and Low Segregation #### 
####*************************************************************************************

# DI quartile breaks (from data_persmarg_breaks_strat df in script 2_03 line 136)
# Rural: min = 0; Q1 = 0.2284755; Q2 = 0.3234086; Q3 = 0.4341063; max = 0.9265899
# Urban: min = 0; Q1 = 0.3220073; Q2 = 0.4174943; Q3 = 0.5144356; max = 0.8684154

# 8a Create dataframe with needed 2010 vars 
f6_2010 <- data %>% 
  dplyr::select(year, st_cnty_yr_id, totPop, blackPop, nhwhitePop,
                meanPM2.5, fospp_count, ogw_count, nox_autos, di) %>% 
  filter(year == 2010) %>% 
  rename(di_cont = di)

# 8b Identify counties that are consistently segregated and comparison counties
#    We make a three level and two level variable here, but decided to use the 
#    two level variable because it was easier to interpret in the plots
inequality_df <- senke_data_strat %>% 
  full_join(f6_2010, by = 'st_cnty_yr_id') %>% 
  mutate(pers_seg = ifelse(di == 4 & di_1940_ws == 4, 1, 0),
         pers_int = ifelse(di == 1 & di_1940_ws == 1, 1, 0)) %>% 
  mutate(pers_seg_3cat = case_when(
    pers_seg == 1 ~ "Consistently High Segregation",
    plural_bin_imp == "Rural" & di_cont > 0.4341063 & pers_seg == 0 ~ "High Segregation",
    plural_bin_imp == "Urban" & di_cont > 0.5144356 & pers_seg == 0  ~ "High Segregation",
    plural_bin_imp == "Rural" & di_cont <= 0.4341063 & pers_seg == 0  ~ "Low Segregation",
    plural_bin_imp == "Urban" & di_cont <= 0.5144356 & pers_seg == 0  ~ "Low Segregation",
    )) %>% 
  mutate(pers_seg_2cat = case_when(
    pers_seg == 1 ~ "Consistently High Segregation",
    pers_int == 1 ~ "Consistently Low Segregation"
  )) %>% 
  filter(!is.na(pers_seg_2cat))

# 8c Calculate percent black and white
inequality_df <- inequality_df %>% 
  mutate(perc_black = 100*(blackPop/totPop),
         perc_white = 100*(nhwhitePop/totPop))

# 8d Pivot longer and nest
inequality_df <- inequality_df %>% 
  pivot_longer(cols = c('meanPM2.5', 'fospp_count', 'nox_autos', 'ogw_count'), 
               names_to = 'envExp', values_to = 'expEst') %>% 
  pivot_longer(cols = c('perc_black', 'perc_white'),
               names_to = 'perc_race', values_to = 'perc_race_value') %>% 
  group_by(envExp, perc_race, plural_bin_imp) %>% 
  nest()

# 8e Add labeling variables
inequality_df <- inequality_df %>% 
  ungroup() %>% 
  mutate(
    envExpLab = case_when(
      envExp == 'fospp_count' ~ 'Fossil Fuel Plants (count)',
      envExp == 'ogw_count' ~ 'Oil/Gas Wells (count)',
      envExp == 'nox_autos' ~ 'Vehicle NOx (tons/yr)',
      envExp == 'meanPM2.5' ~ 'PM2.5 (ug/m^3)'),
    raceLab = case_when(
      perc_race == 'perc_black' ~ '% Black',
      perc_race == 'perc_white' ~ '% Non-Hispanic White'),
    low_lim = map_dbl(.x = data, ~min(.x$expEst, na.rm = T)),
    high_lim = map_dbl(.x = data, ~max(.x$expEst, na.rm = T)),
    high_lim_adj = c(7.0, 7.0, 1.5, 1.5, 30, 30, 2650, 2650,
                     7.0, 7.0, 16, 16, 1500, 1500, 600, 600)) 

# 8f Create plots for each dataframe
# 8f.i Source function to create plots
source(paste0(project.folder, 'functions/final_maps_plots.R'))
# 8f.ii Create plots
inequality_df$plot <- list(NA)
for(i in 1:length(inequality_df$envExp)){
  inequality_df$plot[[i]] <- perc_race_plot(row = i)
}
# 8f.iii Add legend to bottom left plots
inequality_df$plot[[1]] = inequality_df$plot[[1]] + theme(legend.position = "bottom", text = element_text(size = 13)) + labs(color = "2010 Segregation: ")
inequality_df$plot[[9]] = inequality_df$plot[[9]] + theme(legend.position = "bottom", text = element_text(size = 13)) + labs(color = "2010 Segregation: ")

# 8g Combine plots into two multi-panel figures (one Rural, one Urban) 
# 8g.i Rural
rural_inequality_plot <- 
  (inequality_df$plot[[3]] | inequality_df$plot[[4]]) /
  (inequality_df$plot[[7]] | inequality_df$plot[[8]]) /
  (inequality_df$plot[[5]] | inequality_df$plot[[6]]) /
  (inequality_df$plot[[1]] | inequality_df$plot[[2]]) 
# 8g.ii Urban
urban_inequality_plot <- 
  (inequality_df$plot[[11]] | inequality_df$plot[[12]]) /
  (inequality_df$plot[[15]] | inequality_df$plot[[16]]) /
  (inequality_df$plot[[13]] | inequality_df$plot[[14]]) /
  (inequality_df$plot[[9]] | inequality_df$plot[[10]]) 

# 8h Save out plots
ggsave(rural_inequality_plot,
       filename = paste0(final_plot_path, '/figS7_percBlackWhite_rural_oct25.jpg'),
       width = 9.1, height = 8.1, scale = 1.2)
ggsave(urban_inequality_plot,
       filename = paste0(final_plot_path, '/fig3_percBlackWhite_urban_oct25.jpg'),
       width = 9.3, height = 8.3, scale = 1.2)

####**************************************************************************
#### 9: Figure S1 - Map of residuals for models with high Morans I values #### 
####**************************************************************************

# 9a Keep only needed vars in 2010 county shapefile
county2010_tc_resids <- county2010_tc %>% 
  dplyr::select(-variable, -value)

# 9b Select only models for mapping
#    Chose the oil/gas wells model with the highest moran's i and the pm2.5 model
#    with highest moran's i 
# 9b.i Load moran's i values
morans <- read_rds(paste0(model_path, 'moran_wTP_oct25.RDS'))
# 9b.ii Select models with worst moran's i values
mods_resid_map <- regs_cbNS %>% 
  filter(modelName == "meanPM2.5_raceEduICE_hsd10_1940_Rural" | 
           modelName == "ogw_count_raceEduICE_hsd10_2010_Rural")

# 9c Merge analysis data with shapefile
pm_resids <- mods_resid_map$data[[2]] %>% 
  left_join(county1940_2, by = c('st_cnty_yr_id' = 'jnhgis40'))
og_resids <- mods_resid_map$data[[1]] %>% 
  left_join(county2010_tc_resids, by = c('st_cnty_yr_id' = 'GEOID'))

# 9d Extract DHARMa residuals for each model and add to merge dataframes
dharma_pm = DHARMa::simulateResiduals(mods_resid_map$nlME[[2]])
pm_resids$Residuals = residuals(dharma_pm)
dharma_og = DHARMa::simulateResiduals(mods_resid_map$nlME[[1]])
og_resids$Residuals = residuals(dharma_og)

# 9e Create maps
# 9e.i PM2.5
pm_resid_map <- pm_resids %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = Residuals),
          color = 'gray80', linewidth = 0.1) +
  scale_fill_viridis(option = "mako") +
  labs(subtitle = bquote("A: Model Residuals - Black/White + Ed ICE & PM"[2.5]~", Rural 1940")) +
  geom_sf(fill = "transparent", color = 'black', linewidth = 0.3,
          data = county2010_tc_resids) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        legend.position = 'bottom',
        text = element_text(size = 13)) 
pm_resid_map
# 9e.ii Oil/gas wells
og_resid_map <- og_resids %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = Residuals),
          color = 'gray80', linewidth = 0.1) +
  scale_fill_viridis(option = "mako") +
  labs(subtitle = 'B: Model Residuals - Black/White + Ed ICE & Oil/gas Wells, Rural 2010') +
  geom_sf(fill = "transparent", color = 'black', linewidth = 0.3,
          data = county2010_tc_resids) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        legend.position = 'bottom',
        text = element_text(size = 13)) 
og_resid_map

# 9f Combine maps into single plot
resids_map <- pm_resid_map / og_resid_map

# 9g Save out map
ggsave(resids_map,
       filename = paste0(final_plot_path, '/figS1_model_residuals_map_oct25.jpg'),
       width = 9, height = 8, scale = 1.2)













