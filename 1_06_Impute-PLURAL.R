# Impute Missing PLURAL Values for Rural/Urban Definition
# Env Exposures & Segregation 1940 to 2010
# Jenni A. Shearston 
# Updated 09/20/2025

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Impute missing PLURAL values

####**************
#### N: Notes ####
####**************

# Na Description
# In this script we impute PLURAL values for counties that did not have a Census 
# place. These counties have missing values because they had missing Census places, 
# and PLURAL was based on Census places. To determine county-level PLURAL values 
# we averaged the value of all places within the county (script 1_04). We use data 
# compiled by Elizabeth Blake (EB), who reviewed all counties missing a PLURAL value  
# using Google maps, old 1940s maps, and present day RUCA values (script 1_05), to  
# provide context & assist with imputation. To impute PLURAL values for counties 
# missing a place we take the average PLURAL value of all the county's neighbors. We 
# do not include missing or previously imputed values in the imputation calculations.
# Of note, this means that for a few counties, such as St Bernard LA and Matthews
# VA in 1940, the county is imputed with the value of one of its neighboring
# counties, because all other neighboring counties were also missing values. For
# five counties that did not successfully impute using the st_touches function,
# I manually identified their neighbors in QGIS and averaged those (non-missing,
# non-previously-imputed) values. I then compared the binary cutoff based on all
# imputed values with the notes EB made as a gut check. In general, these imputed
# values matched (though not always). 

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
merged_data_path <- paste0(project.folder, 'data/merged_data/')
rural_data_path <- paste0(project.folder, 'data/uhl_ruralurban_data/')
prelim_plot_path <- paste0(project.folder, 'outputs/prelim_plots/plots_for_dichot_plural/')
spatial_data_path <- paste0(project.folder, 'data/county_shapefiles/')

# 0d Load rural/urban data
#    Note: CRS is USA_Contiguous_Albers_Equal_Area_Conic
rural_county<- read_fst(paste0(rural_data_path, 'plural_1940_2010_clean.fst'))
rural_mis <- read_csv(paste0(merged_data_path, 'counties_missing_plural_wdetails_from_elizabeth.csv'))
rural1940_place <- st_read(paste0(rural_data_path, 'PLURAL_SHP/plural_indices_scaled_across_years_1940.shp'))
rural2010_place <- st_read(paste0(rural_data_path, 'PLURAL_SHP/plural_indices_scaled_across_years_2010.shp'))
ruca <- readxl::read_excel(paste0(project.folder, 'data/ruca2010revised.xlsx'),
                           sheet = 'Data', range = 'A2:I74004') %>% 
  janitor::clean_names() %>% rename(fips = state_county_fips_code, state = select_state,
                                    county_name = select_county, ruca = primary_ruca_code_2010,
                                    area = land_area_square_miles_2010)
  
# 0e Load county shapefiles
#    Note: CRS is USA_Contiguous_Albers_Equal_Area_Conic
county1940 <- st_read(paste0(spatial_data_path, 
                             'FINAL_1940_county_shapefile_clean/counties_contiguous_1940.shp')) %>% 
  janitor::clean_names()
county2010 <- st_read(paste0(spatial_data_path, 
                             'FINAL_2010_county_shapefile_clean/counties_contiguous_2010.shp')) %>% 
  janitor::clean_names()
cwalk <- read_fst(paste0(spatial_data_path, 'crosswalk/FINAL_county_crosswalk_clean_1940-2010.fst'))

####*************************************
#### 1: Impute missing PLURAL values #### 
####*************************************

# 1a Split plural into 1940 and 2010 dataframes
plural40 <- rural_county %>% filter(year == '1940')
plural10 <- rural_county %>% filter(year == '2010')

# 1b Add plural data to county shapefiles
county1940 <- county1940 %>% left_join(plural40, by = c('jnhgis40' = 'st_cnty_yr_id'))
county2010 <- county2010 %>% left_join(plural10, by = c('jfips10' = 'st_cnty_yr_id'))

# 1c Identify counties that touch each other (e.g., identify neighbors)
# 1c.i Identify neighbors
index40 <- st_touches(county1940, county1940)
# 1c.ii Check one county visually in QGIS: Abbeville SC 1940
index40[1]         # 59, 851, 1111, 1113, 1574, 1812
county1940[59,]    # Anderson SC, correct
county1940[851,]   # Elbert GA, correct
county1940[1111,]  # Greenville SC, correct
county1940[1113,]  # Greenwood SC, correct
county1940[1574,]  # Laurens SC, correct
county1940[1812,]  # McCormick SC, correct

# 1d Determine average PLURAL value of all counties that share a border with a
#    county missing its PLURAL value
# 1d.i Check county that is missing plural_cont in 1940: Alpine CA (index 52)
county1940$plural_cont[52]
# 1d.ii Fill missing counties with average plural_cont of neighbors
county1940 <- county1940 %>% 
  mutate(plural_imp = ifelse(is.na(plural_cont), 
                       apply(index40, 1, function(i){mean(.$plural_cont[i], na.rm = T)}),
                       plural_cont))
# 1d.iii Check Alpine CA example again
county1940$plural_imp[52]
# 1d.iv Check counties that remain missing (n = 6)
#       Kings County NY -- will assign value of Manhattan
#       Dukes County MA -- island; EB suggests Nantucket may be similar?
#       Mathews County VA -- includes an island, might be a problem for id neighbors
#       Prince George County VA -- not sure why missing
#       St Bernard County LA -- tons of little islands in gulf
#       Warwick County VA -- not sure why missing
still_mis_1940 = as.data.frame(county1940) %>% dplyr::select(-geometry) %>% filter(is.na(plural_imp))

# 1e Repeat for 2010
# 1e.i Identify neighbors
index10 <- st_touches(county2010, county2010)
# 1e.ii Fill missing counties with avg plural_cont of neighbors
county2010 <- county2010 %>% 
  mutate(plural_imp = ifelse(is.na(plural_cont), 
                             apply(index10, 1, function(i){mean(.$plural_cont[i], na.rm = T)}),
                             plural_cont))
# 1e.iii Check counties that remain missing (n = 0)
still_mis_2010 <- as.data.frame(county2010) %>% dplyr::select(-geometry) %>% filter(is.na(plural_imp))

# 1f Merge new imputed values and notes from EB
# 1f.i Add new imputed values to full rural_county dataset
imp_plural_1940 <- as.data.frame(county1940) %>% dplyr::select(jnhgis40, plural_imp)
imp_plural_2010 <- as.data.frame(county2010) %>% dplyr::select(jfips10, plural_imp)
rural_county_wimp <- rural_county %>% 
  left_join(imp_plural_1940, by = c('st_cnty_yr_id' = 'jnhgis40')) %>% 
  left_join(imp_plural_2010, by = c('st_cnty_yr_id' = 'jfips10'))
# 1f.ii Add notes from EB
rural_mis_tomerge <- rural_mis %>% dplyr::select(st_cnty_yr_id, plural_notes)
rural_county_wimp <- rural_county_wimp %>% 
  left_join(rural_mis_tomerge, by = 'st_cnty_yr_id')
# 1f.iii Combine two plural_imp variables into one column and keep only plural_notes
#        var from EB
rural_county_wimp <- rural_county_wimp %>% 
  mutate(plural_imp = ifelse(year == '1940', plural_imp.x, plural_imp.y)) %>% 
  mutate(plural_imp = ifelse(is.na(plural_imp), plural_cont, plural_imp),
         plural_notes = plural_notes.y) %>% 
  dplyr::select(-plural_imp.x, -plural_imp.y, -plural_notes.x, -plural_notes.y)

# 1g Replace NYC outer boroughs with value of Manhattan
#    Manhattan PLURAL value = 0.03151749 (1940), 0.01325047 (2010)
rural_county_wimp <- rural_county_wimp %>% 
  mutate(plural_imp = case_when(
    str_detect(plural_notes, 'NYC outer boroughs;') & year == '1940' ~ 0.03151749,
    str_detect(plural_notes, 'NYC outer boroughs;') & year == '2010' ~ 0.01325047,
    TRUE ~ plural_imp
  ))

# 1h Manually impute other counties by identifying neighbors visually in QGIS
#       Dukes County MA -- island; EB suggests Nantucket may be similar?
#       Mathews County VA -- includes an island, might be a problem for id neighbors
#       Prince George County VA -- not sure why missing
#       St Bernard County LA -- tons of little islands in gulf
#       Warwick County VA -- not sure why missing
# 1h.i Write out 1940 shapefile with plural values to be used in QGIS
county1940 %>% st_write(paste0(merged_data_path, 'county1940wplural.shp'))
# 1h.ii Create imputed values for five counties missing plural_imp
#       Note: I visually identified neighboring counties and looked up their 
#             plural_cont values, listed below
dukes <- ((0.714923046688146 + 0.649023082216963 + 0.426940158985797 + 
            0.313943167965568) / 4)
matthews <- 0.754752749600435
stbernard <- 0.342571368548384
princegeorge <- ((0.718461353440811 + 0.724229093366824 + 0.754191243000172 +
                    0.402716617876103 + 0.597283673249989 + 0.638097407467531) / 6)
warwick <- ((0.695319318655086 + 0.572782034879074 + 0.317107316727361) / 3)
# 1h.iii Add imputed values to dataframe
rural_county_wimp <- rural_county_wimp %>% 
  mutate(plural_imp = case_when(
    str_detect(cnty_name, 'Dukes') & year == '1940' ~ dukes,
    str_detect(cnty_name, 'Matthews') & year == '1940' ~ matthews,
    str_detect(cnty_name, 'LouisianaSt Bernard') & year == '1940' ~ stbernard,
    str_detect(cnty_name, 'VirginiaPrince George') & year == '1940' ~ princegeorge,
    str_detect(cnty_name, 'Warwick') & year == '1940' ~ warwick,
    TRUE ~ plural_imp
  ))

# 1i Assign rural/urban cutoff using new PLURAL values from neighbor averaging
rural_county_wimp <- rural_county_wimp %>% 
  mutate(plural_bin_imp = case_when(
    plural_imp >= 0.65 ~ 'Rural',
    plural_imp < 0.65 ~ 'Urban'))

# 1j Compare suggestions from EB with new rural/urban cutoff
#    I reviewed these observations manually using View()
#    The majority of urban/rural decisions match EB comments, but not all, including:
#    Howard County MD, Franklin MN, Plaquemines LA, Union TN, Grainger TN,
#    Powhatan VA
View(rural_county_wimp)

# 1k Save revised datset
rural_county_wimp %>% write_fst(paste0(rural_data_path, 'plural_1940_2010_clean_imputed.fst'))
rural_county_wimp %>% write_csv(paste0(rural_data_path, 'plural_1940_2010_clean_imputed.csv'))





