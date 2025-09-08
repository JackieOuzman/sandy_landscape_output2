# This file is for importing plant data and merge into one file.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

### create a list of files
### create a list of headers
### put file name into df

#################################################################################

# raw data files -------------------------------------------------------


sandy_landscape_folder <- "H:/Output-2/Site-Data/"
site <- "3. SSO2_Wharminda-Masters/"
raw_data <- "Jackies_working/"

path_name<- paste0(sandy_landscape_folder,site,raw_data) 

list_sim_out_file <-
  list.files(
    path = path_name,
    pattern = ".xlsx" , #later we should have a series of csv files ".csv",
    all.files = FALSE,
    full.names = FALSE
  )
list_sim_out_file



# list of raw data  worksheet -------------------------------------------------------
file <- "Wharminda_all _v2.xlsx"


excel_sheets(paste0(path_name, file))

# "APAL soils tidy long"     
# "2.site notes tidy long"    
# "2.GRDC_sands_Verran_PS24tidy lo"
# "2.In-Season soilMoist Tidy long" 

# "3.plant est tidy long"    
# "3.Biomass tidy long"            
# "3.Tillers tidy long"  
# "3.NDVI tidy long"  

# "4_yield tidy long"

# "5.yield quality tidy v1 long old" 
# "5.yield quality tidy v3" #this is the newset one that Brett corrected


# "Growth stages"   
# "2024 notes"                     





## plant data at a PLOT level
plant_count <- read_excel(paste0(path_name, file), sheet = "3.plant est tidy long" , col_types = "text" )
plant_biomass <- read_excel(paste0(path_name, file), sheet = "3.Biomass tidy long" , col_types = "text" )
plant_Tillers <- read_excel(paste0(path_name, file), sheet = "3.Tillers tidy long" , col_types = "text" )
plant_est_NDVI <- read_excel(paste0(path_name, file), sheet = "3.NDVI tidy long" , col_types = "text" )
Yield <- read_excel(paste0(path_name, file), sheet = "4_yield tidy long" , col_types = "text" )
#Yield_quaility <- read_excel(paste0(path_name, file), sheet = "5.yield quality tidy v1 long" , col_types = "text" )
Yield_quaility_2 <- read_excel(paste0(path_name, file), sheet = "5.yield quality tidy v3" , col_types = "text" )




plant_stages <- read_excel(paste0(path_name, file), sheet = "Growth stages" , 
                           col_types = "text" , 
                           skip = 2
                           )


# merge the plant_count and biomass plant  df-----------------------------------
dim(plant_count) #16
names(plant_count)


unique(plant_count$date)
plant_count$date <- as.Date(plant_count$date,format = "%d/%m/%Y", origin = "1899-12-30")
unique(plant_count$date)

dim(plant_biomass) #16
names(plant_biomass)
unique(plant_biomass$date)  
plant_biomass$date <- as.numeric(plant_biomass$date)
plant_biomass$date <- as.Date(plant_biomass$date, origin = "1899-12-30")  


plant_count_biomass <- bind_rows(plant_count, plant_biomass)
names(plant_count_biomass)

rm(plant_count, plant_biomass)

# merge the plant_Tillers -------------------------------------------------------

dim(plant_Tillers) #16
names(plant_Tillers)

unique(plant_Tillers$date)  
plant_Tillers$date <- as.numeric(plant_Tillers$date)
plant_Tillers$date <- as.Date(plant_Tillers$date, origin = "1899-12-30")  


dim(plant_count_biomass) #16
names(plant_count_biomass)



plant_count_biomass_tillers <- bind_rows(plant_count_biomass, plant_Tillers)

rm (plant_count_biomass, plant_Tillers )


# merge NDVI -------------------------------------------------------

dim(plant_est_NDVI) #16
names(plant_est_NDVI)
unique(plant_est_NDVI$date)  
plant_est_NDVI$date <- as.numeric(plant_est_NDVI$date)
plant_est_NDVI$date <- as.Date(plant_est_NDVI$date, origin = "1899-12-30") 



plant_count_biomass_tillers_NDVI <- bind_rows(plant_count_biomass_tillers, plant_est_NDVI)

rm (plant_count_biomass_tillers, plant_est_NDVI )


# merge the yield -------------------------------------------------------
dim(Yield) #16
names(Yield)


dim(Yield) #16
names(Yield)
unique(Yield$date)  
Yield$date <- as.numeric(Yield$date)
Yield$date <- as.Date(Yield$date, origin = "1899-12-30") 



plant_count_biomass_tillers_NDVI_yld <- bind_rows(plant_count_biomass_tillers_NDVI, Yield)
rm (plant_count_biomass_tillers_NDVI, Yield )

# merge the yield quality -----------------------------------------------------

# dim(Yield_quaility) #17
# names(Yield_quaility)
# unique(Yield_quaility$date)  
# Yield_quaility$date <- as.numeric(Yield_quaility$date)
# Yield_quaility$date <- as.Date(Yield_quaility$date, origin = "1899-12-30") 
# 
# 
# 
# plant_count_biomass_tillers_NDVI_yld_quaility <- bind_rows(plant_count_biomass_tillers_NDVI_yld, Yield_quaility)
# 
# rm (plant_count_biomass_tillers_NDVI_yld, Yield_quaility )


################################################################################
plant_count_biomass_tillers_NDVI_yld_quaility <- plant_count_biomass_tillers_NDVI_yld

# merge the yield quality 2 -----------------------------------------------------

dim(Yield_quaility_2) #17
names(Yield_quaility_2)
unique(Yield_quaility_2$date)  
Yield_quaility_2$date <- as.numeric(Yield_quaility_2$date)
Yield_quaility_2$date <- as.Date(Yield_quaility_2$date, origin = "1899-12-30") 


plant_count_biomass_tillers_NDVI_yld_quaility_2 <- bind_rows(plant_count_biomass_tillers_NDVI_yld_quaility, Yield_quaility_2)

rm (plant_count_biomass_tillers_NDVI_yld_quaility, Yield_quaility_2 )


################################################################################

plant_count_biomass_tillers_NDVI_yld_quaility_2$value <- as.numeric(plant_count_biomass_tillers_NDVI_yld_quaility_2$value)




# assign sampling dates to plant stage and vice versa

unique(plant_count_biomass_tillers_NDVI_yld_quaility_2$date)

NA_DATES <-plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% 
  filter (is.na(date))
## looks like its just the yield data that is missing dates - I am waiting for Kenton / Brett I added as 31/12/2024
NA_DATES


# #convert to number first or date
# list_of_sampling_dates$date <- as.numeric(list_of_sampling_dates$date)
# plant_stages$`start date` <- as.numeric(plant_stages$`start date`)
# plant_stages$`end date` <- as.numeric(plant_stages$`end date`) 
# plant_stages
# 
# 
# plant_count_biomass_tillers_NDVI_yld$date <- as.numeric(plant_count_biomass_tillers_NDVI_yld$date)
# plant_count_biomass_tillers_NDVI_yld$date <- as.Date(plant_count_biomass_tillers_NDVI_yld$date, origin = "1899-12-30")
 
 sort(unique(plant_count_biomass_tillers_NDVI_yld_quaility_2$date))
 str(plant_count_biomass_tillers_NDVI_yld_quaility_2$date)

 NA_DATES <-plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% 
   filter (is.na(date))
 NA_DATES
 
 plant_count_biomass_tillers_NDVI_yld_quaility_2 <- plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% mutate(
   After_Phenology_stage = case_when(
     date <        as.Date("2024-06-06") ~ "PreSowing",
     between(date, as.Date("2024-06-06"), as.Date("2024-06-07")) ~ "After.Sowing",
     between(date, as.Date("2024-06-07"), as.Date("2024-06-12")) ~ "After.Germination",
     between(date, as.Date("2024-06-12"), as.Date("2024-07-02")) ~ "After.Emergence",
     
     between(date, as.Date("2024-07-02"), as.Date("2024-08-07")) ~ "After.VernalSaturation",
     between(date, as.Date("2024-08-07"), as.Date("2024-08-30") ) ~ "After.TerminalSpikelet",
     between(date, as.Date("2024-08-30"), as.Date("2024-09-15")) ~ "After.FlagLeaf",
     
     between(date, as.Date("2024-09-15"), as.Date("2024-09-21")) ~ "After.Heading",
     between(date, as.Date("2024-09-21"), as.Date("2024-09-29")) ~ "After.Flowering",
     between(date, as.Date("2024-09-29"), as.Date("2024-10-31")) ~ "After.StartGrainFill",
     between(date, as.Date("2024-10-31"), as.Date("2024-11-18")) ~ "After.EndGrainFill",
     #between(date, as.Date("2024-11-18"), as.Date("")) ~ "After.Maturity",
     date > as.Date("2024-11-18") ~  "After.Harvest",
     
     .default = "other"
   ))
 
 plant_count_biomass_tillers_NDVI_yld_quaility_2
 
 
 
 
 
 
 
 
 # 
 # plant_count_biomass_tillers_NDVI_yld <- plant_count_biomass_tillers_NDVI_yld %>% mutate(
 #  After_Phenology_stage = case_when(
 #    date <        as.Date("2024-06-06") ~ "PreSowing",
 #    between(date, as.Date("2024-06-06"), as.Date("2024-11-18")) ~ "growing season",
 #    date >        as.Date("2024-11-18") ~ "after harvest",
 #    
 #    .default = "other"
 #  ))
 # 
 # plant_count_biomass_tillers_NDVI_yld

unique(plant_count_biomass_tillers_NDVI_yld_quaility_2$After_Phenology_stage)
test_other <- plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% filter(After_Phenology_stage =="other")
unique(test_other$date)
### Add in more details from serenity file based on short ID -----
# Missing all of this

# file_serenity <- "exp3510.obsUnitsDesign.080125.xlsx"
# path_serenity <- "H:/Output-2/Site-Data/2._SSO2_Copeville-Farley/1. SSO2_Trial design_MetaData/"
# 
# excel_sheets(paste0(path_serenity, file_serenity))
# 
# serenity_details <- read_excel(paste0(path_serenity, file_serenity), 
#                                sheet = "exp3510.obsUnitsDesign.080125" 
#                                #, col_types = "text" 
#                                )
# names(serenity_details)
# names(plant_bind_rows_end)
# 
# serenity_details<- serenity_details %>% select(shortID, wholeplot, bay, block, row )
# 
# plant_bind_rows_end_serenity<- left_join(
#   plant_bind_rows_end, serenity_details,
#   by = join_by(Short_ID == shortID))







### Add in new clm days after sowing -----

Sowing_date <- as.Date("2024-06-06")
# get date clm into the correct format
# names(plant_bind_rows_end_serenity)
# str(plant_bind_rows_end_serenity$date)



plant_count_biomass_tillers_NDVI_yld_quaility_2 <- plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% 
  mutate(sowing_date = Sowing_date,
         period_since_sowing = days(date - Sowing_date),
         days_since_sowing = time_length(period_since_sowing,unit="days"))



### Clean up ###
back_up <- plant_count_biomass_tillers_NDVI_yld_quaility_2
names(plant_count_biomass_tillers_NDVI_yld_quaility_2)
plant_count_biomass_tillers_NDVI_yld_quaility_2 <- plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% 
  select(#- "Bay(Range)_Row_Treatment to join" ,
         -  "Column1" 
         )


list_of_variables <- distinct(plant_count_biomass_tillers_NDVI_yld_quaility_2, variable)

plant_count_biomass_tillers_NDVI_yld_quaility_2 <- plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% dplyr::rename(variable_old = variable)

plant_count_biomass_tillers_NDVI_yld_quaility_2 <- plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% mutate(
  variable = case_when(
    
    
    variable_old == "Total Emergence (plants/m²) - Final Establishment"       ~ "total_emergence_plants_m2_final_establishment",
    variable_old == "Total Emergence (plants/m²)"                             ~ "total_emergence_plants_m2",
    variable_old == "Biomass (t/ha)"                                          ~  "biomass_t_ha",
    variable_old == "tiller per m2"                                           ~ "tiller_per_m2",
    variable_old == "NDVI"                                                    ~ "NDVI",
    variable_old ==  "Field harvest yield t/ha"                              ~ "yield_t_ha",
    variable_old == "Harvest Weight"                                        ~ "harvest_weight",
    
       .default = variable_old
  ))

list_of_variables <- distinct(plant_count_biomass_tillers_NDVI_yld_quaility_2, variable)
names (plant_count_biomass_tillers_NDVI_yld_quaility_2)

# test_TW <- plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% 
#   select("label"  , "Ripping factor"   , "Nutrient factor", "value", "variable", "source" )
# 
# test_TW <- test_TW %>% 
#   filter(variable == "Test Weight"|
#            variable ==   "TGW"|
#            variable == "TGW converted (g)"|
#            variable == "test_weight"|
#            variable == "Grain test weight - converted(kg/hL)")
# str(test_TW)
# 
# 
# 
# summary <- test_TW %>% group_by(variable) %>% 
#   
#   summarise(
#     mean_value = mean(value, na.rm = TRUE),
#     sd_value = sd(value, na.rm = TRUE),
#     min_value = min(value, na.rm = TRUE),
#     max_value = max(value, na.rm = TRUE),
#     count = n()
#   )
# source_TW <- plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% 
# group_by(variable) %>% 
#   distinct(variable, .keep_all = TRUE) %>% 
#   select(variable, source)
# 
# summary <- left_join(summary, source_TW)
# 
# 
# write.csv(summary ,
#           paste0(path_name, "R_outputs/step1/", "plant_merged_source_TW.csv"), row.names = FALSE )

# ####
# test_protien <- plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% 
#   select("label"  , "Ripping factor"   , "Nutrient factor", "value", "variable", "source" )
# 
# distinct(test_protien , variable)
# 
# test_protien <- test_protien %>% 
#   filter(variable == "percent_protein"|
#            variable ==   "Protein")
# 
# str(test_protien)
# 
# 
# 
# summary_protien <- test_protien %>% group_by(variable) %>% 
#   
#   summarise(
#     mean_value = mean(value, na.rm = TRUE),
#     sd_value = sd(value, na.rm = TRUE),
#     min_value = min(value, na.rm = TRUE),
#     max_value = max(value, na.rm = TRUE),
#     count = n()
#   )
# 
# 
# summary_protien <- left_join(summary_protien, source_TW)
# write.csv(summary_protien ,
#           paste0(path_name, "R_outputs/step1/", "plant_merged_protien.csv"), row.names = FALSE )
# 
# ####
# 
# test_screening <- plant_count_biomass_tillers_NDVI_yld_quaility_2 %>% 
#   select("label"  , "Ripping factor"   , "Nutrient factor", "value", "variable", "source" )
# 
# distinct(test_screening , variable)
# 
# test_screening <- test_screening %>% 
#   filter(variable == "screening"|
#            variable ==   "Screenings"|
#            variable ==   "Screenings- converted (%)")
# 
# str(test_screening)
# 
# 
# 
# summary_screening <- test_screening %>% group_by(variable) %>% 
#   
#   summarise(
#     mean_value = mean(value, na.rm = TRUE),
#     sd_value = sd(value, na.rm = TRUE),
#     min_value = min(value, na.rm = TRUE),
#     max_value = max(value, na.rm = TRUE),
#     count = n()
#   )
# 
# 
# summary_screening <- left_join(summary_screening, source_TW)
# write.csv(summary_screening ,
#           paste0(path_name, "R_outputs/step1/", "plant_merged_screening.csv"), row.names = FALSE )
# 





path_name


## write out csv file for checking and next stage of analysis
write.csv(plant_count_biomass_tillers_NDVI_yld_quaility_2 ,
          paste0(path_name, "R_outputs/step1/", "plant_merged.csv"), row.names = FALSE )




