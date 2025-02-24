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
file <- "Wharminda_all.xlsx"


excel_sheets(paste0(path_name, file))

# "APAL soils tidy long"     
# "2.site notes tidy long"    
# "2.GRDC_sands_Verran_PS24tidy lo"
# "2.In-Season soilMoist Tidy long" 

# "3.plant est tidy long"    
# "3.Biomass tidy long"            
# "3.Tillers tidy long"  
# "3.NDVI tidy long"      
# "4.yield tidy long"              
# "Growth stages"   
# "2024 notes"                     




## plant data at a PLOT level
plant_count <- read_excel(paste0(path_name, file), sheet = "3.plant est tidy long" , col_types = "text" )
plant_biomass <- read_excel(paste0(path_name, file), sheet = "3.Biomass tidy long" , col_types = "text" )
plant_Tillers <- read_excel(paste0(path_name, file), sheet = "3.Tillers tidy long" , col_types = "text" )
plant_est_NDVI <- read_excel(paste0(path_name, file), sheet = "3.NDVI tidy long" , col_types = "text" )
Yield <- read_excel(paste0(path_name, file), sheet = "4.yield tidy long" , col_types = "text" )

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

unique(Yield$date)  
str(Yield$date) 
#Yield$date <- as.numeric(Yield$date)
#Yield$date <- as.Date(Yield$date, origin = "1899-12-30") 
Yield$date <- as.Date(Yield$date, "%d/%m/%Y") 
Yield$date <- as.Date(Yield$date, , origin = "1899-12-30") 
str(Yield$date)

plant_count_biomass_tillers_NDVI_yld <- bind_rows(plant_count_biomass_tillers_NDVI, Yield)
rm (plant_count_biomass_tillers_NDVI, Yield )





# assign sampling dates to plant stage and vice versa

unique(plant_count_biomass_tillers_NDVI_yld$date)

NA_DATES <-plant_count_biomass_tillers_NDVI_yld %>% 
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
 
 sort(unique(plant_count_biomass_tillers_NDVI_yld$date))
 str(plant_count_biomass_tillers_NDVI_yld$date)

 NA_DATES <-plant_count_biomass_tillers_NDVI_yld %>% 
   filter (is.na(date))
 NA_DATES
 

 plant_count_biomass_tillers_NDVI_yld <- plant_count_biomass_tillers_NDVI_yld %>% mutate(
  After_Phenology_stage = case_when(
    date <        as.Date("2024-06-06") ~ "PreSowing",
    between(date, as.Date("2024-06-06"), as.Date("2024-11-18")) ~ "growing season",
    date >        as.Date("2024-11-18") ~ "after harvest",
    
    .default = "other"
  ))

 plant_count_biomass_tillers_NDVI_yld

unique(plant_count_biomass_tillers_NDVI_yld$After_Phenology_stage)
test_other <- plant_count_biomass_tillers_NDVI_yld %>% filter(After_Phenology_stage =="other")
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



 plant_count_biomass_tillers_NDVI_yld <- plant_count_biomass_tillers_NDVI_yld %>% 
  mutate(sowing_date = Sowing_date,
         period_since_sowing = days(date - Sowing_date),
         days_since_sowing = time_length(period_since_sowing,unit="days"))





## write out csv file for checking and next stage of analysis
write.csv(plant_count_biomass_tillers_NDVI_yld ,
          paste0(path_name, "R_outputs/step1/", "plant_merged.csv"), row.names = FALSE )




