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
site <- "1. SSO2_Walpeup-Pole/"
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
file <- "Walpuep_all.xlsx"

excel_sheets(paste0(path_name, file))

# "2.Walpeup Basel soil character" 
# "2.Soil Water"                  
# "3.Biomass"                     
# "3.Biomass_flowering"            
# "3.NDVI"                        
# "3.Plant count"                 
# "4. Yield"   
# "Growth stages" 
# "Notes 2025"                    
# "Older Notes"                   





## plant data at a PLOT level
plant_biomass1 <- read_excel(paste0(path_name, file), sheet = "3.Biomass" , col_types = "text" )
plant_biomass2 <- read_excel(paste0(path_name, file), sheet = "3.Biomass_flowering" , col_types = "text" )
plant_est_NDVI <- read_excel(paste0(path_name, file), sheet = "3.NDVI" , col_types = "text" )
plant_count <- read_excel(paste0(path_name, file), sheet = "3.Plant count" , col_types = "text" )
Yield <- read_excel(paste0(path_name, file), sheet = "4. Yield" , col_types = "text" )

plant_stages <- read_excel(paste0(path_name, file), sheet = "Growth stages" , 
                           col_types = "text" #, 
                           #skip = 4
                           )


# merge the biomass plant  worksheet 1 and 2 -------------------------------------------------------
dim(plant_biomass1) #24
dim(plant_biomass2) #17

names(plant_biomass1)
plant_biomass1 <- plant_biomass1 %>% select(
  -"Temp for look up row and bay combined",
  -"Column18",
  -"Column19",
  -"Column20" ,
  -"Column21"  ,
  -"Column22" ,
  -"Column23"  ,
  -"Column24"
)
dim(plant_biomass1)#16
names(plant_biomass2)

plant_biomass2 <- plant_biomass2 %>% select(-"temp combined row and bay")
  
  
plant_biomass <- bind_rows(plant_biomass1, plant_biomass2)
names(plant_biomass)

rm(plant_biomass1, plant_biomass2)

# merge the plant_est_NDVI -------------------------------------------------------

dim(plant_est_NDVI) #17
names(plant_est_NDVI)
plant_est_NDVI <- plant_est_NDVI %>% select(-"Temp for look up row and bay combined")

dim(plant_biomass) #16
names(plant_biomass)

biomass_NDVI <- bind_rows(plant_biomass, plant_est_NDVI)

rm (plant_biomass,plant_est_NDVI )


# merge the plant_count -------------------------------------------------------

dim(plant_count) #17
names(plant_count)
plant_count <- plant_count %>% select(-"temp row and bay combined")

dim(biomass_NDVI) #16
dim(plant_count) #16
names(biomass_NDVI)
names(plant_count)

biomass_NDVI_plant_count <- bind_rows(biomass_NDVI, plant_count)

rm (biomass_NDVI, plant_count )


# merge the yield -------------------------------------------------------
dim(Yield) #18
names(Yield)
Yield<- Yield %>% select(- "temp combined row and bay",
                         - "Column18")



biomass_NDVI_plant_count_yld <- bind_rows(biomass_NDVI_plant_count, Yield)


rm (biomass_NDVI_plant_count, Yield )





# assign sampling dates to plant stage and vice versa

unique(biomass_NDVI_plant_count_yld$date)

NA_DATES <-biomass_NDVI_plant_count_yld %>% 
  filter (is.na(date))
## looks like its just the yield data that is missing dates - I am waiting for Kenton



# #convert to number first or date
# list_of_sampling_dates$date <- as.numeric(list_of_sampling_dates$date)
# plant_stages$`start date` <- as.numeric(plant_stages$`start date`)
# plant_stages$`end date` <- as.numeric(plant_stages$`end date`) 
# plant_stages
# 
# 
biomass_NDVI_plant_count_yld$date <- as.numeric(biomass_NDVI_plant_count_yld$date)
biomass_NDVI_plant_count_yld$date <- as.Date(biomass_NDVI_plant_count_yld$date, origin = "1899-12-30")
 
 sort(unique(biomass_NDVI_plant_count_yld$date))
 str(biomass_NDVI_plant_count_yld$date)

 NA_DATES <-biomass_NDVI_plant_count_yld %>% 
   filter (is.na(date))
 
 

 biomass_NDVI_plant_count_yld <- biomass_NDVI_plant_count_yld %>% mutate(
  After_Phenology_stage = case_when(
    date <        as.Date("2024-08-14") ~ "before start of critical period",
    date >        as.Date("2024-09-18") ~ "after start of critical period",
    
    .default = "critical period"
  ))

 biomass_NDVI_plant_count_yld



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

Sowing_date <- as.Date("2024-04-26")
# get date clm into the correct format
# names(plant_bind_rows_end_serenity)
# str(plant_bind_rows_end_serenity$date)



 biomass_NDVI_plant_count_yld <- biomass_NDVI_plant_count_yld %>% 
  mutate(sowing_date = Sowing_date,
         period_since_sowing = days(date - Sowing_date),
         days_since_sowing = time_length(period_since_sowing,unit="days"))





## write out csv file for checking and next stage of analysis
write.csv(biomass_NDVI_plant_count_yld ,
          paste0(path_name, "R_outputs/step1/", "plant_merged.csv"), row.names = FALSE )




