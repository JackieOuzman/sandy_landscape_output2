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
site <- "2._SSO2_Copeville-Farley/"
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
file <- "Copeville_all.xlsx"

excel_sheets(paste0(path_name, file))

# "APAL soils tidy long" 
# "2.Copeville Pen tidy long" 
# "2.Base soil tidy long"          
# "2.Baseline N + Water tidy long"
# "2.soil water tidy long"   
# "3.plant measure tidy long"      
# "3.Plants Est SUBPLOT tidy long" 
# "3.Plants Est and NDVI tidy long"
# "3.Plant counts tidy long"       ### This looks like duplicated data I won't import 
# "4.yield tidy long"  
# "growth_stages"
# "Copeville 2024"   
# "extra notes"                    
# "metadata"            





## plant data at a PLOT level
plant <- read_excel(paste0(path_name, file), sheet = "3.plant measure tidy long" , col_types = "text" )
plant_est_NDVI <- read_excel(paste0(path_name, file), sheet = "3.Plants Est and NDVI tidy long" , col_types = "text" )
#plant_count <- read_excel(paste0(path_name, file), sheet = "3.Plant counts tidy long" , col_types = "text" )
Yield <- read_excel(paste0(path_name, file), sheet = "4.yield tidy long" , col_types = "text" )

plant_stages <- read_excel(paste0(path_name, file), sheet = "growth_stages" , 
                           col_types = "text" #, 
                           #skip = 4
                           )


# merge the plant  worksheet 1 and 2 -------------------------------------------------------
dim(plant) #18
dim(plant_est_NDVI) #18



plant_bind_rows1 <- bind_rows(plant, plant_est_NDVI)

# merge the plant  worksheet 3 -------------------------------------------------------
# I think this is duplicated so I won't import it
# dim(plant_count) #17
# plant_bind_rows <- bind_rows(plant_bind_rows1, plant_count)

plant_bind_rows <- plant_bind_rows1

# merge the plant  worksheet 4 -------------------------------------------------------
dim(Yield) #17
plant_bind_rows_end <- bind_rows(plant_bind_rows, Yield)


names(plant_bind_rows_end)
plant_bind_rows_end <- plant_bind_rows_end %>% select(
  site,
  date,
  TreatmentDescription,
  Short_ID,
  depth,
  variable,
  value,
  source, 
  commet,
  `Ripping factor`  ,
  `Nutrient factor`
)


# assign sampling dates to plant stage and vice versa



# #convert to number first or date
# list_of_sampling_dates$date <- as.numeric(list_of_sampling_dates$date)
# plant_stages$`start date` <- as.numeric(plant_stages$`start date`)
# plant_stages$`end date` <- as.numeric(plant_stages$`end date`) 
# plant_stages
# 
# 
 plant_bind_rows_end$date <- as.numeric(plant_bind_rows_end$date)
 plant_bind_rows_end$date <- as.Date(plant_bind_rows_end$date, origin = "1899-12-30")
 
 unique(plant_bind_rows_end$date)
 str(plant_bind_rows_end$date)


plant_bind_rows_end <- plant_bind_rows_end %>% mutate(
  After_Phenology_stage = case_when(
    date <        as.Date("2024-05-28") ~ "PreSowing",
    between(date, as.Date("2024-05-28"), as.Date("2024-06-01")) ~ "After.Sowing",
    between(date, as.Date("2024-06-01"), as.Date("2024-06-09")) ~ "After.Germination",
    between(date, as.Date("2024-06-09"), as.Date("2024-06-24")) ~ "After.Emergence",

    between(date, as.Date("2024-06-24"), as.Date("2024-08-03")) ~ "After.VernalSaturation",
    between(date, as.Date("2024-08-03"), as.Date("2024-08-26") ) ~ "After.TerminalSpikelet",
    between(date, as.Date("2024-08-26"), as.Date("2024-09-10")) ~ "After.FlagLeaf",
    between(date, as.Date("2024-09-10"), as.Date("2024-09-18")) ~ "After.Heading",
    between(date, as.Date("2024-09-18"), as.Date("2024-09-26")) ~ "After.Flowering",
    between(date, as.Date("2024-09-26"), as.Date("2024-10-29")) ~ "After.StartGrainFill",
    between(date, as.Date("2024-10-29"), as.Date("2024-10-31")) ~ "After.EndGrainFill",
    between(date, as.Date("2024-10-31"), as.Date("2024-11-18")) ~ "After.Maturity",
    date > as.Date("2024-11-18") ~  "After.Harvest",
    
    .default = "other"
  ))

plant_bind_rows_end



### Add in more details from serenity file based on short ID -----

file_serenity <- "exp3510.obsUnitsDesign.080125.xlsx"
path_serenity <- "H:/Output-2/Site-Data/2._SSO2_Copeville-Farley/1. SSO2_Trial design_MetaData/"

excel_sheets(paste0(path_serenity, file_serenity))

serenity_details <- read_excel(paste0(path_serenity, file_serenity), 
                               sheet = "exp3510.obsUnitsDesign.080125" 
                               #, col_types = "text" 
                               )
names(serenity_details)
names(plant_bind_rows_end)

serenity_details<- serenity_details %>% select(shortID, wholeplot, bay, block, row )

plant_bind_rows_end_serenity<- left_join(
  plant_bind_rows_end, serenity_details,
  by = join_by(Short_ID == shortID))




### Add in new clm days after sowing -----

Sowing_date <- as.Date("2024-05-28")
# get date clm into the correct format
names(plant_bind_rows_end_serenity)
str(plant_bind_rows_end_serenity$date)



plant_bind_rows_end_serenity <- plant_bind_rows_end_serenity %>% 
  mutate(sowing_date = Sowing_date,
         period_since_sowing = days(date - Sowing_date),
         days_since_sowing = time_length(period_since_sowing,unit="days"))





## write out csv file for checking and next stage of analysis
write.csv(plant_bind_rows_end_serenity ,
          paste0(path_name, "R_outputs/step1/", "plant_merged_test.csv"), row.names = FALSE )




