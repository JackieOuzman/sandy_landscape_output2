# This file is for importing plant data and merge into one file. soil data
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
# "3.Plant counts tidy long"       
# "4.yield tidy long"  
# "growth_stages"
# "Copeville 2024"   
# "extra notes"                    
# "metadata"            





## plant data at a PLOT level
plant <- read_excel(paste0(path_name, file), sheet = "3.plant measure tidy long" , col_types = "text" )
plant_est_NDVI <- read_excel(paste0(path_name, file), sheet = "3.Plants Est and NDVI tidy long" , col_types = "text" )
plant_count <- read_excel(paste0(path_name, file), sheet = "3.Plant counts tidy long" , col_types = "text" )
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
dim(plant_count) #17
plant_bind_rows <- bind_rows(plant_bind_rows1, plant_count)

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
  value
)


# assign sampling dates to plant satge and vice versa

list_of_sampling_dates <- plant_bind_rows_end %>% distinct(date)
list_of_sampling_dates <- list_of_sampling_dates %>%  arrange()
plant_stages
list_of_sampling_dates

#convert to number first or date
list_of_sampling_dates$date <- as.numeric(list_of_sampling_dates$date)
plant_stages$`start date` <- as.numeric(plant_stages$`start date`)
plant_stages$`end date` <- as.numeric(plant_stages$`end date`) 
plant_stages

temp <- list_of_sampling_dates %>% mutate(
  After_Phenology_stage = case_when(
    date <  45440 ~ "PreSowing",
    between(date, 45440, 45444) ~ "After.Sowing",
    between(date, 45444, 45452) ~ "After.Germination",
    between(date, 45452, 45467) ~ "After.Emergence",
    
    between(date, 45467,45507) ~ "After.VernalSaturation",
    between(date, 45507,45530 ) ~ "After.TerminalSpikelet",
    between(date, 45530,45545) ~ "After.FlagLeaf",
    between(date, 45545,45553) ~ "After.Heading",
    between(date, 45553,45561) ~ "After.Flowering",
    between(date, 45561,45594) ~ "After.StartGrainFill",
    between(date, 45594,45596) ~ "After.EndGrainFill",
    between(date,  45596,45614) ~ "After.Maturity",
    date > 45614 ~  "After.Harvest",
    
    .default = "other"
  ))
    
temp




## write out csv file for checking and next stage of analysis
write.csv(plant_bind_rows_end ,
          paste0(path_name, "R_outputs/", "plant_merged_test.csv"), row.names = FALSE )


write.csv(temp ,
          paste0(path_name, "R_outputs/", "plant_temp.csv"), row.names = FALSE )

