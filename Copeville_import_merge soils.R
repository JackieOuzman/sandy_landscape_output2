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
# "Copeville 2024"   
# "extra notes"                    
# "metadata"            


## soil data at a PLOT level
Penetrometer <- read_excel(paste0(path_name, file), sheet = "2.Copeville Pen tidy long", col_types = "text" )
Base_soil <- read_excel(paste0(path_name, file), sheet = "2.Base soil tidy long" , col_types = "text" )
Base_soil_N_water <- read_excel(paste0(path_name, file), sheet = "2.Baseline N + Water tidy long" , col_types = "text" )
Soil_water <- read_excel(paste0(path_name, file), sheet = "2.soil water tidy long" , col_types = "text" )





# merge the soil  worksheet -------------------------------------------------------
dim(Penetrometer) #18
dim(Base_soil) #18

str(Base_soil)
Base_soil <- Base_soil %>% rename("row" = "run" )
Base_soil <- Base_soil %>% select(-Column17, -shortID)
Penetrometer <- Penetrometer %>% select(-Column17, -shortID)


df <- Base_soil
clm_names <- names(df)
clm_names_df <- as.data.frame(clm_names)
clm_names_df <- sort(clm_names_df$clm_names)
Base_soil_sort <- Base_soil[, clm_names_df]


df <- Penetrometer
clm_names <- names(df)
clm_names_df <- as.data.frame(clm_names)
clm_names_df <- sort(clm_names_df$clm_names)
Penetrometer_sort <- Penetrometer[, clm_names_df]


str(Penetrometer_sort)
str(Base_soil_sort)


soil_bind_rows1 <- bind_rows(Penetrometer_sort, Base_soil_sort)

## next  worksheets

names(Soil_water) #17

Soil_water <- Soil_water %>% select( -shortID)

df <- Soil_water
clm_names <- names(df)
clm_names_df <- as.data.frame(clm_names)
clm_names_df <- sort(clm_names_df$clm_names)
Soil_water_sort <- Soil_water[, clm_names_df]

names(soil_bind_rows1)
names(Soil_water_sort)


soil_bind_rows <- bind_rows(soil_bind_rows1, Soil_water_sort)


## next  worksheets

names(Base_soil_N_water) #17

Base_soil_N_water <- Base_soil_N_water %>% select(-"Column13", -"Column14" , -"Column15" , -"Column16")

df <- Base_soil_N_water
clm_names <- names(df)
clm_names_df <- as.data.frame(clm_names)
clm_names_df <- sort(clm_names_df$clm_names)
Base_soil_N_water_sort <- Base_soil_N_water[, clm_names_df]

names(soil_bind_rows)
names(Base_soil_N_water_sort)


soil_bind_rows_end <- bind_rows(soil_bind_rows, Base_soil_N_water_sort)



## subset the dataset
names(soil_bind_rows_end)


soil_bind_rows_end <- soil_bind_rows_end %>% select(
  site,
  date,
  TreatmentDescription,
  Short_ID,
  depth,
  variable,
  value
)


write.csv(soil_bind_rows_end ,
          paste0(path_name, "R_outputs/", "soil_merged_test.csv"), row.names = FALSE )

