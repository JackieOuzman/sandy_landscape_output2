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


## plant data at a PLOT level
plant <- read_excel(paste0(path_name, file), sheet = "3.plant measure tidy long" )
plant_est_NDVI <- read_excel(paste0(path_name, file), sheet = "3.Plants Est and NDVI tidy long" )
plant_count <- read_excel(paste0(path_name, file), sheet = "3.Plant counts tidy long" )
Yield <- read_excel(paste0(path_name, file), sheet = "4.yield tidy long" )
