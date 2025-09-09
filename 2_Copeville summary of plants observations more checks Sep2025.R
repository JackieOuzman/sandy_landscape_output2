library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
#install.packages("openxlsx")
library(openxlsx)

# data files -------------------------------------------------------
site_name <- "Copeville"
data_grouping <- "Plant observation"

sandy_landscape_folder <- "H:/Output-2/Site-Data/"
site <- "2._SSO2_Copeville-Farley/"
raw_data <- "Jackies_working/"
R_outputs <- "R_outputs/step1/"


path_name<- paste0(sandy_landscape_folder,site,raw_data, R_outputs) 
path_name2<- paste0(sandy_landscape_folder,site,raw_data, "R_outputs/checked_data/") 

list_sim_out_file <-
  list.files(
    path = path_name,
    pattern = ".csv" , 
    all.files = FALSE,
    full.names = FALSE
  )
list_sim_out_file

## read file -------------------------------------------------------
plant <- read_csv(paste0(path_name, "/plant_merged.csv"))


#### check dates are correct


str(plant$date)
plant$date <- ymd(plant$date)
test <- plant %>% count(date, variable)
test


#### check it has all come in ###
str(plant)
plant %>% distinct(variable)

NDVI_DATES <- plant %>% 
  filter(variable== "NDVI") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE))
NDVI_DATES
### all good

############################################################################
plant %>% distinct(variable)

yld_DATES <- plant %>% 
  filter(variable== "yield_t_ha_corrected") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
yld_DATES
##Yes we have this


#############################################################################
plant %>% distinct(variable)

plants_m2_DATES <- plant %>% 
  filter(variable== "plants_m2") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
plants_m2_DATES
##Yes we have this


av_plants_m2_DATES <- plant %>% 
  filter(variable== "av_plants_m2") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
av_plants_m2_DATES
##Yes we have this
#############################################################################
#tiller_per_m2
str(plant)
plant %>% distinct(variable)

tiller_per_m2_DATES <- plant %>% 
  filter(variable== "tiller_per_m2") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
tiller_per_m2_DATES
#all good

#############################################################################
#biomass_t_ha
str(plant)
plant %>% distinct(variable)

biomass_kg_ha_DATES <- plant %>% 
  filter(variable== "biomass_kg_ha") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
biomass_kg_ha_DATES



#############################################################################
#HI
plant %>% distinct(variable)

HI_DATES <- plant %>% 
  filter(variable== "harvest_index") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
HI_DATES

############################################################################
#Grain weight
plant %>% distinct(variable)

GW_DATES <- plant %>% 
  filter(variable== "grain_weigh_g") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
GW_DATES


###########################################################################
#Grain number
plant %>% distinct(variable)

Grain_numb_DATES <- plant %>% 
  filter(variable== "grain_number_m2") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
Grain_numb_DATES

###########################################################################
#Grain number
plant %>% distinct(variable)

Head_numb_DATES <- plant %>% 
  filter(variable== "head_number_m2") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
Head_numb_DATES

###########################################################################
#Grain protien
plant %>% distinct(variable)

protien_DATES <- plant %>% 
  filter(variable== "percent_protein") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
protien_DATES
###########################################################################
#Grain n_removal
plant %>% distinct(variable)

n_removal_DATES <- plant %>% 
  filter(variable== "n_removal") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
n_removal_DATES

plant %>% distinct(variable)