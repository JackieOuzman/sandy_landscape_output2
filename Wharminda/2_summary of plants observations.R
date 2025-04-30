# This file is for summering and checking plant data that has been merge
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
#install.packages("openxlsx")
library(openxlsx)

# data files -------------------------------------------------------
site_name <- "Wharminda"
data_grouping <- "Plant observation"

sandy_landscape_folder <- "H:/Output-2/Site-Data/"
site <- "3. SSO2_Wharminda-Masters/"
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
                  
str(plant)
unique(plant$After_Phenology_stage)
#remove rows with no data values
plant <- plant %>% filter(!is.na(value ))
str(plant)
unique(plant$source)

#unique(plant_test$source_abbreviation)

## abbreviation the source name 
plant <- plant %>% 
  mutate(source1 =
            gsub('[^[:alnum:] ]', '', source)) %>% 
   mutate(source2 = 
            gsub("fs1cbrnexuscsiroauafsandysoilsiiworkOutput2SiteData3 SSO2WharmindaMasters3","",source1))  %>% 
   mutate(source_abbreviation =
            gsub("[[:space:]]", "", source2)) %>% 
   select(-source1, - source2)


plant$source_abbreviation



## how many dates of collection has been captured?  ----------------------------
dates_of_collection <- plant %>%  distinct(date, .keep_all = TRUE) %>%  
  arrange(date) %>% select(site, date, After_Phenology_stage )
dates_of_collection #16 


write.csv(dates_of_collection ,
          paste0(path_name,  "/Check_dates_of_collected_samples_plants.csv"), row.names = FALSE )


## how many different observations were collected  ----------------------------
types_of_data1 <- plant %>%  distinct(variable) %>%  arrange(variable)
types_of_data1
write.csv(types_of_data1 ,
          paste0(path_name,  "/Check_data_types_collected_samples_plants_v1.csv"), row.names = FALSE )
## Fixed up names
plant <- plant %>%
  mutate(variable =  case_when(
    variable == "Biomass (t/ha)"  ~ "Biomass_t_ha",
    variable == "tiller per m2"  ~ "Tiller_m2",
    variable == "Total Emergence (plants/m²)"  ~ "Plants_m2",
    variable == "Total Emergence (plants/m²) - Final Establishment"  ~ "Final Establishment_Plants_m2",
    variable == "NDVI" ~ "NDVI",
    variable == "Field harvest yield t/ha" ~ "Yield",
    variable == "Harvest Weight"~ "Harvest_Weight",
    variable == "Test Weight"~ "Test_Weight",
    variable == "Screenings"~ "Screenings",
    variable == "TGW"~ "TGW",
    variable == "Moisture" ~ "Moisture",
    variable == "Protein" ~ "Protein",
    variable == "Grain test weight - converted(kg/hL)" ~ "Grain_test_weight",
    variable == "TGW converted (g)"~ "TGW_g",
    variable == "Screenings- converted (%)" ~ "Screenings_perc",
    
  ))

types_of_data2 <- plant %>%  distinct(variable) %>%  arrange(variable)
types_of_data2 



write.csv(types_of_data2 ,
          paste0(path_name,  "/Check_data_types_collected_samples_plants_v2.csv"), row.names = FALSE )

## remove the data point called %tiller - is made from tiller m2 and doesnt match any other dataset


## how many different depths were observations were collected at? ----------------------------
depths  <- plant %>%  distinct(depth    ) %>%  arrange(depth    )
depths #NA?

Depth_NA <- plant %>% filter(is.na(depth))
#recode everything to surface

plant <- plant %>%  mutate(depth = "surface") 

## how many different treatments in the dataset? ----------------------------

treatment  <- plant %>%  distinct(TreatmentDescription) %>%  arrange(TreatmentDescription)
 
write.csv(treatment ,
          paste0(path_name,  "/Check_treatments_names_samples_plants_v1.csv"), row.names = FALSE )



######################################################################################################
#rm(dates_of_collection, Depth_NA, depths,  treatment, types_of_data1, types_of_data2)



# Write out csv file that has been checked pass1 and correction pass1 ----
today_date <- today()



write.csv(plant ,
          paste0(path_name2,  "plant_merged", today_date, ".csv"), 
          row.names = FALSE )


