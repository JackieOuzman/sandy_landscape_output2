# This file is for summering and checking plant data that has been merge PART 2
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
R_outputs <- "R_outputs"


path_name<- paste0(sandy_landscape_folder,site,raw_data, R_outputs) 

list_sim_out_file <-
  list.files(
    path = path_name,
    pattern = ".csv" , 
    all.files = FALSE,
    full.names = FALSE
  )
list_sim_out_file

## read file -------------------------------------------------------
plant <- read_csv(paste0(path_name, "/plant_merged_check1_2025-01-22.csv"))




# plots  variable 1----------------------------------------------
#####   -------------------------------

names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "NDVI"

# arrange the date


plant %>% 
  filter(variable == variable_for_plot) %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= TreatmentDescription , y = value))+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ date)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot),
       #caption = 
       )

### add mean value for each date
### pull out one treatment or a few (see book)
