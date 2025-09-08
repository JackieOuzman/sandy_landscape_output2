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
R_outputs <- "R_outputs/checked_data/"


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
plant <- read_csv(paste0(path_name, "/plant_merged2025-09-09.csv"))
                  
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
### all good
#############################################################################
total_emergence_plants_m2_1_DATES <- plant %>% 
  filter(variable== "total_emergence_plants_m2") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
total_emergence_plants_m2_1_DATES
##Yes we have this


total_emergence_plants_m2_final_establishment_DATES <- plant %>% 
  filter(variable== "total_emergence_plants_m2_final_establishment") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
total_emergence_plants_m2_final_establishment_DATES
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

biomass_t_ha_DATES <- plant %>% 
  filter(variable== "biomass_t_ha") %>% 
  group_by(date ) %>% 
  summarise(
    count = n(),
    mean_value = mean(value, na.rm = TRUE),
    max_DAS = max(days_since_sowing, na.rm = TRUE))
biomass_t_ha_DATES


plant %>% distinct(variable)





### what was collected at date 1  ----------------------------------------------

date1 <- ymd( "2024-07-04")
date1
collection1 <- plant %>%  filter(	date == date1)
collection1


observation_1 <- collection1 %>%  distinct(variable ) %>%  arrange(variable )
observation_1 #2 types of observations (2 are the same? just different names?)


source_1 <- collection1 %>% 
  group_by(variable, source  ) %>% 
  summarise(n = n())
source_1


treatments_1 <- collection1 %>% 
  group_by(TreatmentDescription, variable ) %>% 
   summarise(n = n())
treatments_1




# plots  DATE 1----------------------------------------------
##### Date1 variable 1  -------------------------------

names(collection1)
str(collection1)

variable_for_plot <- "NDVI"
  
collection1 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date1),
       caption = paste0 ("Number of data sources:", distinct(collection1, source_abbreviation) %>% count()))
  

##### Date1 variable 2  -------------------------------

names(collection1)
str(collection1)

variable_for_plot <- "Plants_m2"

collection1 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date1),
       caption = paste0 ("Number of data sources:", distinct(collection1, source_abbreviation) %>% count()))


# plots  DATE2 ----------------------------------------------
rm( collection1, date1, source_1, treatments_1, observation_1)

date2 <- ymd("2024-07-12")
str(date2)
collection2 <- plant %>%  filter(	date == date2)
collection2
str(unique(collection2$date))

observation_2 <- collection2 %>%  distinct(variable ) %>%  arrange(variable )
observation_2 #1 NDVI  


source_2 <- collection2 %>% 
  group_by(variable, source  ) %>% 
  summarise(n = n())
source_2


treatments_2 <- collection2 %>% 
  group_by(TreatmentDescription, variable ) %>% 
  summarise(n = n())
treatments_2

##### Date2 variable 1  -------------------------------

names(collection2)
str(collection2)

variable_for_plot <- "NDVI"

collection2 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date2),
       caption = paste0 ("Number of data sources:", distinct(collection2, source) %>% count()))






# plots  DATE3 ----------------------------------------------
rm( collection2, date2, source_2, treatments_2, observation_2)

date3 <-  "2024-07-16"
collection3 <- plant %>%  filter(	date == date3)
collection3


observation_3 <- collection3 %>%  distinct(variable ) %>%  arrange(variable )
observation_3 #2 


source_3 <- collection3 %>% 
  group_by(variable, source  ) %>% 
  summarise(n = n())
source_3


treatments_3 <- collection3 %>% 
  group_by(TreatmentDescription, variable ) %>% 
  summarise(n = n())
treatments_3

##### Date3 variable 1  -------------------------------



variable_for_plot <- "NDVI"

collection3 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date3),
       caption = paste0 ("Number of data sources:", distinct(collection3, source) %>% count()))

##### Date3 variable 2  -------------------------------



variable_for_plot <- "Plants_m2"

collection3 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date3),
       caption = paste0 ("Number of data sources:", distinct(collection3, source) %>% count()))

# plots  DATE4 ----------------------------------------------
rm( collection3, date3, source_3, treatments_3, observation_3)

date4 <- "2024-07-22"
collection4 <- plant %>%  filter(	date == date4)
collection4


observation_4 <- collection4 %>%  distinct(variable ) %>%  arrange(variable )
observation_4 #1 


source_4 <- collection4 %>% 
  group_by(variable, source  ) %>% 
  summarise(n = n())
source_4


treatments_4 <- collection4 %>% 
  group_by(TreatmentDescription, variable ) %>% 
  summarise(n = n())
treatments_4

##### Date4 variable 1  -------------------------------



variable_for_plot <- "NDVI"

collection4 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date4),
       caption = paste0 ("Number of data sources:", distinct(collection4, source) %>% count()))





# plots  DATE5 ----------------------------------------------
rm( collection4, date4, source_4, treatments_4, observation_4)

date5 <- "2024-07-31"
collection5 <- plant %>%  filter(	date == date5)



observation_5 <- collection5 %>%  distinct(variable ) %>%  arrange(variable )
observation_5 #2 


source_5 <- collection5 %>% 
  group_by(variable, source  ) %>% 
  summarise(n = n())
source_5


treatments_5 <- collection5 %>% 
  group_by(TreatmentDescription,variable ) %>% 
  summarise(n = n())

# treatments_5 <- collection5 %>% 
#   group_by(TreatmentDescription) %>% 
#   summarise(n = n()) 
 treatments_5

##### Date5 variable 1  -------------------------------

 

variable_for_plot <- "NDVI"
 
 source_5_2 <- collection5 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(variable, source  ) %>% 
   summarise(n = n()) %>% 
   count()
 source_5_2

collection5 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date5),
       caption = paste0 ("Number of data sources:", source_5_2[1,2]))






# plots  DATE6 ----------------------------------------------
rm( collection5, date5, source_5, treatments_5, observation_5)

date6 <- "2024-08-09"
collection6 <- plant %>%  filter(	date == date6)



observation_6 <- collection6 %>%  distinct(variable ) %>%  arrange(variable )
observation_6 #2 


source_6 <- collection6 %>% 
  group_by(variable, source  ) %>% 
  summarise(n = n())
source_6


treatments_6 <- collection6 %>% 
  group_by(TreatmentDescription,variable ) %>% 
  summarise(n = n())

# treatments_5 <- collection5 %>% 
#   group_by(TreatmentDescription) %>% 
#   summarise(n = n()) 
 treatments_6

##### Date6 variable 1  -------------------------------



variable_for_plot <- "NDVI"

collection6 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date6),
       caption = paste0 ("Number of data sources:", distinct(collection6, source) %>% count()))



# plots  DATE7 ----------------------------------------------
rm( collection6, date6, source_6, treatments_6, observation_6)

date7 <- "2024-08-14"
collection7 <- plant %>%  filter(	date == date7)



observation_7 <- collection7 %>%  distinct(variable ) %>%  arrange(variable )
observation_7 #1 


source_7 <- collection7 %>% 
  group_by(variable, source  ) %>% 
  summarise(n = n())
source_7


treatments_7 <- collection7 %>% 
  group_by(TreatmentDescription,variable ) %>% 
  summarise(n = n())

# treatments_5 <- collection5 %>% 
#   group_by(TreatmentDescription) %>% 
#   summarise(n = n()) 
treatments_7

##### Date7 variable 1  -------------------------------



variable_for_plot <- "NDVI"

collection7 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date7),
       caption = paste0 ("Number of data sources:", distinct(collection7, source) %>% count()))


# plots  DATE8 ----------------------------------------------
rm( collection7, date7, source_7, treatments_7, observation_7)

date8 <- "2024-08-15"
collection8 <- plant %>%  filter(	date == date8)



observation_8 <- collection8 %>%  distinct(variable ) %>%  arrange(variable )
observation_8 


source_8 <- collection8 %>% 
  group_by(variable, source  ) %>% 
  summarise(n = n())
source_8


treatments_8 <- collection8 %>% 
  group_by(TreatmentDescription,variable ) %>% 
  summarise(n = n())

treatments_8

##### Date8 variable 1  -------------------------------



variable_for_plot <- "Biomass_t_ha"

collection8 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date8),
       caption = paste0 ("Number of data sources:", distinct(collection8, source) %>% count()))


##### Date8 variable 2  -------------------------------



variable_for_plot <- "Tiller_m2"

collection8 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date8),
       caption = paste0 ("Number of data sources:", distinct(collection8, source) %>% count()))



# plots  DATE9 ----------------------------------------------
rm( collection8, date8, source_8, treatments_8, observation_8)

unique(plant$date)
date9 <- "2024-08-21"

collection9 <- plant %>%  filter(	date == date9)
 

 
 observation_9 <- collection9 %>%  distinct(variable ) %>%  arrange(variable )
 observation_9 
 
 
 source_9 <- collection9 %>% 
   group_by(variable, source  ) %>% 
   summarise(n = n())
 source_9
 
 
 treatments_9 <- collection9 %>% 
   group_by(TreatmentDescription,variable ) %>% 
   summarise(n = n())
 
 # treatments_9 <- collection9 %>% 
 #    group_by(TreatmentDescription) %>% 
 #    summarise(n = n()) 
 
 treatments_9
 
 ##### Date9 variable 1  -------------------------------
 
 observation_9
 
 variable_for_plot <- "NDVI"
 
 
 collection9 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(TreatmentDescription ) %>% 
   summarise(n = n())
 
 source_5_2 <- collection9 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(variable, source  ) %>% 
   summarise(n = n()) %>% 
   count()
 source_5_2
 
 collection9 %>% 
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
  facet_wrap(.~source_abbreviation)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, " collected on: " , date9),
       caption = paste0 ("Number of data sources:", source_5_2[1,2]))




 # plots  DATE10 ----------------------------------------------
 rm( collection9, date9, source_9, treatments_9, observation_9)
 
 unique(plant$date)
 date10 <- "2024-08-26"
 
 collection10 <- plant %>%  filter(	date == date10)
 
 
 
 observation_10 <- collection10 %>%  distinct(variable ) %>%  arrange(variable )
 observation_10 
 
 
 source_10 <- collection10 %>% 
   group_by(variable, source  ) %>% 
   summarise(n = n())
 source_10
 
 
 treatments_10 <- collection10 %>% 
   group_by(TreatmentDescription,variable ) %>% 
   summarise(n = n())
 
 # treatments_9 <- collection9 %>% 
 #    group_by(TreatmentDescription) %>% 
 #    summarise(n = n()) 
 
 treatments_9
 
 ##### Date10 variable 1  -------------------------------
 
 observation_10
 
 variable_for_plot <- "NDVI"
 
 
 collection10 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(TreatmentDescription ) %>% 
   summarise(n = n())
 
 source_5_2 <- collection10 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(variable, source  ) %>% 
   summarise(n = n()) %>% 
   count()
 source_5_2
 
 collection10 %>% 
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
   facet_wrap(.~source_abbreviation)+
   labs(title = paste0(site_name, ": ", data_grouping),
        subtitle = paste0(variable_for_plot, " collected on: " , date10),
        caption = paste0 ("Number of data sources:", source_5_2[1,2]))
 

 # plots  DATE11 ----------------------------------------------
 rm( collection10, date10, source_10, treatments_10, observation_10)
 
 unique(plant$date)
 date11 <- "2024-09-06"
 
 collection11 <- plant %>%  filter(	date == date11)
 
 
 
 observation_11 <- collection11 %>%  distinct(variable ) %>%  arrange(variable )
 observation_11 
 
 
 source_11 <- collection11 %>% 
   group_by(variable, source  ) %>% 
   summarise(n = n())
 source_11
 
 
 treatments_11 <- collection11 %>% 
   group_by(TreatmentDescription,variable ) %>% 
   summarise(n = n())
 
 # treatments_11 <- collection11 %>% 
 #    group_by(TreatmentDescription) %>% 
 #    summarise(n = n()) 
 
 treatments_11
 
 ##### Date11 variable 1  -------------------------------
 
 observation_11
 
 variable_for_plot <- "NDVI"
 
 
 collection11 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(TreatmentDescription ) %>% 
   summarise(n = n())
 
 source_5_2 <- collection11 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(variable, source  ) %>% 
   summarise(n = n()) %>% 
   count()
 source_5_2
 
 collection11 %>% 
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
   facet_wrap(.~source_abbreviation)+
   labs(title = paste0(site_name, ": ", data_grouping),
        subtitle = paste0(variable_for_plot, " collected on: " , date11),
        caption = paste0 ("Number of data sources:", source_5_2[1,2]))
 
 
 # plots  DATE12 ----------------------------------------------
 rm( collection11, date11, source_11, treatments_11, observation_11)
 
 unique(plant$date)
 date12 <- "2024-09-18"
 
 collection12 <- plant %>%  filter(	date == date12)
 
 
 
 observation_12 <- collection12 %>%  distinct(variable ) %>%  arrange(variable )
 observation_12 
 
 
 source_12 <- collection12 %>% 
   group_by(variable, source  ) %>% 
   summarise(n = n())
 source_12
 
 
 treatments_12 <- collection12 %>% 
   group_by(TreatmentDescription,variable ) %>% 
   summarise(n = n())
 
 # treatments_12 <- collection12 %>% 
 #    group_by(TreatmentDescription) %>% 
 #    summarise(n = n()) 
 
 treatments_12
 
 ##### Date12 variable 1  -------------------------------
 
 observation_12
 
 variable_for_plot <- "NDVI"
 
 
 collection12 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(TreatmentDescription ) %>% 
   summarise(n = n())
 
 source_5_2 <- collection12 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(variable, source  ) %>% 
   summarise(n = n()) %>% 
   count()
 source_5_2
 
 collection12 %>% 
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
   facet_wrap(.~source_abbreviation)+
   labs(title = paste0(site_name, ": ", data_grouping),
        subtitle = paste0(variable_for_plot, " collected on: " , date12),
        caption = paste0 ("Number of data sources:", source_5_2[1,2]))

 
  # plots  DATE13 ----------------------------------------------
 rm( collection12, date12, source_12, treatments_12, observation_12)
 
 unique(plant$date)
 date13 <- "2024-09-23"
 
 collection13 <- plant %>%  filter(	date == date13)
 
 
 
 observation_13 <- collection13 %>%  distinct(variable ) %>%  arrange(variable )
 observation_13 
 
 
 source_13 <- collection13 %>% 
   group_by(variable, source  ) %>% 
   summarise(n = n())
 source_13
 
 
 treatments_13 <- collection13 %>% 
   group_by(TreatmentDescription,variable ) %>% 
   summarise(n = n())
 
 # treatments_13 <- collection13 %>% 
 #    group_by(TreatmentDescription) %>% 
 #    summarise(n = n()) 
 
 treatments_13
 
 ##### Date13 variable 1  -------------------------------
 
 observation_13
 
 variable_for_plot <- "NDVI"
 
 
 collection13 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(TreatmentDescription ) %>% 
   summarise(n = n())
 
 source_5_2 <- collection13 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(variable, source  ) %>% 
   summarise(n = n()) %>% 
   count()
 source_5_2
 
 collection13 %>% 
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
   facet_wrap(.~source_abbreviation)+
   labs(title = paste0(site_name, ": ", data_grouping),
        subtitle = paste0(variable_for_plot, " collected on: " , date13),
        caption = paste0 ("Number of data sources:", source_5_2[1,2]))
 
 
 
 
 
 
 
 
 
 ##### Date13 variable 2  -------------------------------
 
 observation_13
 
 variable_for_plot <- "Biomass_t_ha"
 
 
 collection13 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(TreatmentDescription ) %>% 
   summarise(n = n())
 
 source_5_2 <- collection13 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(variable, source  ) %>% 
   summarise(n = n()) %>% 
   count()
 source_5_2
 
 collection13 %>% 
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
   facet_wrap(.~source_abbreviation)+
   labs(title = paste0(site_name, ": ", data_grouping),
        subtitle = paste0(variable_for_plot, " collected on: " , date13),
        caption = paste0 ("Number of data sources:", source_5_2[1,2]))
 
 
 
 # plots  DATE14 ----------------------------------------------
 rm( collection13, date13, source_13, treatments_13, observation_13)
 
 unique(plant$date)
 date14 <- "2024-09-24"
 
 collection14 <- plant %>%  filter(	date == date14)
 
 
 
 observation_14 <- collection14 %>%  distinct(variable ) %>%  arrange(variable )
 observation_14 
 
 
 source_14 <- collection14 %>% 
   group_by(variable, source  ) %>% 
   summarise(n = n())
 source_14
 
 
 treatments_14 <- collection14 %>% 
   group_by(TreatmentDescription,variable ) %>% 
   summarise(n = n())
 
 # treatments_14 <- collection14 %>% 
 #    group_by(TreatmentDescription) %>% 
 #    summarise(n = n()) 
 
 treatments_14
 
 ##### Date14 variable 1  -------------------------------
 
 observation_14
 
 variable_for_plot <- "Tiller_m2"
 
 
 collection14 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(TreatmentDescription ) %>% 
   summarise(n = n())
 
 source_5_2 <- collection14 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(variable, source  ) %>% 
   summarise(n = n()) %>% 
   count()
 source_5_2
 
 collection14 %>% 
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
   facet_wrap(.~source_abbreviation)+
   labs(title = paste0(site_name, ": ", data_grouping),
        subtitle = paste0(variable_for_plot, " collected on: " , date14),
        caption = paste0 ("Number of data sources:", source_5_2[1,2]))
 
 
 # plots  DATE15 ----------------------------------------------
 rm( collection14, date14, source_14, treatments_14, observation_14)
 
 unique(plant$date)
 date15 <- "2024-10-16"
 
 collection15 <- plant %>%  filter(	date == date15)
 
 
 
 observation_15 <- collection15 %>%  distinct(variable ) %>%  arrange(variable )
 observation_15 
 
 
 source_15 <- collection15 %>% 
   group_by(variable, source  ) %>% 
   summarise(n = n())
 source_15
 
 
 treatments_15 <- collection15 %>% 
   group_by(TreatmentDescription,variable ) %>% 
   summarise(n = n())
 
 # treatments_15 <- collection15 %>% 
 #    group_by(TreatmentDescription) %>% 
 #    summarise(n = n()) 
 
 treatments_15
 
 ##### Date15 variable 1  -------------------------------
 
 observation_15
 
 variable_for_plot <- "NDVI"
 
 
 collection15 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(TreatmentDescription ) %>% 
   summarise(n = n())
 
 source_5_2 <- collection15 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(variable, source  ) %>% 
   summarise(n = n()) %>% 
   count()
 source_5_2
 
 collection15 %>% 
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
   facet_wrap(.~source_abbreviation)+
   labs(title = paste0(site_name, ": ", data_grouping),
        subtitle = paste0(variable_for_plot, " collected on: " , date15),
        caption = paste0 ("Number of data sources:", source_5_2[1,2]))
 
 
 # plots  DATE16 ----------------------------------------------
 rm( collection15, date15, source_15, treatments_15, observation_15)
 
 unique(plant$date)
 date16 <- "2024-11-19"
 
 collection16 <- plant %>%  filter(	date == date16)
 
 
 
 observation_16 <- collection16 %>%  distinct(variable ) %>%  arrange(variable )
 observation_16 
 
 
 source_16 <- collection16 %>% 
   group_by(variable, source  ) %>% 
   summarise(n = n())
 source_16
 
 
 treatments_16 <- collection16 %>% 
   group_by(TreatmentDescription,variable ) %>% 
   summarise(n = n())
 
 # treatments_16 <- collection16 %>% 
 #    group_by(TreatmentDescription) %>% 
 #    summarise(n = n()) 
 
 treatments_16
 
 ##### Date16 variable 1  -------------------------------
 
 observation_16
 
 variable_for_plot <- "Yield"
 
 
 collection16 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(TreatmentDescription ) %>% 
   summarise(n = n())
 
 source_5_2 <- collection16 %>% 
   filter(variable == variable_for_plot) %>%
   group_by(variable, source  ) %>% 
   summarise(n = n()) %>% 
   count()
 source_5_2
 
 collection16 %>% 
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
   facet_wrap(.~source_abbreviation)+
   labs(title = paste0(site_name, ": ", data_grouping),
        subtitle = paste0(variable_for_plot, " collected on: " , date16),
        caption = paste0 ("Number of data sources:", source_5_2[1,2]))
 
 
 
 
 
 
 
 
 
 
 
