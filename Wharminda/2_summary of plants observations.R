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
    variable == "NDVI" ~ "NDVI",
    variable == "Corrected at 12.5% yield (t/ha)" ~ "Yield"
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


####UP TO HERE



### what was collected at date 1  ----------------------------------------------

date1 <- "2024-06-21"
collection1 <- plant %>%  filter(	date == date1)
collection1


observation_1 <- collection1 %>%  distinct(variable ) %>%  arrange(variable )
observation_1 #3 types of observations (2 are the same? just different names?)


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
  




# plots  DATE2 ----------------------------------------------
rm( collection1, date1, source_1, treatments_1, observation_1)

date2 <- "2024-07-10"
collection2 <- plant %>%  filter(	date == date2)
collection2


observation_2 <- collection2 %>%  distinct(variable ) %>%  arrange(variable )
observation_2 #2 NDVI and Plants_m2 


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

variable_for_plot <- "Plants_m2"

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


##### Date2 variable 2  -------------------------------

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

date3 <- "2024-08-02"
collection3 <- plant %>%  filter(	date == date3)
collection3


observation_3 <- collection3 %>%  distinct(variable ) %>%  arrange(variable )
observation_3 #1 


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


# plots  DATE4 ----------------------------------------------
rm( collection3, date3, source_3, treatments_3, observation_3)

date4 <- "2024-08-14"
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

##### Date4 variable 2  -------------------------------



variable_for_plot <- "Biomass_kg_ha"

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

date5 <- "2024-09-10"
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

date6 <- "2024-09-18"
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

##### Date6 variable 2  -------------------------------



variable_for_plot <- "Biomass_kg_ha"

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

date7 <- "2024-10-09"
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

date8 <- "2024-10-23"
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



variable_for_plot <- "NDVI"

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
 #date9 <- "NA" #"2024-xx-xx"

 #collection9 <- plant %>%  filter(	date == date9)
 
 collection9 <- plant %>%  filter(is.na(date))
 
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
 
 variable_for_plot <- "Biomass_kg_ha"
 
 
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

##### Date9 variable 2  -------------------------------



variable_for_plot <- "Yield"


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









