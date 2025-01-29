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
plant <- read_csv(paste0(path_name, "/plant_merged_check1_2025-01-24.csv"))



# NDVI ----



names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "NDVI"

######  Calculate mean ----
mean_NDVI_By_date <- plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>% 
  group_by(date) %>% 
  summarise(NDVI_mean=mean(value))


##### Plot 1. Facet wrap by date, treatments vs NDVI ----
plant %>% 
  filter(variable == variable_for_plot) %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= TreatmentDescription , y = value))+
  geom_hline(data=mean_NDVI_By_date, aes(yintercept=NDVI_mean, 
                                         col=as.factor(date)),
             linewidth=1.0, colour = "blue")+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ date)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot),
       caption = "Mean value displayed for each date"
       )


##### Plot 2. Facet wrap treatments Dates vs NDVI  ----


plant %>% 
  filter(variable == variable_for_plot) %>%
  filter( value != 0) %>% 
  
  ggplot(aes(x= date , y = value, group = date))+
  geom_point()+
  #geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(`Nutrient factor`~ `Ripping factor`, ncol = 5)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot),
       caption = ""
  )



##### Plot 3. T1 Facet wrap ripping treatments Dates vs NDVI  ----

T1_all_rip <- plant %>% filter( `Nutrient factor` == "T1")
T1_all_rip %>% 
  filter(variable == variable_for_plot) %>%
  #filter( `Nutrient factor` == "T1") %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= date , y = value, group = date))+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ TreatmentDescription)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, ": ", unique(T1_all_rip$`Nutrient factor` )),
       caption = ""
  )

##### Plot 4. T1 Facet wrap ripping treatments Phenology_stage vs NDVI  ----
str(T1_all_rip)

T1_all_rip <- plant %>% filter( `Nutrient factor` == "T1")
T1_all_rip %>%  distinct(After_Phenology_stage)

After_Phenology_stage_order <- c(
 # "After.PreSowing",
 # "After.Sowing",
 # "After.Germination",
    "After.Emergence",
    "After.VernalSaturation",
    "After.TerminalSpikelet",
    "After.FlagLeaf", 
    "After.Heading",
    "After.Flowering",
    "After.StartGrainFill",
  #"After.EndGrainFill",
    "After.Maturity"
 # "After.HarvestRipe"
  )

T1_all_rip$After_Phenology_stage <-  factor(T1_all_rip$After_Phenology_stage, levels = After_Phenology_stage_order)

T1_all_rip$After_Phenology_stage

T1_all_rip %>% 
  filter(variable == variable_for_plot) %>%
  #filter( `Nutrient factor` == "T1") %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= After_Phenology_stage , y = value, group = After_Phenology_stage))+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ TreatmentDescription)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, ": ", unique(T1_all_rip$`Nutrient factor` )),
       caption = ""
  )


##### Table 1. Summary stats of NDVI for each Treatments  ----
str(plant)


NDVI_summary <- plant %>% 
  filter(variable == variable_for_plot) %>%
  filter( value != 0) %>% 
  group_by(TreatmentDescription, After_Phenology_stage) %>% 
  summarise(NDVI_max =  max(value, na.rm = TRUE),
            NDVI_min =  min(value, na.rm = TRUE),
            NDVI_mean = mean(value, na.rm = TRUE),
            NDVI_stdev = sd(value, na.rm = TRUE))

NDVI_summary

##### Table 2. Growth stage max NDVI occurred for each Treatments  ----


Max_NDVI_growth_stage <- NDVI_summary %>% 
  group_by(TreatmentDescription) %>% 
  summarise(NDVI_max =  max(NDVI_max, na.rm = TRUE))

Max_NDVI_growth_stage1 <- left_join(Max_NDVI_growth_stage, NDVI_summary)

Max_NDVI_growth_stage1 <- Max_NDVI_growth_stage1 %>% 
  select("TreatmentDescription" ,
         "After_Phenology_stage",
         "NDVI_max" ,
         "NDVI_min"  ,
         "NDVI_mean"  ,          
         "NDVI_stdev"  )
Max_NDVI_growth_stage1
write.csv(Max_NDVI_growth_stage1 ,
          paste0(path_name, "/", "Max_NDVI_growth_stage.csv"), row.names = FALSE )



# Tillers ----



names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "Tillers_m2"

######  Calculate mean ----
mean_Tillers_By_date <- plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>% 
  group_by(date) %>% 
  summarise(Tillers_mean=mean(value))


##### Plot 1. Facet wrap by date, treatments vs Tillers ----
plant %>% 
  filter(variable == variable_for_plot) %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= TreatmentDescription , y = value))+
  geom_hline(data=mean_Tillers_By_date, aes(yintercept=Tillers_mean, 
                                         col=as.factor(date)),
             linewidth=1.0, colour = "blue")+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ date)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot),
       caption = "Mean value displayed for each date"
  )


##### Plot 2. Facet wrap treatments Dates vs Tillers  ----


plant %>% 
  filter(variable == variable_for_plot) %>%
  filter( value != 0) %>% 
  
  ggplot(aes(x= date , y = value, group = date))+
  geom_point()+
  #geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(`Nutrient factor`~ `Ripping factor`, ncol = 5)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot),
       caption = ""
  )



##### Plot 3. T1 Facet wrap ripping treatments Dates vs Tillers  ----

T1_all_rip <- plant %>% filter( `Nutrient factor` == "T1")
T1_all_rip %>% 
  filter(variable == variable_for_plot) %>%
  #filter( `Nutrient factor` == "T1") %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= date , y = value, group = date))+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ TreatmentDescription)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, ": ", unique(T1_all_rip$`Nutrient factor` )),
       caption = ""
  )

##### Plot 4. T1 Facet wrap ripping treatments Phenology_stage vs Tillers  ----
str(T1_all_rip)

T1_all_rip <- plant %>% filter( `Nutrient factor` == "T1")
T1_all_rip %>%  distinct(After_Phenology_stage)

After_Phenology_stage_order <- c(
  # "After.PreSowing",
  # "After.Sowing",
  # "After.Germination",
  "After.Emergence",
  "After.VernalSaturation",
  "After.TerminalSpikelet",
  "After.FlagLeaf", 
  "After.Heading",
  "After.Flowering",
  "After.StartGrainFill",
  #"After.EndGrainFill",
  "After.Maturity"
  # "After.HarvestRipe"
)

T1_all_rip$After_Phenology_stage <-  factor(T1_all_rip$After_Phenology_stage, levels = After_Phenology_stage_order)

T1_all_rip$After_Phenology_stage

T1_all_rip %>% 
  filter(variable == variable_for_plot) %>%
  #filter( `Nutrient factor` == "T1") %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= After_Phenology_stage , y = value, group = After_Phenology_stage))+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ TreatmentDescription)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, ": ", unique(T1_all_rip$`Nutrient factor` )),
       caption = ""
  )


##### Table 1. Summary stats of Tillers for each Treatments  ----
str(plant)


Tillers_summary <- plant %>% 
  filter(variable == variable_for_plot) %>%
  filter( value != 0) %>% 
  group_by(TreatmentDescription, After_Phenology_stage) %>% 
  summarise(Tillers_max =  max(value, na.rm = TRUE),
            Tillers_min =  min(value, na.rm = TRUE),
            Tillers_mean = mean(value, na.rm = TRUE),
            Tillers_stdev = sd(value, na.rm = TRUE))

Tillers_summary

##### Table 2. Growth stage max Tillers occurred for each Treatments  ----


Max_Tillers_growth_stage <- Tillers_summary %>% 
  group_by(TreatmentDescription) %>% 
  summarise(Tillers_max =  max(Tillers_max, na.rm = TRUE))

Max_Tillers_growth_stage1 <- left_join(Max_Tillers_growth_stage, Tillers_summary)

Max_Tillers_growth_stage1 <- Max_Tillers_growth_stage1 %>% 
  select("TreatmentDescription" ,
         "After_Phenology_stage",
         "Tillers_max" ,
         "Tillers_min"  ,
         "Tillers_mean"  ,          
         "Tillers_stdev"  )
Max_Tillers_growth_stage1
write.csv(Max_Tillers_growth_stage1 ,
          paste0(path_name, "/", "Max_Tillers_growth_stage.csv"), row.names = FALSE )


# Plants_m2 ----



names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "Plants_m2"

######  Calculate mean ----
mean_Plants_By_date <- plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>% 
  group_by(date) %>% 
  summarise(Plants_mean=mean(value))


##### Plot 1. Facet wrap by date, treatments vs Plants ----
plant %>% 
  filter(variable == variable_for_plot) %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= TreatmentDescription , y = value))+
  geom_hline(data=mean_Plants_By_date, aes(yintercept=Plants_mean, 
                                            col=as.factor(date)),
             linewidth=1.0, colour = "blue")+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ date)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot),
       caption = "Mean value displayed for each date"
  )


##### Plot 2. Facet wrap treatments Dates vs Plants  ----


plant %>% 
  filter(variable == variable_for_plot) %>%
  filter( value != 0) %>% 
  
  ggplot(aes(x= date , y = value, group = date))+
  geom_point()+
  #geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(`Nutrient factor`~ `Ripping factor`, ncol = 5)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot),
       caption = ""
  )



##### Plot 3. T1 Facet wrap ripping treatments Dates vs Plants  ----

T1_all_rip <- plant %>% filter( `Nutrient factor` == "T1")
T1_all_rip %>% 
  filter(variable == variable_for_plot) %>%
  #filter( `Nutrient factor` == "T1") %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= date , y = value, group = date))+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ TreatmentDescription)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, ": ", unique(T1_all_rip$`Nutrient factor` )),
       caption = ""
  )

##### Plot 4. T1 Facet wrap ripping treatments Phenology_stage vs Plants  ----
str(T1_all_rip)

T1_all_rip <- plant %>% filter( `Nutrient factor` == "T1")
T1_all_rip %>%  distinct(After_Phenology_stage)

After_Phenology_stage_order <- c(
  # "After.PreSowing",
  # "After.Sowing",
  # "After.Germination",
  "After.Emergence",
  "After.VernalSaturation",
  "After.TerminalSpikelet",
  "After.FlagLeaf", 
  "After.Heading",
  "After.Flowering",
  "After.StartGrainFill",
  #"After.EndGrainFill",
  "After.Maturity"
  # "After.HarvestRipe"
)

T1_all_rip$After_Phenology_stage <-  factor(T1_all_rip$After_Phenology_stage, levels = After_Phenology_stage_order)

T1_all_rip$After_Phenology_stage

T1_all_rip %>% 
  filter(variable == variable_for_plot) %>%
  #filter( `Nutrient factor` == "T1") %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= After_Phenology_stage , y = value, group = After_Phenology_stage))+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ TreatmentDescription)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot, ": ", unique(T1_all_rip$`Nutrient factor` )),
       caption = ""
  )


##### Table 1. Summary stats of Plants for each Treatments  ----
str(plant)


Plants_summary <- plant %>% 
  filter(variable == variable_for_plot) %>%
  filter( value != 0) %>% 
  group_by(TreatmentDescription, After_Phenology_stage) %>% 
  summarise(Plants_max =  max(value, na.rm = TRUE),
            Plants_min =  min(value, na.rm = TRUE),
            Plants_mean = mean(value, na.rm = TRUE),
            Plants_stdev = sd(value, na.rm = TRUE))

Plants_summary

##### Table 2. Growth stage max Plants occurred for each Treatments  ----


Max_Plants_growth_stage <- Plants_summary %>% 
  group_by(TreatmentDescription) %>% 
  summarise(Plants_max =  max(Plants_max, na.rm = TRUE))

Max_Plants_growth_stage1 <- left_join(Max_Plants_growth_stage, Plants_summary)

Max_Plants_growth_stage1 <- Max_Plants_growth_stage1 %>% 
  select("TreatmentDescription" ,
         "After_Phenology_stage",
         "Plants_max" ,
         "Plants_min"  ,
         "Plants_mean"  ,          
         "Plants_stdev"  )
Max_Plants_growth_stage1
write.csv(Max_Plants_growth_stage1 ,
          paste0(path_name, "/", "Max_Plants_growth_stage.csv"), row.names = FALSE )


str(plant)
unique(plant$variable)
# Yield ----



names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "Yield"

######  Calculate mean ----
mean_Yield_By_date <- plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>% 
  group_by(date) %>% 
  summarise(Yield_mean=mean(value))


##### Plot 1. Facet wrap by date, treatments vs Yield ----
plant %>% 
  filter(variable == variable_for_plot) %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= TreatmentDescription , y = value))+
  geom_hline(data=mean_Yield_By_date, aes(yintercept=Yield_mean, 
                                           col=as.factor(date)),
             linewidth=1.0, colour = "blue")+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_wrap(.~ date)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot),
       caption = "Mean value displayed for each date"
  )





After_Phenology_stage_order <- c(
  # "After.PreSowing",
  # "After.Sowing",
  # "After.Germination",
  "After.Emergence",
  "After.VernalSaturation",
  "After.TerminalSpikelet",
  "After.FlagLeaf", 
  "After.Heading",
  "After.Flowering",
  "After.StartGrainFill",
  #"After.EndGrainFill",
  "After.Maturity"
  # "After.HarvestRipe"
)



##### Table 1. Summary stats of Yield for each Treatments  ----
str(plant)


Yield_summary <- plant %>% 
  filter(variable == variable_for_plot) %>%
  filter( value != 0) %>% 
  group_by(TreatmentDescription, After_Phenology_stage) %>% 
  summarise(Yield_max =  max(value, na.rm = TRUE),
            Yield_min =  min(value, na.rm = TRUE),
            Yield_mean = mean(value, na.rm = TRUE),
            Yield_stdev = sd(value, na.rm = TRUE)) %>% 
  arrange(Yield_mean)

Yield_summary



write.csv(Yield_summary ,
          paste0(path_name, "/", "Yield_summary.csv"), row.names = FALSE )




