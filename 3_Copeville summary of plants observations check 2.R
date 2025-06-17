# This file is for summering and checking plant data that has been merge PART 2
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
#install.packages("openxlsx")
library(openxlsx)
library(ggpmisc)
#install.packages("ggpmisc")


# data files -------------------------------------------------------
site_name <- "Copeville"
data_grouping <- "Plant observation"

sandy_landscape_folder <- "H:/Output-2/Site-Data/"
site <- "2._SSO2_Copeville-Farley/"
raw_data <- "Jackies_working/"
R_outputs <- "R_outputs/checked_data"


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
plant <- read_csv(paste0(path_name, "/plant_merged2025-06-18.csv"))



str(plant)

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


##### Plot 2a. Facet wrap treatments Dates vs NDVI  ----


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
  facet_grid(`Nutrient factor`~ `Ripping factor`)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot),
       caption = ""
  )

ggsave(
  device = "png",
  filename = "Copeville_NDVI_vs_Date_Plot_2a.png",
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)

##### Plot 2b. Facet wrap treatments days after sowing vs NDVI  ----

str(plant)
plant %>% 
  filter(variable == variable_for_plot) %>%
  filter( value != 0) %>% 
  
  ggplot(aes(x= days_since_sowing , y = value, group = date))+
  geom_point()+
  #geom_boxplot(alpha = 0.2)+
  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  facet_grid(`Nutrient factor`~ `Ripping factor`)+
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0(variable_for_plot),
       caption = ""
  )



ggsave(
  device = "png",
  filename = "Copeville_NDVI_vs_DaysAfterSwowing_Plot_2b.png",
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)



##### Plot 3a. T1 Facet wrap ripping treatments days_since_sowing vs NDVI  ----

str(plant)

#https://cran.r-project.org/web/packages/ggpmisc/vignettes/model-based-annotations.html
Nutrient_Treatment <- "T6"


Tx_all_rip <- plant %>% filter( `Nutrient factor` == Nutrient_Treatment)
formula <- y ~ poly(x, 2, raw = TRUE)


Tx_all_rip %>% 
  filter(variable == variable_for_plot) %>%
  filter( value != 0) %>% 
  ggplot(aes(x= days_since_sowing , y = value#, 
             #group= days_since_sowing
             ))+
  geom_point()+
  geom_smooth(
              method = "glm", 
              formula = formula, 
              se = FALSE
    )+
  stat_poly_line(formula = formula) +
  stat_poly_eq(use_label(c("eq")),  
               label.y = "bottom",
               method = "glm",formula = formula)+
  #geom_boxplot(alpha = 0.2)+ #need the grouping for this but I can't get line fit with grouping
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) + 
  facet_wrap(.~ `Ripping factor`)+
  #scale_x_continuous(limits = c(50, 150), breaks = c(50, 75, 100, 125, 150))+
  labs(title = paste0(site_name),
       subtitle = 
         paste0(variable_for_plot,
                 " vs Days after sowing",
                ". Nutrient treatment = ", 
                unique(Tx_all_rip$`Nutrient factor`),
                ". All ripping treatments displayed as facet." ),
                caption = ""
  )


ggsave(
  device = "png",
  filename = paste0("Copeville_", Nutrient_Treatment,  "_NDVI_vs_DaysAfterSwowing_Plot_3a.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
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

######  Calculate new clm for tiller survival  ----
str(plant)
names(plant)

Tiller_subset <-  
  plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>%
  select(
    Short_ID,
    date,
    variable,
    value
    )  %>% 
   
  pivot_wider(names_from = date       ,
              values_from = value) 

Tiller_subset <- Tiller_subset %>% 
  rename(end_date = `2024-09-26`,
         start_date = `2024-08-07`)
  
Tiller_subset <- Tiller_subset %>% 
mutate(Tiller_survival_percent =
           (end_date / start_date)*100) #end date - start date *100

Tiller_subset <- left_join(Tiller_subset, plant)

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




##### Plot 2. Treatments vs Tiller_survival_perc ----
Tiller_subset
names(Tiller_subset)

Tiller_subset %>% 
  filter(variable == variable_for_plot) %>% 
  filter( value != 0) %>% 
  ggplot(aes(x= TreatmentDescription , y = Tiller_survival_percent))+
  geom_point()+
  geom_boxplot(alpha = 0.2)+
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1
  ),
  axis.title = element_blank()) +                         
  labs(title = paste0(site_name, ": ", data_grouping),
       subtitle = paste0("Tiller survival percent"),
       caption = ""
  )

ggsave(
  device = "png",
  filename = paste0("Copeville_", "Tiller_survival_percent",  "_vs_Treamnet.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
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



ggsave(
  device = "png",
  filename = paste0("Copeville_", "Yield",  "_vs_Treamnet.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)



# Biomass ----



names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "Biomass_kg_ha"

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


Biomass_summary <- plant %>% 
  filter(variable == variable_for_plot) %>%
  filter( value != 0) %>% 
  group_by(TreatmentDescription, After_Phenology_stage) %>% 
  summarise(Biomass_max =  max(value, na.rm = TRUE),
            Biomass_min =  min(value, na.rm = TRUE),
            Biomass_mean = mean(value, na.rm = TRUE),
            Biomass_stdev = sd(value, na.rm = TRUE)) %>% 
  arrange(Biomass_mean)

Biomass_summary



write.csv(Biomass_summary ,
          paste0(path_name, "/", "Biomass_summary.csv"), row.names = FALSE )



ggsave(
  device = "png",
  filename = paste0("Copeville_", "Biomass",  "_vs_Treamnet.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)

