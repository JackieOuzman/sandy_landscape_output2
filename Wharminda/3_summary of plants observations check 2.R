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
plant <- read_csv(paste0(path_name, "/plant_merged2025-04-30.csv"))
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
  filename = "wharminda_NDVI_vs_Date_Plot_2a.png",
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
  filename = "wharminda_NDVI_vs_DaysAfterSwowing_Plot_2b.png",
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)



##### Plot 3a. T1 Facet wrap ripping treatments days_since_sowing vs NDVI  ----

str(plant)

#https://cran.r-project.org/web/packages/ggpmisc/vignettes/model-based-annotations.html
Nutrient_Treatment <- "T5"


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
  filename = paste0("wharminda_", Nutrient_Treatment,  "_NDVI_vs_DaysAfterSwowing_Plot_3a.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)


##### Plot 4. T1 Facet wrap ripping treatments Phenology_stage vs NDVI  ----
# str(T1_all_rip)
# 
# T1_all_rip <- plant %>% filter( `Nutrient factor` == "T1")
# T1_all_rip %>%  distinct(After_Phenology_stage)
# 
# After_Phenology_stage_order <- c(
#   "before start of critical period",
#   "critical period",
#   "after start of critical period"
#   )
# 
# T1_all_rip$After_Phenology_stage <-  factor(T1_all_rip$After_Phenology_stage, levels = After_Phenology_stage_order)
# 
# T1_all_rip$After_Phenology_stage
# 
# T1_all_rip %>% 
#   filter(variable == variable_for_plot) %>%
#   #filter( `Nutrient factor` == "T1") %>% 
#   filter( value != 0) %>% 
#   
#   ggplot(aes(x= After_Phenology_stage , y = value, group = After_Phenology_stage))+
#   geom_point()+
#   geom_boxplot(alpha = 0.2)+
#   
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 45, 
#                                    vjust = 1, 
#                                    hjust=1
#   ),
#   axis.title = element_blank()) +                         
#   facet_wrap(.~ TreatmentDescription)+
#   labs(title = paste0(site_name, ": ", data_grouping),
#        subtitle = paste0(variable_for_plot, ": ", unique(T1_all_rip$`Nutrient factor` )),
#        caption = ""
#   )


##### Table 1. Summary stats of NDVI for each Treatments  ----
# str(plant)
# 
# 
# NDVI_summary <- plant %>% 
#   filter(variable == variable_for_plot) %>%
#   filter( value != 0) %>% 
#   group_by(TreatmentDescription, After_Phenology_stage) %>% 
#   summarise(NDVI_max =  max(value, na.rm = TRUE),
#             NDVI_min =  min(value, na.rm = TRUE),
#             NDVI_mean = mean(value, na.rm = TRUE),
#             NDVI_stdev = sd(value, na.rm = TRUE))
# 
# NDVI_summary
# 
# ##### Table 2. Growth stage max NDVI occurred for each Treatments  ----
# 
# 
# Max_NDVI_growth_stage <- NDVI_summary %>% 
#   group_by(TreatmentDescription) %>% 
#   summarise(NDVI_max =  max(NDVI_max, na.rm = TRUE))
# 
# Max_NDVI_growth_stage1 <- left_join(Max_NDVI_growth_stage, NDVI_summary)
# 
# Max_NDVI_growth_stage1 <- Max_NDVI_growth_stage1 %>% 
#   select("TreatmentDescription" ,
#          "After_Phenology_stage",
#          "NDVI_max" ,
#          "NDVI_min"  ,
#          "NDVI_mean"  ,          
#          "NDVI_stdev"  )
# Max_NDVI_growth_stage1
# write.csv(Max_NDVI_growth_stage1 ,
#           paste0(path_name, "/", "Max_NDVI_growth_stage.csv"), row.names = FALSE )
# 


# Biomass ----



names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "Biomass_t_ha"

######  Calculate mean ----
Biomass_t_ha_by_date <- plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>% 
  group_by(date) %>% 
  summarise(Tillers_mean=mean(value))



##### Plot 1. Facet wrap by date, treatments vs biomass ----
plant %>% 
  filter(variable == variable_for_plot) %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= TreatmentDescription , y = value))+
  geom_hline(data=Biomass_t_ha_by_date, aes(yintercept=Tillers_mean, 
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






ggsave(
  device = "png",
  filename = paste0("wharminda_", "biomass",  "_vs_Treamnet.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)

##### Plot 3. T1 Facet wrap ripping treatments Dates vs biomass  ----

# T1_all_rip <- plant %>% filter( `Nutrient factor` == "T1")
# T1_all_rip %>% 
#   filter(variable == variable_for_plot) %>%
#   #filter( `Nutrient factor` == "T1") %>% 
#   filter( value != 0) %>% 
#   
#   ggplot(aes(x= date , y = value, group = date))+
#   geom_point()+
#   geom_boxplot(alpha = 0.2)+
#   
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 45, 
#                                    vjust = 1, 
#                                    hjust=1
#   ),
#   axis.title = element_blank()) +                         
#   facet_wrap(.~ TreatmentDescription)+
#   labs(title = paste0(site_name, ": ", data_grouping),
#        subtitle = paste0(variable_for_plot, ": ", unique(T1_all_rip$`Nutrient factor` )),
#        caption = ""
#   )

##### Plot 4. T1 Facet wrap ripping treatments Phenology_stage vs biomass  ----
# str(T1_all_rip)
# 
# T1_all_rip <- plant %>% filter( `Nutrient factor` == "T1")
# T1_all_rip %>%  distinct(After_Phenology_stage)
# 
# After_Phenology_stage_order <- c(
#   "before start of critical period",
#   "critical period",
#   "after start of critical period"
# )
# 
# T1_all_rip$After_Phenology_stage <-  factor(T1_all_rip$After_Phenology_stage, levels = After_Phenology_stage_order)
# 
# T1_all_rip$After_Phenology_stage
# 
# T1_all_rip %>% 
#   filter(variable == variable_for_plot) %>%
#   #filter( `Nutrient factor` == "T1") %>% 
#   filter( value != 0) %>% 
#   
#   ggplot(aes(x= After_Phenology_stage , y = value, group = After_Phenology_stage))+
#   geom_point()+
#   geom_boxplot(alpha = 0.2)+
#   
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 45, 
#                                    vjust = 1, 
#                                    hjust=1
#   ),
#   axis.title = element_blank()) +                         
#   facet_wrap(.~ TreatmentDescription)+
#   labs(title = paste0(site_name, ": ", data_grouping),
#        subtitle = paste0(variable_for_plot, ": ", unique(T1_all_rip$`Nutrient factor` )),
#        caption = ""
#   )


##### Table 1. Summary stats of biomass for each Treatments  ----
# str(plant)
# 
# 
# biomass_summary <- plant %>% 
#   filter(variable == variable_for_plot) %>%
#   filter( value != 0) %>% 
#   group_by(TreatmentDescription, After_Phenology_stage) %>% 
#   summarise(biomass_max =  max(value, na.rm = TRUE),
#             biomass_min =  min(value, na.rm = TRUE),
#             biomass_mean = mean(value, na.rm = TRUE),
#             biomass_stdev = sd(value, na.rm = TRUE))
# 
# biomass_summary

##### Table 2. Growth stage max biomass occurred for each Treatments  ----


# Max_biomass_growth_stage <- biomass_summary %>% 
#   group_by(TreatmentDescription) %>% 
#   summarise(biomass_max =  max(biomass_max, na.rm = TRUE))
# 
# Max_biomass_growth_stage1 <- left_join(Max_biomass_growth_stage, biomass_summary)
# 
# Max_biomass_growth_stage1 <- Max_biomass_growth_stage1 %>% 
#   select("TreatmentDescription" ,
#          "After_Phenology_stage",
#          "biomass_max" ,
#          "biomass_min"  ,
#          "biomass_mean"  ,          
#          "biomass_stdev"  )
# Max_biomass_growth_stage1
# write.csv(Max_biomass_growth_stage1 ,
#           paste0(path_name, "/", "biomass_growth_stage.csv"), row.names = FALSE )


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
  "before start of critical period",
  "critical period",
  "after start of critical period"
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
# str(plant)
# 
# 
# Plants_summary <- plant %>% 
#   filter(variable == variable_for_plot) %>%
#   filter( value != 0) %>% 
#   group_by(TreatmentDescription, After_Phenology_stage) %>% 
#   summarise(Plants_max =  max(value, na.rm = TRUE),
#             Plants_min =  min(value, na.rm = TRUE),
#             Plants_mean = mean(value, na.rm = TRUE),
#             Plants_stdev = sd(value, na.rm = TRUE))
# 
# Plants_summary

##### Table 2. Growth stage max Plants occurred for each Treatments  ----


# Max_Plants_growth_stage <- Plants_summary %>% 
#   group_by(TreatmentDescription) %>% 
#   summarise(Plants_max =  max(Plants_max, na.rm = TRUE))
# 
# Max_Plants_growth_stage1 <- left_join(Max_Plants_growth_stage, Plants_summary)
# 
# Max_Plants_growth_stage1 <- Max_Plants_growth_stage1 %>% 
#   select("TreatmentDescription" ,
#          "After_Phenology_stage",
#          "Plants_max" ,
#          "Plants_min"  ,
#          "Plants_mean"  ,          
#          "Plants_stdev"  )
# Max_Plants_growth_stage1
# write.csv(Max_Plants_growth_stage1 ,
#           paste0(path_name, "/", "Max_Plants_growth_stage.csv"), row.names = FALSE )
# 
# 
# str(plant)
# unique(plant$variable)


# Tiller_m2  ----



names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "Tiller_m2"

######  Calculate mean ----
mean_Tillers_By_date <- plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>% 
  group_by(date) %>% 
  summarise(Plants_mean=mean(value))

mean_Tillers_By_date
##### Plot 1. Facet wrap by date, treatments vs Plants ----
plant %>% 
  filter(variable == variable_for_plot) %>% 
  filter( value != 0) %>% 
  
  ggplot(aes(x= TreatmentDescription , y = value))+
  # geom_hline(data=mean_Plants_By_date, aes(yintercept=mean_Tillers_By_date, 
  #                                          col=as.factor(date)),
  #          linewidth=1.0, colour = "blue")+
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


ggsave(
  device = "png",
  filename = paste0("wharminda_", "Tillers",  "_vs_Treamnet.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)


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
  "before start of critical period",
  "critical period",
  "after start of critical period"
)



##### Table 1. Summary stats of Yield for each Treatments  ----
# str(plant)
# 
# 
# Yield_summary <- plant %>% 
#   filter(variable == variable_for_plot) %>%
#   filter( value != 0) %>% 
#   group_by(TreatmentDescription, After_Phenology_stage) %>% 
#   summarise(Yield_max =  max(value, na.rm = TRUE),
#             Yield_min =  min(value, na.rm = TRUE),
#             Yield_mean = mean(value, na.rm = TRUE),
#             Yield_stdev = sd(value, na.rm = TRUE)) %>% 
#   arrange(Yield_mean)
# 
# Yield_summary
# 
# 
# 
# write.csv(Yield_summary ,
#           paste0(path_name, "/", "Yield_summary.csv"), row.names = FALSE )



ggsave(
  device = "png",
  filename = paste0("wharminda_", "Yield",  "_vs_Treamnet.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)





##### Plot 1. Facet wrap by date, treatments vs Yield quality Protein only----
names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "Protein"

######  Calculate mean ----
mean_Yield_By_date <- plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>% 
  group_by(date) %>% 
  summarise(Yield_mean=mean(value))


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


ggsave(
  device = "png",
  filename = paste0("wharminda_", "Protein",  "_vs_Treamnet.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)


##### Plot 1. Facet wrap by date, treatments vs Yield quality Moisture only----
names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "Moisture"

######  Calculate mean ----
mean_Yield_By_date <- plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>% 
  group_by(date) %>% 
  summarise(Yield_mean=mean(value))


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


ggsave(
  device = "png",
  filename = paste0("wharminda_", "Moisture",  "_vs_Treamnet.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)

##### Plot 1. Facet wrap by date, treatments vs Yield quality TGW_g  only----
names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "TGW_g"

######  Calculate mean ----
mean_Yield_By_date <- plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>% 
  group_by(date) %>% 
  summarise(Yield_mean=mean(value))


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


ggsave(
  device = "png",
  filename = paste0("wharminda_", "TGW_g",  "_vs_Treamnet.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)


##### Plot 1. Facet wrap by date, treatments vs Yield quality Screenings_perc  only----
names(plant)
str(plant)
plant %>%  distinct(variable)

variable_for_plot <- "Screenings_perc"

######  Calculate mean ----
mean_Yield_By_date <- plant %>% 
  filter(variable == variable_for_plot, na.rm = TRUE) %>% 
  group_by(date) %>% 
  summarise(Yield_mean=mean(value))


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


ggsave(
  device = "png",
  filename = paste0("wharminda_", "Screenings_perc",  "_vs_Treamnet.png"),
  path= paste0(sandy_landscape_folder,site,raw_data, "R_outputs/plots/") ,
  width=8.62,
  height = 6.28,
  dpi=600
)
