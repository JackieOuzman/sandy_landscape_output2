#Temp for source names


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
plant <- read_csv(paste0(path_name, "/plant_merged_test.csv"), 
                  col_types = cols(date = col_double()))

# convert date clm
plant$date <- as.Date(plant$date, origin = "1899-12-30")

#remove rows with no data values
plant <- plant %>% filter(!is.na(value ))
str(plant)
#################################################################################



plant <- plant %>% 
  mutate(source1 =
           gsub('[^[:alnum:] ]', '', source)) %>% 
  mutate(source2 = 
           gsub("fs1cbrnexuscsiroauafsandysoilsiiworkOutput2SiteData2SSO2CopevilleFarley","",source1)) %>% 
  mutate(source_abbreviation =
    gsub("[[:space:]]", "", source2)) %>% 
  select(-source1, - source2)
plant


#### I should remove some data Biomasstiller 7/8/2024 NDVI records

unique(plant$source_abbreviation) # "3PlantMeasurementsSSO2Biomasstillercounts2024xlsx"




check<-
  plant %>%
  filter(!source_abbreviation %in% c("3PlantMeasurementsSSO2Biomasstillercounts2024xlsx") |
           !date %in% c("2024-08-07")|
           !variable %in% c("NDVI")
         )

unique(check$date)
unique(check$source_abbreviation)

Biomasstillercounts_after_filter <- check %>% filter(source_abbreviation == "3PlantMeasurementsSSO2Biomasstillercounts2024xlsx")
Biomasstillercounts_after_filter_07_08 <- Biomasstillercounts_after_filter %>% 
  filter(date == "2024-08-07")
Biomasstillercounts_after_filter_07_08
unique(Biomasstillercounts_after_filter_07_08$variable)


unique(Biomasstillercounts_after_filter$date)
unique(Biomasstillercounts_after_filter$variable)

df <- data.frame(team=c('P1', 'P2', 'P1', 'P4', 'P5', 'P6', 'P7', 'P8'),
                 points=c('A', 'A', 'B', 'B', 'C', 'C', 'C', 'D'),
                 assists=c(133, 128, 131, 139, 134,55,66,135),
                 rebounds=c(18, 18, 14, 13, 12, 15, 17, 12))
df

df %>%
  filter(!team %in% c('P1')& !points %in% c('A')) %>% 
  arrange(team)
## this removes all of P1 and all of points A
## but I want to remove JUST P1 with A
df %>%
  filter(!team %in% c('P1')| !points %in% c('A'))%>% 
  arrange(team)
## but I want to remove JUST P1 with A ie keep P1 with B _ done :)
