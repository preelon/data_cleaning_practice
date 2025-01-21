rm(list=ls())
#libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(devtools)
library(sf)

#reading the latest HMIS data from 2023-2024
#mal_2023_2024 <-read_csv("C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/raw/malaria2023-2024.csv", col_names = FALSE)

#saving the 2023/24 data in RDS form
#saveRDS(mal_2023_2024, file = "C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/raw/malaria2023-2024.rds")

#reading the HMIS data from 2017-2023
#mal_2017_2023 <- read_csv("C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/raw/malaria 2017-23.csv")

#saving the 2017-23 data in RDS form
#saveRDS(mal_2017_2023, file = "C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/raw/mal_2017_2023.rds")

#loading the RDS file for the 2017-2023 HMIS dataset
mal_17_23 <- read_rds("C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/raw/mal_2017_2023.rds")%>%
  select(-data_element_id, -organisation_unit_id, -category_option_combo_id, -attribute_option_combo_id)

#loading the RDS file for the latest 2023-2024 HMIS
mal_23_24 <- read_rds("C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/raw/malaria2023-2024.rds")%>%
  select(-data_element_id, -organisation_unit_id, -category_option_combo_id, -attribute_option_combo_id)

#comparing the two HMIS data (the one from 2017-2023 and 2023-2024)
#mal_23_24 %>%
# mutate(year = year(period_start_date)) %>%
#filter(region== "South West Ethiopia Region") %>%
#group_by(region, data_element_name, year) %>%
#summarise(total_val = sum(value, na.rm=T)) %>%
#ungroup()

#table(mal_23_24$region)

#mal_17_23 %>%
# mutate(year = year(period_start_date))%>%
#filter(region== "Southwest Ethiopia Regional Health Bureau", year== 2023) %>%
#group_by(region, data_element_name, year) %>%
#summarise(total_val = sum(value, na.rm=T)) %>%
#ungroup()

#table(mal_17_23$region)

#mal_23_24 %>% 
# group_by(region & data_element_name) %>%
#summarise()

#removing the dataframes i no more use from the environment
rm(mal_17_23)
rm(mal_23_24)
rm(headers)

#creating a vector containing the header names (I'm creating this because the 2023-24 HMIS data
# doesn't have a header), so the headers are the ones in the 2017-23 dataset
#headers <- colnames(mal_17_23)
#print(headers)

#assigning the headers to the mal_23_24 dataset
#names(mal_23_24) <- headers

#saving the 2023/24 data with headers in RDS form
#saveRDS(mal_23_24, file = "C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/raw/malaria2023-2024.rds")


#reading the Ethiopia's shapefile
dat_1082 <- read_csv("C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/raw/eth_admpop_adm3_model_input_pop22_1082.csv") %>%
 select(id_1082, region, zone, woreda, T_TL)

#creating data frame for central and South regions
south <- c("Alle", "Amaro", "Basketo", "Burji", "Derashe", "Gamo", "Gofa", "Gedeo", "Konso", "South Omo", "Wolayita" )

central <- c("Guraghe", "Hadiya", "Halaba", "Kembata Tembaro", "Siltie", "Yem Special")

#standardizing the shapefile based on the latest regional classification
dat_1082 <- dat_1082 %>%
  mutate(region= case_when(zone %in% south ~ "South",
                           zone %in% central ~ "Central",
                           TRUE ~ region)) %>%
  mutate(region= gsub(" Ethiopia", "", region))%>% #removing Ethiopia from regional name
  mutate(region= str_trim(region, side = "both")) #removing trailing spaces from the regions

unique(dat_1082$region)


#extracting the 2024 data from the second dataset so that it can be merged into the 2017-2023 data
#mal_2024 <- mal_23_24_small %>%
#filter(year(period_start_date)==2024 & reg) 



#combining the 2017-2023 dataset to the 2024 dataset; this is my working dataset from now on
mal_2017_2024 <- bind_rows(mal_17_23, mal_23_24 %>% filter(year(period_start_date)==2024)
) %>% mutate(region_old= region,
             zone_old= zone,
             woreda_old= woreda,
             facility_old = facility,
             phcu_old= phcu)

#saving the raw form of the 2017 to 2014 dataset
saveRDS(mal_2017_2024, file = "C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/raw/mal_2017_2024.rds")

#reading the saved 2017-2024 rds file
readRDS("C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/raw/mal_2017_2024.rds")



#standardizing regional names in the HMIS data
unique(mal_2017_2024$region)
unique(dat_1082$region)

mal_2017_2024 <- mal_2017_2024 %>%
  mutate(region=str_to_title(region),
         zone= str_to_title(zone),
         woreda= str_to_title(woreda),
         facility = str_to_title(facility),
         phcu= str_to_title(phcu)) %>%
  mutate(region = gsub(" Ethiopia Regional Health Bureau", "", region)) %>%
  mutate(region= gsub(" Regional Health Bureau", "", region)) %>%
  mutate(region= gsub(" Ethiopia", "", region)) %>%
  mutate(region=gsub(" Region", "", region)) %>%
  mutate(region=gsub(" City Administration", "", region))

#lets create a data frame of unique region names from the reference
region_names <- data.frame(regions= unique(mal_2017_2024$region))

#removing unnecessary frames from the env't
rm(mal_17_23)
rm(mal_23_24)
rm(region_names_common)


#checking the region names that are in the shape file but in the HMIS data
inconsistent_region <- region_names %>%
  filter(!regions %in% dat_1082$region) %>%
  arrange(regions) #it results in region names that are in the HMIS data but in the shape file
# results were 3 (Southwest, Oromiya and Centraln)

#changing the 3 inconsistent names to the ones in shapefile
mal_2017_2024 <- mal_2017_2024 %>%
  mutate(region= case_when(
    region== "Centraln" ~ "Central",
    region== "Oromiya" ~ "Oromia",
    region== "Southwest" ~ "South West",
    TRUE ~ region))

unique(mal_2017_2024$region)

#bringing regional names into consistency #this script gives the same output as the above
#mal_2017_2024 <- mal_2017_2024 %>%
#mutate(region= str_replace_all(region, c("Centraln" = "Central",
#"Southwest" = "South West",
#"Oromiya" = "Oromia")))       

#saving the 2017-2024 dataframe into the repository
saveRDS(mal_2017_2024, file = "C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/processed/mal_2017_2024.rds")

readRDS("C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/processed/mal_2017_2024.rds")
