rm(list=ls())
#libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(devtools)
library(sf)


#list.files("data/raw")
#loading the RDS file for the 2017-2023 HMIS dataset
mal_17_23_orig =read_csv("data/raw/malaria 2017-23.csv")

#saving a df of 2017-23 data with the below selected variables
mal_17_23 <- mal_17_23_orig %>%
  select(-data_element_id, -organisation_unit_id, -category_option_combo_id, -attribute_option_combo_id)

#loading the RDS file for the latest 2023-2024 HMIS
mal_23_24_orig <- read_csv("data/raw/malaria2023-2024.csv")

 #while seeing the mal_23_24 data, I learned that it doesn't have a header so lets create one
#creating a vector containing the header names (the headers are the ones in the 2017-23 dataset)
  headers <- colnames(mal_17_23_orig)
  
  #assigning the headers to the mal_23_24 dataset
  names(mal_23_24_orig) <- headers
  
#creating a df of a 23-24 Hmis data with the below selected variables
  mal_23_24<- mal_23_24_orig %>%
    select(-data_element_id, -organisation_unit_id, -category_option_combo_id, -attribute_option_combo_id)

#checking the headers for the mal_23_24 data to make sure the headers are in place  
  names(mal_23_24)
  
#saving the 2023/24 data with headers in RDS form
  write.csv(mal_23_24, file = "data/processed/mal_2023_2024_wiz_headers.csv")
  
#combining the 2017-2023 data set to the 2024 data set; this is my working data set from now on wards
  mal_2017_2024 <- bind_rows(mal_17_23, mal_23_24 %>% 
                               filter(year(period_start_date)==2024)) %>% #this is to filter only the 2024 data from the 23-24 hmis
    mutate(region_old= region,
               zone_old= zone,
               woreda_old= woreda,
               facility_old = facility,
               phcu_old= phcu) %>%
    mutate(facility = case_when(!is.na(phcu) & is.na(facility)  ~ phcu, # fill NA within facility column
                                is.na(phcu) & is.na(facility) & !is.na(woreda) ~ woreda,
                                is.na(phcu) & is.na(facility) & is.na(woreda) ~ zone,
                                TRUE ~ facility)) %>%
    mutate(woreda = case_when(is.na(woreda)~ zone,
                              TRUE ~ woreda)) %>% # fill NA within woreda column
    mutate(region = str_to_title(region)) %>% # title cases
    mutate(zone = str_to_title(zone)) %>%
    mutate(woreda = str_to_title(woreda)) %>%
    mutate(facility = str_to_title(facility)) %>%
    mutate(phcu= str_to_title(phcu))%>%
    mutate(region = str_trim(region, side = "both"))%>%
    mutate(zone= str_trim(zone, side = "both"))%>%
    mutate(woreda= str_trim(woreda, side = "both"))%>%
    mutate(facility= str_trim(facility, side = "both"))%>%
    mutate(phcu= str_trim(phcu, side = "both")) %>%
    mutate(region = gsub(",", " ", region)) %>%   # remove text that could introduce inconsistencies 
    mutate(region = gsub(" Regional Health Bureau", "", region)) %>%    
    mutate(region = gsub(" Ethiopia", "", region)) %>%   # remove text that could introduce inconsistencies 
    mutate(region = gsub(" Ethiopia Regional Health Bureau", "", region)) %>%
    mutate(region= gsub(" Regional Health Bureau", "", region)) %>%
    mutate(region= gsub(" Ethiopia", "", region)) %>%
    mutate(region=gsub(" Region", "", region)) %>%
    mutate(region=gsub(" City Administration", "", region))
  
----------------------------------------------------------------------------------------- 
#I kept the below codes (upto the level of the dash) because I want to refer them for my future use

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
--------------------------------------------------------------------------------------------
#removing the data frames i don't need from the environment
rm(mal_17_23_orig)
rm(mal_23_24_orig)
rm(dat_1082)

#reading the Ethiopia's shapefile and changing the region, zone and woreda to title case
dat_1082_orig <- read_csv("data/raw/eth_admpop_adm3_model_input_pop22_1082.csv")


#standardizing the regions in the shape file based on the recent adminstrative division
#creating data frame for central and South regions
south <- c("Alle", "Amaro", "Basketo", "Burji", "Derashe", "Gamo", "Gofa", "Gedeo", "Konso", "South Omo", "Wolayita" )

central <- c("Guraghe", "Hadiya", "Halaba", "Kembata Tembaro", "Siltie", "Yem Special")

#integrating south and central regions in the sf
dat_1082_orig <- dat_1082_orig %>%
  mutate(region= case_when(zone %in% south ~ "South",
                           zone %in% central ~ "Central",
                           TRUE ~ region))

#saving the shapefile with updated regional classification
write.csv(dat_1082_orig, "data/processed/eth_sf_updated.csv")

#creating a shapefile df with only the below selected variables
dat_1082 <- dat_1082_orig %>% #this will be my working shapefile during the cleaning
  select(id_1082, region, zone, woreda, pop_2022= T_TL) 

#removing words and spaces that cause inconsistencies 
dat_1082 <- dat_1082 %>%
  mutate(region = str_to_title(region)) %>% # title cases
  mutate(zone = str_to_title(zone)) %>%
  mutate(woreda = str_to_title(woreda)) %>%
  mutate(region= gsub(" Ethiopia", "", region))%>% #removing Ethiopia from regional name
  mutate(region= str_trim(region, side = "both")) #removing trailing spaces from the regions


unique(dat_1082$region)

#checking the region names that are in the shape file but in the HMIS data
data.frame(regions= unique(mal_2017_2024$region)) %>%
  filter(!regions %in% dat_1082$region) %>% #it results in region names that are in the HMIS data but in the shape file
  arrange(regions)                         # results were 3 (Southwest, Oromiya and Centraln)

#changing the 3 inconsistent names in the HMIS
mal_2017_2024 <- mal_2017_2024 %>%
  mutate(region= case_when(
    region== "Centraln" ~ "Central",
    region== "Oromiya" ~ "Oromia",
    region== "Southwest" ~ "South West",
    TRUE ~ region))

#bringing the 3 regional names into consistency:this script gives the same output as the above
#mal_2017_2024 <- mal_2017_2024 %>%
#mutate(region= str_replace_all(region, c("Centraln" = "Central",
#"Southwest" = "South West",
#"Oromiya" = "Oromia")))  

#re-checking if the 3 regional name inconsistencies were taken care of
data.frame(regions= unique(mal_2017_2024$region)) %>%
  filter(!regions %in% dat_1082$region) %>% 
  arrange(regions)  #inconsistencies were corrected

#removing the data sets I don't need for this analysis from the env't
rm(mal_17_23)
rm(mal_23_24)
rm(dat_1082_orig)


identical(unique(mal_2017_2024$region), unique(dat_1082$region)) #this tells me whether the regions in the sf
                                                                  #& HMIS are the same or not (TRUE/FALSE)

sort(unique(mal_2017_2024$region))
sort(unique(dat_1082$region))

#saving the 2017-2024 dataframe into the repository
saveRDS(mal_2017_2024, file = "data/processed/region_reconciled.rds")

#-------------------------------End---------------------------------