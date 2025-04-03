rm(list=ls())
#libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(devtools)
library(sf)

#mal_2017_2024_saved <- readRDS("data/processed/region_reconciled.rds")

#list.files("data/raw")
#loading the RDS file for the 2017-2023 HMIS dataset
mal_17_23_orig =read_csv("data/raw/malaria 2017-23.csv")

#saving a df of 2017-23 data with the below selected variables
mal_17_23 <- mal_17_23_orig %>%
  select(-data_element_id, -category_option_combo_id, -attribute_option_combo_id)

#loading the RDS file for the latest 2023-2024 HMIS
mal_23_24_orig <- read_csv("data/raw/malaria2023-2024.csv")

 #while seeing the mal_23_24 data, I learned that it doesn't have a header so lets create one
#creating a vector containing the header names (the headers are the ones in the 2017-23 dataset)
  headers <- colnames(mal_17_23_orig)
  
  #assigning the headers to the mal_23_24 dataset
  names(mal_23_24_orig) <- headers
  
#creating a df of a 23-24 Hmis data with the below selected variables
  mal_23_24<- mal_23_24_orig %>%
    select(-data_element_id, -category_option_combo_id, -attribute_option_combo_id)

#changing the dates for the period_sart_date to 01 as I saw same dates that
#are not 01
mal_17_23 <- mal_17_23 %>%
    mutate(period_start_date = ymd(paste0(year(period_start_date), "-", 
                                          month(period_start_date), 
                                          "-01")))
  
mal_23_24 <- mal_23_24 %>%
    mutate(period_start_date = ymd(paste0(year(period_start_date), "-", 
                                          month(period_start_date), 
                                          "-01")))

#extracting 2023 from both data frames so that i can compare and decide
#from where to take the 2023 values
mal_23_24_2023 <- mal_23_24 |>
  filter(year(period_start_date)==2023) |>
  #group_by(region, data_element_name)|>
  #summarise(total_cases= sum(value), .groups = "drop")
  
  mal_17_23_2023 <- mal_17_23 |>
  filter(year(period_start_date)== 2023)
#group_by(region, data_element_name) |>
#summarize(total_cases= sum(value, na.rm = T), .groups = "drop")

write.csv(mal_17_23_2023, "data/2023_1_temp.csv") #I assessed the d/ce and
write.csv(mal_23_24_2023, "data/2023_2_temp.csv") #the 2023 value for all regions
#except for south and south west should be taken from the 2023-24 df
#for south and south west we will take the data fro 2017-2023 df
  
#based on my observation above, I'll create a 2017-2024 df by taking 2023 value 
#for the sw and south regions from the 2017-2023 and for the rest of the
#regions from the 2023-24 df
  unique(mal_17_23$region)
  
#Extract South & South West 2023 data from mal_17_23
south_sw_2023 <- mal_17_23 %>%
  filter(year(period_start_date) == 2023 & region %in% c("South Ethiopia Regional Health Bureau",
                                                           "Southwest Ethiopia Regional Health Bureau"))
  
#Extract all other regions' 2023 data from mal_23_24
other_regions_2023 <- mal_23_24 %>%
  filter(year(period_start_date) == 2023 & !region %in% c("South Ethiopia Regional Health Bureau",
                                                            "Southwest Ethiopia Regional Health Bureau"))
 rm(data_2017_2022) 
#Extract 2017-2022 data from mal_17_23
mal_2017_2022 <- mal_17_23 %>%
    filter(year(period_start_date) < 2023)
  
#Extract 2024 data from mal_23_24
mal_2024 <- mal_23_24 %>%
  filter(year(period_start_date) == 2024)
  
#Now lets Combine all the datasets I create above
mal_2017_2024 <- bind_rows(mal_2017_2022, 
                           south_sw_2023, 
                           other_regions_2023,
                           mal_2024)  
names(mal_2017_2024)
#lets select only the required cols before saving the final 2017-2024 data
mal_2017_2024 <- mal_2017_2024 |>
  select(-period, -period_end_date)

#lets save the mal_2017_2024 data for future use
  saveRDS(mal_2017_2024, "data/processed/hmis 2017-2024 uncleaned.rds")
  
#standardizing the hmis variables 
  mal_2017_2024<- mal_2017_2024 %>%
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

#reading the Ethiopia's shapefile and changing the region, zone and woreda to title case
shape_file<- read_csv("data/raw/eth_admpop_adm3_model_input_pop22_1082.csv") %>%
    select(id_1082, region, zone, woreda, pop_2022= T_TL)


#standardizing the regions in the shape file based on the recent adminstrative division
#creating data frame for central and South regions
south <- c("Alle", "Amaro", "Basketo", "Burji", "Derashe", "Gamo", "Gofa", "Gedeo", "Konso", "South Omo", "Wolayita" )

central <- c("Guraghe", "Hadiya", "Halaba", "Kembata Tembaro", "Siltie", "Yem Special")

#integrating south and central regions in the sf
shape_file <- shape_file %>%
  mutate(region= case_when(zone %in% south ~ "South",
                           zone %in% central ~ "Central",
                           TRUE ~ region))

#removing the original sf from memory

sort(unique(shape_file$region))
sort(unique(shape_file$zone))
sort(unique(shape_file$woreda))

#removing words and spaces that cause inconsistencies 
shape_file <- shape_file %>%
  mutate(region = str_to_title(region)) %>% # title cases
  mutate(zone = str_to_title(zone)) %>%
  mutate(woreda = str_to_title(woreda)) %>%
  mutate(region= gsub("Ethiopia", "", region))%>% #removing Ethiopia from regional name
  mutate(region= str_trim(region, side = "both")) #removing trailing spaces from the regions

#saving the shapefile with updated regional classification
write.csv(shape_file, "data/processed/eth_shape_file_updated.csv", 
          row.names = F, quote = F)

unique(shape_file$region)

#checking the region names that are in the shape file but in the HMIS data
data.frame(regions= unique(mal_2017_2024$region)) %>%
  filter(!regions %in% shape_file$region) %>% #it results in region names that are in the HMIS data but in the shape file
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
  filter(!regions %in% shape_file$region) %>% 
  arrange(regions)  #inconsistencies were corrected


identical(sort(unique(mal_2017_2024$region)), sort(unique(shape_file$region))) #this tells me whether the regions in the sf
                                                                  #& HMIS are the same or not (TRUE/FALSE)


#checking if the code is repeatable
#identical(mal_2017_2024_saved, mal_2017_2024)

#saving the 2017-2024 dataframe into the repository
saveRDS(mal_2017_2024, file = "data/processed/region_reconciled.rds")

#-------------------------------End---------------------------------