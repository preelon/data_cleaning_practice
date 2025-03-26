rm(list=ls())

#library
library("tidyverse")

#read the 2017-2024 data for cleaning
mal_2017_2024 <-readRDS("data/processed/hmis 2017-2024 uncleaned.rds")

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

#reading the Ethiopia's shapefile and changing the region, zone and woreda to title case
  shape_file<- read_csv("data/raw/eth_admpop_adm3_model_input_pop22_1082.csv") %>%
  select(id_1082, region, zone, woreda, pop_2022= T_TL)


#standardizing the regions in the shape file based on the recent adminstrative division
#creating data frame for central and South regions
south <- c("Alle", "Amaro", "Basketo", "Burji", 
           "Derashe", "Gamo", "Gofa", "Gedeo", 
           "Konso", "South Omo", "Wolayita" )

central <- c("Guraghe", "Hadiya", "Halaba", "Kembata Tembaro", 
             "Siltie", "Yem Special")

#integrating south and central regions in the sf
shape_file <- shape_file %>%
  mutate(region= case_when(zone %in% south ~ "South",
                           zone %in% central ~ "Central",
                           TRUE ~ region))


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
saveRDS(mal_2017_2024, file = "data/processed/hmis_region_reconciled.rds")

#-------------------------------End---------------------------------