rm(list = ls())
#In this script I'll be practicing some data visualizations on the cleaned HMIS data
#load packages
library(tidyverse)
library(ggplot2)
#reading the hmis file
my_hmis <- readRDS("data/processed/hmis_woreda_reconciled.rds")

#1. lets explore the hmis data
head(my_hmis)
summary(my_hmis) #woreda_1082 column should be my woreda and zone_1082 aswell
                 #so lets change woreda_1082 and zone_1082 to woreda and zone
                 #additionally the category option combo and the attribute option
                 #combo names should be separated in different columns
glimpse(my_hmis)
summary(my_hmis)

# i. lets see if there are NA values in the dataset
colSums(is.na(my_hmis))

#2. data cleaning
#i. deal with missing values
#while checked above there is no missing value in the col we don't expect a missing value

#ii. convert col to correct data type
my_hmis <- my_hmis%>%
  mutate(period_start_date = as.Date(period_start_date)) %>%
  mutate(year= year(period_start_date))

#iii. standardization
my_hmis <- my_hmis %>%
  mutate(woreda= woreda_1082) %>%
  mutate(zone= zone_1082) %>%
  select(-woreda_1082, -zone_1082,-sn, -pop_2022) 
  #separate(category_option_combo_name,
   #        into = c("sex", "age_group"),
    #       sep = ", ",
     #      remove = F) %>%
  #separate(attribute_option_combo_name,
   #        into = c("dep't", "outcome"),
    #       sep = ", ",
     #      remove = F)

#lets save the data elements and see to catagorize them accordingly
data_elements <- table(my_hmis$data_element_name)
#write.csv(data_elements, "data/temp.csv") #commented out after implemented once

sapply(my_hmis, class) #shows the class for each variables

#lets create vectors for the respective data elements
pf_conf <- c("B50-196 Malaria (Plasmodium falciparum malaria)",
             "B50-197 Malaria (Plasmodium falciparum malaria with cerebral complications)",
             "B50-198 Malaria (Other severe and complicated Plasmodium falciparum malaria)",
             "B50-199 Malaria (Plasmodium falciparum malaria unspecified)",
             "ESV-ICD11 1F40 - Malaria due to Plasmodium falciparum") 

pv_conf <- c("B50-200 Malaria (Plasmodium vivax malaria)",
             "B50-201 Malaria (Plasmodium vivax malaria with other complications)",
             "B50-202 Malaria (Plasmodium vivax malaria without complication)",
             "ESV-ICD11 1F41 - Malaria due to Plasmodium vivax")

clinical<- c("B50-204 Malaria (Unspecified malaria)",
             "ESV-ICD11 1F45 - Malaria without parasitological confirmation")

tests <- c("-2017_Total number of slides or RDT performed for malaria diagnosis","MAL_Slides or RDT performed for malaria diagnosis",
           "MAL_Total number of slides or RDT performed for malaria diagnosis")

positives <- c("MAL_Slides or RDT Positive",
               "MAL_Total number of slides or RDT Positive")

pm_conf <- c("ESV-ICD11 1F42 - Malaria due to Plasmodium malariae")

po_conf <- c("ESV-ICD11 1F43 - Malaria due to Plasmodium ovale")

other_conf <- c("ESV-ICD11 1F44 - Other parasitologically confirmed malaria")

mixed_conf <- c("B50-203 Mixed Malaria (Other parasitologically confirmed malaria)")

hh_llin_need <- c("Number of HHs that need LLINs in the last 12 months")

hh_llin_received <- c("Number of targeted HHs received at least one LLINs in the last 12 months")

#lets change data elements in the same vector to one element  
my_hmis <- my_hmis %>% 
  mutate(data_type= NA) %>%
  mutate(data_type= case_when(data_element_name %in% pf_conf ~ "pf_conf",
                              data_element_name %in% pv_conf ~ "pv_conf",
                              data_element_name %in% clinical ~ "clinical",
                              data_element_name %in% tests ~ "tests",
                              data_element_name %in% positives ~ "positives",
                              data_element_name %in% pm_conf ~ "pm_conf",
                              data_element_name %in% po_conf ~ "po_conf",
                              data_element_name %in% other_conf ~ "other_conf",
                              data_element_name %in% mixed_conf ~ "mixed",
                              data_element_name %in% hh_llin_need ~ "hh_llin_need",
                              data_element_name %in% hh_llin_received  ~ "hh_llin_received",
                              TRUE ~ data_element_name)) 

#lets see the frequency table for each data types
table(my_hmis$data_type, useNA = "always")

#lets further standardize the data elements by creating a new col to specify
#confirmed and presumed cases
confirmed <- c("pf_conf", "pv_conf", "po_conf", 
               "pm_conf", "other_conf", "mixed")

my_hmis <- my_hmis %>%
  mutate(mal_type= case_when(data_type %in% confirmed ~ "confirmed",
                             data_type== "clinical" ~ "presumed",
                             T ~ NA))

table(my_hmis$mal_type, useNA = "always")

#assigning facility type to facilities
my_hmis <- my_hmis %>%
  mutate(facility_type = case_when(
    grepl(" Hospital|University Specialized", facility, ignore.case = TRUE) ~ "Hospital",
    grepl(" Health Center|HC|health centre|health centeer|Health  Center|Health Henter|
          Health Cente|12health Center| Center" , facility, ignore.case = TRUE) ~ "Health Center",
    grepl(" Health Post|HP|Health  post|Healrh Post|Helth Post|Heath Post|01Health Post|Heaalth Post|
    Haelth Post|Lijomy Healh Post|Helath Post|Haelth Post|Heakth Post|Healt Post|
           Post|H/Post|Healh Post||Health Post| Post|Healthe Post|Health Popst|Health Pos|
          Helthe Poset|heathe post|Health Pos|Heallth Post|_Health Post|_Health Pos", facility, ignore.case = TRUE) ~ "Health Post",
    grepl(" Clinic|MediumClinic|Speciality Center|Surgical Center|Speciality|Home Based Service|
          Medium| M/Clinic|H.clinic|Mediu|Clinc|Mulu Primary|Mch Center", facility, ignore.case = TRUE) ~ "PPM",
    TRUE ~ "Other"))

table(my_hmis$facility_type, useNA = "always")


#grouping the hmis and forming aggregation for the value before saving
my_hmis_out <- my_hmis |>
  group_by(id_1082, region, zone, woreda, facility, period_start_date, 
           year, data_type, mal_type,facility_type, 
           category_option_combo_name, attribute_option_combo_name) |>
  summarise(value= sum(value, na.rm= T), .groups = "drop") 

#save this version of the cleaned hmis
saveRDS(my_hmis_out, "data/processed/hmis_data_type_assigned.rds")
