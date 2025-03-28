rm(list = ls())

#In this script I'll be practicing some data visualizations on the cleaned HMIS data
#load packages
library(tidyverse)
library(ggplot2)

#reading the hmis file
my_hmis <- readRDS("data/processed/hmis_data_type_assigned.rds")

#reading population data
pop_data <- read.csv("data/processed/pop data from UN.csv")

#reading the shape file
shape_file <- read.csv("data/processed/eth_shape_file_updated.csv")


#lets change the pop_data to onger form & standardize the cols
pop_data_long <- pop_data |>
  pivot_longer(cols = c("X2017","X2018", "X2019", "X2020", "X2021",
                        "X2022", "X2023", "X2024"),
               names_to = "year",
               values_to = "pop") 

#standardizing the long pop data
pop_data_long <- pop_data_long |>
  select(-r) |>
  mutate(year= gsub("^X", "", year)) |>
  mutate(year= as.numeric(year)) |>#change into numeric format
  rename(region= Area) |>
  mutate(pop= pop*1000)
  

#create a data frame of onfirmed malaria cases by region and 
#add population data to it
unique(my_hmis$data_type)

regional_hmis <- my_hmis |>
  filter(data_type %in% c("pf_conf", "pv_conf",
                          "tests", "positives", "mixed", 
                          "other_conf", "pm_conf",
                          "po_conf")) |>
  group_by(region, year, data_type) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop")

#lets join the regional_hmis with pop_data to get the respective year pop
regional_hmis_pop <- regional_hmis |>
  left_join(pop_data_long, by= c("year", "region"))

#lets check if there are unacceptable NA values
x= regional_hmis_pop |>
  filter(is.na(pop)) #the new region's pop from 2017-2022 is NA b/c not divided
                     #before the 2023. also SNNPR in 2023 and 2024

#line graph of confirmed pf cases incidence rate by region
regional_hmis_pop |>
  filter(data_type== "pf_conf") |>
  group_by(year, region, total_cases, pop) |>
  summarise(incidence_rate= (total_cases/pop)*1000, .groups = "drop") |>
  ggplot(aes(x= year, y= incidence_rate))+
  geom_line(linewidth= 1, color= "blue")+
  theme_classic()+
  labs(title = "pf_confirmed incidence trend by region",
       x= "incidence rate",
       y= "year")+
  facet_wrap(~region)
  
#------------------------------------------------------------------------------
# lets project population number for the years 2017-2024 based on 
# 2022 pop number given
shape_file <- shape_file |>
  mutate(pop_2022_ref = pop_2022) |>
  mutate(pop_2021= pop_2022*(1.0266^-1)) |>
  mutate(pop_2020= pop_2022*(1.0266^-2)) |>
  mutate(pop_2019= pop_2022*(1.0266^-3)) |>
  mutate(pop_2018= pop_2022*(1.0266^-4)) |>
  mutate(pop_2017= pop_2022*(1.0266^-5)) |>
  mutate(pop_2023= pop_2022*(1.0266^1)) |>
  mutate(pop_2024= pop_2022*(1.0266^2)) |>
  select(id_1082, region, zone, woreda, pop_2022_ref, pop_2017, pop_2018, pop_2019,
         pop_2020, pop_2021, pop_2022, pop_2023, pop_2024)

#lets aggregate the pop number at region level
pop_data_sf <- shape_file |>
  group_by(region) |>
  summarise(pop_2017_agg = sum(pop_2017, na.rm = T),
            pop_2018_agg = sum(pop_2018, na.rm = T),
            pop_2019_agg = sum(pop_2019, na.rm = T),
            pop_2020_agg = sum(pop_2020, na.rm = T),
            pop_2021_agg = sum(pop_2021, na.rm = T),
            pop_2022_agg = sum(pop_2022, na.rm = T),
            pop_2023_agg = sum(pop_2023, na.rm = T),
            pop_2024_agg = sum(pop_2024, na.rm = T), .groups = "drop")

#joining the hmis and sf
hmis_pop_included <- my_hmis |>
  left_join(shape_file, by= c("id_1082", "region", "zone", "woreda"))

#lets check to ensure the joining was perfect fit
hmis_pop_included |>
  filter(is.na(pop_2017)) 


