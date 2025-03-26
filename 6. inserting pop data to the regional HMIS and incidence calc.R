rm(list = ls())

#In this script I'll be practicing some data visualizations on the cleaned HMIS data
#load packages
library(tidyverse)
library(ggplot2)

#reading the hmis file
my_hmis <- readRDS("data/processed/hmis_data_type_assigned.rds")

#reading population data
pop_data <- read.csv("data/processed/pop data from UN.csv")

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
  


