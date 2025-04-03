rm(list = ls())

#In this script I'll be practicing some data visualizations on the cleaned HMIS data
#load packages
library(tidyverse)
library(ggplot2)

#reading the hmis file
my_hmis <- readRDS("data/processed/hmis_data_type_assigned.rds")

#reading the shape file
shape_file <- read.csv("data/processed/eth_shape_file_updated.csv")

# lets project population number for the years 2017-2024 based on 
# 2022 pop number in the sf
shape_file <- shape_file |>
  mutate(pop_2021= pop_2022*(1.0266^-1)) |>
  mutate(pop_2020= pop_2021*(1.0266^-1)) |>
  mutate(pop_2019= pop_2020*(1.0266^-1)) |>
  mutate(pop_2018= pop_2019*(1.0266^-1)) |>
  mutate(pop_2017= pop_2018*(1.0266^-1)) |>
  mutate(pop_2023= pop_2022*(1.0266^1)) |>
  mutate(pop_2024= pop_2023*(1.0266^1)) |>
  select(id_1082, region, zone, woreda, pop_2017, pop_2018, pop_2019,
         pop_2020, pop_2021, pop_2022, pop_2023, pop_2024)

#lets aggregate the pop number at region level
pop_data_regional <- shape_file |>
  group_by(region) |>
  summarise(pop_2017_agg = sum(pop_2017, na.rm = T),
            pop_2018_agg = sum(pop_2018, na.rm = T),
            pop_2019_agg = sum(pop_2019, na.rm = T),
            pop_2020_agg = sum(pop_2020, na.rm = T),
            pop_2021_agg = sum(pop_2021, na.rm = T),
            pop_2022_agg = sum(pop_2022, na.rm = T),
            pop_2023_agg = sum(pop_2023, na.rm = T),
            pop_2024_agg = sum(pop_2024, na.rm = T), .groups = "drop")

#lets change the pop regional data to longer format
pop_data_regional_long <- pop_data_regional |>
  pivot_longer(cols = c("pop_2017_agg", "pop_2018_agg", "pop_2019_agg",
                        "pop_2020_agg", "pop_2021_agg", "pop_2022_agg",
                        "pop_2023_agg", "pop_2024_agg"), 
               names_to = "years", values_to = "population") 

#remove text from the year column in the long data
pop_data_regional_long <- pop_data_regional_long |>  
mutate(years= case_when(grepl("2017", years) ~ "2017",
                        grepl("2018", years) ~ "2018",
                        grepl("2019", years) ~ "2019",
                        grepl("2020", years) ~ "2020",
                        grepl("2021", years) ~ "2021",
                        grepl("2022", years) ~ "2022",
                        grepl("2023", years) ~ "2023",
                        grepl("2024", years) ~ "2024",
       T ~ years)) |>
  mutate(years = as.numeric(years))

#lets left join the long regional pop data to the HMIS
hmis_regional_pop_joined <- my_hmis |>
  left_join(pop_data_regional_long, by= c("region", "year"= "years"))

#saving the cleaned HMIS with pop data at woreda level
saveRDS(hmis_regional_pop_joined, "data/processed/Cleaned HMIS with reg pop.rds")

#______________________________END______________________________________

#the velow script in between the broken lines is lefe as there was some issues
# and replaced with the above regional level pop
#--------------------------------------------------------------------------
#joining the hmis and sf
hmis_pop_included <- my_hmis |>
  left_join(shape_file, by= c("id_1082", "region", "zone", "woreda"))

#lets check to ensure the joining was perfect fit
hmis_pop_included |>
  filter(is.na(pop_2017)) #0 unmatched

#saving the cleaned HMIS with pop data at woreda level
saveRDS(hmis_pop_included, "data/processed/cleaned hmis with pop data.rds")

#----------------------------------END-----------------------------------------
