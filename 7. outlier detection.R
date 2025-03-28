#on this script I am to perform identification of outliers using box plots,
# line graphs and the pull function. I also aim to replace those outliers 
#that look suspicious with proper values

rm(list = ls())

#libraries
library("tidyverse")
library("lubridate")
library(RColorBrewer)

mycols = c(brewer.pal(11, "Spectral"))

#read the hmis data to work on
my_hmis <- readRDS("data/processed/hmis_data_type_assigned.rds")

#lets create vectors that specify large and small regions- will help to
#separately construct box plot for the large & small regions
large_regions <- c("Amhara", "Oromia")
small_regions <- c("Addis Ababa", "Tigray", "Afar", "Somali", 
                   "Benishangul Gumuz","Central", "South", "South West",
                   "Gambella", "Harari","Dire Dawa","Sidama")
# 1. REGIONAL LEVEL PLOTS
# i. distribution of pf cases across large regions boxplot
my_hmis |>
  filter(data_type== "pf_conf", region %in% large_regions) |>
  group_by(year, region) %>%
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  #Boxplot
  ggplot(aes(x = region, y = total_cases)) +
  geom_boxplot(fill="skyblue", outlier.colour = "red") +
  labs(title = "malaria distribtion by region",
       x="region",
       y="total cases")

#saving the plot
ggsave(filename = "outputs/outlier detection/distribution of pf cases in large region.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")


# ii. distribution of pf cases across small regions boxplot
my_hmis |>
  filter(data_type== "pf_conf", region %in% small_regions) |>
  group_by(year, region) %>%
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  #Boxplot
  ggplot(aes(x = region, y = total_cases)) +
  geom_boxplot(fill="skyblue", outlier.colour = "red") +
  labs(title = "malaria distribtion by region",
       x="region",
       y="total cases")+
  facet_wrap(~region, scale= "free") #Afar, AA and Harari regions showed outliers


#saving the plot
ggsave(filename = "outputs/outlier detection/distribution of pf cases in small region.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

# 2. ZONAL LEVEL PLOTS (smaller regions)
# i. box plot for the smaller regions and at zonal level
my_hmis |>
  filter(data_type== "pf_conf", region %in% small_regions) |>
  group_by(region,zone, woreda, year) %>%
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x =zone, y = total_cases)) +
  geom_boxplot(fill= "skyblue", outlier.colour = "red") +
  labs(title = "pf malaria distribtion by zone",
       subtitle = "In smaller regions",
       x="Zones",
       y="Total cases")+
  coord_flip()+
  facet_wrap(~region, scale= "free") #scale= "free" makes the y axis specific
                                     #to each regions

#saving the plot
ggsave(filename = "outputs/outlier detection/distribution of pf cases at zonal level in small region.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

# selecting 1 zone with out lier from each small regions
zone_outlier <- c("Region 14", "Gabi /Zone 3", "Metekel", "Kembata Tembaro",
                  "Dire Dawa Urban", "Agnewak", "Harari", "Sidama",
                  "Afder", "Wolayita", "Bench Sheko", "Western")

# 3. DISTRICT LEVEL PLOT (smaller regions)
# since there are over 1000 districts I'll choose some districts only from 
#the zones that showed outliers

sort(unique(my_hmis$zone))

# i- box plot for the selected zones with outl ier from small regions
my_hmis |>
  filter(zone %in% zone_outlier, data_type== "pf_conf") |>
  group_by(zone, woreda, year) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x= woreda, y= total_cases))+
  geom_boxplot(fill= "skyblue" ,outlier.colour = "red")+
  coord_flip()+
  labs(title = "pf cases at woreda level",
       subtitle = "from selected zones with outliers",
       x= "woreda", 
       y= "Total cases")+
  facet_wrap(~zone, scale= "free")+
  theme_minimal()

#saving the plot
ggsave(filename = "outputs/outlier detection/distribution of pf cases at woreda level in selected zones.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

#the districts for Sidama region are stuffed so lets plot the above 
#separate for Sidama region for better view
my_hmis |>
  filter(zone== "Sidama", data_type== "pf_conf") |>
  group_by(zone, woreda, year) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x= woreda, y= total_cases))+
  geom_boxplot(fill = "skyblue", outlier.colour = "red")+
  coord_flip()+
  labs(title = "pf cases at woreda level",
       subtitle = "from Sidama zone",
       x= "woreda", 
       y= "Total cases") #Bilate zuria woreda is the one with large out lier
  
#saving the plot
ggsave(filename = "outputs/outlier detection/distribution of pf cases at woreda level in sidama zone.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")


# now lets do the same for large regions
# 1. ZONAL LEVEL PLOT
# i. distribution of pf cases at zonal level across large regions box plot
my_hmis |>
  filter(data_type== "pf_conf", region %in% large_regions) |>
  group_by(year, region, zone) %>%
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x = zone, y = total_cases)) +
  geom_boxplot(fill="skyblue", outlier.colour = "red") +
  labs(title = "malaria distribtion by region",
       x="region",
       y="total cases")+
  facet_wrap(~region, scale= "free")+
  coord_flip()+
  theme_minimal()

#saving the plot
ggsave(filename = "outputs/outlier detection/zonal level pf malaria dstribution in larger regions.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

#lets select 2 zones with max outlier from the large regions and 
#create a vector
zone_outlier_LR <- c("Awi", "Wag Hamra", "West Wellega", "Jimma")

# ii- box plot of zones with oulier from the large regions
my_hmis |>
  filter(zone %in% zone_outlier_LR, data_type== "pf_conf") |>
  group_by(zone, woreda, period_start_date)|>
  summarise(total_cases = sum(agg_value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x= woreda, y= total_cases))+
  geom_boxplot(fill= "skyblue", outlier.colour = "red")+
  facet_wrap(~zone, scale= "free")+
  coord_flip()+
  theme_minimal()

#saving the plot
ggsave(filename = "outputs/outlier detection/woreda level pf malaria dstribution in larger regions.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")


#2. WOREDA LEVEL PLOT for all the regions

#selecting woredas with max outlier from the selected zones from 
#both the large & small regions (2 per region- large regions and 1 per 
#region from the small regions)
woreda_outliers <- c("Bilate Zuria", "Raso", "Shay Bench", "Dechatu",
                     "Awash", "Abadir", "Kacha Bira", "Dangur", "Arada", 
                     "Boloso Bombe","Ayehu Guwagusa", "Guangua", "Zequala", 
                     "Abergele (Am)", "Shebe Sambo", "Sekoru",
                     "Nejo", "Babo")

# time series analysis for the selected districts 
my_hmis |>
  filter(woreda %in% woreda_outliers, data_type== "pf_conf")|>
  group_by(woreda, period_start_date) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop")|>
  ggplot(aes(x= period_start_date,
             y= total_cases,
             group = woreda))+
  geom_line(linewidth = 0.6)+
  facet_wrap(~woreda, scale= "free")+
  theme_minimal()

#saving the plot
ggsave(filename = "outputs/outlier detection/time series analysis for selected woredas.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")


#selecting districts that looks suspicious on the time serious analysis and
#need further investigation
#1. Abadir Woreda
my_hmis |>
  filter(woreda== "Abadir", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 439 

x <- my_hmis |>
  filter(woreda== "Abadir", agg_value == 439) #Jinela Health Center in Harari
                                              #in 2024

#lets see the records of pf_conf cases from that HC in 2024
x= my_hmis |>
  filter(facility == "Jinela Health Center", 
         year== 2024, data_type== "pf_conf") #looking at the 9 observations
                       #it looks like 239 is mistaken for 439
                       #so I replaced 236 for 439 (avg of the values)

hmis_outlier_resolved <- my_hmis |>
  mutate(agg_value= case_when(woreda== "Abadir" & 
                              facility== "Jinela Health Center" & 
                                agg_value== 439 ~ 236,
                              TRUE ~ agg_value))

# 2. Abergele (Am)
my_hmis |>
  filter(woreda== "Abergele (Am)", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 1369 

x <- my_hmis |>
  filter(woreda== "Abergele (Am)" , agg_value == 1369) #Niwrake Health Center
                                                      # in 2023

#lets see the records of pf_conf cases from that HC in 2023
x= my_hmis |>
  filter(facility == "Niwrake Health Center", 
         year== 2023, data_type== "pf_conf") #looks acceptable so left as it is

# 3. Arada
my_hmis |>
  filter(woreda== "Arada", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 22 

x <- my_hmis |>
  filter(woreda== "Arada" , agg_value == 22, 
         data_type== "pf_conf") #Ras Desta, 2024

x= my_hmis |>
  filter(facility == "Ras Desta Damitew General Hospital", 
         year== 2024, data_type== "pf_conf") #looks acceptable so left as 
                                            #it is

# 4. Ayehu Guwagusa
my_hmis |>
  filter(woreda== "Ayehu Guwagusa", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 2143 

x <- my_hmis |>
  filter(woreda== "Ayehu Guwagusa" , 
         agg_value == 2143, 
         data_type== "pf_conf") #Ayo HC, 2024

x= my_hmis |>
  filter(facility == "Ayo Health Center", 
         year== 2024, data_type== "pf_conf") #since thae peak is only in a 
                       #single month, looks suspicious- replaced with avg

#replacing the 2143 value in Ayo HC in Ayehu Guwagusa by avg of promimal period
hmis_outlier_resolved <- hmis_outlier_resolved |>
  mutate(agg_value= case_when(woreda== "Ayehu Guwagusa" & 
                                facility== "Ayo Health Center" & 
                                agg_value== 2143 ~ 1310,
                              TRUE ~ agg_value))

# 5. Babo
my_hmis |>
  filter(woreda== "Babo", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 5876 

x <- my_hmis |>
  filter(woreda== "Babo" , 
         agg_value == 5876, 
         data_type== "pf_conf") #Amballo Dila HC, 2024

x= my_hmis |>
  filter(facility == "Amballo Dila Health Center", 
         year == 2024, data_type== "pf_conf") #it is a progressive increase 
                                              #seems acceptable

# 6. Bilate Zuria
my_hmis |>
  filter(woreda== "Bilate Zuria", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 924 

x <- my_hmis |>
  filter(woreda== "Bilate Zuria" , 
         agg_value == 924, 
         data_type== "pf_conf") #Amballo Dila HC, 2024

x= my_hmis |>
  filter(facility == "Balela  Health Center", 
         year == 2024, data_type== "pf_conf") #looks acceptable

# 7. Boloso Bombe
my_hmis |>
  filter(woreda== "Boloso Bombe", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 1787

x <- my_hmis |>
  filter(woreda== "Boloso Bombe" , 
         agg_value == 1787, 
         data_type== "pf_conf") #Bombe Primary Hospital, 2024

x= my_hmis |>
  filter(facility == "Bombe Primary Hospital", 
         year == 2024, data_type== "pf_conf") #looks acceptable

# 8. Dechatu
my_hmis |>
  filter(woreda== "Dechatu", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 95

x <- my_hmis |>
  filter(woreda== "Dechatu" , 
         agg_value == 95, 
         data_type== "pf_conf") #Dilchora  General Hospital, 2024

x= my_hmis |>
  filter(facility == "Dilchora  General Hospital", 
         year == 2022, data_type== "pf_conf") #changed to NA as it looked suspicious 

#replacing the 95 value in Dilchora  General Hospital by NA
hmis_outlier_resolved <- hmis_outlier_resolved |>
  mutate(agg_value= case_when(woreda== "Dechatu" & 
                                facility== "Dilchora  General Hospital" & 
                                agg_value== 95 ~ NA,
                              TRUE ~ agg_value))

# 9. Guangua
my_hmis |>
  filter(woreda== "Guangua", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 1312

x <- my_hmis |>
  filter(woreda== "Guangua" , 
         agg_value == 1312, 
         data_type== "pf_conf") #Yemalie Health Center, 2024

x= my_hmis |>
  filter(facility == "Yemalie Health Center", 
         year == 2024, data_type== "pf_conf") #looks acceptable

# 10. Kacha Bira
my_hmis |>
  filter(woreda== "Kacha Bira", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 1339

x <- my_hmis |>
  filter(woreda== "Kacha Bira" , 
         agg_value == 1339, 
         data_type== "pf_conf") #Yemalie Health Center, 2024

x= my_hmis |>
  filter(facility == "Walana Health Post", 
         year == 2024, data_type== "pf_conf") #looks suspicious, replace with avg


hmis_outlier_resolved <- hmis_outlier_resolved |>
  mutate(agg_value= case_when(woreda== "Kacha Bira" & 
                                facility== "Walana Health Post" & 
                                agg_value== 1339 ~ 371,
                              TRUE ~ agg_value))

#10. Nejo
my_hmis |>
  filter(woreda== "Nejo", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 2037

x <- my_hmis |>
  filter(woreda== "Nejo" , 
         agg_value == 2037, 
         data_type== "pf_conf") #Yemalie Health Center, 2024

x= my_hmis |>
  filter(facility == "Amuma Gute Health Center", 
         year == 2024, data_type== "pf_conf") #replace with avg value

#replacing the 2037 value with avg
hmis_outlier_resolved <- hmis_outlier_resolved |>
  mutate(agg_value= case_when(facility== "Amuma Gute Health Center" & 
                                woreda== "Walana Health Post" & 
                                agg_value== 1339 ~ 330,
                              TRUE ~ agg_value))

#10. Sekoru
my_hmis |>
  filter(woreda== "Sekoru", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 1946

x <- my_hmis |>
  filter(woreda== "Sekoru" , 
         agg_value == 1946, 
         data_type== "pf_conf") #Yemalie Health Center, 2024

x= my_hmis |>
  filter(facility == "Kumbi Health Center", 
         year == 2024, data_type== "pf_conf") #seems a progressive increase

#10. Sekoru
my_hmis |>
  filter(woreda== "Sekoru", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 1946

x <- my_hmis |>
  filter(woreda== "Sekoru" , 
         agg_value == 1946, 
         data_type== "pf_conf") #Yemalie Health Center, 2024

x= my_hmis |>
  filter(facility == "Kumbi Health Center", 
         year == 2024, data_type== "pf_conf") #seems acceptable

#10. Shay Bench
my_hmis |>
  filter(woreda== "Shay Bench", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 615

x <- my_hmis |>
  filter(woreda== "Shay Bench" , 
         agg_value == 615, 
         data_type== "pf_conf") #Yemalie Health Center, 2024

x= my_hmis |>
  filter(facility == "Shonga Dosha Health Post", 
         year == 2024, data_type== "pf_conf") #looks suspicious-remove


#removing the 615 value 
hmis_outlier_resolved <- hmis_outlier_resolved |>
  mutate(agg_value= case_when(facility== "Shonga Dosha Health Post" & 
                                woreda== "Shay Bench" & 
                                agg_value== 615 ~ NA,
                              TRUE ~ agg_value))

#10. Shebe Sambo
my_hmis |>
  filter(woreda== "Shebe Sambo", data_type== "pf_conf") |>
  pull(agg_value) |>
  summary() #max value= 4166

x <- my_hmis |>
  filter(woreda== "Shebe Sambo" , 
         agg_value == 4166, 
         data_type== "pf_conf") #Yemalie Health Center, 2024

x= my_hmis |>
  filter(facility == "Kishe Health Center", 
         year == 2024, data_type== "pf_conf") #looks a case up surge- leave it
----------------------------------------------------------------------------

#below is another way of identifying outliers
#lets see the summary of the aggregated value so that I can identify those
#with higher values
my_hmis |>
  filter(data_type=="pf_conf") |>
  pull(agg_value) |>
  summary() #i see the max value is 6172

x <-my_hmis |>
  filter(agg_value== 6172, data_type== "pf_conf") #Gunfi Health Center in
#Begi woreda, west wellega

#lets make time series for that specific health center
my_hmis |>
  filter(facility== "Gunfi Health Center", data_type=="pf_conf") |>
  ggplot(aes(x= period_start_date, y= agg_value)) +
  geom_line(linewidth=1) #it shows a sudden spike and drop w/h is suspecios

#lets extract the mean for the values other than the outliers to replace
#the outliers with this mean value
my_hmis |>
  filter(facility== "Gunfi Health Center" & 
           woreda== "Begi" & 
           agg_value < 6172 &
           data_type== "pf_conf") |>
  group_by(period_start_date, agg_value, data_type)|>
  summarise(mean_val= mean(agg_value), .groups = "drop")|> #317
  mutate(agg_value= case_when(agg_value >= 6172 ~ 317,
                              TRUE ~ agg_value)) |>
  ggplot(aes(x= period_start_date, y= agg_value)) +
  geom_line(linewidth=1)

#lets cntinue the same process for the other hfs
my_hmis |>
  filter(data_type == "pf_conf")|>
  pull(agg_value)|>
  summary()


#lets create a vector for checked hfs 
hf_checked <- c("Gunfi Health Center")

my_hmis |>
  filter(data_type=="pf_conf", !facility %in% hf_checked ) |>
  pull(agg_value) |>
  summary() #max val= 5876


x <-my_hmis |>
  filter(agg_value== 5876, data_type== "pf_conf") # Amballo Dila Health Center

my_hmis |>
  filter(facility== "Amballo Dila Health Center", data_type=="pf_conf") |>
  ggplot(aes(x= period_start_date, y= agg_value)) +
  geom_line()

#update the hf_checked vector by adding Amballo HC
hf_checked <- c("Gunfi Health Center", "Amballo Dila Health Center")

my_hmis |>
  filter(data_type=="pf_conf", !facility%in% hf_checked) |>
  pull(agg_value) |>
  summary() #4950

x <-my_hmis |>
  filter(agg_value== 4590, data_type== "pf_conf") #Genji Health Center

#update the hf_checked vector by adding Genji HC 
hf_checked <- c("Gunfi Health Center", "Amballo Dila Health Center",
"Genji Health Center")

my_hmis |>
  filter(facility== "Genji Health Center", data_type=="pf_conf") |>
  ggplot(aes(x= period_start_date, y= agg_value)) +
  geom_line()

#lets do the detection for large regions
my_hmis |>
  filter(region %in% large_regions, data_type== "pf_conf") |>
  group_by(region, zone, woreda, period_start_date) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop")|>
  ggplot(aes(x= zone,
             y= total_cases))+
  facet_wrap(~region, scales = "free")+
  geom_boxplot(fill= "green", outlier.size = 1, outlier.color = "red")+
  theme(panel.grid.major= element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_flip()

#select zones from large regions for further check
large_zones_to_investigate <- c("South Gondar", "West Gondar",
                                "Central Gondar", "Buno Bedele", 
                                "Ilu Aba Bora", "West Wellega")

my_hmis |>
  filter(zone %in% large_zones_to_investigate, data_type== "pf_conf") |>
  group_by(zone, woreda, period_start_date) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop")|>
  ggplot(aes(x= woreda,
             y= total_cases))+
  facet_wrap(~zone, scales = "free")+
  geom_boxplot(fill= "green", outlier.size = 0.7, outlier.color = "red")+
  theme(panel.grid.major= element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_flip()

#select woredas from large regions for further check
large_wor_furher <- c("Chwaka","Darimu", "Nejo", "Babo", "Dera (Am)",
                      "East Dembia", "Quara")

#constructing a time series for the woredas selected for further investigation
my_hmis |>
  filter(woreda %in% large_wor_furher, data_type== "pf_conf") |>
  group_by(woreda, period_start_date) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop")|>
  ggplot(aes(x= period_start_date,
             y= total_cases,
             group = woreda))+
  facet_wrap(~woreda, scales = "free", ncol=1)+
  geom_line(linewidth= 0.5)+  #group= woreda rather than facility
  theme_minimal()+
  labs(x= "Period",
       y= "Total cases")




ggsave(filename = "outputs/time series for selected woredas.tiff", 
       width = 10, height = 8, compression = "lzw", bg="white")

#facility level plot from selected woredas
my_hmis|>
  filter(woreda %in% large_wor_furher, data_type== "pf_conf") |>
  group_by(woreda, facility, period_start_date) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x= period_start_date,
             y= total_cases,
             group = woreda))+
  geom_line(linewidth= 0.5)
  facet_wrap(~ woreda, ncol=1)+
  theme_minimal()
