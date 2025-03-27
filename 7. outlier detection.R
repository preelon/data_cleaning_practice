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
  geom_boxplot(fill="lightgreen", outlier.colour = "red") +
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
  geom_boxplot(fill="lightgreen", outlier.colour = "red") +
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
  geom_boxplot(fill= "lightgreen", outlier.colour = "red") +
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

# 3. DISTRICT LEVEL PLOT (smaller regions)
# since there are over 1000 districts I'll choose some districts only from 
#the zones that showed outliers

sort(unique(my_hmis$zone))

# selecting 1 zone with outlier from each small regions
zone_outlier <- c("Region 14", "Gabi /Zone 3", "Metekel", "Kembata Tembaro",
                       "Dire Dawa Urban", "Agnewak", "Harari", "Sidama",
                       "Afder", "Wolayita", "Bench Sheko", "Western")

# i- box plot for the elected zones with outlier from small regions
my_hmis |>
  filter(zone %in% zone_outlier, data_type== "pf_conf") |>
  group_by(zone, woreda, year) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x= woreda, y= total_cases))+
  geom_boxplot(fill = "green", outlier.colour = "red")+
  coord_flip()+
  labs(title = "pf cases at woreda level",
       subtitle = "from selected zones with outliers",
       x= "woreda", 
       y= "Total cases")+
  facet_wrap(~zone, scale= "free")

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
  geom_boxplot(fill = "green", outlier.colour = "red")+
  coord_flip()+
  labs(title = "pf cases at woreda level",
       subtitle = "from Sidama zone",
       x= "woreda", 
       y= "Total cases") #Bilate zuria woreda is the one with large out lier
  
#saving the plot
ggsave(filename = "outputs/outlier detection/distribution of pf cases at woreda level in sidama zone.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

#selecting 1 woreda with max outlier from each zone 
woreda_outliers <- c("Bilate Zuria", "Raso", "Shay Bench", "Dechatu",
                     "Awash", "Abadir", "Kacha Bira", "Dangur", "Arada", 
                     "Boloso Bombe")

# time series plotting for the selected districts from small regions
my_hmis |>
  filter(woreda %in% woreda_outliers, data_type== "pf_conf")|>
  group_by(woreda, period_start_date) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop")|>
  ggplot(aes(x= period_start_date,
             y= total_cases,
             group = woreda))+
  geom_line(linewidth = 0.7)+
  facet_wrap(~woreda, scale= "free")+
  theme_minimal()
  
#saving the plot
ggsave(filename = "outputs/outlier detection/time series for selected woredas from small regions.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

# now lets do the same for large regions
# 1. ZONAL LEVEL PLOT
# i. distribution of pf cases at zonal level across large regions box plot
my_hmis |>
  filter(data_type== "pf_conf", region %in% large_regions) |>
  group_by(year, region, zone) %>%
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x = zone, y = total_cases)) +
  geom_boxplot(fill="lightgreen", outlier.colour = "red") +
  labs(title = "malaria distribtion by region",
       x="region",
       y="total cases")+
  facet_wrap(~region, scale= "free")+
  coord_flip()

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
  geom_boxplot(fill= "green", outlier.colour = "red")+
  facet_wrap(~zone, scale= "free")+
  coord_flip()

#saving the plot
ggsave(filename = "outputs/outlier detection/woreda level pf malaria dstribution in larger regions.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

#2. WOREDA LEVEL PLOT
#selecting 2 woredas with max outlier from selected zones in large regions
#and creating a vector
woreda_outliers_LR <- c("Ayehu Guwagusa", "Guangua", "Zequala", 
                        "Abergele (Am)", "Shebe Sambo", "Sekoru",
                        "Nejo", "Babo")

# i- time series for selected woredas
my_hmis |>
  filter(woreda %in% woreda_outliers_LR, data_type == "pf_conf")|>
  group_by(woreda, period_start_date) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop")|>
  ggplot(aes(x= period_start_date,
             y= total_cases,
             group = woreda)) +
  geom_line(linewidth= 0.8)+
  facet_wrap(~woreda)+
  theme_minimal()

#saving the plot
ggsave(filename = "outputs/outlier detection/time series for selected woredas from large regions.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white") 
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
