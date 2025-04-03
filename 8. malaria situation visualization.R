rm(list = ls())
#this script aims to create basic visualizations to assess the malaria
#situation in Eth 

#libraries
library("tidyverse")
library("ggplot2")

#reading the hmis data
my_hmis <- readRDS("data/processed/Cleaned HMIS with reg pop.rds")

# 1. Reported cases (presumed vs confirmed)
#i-pie chart for presumed vs confirmed cases; national level
my_hmis |> 
  filter(mal_type %in% c("confirmed", "presumed")) |> 
  group_by(mal_type) |>  # Group only by mal_type to get total counts
  summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") |> 
  mutate(percentage = (total_cases / sum(total_cases)) * 100) |>  # Compute overall percentage
  ggplot(aes(x = "", y = total_cases, fill = mal_type)) +  
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "black") +
  scale_fill_manual(values = c("confirmed" = "skyblue",
                               "presumed" = "#DB778F")) +
  labs(title = "Malaria Cases: Confirmed vs Presumed",
       subtitle = "2017-2024",
       fill = "Malaria type") +
  theme_void()

#saving the plot
ggsave(filename = "outputs/malaria situation vis/presumed vs confirmed.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

# a. presumed vs confirmed cases by region
my_hmis |>
  filter(mal_type %in% c("presumed", "confirmed")) |>
  group_by(region, mal_type)|>
  summarise(total_cases = sum(value, na.rm = T), .groups = "drop") |>
  group_by(region) |>
  mutate(percentage= total_cases/sum(total_cases))|>
  ggplot(aes(x= region,
             y= total_cases, 
             fill = mal_type)) +
  geom_bar(stat = "identity",
           position = position_dodge2(width = 0.9, preserve = "single"))+
  geom_text(aes(label = scales::percent(percentage, accuracy = 1), group = mal_type),  
            position = position_dodge2(width = 0.9, preserve = "single"),  
            vjust = -0.5, size = 5)+
  scale_fill_manual(values = c("presumed" = "#09A39A",
                               "confirmed" = "#A053A1"))+
  labs(title = "presumed vs confirmed cases by region",
       X= "Region", 
       y= "Total cases",
       fill= "Malaria type")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~region, scales= "free")

my_hmis |>
  filter(mal_type %in% c("presumed", "confirmed")) |>
  group_by(region,zone, mal_type)|>
  summarise(total_cases = sum(value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x= reorder(zone,total_cases),
             y= total_cases, 
             fill = mal_type)) +
  geom_bar(stat = "identity",
           position =  "stack")+
  scale_fill_manual(values = c("presumed" = "#09A39A",
                               "confirmed" = "#A053A1"))+
  labs(title = "presumed vs confirmed cases by zone",
       x= "Zones", 
       y= "Total cases",
       fill= "Malaria type")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~region, scales= "free")+
  coord_flip()

#saving the plot
ggsave(filename = "outputs/malaria situation vis/presumed vs confirmed stack bar by zone.tiff", width = 13,
       height = 8, compression= "lzw", bg= "white")

# b. presumed vs confirmed cases by region- stacked bar chart
my_hmis |>
  filter(mal_type %in% c("presumed", "confirmed")) |>
  group_by(region, mal_type) |>
  summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(region) |>
  mutate(percentage = total_cases / sum(total_cases)) |>
  ggplot(aes(x = region, 
             y = percentage, 
             fill = mal_type)) +
  geom_bar(stat = "identity", 
           position = "fill") +  # Stacks bars to 100%
  geom_text(aes(label = scales::percent(percentage, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            color = "white", size = 5)+
  scale_fill_manual(values = c("presumed" = "#09A39A", 
                               "confirmed" = "#A053A1")) +
  scale_y_continuous(labels = scales::percent_format()) + # Convert y-axis to percentage
  labs(title = "Percentage of Presumed vs Confirmed Cases by Region",
       x = "Region", 
       y = "Percentage of Cases", 
       fill = "Malaria Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#saving the plot
ggsave(filename = "outputs/malaria situation vis/presumed vs confirmed stacked bar by region.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

#confirmed cases by HF type (clustered bar chart)
my_hmis |>
  filter(mal_type == "confirmed") |>
  group_by(region, zone, woreda, facility_type) |>
  summarise(total_cases= sum(value, na.rm = T), .groups = "drop")|>
  ggplot(aes(reorder(x=region, total_cases), y= total_cases, fill = facility_type))+
  geom_bar(stat= "identity", position = "dodge")+
  scale_fill_manual(values = c("Hospital"= "#04A777", "Health Center"= "skyblue",
                               "Health Post"= "#D90368"))+
  labs(title = "Confirmed malaria cases by region and facility type",
       subtitle = "2017-2024",
       x= "Region",
       y= "Total confirmed cases")+
  facet_wrap(~region, scale= "free")+
  theme_minimal()

#saving the plot
ggsave(filename = "outputs/malaria situation vis/confirmed cases by hf clustered bar.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

#confirmed cases by HF type (stacked bar chart)
my_hmis |>
  filter(mal_type == "confirmed", 
         facility_type %in% c("Hospital", "Health Center", "Health Post")) |>
  group_by(region, facility_type) |>
  summarise(total_cases= sum(value, na.rm = T), .groups = "drop")|>
  group_by(region)|>
  #mutate(total_region_cases = sum(total_cases)) |>  # Ensure the correct total
  #ungroup() |> 
  mutate(percentage = total_cases / sum(total_cases))|>
  ggplot(aes(x=region, y= percentage, fill = facility_type))+
  geom_bar(stat= "identity", position = "fill")+
  geom_text(aes(label = scales::percent(percentage, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            color = "black", size = 5)+
  scale_fill_manual(values = c("Hospital"= "tomato", "Health Center"= "skyblue",
                               "Health Post"= "yellow2"))+
  labs(title = "Confirmed malaria cases by region and facility type",
       subtitle = "2017-2024",
       x= "Region",
       y= "Total confirmed cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#saving the plot
ggsave(filename = "outputs/malaria situation vis/confirmed cases by hf stacked bar.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")
  
#2. Case Incidence rates
#national malaria incidence DOES NoT MAKE SENCE, DOWNFALLING INCIDENCE
my_hmis |>
  filter(mal_type== "confirmed")|>
  group_by(year) |>
  summarise(total_cases= sum(value, na.rm = T), 
            total_pop= sum(population, na.rm = T))|>
  ungroup()|>
  group_by(year)|>
  mutate(incidence_rate= (total_cases/total_pop)*1000)|>
  ggplot(aes(x= year, y= incidence_rate))+
  geom_line(linewidth= 1)
  
#PF Case incidence rate by region (time series)
my_hmis |>
  filter(data_type == "pf_conf") |>
  group_by(region, year, population) |>
  summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(region) |>
  mutate(incidence_rate = (total_cases / population) * 1000) |>
  ungroup() |>
  ggplot(aes(x = year, y = incidence_rate, color = region, group = region)) +  # ✅ Fix x-axis & group
  geom_line(linewidth = 1) + 
  geom_point() +  
  labs(title = "Incidence Rate Over Time by Region",
       x = "Year",
       y = "Incidence Rate (per 1,000)",
       color = "Region") +
  theme_minimal()

#saving the plot
ggsave(filename = "outputs/malaria situation vis/PF case incidence by region multi line.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

#time series of PF cases incidenbce rate faceted by region
my_hmis |>
  filter(data_type == "pf_conf") |>
  group_by(region, year, population) |>
  summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(region) |>
  mutate(incidence_rate = (total_cases / population) * 1000) |>
  ungroup() |>
  ggplot(aes(x = year, y = incidence_rate)) +  
  geom_line(linewidth = 1) + 
  geom_point() +  
  labs(title = "Incidence Rate Over Time by Region",
       x = "Year",
       y = "Incidence Rate (per 1,000)") +
  theme_minimal()+
  facet_wrap(~region, scale="free")

#saving the plot
ggsave(filename = "outputs/malaria situation vis/PF case incidence by region facet wrapped.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

# i. Outpatient cases per 1000


#3. Death incidence rate


#4. Percentage cases due to pf an pv
#clusterd bar chart to show national malaria dist by species each year
my_hmis |>
  filter(data_type %in% c("pf_conf", "pv_conf", "po_conf", "pm_conf",
                          "other_conf", "mixed")) |>
  group_by(year, data_type) |>
  summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") |>
  filter(total_cases > 0)|>
  group_by(year)|>
  mutate(percentage= (total_cases/sum(total_cases))*100, na.rm = T)|>
  ungroup()|>
  ggplot(aes(x = reorder(data_type, total_cases), y = total_cases)) +
  geom_bar(stat = "identity", width = 0.6, 
           position = position_dodge(width = 0.8),
           fill = "orange1") +  
  geom_text(aes(label = paste0(round(percentage, 0), "%"), 
                vjust = ifelse(data_type %in% c("pf_conf", "pv_conf"), 1.5, -0.5)),  
            position = position_dodge(width = 0.8), 
            size = 3, color = "black")+
  labs(title = "National Distribution of Malaria by Species",
       subtitle = "2017-2024",
       x = "Malaria Species",
       y = "Total Cases",
       fill = "Species") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  facet_wrap(~year)+
  coord_flip()

#saving the plot
ggsave(filename = "outputs/malaria situation vis/national malaria by species cluster bar.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

#pie chart to show national malaria cases by species each year
my_hmis |>
  filter(data_type %in% c("pf_conf", "pv_conf", "po_conf", "pm_conf", "other_conf", "mixed")) |>
  mutate(data_type = case_when(
    data_type %in% c("po_conf", "pm_conf", "other_conf") ~ "others",
    TRUE ~ data_type)) |>
  group_by(year, data_type) |>
  summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(percentage = (total_cases / sum(total_cases, na.rm = TRUE)) * 100) |>
  ungroup() |>
  ggplot(aes(x = "", y = percentage, fill = data_type)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Ensures full pie slices
  coord_polar(theta = "y", start = 0) + 
  geom_text(aes(label = ifelse(data_type %in% c("pf_conf", "pv_conf"), 
                               paste0(round(percentage, 0), "%"), "")), 
            position = position_stack(vjust = 0.5),  
            size = 3, color = "white") +  
  labs(title = "National Malaria Cases by Species", 
       subtitle = "2017-2024",
       fill = "Malaria Species") +
  scale_fill_manual(values = c("pf_conf" = "#0072B2",   
                               "pv_conf" = "orange",
                               "mixed" = "skyblue",
                               "others" ="grey"))+
  theme_void() +  
  facet_wrap(~year, scale= "free")

#saving the plot
ggsave(filename = "outputs/malaria situation vis/national malaria by species pie chart.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

#stacked bar chart to see pf_pv proportion by region and year
my_hmis |>
  filter(data_type %in% c("pf_conf", "pv_conf", "po_conf", 
                          "pm_conf", "other_conf", "mixed")) |>
  mutate(data_type = case_when(
    data_type %in% c("po_conf", "pm_conf", "other_conf") ~ "others",
    T ~ data_type))|>
  group_by(region, year, data_type) |>
  summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(region, year) |>
  mutate(percentage = (total_cases / sum(total_cases, na.rm = TRUE)) * 100) |>
  ungroup()|>
  ggplot(aes(x = factor(year), y = percentage, fill = data_type)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = ifelse(data_type %in% c("pf_conf", "pv_conf"), 
                               paste0(round(percentage, 0), "%"), "")), 
            position = position_stack(vjust = 0.5),  
            size = 3, color = "black")+
  labs(title = "Regional Malaria Cases by Species", 
       subtitle = "2017-2024",
       fill = "Malaria Species",
       x = "Year",
       y = "Percentage") +
  scale_fill_manual(values = c("pf_conf" = "#A6CEE3",   
                               "pv_conf" = "#FF7F0E",   
                               "others" = "#999999", 
                               "mixed" = "#1F77B4")) +   
  theme_minimal() + 
  facet_wrap(~region, scale= "free") 

#saving the plot
ggsave(filename = "outputs/malaria situation vis/regional malaria by species stacked.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

#malaria cases by species by region cluster bar chart 
my_hmis |>
  filter(mal_type== "confirmed")  |>
  group_by(region, data_type) |>
  summarise(total_cases = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(region) |>
  mutate(percentage = total_cases / sum(total_cases) * 100) |>
  ungroup() |>
  ggplot(aes(x = reorder(data_type, total_cases), y = total_cases)) +  
  geom_bar(stat = "identity", position = "dodge", fill="skyblue" ) +  
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            position = position_stack(vjust = 0.5),  
            size = 3, color = "black") +  
  #scale_fill_manual(values = c("pf_conf" = "yellow2", "pv_conf" = "#DB778F")) +  
  facet_wrap(~region, scale="free" ) +  
  labs(title = "Malaria Cases: Pf vs Pv by Region",
       subtitle = "2017-2024",
       x = "Year",
       y = "Percentage of Cases") +  
  theme_minimal() +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

#saving the plot
ggsave(filename = "outputs/malaria situation vis/regional malaria by species clustered bar.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")


# 5. testing coverage and positivity
# 5.1. testing rate # we dont have data source for 
                    #number of suspects in ethiopia to calculate testing rate

# 5.2. TPR (stratify by HF and CHW confirmed cases)
#cluster bar chart
my_hmis |>
  filter(data_type %in% c("tests", "positives")) |>
  group_by(region, facility_type, data_type) |>
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = data_type, values_from = total) |>
  mutate(TPR = (positives / tests) * 100) |>
  ungroup() |>
  ggplot(aes(x = region, y = TPR, fill = facility_type)) +
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.8)) +  
  geom_text(aes(label = paste0(round(TPR, 0), "%")),  
            position = position_dodge(width = 0.8),  
            vjust = 1, size = 3, color = "white") +  
  labs(
    title = "TPR by Facility Types Across the Regions",
    subtitle = "2017-2024",
    x = "Regions", y = "TPR (%)", fill = "Facility Types"
  ) +
  scale_fill_manual(values = c(
    "Health Post" = "#E69F00",
    "Health Center" = "#0072B2",
    "Hospital" = "#009E73"
  )) +
  theme_minimal() +
  facet_wrap(~region, scales = "free")

#saving the plot
ggsave(filename = "outputs/malaria situation vis/regional TPR by facility type clustered bar.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

# 7. ITN Distribution
# 7.1. HHs in ITN need vs HHs reveived ITN
my_hmis |>
  filter(data_type %in% c("hh_llin_need", "hh_llin_received")) |>
  group_by(region, year, data_type) |>
  summarise(totals = sum(value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x= reorder(region, totals), y= totals, fill= data_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
  labs(title = "ITN need vs access by region",
       subtitle = "2017 - 2024",
       x = "Years", y= "Total Number",
       fill = "") +
  theme_minimal()+
  facet_wrap(~year, scale= "free")+
  coord_flip()
  
#saving the plot
ggsave(filename = "outputs/malaria situation vis/regional ITN need vs access cluster bar.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")
  

# 7.2. LLIN Coverage rate by region
my_hmis |>
  filter(data_type %in% c("hh_llin_need", "hh_llin_received")) |>
  group_by(region, year, data_type) |>
  summarise(totals = sum(value, na.rm = T), .groups = "drop")|>
  pivot_wider(names_from = data_type, values_from = totals) |>
  mutate(llin_coverage_rate= (hh_llin_received/hh_llin_need)*100)|>
  ggplot(aes(x= year, y= llin_coverage_rate)) +
  geom_bar(stat= "identity", position = position_dodge(width = 0.8), 
           fill= "skyblue")+
  labs(title = "LLIN Coverage Rate by region",
       subtitle = "2017 - 2024",
       x= "Years",
       y= "Coverage Rate")+
  theme_minimal()+
  facet_wrap(~region)

#saving the plot
ggsave(filename = "outputs/malaria situation vis/regional TPR by facility type clustered bar.tiff", width = 10,
       height = 8, compression= "lzw", bg= "white")

# 8. Treatment and commodity availablity : no available data source

# 9. Malaria vaccine coverage (from admin report, no data currently)

# 10. Malaria surveilence system sensitivity cascade
# the question included under this part all dont have data source

# 11. Elimination based surveilence
# a. percentage confirmed cases fully investigated and classified

# b. Percentage of foci fully investigated and classified

# c. ABER

# 12. Maps
# a. confirmed malaria case incidence

# b. TPR

#Reporting rate






ggplot(dat_case_summary, aes(x = year, y = incidence, color = health_zone)) +
  geom_line() +
  ylim(c(0, NA)) +
  facet_wrap(vars(province), scale = "free_y") +
  theme(legend.position = "none")

my_hmi