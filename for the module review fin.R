
#libraries
library("tidyverse")
library("lubridate")

#ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) +  <GEOM_FUNCTION>()

#read the hmis data to work on
my_hmis <- readRDS("data/processed/hmis_data_type_assigned.rds")

confirmed <- c("pf_conf", "pv_conf", "po_conf", "pm_conf", "other_conf", "mixed")

my_hmis <- my_hmis |>
  mutate(mal_type= case_when(data_type %in% confirmed ~ "confirmed",
                             data_type== "clinical" ~ "presumed",
                             TRUE ~ NA))
#outlier detection
my_hmis |>
  filter(data_type== "pf_conf") |>
  group_by(year, region) %>%
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  #Outlier Detection (Boxplot)
  ggplot(aes(x = region, y = total_cases)) +
  geom_boxplot(fill="lightgreen", outlier.colour = "red") +
  labs(title = "malaria distribtion by region",
       x="region",
       y="total cases")+
  coord_flip()
  

glimpse(my_hmis)
#malaria cases by region bar graph
my_hmis |>
  filter(data_type== "pf_conf") |>
  group_by(region, zone, woreda, year, data_type,) |>
  summarise(total_cases = sum(agg_value, na.rm = TRUE), .groups = "drop") |>
ggplot(aes(reorder(x= region, total_cases), 
           y= total_cases)) +
  geom_bar(stat = "identity", fill= "tomato")+
  coord_flip()+
  #facet_wrap(~year)+
  labs(title = "malaria cases by region",
       subtitle = "2017-2024",
       x= "regions",
       y= "total cases")

#pie chart for presumed vs confirmed cases
my_hmis |>
  filter(mal_type %in% c("confirmed", "presumed")) |>
  group_by(region, year, mal_type) |>
  summarise(total_cases = sum(agg_value, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = "", y = total_cases, fill = mal_type)) +  
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  
  scale_fill_manual(values = c("confirmed" = "orange", "presumed" = "purple")) +
  labs(title = "Malaria Cases: Confirmed vs Presumed") +
  theme_void()

#pf vs pv pie chart
my_hmis |>
  filter(data_type %in% c("pf_conf", "pv_conf")) |>
  group_by(region, year, data_type) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x="", y=total_cases, fill = data_type))+
  geom_bar(stat = "identity", width = 1, show.legend = T) +
  coord_polar(theta = "y")+
  scale_fill_manual(values = c(pf_conf= "violet", 
                               pv_conf= "lightgreen"))+
  theme_void()+
  #facet_wrap(~region)+
  theme(axis.text = element_blank(),  # Hide axis text
        axis.ticks = element_blank(),  # Hide axis ticks
        panel.grid = element_blank())

#top 10 woredas with hign malaria burden
my_hmis %>%
  filter(year(period_start_date)== 2024, data_type== "positives") %>%
  group_by(region, zone, woreda) %>%
  mutate(region_zone= paste0(region, "-", zone)) %>%
  mutate(woreda= paste0(region_zone, " ", woreda)) %>%
  summarise(total_cases = sum(agg_value, na.rm = T), .groups = "drop") %>%
  arrange(desc(total_cases)) %>%
  head(10) %>%
  ggplot(aes(x= reorder(woreda, total_cases),
             y=total_cases)) +
  geom_bar(stat= "identity", fill= "skyblue")+
  coord_flip()

#heat map for pf burden by region
my_hmis |>
  filter(data_type == "pf_conf") |>
  group_by(region, year, data_type) |>
  summarise(total_cases = sum(agg_value, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = region, y = year, fill = total_cases)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +  # Color scale for total_cases
  labs(title = "Malaria Burden by Region and Year", 
       x = "Region", 
       y = "Year", 
       fill = "Total Cases") +
  theme_minimal() +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


#suspected, tested and positive trend area chart
my_hmis |>
  filter(data_type %in% c("tests", "positives", "clinical")) |>
  group_by(region, year, data_type) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  ggplot(aes(x = year, y = total_cases, fill = data_type)) +
  geom_area(alpha = 0.6, position = "identity") +  #identity prevnts stacking, alpha 0.6 is transparency 
  #geom_line(aes(color = data_type), linewidth = 1) +  # Line graph on top
  scale_fill_manual(values = c("orange", "red", "skyblue")) +  # Custom colors
  #scale_color_manual(values = c("blue", "darkorange", "darkred")) +  # Line colors
  labs(title = "Malaria Testing and Cases Over Time",
       x = "years",
       y = "Totals",
       fill = "types") +
  facet_wrap(~region)+
  theme_minimal()     

#lets calculate pop growth rate from 2025 and 2024 pop i got from UN
pop_2024 <-132059767
pop_2025 <-135472051
pop_growth_rate <- round(((pop_2025-pop_2024)/pop_2024)*100, 2)

#construct pf_pv epidemic curve
my_hmis |>
  filter(data_type %in% c("pf_conf", "pv_conf")) |>
  group_by(year, data_type) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop") |>
  mutate(total_pop = case_when(
    year == 2024 ~ pop_2024,  # For 2024, use known population
    year < 2024 & year >= 2017 ~ {
      # For years before 2024, project the population backward
      pop_for_year <- pop_2024*(1 + pop_growth_rate)^(2024 - year)
      round(pop_for_year, 0)  # Round population to nearest integer
    },
    TRUE ~ NA_real_  # For any other years (just in case)
  )) |>
  mutate(incidence_rate= (total_cases/total_pop)*1000) |>
  ggplot(aes(x=year, y= incidence_rate, fill = data_type)) +
  geom_area(alpha = 0.8, position = "identity")+
  scale_fill_manual(values = c("blue", "lightgreen"))+
  labs(title = "pf_pv epi curve",
       subtitle = "2017-2024",
       x= "years",
       y= "total cases",
       fill= "mal species")+
  theme_minimal()

# Scatter Plot with Trend Lines
my_hmis |>
  filter(data_type %in% c("pf_conf", "pv_conf"))|>
  group_by(region, year) |>
  summarise(total_cases= sum(agg_value, na.rm = T), .groups = "drop")|>
ggplot(aes(x = year, y = total_cases)) +
geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +  # Adding a linear trend line
  theme_minimal() +
  labs(title = "Malaria Cases vs. year",
       x = "Year", 
       y = "Total Cases")+
  facet_wrap(~region)

#PF confirmed trend by region
my_hmis |>
  filter(data_type == "pv_conf") |>
  group_by(region, data_type, year) |>
  summarise(total_cases = sum(agg_value, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = total_cases, group = region)) +  # Added group and color
  geom_line(linewidth = 1.2, alpha = 0.7, color= "brown") +  
  labs(title = "PF Confirmed Cases Trend by Region",
       x = "Years",   # Fixed typo: X → x
       y = "Total Cases") +
  facet_wrap(~region) + 
  theme_minimal()