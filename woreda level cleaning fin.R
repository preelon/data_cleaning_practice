rm(list=ls())
#this script aims to crean the 3rd admistrative level(Woreda) names in the HMIS
#I will start reading the necessary files
#reading the zone reconciled HMIS data
mal_2017_2024 <- readRDS("data/processed/HMIS_zones_reconciled.rds")%>%
  mutate(woreda = case_when (region=="Addis Ababa" & zone != "Region 14" ~ zone,
                             TRUE ~ woreda)) %>%
  mutate(zone = case_when (region=="Addis Ababa" ~ "Region 14",
                           TRUE ~ zone))

#reading the AHRI matched file
ahri_matched <- read_csv("data/raw/ETH_DHIS2_woreda_final_AHRI_matched.csv") %>%
  janitor::clean_names() %>%
  mutate(woreda_1082 = str_to_title(woreda_1082)) 

#removing names and spaces that can cause inconsistencies from the AHRI matched data set
ahri_matched <- ahri_matched %>%
  mutate(dhis2_region= gsub("[^[:alnum:] ]", "", dhis2_region)) %>%
  mutate(dhis2_region= gsub("\\s+", " ", dhis2_region))%>%
  mutate(dhis2_zone= gsub("[^[:alnum:] ]", "", dhis2_zone)) %>%
  mutate(dhis2_zone= gsub("\\s+", " ", dhis2_zone)) %>%
  mutate(dhis2_woreda= gsub("[^[:alnum:] ]", "", dhis2_zone)) %>%
  mutate(dhis2_woreda= gsub("\\s+", " ", dhis2_woreda)) %>%
  mutate(dhis2_region=str_to_title(dhis2_region)) %>%
  mutate(dhis2_zone=str_to_title(dhis2_zone)) %>%
  mutate(dhis2_woreda=str_to_title(dhis2_woreda))


#reading the eth shapefile
dat_1082 <- read_csv("data/processed/eth_sf_updated.csv") %>%
  select(id_1082, region,zone, woreda, pop_2022=T_TL) %>%
  mutate(region = gsub(" Ethiopia", "", region)) %>%  
  mutate(zone = case_when(woreda %in% c("Akaki Kality","Nifas Silk Lafto","Kolfe Keraniyo",
                                        "Bole","Lideta","Kirkos","Yeka","Addis Ketema",
                                        "Arada","Gulele","Lemi Kura") ~ paste0(woreda," Sub City"),
                          TRUE ~ zone)) %>%
  mutate(region = gsub(" Ethiopia", "", region)) %>%   # remove text that could introduce inconsistencies 
  mutate(region = str_trim(region, side = "both")) %>%
  mutate(woreda = str_trim(woreda, side = "both")) %>%
  mutate(woreda = str_to_title(woreda))%>%
  mutate(woreda= gsub("[^[:alnum:] ]", "", woreda)) %>%
  mutate(woreda= gsub("\\s+", " ", woreda)) %>%
  mutate(woreda= gsub(" ", "_", woreda)) %>%
  mutate(woreda= gsub("___", "_", woreda)) %>%
  mutate(woreda= gsub("__", "_", woreda)) %>%
  mutate(woreda= gsub("_", " ", woreda))

rm(ahri_hmis_joined)

#creating a df with unique values of the below selected variables
#unique_hmis <- mal_2017_2024 %>%
 # select(organisation_unit_name, region, zone, woreda,facility, phcu) %>%
 # distinct()

#left joining the AHRI matched dat with the sf
ahri_sf_joined <- ahri_matched %>%
  left_join(dat_1082, by= c("dhis2_region"= "region", "woreda_1082"="woreda"))

#filtering out those NA values after the left join
for_manual_reconcilation <- ahri_sf_joined %>%
  mutate(Is_na = ifelse(is.na(id_1082), 1, 0)) #350 inconsistent id b/n the sf and AHRI matched doc

#saving the AHRI an sf joined data so that i can manually correct the unmatched zones
write.csv(for_manual_reconcilation, "data/for_manual_reconcilation.csv")

#reading the data i saved for manual reconcilation
correct_match <- read_csv("data/for_manual_reconcilation.csv") %>%
  filter(Is_na== 1) %>%
  dplyr::select(sn, dhis2_woreda, woreda_1082)
unique(re_matched$woreda_1082.y)
#left joining the data i prepared for manual reconcilation with the previous ahri and sf joined
 re_matched <- ahri_sf_joined %>%
   left_join(correct_match, by = c("sn", "dhis2_woreda")) %>%
   mutate(woreda_1082.x = case_when(is.na(woreda_1082.y) ~ woreda_1082.x,
                                    TRUE ~ woreda_1082.y)) %>%
   mutate(woreda_1082 = woreda_1082.x) %>%
   dplyr::select(!c(woreda_1082.x, woreda_1082.y, zone, id_1082, pop_2022)) %>%
   mutate(woreda_1082 = str_to_title(woreda_1082)) %>%
   left_join(dat_1082, by = c("dhis2_region"="region", "woreda_1082"="woreda")) %>% 
   names(re_matched)
 
 
unique(re_matched$woreda_1082)
#creating another copy of the HMIS which i can work on
mal_2017_2024_cop <- mal_2017_2024 %>%
  mutate(woreda = gsub(" Worho", "", woreda))%>%
  mutate(woreda = gsub(" Sub City", "", woreda))%>%
  mutate(woreda = gsub(" Subcity", "", woreda))%>%
  mutate(woreda = str_trim(woreda, side = "both"))%>%
  mutate(woreda = gsub("\\s+", " ", woreda)) %>%
  left_join(re_matched, by = c("region" = "dhis2_region",
                          "zone" = "dhis2_zone",
                          "woreda" = "dhis2_woreda"), relationship = "many-to-many") %>%
  mutate(woreda= woreda_1082) %>%
  
  
sort(unique(mal_2017_2024_cop$woreda_1082))
NAz <- mal_2017_2024_cop %>%
  filter(is.na(woreda_1082)) %>%
  distinct()
       