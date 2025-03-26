rm(list = ls())
#libraries
library(tidyverse)
#reading the necessary files
#reading the HMIS 
mal_2017_2024 <- readRDS("data/processed/HMIS_zones_reconciled.rds") %>%
  mutate(facility = case_when(!is.na(phcu) & is.na(facility)  ~ phcu, # fill NA within facility column
                              is.na(phcu) & is.na(facility) & !is.na(woreda) ~ woreda,
                              is.na(phcu) & is.na(facility) & is.na(woreda) ~ zone,
                              TRUE ~ facility)) %>%
  mutate(woreda = case_when(is.na(woreda)~ zone,
                            TRUE ~ woreda)) %>%
  mutate(woreda= gsub("\\s+", " ", trimws(woreda))) %>%
  #mutate(woreda= gsub("_", " ", woreda))%>%
  mutate(woreda= str_to_title(woreda)) %>%
  #mutate(woreda = gsub("[^[:alnum:] ]", " ", woreda)) %>% #removes special characters including underscore and hyphene
  mutate(woreda= gsub("-", " ", woreda)) %>%
  mutate(woreda= gsub(" Woreda", "", woreda)) %>%
  mutate(woreda= gsub(" Tho", "", woreda))%>%
  mutate(woreda= gsub(" Worda", "", woreda)) %>%
  mutate(woreda= gsub(" Worho", "", woreda)) %>%
  mutate(woreda= gsub(" Woho", "", woreda)) %>%
  mutate(woreda= gsub(" City Administration", "", woreda)) %>%
  mutate(woreda= gsub(" City Admin", "", woreda)) %>%
  mutate(woreda= gsub(" Administration", "", woreda))%>%
  mutate(woreda= gsub(" Admin", "", woreda))%>%
  #mutate(woreda= gsub(" tho", "", woreda))
  # mutate(woreda= gsub(" Town", "", woreda)) %>%
  mutate(woreda= gsub(" Subcity", "", woreda)) %>%
  mutate(woreda= gsub(" Sub City", "", woreda))%>%
  mutate(woreda= gsub(" Town Adminstration", "", woreda)) %>%
  mutate(woreda= gsub(" Cityistration", "", woreda)) %>%
  mutate(woreda = ifelse(grepl("phcu", woreda), paste0(" ", woreda), woreda)) %>%
  mutate(woreda= str_to_title(woreda)) %>%
  mutate(woreda= gsub("\\s+", " ", woreda))%>%
  mutate(woreda= str_trim(woreda, side = "both")) %>%
  mutate(woreda = gsub("Dr ", "Dr. ", woreda)) %>%
  mutate(woreda = gsub("'", "", woreda)) %>%
  mutate(woreda = gsub("_phcu", " Phcu", woreda)) %>%
  mutate(woreda = case_when (grepl("Universal", woreda) & grepl("Medium Clinic", woreda) ~ "Universal Medium Clinic",
                             TRUE ~ woreda)) %>%
  mutate(woreda = case_when (grepl("Dr.fira", woreda) & grepl("ol Medium Clinic", woreda) ~ "Dr.firaol Medium Clinic",
                             TRUE ~ woreda)) %>%
  mutate(woreda = case_when(woreda== "Shakisophcu" ~ "Shakiso Phcu",
                            TRUE ~ woreda)) %>%
  mutate(zone  = case_when(region == "Afar" & woreda %in% c("Dalifaghe Primary Hospital", "Dalifage Primary Hospital") ~ "Hari /Zone 5",
                           region == "Afar" & woreda == "Abala Primary Hospital" ~ "Kilbati /Zone 2",
                           region == "Dire Dawa" & woreda %in% c("Melkajebdu Phcu", "Addis Ketema Phcu", 
                                                                 "Jelobelina Phcu", "Jellobelina Phcu",
                                                                 "Goro Phcu") ~ "Dire Dawa Urban", 
                           region == "Dire Dawa" & woreda == "Industry Mender Phcu" ~ "Dire Dawa Rural",
                           region == "Harari" ~ "Harari",
                           TRUE ~ zone)) %>%
  mutate(region = str_trim(region, side = "both")) %>%
  mutate(zone = str_trim(zone, side = "both")) %>%
  mutate(woreda = str_trim(woreda, side = "both")) 

#reading the AHRI matched file
ahri_matched <- read_csv("data/processed/ETH_DHIS2_woreda_final_AHRI_matched 2.csv")%>%
  janitor::clean_names() %>%
  mutate(dhis2_region = str_to_title(dhis2_region),
         dhis2_zone = str_to_title(dhis2_zone),
         dhis2_woreda = str_to_title(dhis2_woreda)) %>%
  mutate(dhis2_woreda = gsub("Dr ", "Dr.", dhis2_woreda))%>%
  mutate(woreda_1082 = str_to_title(woreda_1082)) %>%
  mutate(woreda_1082= str_trim(woreda_1082, side = "both")) %>%
  mutate(zone_1082= str_trim(zone_1082, side= "both"))%>%
  mutate(zone_1082= str_to_title(zone_1082)) %>%
  mutate(dhis2_region= str_trim(dhis2_region, side = "both"))

#reading the shape file
shape_file <- read_csv("data/processed/eth_shape_file_updated.csv") %>%
  janitor::clean_names() %>%
  mutate(region= str_to_title(region)) %>%
  mutate(zone= str_to_title(zone)) %>%
  mutate(woreda= str_to_title(woreda)) %>%
  select(id_1082, region, zone, woreda, pop_2022) %>%
  mutate(region= str_trim(region))%>%
  mutate(zone= str_trim(zone)) %>%
  mutate(woreda= str_trim(woreda)) %>%
  mutate(woreda= case_when(woreda== "Adama Tulu Jido Kombolcha" & zone== "East Shewa" ~ "Adami Tulu Jido Kombolcha",
                           TRUE ~ woreda))

#lets join the ahri and the hmis data
hmis_ahri_joined <- mal_2017_2024 %>%
  left_join(ahri_matched, by= c("region" = "dhis2_region",
                                "zone" = "dhis2_zone",
                                "woreda" = "dhis2_woreda"))

hmis_ahri_joined %>%
  filter(zone=="Guji" & woreda== "Haro Wolabu") %>%
  distinct(region, zone, woreda, zone_1082, woreda_1082)

#check if there ara unmatched woredas between the HMIS and the AHRI
not_matched_hmis_ahri <- hmis_ahri_joined %>%
  filter(is.na(woreda_1082)) %>%
  distinct(region, zone, woreda) #resulted 3 observations

#Lets deal with the 3 unmatched woredas
#NB: I inserted the 10 new health facilities manually in the ahri excel sheet
mal_2017_2024 <- mal_2017_2024 %>%
  mutate(woreda= case_when(woreda== "Kolfe" ~ "Kolfe Keraniyo",
                           woreda== "St Paulos Comprehensive Specialized Hospital" ~ "Gulele",
                           woreda== "Metero Phcu Phcu" ~ "Metero Phcu",
                           TRUE ~ woreda))

#lets check if the 3 unmatched woredas are resolved
#rejoin 1st 
hmis_ahri_joined <- mal_2017_2024 %>%
  left_join(ahri_matched, by= c("region" = "dhis2_region",
                                "zone" = "dhis2_zone",
                                "woreda" = "dhis2_woreda"))

#check if there ara unmatched woredas between the HMIS and the AHRI
not_matched_hmis_ahri <- hmis_ahri_joined %>%
  filter(is.na(woreda_1082)) %>%
  distinct(region, zone, woreda) #0 unmatched

shape_file%>%
  filter(zone=="Guji" & woreda== "Haro Walabu")

#now lets join the hmis_ahri_joined data to the sf to get id_1082
hmis_ahri_sf_joined <- hmis_ahri_joined %>%
  left_join(shape_file, by= c("region","zone", "woreda_1082" = "woreda")) 

#lets see if there are unmatched wredas between the sf and the hmis_ahri_joined
not_matched <- hmis_ahri_sf_joined %>%
  filter(is.na(id_1082)) %>%
  distinct(region, zone, zone_1082, woreda, woreda_1082) #181 woredas are unmatched, initially was 295
#some corrections made on the ahri excel

#lets deal with the 183 unmatched woredas
hmis_ahri_joined <- hmis_ahri_joined %>%
  mutate(zone= case_when(woreda_1082 %in% c("Welmera", "Sebeta Hawas") ~ "Finfine Special",
                         woreda_1082 %in% c("Wachile", "Arero","Dhas") ~ "Borena",
                         woreda_1082== "Dera (Or)" ~ "North Shewa (Or)",
                         woreda_1082== "Baso Liben" ~ "East Gojam",
                         woreda_1082 %in% c("Akaki", "Bereh", "Mulo", "Sululta") ~ "Finfine Special",
                         woreda_1082== "Makuey" ~ "Nuwer",
                         woreda_1082== "Samera Logiya Town" ~ "Awsi /Zone 1",
                         woreda_1082== "Bahir Dar Town" ~ "West Gojam",
                         TRUE~ zone)) %>%
  mutate(woreda_1082= case_when(woreda_1082== "Gonder Zuriya" ~ "Gonder Zuria",
                                woreda_1082== "Gomibora" ~ "Gombora",
                                woreda_1082== "Halaba Kulito Town" ~ "Kulito Town",
                                woreda_1082== "Bare" ~ "Barey",
                                woreda_1082== "Dawa Cheffa" ~ "Dewa Cheffa",
                                woreda_1082== "Wogera" ~ "Wegera",
                                woreda_1082== "Dera(Am)" ~ "Dera (Am)",
                                woreda_1082== "Magale" ~ "Megale",
                                woreda_1082== "Dangla" ~ "Dangila",
                                woreda_1082== "Gazgibla" ~ "Gaz Gibla",
                                woreda_1082== "Lanfro" ~ "Lanfero",
                                woreda_1082== "Oda Bultum" ~ "Kuni /Oda Bultum",
                                woreda_1082== "Andabet" ~ "Andabet/ West Esite",
                                woreda_1082== "Wenberma" ~ "Wemberma",
                                woreda_1082== "Ankash" ~ "Ankasha",
                                woreda_1082== "Abergele(Am)" ~ "Abergele (Am)",
                                woreda_1082== "Abeshgie" ~ "Abeshege",
                                woreda_1082== "Tarma Ber" ~ "Tarema Ber",
                                woreda_1082== "Buldigilu" ~ "Bilidigilu",
                                woreda_1082== "Meneze Keya Gebriel" ~ "Menze Keya Gabriel",
                                woreda_1082== "Erabti" ~ "Erebti",
                                woreda_1082== "Dilla Zuria" ~ "Dila Zuria",
                                woreda_1082== "Bahir Dar Zuria" ~ "Bahirdar Zuria",
                                woreda_1082== "Agena" ~ "Ezha",
                                woreda_1082== "Muhor Na Aklil" ~ "Muhur Na Aklil",
                                woreda_1082== "Deri Saja Zuria" ~ "Yem Sp Woreda",
                                woreda_1082== "Telalak"~ "Telalek",
                                woreda_1082== "Finote Selam" ~ "Finote Selam Town",
                                woreda_1082== "Gambela Town" ~ "Gambella Town",
                                woreda_1082== "Dilla Town" ~ "Dila Town",
                                woreda_1082== "Wonchi" ~ "Wenchi",
                                woreda_1082== "Semurobi" ~ "Samurobi",
                                woreda_1082== "Debre Birhan Town" ~ "Debre Berhan Town",
                                woreda_1082== "Gambela Zuria" ~ "Gambella Zuria",
                                woreda_1082== "Marawi Town" ~ "Merawi Town",
                                woreda_1082== "Jamma Town" ~ "Jama",
                                woreda_1082== "Tsagbji" ~ "Tsagbeji",
                                woreda_1082== "Erer(HR)" ~ "Erer (Hr)",
                                woreda_1082== "Laelay Adiyabo" ~ "Laelay Adiabo",
                                woreda_1082== "Demboya" ~ "Damboya",
                                woreda_1082== "Asosa" ~ "Assosa",
                                woreda_1082== "Jilye Timuga" ~ "Jilye Tumuga",
                                woreda_1082== "Abala" ~ "Abaala",
                                woreda_1082== "Laha Town" ~ "Melekoza",
                                woreda_1082== "Mekan Eyesus" ~ "Mekan Eyesuse",
                                woreda_1082== "Alfa" ~ "Alefa",
                                woreda_1082== "Babile" ~ "Babile Town",
                                woreda_1082== "Meki Town" ~ "Dugda",
                                woreda_1082== "Enbese Sarmeder" ~ "Enebse Sarmder",
                                woreda_1082== "East Dembiya" ~ "East Dembia",
                                woreda_1082== "Enarj Enawega" ~ "Enarj Enawga",
                                woreda_1082== "Mida Weremo" ~ "Mida Woremo",
                                woreda_1082== "Debre Markose Town" ~ "Debre Markos Town",
                                woreda_1082 %in% c("Halaba", "Halaba Kulito") ~ "Kulito Town",
                                woreda_1082== "Debr Tabor Town" ~ "Debre Tabor Town",
                                woreda_1082== "Maya Town" ~ "Haromaya Town",
                                woreda_1082== "Mekane Selam" ~ "Borena /Debresina",
                                woreda_1082== "Menz Gera Midir" ~ "Menze Gera Midir",
                                woreda_1082== "Wegidi" ~"Wegde",
                                woreda_1082== "Sululta" ~ "Sululta Town",
                                woreda_1082== "Jimma  Town" ~ "Jimma Town",
                                woreda_1082== "Holota Town" ~ "Holeta Town",
                                woreda_1082== "Basona" ~ "Basona Worena",
                                woreda_1082== "Gunchire" ~ "Enemor Ener",
                                woreda_1082== "Moyale Town" ~ "Moyale (Or)",
                                woreda_1082== "Sheno Town" ~ "Kimbibit",
                                woreda_1082== "Woreta" ~ "Woreta Town",
                                woreda_1082== "Bule Town" ~ "Bule Hora Town",
                                woreda_1082==  "Menz Lalo Midir" ~ "Menze Lalo Midir",
                                woreda_1082== "Bure(Am)" ~ "Bure (Am)",
                                woreda_1082== "Badewacho" ~ "Misrak Badawacho",
                                woreda_1082== "Efertana Gidem" ~ "Eferatana Gidem",
                                woreda_1082== "South Shewa" ~ "Becho (SW Shewa)",
                                woreda_1082== "North Wollo Town" ~ "Mersa Town",
                                woreda_1082== "Dega Damo" ~ "Dega Damot",
                                woreda_1082== "Guji" ~ "Saba Boru",
                                woreda_1082== "Hosaena" ~ "Hosaena Town",
                                (woreda_1082== "Debub Sodo" & zone == "Wolayita") ~ "Areka Town",
                                woreda_1082== "Butajira" ~ "Butajira Town",
                                woreda_1082== "Wolayita Town" ~ "Sodo Town",
                                woreda_1082==  "Saya Deberna Wayu" ~ "Siya Debirna Wayu",
                                woreda_1082== "Mojana" ~ "Mojan Wedera",
                                woreda_1082== "North Shewa Town" ~ "Menze Mama Midir",
                                woreda_1082== "Dodla Town" ~ "Dodola Town",
                                woreda_1082== "Ofa" & zone== "Arsi" ~ "Shirka",
                                TRUE ~ woreda_1082))

#rejoin the the hmis_ahri and the sf to incorporate the changes
hmis_ahri_sf_joined <- hmis_ahri_joined %>%
  left_join(shape_file, by= c("region","zone","woreda_1082" = "woreda"))

#lets see the unmatched woredas again to ensure the changes are incorporated
not_matched <- hmis_ahri_sf_joined %>%
  filter(is.na(id_1082)) %>%
  distinct(region, zone_1082, woreda_1082) # now there are 11 unmatched woredas

#lets deal with the remaining 11 unmatched woredas
hmis_ahri_joined <- hmis_ahri_joined %>%
  mutate(zone_1082=case_when(woreda_1082== "Sululta Town" ~ "Finfine Special",
                        woreda_1082== "Akaki Kality" ~ "Region 14",
                        zone== "Erer (Hr)" ~ "Harari",
                        woreda_1082== "Assosa" ~ "Assosa",
                        woreda_1082== "East Borena" ~ "Bale",
                        woreda_1082== "Dodola Town" ~ "West Arsi",
                        woreda_1082== "Meda Welabu" & zone_1082== "Guji" ~ "Bale",
                        TRUE ~ zone_1082)) %>%
  mutate(woreda_1082= case_when(region== "Central" & zone== "Guraghe" & woreda_1082== "Sodo Town" ~ "Sodo",
                                woreda_1082== "Muhor Na Aklil" & zone== "Guraghe" ~ "Muhur Na Aklil",
                                woreda_1082== "Yem SP Woreda" ~ "Yem Sp Woreda",
                                woreda_1082== "Becho (SW Shewa)" ~ "Becho (Sw Shewa)",
                                zone== "Hadiya" & woreda_1082=="Gimbichu" ~ "Gimbichu Town",
                                zone== "Bale" & woreda_1082== "Batu" ~ "Goba (Or)",
                                woreda_1082== "Erer(Hr)" ~ "Erer (Hr)",
                                woreda_1082== "Dera Am" & zone== "South Gondar" ~ "Dera (Am)",
                                woreda_1082== "Batu" ~ "Adami Tulu Jido Kombolcha",
                                zone== "Yem Special" ~ "Yem Sp Woreda",
                                zone== "Wolayita" & woreda_1082== "Sodo" ~ "Sodo Town",
                                woreda_1082== "East Borena" ~ "Meda Welabu",
                                woreda_1082== "Wasama" ~"Abaala",
                                grepl("West", woreda_1082) & grepl("Belesa", woreda_1082) ~ "West Belesa", 
                                TRUE ~ woreda_1082)) 
  

#rejoin the the hmis_ahri and the sf to incorporate the changes
hmis_ahri_sf_joined <- hmis_ahri_joined %>%
  left_join(shape_file, by= c("region","zone_1082"= "zone","woreda_1082" = "woreda"))

#lets see the unmatched woredas again to ensure the changes are incorporated
not_matched <- hmis_ahri_sf_joined %>%
  filter(is.na(id_1082)) %>%
  distinct(region, zone_1082, woreda_1082) # now there are 0 unmatched woredas


#save the cleaned woreda reconciled data set (which is hmis_ahri_joined_fin)
saveRDS(hmis_ahri_sf_joined, "data/processed/hmis_woreda_reconciled.rds")
#-----------------------------------END-----------------------------------------
#to extract how many years of data the data set is
sort(unique(year(ymd(hmis_ahri_joined$period_start_date))))

unique(sort(hmis_ahri_sf_joined$woreda_1082))
