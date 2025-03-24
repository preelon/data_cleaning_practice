rm(list=ls())
#mal_2017_2024_saved <- readRDS("data/processed/HMIS_zones_reconciled.rds")

#standardizing the second administrative structure "Zone" names
#I will start by reading the files i need
#reading the region reconciled hmis data
mal_2017_2024 <- readRDS("data/processed/region_reconciled.rds")

#reading the Ethiopia's shape file
shape_file <- read_csv("data/processed/eth_shape_file_updated.csv") 

#checking the unique zones number before starting standardization
sort(unique(mal_2017_2024$zone)) #358

#Removing words and spaces that cause inconsistencies from the zonal name
mal_2017_2024 <- mal_2017_2024 %>%
  mutate(zone = gsub(" Zone", "", zone)) %>%
  mutate(zone =gsub(" Woreda", "", zone)) %>%
  mutate(zone = gsub(" Sub City", "", zone)) %>%
  mutate(zone = gsub(" Subcity", "", zone)) %>%
  mutate(zone = gsub(" Town", "", zone)) %>%
  mutate(zone= gsub(" Operational", "", zone)) %>%
  mutate(zone= gsub(" Health Office", "", zone)) %>%
  mutate(zone = gsub(" Zhd", "", zone)) %>%
  mutate(zone = gsub(" Zho", "", zone)) %>%
  mutate(zone = gsub(" Tho", "", zone)) %>%
  mutate(zone = gsub(" Worho", "", zone))%>%
  mutate(zone= gsub(" Zonal Health Department", "", zone)) %>%
  mutate(zone= gsub(" Administration Ho", "", zone))%>%
  mutate(zone= gsub(" City Administration", "", zone)) %>%
  mutate(zone= gsub(" CITY Health Office", "", zone)) %>%
  mutate(zone= gsub("  ", " ", zone)) %>%
  mutate(zone = str_trim(zone, side = "both")) 

#checking the unique zone names after performing the above initial phase zone standardization
sort(unique(mal_2017_2024$zone)) #221

#checking for inconsistency in zone names between the sf and HMIS data
data.frame(zones= unique(mal_2017_2024$zone)) %>%
  filter(!zones %in% shape_file$zone) %>%
  arrange(zones) #at this stage it shows that there are 168 zone names in HMIS but in the SF

#standardizing the zone names in the HMIS to that of the sf
mal_2017_2024 <- mal_2017_2024 %>%
  mutate(zone= gsub("Gojjam", "Gojam", zone),
         zone= gsub("Wollega", "Wellega", zone),
         zone= gsub("Wollo", "Wello", zone)) %>%
  mutate (zone = case_when(zone %in% c("Northern Sidama", "Central Sidama","Southern Sidama", "Hawassa", "Eastern Sidama", "Hawassa Comprehensives Specialized Hospital") ~ "Sidama",
                           zone== "Kembata" ~ "Kembata Tembaro", 
                           zone %in% c("Melkajebdu", "Biyoawale", "Jeldessa") ~ "Dire Dawa Rural",
                           zone %in% c("East Gurage","Kebena Special", "Mareko Special", "Gurage") ~ "Guraghe",
                           zone== "Koore" ~ "Amaro",
                           zone %in% c("Zone 1", "Assayita Primary Hospital","Ayssaita General Hospital", "Abala Primary Hospital", "Dubti Referral Hospital", "Chifra Primary Hospital", "Logia Primary Hospital", "Dubti General Hospital") ~ "Awsi /Zone 1",
                           zone %in% c("Dalifage Primary Hospital","Dalifaghe Primary Hospital", "Gende Kore", "Gendekore", "Sabiyan General Hospital", "Legehare", "Dire Dawa", "Dilchora General Hospital") ~ "Dire Dawa Urban",
                           zone== "West Omo" ~ "Mirab Omo",
                           zone %in% c("Asalla", "Asela") ~ "Arsi", 
                           zone %in% c("Bahirdar", "North Gojam") ~ "West Gojam",
                           zone %in% c("Hakim", "Sofi", "Amir Nur", "Shenkor", "Jugel General Hospital", "Haramaya University Hiwot Fana  Comprehensive Specialized Hospital","Haramaya University Hiwot Fana Comprehensive Specialized Hospital", "Aboker", "Abadir", "Harar General Hospital", "Harar Police Primary Hospital", "Yemaj General Hospital") ~ "Harari",
                           zone %in% c("Debere Birhan", "Debrebirhan") ~ "North Shewa (Am)",
                           zone== "Jinela" ~ "Harari",
                           zone== "Nekemte" ~ "East Wellega",
                           zone %in% c("Jimma Special", "Agaro") ~ "Jimma",
                           zone== "Ari" ~ "South Omo",
                           zone %in% c("Dessie", "Kombolcha") ~ "South Wello",
                           (zone== "North Shewa" & region== "Amhara") ~ "North Shewa (Am)",
                           (zone== "North Shewa" & region== "Oromia") ~ "North Shewa (Or)",
                           zone== "Dire Teyara" ~ "Harari",
                           zone %in% c("Bishoftu","Batu", "Adama Special", "Mojo", "Modjo", "Adama City") ~ "East Shewa",
                           zone %in% c("SHEGER", "Holota", "Sendafa Bake", "Sheger City", "Holeta") ~ "Finfine Special",
                           zone %in% c("Woldia", "Woldiya") ~ "North Wello",
                           zone== "Debretabor" ~ "South Gondar",
                           zone %in% c("Zone 3", "Gewane Primary Hospital", "Mohammed Akle Memorandom General Hospital", "Mohammed Akle Memorial Hospital") ~ "Gabi /Zone 3",
                           zone== "Oromia Special" ~ "Oromia",
                           zone %in% c("Zone 2", "Berahlae Primary Hospital", "Berahlae Primary Hospital", "Berhale Primary Hospital") ~ "Kilbati /Zone 2",
                           zone %in% c("Zone 4", "Kelewan District Hospital") ~ "Fanti /Zone 4",
                           zone== "Tembaro Special" ~"Kembata Tembaro",
                           zone %in% c("East Borena", "Shakiso") ~ "Guji",
                           zone== "Metu" ~ "Ilu Aba Bora",
                           zone== "Wahil" ~ "Dire Dawa Rural",
                           zone %in% c("Shashemene", "Dodola", "Arsi Negele", "Dodola") ~ "West Arsi",
                           zone %in% c("Agniwa", "Gambella", "Gambella Referal Hospital", "Gambella General Hospital") ~ "Agnewak",
                           zone== "Moyale" ~ "Borena",
                           zone== "Yem" ~ "Yem Special",
                           zone==  "Gardula" ~ "Derashe",
                           zone %in% c("Debre Markos", "Debremarkos") ~ "East Gojam",
                           zone== "Maya" ~ "East Hararge",
                           zone== "Konta" ~ "Konta Special",
                           zone== "Zone 5" ~ "Hari /Zone 5",
                           zone== "Gondar" ~ "Central Gondar",
                           zone %in% c("Gilgel Beles", "Gilgelbeles") ~ "Metekel",
                           zone %in% c("Goro", "Woliso") ~ "South West Shewa",
                           zone %in% c("Ambo", "Ambotown") ~ "West Shewa",
                           zone %in% c("Bule Hora", "Bule Hora Health Department") ~ "West Guji",
                           zone %in% c("Sultan Sheikh Hassan Memorial Referral Hospital", "Jigjiga University Sultan Sheikh Hasan Yabare Comprhensive Specialized Hospital") ~ "Fafan",
                           zone== "Itang Special" ~ "Itang Special Woreda", 
                           zone== "Nedjo" ~ "West Wellega",
                           zone== "Robe" ~ "Bale",
                           zone== "Sheno" ~ "North Shewa (Or)",
                           zone== "Mao Komo" ~ "Mao Komo Special",
                           zone== "Shebelle" ~ "Shabelle",
                           zone== "Korahay" ~ "Korahe",
                           zone == "Kaffa" ~ "Kefa",
                           zone == "Dawro" ~ "Dawuro",
                           zone == "Nuer" ~ "Nuwer",
                           zone == "Waghimera" ~ "Wag Hamra",
                           zone == "Goffa" ~ "Gofa",
                           zone == "Silte" ~ "Siltie",
                           zone == "Erar" ~ "Erer",
                           zone == "Liben" ~ "Liban",
                           zone == "Dawa" ~ "Daawa",
                           zone == "Sitti" ~ "Siti",
                           zone == "Asosa" ~ "Assosa",
                           zone == "Dollo" ~ "Doolo",
                           zone == "Ale" ~ "Alle",
                           zone == "Wolaita" ~ "Wolayita",
                           zone== "Horo Guduru Wellega" ~ "Horo Gudru Wellega",
                           zone %in% c("St Paulos Comprehensive Specialized Hospital", "St. Peter General Hospital") ~ "Gulele",
                           zone== "Tikur Anbessa Comprehensive Specialized Hospital" ~ "Lideta",
                           zone== "Dagmawi Minilik Comprehensive Specialized Hospital" ~ "Yeka",
                           zone== "Alert General Hospital" ~ "Kolfe Keraniyo",
                           zone %in% c("Ras Desta Damitew General Hospital", "Yekatit 12 General Hospital Medical College") ~ "Arada",
                           TRUE ~ zone))

#rechecking the zones names that are in the HMIS but in the sf after performing the
#above step to standardize the names (inconsistent zone names)
data.frame(zones= unique(mal_2017_2024$zone)) %>%
  filter(!zones %in% shape_file$zone) %>%
  arrange(zones) #now there are 25 unmatched zones, that are all AA zones 

#lets deal with the AA scenario to get all the zones matched
mal_2017_2024 <- mal_2017_2024 %>%
  mutate(woreda= case_when(region== "Addis Ababa" & woreda !=zone ~ zone,
                           TRUE ~ woreda)) %>%
  mutate(zone= case_when(region== "Addis Ababa" ~ "Region 14",
                         region== "Dire Dawa" & zone== "Addis Ketema" ~"Dire Dawa Urban",
                         TRUE ~ zone)) 

#now lets see if there are unmatched zones between the HMIS and the shapefile
data.frame(zones= unique(mal_2017_2024$zone)) %>%
  filter(!zones %in% shape_file$zone) %>%
  arrange(zones) 
#

#checking if the code is repeatable
#identical(mal_2017_2024_saved, mal_2017_2024)

#saving the HMIS dataset with reconciled zone names

saveRDS(mal_2017_2024, file = "data/processed/HMIS_zones_reconciled.rds")

#---------------------------------END--------------------------------------

