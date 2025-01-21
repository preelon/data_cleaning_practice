#standardizing the second adminstrative structure "Zone" names

#creating a data frame of unique zones from the reference dataset
#zone_names <- data.frame(zones= unique(dat_1082$zone))

#cleaning the zonal names in the HMIS dataset
unique(mal_2017_2024$zone)


#Removing words and spaces that cause inconsistencies from the zonal name
mal_2017_2024$zone <- trimws(as.character(mal_2017_2024$zone))

mal_2017_2024 <- mal_2017_2024 %>%
  mutate(zone = gsub(" Zone", "", zone)) %>%
  mutate(zone =gsub(" Woreda", "", zone)) %>%
  mutate(zone = str_trim(zone, side = "both")) %>%
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
  mutate(zone= gsub(" CITY Health Office", "", zone)) 



#standardizing similar names with different spellings
mal_2017_2024 <- mal_2017_2024 %>%
  mutate(zone = case_when(
    zone == "Alert General Hospital" ~ "Alert Specialized Hospital",
    zone == "Dagmawi Menilik Referral Hospital" ~ "Dagmawi Minilik Comprehensive Specialized Hospital",
    zone == "Yekatit 12 General Hospital Medical College" ~ "Yekatit 12 Medical College General Hospital",
    zone == "Ras Desta Damitew General Hospital" ~ "Ras Desta Damtew Memoreal General Hospital",
    zone == "St Paulo's Comprehensive Specialized Hospital" ~ "St. Paul Specialized Referral Hospital",  
    zone == "Shebelle" ~ "Shabelle",
    zone == "Korahay" ~ "Korahe",
    zone == "Kaffa" ~ "Kefa",
    zone == "Kolfe" ~ "Kolfe Keraniyo",
    zone == "Dawro" ~ "Dawuro",
    zone == "Nuer" ~ "Nuwer",
    zone == "Waghimera" ~ "Wag Hamra",
    zone == "Goffa" ~ "Gofa",
    zone == "Mohammed Akle Memorandom General Hospital" ~ "Mohammed Akle Memorial Hospital", 
    zone == "Silte" ~ "Siltie",
    zone == "Erar" ~ "Erer",
    zone == "Liben" ~ "Liban",
    zone == "Dawa" ~ "Daawa",
    zone == "Sitti" ~ "Siti",
    zone == "Asosa" ~ "Assosa",
    zone == "Dollo" ~ "Doolo",
    zone == "Ale" ~ "Alle",
    zone == "Gilgelbeles" ~ "Gilgel Beles",
    zone == "North Wollo" ~ "North Wello",
    zone == "Wolaita" ~ "Wolayita",
    zone == "Dalifage Primary Hospital" ~ "Dalifaghe Primary Hospital",
    zone== "St. Peter General Hospital" ~ "St. Peter Tb Specialized Hospital",
    zone== "Berahlae Primary Hospital" ~ "Berhale Primary Hospital",
    zone== "Assayita Primary Hospital"  ~"Ayssaita General Hospital",
    zone== "Haramaya University Hiwot Fana  Comprehensive Specialized Hospital" ~ "Haramaya University Hiwot Fana Comprehensive Specialized Hospital",
    zone== "West Gojjam" ~ "West Gojam",
    zone== "East Gojjam" ~ "East Gojam",
    zone== "Horo Guduru Wollega" ~ "Horo Gudru Wellega",
    zone== "West Wollega" ~ "West Wellega",
    zone== "Tikur Anbessa Specialized Hospital" ~ "Tikur Anbessa Comprehensive Specialized Hospital",
    zone== "St Paulo's  Comprehensive Specialized Hospital" ~ "St. Paul Specialized Referral Hospital",
    zone== "Alert General  Hospital" ~ "Alert Specialized Hospital",
    TRUE ~ zone))

unique(mal_2017_2024$zone)


mal_2017_2024 <- mal_2017_2024 %>%
  mutate (zone = case_when(zone %in% c("Northern Sidama", "Central Sidama", "Southern  Sidama", "Hawassa", "Eastern Sidama", "Hawassa Comprehensives Specialized Hospital") ~ "Sidama",
                           zone== "Kembata" ~ "Kembata Tembaro", 
                           zone %in% c("Melkajebdu", "Biyoawale", "Jeldessa") ~ "Dire Dawa Rural",
                           zone %in% c("East Gurage","Kebena Special", "Mareko Special", "Gurage") ~ "Guraghe",
                           zone== "Koore" ~ "Amaro",
                           zone %in% c("Zone 1", "Ayssaita General Hospital", "Abala Primary Hospital", "Dubti Referral Hospital", "Chifra Primary Hospital", "Logia Primary Hospital", "Dubti General Hospital") ~ "Awsi /Zone 1",
                           zone %in% c("Dalifaghe Primary Hospital", "Gende Kore", "Gendekore", "Sabiyan General Hospital", "Legehare", "Dire Dawa", "Dilchora  General Hospital") ~ "Dire Dawa Urban",
                           zone== "West Omo" ~ "Mirab Omo",
                           zone %in% c("Asalla", "Asela") ~ "Arsi", 
                           zone %in% c("Bahirdar", "North Gojjam") ~ "West Gojam",
                           zone %in% c("Hakim", "Sofi", "Amir Nur", "Shenkor", "Jugel General Hospital", "Haramaya University Hiwot Fana Comprehensive Specialized Hospital", "Aboker", "Abadir", "Harar General Hospital", "Harar Police Primary Hospital", "Yemaj General Hospital") ~ "Harari",
                           zone %in% c("Debere Birhan", "Debrebirhan") ~ "North Shewa (AM)",
                           zone== "Jinela" ~ "Harari",
                           zone== "Nekemte" ~ "East Wellega",
                           zone %in% c("Jimma Special", "Agaro") ~ "Jimma",
                           zone %in% c("Bole", "Akaki Kality", "Nifas Silk Lafto", "Kolfe Keraniyo", "Lideta", "Kirkos",
                                       "Yeka", "Addis Ketema", "Arada", "Gulele", "Lemi Kura", "Yekatit 12 Medical College General Hospital", "Alert Specialized Hospital", "St. Paul Specialized Referral Hospital","Tikur Anbessa Comprehensive Specialized Hospital", "Tirunesh Beijing Hospital", "Zewditu Memorial General Hospital", "St. Peter Tb Specialized Hospital", "Dagmawi Minilik Comprehensive Specialized Hospital", "Yeka Kotebe General Hospital", "Ras Desta Damtew Memoreal General Hospital", "Gandhi Memorial General Hospital", "Amanuel Referal Hospital") ~ "Region 14",
                           zone== "Ari" ~ "South Omo",
                           zone %in% c("South Wollo", "Dessie", "Kombolcha") ~ "South Wello",
                           (zone== "North Shewa" & region== "Amhara") ~ "North Shewa (AM)",
                           (zone== "North Shewa" & region== "Oromia") ~ "North Shewa (OR)",
                           zone== "Dire Teyara" ~ "Harari",
                           zone %in% c("Bishoftu","Batu", "Adama Special", "Mojo", "Modjo", "Adama City") ~ "East Shewa",
                           zone %in% c("SHEGER", "Holota", "Sendafa Bake", "Sheger City", "Holeta") ~ "Finfine Special",
                           zone %in% c("Woldia", "Woldiya") ~ "North Wello",
                           zone== "Debretabor" ~ "South Gondar",
                           zone %in% c("Zone 3", "Gewane Primary Hospital", "Mohammed Akle Memorial Hospital") ~ "Gabi /Zone 3",
                           zone== "Oromia Special" ~ "Oromia",
                           zone %in% c("Zone 2", "Berahlae Primary Hospital", "Berhale Primary Hospital") ~ "Kilbati /Zone 2",
                           zone %in% c("Zone 4", "Kelewan District  Hospital") ~ "Fanti /Zone 4",
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
                           zone== "Gilgel Beles" ~ "Metekel",
                           zone %in% c("Goro", "Woliso") ~ "South West Shewa",
                           zone %in% c("Ambo", "Ambotown") ~ "West Shewa",
                           zone %in% c("Bule Hora", "Bule Hora Health Department") ~ "West Guji",
                           zone %in% c("Sultan Sheikh Hassan Memorial Referral Hospital", "Jigjiga University Sultan Sheikh Hasan Yabare Comprhensive Specialized Hospital") ~ "Fafan",
                           zone== "Itang Special" ~ "Itang Special woreda", 
                           zone== "Nedjo" ~ "West Wellega",
                           zone== "Robe" ~ "Bale",
                           zone== "Sheno" ~ "North Shewa (OR)",
                           zone== "Mao Komo" ~ "Mao Komo Special",
                           TRUE ~ zone))

#creating a data frame of the zones in the 2017 to 2024 HMIS 
zone_names <- data.frame(zones= unique(mal_2017_2024$zone))                              

#checking for inconsistency in zone names between the sf and HMIS data
inconsistent_zones <- zone_names %>%
  filter(!zones %in% dat_1082$zone) %>%
  arrange(zones)


#removing unncessary dataframes
rm(inconsistent_region)
rm(inconsistent_zones)



print(mal_2017_2024, max=Inf)
unique(mal_2017_2024$zone)
unique(dat_1082$zone)

#saving the region and zone level cleaned 2017-2024 dataset
saveRDS(mal_2017_2024, file = "C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/processed/mal_17_24_cleaned.rds")

#saving the csv form
write.csv(mal_2017_2024, "C:/Users/edemssie/OneDrive - PATH/Desktop/R projects/data cleaning practice/data/processed/mal_17_24_cleaned.csv", row.names = FALSE)