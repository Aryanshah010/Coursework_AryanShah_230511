library(tidyverse)
library(stringr)

ks4_2022<-read_csv("Obtained Data/2021-2022/england_ks4final.csv")
school_info_2022<-read_csv("Obtained Data/2021-2022/england_school_information.csv")
Towns<-read_csv("Cleaned Data/TOWNS.csv")

cleanSchoolInfo2022<-school_info_2022 %>% 
  mutate(
    District=LANAME,
    shortPostcode=toupper(str_trim(substr(POSTCODE,1,4))),
    Town=TOWN
    ) %>% 
  select(URN,POSTCODE,shortPostcode,District,Town)

cleanTown<-Towns %>% 
  mutate(shortPostcode=toupper(str_trim(shortPostcode)))

county_cleanSchoolInfo2022<-cleanSchoolInfo2022 %>% 
  left_join(
    cleanTown %>% select(shortPostcode,County),
    by="shortPostcode"
  ) %>% 
  drop_na(County)

ks4final<-ks4_2022 %>% 
  mutate(
    year=2022,
    attainment_8_score=ATT8SCR,
    school_name=SCHNAME
  ) %>% 
  select(URN,year,attainment_8_score,school_name) %>% 
  filter(!is.na(URN), !is.na(attainment_8_score))

combined2022<-ks4final %>% 
  left_join(county_cleanSchoolInfo2022, by="URN") %>% 
  drop_na(County,District,attainment_8_score,Town)

ks4_2023<-read_csv("Obtained Data/2022-2023/england_ks4final.csv")
school_info_2023<-read_csv("Obtained Data/2022-2023/england_school_information.csv")

cleanSchoolInfo2023<-school_info_2023 %>% 
  mutate(
    District=LANAME,
    shortPostcode=toupper(str_trim(substr(POSTCODE,1,4))),
    Town=TOWN
  ) %>% 
  select(URN,POSTCODE,shortPostcode,District,Town)

cleanTown<-Towns %>% 
  mutate(shortPostcode=toupper(str_trim(shortPostcode)))

county_cleanSchoolInfo2023<-cleanSchoolInfo2023 %>% 
  left_join(
    cleanTown %>% select(shortPostcode,County),
    by="shortPostcode"
  ) %>% 
  drop_na(County)

ks4final<-ks4_2023 %>% 
  mutate(
    year=2023,
    attainment_8_score=ATT8SCR,
    school_name=SCHNAME
  ) %>% 
  select(URN,year,attainment_8_score,school_name) %>% 
  filter(!is.na(URN), !is.na(attainment_8_score))


combined2023<-ks4final %>% 
  left_join(county_cleanSchoolInfo2023, by="URN") %>% 
  drop_na(County,District,attainment_8_score,Town)

ks4_2024<-read_csv("Obtained Data/2023-2024/england_ks4final.csv")
school_info_2024<-read_csv("Obtained Data/2023-2024/england_school_information.csv")

cleanSchoolInfo2024<-school_info_2024 %>% 
  mutate(
    District=LANAME,
    shortPostcode=toupper(str_trim(substr(POSTCODE,1,4))),
    Town=TOWN
  ) %>% 
  select(URN,POSTCODE,shortPostcode,District,Town)

cleanTown<-Towns %>% 
  mutate(shortPostcode=toupper(str_trim(shortPostcode)))

county_cleanSchoolInfo2024<-cleanSchoolInfo2024 %>% 
  left_join(
    cleanTown %>% select(shortPostcode,County),
    by="shortPostcode"
  ) %>% 
  drop_na(County)

ks4final<-ks4_2024 %>% 
  mutate(
    year=2024,
    attainment_8_score=ATT8SCR,
    school_name=SCHNAME
  ) %>% 
  select(URN,year,attainment_8_score,school_name) %>% 
  filter(!is.na(URN), !is.na(attainment_8_score))


combined2024<-ks4final %>% 
  left_join(county_cleanSchoolInfo2024, by="URN") %>% 
  drop_na(County,District,attainment_8_score,Town)

combinedSchool=bind_rows(combined2022,combined2023,combined2024)

# Clean the combinedSchool data by removing non-numeric values in attainment_8_score
combinedSchool_cleaned <- combinedSchool %>%
  filter(!attainment_8_score %in% c("SUPP", "NE", "NA")) %>%  # Remove unwanted codes
  mutate(attainment_8_score = as.numeric(attainment_8_score)) %>%  # Convert to numeric
  drop_na(attainment_8_score)  # Remove any remaining NA values

write_csv(combinedSchool_cleaned,"Cleaned Data/cleanSchool.csv")

