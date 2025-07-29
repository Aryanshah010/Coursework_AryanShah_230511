library(tidyverse)
library(lubridate)

col_names=c("TransactionID","Price","Date","Postcode","PropertyType","OldNew","Duration",
          "PAON","SAON","Street","Locality","Town","District","County","CategoryType","RecordStatus")

HousePrices2021 = read_csv("Obtained Data/HousePrices2021.csv",col_names = FALSE)
colnames(HousePrices2021)=col_names

HousePrices2022 = read_csv("Obtained Data/HousePrices2022.csv",col_names = FALSE)
colnames(HousePrices2022)=col_names

HousePrices2023 = read_csv("Obtained Data/HousePrices2023.csv",col_names = FALSE)
colnames(HousePrices2023)=col_names

HousePrices2024 = read_csv("Obtained Data/HousePrices2024.csv",col_names = FALSE)
colnames(HousePrices2024)=col_names

HousePrices =bind_rows(HousePrices2021,HousePrices2022,HousePrices2023,HousePrices2024)

cleanHousePrices = HousePrices %>%
  filter(County=="SOUTH YORKSHIRE"|County=="WEST YORKSHIRE") %>% 
  mutate(
    Date=as.Date(Date),
    Year=year(Date),
    shortPostcode=str_trim(substr(Postcode,1,4)),
    District=str_to_upper(District),
    Town=str_to_upper(Town)
  ) %>% 
  select(Year,Price,Postcode,shortPostcode,CategoryType,Town,District,County)


write.csv(cleanHousePrices, "Cleaned Data/cleanHousePrices.csv") 
