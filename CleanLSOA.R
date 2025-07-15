library(tidyverse)

# Define Yorkshire districts
yorkshire_districts <- list(
  "SOUTH YORKSHIRE" = c("SHEFFIELD", "BARNSLEY", "DONCASTER", "ROTHERHAM"),
  "WEST YORKSHIRE" = c("LEEDS", "BRADFORD", "CALDERDALE", "KIRKLEES", "WAKEFIELD")
)
districts <- unlist(yorkshire_districts)

# Load and clean Postcode to LSOA dataset
postcode_lsoa <- read_csv("Obtained Data/Postcode to LSOA 1.csv") %>%
  mutate(Postcode = toupper(gsub(" ", "", pcds)),
         ladnm = toupper(ladnm)) %>%
  filter(ladnm %in% districts) %>%
  mutate(County = case_when(
    ladnm %in% yorkshire_districts$`SOUTH YORKSHIRE` ~ "SOUTH YORKSHIRE",
    ladnm %in% yorkshire_districts$`WEST YORKSHIRE` ~ "WEST YORKSHIRE",
    TRUE ~ NA_character_
  )) %>%
  select(Postcode, District = ladnm, LSOA = lsoa11nm, County) %>%
  distinct()

# Save cleaned dataset
write_csv(postcode_lsoa, "Cleaned Data/cleanPostcodeLSOA.csv")
