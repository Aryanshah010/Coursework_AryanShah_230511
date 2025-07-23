library(tidyverse)

# Define paths and local authorities
data_path <- "Obtained Data/"
cleaned_path <- "Cleaned Data/"
south_yorks <- c("Sheffield", "Doncaster", "Rotherham", "Barnsley")
west_yorks <- c("Leeds", "Bradford", "Wakefield", "Kirklees", "Calderdale")
yorkshire_las <- c(south_yorks, west_yorks)

# Load raw Postcode to LSOA dataset
postcode_lsoa <- read_csv("Obtained Data/Postcode to LSOA 1.csv") %>%
  mutate(
    Postcode = toupper(gsub(" ", "", pcds)),  # Standardize full postcode
    LSOA = lsoa11cd,  # Use LSOA code (e.g., E01012345)
    District = toupper(ladnm),
    County = case_when(
      str_detect(toupper(ladnm), "SHEFFIELD|BARNSLEY|DONCASTER|ROTHERHAM") ~ "South Yorkshire",
      str_detect(toupper(ladnm), "LEEDS|BRADFORD|CALDERDALE|KIRKLEES|WAKEFIELD") ~ "West Yorkshire",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(County)) %>%  # Restrict to South/West Yorkshire
  select(Postcode, LSOA, District, County) %>%
  distinct()

# Debugging: Check postcode_lsoa
cat("Rows in postcode_lsoa:", nrow(postcode_lsoa), "\n")
cat("Unique districts in postcode_lsoa:", unique(postcode_lsoa$District), "\n")
cat("Sample LSOA in postcode_lsoa:", head(postcode_lsoa$LSOA, 10), "\n")

# Function to clean a single year's ks4final data
clean_ks4final <- function(year, ks4_file, school_info_file) {
  # Load only required columns
  ks4 <- read_csv(ks4_file, 
                  col_select = c("URN", "SCHNAME", "ATT8SCR", "LEA", "PCODE"),
                  guess_max = 20000, 
                  show_col_types = FALSE)
  
  school_info <- read_csv(school_info_file, 
                          col_select = c("URN", "SCHNAME", "LANAME", "POSTCODE"),
                          guess_max = 20000, 
                          show_col_types = FALSE)
  
  # Log parsing problems
  if (nrow(problems(ks4)) > 0) {
    cat("Parsing issues in", ks4_file, ":\n")
    print(problems(ks4))
    write_csv(problems(ks4), paste0(cleaned_path, "problems_ks4_", year, ".csv"))
  }
  if (nrow(problems(school_info)) > 0) {
    cat("Parsing issues in", school_info_file, ":\n")
    print(problems(school_info))
    write_csv(problems(school_info), paste0(cleaned_path, "problems_school_info_", year, ".csv"))
  }
  
  # Check for required columns
  required_cols <- c("URN", "SCHNAME", "LANAME", "POSTCODE")
  missing_cols <- setdiff(required_cols, colnames(school_info))
  if (length(missing_cols) > 0) {
    cat("Missing columns in", school_info_file, ":", missing_cols, "\n")
    stop("Required columns not found.")
  }
  
  required_ks4_cols <- c("URN", "SCHNAME", "ATT8SCR")
  missing_ks4_cols <- setdiff(required_ks4_cols, colnames(ks4))
  if (length(missing_ks4_cols) > 0) {
    cat("Missing columns in", ks4_file, ":", missing_ks4_cols, "\n")
    stop("Required columns not found.")
  }
  
  # Debugging: Check LANAME values
  cat("Unique LANAME values in", school_info_file, ":", unique(str_trim(school_info$LANAME)), "\n")
  
  # Log non-numeric ATT8SCR values
  non_numeric_att8 <- ks4 %>%
    filter(!is.na(ATT8SCR) & !grepl("^[0-9.]+$", ATT8SCR)) %>%
    select(URN, SCHNAME, ATT8SCR)
  if (nrow(non_numeric_att8) > 0) {
    cat("Non-numeric ATT8SCR values in", ks4_file, ":\n")
    print(non_numeric_att8)
    write_csv(non_numeric_att8, paste0(cleaned_path, "non_numeric_att8_", year, ".csv"))
  }
  
  # Standardize and select relevant columns from school_info
  school_info_yorkshire <- school_info %>%
    rename(SchoolName = SCHNAME,
           LocalAuthority = LANAME,
           Postcode = POSTCODE) %>%
    mutate(LocalAuthority = str_trim(toupper(LocalAuthority))) %>%  # Trim whitespace and standardize case
    filter(LocalAuthority %in% toupper(yorkshire_las)) %>%  # Exact match for simplicity
    mutate(Postcode = toupper(gsub(" ", "", Postcode))) %>%  # Clean Postcode
    select(URN, SchoolName, LocalAuthority, Postcode) %>%
    mutate(County = case_when(
      str_detect(LocalAuthority, "SHEFFIELD|BARNSLEY|DONCASTER|ROTHERHAM") ~ "South Yorkshire",
      str_detect(LocalAuthority, "LEEDS|BRADFORD|CALDERDALE|KIRKLEES|WAKEFIELD") ~ "West Yorkshire",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(County))
  
  # Debugging: Check school_info_yorkshire
  cat("Rows in school_info_yorkshire for", year, ":", nrow(school_info_yorkshire), "\n")
  cat("Unique LocalAuthority in school_info_yorkshire:", unique(school_info_yorkshire$LocalAuthority), "\n")
  
  # Standardize and select relevant columns from ks4
  ks4_yorkshire <- ks4 %>%
    rename(SchoolName_ks4 = SCHNAME,
           Attainment8 = ATT8SCR,
           Postcode_ks4 = PCODE) %>%
    mutate(Postcode_ks4 = toupper(gsub(" ", "", Postcode_ks4))) %>%  # Clean Postcode
    semi_join(school_info_yorkshire, by = c("URN" = "URN")) %>%
    left_join(school_info_yorkshire, by = "URN") %>%
    mutate(SchoolName = coalesce(SchoolName, SchoolName_ks4),
           Year = year) %>%
    select(URN, SchoolName, LocalAuthority, County, Postcode, Attainment8, Year)
  
  # Remove NA in critical columns
  ks4_yorkshire <- ks4_yorkshire %>%
    filter(!is.na(Attainment8), !is.na(URN), !is.na(LocalAuthority))
  
  # Standardize formats and clean Postcode
  ks4_yorkshire <- ks4_yorkshire %>%
    mutate(Attainment8 = as.numeric(Attainment8),
           LocalAuthority = as.factor(LocalAuthority),
           County = as.factor(County),
           Year = as.character(Year),
           Postcode = as.character(Postcode))
  
  # Remove outliers (Attainment 8 scores beyond reasonable range, 0–100)
  ks4_yorkshire <- ks4_yorkshire %>%
    filter(Attainment8 >= 0 & Attainment8 <= 100)
  
  # Log unmatched postcodes
  unmatched_postcodes <- ks4_yorkshire %>%
    anti_join(postcode_lsoa, by = "Postcode") %>%
    select(URN, SchoolName, Postcode, LocalAuthority)
  if (nrow(unmatched_postcodes) > 0) {
    cat("Unmatched postcodes in", ks4_file, ":\n")
    print(unmatched_postcodes)
    write_csv(unmatched_postcodes, paste0(cleaned_path, "unmatched_postcodes_", year, ".csv"))
  }
  
  # Join with postcode_lsoa to add LSOA and District
  ks4_yorkshire <- ks4_yorkshire %>%
    left_join(postcode_lsoa %>% select(Postcode, LSOA, District), by = "Postcode") %>%
    filter(!is.na(LSOA), !is.na(District))  # Keep only valid LSOA and District
  
  # Debugging: Check ks4_yorkshire
  cat("Rows in ks4_yorkshire for", year, ":", nrow(ks4_yorkshire), "\n")
  cat("Unique districts in ks4_yorkshire:", unique(ks4_yorkshire$District), "\n")
  cat("Sample LSOA in ks4_yorkshire:", head(ks4_yorkshire$LSOA, 5), "\n")
  
  # Save cleaned file
  output_file <- paste0(cleaned_path, "cleaned_ks4final_", year, ".csv")
  write_csv(ks4_yorkshire, output_file)
  
  return(ks4_yorkshire)
}

# Process each year
ks4_2021 <- clean_ks4final(
  year = "2021-2022",
  ks4_file = paste0(data_path, "2021-2022/england_ks4final.csv"),
  school_info_file = paste0(data_path, "2021-2022/england_school_information.csv")
)

ks4_2022 <- clean_ks4final(
  year = "2022-2023",
  ks4_file = paste0(data_path, "2022-2023/england_ks4final.csv"),
  school_info_file = paste0(data_path, "2022-2023/england_school_information.csv")
)

ks4_2023 <- clean_ks4final(
  year = "2023-2024",
  ks4_file = paste0(data_path, "2023-2024/england_ks4final.csv"),
  school_info_file = paste0(data_path, "2023-2024/england_school_information.csv")
)

# Merge all years
ks4_all_years <- bind_rows(ks4_2021, ks4_2022, ks4_2023)

# Save merged file
write_csv(ks4_all_years, paste0(cleaned_path, "cleaned_ks4final_all_years.csv"))

# Summary of cleaned data
cat("Cleaning Summary:\n")
cat("2021-2022 rows:", nrow(ks4_2021), "\n")
cat("2022-2023 rows:", nrow(ks4_2022), "\n")
cat("2023-2024 rows:", nrow(ks4_2023), "\n")
cat("All years rows:", nrow(ks4_all_years), "\n")

