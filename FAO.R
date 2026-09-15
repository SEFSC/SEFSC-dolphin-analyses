

library(tidyverse)
library(readr)

# 1. Download official FAO Global Capture dataset (Zip file)
data_url <- "https://www.fao.org/fishery/static/Data/Capture_2024.1.0.zip"
temp_zip  <- tempfile(fileext = ".zip")
temp_dir  <- tempdir()

download.file(data_url, destfile = temp_zip, mode = "wb")
unzip(temp_zip, exdir = temp_dir)

# 2. Load capture quantities and metadata code lists
capture_qty <- read_csv(file.path(temp_dir, "Capture_Quantity.csv"), show_col_types = FALSE)
species_cl  <- read_csv(file.path(temp_dir, "CL_FI_SPECIES_GROUPS.csv"), show_col_types = FALSE)
area_cl     <- read_csv(file.path(temp_dir, "CL_FI_WATERAREA_GROUPS.csv"), show_col_types = FALSE)

# 3. Filter code lists for Target Species and Area 31
# Area 31 = Western Central Atlantic
area_31_codes <- area_cl %>%
  filter(Code == 31 | Identifier == 31) %>%
  pull(Code)

# DOL = Common Dolphinfish (Mahi-mahi), DOQ = Pompano Dolphinfish
target_species <- species_cl %>%
  filter(
    `3ALPHA_CODE` %in% c("DOL", "DOQ") |
      str_detect(Scientific_Name, regex("Coryphaena|Delphinidae", ignore_case = TRUE))
  )

# 4. Filter the capture data
dolphin_catches <- capture_qty %>%
  filter(
    AREA_CODE %in% area_31_codes,
    SPECIES_CODE %in% target_species$`3ALPHA_CODE`
  ) %>%
  left_join(
    select(target_species, `3ALPHA_CODE`, Scientific_Name, Name_En),
    by = c("SPECIES_CODE" = "3ALPHA_CODE")
  )

# 5. Summarize catches by year, species, and reporting country
catch_summary <- dolphin_catches %>%
  group_by(PERIOD, Name_En, Scientific_Name, COUNTRY_CODE) %>%
  summarise(Total_Tonnes = sum(VALUE, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(PERIOD), desc(Total_Tonnes))

# Inspect first 15 rows
print(head(catch_summary, 15))

# Export filtered data
write_csv(dolphin_catches, "fao_area31_dolphin_catches.csv")