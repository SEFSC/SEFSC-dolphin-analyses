
library(tidyverse)
library(readr)
library(pals)

# 1. Download official FAO Global Capture production zip file
data_url <- "https://www.fao.org/fishery/static/Data/Capture_2024.1.0.zip"
temp_zip  <- tempfile(fileext = ".zip")
temp_dir  <- tempdir()

download.file(data_url, destfile = temp_zip, mode = "wb")
unzip(temp_zip, exdir = temp_dir)

# 2. Read raw datasets
capture_qty <- read_csv(file.path(temp_dir, "Capture_Quantity.csv"), show_col_types = FALSE)
species_cl  <- read_csv(file.path(temp_dir, "CL_FI_SPECIES_GROUPS.csv"), show_col_types = FALSE)
country_cl  <- read_csv(file.path(temp_dir, "CL_FI_COUNTRY_GROUPS.csv"), show_col_types = FALSE)

# Convert all column names to uppercase
colnames(capture_qty) <- toupper(colnames(capture_qty))
colnames(species_cl)  <- toupper(colnames(species_cl))
colnames(country_cl)  <- toupper(colnames(country_cl))

# 3. Rename columns using Base R pattern matching (prevents Tidyverse evaluation errors)
colnames(capture_qty)[grep("AREA|WATERAREA", colnames(capture_qty))[1]] <- "AREA"
colnames(capture_qty)[grep("SPECIES|3ALPHA", colnames(capture_qty))[1]] <- "SPECIES"
colnames(capture_qty)[grep("COUNTRY|UN_CODE", colnames(capture_qty))[1]] <- "COUNTRY"
colnames(capture_qty)[grep("PERIOD|YEAR", colnames(capture_qty))[1]]    <- "YEAR"
colnames(capture_qty)[grep("VALUE|QUANTITY|CATCH", colnames(capture_qty))[1]] <- "VALUE"

colnames(species_cl)[grep("3ALPHA|SPECIES|CODE", colnames(species_cl))[1]] <- "SPECIES"
colnames(species_cl)[grep("SCIENTIFIC", colnames(species_cl))[1]]          <- "SCIENTIFIC_NAME"
colnames(species_cl)[grep("NAME_EN|ENGLISH", colnames(species_cl))[1]]     <- "SPECIES_EN"

colnames(country_cl)[grep("UN_CODE|CODE|IDENTIFIER", colnames(country_cl))[1]] <- "COUNTRY"
colnames(country_cl)[grep("NAME_EN|NAME|COUNTRY_EN", colnames(country_cl))[1]] <- "JURISDICTION"

# 4. Filter species metadata for Coryphaena hippurus (DOL)
target_species <- species_cl %>%
  filter(SPECIES == "DOL" | str_detect(SCIENTIFIC_NAME, regex("^Coryphaena hippurus$", ignore_case = TRUE)))

# 5. Filter landings data for FAO Areas 21 & 31 and join names
mahi_landings <- capture_qty %>%
  filter(
    AREA %in% c(21, 31),
    SPECIES == "DOL"
  ) %>%
  left_join(select(target_species, SPECIES, SCIENTIFIC_NAME, SPECIES_EN), by = "SPECIES") %>%
  left_join(select(country_cl, COUNTRY, JURISDICTION), by = "COUNTRY")

# 6. Aggregate landings by Jurisdiction, Area, and Year
landings_by_jurisdiction <- mahi_landings %>%
  group_by(JURISDICTION, AREA, SCIENTIFIC_NAME, YEAR) %>%
  summarise(
    Landings_Tonnes = sum(as.numeric(VALUE), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(YEAR), AREA, JURISDICTION)

# Preview results
head(landings_by_jurisdiction, 20)

df <- as.data.frame(landings_by_jurisdiction)
apply(df, 2, table)

df$lbs <- df$Landings_Tonnes * 2204.62

df_recent <- df[which(df$YEAR >= 2015), ]

par(mar = c(4, 15, 2, 1))
barplot(sort(tapply(df$lbs/10^6, df$JURISDICTION, mean, na.rm = T)), las = 1, horiz = T, 
        xlab = "millions of pounds", main = "average annual landings (FAO 1950 - 2022)")
barplot(sort(tapply(df_recent$lbs/10^6, df_recent$JURISDICTION, mean, na.rm = T)), las = 1, horiz = T, 
        xlab = "millions of pounds", main = "average annual landings (FAO 1950 - 2022)")

df_int <- df[which(df$JURISDICTION != "United States of America"), ]

dev.off()
tot <- tapply(df_int$lbs, df_int$YEAR, sum, na.rm = T)
plot(names(tot), tot/10^6, type = "l")


lis <- names(sort(tapply(df_int$lbs/10^6, df_int$JURISDICTION, mean, na.rm = T), decreasing = T))

df_int$JURISDICTION <- factor(df_int$JURISDICTION, levels = lis)

dat <- tapply(df_int$lbs, list(df_int$JURISDICTION, df_int$YEAR), sum, na.rm = T)
dat[is.na(dat)] <- 0
dat
cols <- glasbey(26)

b <- barplot(dat/10^6, names.arg = 1950:2022, col = cols, border = NA, las = 2, 
        ylab = "millions of pounds", main = "FAO reported landings by jurisdiction")
legend("topleft", ncol = 2, rownames(dat), col = cols, pch = 15, bty = "n")
abline(h = 0)

d <- read.csv("data/FINAL_files/complete_catch_dataset_09052025b.csv")
d <- d[which(d$Fleet == "Intl"), ]
dtot <- tapply(d$Catch_lbs, d$Year, sum, na.rm = T)

xs <- b[which(1950:2022 >= 1986)]

lines(xs, dtot/10^6, lty = 1, col = "#00000080", lwd = 3)
legend(0.5, 8, "total landings estimated by SAU", lwd = 3, lty = 1, col = "#00000080", bty = "n")

