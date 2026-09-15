
# clear workspace
rm(list = ls())

# load libraries
library(rvest)
library(dplyr)
library(readr)
library(purrr)

##############  data download  #################

# url for MRIP data
base_url <- "https://apps-st.fisheries.noaa.gov/st1/recreational/MRIP_Survey_Data/CSV/"

# Scrape all zip file links from the NOAA index page
page <- read_html(base_url)
zip_names <- page %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  .[grepl("\\.zip$", ., ignore.case = TRUE)] %>% 
  unique()

temp_dir <- tempdir()

# Function to download, extract, read, and filter 'size' files on-the-fly
process_size_file <- function(zip_name) {
  zip_url <- paste0(base_url, zip_name)
  temp_zip <- tempfile(fileext = ".zip")
  
  message("Processing: ", zip_name)
  download.file(zip_url, temp_zip, mode = "wb", quiet = TRUE)
  
  # List contents inside the zip
  zipped_contents <- unzip(temp_zip, list = TRUE)$Name
  size_files <- zipped_contents[grepl("^size_.*\\.csv$", zipped_contents, ignore.case = TRUE)]
  
  if (length(size_files) == 0) {
    unlink(temp_zip)
    return(NULL)
  }
  
  # Unzip size files to temporary directory
  unzip(temp_zip, files = size_files, exdir = temp_dir)
  unlink(temp_zip) # Remove zip immediately to free disk space
  
  # Process each extracted size CSV file
  size_dfs <- map(size_files, function(sf) {
    file_path <- file.path(temp_dir, sf)
    
  # Read raw file with light column typing
    df <- read_csv(
      file_path, 
      show_col_types = FALSE, 
      col_types = cols(.default = col_character()) # Read all as character initially to prevent type mismatches across years
    )
    
  # Standardize column names to lowercase
    colnames(df) <- tolower(colnames(df))
    
  # Filter for non-imputed rows directly on loading
    if ("lngth_imp" %in% colnames(df)) {
      # Keep rows where lngth_imp is '0' (directly measured) or NA/empty in legacy datasets
      df <- df %>% 
        filter(lngth_imp == "0" | is.na(lngth_imp))
    }
    
    # Clean up the extracted file from disk after loading into RAM
    file.remove(file_path)
    
    return(df)
  })
  
  # Combine dataframes from this specific zip file
  bind_rows(size_dfs)
}

# Process all zip files and bind them into a single clean data frame
combined_size_df <- map(zip_names, process_size_file) %>% 
  compact() %>% 
  bind_rows()

# Cast key numeric columns back to numbers to save memory
combined_size_df <- combined_size_df %>% 
  mutate(
    lngth = as.numeric(lngth),
    wgt = as.numeric(wgt),
    year = as.integer(year),
    wave = as.integer(wave)
  )

message("Done! Successfully concatenated ", nrow(combined_size_df), " rows of measured (non-imputed) size data.")

# Optional: Export combined dataset to CSV or parquet
# write_csv(combined_size_df, "mrip_size_non_imputed.csv")

##############  data analysis  #################

rm(list = ls())
d <- read.csv("mrip_size_non_imputed.csv")

head(d)
table(d$common)

# subset dolphin only
d1 <- d[which(d$common == "DOLPHIN"), ]
dim(d)
dim(d1)

apply(d1[, 3:9], 2, table)
apply(d1[, 20:23], 2, table)

# Subset subregion - code for region of trip
# 4   = North Atlantic (ME; NH; MA; RI; CT) 
# 5   = Mid-Atlantic (NY; NJ; DE; MD; VA) 
# 6   = South Atlantic (NC; SC; GA; EFL) 
# 7   = Gulf of America (WFL; AL; MS; LA) 
d1 <- d1[which(d1$sub_reg == 4 | d1$sub_reg == 5 | d1$sub_reg == 6), ]

# state FIPS codes 
fl_to_sc <- c(12, 13, 45)
nc_to_me <- c(37, 51, 24, 10, 34, 36, 9, 44, 25, 33, 23)

d1$reg <- NA
d1$reg[which(d1$st %in% fl_to_sc)] <- "S"
d1$reg[which(d1$st %in% nc_to_me)] <- "N"

# Subset by collapsed fishing mode code 
# 1=Man-Made
# 2=Beach/Bank
# 3=Shore
# 4=Headboat
# 5=Charter Boat (sub_reg=6 or 7 & mode_f=7)
# 7=Private/Rental Boat
d1$sect <- NA
d1$sect[which(d1$mode_fx == 7)] <- "Rec"
d1$sect[which(d1$mode_fx == 4 | d1$mode_fx == 5 | d1$mode_fx == 6)] <- "Hire"  

table(d1$reg, d1$sect, useNA = "always")

# remove NAs
d1 <- d1[!is.na(d1$sect), ]

# combine sector and region into 4 groups 
d1$cat <- paste0(d1$sect, d1$reg)
unique(d1$cat)

lis <- c("HireN", "RecN", "HireS", "RecS")

# plot histograms
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), mgp = c(2.3, 1, 0))

for (i in lis) { 
  d2 <- d1[which(d1$cat == i), ]
  hist(d2$lngth, breaks = seq(100, 1750, 50), main = i, 
       xlab = "fork length (mm)")
  axis(1, at = seq(0, 2000, 500))
abline(v = 500, col = 2, lwd = 1.5)
mtext(side = 3, line = -1, paste("           mean =", round(mean(d2$lngth, na.rm = T), 1)))
    }

table(d1$reg, d1$sect)

#d3 <- d1[which(d1$sect == "Hire"), ]

# look at how average size varies by state
fips <- c(12, 13, 45, 37, 51, 24, 10, 34, 36, 9, 44, 25)
d1$st2 <- factor(d1$st, levels = fips)

stls <- c("Florida", "Georgia", "South Carolina", "North Carolina",
  "Virginia", "Maryland", "Delaware", "New Jersey", "New York", "Connecticut",
  "Rhode Island", "Massachusetts")

dev.off()

par(mar = c(7, 4, 1, 1))
plot(d1$lngth ~ d1$st2, axes = F, xlab = "", 
     ylab = "fork length (mm)")
axis(1, at = 1:12, lab = stls, las = 2)
axis(2, las = 2)
box()
abline(h = 508, col = 2)

# compile data fields for output
fin <- data.frame(d1$lngth, d1$wgt, d1$cat, d1$reg, d1$st, d1$year, d1$month)
names(fin) <- c("length", "weight", "group", "region", "state_FIPS", "year", "month")

apply(fin, 2, table, useNA = "always")
tapply(fin$length, fin$group, mean, na.rm = T)
fin <- fin[order(fin$group), ]

table(is.na(fin$length))
fin <- fin[!is.na(fin$length), ]

# output length comps for each region
#write.csv(fin, file = "MRIP_length_comps.csv", row.names = FALSE)


# check extraction was done correctly comparing to a single data file

d4 <- d[which(d$common == "DOLPHIN"), ]
d4 <- d4[which(d4$year == 2024), ]
d4 <- d4[which(d4$wave == 1), ]
dim(d4)

d5 <- read.csv("../../Desktop/size_20241.csv")
d5 <- d5[which(d5$COMMON == "DOLPHIN"), ]
d5 <- d5[which(d5$LNGTH_IMP == 0), ]
dim(d5)

cbind(d4$lngth, d5$LNGTH)
cor(d4$lngth, d5$LNGTH)
