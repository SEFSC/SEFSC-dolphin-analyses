
# clear workspace
rm(list = ls())

# load libraries 
library(maps)
library(emmeans)
library(marmap)
library(dplyr)
library(readr)
library(sp)
library(maps) 


# load pelagic longline data ------------------------------------------
# data request from June 23, 2023 - sent to M. Damiano by S. Alhale 
# file is "damiano_request623.xlsx" and folder is "Matt Damiano PLL request" on Google Drive

dat <- read.csv("C://Users/mandy.karnauskas/Desktop/CONFIDENTIAL/PLL_1986_2022_catch_effort.csv")

head(dat)
dim(dat)

table(dat$TDOL, useNA = "always")  # target dolphin?    - ~8% targeted dolphin trips
table(dat$PLL, useNA = "always")   # PLL gear used Y/N  - ~91% PLL trips

# format latitude and longitude -----------------------------------
dat$lat <- dat$LATDEG + dat$LATMIN/60
dat$lon <- -(dat$LONDEG + dat$LONMIN/60)

# check location of points
#map("world")
#points(d$lon, d$lat, col = 2, pch = 19, cex = 0.4)

# load area polygons and subset PLL data points by area
ar <- read.csv("data/eez/dolphin_OM_areas.csv")

dat$area <- NA
table(ar$region)
lis <- unique(ar$region)

# for each area, find the PLL points that fall in the polygon and label the area
for (i in lis) {
  pol <- ar[which(ar$region == i), 1:2]
  pts <- point.in.polygon(dat$lon, dat$lat, pol.x = pol$X, pol.y = pol$Y)
  dat$area[which(pts == 1)] <- i
}

# check that subsetting was done correctly - color-coded areas
table(dat$area, useNA = "always")
map("world", xlim = c(-100, -30), ylim = c(5, 55))
axis(1); axis(2); box()
points(dat$lon, dat$lat, pch = 19, cex = 0.4, col = as.numeric(as.factor(dat$area))+1)

dim(dat)
table(dat$area, useNA = "always")

# subset out areas not in domain -----------------------------
d <- dat[!is.na(dat$area), ]
dim(d)

d$arnew <- as.character(d$area)
d$arnew[which(d$area == "CAR")] <- "CAR+FLK"
d$arnew[which(d$area == "FLK")] <- "CAR+FLK"
d$arnew[which(d$area == "NNC")] <- "NNC+VBM"
d$arnew[which(d$area == "VBM")] <- "NNC+VBM"
table(d$arnew, useNA = "always")

# get depths for each lat/lon -----------------------------------------
# Define the geographic bounding box for your dataset (with a small buffer)
min_lon <- min(d$lon) - 1
max_lon <- max(d$lon) + 1
min_lat <- min(d$lat) - 1
max_lat <- max(d$lat) + 1

# Download NOAA ETOPO bathymetry data for the region (res = 1 minute grid)
# Note: marmap caches the downloaded file locally so it only downloads once!
atlantic_bath <- getNOAA.bathy(lon1 = min_lon, lon2 = max_lon, 
                               lat1 = min_lat, lat2 = max_lat, 
                               resolution = 1)

# Extract depths for your coordinate points
# get.depth extracts values via linear interpolation or nearest grid point
extracted_depths <- get.depth(atlantic_bath, x = d[, c("lon", "lat")], locator = FALSE)
dim(extracted_depths)
dim(d)

# Append depths to your original data frame
# Bathymetry is returned as negative elevation; abs() converts to positive depth in meters
d$depth <- abs(extracted_depths$depth)

# check depth extraction
depth_palette <- colorRampPalette(c("cyan", "blue", "navy"))(100)

# Normalize depths to indices (1 to 100) for color assignment
depth_normalized <- cut(d$depth, breaks = 100, labels = FALSE)
point_colors <- depth_palette[depth_normalized]

# Set up map plot bounds with a small buffer around your points
x_range <- c(min(d$lon) - 2, max(d$lon) + 2)
y_range <- c(min(d$lat) - 2, max(d$lat) + 2)

# Draw base map
map("world", fill = TRUE, col = "grey85", bg = "aliceblue",
    xlim = x_range, ylim = y_range, 
    xlab = "Longitude", ylab = "Latitude", main = "Extracted Bathymetry Point Validation")

# 4. Add point locations with assigned depth colors
points(d$lon, d$lat, col = point_colors, pch = 15, cex = 0.7)

# 5. Add a simple legend
legend(-90, 45, legend = c(paste(round(min(d$depth)), "m (Shallow)"),
                  paste(round(max(d$depth)), "m (Deep)")),
       col = c(depth_palette[1], depth_palette[100]), 
       pch = 19, bg = "white", title = "Depth")

# Save dataframe to start fresh here ---------------------------------------
#write_csv(d, "C://Users/mandy.karnauskas/Desktop/CONFIDENTIAL/PLL_1986_2022_rawdata.csv")




## start here with PLL data including depth and area fields -------------------------

rm(list = ls())
d <- read.csv("C://Users/mandy.karnauskas/Desktop/CONFIDENTIAL/PLL_1986_2022_rawdata.csv")

# number of observations by year 
table(d$SET_YEAR, useNA = "always")
barplot(table(d$SET_YEAR, useNA = "always"), las = 2)

# look at dolphin catch within PLL data -----------------------------
names(d)[grep("DOL", names(d))]

table(d$TDOL, useNA = "always")  # dolphin targeted trips
table(d$DOLK, useNA = "always")  # number of dolphin kept
table(d$DOLA, useNA = "always")  # number of dolphin discarded alive
table(d$DOLD, useNA = "always")  # number of dolphin discarded dead

dev.off()
par(mfrow = c(2, 2), mex = 0.7)
hist(d$DOLK, main = "# dolphin kept")
hist(d$DOLA, main = "# dolphin discarded alive")
hist(d$DOLD, main = "# dolphin discarded dead")
hist(d$DOLPHIN_POUNDS, main = "pounds of dolphin kept")

# replace NAs with zeros
d$DOLA[is.na(d$DOLA)] <- 0
d$DOLK[is.na(d$DOLK)] <- 0
d$DOLD[is.na(d$DOLD)] <- 0
d$DOLPHIN_POUNDS[is.na(d$DOLPHIN_POUNDS)] <- 0

# calculate total dolphin kept or discarded -----------------------
d$DOLTOT <- d$DOLK + d$DOLA + d$DOLD
hist(d$DOLTOT, main = "# dolphin caught")   # total dolphin kept or discarded

# remove zero hook data to avoid infinite CPUE
d <- d[-which(d$HOOKS == 0), ]
# remove sets where there are more fish than hooks
d <- d[-which(d$DOLTOT > d$HOOKS), ]

# calculate CPUE ------------------------------------------------
#d$cpue <- d$DOLPHIN_POUNDS / d$HOOKS

d$cpue <- d$DOLTOT / (d$HOOKS / 1000)
max(d$cpue, na.rm = T)
hist(d$cpue)
quantile(d$cpue, probs = 0.99, na.rm = T)
hist(log(d$cpue))
d <- d[!is.na(d$cpue), ]
dim(d)

# make new date variables 
d$year <- as.numeric(substr(d$SDATE, 1, 4))
d$mon  <- as.numeric(substr(d$SDATE, 5, 6))
d$monold  <- d$mon
table(d$year, useNA = "always")
table(d$mon, useNA = "always")
d <- d[-which(d$mon == 0), ]
table(d$year == d$SET_YEAR)

# convert month December to following year 
d$mon[which(d$mon == 12)] <- 0.5
d$year[which(d$mon == 0.5)] <- d$SET_YEAR[which(d$mon == 0.5)] + 1
table(d$year, useNA = "always")
table(d$mon, useNA = "always")
table(as.numeric(substr(d$SDATE, 5, 6)), useNA = "always")

table(d$year, useNA = "always")
barplot(table(d$year, useNA = "always"), las = 2)

d$arnew <- factor(d$arnew, levels = c("CAR+FLK", "NCFL", "NNC+VBM", "NED", "NCA"))

# explore factors to include in model --------------------------

par(mfrow = c(5, 5))
for (i in 26:50) {
  f <- tapply(d$cpue, d[, i], mean, na.rm = T)
  barplot(f, las = 1, main = names(d[i]))
}

d <- d[which(d$year >= 1986 & d$year <= 2022), ]

par(mfrow = c(2, 2))
barplot(tapply(d$cpue, d$year, mean, na.rm = T), las = 2)
barplot(tapply(d$cpue, d$mon, mean, na.rm = T))
barplot(tapply(d$cpue, d$PLL, mean, na.rm = T))
barplot(tapply(d$cpue, d$TDOL, mean, na.rm = T))

# depth bins 
hist(d$depth)
quantile(d$depth, probs = seq(0, 1, 0.2))
d$depbin <- cut(d$depth, breaks = c(0, 400, 800, 1200, 3000, 10000))
table(d$depbin, useNA = "always")
barplot(tapply(d$cpue, d$depbin, mean, na.rm = T))

# look at numbers of observations by different factors 
apply(d[,26:52], 2, table, useNA = "always")

# prepare factors for standardization --------------------

# yearbins 
#d$yrbin <- cut(d$year, breaks = c(1984, 1994, 2004, 2014, 2024))
d$yrbin <- cut(d$year, breaks = seq(1984, 2024, 5))
table(d$yrbin, useNA = "always")

# year as factor
d$year <- as.factor(d$year)

# temperature bins by 5-degree increments
d$tempbin <- cut(d$TEMP, breaks = c(0, seq(70, 85, 5)))
table(d$tempbin, useNA = "always")

# season
d$seas <- cut(d$mon, breaks = c(0, 2.5, 5.5, 8.5, 12))
table(d$seas, useNA = "always")
d$seas <- as.character(d$seas)
d$season <- NA
d$season[which(d$seas == "(0,2.5]")]   <- "winter"
d$season[which(d$seas == "(2.5,5.5]")] <- "spring"
d$season[which(d$seas == "(5.5,8.5]")] <- "summer"
d$season[which(d$seas == "(8.5,12]")]  <- "fall"
d$season <- factor(d$season, levels = c("winter", "spring", "summer", "fall"))
table(d$season, useNA = "always")

# month as factor
d$mon <- as.factor(d$mon)
table(d$mon, useNA = "always")

# hooks between floats as binned factor
table(d$HBFL)
d$HBFL[which(d$HBFL > 10)] <- NA
d$HBFLbin <- cut(d$HBFL, breaks = c(0, 2, 3, 4, 5, 10))
table(d$HBFLbin, useNA = "always")

# little resolution in data for bait and lights/hooks (similar values for all obs)
table(d$BAIT)
hist(d$LIGHTS/d$HOOKS)

# look at targeting - left is change in CPUE, right is number of obs by category
par(mfrow = c(7, 2), mex = 0.5)
for (i in 32:38) {
  f <- tapply(d$cpue, d[, i], mean, na.rm = T)
  barplot(f, las = 1, main = names(d[i]))
  barplot(table(d[, i]), main = names(d[i]))
}
# only major targeting is swordfish and mixed

# change to zeros and ones
for (i in 32:38) {
  d[,i] <- d[,i] == "Y"
  d[,i] <- as.numeric(d[,i])
  d[,i] <- as.factor(d[, i])
}

# convert targeting to factors
d$TSWO <- as.factor(d$TSWO)
d$TMIX <- as.factor(d$TMIX)
d$TDOL <- as.factor(d$TDOL)

# standardize the CPUE -------------------------------------------

# make presence/absence variable 
table(d$cpue == 0, useNA = "always")                 # # of observations > 1
d$pres <- d$cpue
d$pres[d$pres > 0] <- 1
table(d$pres, useNA = "always")

# model the presence-absence as a binomial regression 
outp <- glm(pres ~ year + arnew*season  + tempbin + depbin + TDOL + TSWO + TMIX, data = d, family = "binomial")  #  + HBFLbin
summary(outp)        
a_table <- anova(outp, test = "Chisq")

total_deviance <- outp$null.deviance
factor_deviance <- a_table$Deviance  # Extract deviance explained by each factor
factors <- rownames(a_table)[!is.na(factor_deviance)]  # Filter out the first row (which is NA for the NULL model)
dev_values <- factor_deviance[!is.na(factor_deviance)]
residual_dev <- outp$deviance  # residual (unexplained) deviance
deviance_table <- data.frame(
  Factor = c(factors, "Residual Deviance"),
  `Deviance Explained` = c(dev_values, residual_dev),
  `Pct Deviance Explained (%)` = round(c(dev_values, residual_dev) / total_deviance * 100, 2),
  check.names = FALSE)
print(deviance_table)

overall_r2 <- (outp$null.deviance - outp$deviance) / outp$null.deviance * 100
cat("Total Deviance Explained by Model:", round(overall_r2, 2), "%\n")


# subset abundance when present data
dp <- d[d$cpue > 0, ]

# model the log abundance when presence
out <- glm(log(cpue) ~  year + arnew*season + depbin + TDOL + TSWO + TMIX, data = dp, family = "gaussian")  # + HBFLbin + tempbin 
summary(out)        
#anova(out) 
b_table <- anova(out, test = "Chisq")

total_deviance <- out$null.deviance
factor_deviance <- b_table$Deviance  # Extract deviance explained by each factor
factors <- rownames(b_table)[!is.na(factor_deviance)]  # Filter out the first row (which is NA for the NULL model)
dev_values <- factor_deviance[!is.na(factor_deviance)]
residual_dev <- out$deviance  # residual (unexplained) deviance
deviance_table <- data.frame(
  Factor = c(factors, "Residual Deviance"),
  `Deviance Explained` = c(dev_values, residual_dev),
  `Pct Deviance Explained (%)` = round(c(dev_values, residual_dev) / total_deviance * 100, 2),
  check.names = FALSE)
print(deviance_table)

overall_r2 <- (out$null.deviance - out$deviance) / out$null.deviance * 100
cat("Total Deviance Explained by Model:", round(overall_r2, 2), "%\n")

# combined variance function 
comb.var <- function(A, Ase, P, Pse, p) { (P^2 * Ase^2 + A^2 * Pse^2 + 2 * p * A * P * Ase * Pse)  }   # combined var



# yearly index --------------------------------
# calculate the least-squares means from the linear models
# emm_options(rg.limit = 300000)
# predlogit <- as.data.frame(emmeans(outp, specs = ~ year, type = "response"))
# predpos  <- as.data.frame(emmeans(out, specs = ~ year, type = "response"))
# table(predpos$year == predlogit$year)  # check that years are the same
# predlogit
# predpos
# 
# # calculate correlation between indices 
# co <- cor(predlogit$prob, predpos$response, method="pearson")
# co  
# # calculate the combined index and the combined SE
# predind <-  predlogit$prob * predpos$response    # estimated abundance is prob. of occurrence * estimated abundance when present
# predse <- sqrt(comb.var(predpos$response, predpos$SE, predlogit$prob, predlogit$SE, co))
# # year variable
# yrs <- as.numeric(as.vector(predpos$year))
# # calculate the nominal CPUE
# nom <- tapply(d$cpue, d$year, mean, na.rm = T)
# table(as.numeric(names(nom)) == yrs)
# 
# # output in data frame adn plot
# ind <- data.frame(cbind(yrs, predind, predse, predlogit$prob, predpos$response, nom))
# names(ind) <- c("Year", "index", "SE", "predpos", "Npres", "nominal")
# ind
# 
# dev.off()
# plot(ind$Year, ind$index, type = "l", lwd = 2, col = 4, main = "Nominal versus standardized CPUE from PLL data",
#      xlab = "year", ylab = "CPUE (dolphin / 1000 hooks)", ylim = c(0, 14))
# points(ind$Year, ind$index, pch = 1, lwd = 2, col = 4) 
# lines(ind$Year, ind$index - 1.96*ind$SE, col = 4, lty = 2)
# lines(ind$Year, ind$index + 1.96*ind$SE, col = 4, lty = 2)
# lines(ind$Year, ind$nominal, col = 2, lwd = 2)
# points(ind$Year, ind$nominal, col = 2, lwd = 2, pch = 1)
# legend("topleft", c("nominal CPUE", "standardized CPUE"), lwd = 2, pch = 19, col = c(2, 4), bty = "n")
# 
#save(outp, out, d2_bin, d2_log, total_d2, file = "data/linear_model_outputs.RData")


# now look at migration patterns -----------------------------------
# only area and season

# calculate the least-squares means from the linear models
emm_options(rg.limit = 300000)
predlogit <- as.data.frame(emmeans(outp, specs = ~ arnew*season, type = "response"))
predpos  <- as.data.frame(emmeans(out, specs = ~ arnew*season, type = "response"))
predlogit  
predpos

# calculate correlation between indices 
co <- cor(predlogit$prob, predpos$response, method="pearson")
co  # correlation between indices is very small 

# calculate the combined index and the combined SE
predind <-  predlogit$prob * predpos$response    # estimated abundance is prob. of occurrence * estimated abundance when present
predse <- sqrt(comb.var(predpos$response, predpos$SE, predlogit$prob, predlogit$SE, co))

area <- predpos$arnew
seas <- predpos$seas

mat <- matrix(predind, 5, 4)
matse <- matrix(predse, 5, 4)
rownames(mat) <- predpos$arnew[1:5]
colnames(mat) <- predpos$seas[c(1, 6, 11, 16)]
mat
matse

seasonal_hex <- c("#8CD3DF", "#78B781", "#FFC107", "#A33851")

barplot(t(mat), beside = T, legend = colnames(mat), names.arg = rownames(mat), 
        col = seasonal_hex, las = 1, 
        main = "Standardized catch rates by season and area")
abline(h = 0)

matplot(t(mat), lty = 1, lwd = 1, col = 1:5, type = "b", pch = 19, axes = F, 
        ylab = "standardized catch rate")
axis(1, at = 1:4, colnames(mat))
axis(2, las = 2); box()
legend("topright", col = 1:5, rownames(mat), lty = 1, lwd = 2)


# year + area x season

# now look at migration patterns -----------------------------------
# only area and season

# calculate the least-squares means from the linear models
emm_options(rg.limit = 300000)
predlogit <- as.data.frame(emmeans(outp, specs = ~ year + arnew*season, type = "response"))
predpos  <- as.data.frame(emmeans(out, specs = ~ year + arnew*season, type = "response"))
predlogit  
predpos

# calculate correlation between indices 
co <- cor(predlogit$prob, predpos$response, method="pearson")
co  # correlation between indices is very small 

fin <- data.frame(predlogit, predpos)

# calculate the combined index and the combined SE
fin$predind <-  fin$prob * fin$response    # estimated abundance is prob. of occurrence * estimated abundance when present
fin$predse <- sqrt(comb.var(fin$response, fin$SE.1, fin$prob, fin$SE, co))

fin$season.1 <- as.numeric(fin$season)
fin$yrseas <- as.numeric(as.vector(fin$year)) + (fin$season.1-1)/4

fin <- fin[order(fin$yrseas), ]

plot(0, col = 0, xlim = c(1986, 2022), ylim = c(0, 18), las = 1, xlab = "", ylab = "standardized catch rate")
lis <- levels(fin$arnew)
cols <- rainbow(5)

for (i in 1:5)  {
  m <- which(fin$arnew == lis[i])
  lines(fin$yrseas[m], fin$predind[m], col = cols[i])
  points(fin$yrseas[m], fin$predind[m], col = cols[i])
}
legend("topright", lis, col = cols, lty = 1)

  
# plot to match Tom's plot ------------------------------

scols <- rep(c("red", "green", "blue", "purple"), 37)
par(mfcol = c(3, 2)) 

lis <- levels(fin$arnew)
lis <- lis[c(1:3, 5, 4)]

for (i in 1:5)  {
  m <- which(fin$arnew == lis[i])
  plot(fin$yrseas[m], fin$predind[m], col = 8, type = "l", main = lis[i], 
       xlab = "", ylab = "relative cpue", ylim = c(0, 20))
  points(fin$yrseas[m], fin$predind[m], col = scols, pch = 19)
}
plot.new()
legend("center", c("Winter", "Spring", "Summer", "Fall"),
       col = scols[1:4], lty = 0, pch = 19)



# yearbin x area x season combination -----------------------

# calculate the least-squares means from the linear models
emm_options(rg.limit = 300000)
predlogit <- as.data.frame(emmeans(outp, specs = ~ yrbin*arnew*season, type = "response"))
predpos  <- as.data.frame(emmeans(out, specs = ~ yrbin*arnew*season, type = "response"))
predlogit  
predpos
table(predlogit$yrbin == predpos$yrbin)

co <- cor(predlogit$prob, predpos$response, method="pearson")
co 
predind <-  predlogit$prob * predpos$response    # estimated abundance is prob. of occurrence * estimated abundance when present
predse <- sqrt(comb.var(predpos$response, predpos$SE, predlogit$prob, predlogit$SE, co))

#predlogit$yrbin <- as.numeric(predlogit$yrbin)

par(mfrow = c(2, 4), mar = c(3, 3, 2, 1), mgp = c(2, 1, 0))
#lis <- c("1986-1994", "1995-2004", "2005-2014", "2015-2022")
lis <- unique(as.character(predpos$yrbin))

for (i in lis)  { 
  m <- predind[which(predpos$yrbin == i)]
  mat <- matrix(m, 5, 4)
  rownames(mat) <- levels(d$arnew)
  colnames(mat) <- levels(d$season)
  
  matplot(t(mat), lty = 1, lwd = 1, col = 1:5, type = "b", pch = 19, axes = F, 
          ylab = "standardized catch rate", main = i, ylim = c(0, 18))
  axis(1, at = 1:4, colnames(mat))
  axis(2, las = 2); box()
  legend("topright", col = 1:5, rownames(mat), lty = 1, lwd = 2, bty = "n")
  }




